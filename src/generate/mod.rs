mod expression; // Contains functions that generate code to evaluate each type of expression.
mod optimize_instructions; // Makes optimizations at the instruction level.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use crate::analysis::types::{KindData, Type, TypeInfo};
use crate::ast::{DeclarationAST, ExprAST, StatementAST};
use crate::error::GenerateError;
use crate::instructions::{Constant, Instruction, IntSize};
use crate::util::{reinterpret, OutStream};
use crate::CompilationEnvironment;

use optimize_instructions::optimize;

pub struct CodeGenerator {
    functions: HashMap<String, FunctionInfo>,
}

#[derive(Clone, Debug)]
enum PseudoInstruction {
    Actual(Instruction),
    Temp(TempInstruction),
}

#[derive(Clone, Debug)]
enum TempInstruction {
    Call(String), // Call a function by name (we don't yet know its index).
    #[allow(unused)]
    JumpIfTrue(u32), // This is a unique id. This corresponds to a jump instruction later.
    Jump(u32),
    JumpIfFalse(u32),
    JumpFrom(u32), // This will be removed (will not be an actual instruction),
                   // but allows reasoning about jumps without counting instructions early on (before optimization).
}

/* For supporting lvalues vs rvalue semantics. For every expression we can determine
 * a location, which is enough information to describe to the generator how to (efficiently) *
 * get the value. For some cases, it requires evaluating the expression in whole. In others,
 * we can simply copy from somewhere, possibly with some offset. For others still, we may
 * need to evaluate an expression to determine that offset. */
#[derive(Clone, Debug)]
enum Location<'a> {
    EvaluatedExpression(&'a ExprAST), // We have to fully evaluate this expression.
    Local(String),                    // A local (or argument).
    OffsetFrom(Box<Location<'a>>, isize), // A particular offset from another location.
                                      // Soon - LocalUnknownOffset... maybe (String, Expr). Supporting expressions like arr[i * 2]
}

impl<'a> Location<'a> {
    // Returns a new location struct where the offsets are collapsed to a single
    // level.
    fn collapse_offsets(self) -> Self {
        match self {
            Location::OffsetFrom(inner, offset_a) => {
                let collapsed_inner = inner.collapse_offsets();
                match collapsed_inner {
                    Location::OffsetFrom(inner_inner, offset_b) => {
                        Location::OffsetFrom(inner_inner, offset_a + offset_b)
                    }
                    _ => Location::OffsetFrom(Box::new(collapsed_inner), offset_a),
                }
            }
            _ => self,
        }
    }
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator { functions: HashMap::new() }
    }

    pub fn generate(mut self, env: &CompilationEnvironment) -> Result<Vec<Instruction>, GenerateError> {
        use Instruction as I;
        use PseudoInstruction as PI;

        // TODO: Pruning? Here or in lib / analysis

        let function_list = env.functions.iter().map(|(name, func)| (name, &func.ast)).collect::<Vec<(_, _)>>();

        // Preprocess step: Determine the local variable storage locations
        for (fn_name, _) in &function_list {
            self.functions.insert((*fn_name).to_string(), FunctionInfo::new(env, fn_name)?);
        }

        // Process functions and generate code

        for (fn_name, block) in &function_list {
            let instructions = self.generate_function(env, block, fn_name)?;

            let fn_info =
                self.functions.get_mut(*fn_name).ok_or(GenerateError("Could not find function".to_string()))?;

            fn_info.initial_code = instructions;
        }

        // Driver - calls the main.
        let mut instructions = vec![
            PI::Actual(I::AdvanceStackPtr(8)), // Space for return value. Alignment for main()
            PI::Temp(TempInstruction::Call("main".to_string())),
            PI::Actual(I::RetractStackPtr(4)), // Move to return value
            PI::Actual(I::DebugPrintSigned(IntSize::FourByte)),
            PI::Actual(I::Exit),
        ];

        let mut function_locations: HashMap<String, usize> = HashMap::new();

        function_locations.insert("main".to_string(), instructions.len());
        self.layout_function("main", &mut instructions)?;

        for (fn_name, _) in function_list {
            if fn_name != "main" {
                function_locations.insert(fn_name.to_string(), instructions.len());
                self.layout_function(fn_name, &mut instructions)?;
            }
        }

        instructions
            .into_iter()
            .map(|instr| match instr {
                PseudoInstruction::Actual(instr) => Ok(instr),
                PseudoInstruction::Temp(TempInstruction::Call(name)) => {
                    let location = function_locations
                        .get(&name)
                        .ok_or(GenerateError(format!("Could not find function named {name}")))?;
                    Ok(Instruction::Call(*location))
                }
                PseudoInstruction::Temp(
                    TempInstruction::JumpIfTrue(..)
                    | TempInstruction::JumpFrom(..)
                    | TempInstruction::JumpIfFalse(..)
                    | TempInstruction::Jump(..),
                ) => Err("Expected jump pseudo instructions to be removed".into()),
            })
            .collect::<Result<_, _>>()
    }

    fn resolve_jumps(instructions: Vec<PseudoInstruction>) -> Result<Vec<PseudoInstruction>, GenerateError> {
        use Instruction as I;
        use PseudoInstruction as PI;
        use TempInstruction as T;

        // Maps jump id to the source and target effective indices
        let mut jumps: HashMap<u32, (Option<usize>, Option<usize>)> = HashMap::new();

        let mut final_instructions = vec![];

        let mut effective_index = 0;
        for instr in instructions {
            match instr {
                PI::Temp(T::JumpIfTrue(i) | T::JumpIfFalse(i) | T::Jump(i)) => {
                    jumps.entry(i).or_insert((None, None));

                    let entry = jumps.get_mut(&i).unwrap();

                    if entry.0.is_none() {
                        entry.0 = Some(effective_index);
                    } else {
                        return Err("Two jump sources with same id".into());
                    }
                }
                PI::Temp(T::JumpFrom(i)) => {
                    jumps.entry(i).or_insert((None, None));

                    let entry = jumps.get_mut(&i).unwrap();

                    if entry.1.is_none() {
                        entry.1 = Some(effective_index);
                    } else {
                        return Err("Two jump destinations with same id".into());
                    }
                }
                _ => (),
            }

            if !matches!(instr, PI::Temp(T::JumpFrom(_))) {
                effective_index += 1;
                final_instructions.push(instr);
            }
        }

        final_instructions
            .into_iter()
            .map(|instr| match instr {
                PI::Temp(ref temp @ (T::JumpIfTrue(i) | T::JumpIfFalse(i) | T::Jump(i))) => {
                    if let Some((Some(start), Some(end))) = jumps.get(&i) {
                        let shift = *end as i32 - *start as i32;
                        match temp {
                            T::JumpIfTrue(_) => Ok(PI::Actual(I::RelativeJumpIfTrue(shift))),
                            T::JumpIfFalse(_) => Ok(PI::Actual(I::RelativeJumpIfFalse(shift))),
                            T::Jump(_) => Ok(PI::Actual(I::RelativeJump(shift))),
                            _ => panic!("Expected Jump instruction"),
                        }
                    } else {
                        Err("Could not find start and end to jump".into())
                    }
                }
                i => Ok(i),
            })
            .collect::<Result<Vec<_>, GenerateError>>()
    }

    fn layout_function(&self, name: &str, instructions: &mut Vec<PseudoInstruction>) -> Result<(), GenerateError> {
        let a = self.functions.get(name).ok_or(GenerateError("Function not found".to_string()))?;

        for instr in &a.initial_code {
            instructions.push(instr.clone());
        }

        Ok(())
    }

    fn generate_function(
        &self,
        env: &CompilationEnvironment,
        subtree: &ExprAST,
        name: &str,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let function_info = self.functions.get(name).ok_or(GenerateError("Failed to find function".to_string()))?;

        // TODO: Better alignment functions.
        let expr_type = &env.type_index[&subtree.get_node_data().id];
        let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;

        let mut depth = function_info.top - 16; // Skipping the saved registers is done by Call
        let alignment = get_align_shift(depth, expr_type_info.alignment);
        depth += alignment;

        let mut instructions = vec![PseudoInstruction::Actual(Instruction::AdvanceStackPtr(depth))];
        let mut out = OutStream::new(&mut instructions);
        // TODO: Should this be zero or function_info.top. Can we call it depth?
        self.generate_expression(env, subtree, function_info, depth, &mut out)?;

        Self::generate_return_handoff(env, function_info, &mut out)?;

        let instructions = optimize(instructions);
        Self::resolve_jumps(instructions)
    }

    // Precondition: stack pointer is byte above return value.
    // Postcondition: return value moved to final destination. Function returns.
    // So called because it hands off control to the previous function. Not to be
    // confused with generate_return_expr, which handles the return keyword.
    fn generate_return_handoff(
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        out: &mut OutStream<PseudoInstruction>,
    ) -> Result<(), GenerateError> {
        // If we ever add special move semantics beyond shallow copy...
        // We might need to invoke them here. Hard to say...

        let (return_location, size, return_type) = function_info
            .variables
            .get(&Variable::Return)
            .ok_or(GenerateError("Return type not analyzed".to_string()))?;

        let TypeInfo { alignment, .. } = env.types[return_type];

        Self::generate_write_to_base(*return_location, *size, alignment, out);

        out.push(PseudoInstruction::Actual(Instruction::Return));

        Ok(())
    }

    // Precondition: The stack is aligned so as to hold a value of the expressions type, at the desired position.
    // Postcondition: The stack has the expression value at that desired position. The pointer points one byte above the value.
    // Expressions are being evaluated as rvalues, not as lvalues. In particular, pass a variable here is you want to put its
    // value on the stack, but see generate_statement if you want to store something into that variable.
    #[allow(clippy::too_many_lines)]
    fn generate_expression(
        &self,
        env: &CompilationEnvironment,
        subtree: &ExprAST,
        function_info: &FunctionInfo,
        depth: usize,
        out: &mut OutStream<PseudoInstruction>,
    ) -> Result<(), GenerateError> {
        use ExprAST as E;

        match subtree {
            E::Add(left, right, node_data) => self.generate_math_expr(
                env,
                function_info,
                depth,
                left,
                right,
                crate::ast::MathOperation::Add,
                node_data,
                out,
            ),
            E::Subtract(left, right, node_data) => self.generate_math_expr(
                env,
                function_info,
                depth,
                left,
                right,
                crate::ast::MathOperation::Subtract,
                node_data,
                out,
            ),
            E::Multiply(left, right, node_data) => self.generate_math_expr(
                env,
                function_info,
                depth,
                left,
                right,
                crate::ast::MathOperation::Multiply,
                node_data,
                out,
            ),
            E::Divide(left, right, node_data) => self.generate_math_expr(
                env,
                function_info,
                depth,
                left,
                right,
                crate::ast::MathOperation::Divide,
                node_data,
                out,
            ),
            E::Modulus(left, right, node_data) => self.generate_math_expr(
                env,
                function_info,
                depth,
                left,
                right,
                crate::ast::MathOperation::Modulus,
                node_data,
                out,
            ),
            E::Comparison(left, right, comparison, ..) => {
                self.generate_comparison_expr(env, function_info, depth, left, right, *comparison, out)
            }
            E::And(left, right, _) => {
                self.generate_binary_logic_expr(env, function_info, depth, left, right, true, out)
            }
            E::Or(left, right, _) => {
                self.generate_binary_logic_expr(env, function_info, depth, left, right, false, out)
            }
            E::Not(inner, _) => self.generate_not_expr(env, function_info, depth, inner, out),
            E::IntegerLiteral(num, data) => Self::generate_int_literal_expr(env, *num, data, out),
            E::BooleanLiteral(val, ..) => Ok(Self::generate_bool_literal_expr(*val, out)),
            E::Variable(name, ..) => Ok(Self::generate_variable_expr(env, function_info, name, out)),
            E::Block(statements, expr, ..) => {
                self.generate_block_expr(env, function_info, depth, statements, expr, out)
            }
            E::FunctionCall(name, subexprs, ..) => {
                self.generate_function_call_expr(env, function_info, depth, name, subexprs, out)
            }
            E::If { condition, block, else_branch, .. } => {
                self.generate_if_expr(env, function_info, depth, condition, block, else_branch, out)
            }
            E::While { condition, block, .. } => {
                self.generate_while_expr(env, function_info, depth, condition, block, out)
            }
            E::Return(expr, _) => self.generate_return_expr(env, function_info, depth, expr, out),
            E::StructExpression { name, members, .. } => {
                self.generate_struct_expr(env, function_info, depth, name, members, out)
            }
            E::MemberAccess(..) => self.generate_member_access_expr(env, function_info, depth, subtree, out),
            E::Moved => panic!("ExprAST Moved"),
        }
    }

    fn generate_statement(
        &self,
        env: &CompilationEnvironment,
        statement: &StatementAST,
        function_info: &FunctionInfo,
        depth: usize,
        out: &mut OutStream<PseudoInstruction>,
    ) -> Result<(), GenerateError> {
        match statement {
            StatementAST::ExpressionStatement(expr, _) => {
                let expr_type = &env.type_index[&expr.get_node_data().id];
                let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;

                let align_shift = get_align_shift(depth, expr_type_info.alignment);

                // Align
                out.push(PseudoInstruction::Actual(Instruction::AdvanceStackPtr(align_shift)));

                self.generate_expression(env, expr, function_info, depth + align_shift, out)?;

                // Ignore generated expression
                out.push(PseudoInstruction::Actual(Instruction::RetractStackPtr(expr_type_info.size)));

                // Remove alignment
                out.push(PseudoInstruction::Actual(Instruction::RetractStackPtr(align_shift)));
            }
            StatementAST::Assignment(left, right, ..) => {
                let location = self.locate_expr(left, env)?;
                self.generate_assignment(env, location, right, function_info, depth, out)?;
            }
            StatementAST::CompoundAssignment(..) => {
                return Err("Expected Compound Assignment to have been desugared".into())
            }
            StatementAST::Declaration(decl, ..) => {
                match decl {
                    DeclarationAST::Function { .. } => return Err("Tried to build function in function".into()), // Lambdas?
                    DeclarationAST::Struct { .. } => return Err("Tried to build struct in function".into()), // Lambdas?
                    DeclarationAST::Variable { name, expr, .. } => {
                        self.generate_assignment(env, Location::Local(name.clone()), expr, function_info, depth, out)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn generate_assignment(
        &self,
        env: &CompilationEnvironment,
        location: Location,
        right_expr: &ExprAST,
        function_info: &FunctionInfo,
        depth: usize,
        out: &mut OutStream<PseudoInstruction>,
    ) -> Result<(), GenerateError> {
        let expr_type = &env.type_index[&right_expr.get_node_data().id];
        let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;

        let align_shift = get_align_shift(depth, expr_type_info.alignment);

        // Align
        out.push(PseudoInstruction::Actual(Instruction::AdvanceStackPtr(align_shift)));

        self.generate_expression(env, right_expr, function_info, depth + align_shift, out)?;

        // We handle the most general case involving a field, but really if it
        // is just a variable then we say the field has offset 0.

        let (local, field_offset) = match location.collapse_offsets() {
            Location::Local(name) => (name, 0),
            Location::OffsetFrom(inner, offset) => match *inner {
                Location::Local(name) => (name, offset),
                _ => return Err("Cannot assign into an arbitrary expression".into()),
            },
            _ => return Err("Cannot assign into an arbitrary expression".into()),
        };

        let (var_offset, _, _) = function_info
            .variable_info_by_name(&local)
            .ok_or::<GenerateError>("Could not find local variable".into())?;

        // Store generated value
        Self::generate_write_to_base(var_offset + field_offset, expr_type_info.size, expr_type_info.alignment, out);

        // Remove alignment
        out.push(PseudoInstruction::Actual(Instruction::RetractStackPtr(align_shift)));

        Ok(())
    }

    /// Generates code to move a value (with given size and alignment) to some
    /// location given as an offset from the base pointer (e.g. a local, a return value, a
    /// mutable argument).
    ///
    /// `base_offset` is the location of the target. `val_size` is the size of the value
    /// (and implicitely, the target). `alignment` is the alignment of the type.
    fn generate_write_to_base(
        base_offset: isize,
        val_size: usize,
        alignment: usize,
        out: &mut OutStream<PseudoInstruction>,
    ) {
        let mut bytes_remaining = val_size;

        while bytes_remaining != 0 {
            for int_size in [8, 4, 2, 1] {
                if int_size > alignment || int_size > bytes_remaining {
                    continue;
                }

                if (bytes_remaining - int_size) % int_size != 0 {
                    continue;
                }

                bytes_remaining -= int_size;
                out.push(PseudoInstruction::Actual(Instruction::WriteBase(
                    base_offset + isize::try_from(bytes_remaining).expect("small enough"),
                    int_size.try_into().expect("8, 4, 2, 1 are valid"),
                )));

                break;
            }
        }
    }

    /// Generates code to move a value (with given size and alignment) from some
    /// location given as an offset from the base pointer (e.g. a local, or a
    /// mutable argument) to the stack.
    ///
    /// `base_offset` is the location of the target. `val_size` is the size of the value
    /// (and implicitely, the target). `alignment` is the alignment of the type.
    /// We assume we are already aligned for that type (as is typical).
    fn generate_read_from_base(
        base_offset: isize,
        val_size: usize,
        alignment: usize,
        out: &mut OutStream<PseudoInstruction>,
    ) {
        let mut bytes_remaining = val_size;

        while bytes_remaining != 0 {
            for int_size in [8, 4, 2, 1] {
                if int_size > alignment || int_size > bytes_remaining {
                    continue;
                }

                out.push(PseudoInstruction::Actual(Instruction::ReadBase(
                    base_offset + isize::try_from(val_size - bytes_remaining).expect("small enough"),
                    int_size.try_into().expect("8, 4, 2, 1 are valid"),
                )));

                bytes_remaining -= int_size;

                break;
            }
        }
    }

    /// Generates code to move a value that currently exists on the stack down
    /// some number of bytes (the shift).
    fn generate_stack_retraction(shift: usize, size: usize, alignment: usize, out: &mut OutStream<PseudoInstruction>) {
        if shift == 0 {
            return;
        }

        if size == alignment {
            out.push(PseudoInstruction::Actual(Instruction::RetractMoving(
                shift,
                size.try_into().expect("8, 4, 2, 1"),
            )));
            return;
        }

        let mut bytes_remaining = size;

        while bytes_remaining != 0 {
            for int_size in [8, 4, 2, 1] {
                if int_size > alignment || int_size > bytes_remaining {
                    continue;
                }

                if (size - bytes_remaining) % int_size != 0 {
                    continue;
                }

                out.push(PseudoInstruction::Actual(Instruction::WriteStack(
                    -isize::try_from(bytes_remaining).expect("small"),
                    -isize::try_from(bytes_remaining + shift).expect("small"),
                    int_size.try_into().expect("8, 4, 2, 1 are valid"),
                )));

                bytes_remaining -= int_size;

                break;
            }
        }

        out.push(PseudoInstruction::Actual(Instruction::RetractStackPtr(shift)));
    }

    #[allow(clippy::only_used_in_recursion)]
    fn locate_expr<'a>(&self, expr: &'a ExprAST, env: &CompilationEnvironment) -> Result<Location<'a>, GenerateError> {
        match expr {
            ExprAST::Add(..)
            | ExprAST::Subtract(..)
            | ExprAST::Multiply(..)
            | ExprAST::Divide(..)
            | ExprAST::Modulus(..)
            | ExprAST::Comparison(..)
            | ExprAST::Or(..)
            | ExprAST::And(..)
            | ExprAST::Not(..)
            | ExprAST::IntegerLiteral(..)
            | ExprAST::BooleanLiteral(..)
            | ExprAST::FunctionCall(..)
            | ExprAST::Block(..)
            | ExprAST::If { .. }
            | ExprAST::While { .. }
            | ExprAST::Return(..)
            | ExprAST::StructExpression { .. } => {
                Ok(Location::EvaluatedExpression(expr)) // These are never lvalues, basically.
            }
            ExprAST::Variable(name, _) => Ok(Location::Local(name.clone())),
            ExprAST::MemberAccess(struct_expr, member_name, _) => {
                let struct_type = &env.type_index[&struct_expr.get_node_data().id];
                let struct_info = &env.types[struct_type];

                let KindData::Struct { members } = &struct_info.kind else {
                    return Err("Expected struct type".into());
                };

                let (_, field_alignment) = members[member_name];

                let inner_location = self.locate_expr(struct_expr, env)?;
                Ok(Location::OffsetFrom(
                    Box::new(inner_location),
                    isize::try_from(field_alignment).expect("Small enough"),
                ))
            }
            ExprAST::Moved => panic!("Moved Expression"),
        }
    }
}

fn get_align_shift(depth: usize, alignment: usize) -> usize {
    if depth % alignment != 0 {
        alignment - depth % alignment
    } else {
        0
    }
}

// Information associated with each function. This is a working copy, so many of
// the fields are optional.
#[derive(Debug)]
struct FunctionInfo {
    variables: HashMap<Variable, (isize, usize, Type)>, // maps parameters, locals, and the return value to their position and sizes in memory.
    top: usize,                                         // Points to byte one past the topmost local variable
    initial_code: Vec<PseudoInstruction>,               // Not optimized, and not linked
    parameters: Vec<Variable>,
}

impl FunctionInfo {
    fn new(env: &CompilationEnvironment, name: &str) -> Result<FunctionInfo, GenerateError> {
        let mut info = FunctionInfo {
            variables: HashMap::new(),
            top: 0,
            initial_code: vec![], // To be determined later
            parameters: vec![],
        };

        let analysis_info =
            env.functions.get(name).ok_or(GenerateError("Could not find analyzed function data".to_string()))?;

        // We first allocate return value and arguments, then we push them behind
        // the base pointer and ensure they have 8 alignment. Then we allocate
        // local variables.

        let return_type_info = env
            .types
            .get(&analysis_info.return_type)
            .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

        info.add_variable(
            Variable::Return,
            return_type_info.size,
            return_type_info.alignment,
            analysis_info.return_type.clone(),
        );

        for (name, param_type) in &analysis_info.parameter_types {
            let param_type_info =
                env.types.get(param_type).ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(
                Variable::Parameter(name.clone()),
                param_type_info.size,
                param_type_info.alignment,
                param_type.clone(),
            );
            info.parameters.push(Variable::Parameter(name.clone()));
        }

        info.top += get_align_shift(info.top, 8);

        // Bump everything added so far below the base pointer
        for (offset, _, _) in info.variables.values_mut() {
            *offset -= info.top as isize;
        }

        info.top = 16; // Room for two u64 saved registers

        for (name, local_type) in &analysis_info.local_types {
            if info.variables.contains_key(&Variable::Parameter(name.clone())) {
                continue;
            }

            let local_type = local_type.clone().ok_or(GenerateError("Type not specified".into()))?;

            let local_type_info =
                env.types.get(&local_type).ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(
                Variable::Local(name.clone()),
                local_type_info.size,
                local_type_info.alignment,
                local_type,
            );
        }

        Ok(info)
    }

    // Ensures correct alignment
    fn add_variable(&mut self, variable: Variable, size: usize, alignment: usize, var_type: Type) {
        self.top += get_align_shift(self.top, alignment);

        self.variables.insert(variable, (self.top as isize, size, var_type));
        self.top += size;
    }

    // Checks arguments and locals
    fn variable_info_by_name(&self, name: &str) -> Option<&(isize, usize, Type)> {
        if let Some(info) = self.variables.get(&Variable::Local(name.to_string())) {
            Some(info)
        } else {
            self.variables.get(&Variable::Parameter(name.to_string()))
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
enum Variable {
    Return,
    Parameter(String),
    Local(String),
}
