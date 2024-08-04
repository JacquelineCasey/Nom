
mod optimize_instructions;  // Makes optimizations at the instruction level.


use std::collections::HashMap;
use std::hash::Hash;

use crate::{CompilationEnvironment, util};
use crate::ast::{DeclarationAST, ExprAST, StatementAST};
use crate::analysis::types::{KindData, Type, TypeInfo};
use crate::instructions::{Instruction, IntSize, IntegerBinaryOperation, Constant};
use crate::util::reinterpret;
use crate::error::GenerateError;

use optimize_instructions::optimize;


pub struct CodeGenerator {
    functions: HashMap<String, FunctionInfo>,
}

#[derive(Clone, Debug)]
enum PseudoInstruction {
    Actual (Instruction),
    Temp (TempInstruction)
}

#[derive(Clone, Debug)]
enum TempInstruction {
    Call (String),  // Call a function by name (we don't yet know its index).
    #[allow(unused)] JumpIfTrue (u32),  // This is a unique id. This corresponds to a jump instruction later.
    Jump (u32),
    JumpIfFalse (u32),
    JumpFrom (u32),  // This will be removed (will not be an actual instruction), 
                     // but allows reasoning about jumps without counting instructions early on (before optimization).
}

/* For supporting lvalues vs rvalue semantics. For every expression we can determine 
 * a location, which is enough information to describe to the generator how to (efficiently) *
 * get the value. For some cases, it requires evaluating the expression in whole. In others, 
 * we can simply copy from somewhere, possibly with some offset. For others still, we may
 * need to evaluate an expression to determine that offset. */
#[derive(Clone, Debug)]
enum Location<'a> {
    EvaluatedExpression(&'a ExprAST),  // We have to fully evaluate this expression.
    Local(String), // A local (or argument).
    OffsetFrom(Box<Location<'a>>, isize) // A particular offset from another location.

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
                    },
                    _ => collapsed_inner
                }
            },
            _ => self,
        }
    }
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator { functions: HashMap::new() }
    }

    pub fn generate(mut self, env: &CompilationEnvironment) -> Result<Vec<Instruction>, GenerateError> {
        use PseudoInstruction as PI;
        use Instruction as I;

        // TODO: Pruning? Here or in lib / analysis
        
        let function_list = env.functions.iter()
            .map(|(name, func)| (name, &func.ast))
            .collect::<Vec<(_, _)>>();

        // Preprocess step: Determine the local variable storage locations
        for (fn_name, _) in &function_list {
            self.functions.insert((*fn_name).to_string(), FunctionInfo::new(env, fn_name)?);
        }

        // Process functions and generate code

        for (fn_name, block) in &function_list {
            let instructions = self.generate_function(env, block, fn_name)?;

            let fn_info = self.functions.get_mut(*fn_name)
                .ok_or(GenerateError("Could not find function".to_string()))?;

            fn_info.initial_code = instructions;
        }

        // Driver - calls the main.
        let mut instructions = vec![
            PI::Actual(I::AdvanceStackPtr(8)),  // Space for return value. Alignment for main()
            PI::Temp(TempInstruction::Call("main".to_string())),
            PI::Actual(I::RetractStackPtr(4)),  // Move to return value
            PI::Actual(I::DebugPrintSigned(IntSize::FourByte)),
            PI::Actual(I::Exit)
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

        instructions.into_iter()
            .map(|instr| match instr {
                PseudoInstruction::Actual(instr) => Ok(instr),
                PseudoInstruction::Temp(TempInstruction::Call(name)) => {
                    let location = function_locations.get(&name).ok_or(GenerateError(format!("Could not find function named {name}")))?;
                    Ok(Instruction::Call(*location))
                }
                PseudoInstruction::Temp(
                    TempInstruction::JumpIfTrue(..) 
                    | TempInstruction::JumpFrom(..) 
                    | TempInstruction::JumpIfFalse(..)
                    | TempInstruction::Jump(..)
                ) => {
                    
                    Err("Expected jump pseudo instructions to be removed".into())
                }
            })
            .collect::<Result<_, _>>()
    }

    fn resolve_jumps(instructions: Vec<PseudoInstruction>) -> Result<Vec<PseudoInstruction>, GenerateError> {
        use PseudoInstruction as PI;
        use TempInstruction as T;
        use Instruction as I;

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
                    }
                    else {
                        return Err("Two jump sources with same id".into());
                    }
                },
                PI::Temp(T::JumpFrom(i)) => {
                    jumps.entry(i).or_insert((None, None));

                    let entry = jumps.get_mut(&i).unwrap();

                    if entry.1.is_none() {
                        entry.1 = Some(effective_index);
                    }
                    else {
                        return Err("Two jump destinations with same id".into());
                    }
                },
                _ => (),
            }

            if !matches!(instr, PI::Temp(T::JumpFrom(_))) {
                effective_index += 1;
                final_instructions.push(instr);
            }
        }

        final_instructions.into_iter()
            .map(|instr| match instr {
                PI::Temp(ref temp @ (T::JumpIfTrue(i) | T::JumpIfFalse(i) | T::Jump(i))) => {
                    if let Some((Some(start), Some(end))) = jumps.get(&i) {
                        let shift = *end as i32 - *start as i32;
                        match temp {
                            T::JumpIfTrue(_) => Ok(PI::Actual(I::RelativeJumpIfTrue(shift))),
                            T::JumpIfFalse(_) => Ok(PI::Actual(I::RelativeJumpIfFalse(shift))),
                            T::Jump(_) => Ok(PI::Actual(I::RelativeJump(shift))),
                            _ => panic!("Expected Jump instruction")
                        }
                    }
                    else {
                        Err("Could not find start and end to jump".into())
                    }
                },
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

    fn generate_function(&self, env: &CompilationEnvironment, subtree: &ExprAST, name: &str) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let function_info = self.functions.get(name).ok_or(GenerateError("Failed to find function".to_string()))?;

        let mut instructions = vec![];

        // TODO: Better alignment functions.
        let expr_type = &env.type_index[&subtree.get_node_data().id];
        let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;

        let mut depth = function_info.top - 16;  // Skipping the saved registers is done by Call
        let alignment = get_align_shift(depth, expr_type_info.alignment);  
        depth += alignment;

        instructions.push(PseudoInstruction::Actual(Instruction::AdvanceStackPtr(depth)));

        instructions.append(&mut self.generate_expression(env, subtree, function_info, depth)?);  // TODO: Should this be zero or function_info.top. Can we call it depth?

        instructions.append(&mut self.generate_return(env, function_info)?); //

        let instructions = optimize(instructions);
        let instructions = Self::resolve_jumps(instructions)?;
        Ok(instructions)
    }
    
    // Precondition: stack pointer is byte above return value.
    // Postcondition: return value moved to final destination. Function returns.
    fn generate_return(&self, env: &CompilationEnvironment, function_info: &FunctionInfo) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];
        
        // If we ever add special move semantics beyond shallow copy...
        // We might need to invoke them here. Hard to say...
        
        let (return_location, size, return_type) = function_info.variables.get(&Variable::Return)
            .ok_or(GenerateError("Return type not analyzed".to_string()))?;

        let TypeInfo { alignment, .. } = env.types[return_type];
        
        instructions.append(&mut self.generate_write_to_base(*return_location, *size, alignment)?);

        instructions.push(PseudoInstruction::Actual(Instruction::Return));

        Ok(instructions)
    }  

    // Precondition: The stack is aligned so as to hold a value of the expressions type, at the desired position.
    // Postcondition: The stack has the expression value at that desired position. The pointer points one byte above the value.
    // Expressions are being evaluated as rvalues, not as lvalues. In particular, pass a variable here is you want to put its
    // value on the stack, but see generate_statement if you want to store something into that variable.
    #[allow(clippy::too_many_lines)]
    fn generate_expression(&self, env: &CompilationEnvironment, subtree: &ExprAST, 
        function_info: &FunctionInfo, depth: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {

        use ExprAST as E;
        use PseudoInstruction as PI;
        use Instruction as I;

        let mut instructions = vec![];

        match subtree {
            E::Add(left, right, ..)
            | E::Subtract(left, right, ..)
            | E::Multiply(left, right, ..)
            | E::Divide(left, right, ..)
            | E::Modulus(left, right, ..) => {
                let subtree_type = &env.type_index[&subtree.get_node_data().id];
                let left_type = &env.type_index[&left.get_node_data().id];
                let right_type = &env.type_index[&right.get_node_data().id];

                if subtree_type != left_type || left_type != right_type {
                    return Err("Cannot handle binary operator applied to different types".into());
                }

                if let Type::BuiltIn(curr_type) = subtree_type {
                    let arg_size = curr_type.get_int_size().ok_or(GenerateError("Expected builtin int type".to_string()))?;

                    instructions.append(&mut self.generate_expression(env, left, function_info, depth)?);
                    instructions.append(&mut self.generate_expression(env, right, function_info, depth + arg_size.to_usize())?);

                    instructions.push(PI::Actual(I::IntegerBinaryOperation(
                        match subtree {
                            E::Add(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedAddition,
                            E::Add(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedAddition,
                            E::Subtract(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedSubtraction,
                            E::Subtract(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedSubtraction,
                            E::Multiply(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedMultiplication,
                            E::Multiply(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedMultiplication,
                            E::Divide(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedDivision,
                            E::Divide(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedDivision,
                            E::Modulus(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedSubtraction,
                            E::Modulus(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedModulus,
                            _ => panic!("Known unreachable")
                        }, 
                        arg_size)));
                }
                else {
                    return Err("Cannot run binary operator on non builtin type".into());
                }
            },
            E::Comparison(left, right, comparison, ..) => {
                let left_type = &env.type_index[&left.get_node_data().id];
                let right_type = &env.type_index[&right.get_node_data().id];

                if left_type != right_type {
                    return Err("Left and Right part of comparison have different types".into())
                }

                let align_shift = get_align_shift(depth, env.types[left_type].alignment);

                let Type::BuiltIn(builtin_type) = left_type
                    else { return Err("Tried to compare non builtin types".into()) };

                let Some(int_size) = builtin_type.get_int_size()
                    else { return Err("Tried to compare builtin type without int_size".into()) };

                instructions.push(PI::Actual(I::AdvanceStackPtr(align_shift)));

                instructions.append(&mut self.generate_expression(env, left, function_info, depth + align_shift)?);
                instructions.append(&mut self.generate_expression(env, right, function_info, depth + align_shift + env.types[left_type].size)?);

                instructions.push(PI::Actual(I::IntegerComparisonOperation { comparison: *comparison, size: int_size, signed: builtin_type.is_signed() }));

                instructions.push(PI::Actual(I::RetractMoving(align_shift, IntSize::OneByte)));
            },
            E::And(left, right, _) | E::Or(left, right, _) => {
                let jump_id = util::next_id();

                instructions.append(&mut self.generate_expression(env, left, function_info, depth)?);
                
                instructions.push(PI::Actual(I::Duplicate(IntSize::OneByte)));

                if let E::And(..) = subtree {
                    instructions.push(PI::Temp(TempInstruction::JumpIfFalse(jump_id)));
                }
                else {
                    instructions.push(PI::Temp(TempInstruction::JumpIfTrue(jump_id)));
                }

                instructions.push(PI::Actual(I::RetractStackPtr(1)));  // Overwrite old bool.
                instructions.append(&mut self.generate_expression(env, right, function_info, depth)?);

                instructions.push(PI::Temp(TempInstruction::JumpFrom(jump_id)));
                // If we jumped, the left value is still there. Otherwise, it was repleaced with the right value.
            },
            E::Not(inner, _) => {
                instructions.append(&mut self.generate_expression(env, inner, function_info, depth)?);
                instructions.push(PI::Actual(I::BooleanNot));
            },
            E::IntegerLiteral(num, data) => {
                let num_type = &env.type_index[&data.id];

                let Type::BuiltIn(builtin) = num_type
                    else { return Err("Literal has non built in type".into()) };

                let int_size = builtin.get_int_size().ok_or(GenerateError::from("Literal type did not fit in int"))?;

                if builtin.is_signed() {
                    match int_size {
                        IntSize::OneByte => instructions.push(PI::Actual(I::PushConstant(Constant::OneByte(reinterpret::<i8, u8>(*num as i8))))),
                        IntSize::TwoByte => instructions.push(PI::Actual(I::PushConstant(Constant::TwoByte(reinterpret::<i16, u16>(*num as i16))))),
                        IntSize::FourByte => instructions.push(PI::Actual(I::PushConstant(Constant::FourByte(reinterpret::<i32, u32>(*num as i32))))),
                        IntSize::EightByte => instructions.push(PI::Actual(I::PushConstant(Constant::EightByte(reinterpret::<i64, u64>(*num as i64))))),
                    }
                }
                else {
                    match int_size {
                        IntSize::OneByte => instructions.push(PI::Actual(I::PushConstant(Constant::OneByte(*num as u8)))),
                        IntSize::TwoByte => instructions.push(PI::Actual(I::PushConstant(Constant::TwoByte(*num as u16)))),
                        IntSize::FourByte => instructions.push(PI::Actual(I::PushConstant(Constant::FourByte(*num as u32)))),
                        IntSize::EightByte => instructions.push(PI::Actual(I::PushConstant(Constant::EightByte(*num as u64)))),
                    }
                }
            }
            E::BooleanLiteral(val, ..) => {
                match val {
                    true => instructions.push(PI::Actual(I::PushConstant(Constant::OneByte(1)))),
                    false => instructions.push(PI::Actual(I::PushConstant(Constant::OneByte(0)))),
                }
            }
            E::Variable(name, ..) => {
                // This is placing a variable's value on the stack. See statement for storing
                // a variable.

                // TODO: Shadowing...
                let (offset, size, val_type) = function_info.variable_info_by_name(name).expect("known to exist");
                instructions.append(&mut self.generate_read_from_base(*offset, *size, env.types[val_type].alignment)?);
            }
            E::Block(statements, expr, ..) => {
                for statement in statements {
                    instructions.append(&mut self.generate_statement(env, statement, function_info, depth)?);
                }

                if let Some(expr) = expr {
                    instructions.append(&mut self.generate_expression(env, expr, function_info, depth)?);
                }
            }
            E::FunctionCall(name, subexprs, ..) => {
                // We assume that the depth is already such that a value from the function
                // Can be aligned. If the alignment is not 8 though, we shift, run the function,
                // and then pull the value back.

                let info = self.functions.get(name)
                    .ok_or(GenerateError("Function not found".to_string()))?;

                let align_shift = get_align_shift(depth, 8);

                instructions.push(PI::Actual(I::AdvanceStackPtr(align_shift)));
                
                let (relative_return_loc, return_size, return_type) = info.variables.get(&Variable::Return).expect("Known exists");
                
                // So relative position is how far below the function we currently are.
                
                // Skip past the 
                instructions.push(PI::Actual(I::AdvanceStackPtr(*return_size)));
                let mut relative_position = relative_return_loc + *return_size as isize;

                if subexprs.len() != info.parameters.len() {
                    return Err(
                        format!("Function has wrong number of arguments. {} arguments vs {} parameters", subexprs.len(), info.parameters.len())
                        .into()
                    );
                }

                for (expr, param) in subexprs.iter().zip(&info.parameters) {
                    let (param_loc, size, _) = info.variables.get(param).expect("Known exists");

                    instructions.push(PI::Actual(I::AdvanceStackPtr((param_loc - relative_position) as usize)));
                    relative_position = *param_loc;
                    
                    instructions.append(&mut self.generate_expression(
                        env, 
                        expr, 
                        function_info, 
                        depth + align_shift + (relative_position - relative_return_loc) as usize
                    )?);
                    relative_position += *size as isize;
                }

                // Finally align to function call.
                instructions.push(PI::Actual(I::AdvanceStackPtr((-relative_position) as usize)));

                instructions.push(PI::Temp(TempInstruction::Call(name.clone())));
                
                instructions.push(PI::Actual(I::RetractStackPtr((-relative_return_loc) as usize - return_size)));

                // Retract moving to original expression location
                match return_size {
                    0 => instructions.push(PI::Actual(I::RetractStackPtr(align_shift))),
                    _ => instructions.append(&mut self.generate_stack_retraction(align_shift, *return_size, env.types[return_type].alignment)?),
                }
            },
            E::If { condition, block, else_branch: None, .. } => {
                let mut condition_instrs = self.generate_expression(env, condition, function_info, depth)?;
                let mut block_instrs = self.generate_expression(env, block, function_info, depth)?;
                let skip_jump_id = util::next_id();

                instructions.append(&mut condition_instrs);
                instructions.push(PI::Temp(TempInstruction::JumpIfFalse(skip_jump_id)));
                instructions.append(&mut block_instrs);

                instructions.push(PI::Temp(TempInstruction::JumpFrom(skip_jump_id)));
            },
            E::If { condition, block, else_branch: Some(else_branch), .. } => {
                let mut condition_instrs = self.generate_expression(env, condition, function_info, depth)?;
                let mut block_instrs = self.generate_expression(env, block, function_info, depth)?;
                let mut else_instrs = self.generate_expression(env, else_branch, function_info, depth)?;

                let jump_1_id = util::next_id();
                let jump_2_id = util::next_id();

                instructions.append(&mut condition_instrs);                
                instructions.push(PI::Temp(TempInstruction::JumpIfFalse(jump_1_id)));
                instructions.append(&mut block_instrs);
                instructions.push(PI::Temp(TempInstruction::Jump(jump_2_id)));

                instructions.push(PI::Temp(TempInstruction::JumpFrom(jump_1_id)));
                instructions.append(&mut else_instrs);

                instructions.push(PI::Temp(TempInstruction::JumpFrom(jump_2_id)));
            },
            E::While { condition, block, .. } => {
                let mut condition_instrs = self.generate_expression(env, condition, function_info, depth)?;
                let mut block_instrs = self.generate_expression(env, block, function_info, depth)?;
                let skip_jump_id = util::next_id();
                let back_jump_id = util::next_id();

                instructions.push(PI::Temp(TempInstruction::JumpFrom(back_jump_id)));
                instructions.append(&mut condition_instrs);
                
                instructions.push(PI::Temp(TempInstruction::JumpIfFalse(skip_jump_id)));
                instructions.append(&mut block_instrs);
                instructions.push(PI::Temp(TempInstruction::Jump(back_jump_id)));

                instructions.push(PI::Temp(TempInstruction::JumpFrom(skip_jump_id)));
            },
            E::Return(Some(expr), _) => {
                let expr_type = &env.type_index[&expr.get_node_data().id];
                let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;
                
                let align_shift = get_align_shift(depth, expr_type_info.alignment);

                // Align
                instructions.push(PseudoInstruction::Actual(
                    Instruction::AdvanceStackPtr(align_shift)
                ));
    
                instructions.append(&mut self.generate_expression(env, expr, function_info, depth + align_shift)?);
                
                // Everything above can be its own function, see Statement::Expression too

                instructions.append(&mut self.generate_return(env, function_info)?); //
            },
            E::Return(None, _) => {
                instructions.append(&mut self.generate_return(env, function_info)?); //
            }
            E::StructExpression { name, members, ..  } => {
                let TypeInfo { kind, .. } = env.types.get(&name.clone().into()).expect("Known exists");
                
                let KindData::Struct { members: type_members } = kind
                    else { panic!("Expected struct type") };

                let mut member_exprs = Vec::<(&ExprAST, &Type, usize)>::new();

                for (member_name, expr) in members {
                    let (member_type, member_offset) = &type_members[member_name];
                    member_exprs.push((expr, member_type, *member_offset));
                }

                member_exprs.sort_by_key(|(_, _, offset)| *offset);

                let mut offset_into_struct = 0;

                for (expr, member_type, offset) in member_exprs {
                    if offset_into_struct < offset {
                        instructions.push(PI::Actual(I::AdvanceStackPtr(offset - offset_into_struct)));
                    }

                    instructions.append(&mut self.generate_expression(env, expr, function_info, depth + offset)?);

                    offset_into_struct = offset + env.types[member_type].size;
                }           
            }
            E::MemberAccess(_, _, _) => {
                todo!("GENERATE (this is gonna hurt...)")

                // Need to carefully work out l vs r value case...
                // If I remember correctly, at this point we know that we are evaluating
                // an expression. However, if we want to do so efficiently, we might
                // treat an lvalue vs rvalue left side differently. For a temporary,
                // we want to scoop out the value and discard the rest. For a local,
                // we definitely do not want to evaluate the whole struct, just grab
                // the value instead.

                // I'm picturing a helper which crawls the tree looking to "lvalue
                // evaluate" it into a "location" struct.
            }
            E::Moved => panic!("ExprAST Moved"),
        }

        Ok(instructions)
    }

    fn generate_statement(&self, env: &CompilationEnvironment, statement: &StatementAST, 
        function_info: &FunctionInfo, depth: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {

        let mut instructions = vec![];

        match statement {
            StatementAST::ExpressionStatement(expr, _) => {
                let expr_type = &env.type_index[&expr.get_node_data().id];
                let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;
                
                let align_shift = get_align_shift(depth, expr_type_info.alignment);

                // Align
                instructions.push(PseudoInstruction::Actual(
                    Instruction::AdvanceStackPtr(align_shift)
                ));
    
                instructions.append(&mut self.generate_expression(env, expr, function_info, depth + align_shift)?);

                // Ignore generated expression
                instructions.push(PseudoInstruction::Actual(
                    Instruction::RetractStackPtr(expr_type_info.size)
                ));

                // Remove alignment
                instructions.push(PseudoInstruction::Actual(
                    Instruction::RetractStackPtr(align_shift)
                ));
            },
            StatementAST::Assignment(left, right, ..) => {
                if let ExprAST::Variable(name, ..) = left {
                    instructions.append(&mut self.generate_assignment(env, name, right, function_info, depth)?);
                }
                else {
                    return Err("Could not find variable".into());
                }
            },
            StatementAST::CompoundAssignment(..) =>
                return Err("Expected Compound Assignment to have been desugared".into()),
            StatementAST::Declaration(decl, ..) => {
                match decl {
                    DeclarationAST::Function { .. } => 
                        return Err("Tried to build function in function".into()),  // Lambdas?
                    DeclarationAST::Struct { .. } => 
                        return Err("Tried to build struct in function".into()),  // Lambdas?
                    DeclarationAST::Variable { name, expr, .. } => {
                        instructions.append(&mut self.generate_assignment(env, name, expr, function_info, depth)?);
                    }
                }
            }
        }

        Ok(instructions)
    }

    fn generate_assignment(&self, env: &CompilationEnvironment, var_name: &str, expr: &ExprAST,
        function_info: &FunctionInfo, depth: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {

        let mut instructions = vec![];
        
        let expr_type = &env.type_index[&expr.get_node_data().id];
        let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;
        
        let align_shift = get_align_shift(depth, expr_type_info.alignment);

        // Align
        instructions.push(PseudoInstruction::Actual(
            Instruction::AdvanceStackPtr(align_shift)
        ));

        instructions.append(&mut self.generate_expression(env, expr, function_info, depth + align_shift)?);

        let (offset, size, _) = function_info.variable_info_by_name(var_name)
            .ok_or(GenerateError("Could not find local variable".to_string()))?;
    
        // Store generated value
        instructions.append(&mut self.generate_write_to_base(*offset, *size, expr_type_info.alignment)?);

        // Remove alignment
        instructions.push(PseudoInstruction::Actual(
            Instruction::RetractStackPtr(align_shift)
        ));

        Ok(instructions)
    }

    /// Generates code to move a value (with given size and alignment) to some
    /// location given as an offset from the base pointer (e.g. a local, a return value, a
    /// mutable argument).
    /// 
    /// base_offset is the location of the target. val_size is the size of the value
    /// (and implicitely, the target). alignment is the alignment of the type.
    fn generate_write_to_base(&self, base_offset: isize, val_size: usize, alignment: usize) -> Result<Vec<PseudoInstruction>, GenerateError>  {
        let mut bytes_remaining = val_size;
        let mut instructions = vec![];

        while bytes_remaining != 0 {
            for int_size in [8, 4, 2, 1] {
                if int_size > alignment || int_size > bytes_remaining { 
                    continue; 
                }

                if (bytes_remaining - int_size) % int_size != 0 {
                    continue;
                }

                bytes_remaining -= int_size;
                instructions.push(PseudoInstruction::Actual(Instruction::WriteBase(
                    base_offset + isize::try_from(bytes_remaining).expect("small enough"), 
                    int_size.try_into().expect("8, 4, 2, 1 are valid")
                )));

                break;
            }
        }

        Ok(instructions)
    }

    /// Generates code to move a value (with given size and alignment) from some
    /// location given as an offset from the base pointer (e.g. a local, or a
    /// mutable argument) to the stack.
    /// 
    /// base_offset is the location of the target. val_size is the size of the value
    /// (and implicitely, the target). alignment is the alignment of the type.
    /// We assume we are already aligned for that type (as is typical).
    fn generate_read_from_base(&self, base_offset: isize, val_size: usize, alignment: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut bytes_remaining = val_size;
        let mut instructions = vec![];

        while bytes_remaining != 0 {
            for int_size in [8, 4, 2, 1] {
                if int_size > alignment || int_size > bytes_remaining { 
                    continue; 
                }

                instructions.push(PseudoInstruction::Actual(Instruction::ReadBase(
                    base_offset + isize::try_from(val_size - bytes_remaining).expect("small enough"), 
                    int_size.try_into().expect("8, 4, 2, 1 are valid")
                )));

                bytes_remaining -= int_size;

                break;
            }
        }

        Ok(instructions)
    }

    /// Generates code to move a value that currently exists on the stack down
    /// some number of bytes (the shift).
    fn generate_stack_retraction(&self, shift: usize, size: usize, alignment: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {    
        let mut instructions = vec![];
        if shift == 0 {
            return Ok(instructions);
        }

        if size == alignment {
            instructions.push(PseudoInstruction::Actual(Instruction::RetractMoving(shift, size.try_into().expect("8, 4, 2, 1"))));
            return Ok(instructions);
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

                instructions.push(PseudoInstruction::Actual(Instruction::WriteStack(
                    - isize::try_from(bytes_remaining).expect("small"), 
                    - isize::try_from(bytes_remaining + shift).expect("small"), 
                    int_size.try_into().expect("8, 4, 2, 1 are valid")
                )));

                bytes_remaining -= int_size;

                break;
            }
        }

        instructions.push(PseudoInstruction::Actual(Instruction::RetractStackPtr(shift)));

        Ok(instructions)
    }

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
                Ok(Location::EvaluatedExpression(expr))  // These are never lvalues, basically. 
            },
            ExprAST::Variable(name, _) => {
                Ok(Location::Local(name.clone()))
            },
            ExprAST::MemberAccess(struct_expr, member_name, _) => {
                let struct_type = &env.type_index[&struct_expr.get_node_data().id];
                let struct_info = &env.types[&struct_type];
                
                let KindData::Struct { members } = &struct_info.kind
                    else { return Err("Expected struct type".into()) };
                
                let (_, field_alignment) = members[member_name];

                let inner_location = self.locate_expr(&struct_expr, env)?;
                Ok(Location::OffsetFrom(Box::new(inner_location), isize::try_from(field_alignment).expect("Small enough")))
            },
            ExprAST::Moved => panic!("Moved Expression"),
        }
    }
}

fn get_align_shift(depth: usize, alignment: usize) -> usize {
    if depth % alignment != 0 {
        alignment - depth % alignment
    }
    else {
        0
    }
}

// Information associated with each function. This is a working copy, so many of 
// the fields are optional.
#[derive(Debug)]
struct FunctionInfo {
    variables: HashMap<Variable, (isize, usize, Type)>, // maps parameters, locals, and the return value to their position and sizes in memory.  
    top: usize,  // Points to byte one past the topmost local variable
    initial_code: Vec<PseudoInstruction>, // Not optimized, and not linked
    parameters: Vec<Variable>,
}

impl FunctionInfo {
    fn new(env: &CompilationEnvironment, name: &str) -> Result<FunctionInfo, GenerateError> {
        let mut info = FunctionInfo {
            variables: HashMap::new(),
            top: 0,
            initial_code: vec![],  // To be determined later
            parameters: vec![],
        };

        let analysis_info = env.functions.get(name)
            .ok_or(GenerateError("Could not find analyzed function data".to_string()))?;

        // We first allocate return value and arguments, then we push them behind
        // the base pointer and ensure they have 8 alignment. Then we allocate
        // local variables.
                  
        let return_type_info = env.types.get(&analysis_info.return_type)
            .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

    
        info.add_variable(Variable::Return, return_type_info.size, return_type_info.alignment, analysis_info.return_type.clone());

        for (name, param_type) in &analysis_info.parameter_types {
            let param_type_info = env.types.get(param_type)
                .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(Variable::Parameter(name.clone()), param_type_info.size, param_type_info.alignment, param_type.clone());
            info.parameters.push(Variable::Parameter(name.clone()));
        }

        info.top += get_align_shift(info.top, 8);

        // Bump everything added so far below the base pointer
        for (offset, _, _) in info.variables.values_mut() {
            *offset -= info.top as isize;
        }

        info.top = 16;  // Room for two u64 saved registers

        for (name, local_type) in &analysis_info.local_types {
            if info.variables.contains_key(&Variable::Parameter(name.clone())) {
                continue;
            }

            let local_type = local_type.clone().ok_or(GenerateError("Type not specified".into()))?;

            let local_type_info = env.types.get(&local_type)
                .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(Variable::Local(name.clone()), local_type_info.size, local_type_info.alignment, local_type);
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
        }
        else {
            self.variables.get(&Variable::Parameter(name.to_string()))
        }
    }
}


#[derive(PartialEq, Eq, Hash, Debug)]
enum Variable {
    Return,
    Parameter (String),
    Local (String),
}