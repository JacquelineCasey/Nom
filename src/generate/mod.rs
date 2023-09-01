
mod optimize_instructions;  // Makes optimizations at the instruction level.


use std::collections::HashMap;
use std::hash::Hash;

use crate::ast::{DeclarationAST, ExprAST, StatementAST};
use crate::analysis::{AnalyzedAST, Type};
use crate::instructions::{Instruction, IntSize, IntegerBinaryOperation, Constant};
use crate::util::reinterpret;

use optimize_instructions::optimize;


#[derive(Debug)]
pub struct GenerateError (pub String);

pub struct CodeGenerator {
    functions: HashMap<String, FunctionInfo>,
}

#[derive(Clone)]
enum PseudoInstruction {
    Actual (Instruction),
    Temp (TempInstruction)
}

#[derive(Clone)]
enum TempInstruction {
    Call (String),  // Call a function by name (we don't yet know its index).
}


impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator { functions: HashMap::new() }
    }

    pub fn generate(mut self, analyzed_ast: &AnalyzedAST) -> Result<Vec<Instruction>, GenerateError> {
        let function_list = analyzed_ast.ast.declarations.iter()
            .map(|decl| match decl {
                DeclarationAST::Function { name, block, .. } => {
                    Ok((name, block))
                }
                DeclarationAST::Variable { .. } => {
                    Err(GenerateError("Cannot yet handle global variables".to_string()))
                }
            })
            .collect::<Result<Vec<(_, _)>, _>>()?;

        // Preprocess step: Determine the local variable storage locations
        for (fn_name, _) in &function_list {
            self.functions.insert((*fn_name).to_string(), FunctionInfo::new(analyzed_ast, fn_name)?);
        }

        // Process functions and generate code

        for (fn_name, block) in &function_list {
            let instructions = self.generate_function(analyzed_ast, block, fn_name)?;

            let fn_info = self.functions.get_mut(*fn_name)
                .ok_or(GenerateError("Could not find function".to_string()))?;

            fn_info.initial_code = instructions;
        }
        
        let mut instructions = vec![];

        // Driver - calls the main.
        instructions.push(PseudoInstruction::Actual(Instruction::AdvanceStackPtr(8)));  // Space for return value. Alignment for main()
        instructions.push(PseudoInstruction::Temp(TempInstruction::Call("main".to_string())));
        instructions.push(PseudoInstruction::Actual(Instruction::RetractStackPtr(4)));  // Move to return value
        instructions.push(PseudoInstruction::Actual(Instruction::DebugPrintSigned(IntSize::FourByte)));
        instructions.push(PseudoInstruction::Actual(Instruction::Exit));


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
            })
            .collect::<Result<_, _>>()
    }

    // Push
    fn layout_function(&self, name: &str, instructions: &mut Vec<PseudoInstruction>) -> Result<(), GenerateError> {
        let a = self.functions.get(name).ok_or(GenerateError("Function not found".to_string()))?;
        
        for instr in &a.initial_code {
            instructions.push(instr.clone());
        }

        Ok(())
    }

    fn get_alignment_shift(depth: usize, alignment: usize) -> usize {
        if depth % alignment != 0 {
            alignment - (depth % alignment)
        }
        else {
            0
        }
    }

    fn generate_function(&self, analyzed_ast: &AnalyzedAST, subtree: &ExprAST, name: &str) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let function_info = self.functions.get(name).ok_or(GenerateError("Failed to find function".to_string()))?;

        let mut instructions = vec![];

        // TODO: Better alignment functions.
        let expr_type = analyzed_ast.get_expr_type(subtree);
        let expr_type_info = analyzed_ast.types.get(&expr_type).ok_or(GenerateError("Type not found".to_string()))?;

        let mut depth = function_info.top;
        let alignment = get_align_shift(depth, expr_type_info.alignment);
        depth += alignment;

        instructions.push(PseudoInstruction::Actual(Instruction::AdvanceStackPtr(depth)));

        instructions.append(&mut self.generate_expression(analyzed_ast, subtree, function_info, depth)?);  // TODO: Should this be zero or function_info.top. Can we call it depth?

        instructions.append(&mut Self::generate_return(function_info)?);

        Ok(optimize(instructions))
    }
    
    // Precondition: stack pointer is byte above return value.
    // Postcondition: return value moved to final destination. Function returns.
    fn generate_return(function_info: &FunctionInfo) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        // If we ever add special move semantics beyond shallow copy...
        // We might need to invoke them here. Hard to say...
        
        let (return_location, size) = function_info.variables.get(&Variable::Return)
            .ok_or(GenerateError("Return type not analyzed".to_string()))?;
        
        match size {
            0 => (),
            1 | 2 | 4 | 8 => {
                let int_size: IntSize = (*size).try_into()?;
                instructions.push(PseudoInstruction::Actual(Instruction::WriteBase(*return_location, int_size)));
            } 
            _ => {
                return Err(GenerateError("Not yet implemented: Moving types with weird sizes".to_string()));
            }
        }

        instructions.push(PseudoInstruction::Actual(Instruction::Return));

        Ok(instructions)
    }  

    // Precondition: The stack is aligned so as to hold a value of the expressions type, at the desired position.
    // Postcondition: The stack has the expression value at that desired position. The pointer points one byte above the value.
    // Expressions are being evaluated as rvalues, not as lvalues. In particular, pass a variable here is you want to put its
    // value on the stack, but see generate_statement if you want to store something into that variable.
    fn generate_expression(&self, analyzed_ast: &AnalyzedAST, subtree: &ExprAST, 
        function_info: &FunctionInfo, depth: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {

        let mut instructions = vec![];

        match subtree {
            ExprAST::Add(left, right, ..)
            | ExprAST::Subtract(left, right, ..)
            | ExprAST::Multiply(left, right, ..)
            | ExprAST::Divide(left, right, ..) => {
                let subtree_type = analyzed_ast.get_expr_type(subtree);
                let left_type = analyzed_ast.get_expr_type(left);
                let right_type = analyzed_ast.get_expr_type(right);

                if subtree_type != left_type || left_type != right_type {
                    return Err(GenerateError("Cannot handle binary operator applied to different types".to_string()));
                }

                if let Type::BuiltIn(curr_type) = subtree_type {
                    let arg_size = curr_type.get_int_size().ok_or(GenerateError("Expected builtin int type".to_string()))?;

                    instructions.append(&mut self.generate_expression(analyzed_ast, left, function_info, depth)?);
                    instructions.append(&mut self.generate_expression(analyzed_ast, right, function_info, depth + arg_size.to_usize())?);

                    instructions.push(PseudoInstruction::Actual(Instruction::IntegerBinaryOperation(
                        match subtree {
                            ExprAST::Add(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedAddition,
                            ExprAST::Add(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedAddition,
                            ExprAST::Subtract(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedSubtraction,
                            ExprAST::Subtract(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedSubtraction,
                            ExprAST::Multiply(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedMultiplication,
                            ExprAST::Multiply(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedMultiplication,
                            ExprAST::Divide(..) if curr_type.is_unsigned() => IntegerBinaryOperation::UnsignedDivision,
                            ExprAST::Divide(..) if curr_type.is_signed() => IntegerBinaryOperation::SignedDivision,
                            _ => panic!("Known unreachable")
                        }, 
                        arg_size)));
                }
                else {
                    return Err(GenerateError("Cannot run binary operator on non builtin type".to_string()));
                }
            }
            ExprAST::Literal(num, ..) => {
                // TODO: Support types beyond i32

                instructions.push(PseudoInstruction::Actual(
                    Instruction::PushConstant(Constant::FourByte(reinterpret::<i32, u32>(*num)))
                ));
            }
            ExprAST::Variable(name, ..) => {
                // This is placing a variable's value on the stack. See statement for storing
                // a variable.

                // TODO: Shadowing...
                if let Some((offset, size)) = function_info.variable_info_by_name(name) {
                    instructions.push(PseudoInstruction::Actual(
                        Instruction::ReadBase(offset, IntSize::try_from(size)?)
                    ));
                }
            }
            ExprAST::Block(statements, expr, ..) => {
                for statement in statements {
                    instructions.append(&mut self.generate_statement(analyzed_ast, statement, function_info, depth)?);
                }

                if let Some(expr) = expr {
                    instructions.append(&mut self.generate_expression(analyzed_ast, expr, function_info, depth)?);
                }
            }
        }

        Ok(instructions)
    }

    fn generate_statement(&self, analyzed_ast: &AnalyzedAST, statement: &StatementAST, 
        function_info: &FunctionInfo, depth: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {

        let mut instructions = vec![];

        match statement {
            StatementAST::ExpressionStatement(expr, _) => {
                let expr_type = analyzed_ast.get_expr_type(expr);
                let expr_type_info = analyzed_ast.types.get(&expr_type).ok_or(GenerateError("Type not found".to_string()))?;
                
                let align_shift = get_align_shift(depth, expr_type_info.alignment);

                // Align
                instructions.push(PseudoInstruction::Actual(
                    Instruction::AdvanceStackPtr(align_shift)
                ));
    
                instructions.append(&mut self.generate_expression(analyzed_ast, expr, function_info, depth + align_shift)?);

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
                    instructions.append(&mut self.generate_assignment(analyzed_ast, name, right, function_info, depth)?);
                }
                else {
                    return Err(GenerateError("Could not find variable".to_string()));
                }
            },
            StatementAST::Declaration(decl, ..) => {
                match decl {
                    DeclarationAST::Function { .. } => 
                        return Err(GenerateError("Tried to build function in function".to_string())),  // Lambdas?
                    DeclarationAST::Variable { name, expr, .. } => {
                        instructions.append(&mut self.generate_assignment(analyzed_ast, name, expr, function_info, depth)?);
                    }
                }
            }
        }

        Ok(instructions)
    }

    fn generate_assignment(&self, analyzed_ast: &AnalyzedAST, var_name: &str, expr: &ExprAST,
        function_info: &FunctionInfo, depth: usize) -> Result<Vec<PseudoInstruction>, GenerateError> {

        let mut instructions = vec![];
        
        let expr_type = analyzed_ast.get_expr_type(expr);
        let expr_type_info = analyzed_ast.types.get(&expr_type).ok_or(GenerateError("Type not found".to_string()))?;
        
        let align_shift = get_align_shift(depth, expr_type_info.alignment);

        // Align
        instructions.push(PseudoInstruction::Actual(
            Instruction::AdvanceStackPtr(align_shift)
        ));

        instructions.append(&mut self.generate_expression(analyzed_ast, expr, function_info, depth + align_shift)?);

        let (offset, size) = function_info.variable_info_by_name(var_name)
            .ok_or(GenerateError("Could not find local variable".to_string()))?;
        
        // Store generated expression
        instructions.push(PseudoInstruction::Actual(
            Instruction::WriteBase(offset, size.try_into()?)
        ));

        // Remove alignment
        instructions.push(PseudoInstruction::Actual(
            Instruction::RetractStackPtr(align_shift)
        ));

        Ok(instructions)
    }
}

fn get_align_shift(pointer_pos: usize, alignment: usize) -> usize {
    if pointer_pos % alignment != 0 {
        alignment - pointer_pos % alignment
    }
    else {
        0
    }
}

// Information associated with each function. This is a working copy, so many of 
// the fields are optional.
struct FunctionInfo {
    variables: HashMap<Variable, (isize, usize)>, // maps parameters, locals, and the return value to their position and sizes in memory.  
    top: usize,  // Points to byte one past the topmost local variable
    initial_code: Vec<PseudoInstruction>, // Not optimized, and not linked
}

impl FunctionInfo {
    fn new(analyzed_ast: &AnalyzedAST, name: &str) -> Result<FunctionInfo, GenerateError> {
        let mut info = FunctionInfo {
            variables: HashMap::new(),
            top: 0,
            initial_code: vec![],  // To be determined later
        };

        let analysis_info = analyzed_ast.functions.get(name)
            .ok_or(GenerateError("Could not find analyzed function data".to_string()))?;

        // We first allocate return value and arguments, then we push them behind
        // the base pointer and ensure they have 8 alignment. Then we allocate
        // local variables.
                  
        let return_type_info = analyzed_ast.types.get(&analysis_info.return_type)
            .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

    
        info.add_variable(Variable::Return, return_type_info.size, return_type_info.alignment);

        for (name, param) in &analysis_info.parameter_types {
            let param_type_info = analyzed_ast.types.get(param)
                .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(Variable::Parameter(name.clone()), param_type_info.size, param_type_info.alignment);
        }

        info.align_variables(8);

        // Bump everything added so far below the base pointer
        for (offset, _) in info.variables.values_mut() {
            *offset -= info.top as isize;
        }

        info.top = 16;  // Room for two u64 saved registers

        for (name, local) in &analysis_info.local_types {
            let local_type_info = analyzed_ast.types.get(local)
                .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(Variable::Parameter(name.clone()), local_type_info.size, local_type_info.alignment);
        }

        Ok(info)
    }

    // Ensures correct alignment
    fn add_variable(&mut self, variable: Variable, size: usize, alignment: usize) {
        self.align_variables(alignment);

        self.variables.insert(variable, (self.top as isize, size));
        self.top += size;
    }

    fn align_variables(&mut self, alignment: usize) {
        self.top += get_align_shift(self.top, alignment);
    }

    // Checks arguments and locals
    fn variable_info_by_name(&self, name: &str) -> Option<(isize, usize)> {
        if let Some(info) = self.variables.get(&Variable::Local(name.to_string())) {
            Some(*info)
        }
        else {
            self.variables.get(&Variable::Parameter(name.to_string())).copied()
        }
    }
}


#[derive(PartialEq, Eq, Hash)]
enum Variable {
    Return,
    Parameter (String),
    Local (String),
}