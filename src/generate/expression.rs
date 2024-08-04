use crate::{
    analysis::types::Type,
    ast::{ASTNodeData, ExprAST, MathOperation},
    error::GenerateError,
    instructions::IntegerBinaryOperation,
    CompilationEnvironment, Instruction,
};

use super::{CodeGenerator, FunctionInfo, PseudoInstruction};

use Instruction as I;
use PseudoInstruction as PI;

impl CodeGenerator {
    // Handles Add, Substract, Multiply, Divide, Modulus enum variants
    pub fn generate_math_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        left: &ExprAST,
        right: &ExprAST,
        operation: MathOperation,
        node_data: &ASTNodeData,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let subtree_type = &env.type_index[&node_data.id];
        let left_type = &env.type_index[&left.get_node_data().id];
        let right_type = &env.type_index[&right.get_node_data().id];

        if subtree_type != left_type || left_type != right_type {
            return Err("Cannot handle binary operator applied to different types".into());
        }

        let mut instructions = vec![];

        if let Type::BuiltIn(curr_type) = subtree_type {
            let arg_size = curr_type
                .get_int_size()
                .ok_or(GenerateError("Expected builtin int type".to_string()))?;

            instructions.append(&mut self.generate_expression(env, left, function_info, depth)?);
            instructions.append(&mut self.generate_expression(
                env,
                right,
                function_info,
                depth + arg_size.to_usize(),
            )?);

            instructions.push(PI::Actual(I::IntegerBinaryOperation(
                match operation {
                    MathOperation::Add if curr_type.is_unsigned() => {
                        IntegerBinaryOperation::UnsignedAddition
                    }
                    MathOperation::Add if curr_type.is_signed() => {
                        IntegerBinaryOperation::SignedAddition
                    }
                    MathOperation::Subtract if curr_type.is_unsigned() => {
                        IntegerBinaryOperation::UnsignedSubtraction
                    }
                    MathOperation::Subtract if curr_type.is_signed() => {
                        IntegerBinaryOperation::SignedSubtraction
                    }
                    MathOperation::Multiply if curr_type.is_unsigned() => {
                        IntegerBinaryOperation::UnsignedMultiplication
                    }
                    MathOperation::Multiply if curr_type.is_signed() => {
                        IntegerBinaryOperation::SignedMultiplication
                    }
                    MathOperation::Divide if curr_type.is_unsigned() => {
                        IntegerBinaryOperation::UnsignedDivision
                    }
                    MathOperation::Divide if curr_type.is_signed() => {
                        IntegerBinaryOperation::SignedDivision
                    }
                    MathOperation::Modulus if curr_type.is_unsigned() => {
                        IntegerBinaryOperation::UnsignedSubtraction
                    }
                    MathOperation::Modulus if curr_type.is_signed() => {
                        IntegerBinaryOperation::SignedModulus
                    }
                    _ => panic!("Known unreachable"),
                },
                arg_size,
            )));
        } else {
            return Err("Cannot run binary operator on non builtin type".into());
        }

        Ok(instructions)
    }
}
