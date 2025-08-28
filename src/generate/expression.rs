use crate::{
    ast::{ASTNodeData, MathOperation},
    instructions::{Comparison, IntegerBinaryOperation},
};

use super::{
    get_align_shift, reinterpret, util, Borrow, CodeGenerator, CompilationEnvironment, Constant, ExprAST, FunctionInfo,
    GenerateError, Instruction, IntSize, KindData, Location, PseudoInstruction, StatementAST, TempInstruction, Type,
    TypeInfo, Variable,
};

use Instruction as I;
use PseudoInstruction as PI;

impl CodeGenerator {
    // Handles Add, Substract, Multiply, Divide, Modulus enum variants
    #[allow(clippy::too_many_arguments)]
    pub(super) fn generate_math_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        left: &ExprAST,
        right: &ExprAST,
        operation: MathOperation,
        node_data: &ASTNodeData,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        use IntegerBinaryOperation as IBO;
        use MathOperation as MO;

        let subtree_type = &env.type_index[&node_data.id];
        let left_type = &env.type_index[&left.get_node_data().id];
        let right_type = &env.type_index[&right.get_node_data().id];

        if subtree_type != left_type || left_type != right_type {
            return Err("Cannot handle binary operator applied to different types".into());
        }

        let mut instructions = vec![];

        let Type::BuiltIn(curr_type) = subtree_type else {
            return Err("Cannot run binary operator on non builtin type".into());
        };

        let arg_size = curr_type.get_int_size().ok_or(GenerateError("Expected builtin int type".to_string()))?;

        instructions.append(&mut self.generate_expression(env, left, function_info, depth)?);
        instructions.append(&mut self.generate_expression(env, right, function_info, depth + arg_size.to_usize())?);

        instructions.push(PI::Actual(I::IntegerBinaryOperation(
            match operation {
                MO::Add if curr_type.is_unsigned() => IBO::UnsignedAddition,
                MO::Add if curr_type.is_signed() => IBO::SignedAddition,
                MO::Subtract if curr_type.is_unsigned() => IBO::UnsignedSubtraction,
                MO::Subtract if curr_type.is_signed() => IBO::SignedSubtraction,
                MO::Multiply if curr_type.is_unsigned() => IBO::UnsignedMultiplication,
                MO::Multiply if curr_type.is_signed() => IBO::SignedMultiplication,
                MO::Divide if curr_type.is_unsigned() => IBO::UnsignedDivision,
                MO::Divide if curr_type.is_signed() => IBO::SignedDivision,
                MO::Modulus if curr_type.is_unsigned() => IBO::UnsignedModulus,
                MO::Modulus if curr_type.is_signed() => IBO::SignedModulus,
                _ => panic!("Known unreachable"),
            },
            arg_size,
        )));

        Ok(instructions)
    }

    pub(super) fn generate_comparison_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        left: &ExprAST,
        right: &ExprAST,
        comparison: Comparison,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        let left_type = &env.type_index[&left.get_node_data().id];
        let right_type = &env.type_index[&right.get_node_data().id];

        if left_type != right_type {
            return Err("Left and Right part of comparison have different types".into());
        }

        let align_shift = get_align_shift(depth, env.types[left_type].alignment);

        let Type::BuiltIn(builtin_type) = left_type else {
            return Err("Tried to compare non builtin types".into());
        };

        let Some(int_size) = builtin_type.get_int_size() else {
            return Err("Tried to compare builtin type without int_size".into());
        };

        instructions.push(PI::Actual(I::AdvanceStackPtr(align_shift)));

        instructions.append(&mut self.generate_expression(env, left, function_info, depth + align_shift)?);
        instructions.append(&mut self.generate_expression(
            env,
            right,
            function_info,
            depth + align_shift + env.types[left_type].size,
        )?);

        instructions.push(PI::Actual(I::IntegerComparisonOperation {
            comparison,
            size: int_size,
            signed: builtin_type.is_signed(),
        }));

        instructions.push(PI::Actual(I::RetractMoving(align_shift, IntSize::OneByte)));

        Ok(instructions)
    }

    pub(super) fn generate_binary_logic_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        left: &ExprAST,
        right: &ExprAST,
        is_and: bool, // controls "and" vs "or"
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        let jump_id = util::next_id();

        instructions.append(&mut self.generate_expression(env, left, function_info, depth)?);

        instructions.push(PI::Actual(I::Duplicate(IntSize::OneByte)));

        if is_and {
            instructions.push(PI::Temp(TempInstruction::JumpIfFalse(jump_id)));
        } else {
            instructions.push(PI::Temp(TempInstruction::JumpIfTrue(jump_id)));
        }

        instructions.push(PI::Actual(I::RetractStackPtr(1))); // Overwrite old bool.
        instructions.append(&mut self.generate_expression(env, right, function_info, depth)?);

        instructions.push(PI::Temp(TempInstruction::JumpFrom(jump_id)));
        // If we jumped, the left value is still there. Otherwise, it was repleaced with the right value.

        Ok(instructions)
    }

    pub(super) fn generate_not_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        inner: &ExprAST,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        instructions.append(&mut self.generate_expression(env, inner, function_info, depth)?);
        instructions.push(PI::Actual(I::BooleanNot));

        Ok(instructions)
    }

    pub(super) fn generate_int_literal_expr(
        env: &CompilationEnvironment,
        num: i128,
        node_data: &ASTNodeData,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let num_type = &env.type_index[&node_data.id];

        let Type::BuiltIn(builtin) = num_type else {
            return Err("Literal has non built in type".into());
        };

        let int_size = builtin.get_int_size().ok_or(GenerateError::from("Literal type did not fit in int"))?;

        let push_constant = |constant| Ok(vec![PI::Actual(I::PushConstant(constant))]);
        if builtin.is_signed() {
            match int_size {
                IntSize::OneByte => push_constant(Constant::OneByte(reinterpret::<i8, u8>(num as i8))),
                IntSize::TwoByte => push_constant(Constant::TwoByte(reinterpret::<i16, u16>(num as i16))),
                IntSize::FourByte => push_constant(Constant::FourByte(reinterpret::<i32, u32>(num as i32))),
                IntSize::EightByte => push_constant(Constant::EightByte(reinterpret::<i64, u64>(num as i64))),
            }
        } else {
            match int_size {
                IntSize::OneByte => push_constant(Constant::OneByte(num as u8)),
                IntSize::TwoByte => push_constant(Constant::TwoByte(num as u16)),
                IntSize::FourByte => push_constant(Constant::FourByte(num as u32)),
                IntSize::EightByte => push_constant(Constant::EightByte(num as u64)),
            }
        }
    }

    pub(super) fn generate_bool_literal_expr(val: bool) -> Vec<PseudoInstruction> {
        let mut instructions = vec![];

        if val {
            instructions.push(PI::Actual(I::PushConstant(Constant::OneByte(1))));
        } else {
            instructions.push(PI::Actual(I::PushConstant(Constant::OneByte(0))));
        }

        instructions
    }

    pub(super) fn generate_variable_expr(
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        name: &str,
    ) -> Vec<PseudoInstruction> {
        let mut instructions = vec![];

        // This is placing a variable's value on the stack. See statement for storing
        // a variable.

        // TODO: Shadowing...

        let (offset, size, val_type) = function_info.variable_info_by_name(name).expect("known to exist");
        instructions.append(&mut Self::generate_read_from_base(*offset, *size, env.types[val_type].alignment));

        instructions
    }

    pub(super) fn generate_block_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        statements: &[StatementAST],
        expr: &Option<Box<ExprAST>>,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        for statement in statements {
            instructions.append(&mut self.generate_statement(env, statement, function_info, depth)?);
        }

        if let Some(expr) = expr {
            instructions.append(&mut self.generate_expression(env, expr, function_info, depth)?);
        }

        Ok(instructions)
    }

    pub(super) fn generate_function_call_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        name: &str,
        subexprs: &[ExprAST],
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        // We assume that the depth is already such that a value from the function
        // Can be aligned. If the alignment is not 8 though, we shift, run the function,
        // and then pull the value back.

        let info = self.functions.get(name).ok_or(GenerateError("Function not found".to_string()))?;

        let align_shift = get_align_shift(depth, 8);

        instructions.push(PI::Actual(I::AdvanceStackPtr(align_shift)));

        let (relative_return_loc, return_size, return_type) =
            info.variables.get(&Variable::Return).expect("Known exists");

        // So relative position is how far below the function we currently are.

        // Skip past the
        instructions.push(PI::Actual(I::AdvanceStackPtr(*return_size)));
        let mut relative_position = relative_return_loc + *return_size as isize;

        if subexprs.len() != info.parameters.len() {
            return Err(format!(
                "Function has wrong number of arguments. {} arguments vs {} parameters",
                subexprs.len(),
                info.parameters.len()
            )
            .into());
        }

        for (expr, param) in subexprs.iter().zip(&info.parameters) {
            let (param_loc, size, _) = info.variables.get(param).expect("Known exists");

            instructions.push(PI::Actual(I::AdvanceStackPtr((param_loc - relative_position) as usize)));
            relative_position = *param_loc;

            instructions.append(&mut self.generate_expression(
                env,
                expr,
                function_info,
                depth + align_shift + (relative_position - relative_return_loc) as usize,
            )?);
            relative_position += *size as isize;
        }

        // Finally align to function call.
        instructions.push(PI::Actual(I::AdvanceStackPtr((-relative_position) as usize)));

        instructions.push(PI::Temp(TempInstruction::Call(name.into())));

        instructions.push(PI::Actual(I::RetractStackPtr((-relative_return_loc) as usize - return_size)));

        // Retract moving to original expression location
        match return_size {
            0 => instructions.push(PI::Actual(I::RetractStackPtr(align_shift))),
            _ => instructions.append(&mut Self::generate_stack_retraction(
                align_shift,
                *return_size,
                env.types[return_type].alignment,
            )),
        }

        Ok(instructions)
    }

    pub(super) fn generate_if_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        condition: &ExprAST,
        block: &ExprAST,
        else_branch: &Option<Box<ExprAST>>,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        match else_branch {
            Some(else_branch) => {
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
            }
            None => {
                let mut condition_instrs = self.generate_expression(env, condition, function_info, depth)?;
                let mut block_instrs = self.generate_expression(env, block, function_info, depth)?;
                let skip_jump_id = util::next_id();

                instructions.append(&mut condition_instrs);
                instructions.push(PI::Temp(TempInstruction::JumpIfFalse(skip_jump_id)));
                instructions.append(&mut block_instrs);

                instructions.push(PI::Temp(TempInstruction::JumpFrom(skip_jump_id)));
            }
        }

        Ok(instructions)
    }

    pub(super) fn generate_while_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        condition: &ExprAST,
        block: &ExprAST,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

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

        Ok(instructions)
    }

    pub(super) fn generate_return_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        expr: &Option<Box<ExprAST>>,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        if let Some(expr) = expr {
            let expr_type = &env.type_index[&expr.get_node_data().id];
            let expr_type_info = env.types.get(expr_type).ok_or(GenerateError("Type not found".to_string()))?;

            let align_shift = get_align_shift(depth, expr_type_info.alignment);

            // Align
            instructions.push(PseudoInstruction::Actual(Instruction::AdvanceStackPtr(align_shift)));

            instructions.append(&mut self.generate_expression(env, expr, function_info, depth + align_shift)?);
        }

        instructions.append(&mut Self::generate_return_handoff(env, function_info)?);

        Ok(instructions)
    }

    pub(super) fn generate_struct_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        name: &str,
        members: &[(String, ExprAST)],
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        let TypeInfo { kind, .. } = env.types.get(&name.to_string().into()).expect("Known exists");

        let KindData::Struct { members: type_members } = kind else { panic!("Expected struct type") };

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

        Ok(instructions)
    }

    pub(super) fn generate_member_access_expr(
        &self,
        env: &CompilationEnvironment,
        function_info: &FunctionInfo,
        depth: usize,
        subtree: &ExprAST,
    ) -> Result<Vec<PseudoInstruction>, GenerateError> {
        let mut instructions = vec![];

        let expr_type = &env.type_index[&subtree.get_node_data().id];
        let TypeInfo { size, alignment, .. } = &env.types[expr_type];

        let loc = self.locate_expr(subtree, env)?.collapse_offsets();

        match loc {
            Location::OffsetFrom(inner, offset) => match inner.borrow() {
                Location::Local(name) => {
                    let (base_offset, _, _) = function_info.variable_info_by_name(name).expect("Variable Exists");
                    instructions.append(&mut Self::generate_read_from_base(base_offset + offset, *size, *alignment));
                }
                Location::EvaluatedExpression(expr) => {
                    let struct_type = &env.type_index[&expr.get_node_data().id];
                    let TypeInfo { size: struct_size, alignment: struct_alignment, .. } = &env.types[struct_type];

                    let align_shift = get_align_shift(depth, *struct_alignment);

                    instructions.push(PI::Actual(I::AdvanceStackPtr(align_shift)));

                    instructions.append(&mut self.generate_expression(
                        env,
                        expr,
                        function_info,
                        depth + align_shift,
                    )?);

                    instructions.push(PI::Actual(I::RetractStackPtr(
                        (*struct_size as isize - offset - *size as isize) as usize,
                    )));

                    instructions.append(&mut Self::generate_stack_retraction(
                        align_shift + offset as usize,
                        *size,
                        *alignment,
                    ));
                }
                _ => return Err("Expected other location type".into()),
            },
            _ => return Err("Expected other location type".into()),
        }

        Ok(instructions)
    }
}
