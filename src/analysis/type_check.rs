
use crate::{CompilationEnvironment, ast::StatementAST};
use crate::ast::{ExprAST, DeclarationAST};
use crate::error::AnalysisError;

use super::types::{PartialType, Type, upper_bound_type, BuiltIn};


pub(crate) fn type_check(env: &mut CompilationEnvironment, name: &str) -> Result<(), AnalysisError> {
    let function = env.functions.get_mut(name).ok_or(AnalysisError("Could not find function".into()))?;
    let mut block = std::mem::take(&mut function.ast);
    let return_type = function.return_type.clone();

    type_check_expression(env, &mut block, name, &Some(return_type))?;
    finalize_partial_types_expr(env, &mut block, name)?;

    // We need the old lifetime to die.
    let function = env.functions.get_mut(name).expect("known exists");
    function.ast = block;
    Ok(())
}

// Resolves the types of all expressions in the function. May decide the types of some
// expressions if they are ambiguous. May add conversion nodes to the AST.
//
// An expected type can be passed if the expression has known type. If this is done,
// then the function will error if the resolved type does not match the expected.
// The expected type may be used to decide certain ambiguous expressions, such as
// literals. If the type is not provided, it will be decided (i.e. literals will be
// assumed to be i32, etc.)
#[allow(clippy::too_many_lines)]
fn type_check_expression(env: &mut CompilationEnvironment, expr: &mut ExprAST, function_name: &str, expected: &Option<Type>) -> Result<Type, AnalysisError> {
    // TODO: Conversions!

    let expr_type = match expr {
        ExprAST::Add(left, right, _)
        | ExprAST::Subtract(left, right, _)
        | ExprAST::Multiply(left, right, _)
        | ExprAST::Divide(left, right, _)
        | ExprAST::Modulus(left, right, _) => {
            let left_type = type_check_expression(env, left, function_name, expected)?;
            let right_type = type_check_expression(env, right, function_name, expected)?;

            if left_type != right_type {
                if left_type == Type::PartiallyKnown(PartialType::IntLiteral) {
                    type_check_expression(env, left, function_name, &Some(right_type))?;
                }
                else if right_type == Type::PartiallyKnown(PartialType::IntLiteral) {
                    type_check_expression(env, right, function_name, &Some(left_type.clone()))?;
                }
                else {
                    return Err("Types don't match".into());
                }
            }

            left_type
        },
        ExprAST::Comparison(left, right, _, _) => {
            let left_type = type_check_expression(env, left, function_name, &None)?;
            let right_type = type_check_expression(env, right, function_name, &None)?;

            if left_type != right_type {
                if left_type == Type::PartiallyKnown(PartialType::IntLiteral) {
                    type_check_expression(env, left, function_name, &Some(right_type))?;
                }
                else if right_type == Type::PartiallyKnown(PartialType::IntLiteral) {
                    type_check_expression(env, right, function_name, &Some(left_type))?;
                }
                else {
                    let Some(bound) = upper_bound_type(&left_type, &right_type)
                        else { return Err("Types don't match".into()); };

                    // TODO: This repeat definitely could cause some efficiency issues. 
                    // We need a smarter unification algorithm perhaps...

                    type_check_expression(env, left, function_name, &Some(bound.clone()))?;
                    type_check_expression(env, right, function_name, &Some(bound))?;
                }
            }

            Type::BuiltIn(BuiltIn::Boolean)
        },
        ExprAST::And(left, right, _) | ExprAST::Or(left, right, _) => {
            type_check_expression(env, left, function_name, &Some(Type::BuiltIn(BuiltIn::Boolean)))?;
            type_check_expression(env, right, function_name, &Some(Type::BuiltIn(BuiltIn::Boolean)))?;

            Type::BuiltIn(BuiltIn::Boolean)
        },
        ExprAST::Not(inner, _) => {
            type_check_expression(env, inner, function_name, &Some(Type::BuiltIn(BuiltIn::Boolean)))?;

            Type::BuiltIn(BuiltIn::Boolean)
        },
        ExprAST::Block(statements, final_expr, _) => {
            for stmt in statements {
                match stmt {
                    StatementAST::Assignment(left, right, _) => {
                        let left_type = type_check_expression(env, left, function_name, &None)?;
                        type_check_expression(env, right, function_name, &Some(left_type.clone()))?;
                    },
                    StatementAST::CompoundAssignment(..) =>
                        return Err("Expected Compound Assignment to have been desugared".into()),
                    StatementAST::ExpressionStatement(expr, _) => {
                        type_check_expression(env, expr, function_name, &None)?;
                    }
                    StatementAST::Declaration(DeclarationAST::Variable { expr, name, type_ascription, .. }, _) => {

                        let var_type: Type = type_ascription.clone()
                            .ok_or(AnalysisError::from(format!("Type inference not yet supported - give {name} (in {function_name}) an explicit type.")))?
                            .into();

                        env.functions.get_mut(function_name).expect("known").local_types.insert(name.clone(), Some(var_type.clone()));

                        let expr_type = type_check_expression(env, expr, function_name, &Some(var_type.clone()))?;
                        
                        if expr_type != var_type {
                            return Err("Type checking failed at variable declaration".into())
                        }
                    }
                    StatementAST::Declaration(DeclarationAST::Function { .. }, _) => 
                        return Err("Can not process function declaration here".into()),
                    StatementAST::Declaration(DeclarationAST::Struct { .. }, _) => 
                        return Err("Can not process struct declaration here".into()),
                    
                }
            }
            if let Some(expr) = final_expr {
                type_check_expression(env, expr, function_name, expected)?
            }
            else {
                Type::BuiltIn(BuiltIn::Unit)
            }
        },
        ExprAST::FunctionCall(name, exprs, _) => {
            let func = env.functions.get(name).ok_or(AnalysisError::from("Could not lookup function"))?;
            let return_type = func.return_type.clone();

            for (expr, (_, expected_type)) in exprs.iter_mut().zip(func.parameter_types.clone()) {
                type_check_expression(env, expr, function_name, &Some(expected_type))?;
            }

            return_type            
        },
        ExprAST::IntegerLiteral(literal, _) => {
            match expected {
                Some(inner_type) => {
                    if !integer_literal_fits(*literal, inner_type) {
                        return Err("Literal does not fit in type".into())
                    }
                    
                    inner_type.clone()
                },
                None => {
                    if !integer_literal_fits(*literal, &Type::BuiltIn(BuiltIn::I32)) {
                        return Err("Literal does not fit in i32. i32 was chosen because type of literal was unknown. Type inference needs some help".into())
                    }

                    Type::PartiallyKnown(PartialType::IntLiteral)
                }
            }
        },
        ExprAST::BooleanLiteral(..) => {
            Type::BuiltIn(BuiltIn::Boolean)
        }
        ExprAST::Variable(name, _) => {
            if let Some(inner) = env.functions[function_name].local_types.get(name) {
                inner.clone().ok_or(AnalysisError::from("Variable lookup succeeded, but had unknown_type"))?
            } 
            else if let Some((_, inner)) = env.functions[function_name].parameter_types.iter()
                .find(|(p_name, _p_type)| p_name == name) {
                inner.clone()
            }
            else {
                return Err("AKJSnagkj".into())
            }
    
        }
        ExprAST::If { condition, block, else_branch: None, .. }
        | ExprAST::While { condition, block, .. } => {
            type_check_expression(env, condition, function_name, &Some(Type::BuiltIn(BuiltIn::Boolean)))?;
            type_check_expression(env, block, function_name, &Some(Type::BuiltIn(BuiltIn::Unit)))?
        }
        ExprAST::If { condition, block, else_branch: Some(else_branch), .. } => {
            type_check_expression(env, condition, function_name, &Some(Type::BuiltIn(BuiltIn::Boolean)))?;
            let if_type = type_check_expression(env, block, function_name, expected)?;
            let else_type = type_check_expression(env, else_branch, function_name, expected)?;

            // Note: Applies only if expected was None.
            if if_type != else_type {
                if if_type == Type::PartiallyKnown(PartialType::IntLiteral) {
                    type_check_expression(env, block, function_name, &Some(else_type))?
                }
                else if else_type == Type::PartiallyKnown(PartialType::IntLiteral) {
                    type_check_expression(env, else_branch, function_name, &Some(if_type))?
                }
                else {
                    let Some(bound) = upper_bound_type(&if_type, &else_type)
                        else { return Err("Types don't match".into()); };
                    
                    // TODO: This repeat definitely could cause some efficiency issues. 
                    // We need a smarter unification algorithm perhaps...

                    type_check_expression(env, block, function_name, &Some(bound.clone()))?;
                    type_check_expression(env, else_branch, function_name, &Some(bound))?
                }
            }
            else {
                if_type
            }
        },
        ExprAST::Return(expr, _) => {
            let return_type = env.functions.get(function_name).expect("Function exists").return_type.clone();

            if let Some(inner) = expr {
                type_check_expression(env, inner, function_name, &Some(return_type))?;
            }
            else if return_type != Type::BuiltIn(BuiltIn::Unit) {
                return Err("Expected unit type".into())
            }

            Type::BuiltIn(BuiltIn::Bottom)
        },
        ExprAST::Moved => panic!("ExprAST moved"),
    };

    if expr_type != Type::BuiltIn(BuiltIn::Bottom) {
        if let Some(inner) = expected {
            if *inner != expr_type {
                return Err("Type did not matched expected".into());
            }
        }
    }


    env.type_index.insert(expr.get_node_data().id, expr_type.clone());
    Ok(expr_type)
}

// Converts partial types to final types
#[allow(clippy::only_used_in_recursion)]
fn finalize_partial_types_expr(env: &mut CompilationEnvironment, expr: &mut ExprAST, func_name: &str) -> Result<(), AnalysisError> {
    let found_type = &env.type_index[&expr.get_node_data().id];

    if let Type::PartiallyKnown(PartialType::IntLiteral) = found_type {
        env.type_index.insert(expr.get_node_data().id, Type::BuiltIn(BuiltIn::I32));
    }

    // Refactor... some function like all_child_expr...
    match expr {
        ExprAST::Add(a, b, _)
        | ExprAST::Subtract(a, b, _)
        | ExprAST::Multiply(a, b, _)
        | ExprAST::Divide(a, b, _)
        | ExprAST::Modulus(a, b, _)
        | ExprAST::Comparison(a, b, _, _)
        | ExprAST::Or(a, b, _)
        | ExprAST::And(a, b, _)
        | ExprAST::While { condition: a, block: b, .. } => {
            finalize_partial_types_expr(env, a, func_name)?;
            finalize_partial_types_expr(env, b, func_name)?;
        },
        ExprAST::If { condition, block, else_branch, .. } => {
            finalize_partial_types_expr(env, condition, func_name)?;
            finalize_partial_types_expr(env, block, func_name)?;
            if let Some(else_branch) = else_branch {
                finalize_partial_types_expr(env, else_branch, func_name)?;
            }
        },
        ExprAST::Not(a, _) => {
            finalize_partial_types_expr(env, a, func_name)?;
        },
        ExprAST::Block(statements, final_expr, _) => {
            for stmt in statements {
                match stmt {
                    StatementAST::ExpressionStatement(e, _) => {
                        finalize_partial_types_expr(env, e, func_name)?;
                    },
                    StatementAST::Assignment(a, b, _) => {
                        finalize_partial_types_expr(env, a, func_name)?;
                        finalize_partial_types_expr(env, b, func_name)?;
                    },
                    StatementAST::CompoundAssignment(..) =>
                        return Err("Expected Compound Assignment to have been desugared".into()),
                    StatementAST::Declaration(DeclarationAST::Function { .. }, _) => {
                        panic!("Cannot yet handle functions in functions");
                    }
                    StatementAST::Declaration(DeclarationAST::Struct { .. }, _) => {
                        panic!("Cannot yet handle types in functions");
                    }
                    StatementAST::Declaration(DeclarationAST::Variable { expr, ..  }, _) => {
                        // TODO - type inference here?, use func_name field
                        finalize_partial_types_expr(env, expr, func_name)?;
                    }
                }
            }

            if let Some(e) =  final_expr {
                finalize_partial_types_expr(env, e, func_name)?;
            }
        },
        ExprAST::FunctionCall(_, exprs, _) => {
            for e in exprs {
                finalize_partial_types_expr(env, e, func_name)?;
            }
        },
        ExprAST::Return(expr, _) => {
            if let Some(expr) = expr {
                finalize_partial_types_expr(env, expr, func_name)?;
            }
        },
        ExprAST::IntegerLiteral(_, _)
        | ExprAST::BooleanLiteral(_, _)
        | ExprAST::Variable(_, _) => 
            (),
        ExprAST::Moved => panic!("AST moved"),
    }

    Ok(())
}


fn integer_literal_fits(_literal: i128, _expected: &Type) -> bool {
    // TODO!
    true
}
