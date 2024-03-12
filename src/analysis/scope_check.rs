
use std::collections::HashMap;

use crate::{CompilationEnvironment, error::AnalysisError, ast::{ExprAST, StatementAST, DeclarationAST}};
use super::{Function, types::Type};


// Checks the scope (as well as const-ness) rules, and builds a table of local variables.
pub(crate) fn scope_check(env: &mut CompilationEnvironment, name: &str) -> Result<(), AnalysisError> {
    let function = env.functions.get_mut(name).ok_or(AnalysisError("Could not find function".into()))?;
    let block = std::mem::take(&mut function.ast);
    
    let mut local_types = HashMap::new();
    for (name, param_type) in &function.parameter_types {
        local_types.insert(name.clone(), Some(param_type.clone()));
    }

    scope_check_expression(
        &env.functions,
        &mut local_types, 
        &block
    )?;
    
    // We need the old lifetime to die.
    let function = env.functions.get_mut(name).expect("known exists");
    function.ast = block;
    function.local_types = local_types;

    Ok(())
}

fn scope_check_expression(functions: &HashMap<String, Function>, local_types: &mut HashMap<String, Option<Type>>, expr: &ExprAST) -> Result<(), AnalysisError> {
    match expr {
        ExprAST::Add(left, right, _) 
        | ExprAST::Subtract(left, right, _)
        | ExprAST::Multiply(left, right, _)
        | ExprAST::Divide(left, right, _) 
        | ExprAST::Modulus(left, right, _)
        | ExprAST::Comparison(left, right, _, _)
        | ExprAST::Or(left, right, _)
        | ExprAST::And(left, right, _) => {
            scope_check_expression(functions, local_types, left)?;
            scope_check_expression(functions, local_types, right)?;
        },
        ExprAST::Not(inner, _) => {
            scope_check_expression(functions, local_types, inner)?;
        }
        ExprAST::Block(statements, final_expr, _) => {
            for statement in statements {
                match statement {
                    StatementAST::ExpressionStatement(expr, _) => 
                        scope_check_expression(functions, local_types, expr)?,
                    StatementAST::Assignment(left, right, _) => {
                        scope_check_expression(functions, local_types, left)?;
                        scope_check_expression(functions, local_types, right)?;
                    },
                    StatementAST::Declaration(decl, _) => {
                        match decl {
                            DeclarationAST::Function { .. } => {
                                return Err("Did not expect function declaration".into());
                            }
                            DeclarationAST::Struct { .. } => {
                                return Err("Did not expect struct declaration".into());
                            }
                            DeclarationAST::Variable { name, expr, .. } => {
                                if local_types.contains_key(name) {
                                    return Err("Variable redeclared. Shadowing not yet implemented.".into())
                                }

                                local_types.insert(name.clone(), None);

                                scope_check_expression(functions, local_types, expr)?;
                            }
                        }
                    },
                    StatementAST::CompoundAssignment(..) =>
                        return Err("Expected Compound Assignment to have been desugared".into()),
                }
            }

            if let Some(expr) = final_expr {
                scope_check_expression(functions, local_types, expr)?;
            }
        },
        ExprAST::FunctionCall(name, subexprs, ..) => {
            if !functions.contains_key(name) {
                return Err("Could not find function".into());
            }
            
            for subexpr in subexprs {
                scope_check_expression(functions, local_types, subexpr)?;
            }
        }
        ExprAST::Variable(name, ..) => {
            if !local_types.contains_key(name) {
                return Err(format!("{name} not found in local scope.").into());
            }
        }, 
        ExprAST::IntegerLiteral(..) | ExprAST::BooleanLiteral(..) => (),
        ExprAST::If { condition, block, else_branch, .. } => {
            scope_check_expression(functions, local_types, condition)?;
            scope_check_expression(functions, local_types, block)?;

            if let Some(branch) = else_branch {
                scope_check_expression(functions, local_types, branch)?;
            }
        },
        ExprAST::While { condition, block, .. } => {
            scope_check_expression(functions, local_types, condition)?;
            scope_check_expression(functions, local_types, block)?;
        },
        ExprAST::Return(expr, ..) => {
            if let Some(expr) = expr {
                scope_check_expression(functions, local_types, expr)?;
            }
        },
        ExprAST::Moved => panic!("ExprAST was moved"),
    }

    Ok(())
}