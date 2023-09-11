
pub mod types;


use std::collections::HashMap;

use crate::CompilationEnvironment;
use crate::ast::{ExprAST, DeclarationAST, StatementAST} ;
use crate::error::AnalysisError;

use types::{Type, BuiltIn, TypeInfo};


pub struct Function {
    pub ast: ExprAST,
    pub return_type: Type,
    pub parameter_types: Vec<(String, Type)>,  // Argument order is important, so a Vector is used.
    // Local order *kinda* doesn't matter, so we have a hash map
    // None means the type has not yet been decided.
    pub local_types: HashMap<String, Option<Type>>,  
    pub scope: HashMap<String, bool>,  // Temporary - the bool being true means mutable (aka `var`).
}

impl Function {
    pub(super) fn new(_env: &CompilationEnvironment, ast: ExprAST, 
        params: Vec<(String, String)>, return_type: String) -> Function { // Could become Result

        // TODO: Someday we might want this to add type generation requests to _env
        
        let parameter_types = params.into_iter()
            .map(|(name, type_name)| (name, type_name.into()))
            .collect();
        Function { 
            ast, 
            return_type: return_type.into(), 
            parameter_types, 
            local_types: HashMap::new(), 
            scope: HashMap::new(), 
        }
    }
}

// Checks the scope (as well as const-ness) rules, and builds a table of local variables.
pub(super) fn scope_check(env: &mut CompilationEnvironment, name: &str) -> Result<(), AnalysisError> {
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
        | ExprAST::Divide(left, right, _) => {
            scope_check_expression(functions, local_types, left)?;
            scope_check_expression(functions, local_types, right)?;
        }
        ExprAST::Block(statements, final_expr, _) => {
            for statement in statements {
                match statement {
                    crate::ast::StatementAST::ExpressionStatement(expr, _) => scope_check_expression(functions, local_types, expr)?,
                    crate::ast::StatementAST::Assignment(left, right, _) => {
                        scope_check_expression(functions, local_types, left)?;
                        scope_check_expression(functions, local_types, right)?;
                    }
                    crate::ast::StatementAST::Declaration(decl, _) => {
                        match decl {
                            DeclarationAST::Function { .. } => {
                                return Err("Did not expect function".into());
                            }
                            DeclarationAST::Variable { name, expr, .. } => {
                                if local_types.contains_key(name) {
                                    return Err("Variable redeclared. Shadowing not yet implemented.".into())
                                }

                                local_types.insert(name.clone(), None);

                                scope_check_expression(functions, local_types, expr)?;
                            }
                        }
                    }
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
        ExprAST::IntegerLiteral(..) => (),
        ExprAST::Variable(name, ..) => {
            if !local_types.contains_key(name) {
                return Err(format!("{name} not found in local scope.").into());
            }
        },       
        ExprAST::Moved => panic!("ExprAST was moved"),     
    }

    Ok(())
}

pub(super) fn type_check(env: &mut CompilationEnvironment, name: &str) -> Result<(), AnalysisError> {
    let function = env.functions.get_mut(name).ok_or(AnalysisError("Could not find function".into()))?;
    let mut block = std::mem::take(&mut function.ast);
    let return_type = function.return_type.clone();

    type_check_expression(env, &mut block, name, &Some(return_type))?;

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
fn type_check_expression(env: &mut CompilationEnvironment, expr: &mut ExprAST, function_name: &str, expected: &Option<Type>) -> Result<Type, AnalysisError> {
    // TODO: Conversions!

    let expr_type = match expr {
        ExprAST::Add(left, right, _)
        | ExprAST::Subtract(left, right, _)
        | ExprAST::Multiply(left, right, _)
        | ExprAST::Divide(left, right, _) => {
            let left_type = type_check_expression(env, left, function_name, expected)?;
            let right_type = type_check_expression(env, right, function_name, expected)?;

            if left_type != right_type {
                return Err("Types don't match".into())
            }

            left_type
        },
        ExprAST::Block(statements, final_expr, _) => {
            for stmt in statements {
                match stmt {
                    StatementAST::Assignment(left, right, _) => {
                        let left_type = type_check_expression(env, left, function_name, &None)?;
                        type_check_expression(env, right, function_name, &Some(left_type.clone()))?;
                    },
                    StatementAST::ExpressionStatement(expr, _) => {
                        type_check_expression(env, expr, function_name, &None)?;
                    }
                    StatementAST::Declaration(DeclarationAST::Variable { expr, name, type_ascription, .. }, _) => {

                        let var_type: Type = type_ascription.clone()
                            .ok_or(AnalysisError::from("Type inference not yet supported"))?
                            .into();

                        env.functions.get_mut(function_name).expect("known").local_types.insert(name.clone(), Some(var_type.clone()));

                        let expr_type = type_check_expression(env, expr, function_name, &Some(var_type.clone()))?;
                        
                        if expr_type != var_type {
                            return Err("Type checking failed at variable declaration".into())
                        }
                    }
                    StatementAST::Declaration(DeclarationAST::Function { .. }, _) => 
                        return Err("Can not process function definition here".into()),
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
                    else {
                        inner_type.clone()
                    }
                },
                None => {
                    if !integer_literal_fits(*literal, &Type::BuiltIn(BuiltIn::I32)) {
                        return Err("Literal does not fit in i32. i32 was chosen because type of literal was unknown. Type inference needs some help".into())
                    }
                    else {
                        Type::BuiltIn(BuiltIn::I32)
                    }
                }
            }
        },
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
        ExprAST::Moved => panic!("ExprAST moved"),
    };

    if let Some(inner) = expected {
        if *inner != expr_type {
            return Err("Type did not matched expected".into());
        }
    }

    env.type_index.insert(expr.get_node_data().id, expr_type.clone());
    Ok(expr_type)
}

fn integer_literal_fits(literal: i128, expected: &Type) -> bool {
    // TODO!
    true
}

pub fn get_default_types() -> HashMap<Type, TypeInfo> {
    let mut map = HashMap::new();

    map.insert(Type::BuiltIn(BuiltIn::U8) , TypeInfo { size: 1, alignment: 1 });
    map.insert(Type::BuiltIn(BuiltIn::U16), TypeInfo { size: 2, alignment: 2 });
    map.insert(Type::BuiltIn(BuiltIn::U32), TypeInfo { size: 4, alignment: 4 });
    map.insert(Type::BuiltIn(BuiltIn::U64), TypeInfo { size: 8, alignment: 8 });
    map.insert(Type::BuiltIn(BuiltIn::I8) , TypeInfo { size: 1, alignment: 1 });
    map.insert(Type::BuiltIn(BuiltIn::I16), TypeInfo { size: 2, alignment: 2 });
    map.insert(Type::BuiltIn(BuiltIn::I32), TypeInfo { size: 4, alignment: 4 });
    map.insert(Type::BuiltIn(BuiltIn::I64), TypeInfo { size: 8, alignment: 8 });

    map.insert(Type::BuiltIn(BuiltIn::Unit), TypeInfo { size: 0, alignment: 1 });  // Not sure if this should have an alignment

    map
}
