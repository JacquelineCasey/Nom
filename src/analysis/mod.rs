
use std::collections::HashMap;

use crate::CompilationEnvironment;
use crate::ast::{ExprAST, DeclarationAST, StatementAST} ;
use crate::instructions::IntSize;
use crate::error::AnalysisError;


pub struct Function {
    pub ast: ExprAST,
    pub return_type: Type,
    pub parameter_types: Vec<(String, Type)>,  // Argument order is important, so a Vector is used.
    pub local_types: HashMap<String, Type>,  // Local order *kinda* doesn't matter, so we have a hash map
    pub scope: HashMap<String, bool>,  // Temporary - the bool being true means mutable (aka `var`).
}

impl Function {
    pub(super) fn new(env: &CompilationEnvironment, ast: ExprAST, 
        params: Vec<(String, String)>, return_type: String) -> Result<Function, AnalysisError> {
        
        let parameter_types = params.into_iter()
            .map(|(name, type_name)| (name, type_name.into()))
            .collect();
        Ok(Function { 
            ast, 
            return_type: return_type.into(), 
            parameter_types, 
            local_types: HashMap::new(), 
            scope: HashMap::new(), 
        })
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Type {
    BuiltIn (BuiltIn),
    
    #[allow(dead_code)]
    NotYetImplemented,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum BuiltIn {
    U8,
    U16,
    U32,
    U64, 
    I8,
    I16,
    I32, 
    I64,
    Unit,
}

impl BuiltIn {
    pub fn is_signed(&self) -> bool {
        use BuiltIn as B;
       
        matches!(self, B::I8 | B::I16 | B::I32 | B::I64)
    }

    pub fn is_unsigned(&self) -> bool {
        use BuiltIn as B;
       
        matches!(self, B::U8 | B::U16 | B::U32 | B::U64)
    }

    pub fn get_int_size(&self) -> Option<IntSize> {
        use BuiltIn as B;
        use IntSize as IS;

        match self {
            B::U8 | B::I8 => Some(IS::OneByte),
            B::U16 | B::I16 => Some(IS::TwoByte),
            B::U32 | B::I32 => Some(IS::FourByte),
            B::U64 | B::I64 => Some(IS::EightByte),
            _ => None
        }
    }
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match &value[..] {
            "i8" => Type::BuiltIn(BuiltIn::I8),
            "i16" => Type::BuiltIn(BuiltIn::I16),
            "i32" => Type::BuiltIn(BuiltIn::I32),
            "i64" => Type::BuiltIn(BuiltIn::I64),
            "u8" => Type::BuiltIn(BuiltIn::U8),
            "u16" => Type::BuiltIn(BuiltIn::U16),
            "u32" => Type::BuiltIn(BuiltIn::U32),
            "u64" => Type::BuiltIn(BuiltIn::U64),
            "unit" => Type::BuiltIn(BuiltIn::Unit),
            _ => panic!("User defined type not yet implemented"),
        }
    }
}

#[derive(Debug)]
pub struct TypeInfo {
    pub size: usize,  // Number of bytes the types takes on the stack.
    pub alignment: usize,  // In bytes
}

pub(super) fn scope_check(env: &mut CompilationEnvironment, name: &str) -> Result<(), AnalysisError> {
    let function = env.functions.get_mut(name).ok_or(AnalysisError("Could not find function".into()))?;
    let block = std::mem::take(&mut function.ast);
    
    let mut local_types = HashMap::new();

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

fn scope_check_expression(functions: &HashMap<String, Function>, local_types: &mut HashMap<String, Type>, expr: &ExprAST) -> Result<(), AnalysisError> {
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
                                local_types.insert(name.clone(), Type::BuiltIn(BuiltIn::I32));

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
        ExprAST::Literal(..)
        | ExprAST::Variable(..) => (),       
        ExprAST::Moved => panic!("ExprAST was moved"),     
    }

    Ok(())
}

pub(super) fn type_check(env: &mut CompilationEnvironment, name: &str) -> Result<(), AnalysisError> {
    let function = env.functions.get_mut(name).ok_or(AnalysisError("Could not find function".into()))?;
    let block = std::mem::take(&mut function.ast);

    type_check_expression(env, &block)?;

    // We need the old lifetime to die.
    let function = env.functions.get_mut(name).expect("known exists");
    function.ast = block;
    Ok(())
}

fn type_check_expression(env: &mut CompilationEnvironment, expr: &ExprAST) -> Result<(), AnalysisError> {
    // Very very preliminary
    env.type_index.insert(expr.get_node_data().id, get_expr_type(env, expr));

    match expr {
        ExprAST::Add(left, right, _)
        | ExprAST::Subtract(left, right, _)
        | ExprAST::Multiply(left, right, _)
        | ExprAST::Divide(left, right, _) => {
            type_check_expression(env, left)?;
            type_check_expression(env, right)?;
        },
        ExprAST::Block(statements, final_expr, _) => {
            for stmt in statements {
                match stmt {
                    StatementAST::Assignment(left, right, _) => {
                        type_check_expression(env, left)?;
                        type_check_expression(env, right)?;
                    },
                    StatementAST::ExpressionStatement(expr, _)
                    | StatementAST::Declaration(DeclarationAST::Variable { expr, .. }, _) => 
                        type_check_expression(env, expr)?,
                    StatementAST::Declaration(DeclarationAST::Function { .. }, _) => 
                        return Err("Can not process function definition here".into()),
                }
            }
            if let Some(expr) = final_expr {
                type_check_expression(env, expr)?;
            }
        },
        ExprAST::FunctionCall(_, exprs, _) => {
            for expr in exprs {
                type_check_expression(env, expr)?;
            }
        },
        _ => (),
    }

    Ok(())
}


fn get_expr_type(env: &CompilationEnvironment, subtree: &ExprAST) -> Type {
    // Very very preliminary and naive

    match subtree {
        ExprAST::FunctionCall(name, ..) => env.functions.get(name).expect("Name found").return_type.clone(),
        ExprAST::Block(_, None, _) => Type::BuiltIn(BuiltIn::Unit),
        _ => Type::BuiltIn(BuiltIn::I32)
    }
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
