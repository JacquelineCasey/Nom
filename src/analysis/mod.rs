
use std::collections::HashMap;

use crate::ast::{AST, ExprAST, DeclarationAST} ;
use crate::instructions::IntSize;
use crate::error::AnalysisError;


pub struct AnalyzedAST {
    pub ast: AST,
    pub functions: HashMap<String, FunctionTypeInfo>,
    pub types: HashMap<Type, TypeInfo>,
}

pub struct FunctionTypeInfo {
    pub return_type: Type,
    pub parameter_types: Vec<(String, Type)>,  // Argument order is important, so a Vector is used.
    pub local_types: HashMap<String, Type>,  // Local order *kinda* doesn't matter, so we 
}

#[derive(PartialEq, Eq, Hash)]
pub enum Type {
    BuiltIn (BuiltIn),
    
    #[allow(dead_code)]
    NotYetImplemented,
}

#[derive(PartialEq, Eq, Hash)]
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

#[derive(Debug)]
pub struct TypeInfo {
    pub size: usize,  // Number of bytes the types takes on the stack.
    pub alignment: usize,  // In bytes
}


impl AnalyzedAST {
    pub fn new(ast: AST) -> Result<AnalyzedAST, AnalysisError>  {
        let mut analyzed_ast = AnalyzedAST { 
            ast, 
            functions: HashMap::new(),  // Not yet analyzed
            types: get_default_types(),
        };

        analyzed_ast.analyze()?;

        Ok(analyzed_ast)
    }

    fn analyze(&mut self) -> Result<(), AnalysisError> {
        for decl in &self.ast.declarations {
            match decl {
                DeclarationAST::Function { name, .. } => {
                    self.functions.insert(name.clone(), Self::determine_function_info(decl)?);
                }
                DeclarationAST::Variable { .. } => {
                    return Err("Top level variable not yet supported.".into());
                }
            }
        }

        for decl in &self.ast.declarations {
            match decl {
                DeclarationAST::Function { name, block, .. } => {
                    let mut local_types = HashMap::new();

                    Self::analyze_expression(
                        &self.functions,
                        &mut local_types, 
                        block
                    )?;
                    
                    self.functions.get_mut(name).expect("Known exists").local_types = local_types;
                }
                DeclarationAST::Variable { .. } => {
                    return Err("Top level variable not yet supported.".into());
                }
            }
        }

        Ok(())
    }

    // Deterines parameter types and return type, but does not yet resolve local
    // types.
    fn determine_function_info(decl: &DeclarationAST) -> Result<FunctionTypeInfo, AnalysisError> {
        match decl {
            DeclarationAST::Function { params, block: ExprAST::Block(_, final_expr, _ ), .. } => {
                Ok(FunctionTypeInfo { 
                    return_type: match final_expr {
                        Some(_expr) => Type::BuiltIn(BuiltIn::I32),  // TODO: Get type of expression
                        None => Type::BuiltIn(BuiltIn::Unit)
                    },
                    parameter_types: params.iter()
                        .map(|param| (param.clone(), Type::BuiltIn(BuiltIn::I32)))  // TODO: Get type of parameter.
                        .collect(),
                    local_types: HashMap::new()
                })
            }
            DeclarationAST::Function { .. } => {
                Err("Function did not have block".into())
            }
            DeclarationAST::Variable { .. } => {
                Err("Expected function".into())
            }
        }        
    }

    fn analyze_expression(functions: &HashMap<String, FunctionTypeInfo>, local_types: &mut HashMap<String, Type>, expr: &ExprAST) -> Result<(), AnalysisError> {
        match expr {
            ExprAST::Add(left, right, _) 
            | ExprAST::Subtract(left, right, _)
            | ExprAST::Multiply(left, right, _)
            | ExprAST::Divide(left, right, _) => {
                Self::analyze_expression(functions, local_types, left)?;
                Self::analyze_expression(functions, local_types, right)?;
            }
            ExprAST::Block(statements, final_expr, _) => {
                for statement in statements {
                    match statement {
                        crate::ast::StatementAST::ExpressionStatement(expr, _) => Self::analyze_expression(functions, local_types, expr)?,
                        crate::ast::StatementAST::Assignment(left, right, _) => {
                            Self::analyze_expression(functions, local_types, left)?;
                            Self::analyze_expression(functions, local_types, right)?;
                        }
                        crate::ast::StatementAST::Declaration(decl, _) => {
                            match decl {
                                DeclarationAST::Function { .. } => {
                                    return Err("Did not expect function".into());
                                }
                                DeclarationAST::Variable { name, expr, .. } => {
                                    local_types.insert(name.clone(), Type::BuiltIn(BuiltIn::I32));

                                    Self::analyze_expression(functions, local_types, expr)?;
                                }
                            }
                        }
                    }
                }

                if let Some(expr) = final_expr {
                    Self::analyze_expression(functions, local_types, expr)?;
                }
            },
            ExprAST::FunctionCall(name, subexprs, ..) => {
                if !functions.contains_key(name) {
                    return Err("Could not find function".into());
                }
                
                for subexpr in subexprs {
                    Self::analyze_expression(functions, local_types, subexpr)?;
                }
            }
            ExprAST::Literal(..) => (),
            ExprAST::Variable(..) => (),            
        }

        Ok(())
    }

    #[allow(clippy::unused_self)]
    pub fn get_expr_type(&self, subtree: &ExprAST) -> Type {
        // Very very preliminary and naive

        match subtree {
            ExprAST::Block(_, None, _) => Type::BuiltIn(BuiltIn::Unit),
            _ => Type::BuiltIn(BuiltIn::I32)
        }
    }
}

fn get_default_types() -> HashMap<Type, TypeInfo> {
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
