
use std::collections::HashMap;

use crate::{ast::{AST, ExprAST, DeclarationAST}, instructions::IntSize};


#[derive(Debug)]
pub struct AnalysisError (pub String);

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
                DeclarationAST::Function { name, block, .. } => {
                    self.functions.insert(name.clone(), Self::determine_function_info(block)?);
                }
                DeclarationAST::Variable { .. } => {
                    return Err(AnalysisError("Top level variable not yet supported.".to_string()));
                }
            }
        }

        Ok(())
    }

    fn determine_function_info(block_expr: &ExprAST) -> Result<FunctionTypeInfo, AnalysisError> {
        let mut local_types = HashMap::new();
        
        match block_expr {
            ExprAST::Block(_, final_expr, _ ) => {
                Self::analyze_expression(&mut local_types, block_expr)?;
                Ok(FunctionTypeInfo { 
                    return_type: match final_expr {
                        Some(_expr) => Type::BuiltIn(BuiltIn::I32),  // TODO: Get type of expression
                        None => Type::BuiltIn(BuiltIn::Unit)
                    },
                    parameter_types: vec![],
                    local_types
                })
            },
            _ => Err(AnalysisError("Expected Block".to_string()))
        }
    }

    fn analyze_expression(local_types: &mut HashMap<String, Type>, expr: &ExprAST) -> Result<(), AnalysisError> {
        match expr {
            ExprAST::Add(left, right, _) 
            | ExprAST::Subtract(left, right, _)
            | ExprAST::Multiply(left, right, _)
            | ExprAST::Divide(left, right, _) => {
                Self::analyze_expression(local_types, left)?;
                Self::analyze_expression(local_types, right)?;
            }
            ExprAST::Block(statements, final_expr, _) => {
                for statement in statements {
                    match statement {
                        crate::ast::StatementAST::ExpressionStatement(expr, _) => Self::analyze_expression(local_types, expr)?,
                        crate::ast::StatementAST::Assignment(left, right, _) => {
                            Self::analyze_expression(local_types, left)?;
                            Self::analyze_expression(local_types, right)?;
                        }
                        crate::ast::StatementAST::Declaration(decl, _) => {
                            match decl {
                                DeclarationAST::Function { .. } => {
                                    return Err(AnalysisError("Did not expect function".to_string()));
                                }
                                DeclarationAST::Variable { name, expr, .. } => {
                                    local_types.insert(name.clone(), Type::BuiltIn(BuiltIn::I32));

                                    Self::analyze_expression(local_types, expr)?;
                                }
                            }
                        }
                    }
                }

                if let Some(expr) = final_expr {
                    Self::analyze_expression(local_types, expr)?;
                }
            },
            ExprAST::FunctionCall(name, subexprs, ..) => todo!(),
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
