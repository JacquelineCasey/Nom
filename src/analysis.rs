
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
        use BuiltIn::*;
       
        matches!(self, I8 | I16 | I32 | I64)
    }

    pub fn is_unsigned(&self) -> bool {
        use BuiltIn::*;
       
        matches!(self, U8 | U16 | U32 | U64)
    }

    pub fn get_int_size(&self) -> Option<IntSize> {
        use BuiltIn::*;

        match self {
            U8 | I8 => Some(IntSize::OneByte),
            U16 | I16 => Some(IntSize::TwoByte),
            U32 | I32 => Some(IntSize::FourByte),
            U64 | I64 => Some(IntSize::EightByte),
            _ => None
        }
    }
}

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
        let mut function_info = FunctionTypeInfo { 
            return_type: Type::BuiltIn(BuiltIn::I32), 
            parameter_types: vec![],
            local_types: HashMap::new()
        };
        
        match block_expr {
            ExprAST::Block( .. ) => {
                Self::analyze_expression(&mut function_info, block_expr)?;
            },
            _ => return Err(AnalysisError("Expected Block".to_string()))
        }

        Ok(function_info)
    }

    fn analyze_expression(function_info: &mut FunctionTypeInfo, expr: &ExprAST) -> Result<(), AnalysisError> {
        match expr {
            ExprAST::Add(left, right, _) 
            | ExprAST::Subtract(left, right, _)
            | ExprAST::Multiply(left, right, _)
            | ExprAST::Divide(left, right, _) => {
                Self::analyze_expression(function_info, left)?;
                Self::analyze_expression(function_info, right)?;
            }
            ExprAST::Block(statements, final_expr, _) => {
                for statement in statements {
                    match statement {
                        crate::ast::StatementAST::ExpressionStatement(expr, _) => Self::analyze_expression(function_info, expr)?,
                        crate::ast::StatementAST::Assignment(left, right, _) => {
                            Self::analyze_expression(function_info, left)?;
                            Self::analyze_expression(function_info, right)?;
                        }
                        crate::ast::StatementAST::Declaration(decl, _) => {
                            match decl {
                                DeclarationAST::Function { .. } => {
                                    return Err(AnalysisError("Did not expect function".to_string()));
                                }
                                DeclarationAST::Variable { name, expr, .. } => {
                                    function_info.local_types.insert(name.clone(), Type::BuiltIn(BuiltIn::I32));

                                    Self::analyze_expression(function_info, expr)?;
                                }
                            }
                        }
                    }
                }

                if let Some(expr) = final_expr {
                    Self::analyze_expression(function_info, expr)?;
                }
            }
            _ => (),
        }

        Ok(())
    }

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
