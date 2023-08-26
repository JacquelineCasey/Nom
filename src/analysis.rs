
use std::collections::HashMap;

use crate::ast::AST;

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

pub struct TypeInfo {
    pub size: usize,  // Number of bytes the types takes on the stack.
    pub alignment: usize,  // In bytes
}


impl AnalyzedAST {
    pub fn new(ast: AST) -> AnalyzedAST {
        let mut analyzed_ast = AnalyzedAST { 
            ast, 
            functions: HashMap::new(),  // Not yet analyzed
            types: get_default_types(),
        };

        analyzed_ast.analyze();

        analyzed_ast
    }

    fn analyze(&mut self) {
        todo!();
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
