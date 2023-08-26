

use std::collections::HashMap;

use crate::ast::{AST, DeclarationAST, ExprAST, StatementAST};
use crate::analysis::AnalyzedAST;
use crate::instructions::Instruction;


#[derive(Debug)]
pub struct GenerateError (String);


pub struct CodeGenerator {
    functions: HashMap<String, FunctionInfo>,
}

enum PseudoInstruction {
    Actual (Instruction),
    Temp (TempInstruction)
}

enum TempInstruction {
    Call (String),  // Call a function by name (we don't yet know its index).
}


impl CodeGenerator {
    pub fn generate(mut self, analyzed_ast: &AnalyzedAST) -> Result<Vec<Instruction>, GenerateError> {
        // Preprocess step: Determine the local variable storage locations
        for decl in &analyzed_ast.ast.declarations {
            match decl {
                DeclarationAST::Function { name, .. } => {
                    self.functions.insert(name.clone(), FunctionInfo::new(analyzed_ast, name.clone())?);
                }
                DeclarationAST::Variable { .. } => {
                    return Err(GenerateError("Cannot yet handle global variables".to_string()));
                }
            }
        }

        // Process functions and generate code

        todo!()
    }
}

// Information associated with each function. This is a working copy, so many of 
// the fields are optional.
struct FunctionInfo {
    variables: HashMap<Variable, (isize, usize)>, // maps parameters, locals, and the return value to their position and sizes in memory.  
    top: usize,  // Points to byte one past the topmost local variable
}

impl FunctionInfo {
    fn new(analyzed_ast: &AnalyzedAST, name: String) -> Result<FunctionInfo, GenerateError> {
        let mut info = FunctionInfo {
            variables: HashMap::new(),
            top: 0,
        };

        let analysis_info = analyzed_ast.functions.get(&name)
            .ok_or(GenerateError("Could not find analyzed function data".to_string()))?;

        // We first allocate return value and arguments, then we push them behind
        // the base pointer and ensure they have 8 alignment. Then we allocate
        // local variables.
                  
        let return_type_info = analyzed_ast.types.get(&analysis_info.return_type)
            .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;
    
        info.add_variable(Variable::Return, return_type_info.size, return_type_info.alignment);

        for (name, param) in &analysis_info.parameter_types {
            let param_type_info = analyzed_ast.types.get(&param)
                .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(Variable::Parameter(name.clone()), param_type_info.size, param_type_info.alignment)
        }

        info.align(8);

        // Bump everything added so far below the base pointer
        for (_, (offset, _)) in info.variables.iter_mut() {
            *offset -= info.top as isize;
        }

        info.top = 0;


        for (name, local) in &analysis_info.local_types {
            let local_type_info = analyzed_ast.types.get(&local)
                .ok_or(GenerateError("Could not find analyzed type data".to_string()))?;

            info.add_variable(Variable::Parameter(name.clone()), local_type_info.size, local_type_info.alignment)
        }

        Ok(info)
    }

    // Ensures correct alignment
    fn add_variable(&mut self, variable: Variable, size: usize, alignment: usize) {
        todo!();
    }

    fn align(&mut self, alignment: usize) {
        todo!();
    }
}


#[derive(PartialEq, Eq, Hash)]
enum Variable {
    Return,
    Parameter (String),
    Local (String),
}