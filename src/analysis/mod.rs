pub mod types;

pub mod type_store;
pub use type_store::TypeStore;

mod desugar;
pub use desugar::desugar_after_ast_build;

mod scope_check;
pub use scope_check::scope_check;

mod type_check;
pub use type_check::type_check;

use std::collections::HashMap;

use crate::ast::{ExprAST, TypeAST};
use crate::CompilationEnvironment;

use types::Type;

pub struct Function {
    pub ast: ExprAST,
    pub return_type: Type,

    /// Argument order is important, so a Vector is used.
    pub parameter_types: Vec<(String, Type)>,

    /// Local order *kinda* doesn't matter, so we have a hash map. None means the type has not yet been decided.
    pub local_types: HashMap<String, Option<Type>>,

    #[allow(unused)]
    pub scope: HashMap<String, bool>, // Temporary - the bool being true means mutable (aka `var`).
}

impl Function {
    pub fn new(
        _env: &CompilationEnvironment,
        ast: ExprAST,
        params: Vec<(String, TypeAST)>,
        return_type: &TypeAST,
    ) -> Function {
        // Could become Result

        // TODO: Someday we might want this to add type generation requests to _env

        let parameter_types = params.into_iter().map(|(name, ref type_name)| (name, type_name.into())).collect();
        Function {
            ast,
            return_type: return_type.into(),
            parameter_types,
            local_types: HashMap::new(),
            scope: HashMap::new(),
        }
    }
}
