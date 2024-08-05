pub mod types;

mod desugar;
pub use desugar::desugar; // Desugaring should happen right after the AST is created.

mod scope_check;
pub use scope_check::scope_check; // Scope check happens next. This task enters the compilation queue.

mod type_check;
pub use type_check::type_check; // Finally, types are analyzed and decided. This also enters the compilation queue.

use std::collections::HashMap;

use crate::ast::ExprAST;
use crate::CompilationEnvironment;

use types::Type;

pub struct Function {
    pub ast: ExprAST,
    pub return_type: Type,
    pub parameter_types: Vec<(String, Type)>, // Argument order is important, so a Vector is used.
    // Local order *kinda* doesn't matter, so we have a hash map
    // None means the type has not yet been decided.
    pub local_types: HashMap<String, Option<Type>>,
    pub scope: HashMap<String, bool>, // Temporary - the bool being true means mutable (aka `var`).
}

impl Function {
    pub fn new(
        _env: &CompilationEnvironment,
        ast: ExprAST,
        params: Vec<(String, String)>,
        return_type: String,
    ) -> Function {
        // Could become Result

        // TODO: Someday we might want this to add type generation requests to _env

        let parameter_types =
            params.into_iter().map(|(name, type_name)| (name, type_name.into())).collect();
        Function {
            ast,
            return_type: return_type.into(),
            parameter_types,
            local_types: HashMap::new(),
            scope: HashMap::new(),
        }
    }
}
