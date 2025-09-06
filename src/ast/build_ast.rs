//! This module handles construction of an AST from a Syntax Tree.

use super::build_declaration_ast::build_declaration_ast;
use super::{ASTNodeData, AST};

use crate::error::ASTError;
use crate::token::{Span, Token};

use parsley::SyntaxTree as ST;

/* Functions that build AST Nodes */

pub fn build_ast(tree: &ST<Token>) -> Result<AST, ASTError> {
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == "Program" => {
            let declarations = subexpressions.iter().map(build_declaration_ast).collect::<Result<Vec<_>, _>>()?;

            let span = Span::combine_all(
                &declarations.iter().map(|decl| decl.get_node_data().span.clone()).collect::<Vec<_>>(),
            );

            Ok(AST { declarations, node_data: ASTNodeData::new(span) })
        }
        _ => Err("Failed to build Program AST".into()),
    }
}
