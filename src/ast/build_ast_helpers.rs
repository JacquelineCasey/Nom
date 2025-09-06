//! Helpers for the many functions that construct ASTs from Syntax Trees.

use crate::error::ASTError;
use crate::token::Token;

use parsley::SyntaxTree as ST;

pub(super) fn assert_rule_get_children<'a>(
    tree: &'a ST<Token>,
    expected_name: &str,
) -> Result<&'a [ST<Token>], ASTError> {
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == expected_name => Ok(subexpressions),
        ST::RuleNode { rule_name, .. } => Err(format!("Expected {expected_name} node, found {rule_name} node").into()),
        ST::TokenNode(_) => Err(format!("Expected {expected_name} node, found token node").into()),
    }
}
