//! This module handles constructing [`TypeASTs`](TypeAST) from Syntax Trees.

use super::syntax_tree::{SyntaxTree, SyntaxTreeExtension, ST};
use super::{ASTNodeData, TypeAST};

use crate::error::ASTError;
use crate::token::{Operator as Op, Span, Token, TokenBody as TB};

/* Public (to ast) Function that Constructs a Type */

pub(super) fn build_type_ast(tree: &SyntaxTree) -> Result<TypeAST, ASTError> {
    match tree.assert_rule_get_children("Type")? {
        [ST::TokenNode(Token { body: TB::Identifier(identifier), span })] => {
            Ok(TypeAST::NamedType(identifier.clone(), ASTNodeData::new(span.clone())))
        }
        [child @ ST::RuleNode { rule_name, .. }] if rule_name == "PtrType" => build_pointer_type(child),
        _ => Err("Could not build Type node".into()),
    }
}

/* Functions that Construct Specific Kinds of Types */

fn build_pointer_type(tree: &SyntaxTree) -> Result<TypeAST, ASTError> {
    let children = tree.assert_rule_get_children("PtrType")?;

    children[0].expect_holds(&Op::Times)?;
    let star_span = children[0].span_of_token()?;

    let child_type = build_type_ast(&children[1])?;
    let span = Span::combine(star_span, &child_type.get_node_data().span);
    Ok(TypeAST::Pointer(Box::new(child_type), ASTNodeData::new(span.clone())))
}
