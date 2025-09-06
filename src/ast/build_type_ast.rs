//! This module handles constructing [`TypeASTs`](TypeAST) from Syntax Trees.

use super::build_ast_helpers::assert_rule_get_children;
use super::{ASTNodeData, TypeAST};

use crate::error::ASTError;
use crate::token::{Operator as Op, Span, Token, TokenBody as TB};

use parsley::SyntaxTree as ST;

pub(super) fn build_type_ast(tree: &ST<Token>) -> Result<TypeAST, ASTError> {
    match assert_rule_get_children(tree, "Type")? {
        [ST::TokenNode(Token { body: TB::Identifier(identifier), span })] => {
            Ok(TypeAST::NamedType(identifier.clone(), ASTNodeData::new(span.clone())))
        }
        [child @ ST::RuleNode { rule_name, .. }] if rule_name == "PtrType" => build_pointer_type(child),
        _ => Err("Could not build Type node".into()),
    }
}

fn build_pointer_type(tree: &ST<Token>) -> Result<TypeAST, ASTError> {
    match assert_rule_get_children(tree, "PtrType")? {
        [ST::TokenNode(Token { body: TB::Operator(Op::Times), span: star_span }), child_node @ ST::RuleNode { rule_name, .. }]
            if rule_name == "Type" =>
        {
            let child_type = build_type_ast(child_node)?;
            let span = Span::combine(star_span, &child_type.get_node_data().span);
            Ok(TypeAST::Pointer(Box::new(child_type), ASTNodeData::new(span.clone())))
        }
        _ => Err("Could not build PtrType node".into()),
    }
}
