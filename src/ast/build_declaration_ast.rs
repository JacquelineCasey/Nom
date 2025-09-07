//! This module handles construction of [`DeclarationASTs`](DeclarationAST) from Syntax Trees.

use super::build_expr_ast::build_expr_ast;
use super::build_type_ast::build_type_ast;
use super::syntax_tree::{SyntaxTree, SyntaxTreeExtension, ST};
use super::{ASTNodeData, DeclarationAST, Mutability, TypeAST};

use crate::error::ASTError;
use crate::token::{Keyword as Kw, Operator as Op, Punctuation as Punc, Span, Token, TokenBody as TB};

/* Public (to ast) Declaration Construction */

pub(super) fn build_declaration_ast(tree: &SyntaxTree) -> Result<DeclarationAST, ASTError> {
    let children = tree.assert_rule_get_children("Declaration")?;

    match children {
        [ST::RuleNode { rule_name, .. }] if rule_name == "FunctionDeclaration" => {
            build_function_declaration(&children[0])
        }

        [ST::RuleNode { rule_name, .. }] if rule_name == "TypeDeclaration" => build_type_declaration(&children[0]),

        [decl @ ST::RuleNode { rule_name, .. }, ST::TokenNode(Token { body: TB::Punctuation(Punc::Semicolon), .. })]
            if rule_name == "VariableDeclaration" =>
        {
            build_variable_declaration(decl)
        }

        _ => Err("Failed to build Declaration AST".into()),
    }
}

/* Functions that Construct Specific Declarations */

fn build_function_declaration(tree: &SyntaxTree) -> Result<DeclarationAST, ASTError> {
    let children = tree.assert_rule_get_children("FunctionDeclaration")?;

    if children.len() != 6 {
        return Err("Incorrect number of subnodes to function node".into());
    }

    children[0].expect_holds(&Kw::Fn)?;
    let first_span = children[0].span_of_token()?;

    let name = if let ST::TokenNode(Token { body: TB::Identifier(name), .. }) = &children[1] {
        name.clone()
    } else {
        return Err("Expected function name".into());
    };

    children[3].expect_holds(&Op::ThinRightArrow)?;

    let return_type = build_type_ast(&children[4])?;

    let params = build_parameter_list(&children[2])?;
    let block = build_expr_ast(&children[5])?;

    let span = Span::combine(first_span, &block.get_node_data().span);

    Ok(DeclarationAST::Function { name, params, block, node_data: ASTNodeData::new(span), return_type })
}

fn build_type_declaration(tree: &SyntaxTree) -> Result<DeclarationAST, ASTError> {
    let children = tree.assert_rule_get_children("TypeDeclaration")?;

    match children {
        [ST::RuleNode { rule_name, .. }] if rule_name == "StructDeclaration" => build_struct_declaration(&children[0]),
        _ => Err("Incorrect number of subnodes to type node".into()),
    }
}

fn build_struct_declaration(tree: &SyntaxTree) -> Result<DeclarationAST, ASTError> {
    let children = tree.assert_rule_get_children("StructDeclaration")?;

    children[0].expect_holds(&Kw::Struct)?;
    let first_span = children[0].span_of_token()?;

    let ST::TokenNode(Token { body: TB::Identifier(name), .. }) = &children[1] else {
        return Err("Expected identifier in struct declaration".into());
    };

    children[2].expect_holds(&Punc::LeftCurlyBrace)?;

    let members = build_struct_member_list(&children[3])?;

    children[4].expect_holds(&Punc::RightCurlyBrace)?;
    let last_span = children[4].span_of_token()?;

    Ok(DeclarationAST::Struct {
        name: name.clone(),
        members,
        node_data: ASTNodeData::new(Span::combine(first_span, last_span)),
    })
}

fn build_variable_declaration(tree: &SyntaxTree) -> Result<DeclarationAST, ASTError> {
    let children = tree.assert_rule_get_children("VariableDeclaration")?;

    let ST::TokenNode(Token { body: TB::Keyword(keyword @ (Kw::Val | Kw::Var)), span: first_span }) = &children[0]
    else {
        return Err("Expected var or val in VariableDeclaration node.".into());
    };

    let ST::TokenNode(Token { body: TB::Identifier(name), .. }) = &children[1] else {
        return Err("Expected identifier in VariableDeclaration node.".into());
    };

    let type_ascription = if children.len() == 6 {
        children[2].expect_holds(&Punc::Colon)?;
        Some(build_type_ast(&children[3])?)
    } else {
        None
    };

    children[children.len() - 2].expect_holds(&Op::Equals)?;

    let expr = build_expr_ast(&children[children.len() - 1])?;

    let mutability = match keyword {
        Kw::Var => Mutability::Var,
        Kw::Val => Mutability::Val,
        _ => panic!("Known unreachable"),
    };

    let span = Span::combine(first_span, &expr.get_node_data().span);

    Ok(DeclarationAST::Variable {
        mutability,
        name: name.to_string(),
        expr,
        node_data: ASTNodeData::new(span),
        type_ascription,
    })
}

/* Helpers that Build Parts of Declarations */

fn build_parameter_list(tree: &SyntaxTree) -> Result<Vec<(String, TypeAST)>, ASTError> {
    let children = tree.assert_rule_get_children("ParameterList")?;

    children[0].expect_holds(&Punc::LeftParenthesis)?;
    children[children.len() - 1].expect_holds(&Punc::RightParenthesis)?;

    let list = &children[1..children.len() - 1];
    let mut iter = list.iter();

    let mut parameters = vec![];
    while let Some(node) = iter.next() {
        let ST::TokenNode(Token { body: TB::Identifier(name), .. }) = node else {
            return Err("Expected name".into());
        };

        iter.next().ok_or(ASTError::from("Expected Another Token"))?.expect_holds(&Punc::Colon)?;

        let param_type = build_type_ast(iter.next().ok_or(ASTError::from("Expected type node"))?)?;

        parameters.push((name.clone(), param_type));

        match iter.next() {
            Some(child) => child.expect_holds(&Punc::Comma)?,
            None => break, // In case the iterator restarts?
        }
    }

    Ok(parameters)
}

fn build_struct_member_list(tree: &SyntaxTree) -> Result<Vec<(String, TypeAST)>, ASTError> {
    let children = tree.assert_rule_get_children("StructMemberList")?;

    if children.is_empty() {
        return Ok(vec![]);
    }

    let mut iter = children.iter();
    let mut entries = vec![];

    while let Some(node) = iter.next() {
        match node {
            ST::RuleNode { rule_name, .. } if rule_name == "StructMemberListEntry" => {
                entries.push(build_struct_member_list_entry(node)?);
            }
            _ => return Err("Expected struct member".into()),
        }

        if let Some(node) = iter.next() {
            node.expect_holds(&Punc::Comma)?;
        }
    }

    Ok(entries)
}

fn build_struct_member_list_entry(tree: &SyntaxTree) -> Result<(String, TypeAST), ASTError> {
    let children = tree.assert_rule_get_children("StructMemberListEntry")?;

    let ST::TokenNode(Token { body: TB::Identifier(name), .. }) = &children[0] else {
        return Err("Expected identifier in struct member list entry".into());
    };

    children[1].expect_holds(&Punc::Colon)?;

    Ok((name.clone(), build_type_ast(&children[2])?))
}
