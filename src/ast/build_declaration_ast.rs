//! This module handles construction of [`DeclarationASTs`](DeclarationAST) from Syntax Trees.

use super::build_ast_helpers::assert_rule_get_children;
use super::build_expr_ast::build_expr_ast;
use super::build_type_ast::build_type_ast;
use super::{ASTNodeData, DeclarationAST, Mutability, TypeAST};

use crate::error::ASTError;
use crate::token::{Keyword as Kw, Operator as Op, Punctuation as Punc, Span, Token, TokenBody as TB};

use parsley::SyntaxTree as ST;

/* Public (to ast) Declaration Construction */

pub(super) fn build_declaration_ast(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "Declaration")?;

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

fn build_function_declaration(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "FunctionDeclaration")?;

    if children.len() != 6 {
        return Err("Incorrect number of subnodes to function node".into());
    }

    let ST::TokenNode(Token { body: TB::Keyword(Kw::Fn), span: ref first_span }) = children[0] else {
        return Err("Expected `fn` in function declaration".into());
    };

    let name = if let ST::TokenNode(Token { body: TB::Identifier(name), .. }) = &children[1] {
        name.clone()
    } else {
        return Err("Expected function name".into());
    };

    if !matches!(children[3], ST::TokenNode(Token { body: TB::Operator(Op::ThinRightArrow), .. })) {
        return Err("Expected `->` in function declaration".into());
    }

    let return_type = build_type_ast(&children[4])?;

    let params = build_parameter_list(&children[2])?;
    let block = build_expr_ast(&children[5])?;

    let span = Span::combine(first_span, &block.get_node_data().span);

    Ok(DeclarationAST::Function { name, params, block, node_data: ASTNodeData::new(span), return_type })
}

fn build_type_declaration(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "TypeDeclaration")?;

    match children {
        [ST::RuleNode { rule_name, .. }] if rule_name == "StructDeclaration" => build_struct_declaration(&children[0]),

        _ => Err("Incorrect number of subnodes to type node".into()),
    }
}

fn build_struct_declaration(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "StructDeclaration")?;

    match children {
        [ST::TokenNode(Token { body: TB::Keyword(Kw::Struct), span: first_span }), ST::TokenNode(Token { body: TB::Identifier(name), .. }), ST::TokenNode(Token { body: TB::Punctuation(Punc::LeftCurlyBrace), .. }), list_node @ ST::RuleNode { rule_name, .. }, ST::TokenNode(Token { body: TB::Punctuation(Punc::RightCurlyBrace), span: last_span })]
            if rule_name == "StructMemberList" =>
        {
            let members = build_struct_member_list(list_node)?;

            let span = Span::combine(first_span, last_span);

            Ok(DeclarationAST::Struct { name: name.clone(), members, node_data: ASTNodeData::new(span) })
        }
        _ => Err("Failed to parse struct declaration".into()),
    }
}

fn build_variable_declaration(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "VariableDeclaration")?;

    let build_variable_impl = |keyword: &Kw, first_span: &Span, name: &str, expr_node: &ST<Token>| {
        let mutability = match keyword {
            Kw::Var => Mutability::Var,
            Kw::Val => Mutability::Val,
            _ => panic!("Known unreachable"),
        };

        let type_ascription = if children.len() == 6 {
            match &children[2..4] {
                [ST::TokenNode(Token { body: TB::Punctuation(Punc::Colon), .. }), type_node] => {
                    Some(build_type_ast(type_node)?)
                }
                _ => return Err("Expected type".into()),
            }
        } else {
            None
        };

        let expr = build_expr_ast(expr_node)?;
        let span = Span::combine(first_span, &expr.get_node_data().span);

        Ok(DeclarationAST::Variable {
            mutability,
            name: name.to_string(),
            expr,
            node_data: ASTNodeData::new(span),
            type_ascription,
        })
    };

    match children {
        #[rustfmt::skip]
        [
            ST::TokenNode(Token { body: TB::Keyword(keyword @ (Kw::Val | Kw::Var)), span: first_span }),
            ST::TokenNode(Token { body: TB::Identifier(name), .. }),
            ..,
            ST::TokenNode(Token { body: TB::Operator(Op::Equals), .. }),
            expr_node @ ST::RuleNode { rule_name: last_rule_name, .. }
        ] if last_rule_name == "Expression" => build_variable_impl(keyword, first_span, name, expr_node),
        _ => Err("Failed to parse variable declaration".into()),
    }
}

/* Helpers that Build Parts of Declarations */

fn build_parameter_list(node: &ST<Token>) -> Result<Vec<(String, TypeAST)>, ASTError> {
    let ST::RuleNode { rule_name, subexpressions } = node else {
        return Err("Expected Rule node".into());
    };

    if rule_name != "ParameterList" {
        return Err("Expected parameter list in function declaration".into());
    }

    if !matches!(&subexpressions[0], ST::TokenNode(Token { body: TB::Punctuation(Punc::LeftParenthesis), .. })) {
        return Err("Expected left parenthesis".into());
    }
    if !matches!(
        &subexpressions[subexpressions.len() - 1],
        ST::TokenNode(Token { body: TB::Punctuation(Punc::RightParenthesis), .. })
    ) {
        return Err("Expected right parenthesis".into());
    }

    let list = &subexpressions[1..subexpressions.len() - 1];
    let mut iter = list.iter().peekable();

    let mut parameters = vec![];
    while let Some(node) = iter.next() {
        let ST::TokenNode(Token { body: TB::Identifier(name), .. }) = node else {
            return Err("Expected name".into());
        };

        let Some(ST::TokenNode(Token { body: TB::Punctuation(Punc::Colon), .. })) = iter.next() else {
            return Err("Expected colon".into());
        };

        let param_type = build_type_ast(iter.next().ok_or(ASTError::from("Expected type node"))?)?;

        parameters.push((name.clone(), param_type));

        match iter.next() {
            Some(ST::TokenNode(Token { body: TB::Punctuation(Punc::Comma), .. })) => (),
            Some(_) => return Err("Expected comma".into()),
            None => break, // In case the iterator restarts?
        }
    }

    Ok(parameters)
}

fn build_struct_member_list(tree: &ST<Token>) -> Result<Vec<(String, TypeAST)>, ASTError> {
    let children = assert_rule_get_children(tree, "StructMemberList")?;

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
            match node {
                ST::TokenNode(Token { body: TB::Punctuation(Punc::Comma), .. }) => (),
                _ => return Err("Expected comma".into()),
            }
        }
    }

    Ok(entries)
}

fn build_struct_member_list_entry(tree: &ST<Token>) -> Result<(String, TypeAST), ASTError> {
    let children = assert_rule_get_children(tree, "StructMemberListEntry")?;

    match children {
        [ST::TokenNode(Token { body: TB::Identifier(name), .. }), ST::TokenNode(Token { body: TB::Punctuation(Punc::Colon), .. }), type_node @ ST::RuleNode { rule_name, .. }]
            if rule_name == "Type" =>
        {
            Ok((name.clone(), build_type_ast(type_node)?))
        }
        _ => Err("Could not parse struct member".into()),
    }
}
