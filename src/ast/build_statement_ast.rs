//! This module handles building [`StatementASTs`](StatementAST) from Syntax Trees.

use super::build_declaration_ast::build_declaration_ast;
use super::build_expr_ast::build_expr_ast;
use super::syntax_tree::SyntaxTreeExtension;
use super::{ASTNodeData, MathOperation, StatementAST};

use crate::error::ASTError;
use crate::token::{Operator as Op, Punctuation as Punc, Span, Token, TokenBody as TB};

use parsley::SyntaxTree as ST;

/* Public (to ast) Function to Construct Statements */

pub(super) fn build_statement_ast(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = tree.assert_rule_get_children("Statement")?;

    match children {
        [ST::RuleNode { rule_name, subexpressions }, ST::TokenNode(Token { body: TB::Punctuation(Punc::Semicolon), span: semicolon_span })]
            if rule_name == "Expression" =>
        {
            let expr = build_expr_ast(&subexpressions[0])?;
            let span = Span::combine(&expr.get_node_data().span, semicolon_span);

            Ok(StatementAST::ExpressionStatement(expr, ASTNodeData::new(span)))
        }

        [stmt @ ST::RuleNode { rule_name, .. }, ST::TokenNode(Token { body: TB::Punctuation(Punc::Semicolon), .. })]
            if rule_name == "AssignmentStatement" =>
        {
            build_assignment_statement(stmt)
        }

        [stmt @ ST::RuleNode { rule_name, .. }, ST::TokenNode(Token { body: TB::Punctuation(Punc::Semicolon), .. })]
            if rule_name == "CompoundAssignmentStatement" =>
        {
            build_compound_assignment_statement(stmt)
        }

        [stmt @ ST::RuleNode { rule_name, .. }] if rule_name == "Declaration" => {
            let decl = build_declaration_ast(stmt)?;
            let span = decl.get_node_data().span.clone();
            Ok(StatementAST::Declaration(decl, ASTNodeData::new(span)))
        }

        _ => Err("Failed to build Statement AST".into()),
    }
}

/* Functions that Construct Specific Kinds of Assignments */

fn build_assignment_statement(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = tree.assert_rule_get_children("AssignmentStatement")?;

    match children {
        [ST::RuleNode { rule_name: rule_1, subexpressions: ref sub_expr_1 }, ST::TokenNode(Token { body: TB::Operator(Op::Equals), .. }), ST::RuleNode { rule_name: rule_2, subexpressions: ref sub_expr_2 }]
            if rule_1 == "Expression" && rule_2 == "Expression" && sub_expr_1.len() == 1 && sub_expr_2.len() == 1 =>
        {
            let left = build_expr_ast(&sub_expr_1[0])?;
            let right = build_expr_ast(&sub_expr_2[0])?;

            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

            Ok(StatementAST::Assignment(left, right, ASTNodeData::new(span)))
        }
        _ => Err("Failed to build AssignmentStatement".into()),
    }
}

fn build_compound_assignment_statement(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = tree.assert_rule_get_children("CompoundAssignmentStatement")?;

    match children {
        [ST::RuleNode { rule_name: rule_1, subexpressions: ref sub_expr_1 }, ST::TokenNode(Token { body: TB::Operator(op), .. }), ST::RuleNode { rule_name: rule_2, subexpressions: ref sub_expr_2 }]
            if rule_1 == "Expression" && rule_2 == "Expression" && sub_expr_1.len() == 1 && sub_expr_2.len() == 1 =>
        {
            let left = build_expr_ast(&sub_expr_1[0])?;
            let right = build_expr_ast(&sub_expr_2[0])?;

            let math_op = match op {
                Op::PlusEquals => MathOperation::Add,
                Op::MinusEquals => MathOperation::Subtract,
                Op::TimesEquals => MathOperation::Multiply,
                Op::DivideEquals => MathOperation::Divide,
                Op::ModulusEquals => MathOperation::Modulus,
                _ => return Err("Expected Compound Assignment Operator".into()),
            };

            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

            // This will turn into an assignment and an operation at a later
            // desugar stage.
            Ok(StatementAST::CompoundAssignment(left, right, math_op, ASTNodeData::new(span)))
        }
        _ => Err("Failed to build CompoundAssignmentStatement".into()),
    }
}
