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

    let left = build_expr_ast(&children[0])?;

    children[1].expect_holds(&Op::Equals)?;

    let right = build_expr_ast(&children[2])?;

    let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

    Ok(StatementAST::Assignment(left, right, ASTNodeData::new(span)))
}

fn build_compound_assignment_statement(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = tree.assert_rule_get_children("CompoundAssignmentStatement")?;

    let left = build_expr_ast(&children[0])?;

    let ST::TokenNode(Token { body: TB::Operator(op), .. }) = &children[1] else {
        return Err("Expected operator in compound assignment".into());
    };

    let math_op = match op {
        Op::PlusEquals => MathOperation::Add,
        Op::MinusEquals => MathOperation::Subtract,
        Op::TimesEquals => MathOperation::Multiply,
        Op::DivideEquals => MathOperation::Divide,
        Op::ModulusEquals => MathOperation::Modulus,
        _ => return Err("Expected Compound Assignment Operator".into()),
    };

    let right = build_expr_ast(&children[2])?;

    let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

    // This will turn into an assignment and an operation at a later desugar stage.
    Ok(StatementAST::CompoundAssignment(left, right, math_op, ASTNodeData::new(span)))
}
