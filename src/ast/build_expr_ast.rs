//! This module handles building [`ExprASTS`](ExprAST) from Syntax Trees.

use super::build_statement_ast::build_statement_ast;
use super::build_type_ast::build_type_ast;
use super::syntax_tree::{SyntaxTree, SyntaxTreeExtension, ST};
use super::{ASTNodeData, ExprAST};

use crate::error::ASTError;
use crate::instructions::Comparison;
use crate::token::{Keyword as Kw, Operator as Op, Punctuation as Punc, Span, Token, TokenBody as TB};

/* Public (to ast) Function that Constructs Expressions */

pub(super) fn build_expr_ast(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    // Prevent stack overflow by allocating additional stack as required.
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        match tree {
            ST::RuleNode { rule_name, subexpressions } if rule_name == "Expression" => {
                if subexpressions.len() == 1 {
                    build_expr_ast(&subexpressions[0])
                } else {
                    Err("Expected exactly one child of Expression node".into())
                }
            }
            ST::RuleNode { rule_name, subexpressions } if rule_name == "PrimaryExpression" => {
                if subexpressions.len() == 1 {
                    // Literal, Variable, or Block
                    build_expr_ast(subexpressions.iter().next().expect("Known to exist"))
                } else if subexpressions.len() == 3 {
                    if !matches!(
                        &subexpressions[0],
                        ST::TokenNode(Token { body: TB::Punctuation(Punc::LeftParenthesis), .. })
                    ) || !matches!(
                        &subexpressions[2],
                        ST::TokenNode(Token { body: TB::Punctuation(Punc::RightParenthesis), .. })
                    ) {
                        Err("Expected parentheses".into())
                    } else {
                        build_expr_ast(&subexpressions[1])
                    }
                } else {
                    Err("Wrong number of subtrees at PrimaryExpression".into())
                }
            }
            ST::RuleNode { rule_name, .. } if rule_name == "AdditiveExpression" => build_additive_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "MultiplicativeExpression" => {
                build_multiplicative_expr(tree)
            }
            ST::RuleNode { rule_name, .. } if rule_name == "AccessExpression" => build_access_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "ComparisonExpression" => build_comparision_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "OrExpression" => build_or_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "AndExpression" => build_and_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "NotExpression" => build_not_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "Literal" => build_literal_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "FunctionCall" => build_function_call_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "BlockExpression" => build_block_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "IfExpression" => build_if_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "WhileExpression" => build_while_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "ReturnExpression" => build_return_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "StructExpression" => build_struct_expr(tree),
            ST::RuleNode { rule_name, .. } if rule_name == "AllocUninitExpression" => build_alloc_uninit_expr(tree),
            ST::RuleNode { rule_name, subexpressions: _ } => {
                Err(format!("Expected Expression. Unknown expression node name: {rule_name}").into())
            }
            ST::TokenNode(Token { body: TB::Identifier(name), span }) => {
                Ok(ExprAST::Variable(name.clone(), ASTNodeData::new(span.clone())))
            }
            ST::TokenNode(tok) => Err(format!("Expected expression, found token {tok}").into()),
        }
    })
}

/* Functions that Build Specific Kinds of Expressions */

fn build_additive_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("AdditiveExpression")?;

    combine_binary_ops(children, true, |left, op, right| {
        let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

        if op.holds(&Op::Plus) {
            Ok(ExprAST::Add(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        } else if op.holds(&Op::Minus) {
            Ok(ExprAST::Subtract(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        } else {
            Err("Expected + or -".into())
        }
    })
}

fn build_multiplicative_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("MultiplicativeExpression")?;

    combine_binary_ops(children, true, |left, op, right| {
        let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

        if op.holds(&Op::Times) {
            Ok(ExprAST::Multiply(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        } else if op.holds(&Op::Divide) {
            Ok(ExprAST::Divide(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        } else if op.holds(&Op::Modulus) {
            Ok(ExprAST::Modulus(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        } else {
            Err("Expected *, /, or %".into())
        }
    })
}

fn build_access_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("AccessExpression")?;

    let mut iter = children.iter();

    let mut curr_expr = build_expr_ast(iter.next().ok_or::<ASTError>("Expected a child of AccessExpression".into())?)?;
    let mut curr_span = curr_expr.get_node_data().span.clone();

    while let Some(ST::TokenNode(Token { body: TB::Operator(Op::Dot), .. })) = iter.next() {
        let Some(ST::TokenNode(Token { body: TB::Identifier(member_name), span: new_span })) = iter.next() else {
            return Err("Expected identifier in AccessExpression".into());
        };

        curr_span = Span::combine(&curr_span, new_span);

        curr_expr =
            ExprAST::MemberAccess(Box::new(curr_expr), member_name.clone(), ASTNodeData::new(curr_span.clone()));
    }

    Ok(curr_expr)
}

fn build_comparision_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("ComparisonExpression")?;

    if children.len() == 1 {
        build_expr_ast(&children[0])
    } else if children.len() == 3 {
        let comparison = match &children[1] {
            ST::TokenNode(Token { body: TB::Operator(Op::DoubleEquals), .. }) => Comparison::Equals,
            ST::TokenNode(Token { body: TB::Operator(Op::NotEquals), .. }) => Comparison::NotEquals,
            ST::TokenNode(Token { body: TB::Operator(Op::LessEquals), .. }) => Comparison::LessEquals,
            ST::TokenNode(Token { body: TB::Operator(Op::GreaterEquals), .. }) => Comparison::GreaterEquals,
            ST::TokenNode(Token { body: TB::Operator(Op::Less), .. }) => Comparison::Less,
            ST::TokenNode(Token { body: TB::Operator(Op::Greater), .. }) => Comparison::Greater,
            _ => return Err("Expected comparison operator".into()),
        };

        let left = Box::new(build_expr_ast(&children[0])?);
        let right = Box::new(build_expr_ast(&children[2])?);

        let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

        Ok(ExprAST::Comparison(left, right, comparison, ASTNodeData::new(span)))
    } else {
        Err("Unexpected number of subexpression under ComparisonExpression".into())
    }
}

fn build_or_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("OrExpression")?;

    combine_binary_ops(children, true, |left, op, right| {
        op.expect_holds(&Kw::Or)?;

        let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
        Ok(ExprAST::Or(Box::new(left), Box::new(right), ASTNodeData::new(span)))
    })
}

fn build_and_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("AndExpression")?;

    combine_binary_ops(children, true, |left, op, right| {
        op.expect_holds(&Kw::And)?;

        let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
        Ok(ExprAST::And(Box::new(left), Box::new(right), ASTNodeData::new(span)))
    })
}

fn build_not_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("NotExpression")?;

    if children.len() == 2 {
        children[0].expect_holds(&Kw::Not)?;

        let inner = build_expr_ast(&children[1])?;
        let span = Span::combine(children[0].span_of_token()?, &inner.get_node_data().span);

        Ok(ExprAST::Not(Box::new(inner), ASTNodeData::new(span)))
    } else {
        build_expr_ast(&children[0])
    }
}

fn build_literal_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let [child] = tree.assert_rule_get_children("Literal")? else {
        return Err("Wrong number of subnodes to node Literal".into());
    };

    match child {
        ST::RuleNode { rule_name, subexpressions } if rule_name == "BooleanLiteral" => {
            match subexpressions.as_slice() {
                [ST::TokenNode(Token { body: TB::Keyword(Kw::True), span })] => {
                    Ok(ExprAST::BooleanLiteral(true, ASTNodeData::new(span.clone())))
                }
                [ST::TokenNode(Token { body: TB::Keyword(Kw::False), span })] => {
                    Ok(ExprAST::BooleanLiteral(false, ASTNodeData::new(span.clone())))
                }
                _ => Err("Expected true or false under BooleanLiteral node".into()),
            }
        }
        ST::RuleNode { .. } => Err("Unexpected rule node under Literal node".into()),
        ST::TokenNode(Token { body: TB::NumericLiteral(str), span }) => {
            let num =
                str.parse().map_err(|_| ASTError("Integer parse failed. Literals must fit in i128".to_string()))?;

            Ok(ExprAST::IntegerLiteral(num, ASTNodeData::new(span.clone())))
        }
        ST::TokenNode(_) => Err("Non numeric literal under Literal node".into()),
    }
}

fn build_function_call_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("FunctionCall")?;

    // First child processed at the end, where we split on whether it is an identifier or a keyword (such as free!).

    children[1].expect_holds(&Punc::LeftParenthesis)?;

    children[children.len() - 1].expect_holds(&Punc::RightParenthesis)?;
    let last_span = children[children.len() - 1].span_of_token()?;

    let arg_list = &children[2..children.len() - 1];
    let mut iter = arg_list.iter();

    let mut expressions = vec![];
    while let Some(node) = iter.next() {
        expressions.push(build_expr_ast(node)?);

        match iter.next() {
            Some(comma_node) => comma_node.expect_holds(&Punc::Comma)?,
            None => break, // In case the iterator would restart.
        }
    }

    match &children[0] {
        ST::TokenNode(Token { body: TB::Identifier(name), span: first_span }) => {
            Ok(ExprAST::FunctionCall(name.clone(), expressions, ASTNodeData::new(Span::combine(first_span, last_span))))
        }
        ST::TokenNode(Token { body: TB::Keyword(Kw::Free), span: first_span }) => {
            if expressions.len() != 1 {
                return Err("Expected exactly 1 expression in free! call".into());
            }

            Ok(ExprAST::Free {
                subexpr: Box::new(expressions.pop().expect("known to exist")),
                data: ASTNodeData::new(Span::combine(first_span, last_span)),
            })
        }
        _ => Err("Could not find identifier in FunctionCall".into()),
    }
}

fn build_block_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("BlockExpression")?;

    children[0].expect_holds(&Punc::LeftCurlyBrace)?;
    let first_span = children[0].span_of_token()?;

    children[children.len() - 1].expect_holds(&Punc::RightCurlyBrace)?;
    let last_span = children[children.len() - 1].span_of_token()?;

    // &Vec<...> -> Vec<&...>
    let mut subexpressions: Vec<&SyntaxTree> = children.iter().collect();
    subexpressions.remove(subexpressions.len() - 1); // Discard last brace

    if subexpressions.is_empty() {
        return Ok(ExprAST::Block(vec![], None, ASTNodeData::new(Span::combine(first_span, last_span))));
    }

    let opt_expr = if let ST::RuleNode { rule_name, subexpressions: _ } = &subexpressions[subexpressions.len() - 1] {
        if rule_name == "Expression" {
            let last = subexpressions.remove(subexpressions.len() - 1);

            Some(Box::new(build_expr_ast(last)?))
        } else {
            None
        }
    } else {
        None
    };

    let statements = subexpressions
        .into_iter()
        .skip(1) // Discard first brace
        .map(build_statement_ast)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(ExprAST::Block(statements, opt_expr, ASTNodeData::new(Span::combine(first_span, last_span))))
}

fn build_if_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("IfExpression")?;

    if children.len() < 3 {
        return Err("Expected 3 subexpressions for IfExpression".into());
    }

    children[0].expect_holds(&Kw::If)?;
    let first_span = children[0].span_of_token()?;

    let condition = Box::new(build_expr_ast(&children[1])?);

    let block = Box::new(build_block_expr(&children[2])?);

    let (else_branch, final_span) = if children.len() == 5 {
        children[3].expect_holds(&Kw::Else)?;

        let else_block = build_expr_ast(&children[4])?;
        let else_span = else_block.get_node_data().span.clone();

        (Some(Box::new(else_block)), else_span)
    } else {
        (None, block.get_node_data().span.clone())
    };

    Ok(ExprAST::If { condition, block, else_branch, data: ASTNodeData::new(Span::combine(first_span, &final_span)) })
}

fn build_while_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let [while_node, condition_node, block_node] = tree.assert_rule_get_children("WhileExpression")? else {
        return Err("Expected 3 children of while node".into());
    };

    while_node.expect_holds(&Kw::While)?;
    let first_span = while_node.span_of_token()?;

    let condition = Box::new(build_expr_ast(condition_node)?);

    let block = Box::new(build_block_expr(block_node)?);

    let span = Span::combine(first_span, &block.get_node_data().span);

    Ok(ExprAST::While { condition, block, data: ASTNodeData::new(span) })
}

fn build_return_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("ReturnExpression")?;

    children[0].expect_holds(&Kw::Return)?;
    let first_span = children[0].span_of_token()?;

    let mut span = first_span.clone();

    let expr = if children.len() == 2 {
        let expr = Box::new(build_expr_ast(&children[1])?);

        span = Span::combine(&span, &expr.get_node_data().span);

        Some(expr)
    } else {
        None
    };

    Ok(ExprAST::Return(expr, ASTNodeData::new(span)))
}

fn build_struct_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let children = tree.assert_rule_get_children("StructExpression")?;

    let ST::TokenNode(Token { body: TB::Identifier(ref name), span: ref first_span }) = children[0] else {
        return Err("Expected struct name".into());
    };

    children[1].expect_holds(&Punc::LeftCurlyBrace)?;

    let arg_list = &children[2].assert_rule_get_children("StructExpressionMemberList")?;

    children[3].expect_holds(&Punc::RightCurlyBrace)?;
    let last_span = children[3].span_of_token()?;

    let mut iter = arg_list.iter();
    let mut members = vec![];
    while let Some(member_name_tree) = iter.next() {
        let ST::TokenNode(Token { body: TB::Identifier(member_name), .. }) = member_name_tree else {
            return Err("Expected identifier".into());
        };

        iter.next().ok_or(ASTError::from("Expected colon"))?.expect_holds(&Punc::Colon)?;

        let member_expression = iter.next().ok_or(ASTError::from("Expected expression"))?;

        members.push((member_name.clone(), build_expr_ast(member_expression)?));

        match iter.next() {
            Some(child) => child.expect_holds(&Punc::Comma)?,
            None => break, // In case the iterator would reset.
        }
    }

    Ok(ExprAST::StructExpression {
        name: name.clone(),
        members,
        data: ASTNodeData::new(Span::combine(first_span, last_span)),
    })
}

fn build_alloc_uninit_expr(tree: &SyntaxTree) -> Result<ExprAST, ASTError> {
    let [alloc, leftt_paren, type_node, right_paren] = tree.assert_rule_get_children("AllocUninitExpression")? else {
        return Err("Expected 4 children.".into());
    };

    alloc.expect_holds(&Kw::AllocUninit)?;
    let first_span = alloc.span_of_token()?;

    leftt_paren.expect_holds(&Punc::LeftParenthesis)?;

    let type_ast = build_type_ast(type_node)?;

    right_paren.expect_holds(&Punc::RightParenthesis)?;
    let last_span = right_paren.span_of_token()?;

    Ok(ExprAST::AllocUninit(type_ast, ASTNodeData::new(Span::combine(first_span, last_span))))
}

/* Helpers for AST build functions */

fn combine_binary_ops<F>(subtrees: &[SyntaxTree], left_to_right: bool, combine_fn: F) -> Result<ExprAST, ASTError>
where
    F: Fn(ExprAST, &SyntaxTree, ExprAST) -> Result<ExprAST, ASTError>,
{
    if subtrees.len() == 1 {
        build_expr_ast(subtrees.iter().next().expect("Known to exist"))
    } else if left_to_right {
        let mut iterator = subtrees.iter(); // Matches left to right semantics

        let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

        while let Some(op) = iterator.next() {
            let right = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

            ast = combine_fn(ast, op, right)?;
        }

        Ok(ast)
    } else {
        let mut iterator = subtrees.iter().rev(); // Matches right to left semantics

        let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

        while let Some(op) = iterator.next() {
            let left = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

            ast = combine_fn(left, op, ast)?;
        }

        Ok(ast)
    }
}
