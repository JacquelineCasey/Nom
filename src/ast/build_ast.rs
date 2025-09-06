//! This module handles construction of an AST from a Syntax Tree.

use super::{ASTNodeData, DeclarationAST, ExprAST, MathOperation, Mutability, StatementAST, TypeAST, AST};

use crate::error::ASTError;
use crate::instructions::Comparison;
use crate::token::{Keyword as Kw, Operator as Op, Punctuation as Punc, Span, Token, TokenBody as TB};

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

fn build_declaration_ast(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
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

fn build_statement_ast(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = assert_rule_get_children(tree, "Statement")?;

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

fn build_expr_ast(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    // Prevent stack overflow by allocating additional stack as required.
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        match tree {
            ST::RuleNode { rule_name, subexpressions } if rule_name == "Expression" => {
                if subexpressions.len() == 1 {
                    build_expr_ast(&subexpressions[0])
                } else {
                    Err("Expected exactly on child of Expression node".into())
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
            ST::RuleNode { ref rule_name, .. } if rule_name == "Literal" => build_literal_expr(tree),
            ST::RuleNode { ref rule_name, .. } if rule_name == "FunctionCall" => build_function_call_expr(tree),
            ST::RuleNode { ref rule_name, .. } if rule_name == "BlockExpression" => build_block_expr(tree),
            ST::RuleNode { ref rule_name, .. } if rule_name == "IfExpression" => build_if_expr(tree),
            ST::RuleNode { ref rule_name, .. } if rule_name == "WhileExpression" => build_while_expr(tree),
            ST::RuleNode { ref rule_name, .. } if rule_name == "ReturnExpression" => build_return_expr(tree),
            ST::RuleNode { ref rule_name, .. } if rule_name == "StructExpression" => build_struct_expr(tree),
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

/* Functions that build specific kinds of AST Node */

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

    let return_type = build_type(&children[4])?;

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

    match children {
        [ST::TokenNode(Token { body: TB::Keyword(keyword @ (Kw::Val | Kw::Var)), span: first_span }), ST::TokenNode(Token { body: TB::Identifier(name), .. }), .., ST::TokenNode(Token { body: TB::Operator(Op::Equals), .. }), expr_node @ ST::RuleNode { rule_name: last_rule_name, .. }]
            if last_rule_name == "Expression" =>
        {
            let mutability = match keyword {
                Kw::Var => Mutability::Var,
                Kw::Val => Mutability::Val,
                _ => panic!("Known unreachable"),
            };

            let type_ascription = if children.len() == 6 {
                match &children[2..4] {
                    [ST::TokenNode(Token { body: TB::Punctuation(Punc::Colon), .. }), type_node] => {
                        Some(build_type(type_node)?)
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
                name: name.clone(),
                expr,
                node_data: ASTNodeData::new(span),
                type_ascription,
            })
        }
        _ => Err("Failed to parse variable declaration".into()),
    }
}

fn build_assignment_statement(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = assert_rule_get_children(tree, "AssignmentStatement")?;

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
    let children = assert_rule_get_children(tree, "CompoundAssignmentStatement")?;

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

fn build_additive_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "AdditiveExpression")?;

    combine_binary_ops(children, true, |left, op, right| match op {
        ST::TokenNode(Token { body: TB::Operator(Op::Plus), .. }) => {
            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
            Ok(ExprAST::Add(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        }
        ST::TokenNode(Token { body: TB::Operator(Op::Minus), .. }) => {
            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
            Ok(ExprAST::Subtract(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        }
        _ => Err("Expected + or -".into()),
    })
}

fn build_multiplicative_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "MultiplicativeExpression")?;

    combine_binary_ops(children, true, |left, op, right| match op {
        ST::TokenNode(Token { body: TB::Operator(Op::Times), .. }) => {
            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
            Ok(ExprAST::Multiply(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        }
        ST::TokenNode(Token { body: TB::Operator(Op::Divide), .. }) => {
            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
            Ok(ExprAST::Divide(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        }
        ST::TokenNode(Token { body: TB::Operator(Op::Modulus), .. }) => {
            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
            Ok(ExprAST::Modulus(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        }
        _ => Err("Expected *, /, or %".into()),
    })
}

fn build_access_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "AccessExpression")?;

    let mut iter = children.iter();

    let mut curr_expr = build_expr_ast(iter.next().ok_or::<ASTError>("akjgbnajgnksajn".into())?)?;
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

fn build_comparision_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "ComparisonExpression")?;

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

fn build_or_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "OrExpression")?;

    combine_binary_ops(children, true, |left, op, right| match op {
        ST::TokenNode(Token { body: TB::Keyword(Kw::Or), .. }) => {
            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
            Ok(ExprAST::Or(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        }
        _ => Err("Expected 'or'".into()),
    })
}

fn build_and_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "AndExpression")?;

    combine_binary_ops(children, true, |left, op, right| match op {
        ST::TokenNode(Token { body: TB::Keyword(Kw::And), .. }) => {
            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);
            Ok(ExprAST::And(Box::new(left), Box::new(right), ASTNodeData::new(span)))
        }
        _ => Err("Expected 'and'".into()),
    })
}

fn build_not_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "NotExpression")?;

    if children.len() == 2 {
        let ST::TokenNode(Token { body: TB::Keyword(Kw::Not), span: ref first_span }) = children[0] else {
            return Err("...".into());
        };

        let inner = build_expr_ast(&children[1])?;
        let span = Span::combine(first_span, &inner.get_node_data().span);

        Ok(ExprAST::Not(Box::new(inner), ASTNodeData::new(span)))
    } else {
        build_expr_ast(&children[0])
    }
}

fn build_literal_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "Literal")?;

    if children.len() != 1 {
        return Err("Wrong number of subnodes to node Literal".into());
    }

    let child = &children[0];

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

fn build_function_call_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "FunctionCall")?;

    let ST::TokenNode(Token { body: TB::Identifier(name), span: first_span }) = &children[0] else {
        return Err("Could not find identifier in FunctionCall".into());
    };

    if !matches!(&children[1], ST::TokenNode(Token { body: TB::Punctuation(Punc::LeftParenthesis), .. })) {
        return Err("Expected left parenthesis".into());
    }

    let ST::TokenNode(Token { body: TB::Punctuation(Punc::RightParenthesis), span: last_span }) =
        &children[children.len() - 1]
    else {
        return Err("Expected right parenthesis".into());
    };

    let arg_list = &children[2..children.len() - 1];
    let mut iter = arg_list.iter();

    let mut expressions = vec![];
    while let Some(node) = iter.next() {
        expressions.push(build_expr_ast(node)?);

        match iter.next() {
            Some(ST::TokenNode(Token { body: TB::Punctuation(Punc::Comma), .. })) => (),
            Some(_) => return Err("Expected comma".into()),
            None => break, // In case the iterator restarts?
        }
    }

    Ok(ExprAST::FunctionCall(name.clone(), expressions, ASTNodeData::new(Span::combine(first_span, last_span))))
}

fn build_block_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "BlockExpression")?;

    let ST::TokenNode(Token { body: TB::Punctuation(Punc::LeftCurlyBrace), span: ref first_span }) = children[0] else {
        return Err("Expected open bracket before block".into());
    };

    let ST::TokenNode(Token { body: TB::Punctuation(Punc::RightCurlyBrace), span: ref last_span }) =
        children[children.len() - 1]
    else {
        return Err("Expected closed bracket after block".into());
    };

    // &Vec<...> -> Vec<&...>
    let mut subexpressions: Vec<&ST<Token>> = children.iter().collect();
    subexpressions.remove(subexpressions.len() - 1); // Discard last_brace

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

fn build_if_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "IfExpression")?;

    if children.len() < 3 {
        return Err("Expected 3 subexpressions for IfExpression".into());
    }

    let ST::TokenNode(Token { body: TB::Keyword(Kw::If), span: ref first_span }) = children[0] else {
        return Err("Expected keyword if".into());
    };

    let condition = Box::new(build_expr_ast(&children[1])?);

    let block = Box::new(build_block_expr(&children[2])?);

    let (else_branch, final_span) = if children.len() == 5 {
        if !matches!(children[3], ST::TokenNode(Token { body: TB::Keyword(Kw::Else), .. })) {
            return Err("Expected keyword else".into());
        }

        let else_block = build_expr_ast(&children[4])?;
        let else_span = else_block.get_node_data().span.clone();

        (Some(Box::new(else_block)), else_span)
    } else {
        (None, block.get_node_data().span.clone())
    };

    Ok(ExprAST::If { condition, block, else_branch, data: ASTNodeData::new(Span::combine(first_span, &final_span)) })
}

fn build_while_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "WhileExpression")?;

    if children.len() != 3 {
        return Err("Expected 3 subexpressions for WhileExpression".into());
    }

    let ST::TokenNode(Token { body: TB::Keyword(Kw::While), span: ref first_span }) = children[0] else {
        return Err("Expected keyword while".into());
    };

    let condition = Box::new(build_expr_ast(&children[1])?);

    let block = Box::new(build_block_expr(&children[2])?);

    let span = Span::combine(first_span, &block.get_node_data().span);

    Ok(ExprAST::While { condition, block, data: ASTNodeData::new(span) })
}

fn build_return_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "ReturnExpression")?;

    let ST::TokenNode(Token { body: TB::Keyword(Kw::Return), span: ref return_span }) = children[0] else {
        return Err("Expected keyword Return".into());
    };

    let mut span = return_span.clone();

    let expr = if children.len() == 2 {
        let expr = Box::new(build_expr_ast(&children[1])?);

        span = Span::combine(&span, &expr.get_node_data().span);

        Some(expr)
    } else {
        None
    };

    Ok(ExprAST::Return(expr, ASTNodeData::new(span)))
}

fn build_struct_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "StructExpression")?;

    let ST::TokenNode(Token { body: TB::Identifier(ref name), span: ref first_span }) = children[0] else {
        return Err("Expected struct name".into());
    };

    let ST::TokenNode(Token { body: TB::Punctuation(Punc::LeftCurlyBrace), .. }) = children[1] else {
        return Err("Expected open bracket before struct expression".into());
    };

    let arg_list = assert_rule_get_children(&children[2], "StructExpressionMemberList")?;

    let ST::TokenNode(Token { body: TB::Punctuation(Punc::RightCurlyBrace), span: ref last_span }) = children[3] else {
        return Err("Expected closed bracket after struct expression".into());
    };

    let mut iter = arg_list.iter();

    let mut members = vec![];
    while let Some(member_name_tree) = iter.next() {
        let ST::TokenNode(Token { body: TB::Identifier(member_name), .. }) = member_name_tree else {
            return Err("Expected identifier".into());
        };

        match iter.next() {
            Some(ST::TokenNode(Token { body: TB::Punctuation(Punc::Colon), .. })) => (),
            _ => return Err("Expected colon".into()),
        }

        let Some(member_expression) = iter.next() else {
            return Err("Expected expression".into());
        };

        members.push((member_name.clone(), build_expr_ast(member_expression)?));

        match iter.next() {
            Some(ST::TokenNode(Token { body: TB::Punctuation(Punc::Comma), .. })) => (),
            Some(_) => return Err("Expected comma".into()),
            None => break,
        }
    }

    Ok(ExprAST::StructExpression {
        name: name.clone(),
        members,
        data: ASTNodeData::new(Span::combine(first_span, last_span)),
    })
}

/* Functions that build components of AST Nodes */

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
            Ok((name.clone(), build_type(type_node)?))
        }
        _ => Err("Could not parse struct member".into()),
    }
}

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

        let param_type = build_type(iter.next().ok_or(ASTError::from("Expected type node"))?)?;

        parameters.push((name.clone(), param_type));

        match iter.next() {
            Some(ST::TokenNode(Token { body: TB::Punctuation(Punc::Comma), .. })) => (),
            Some(_) => return Err("Expected comma".into()),
            None => break, // In case the iterator restarts?
        }
    }

    Ok(parameters)
}

fn build_type(tree: &ST<Token>) -> Result<TypeAST, ASTError> {
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
            let child_type = build_type(child_node)?;
            let span = Span::combine(star_span, &child_type.get_node_data().span);
            Ok(TypeAST::Pointer(Box::new(child_type), ASTNodeData::new(span.clone())))
        }
        _ => Err("Could not build PtrType node".into()),
    }
}

/* Helpers for AST build functions */

fn assert_rule_get_children<'a>(tree: &'a ST<Token>, expected_name: &str) -> Result<&'a [ST<Token>], ASTError> {
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == expected_name => Ok(subexpressions),
        ST::RuleNode { rule_name, .. } => Err(format!("Expected {expected_name} node, found {rule_name} node").into()),
        ST::TokenNode(_) => Err(format!("Expected {expected_name} node, found token node").into()),
    }
}

fn combine_binary_ops<F>(subtrees: &[ST<Token>], left_to_right: bool, combine_fn: F) -> Result<ExprAST, ASTError>
where
    F: Fn(ExprAST, &ST<Token>, ExprAST) -> Result<ExprAST, ASTError>,
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
