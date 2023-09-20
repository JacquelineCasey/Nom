// Desugaring is the process of removing syntactic sugar. This process happens
// early so that later analysis steps can take place with a somewhat normalized
// format.

use crate::ast::{AST, DeclarationAST, ExprAST, StatementAST, ASTNodeData};


pub(crate) fn desugar(ast: &mut AST)  {
    for decl in &mut ast.declarations {
        desugar_declaration(decl);
    }
}

fn desugar_declaration(decl: &mut DeclarationAST) {
    match decl {
        DeclarationAST::Function { block: expr, .. } 
        | DeclarationAST::Variable { expr, .. } => desugar_expression(expr),
    }
}

fn desugar_statement(statement: &mut StatementAST) {
    match statement {
        StatementAST::ExpressionStatement(expr, _) => desugar_expression(expr),
        StatementAST::Assignment(a, b, _) => {
            desugar_expression(a);
            desugar_expression(b);
        },
        StatementAST::Declaration(decl, _) => {
            desugar_declaration(decl);
        },
        StatementAST::CompoundAssignment(left, right, op, _) => {
            let left = std::mem::take(left);
            let right = std::mem::take(right);

            let operation = match op {
                crate::ast::MathOperation::Add => ExprAST::Add(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
                crate::ast::MathOperation::Subtract => ExprAST::Subtract(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
                crate::ast::MathOperation::Multiply => ExprAST::Multiply(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
                crate::ast::MathOperation::Divide => ExprAST::Divide(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
            };

            _ = std::mem::replace(statement, StatementAST::Assignment(left, operation, ASTNodeData::new()))
        }
    }
}

fn desugar_expression(expr: &mut crate::ast::ExprAST) {
    match expr {
        ExprAST::Add(a, b, _)
        | ExprAST::Subtract(a, b, _)
        | ExprAST::Multiply(a, b, _)
        | ExprAST::Divide(a, b, _)
        | ExprAST::Comparison(a, b, _, _)
        | ExprAST::Or(a, b, _) 
        | ExprAST::And(a, b, _)
        | ExprAST::While { condition: a, block: b, .. } => {
            desugar_expression(a);
            desugar_expression(b);
        },
        ExprAST::Not(a, _) => {
            desugar_expression(a);
        }
        ExprAST::FunctionCall(_, exprs, _) => {
            for expr in exprs {
                desugar_expression(expr);
            }
        }
        ExprAST::Block(statements, final_expr, _) => {
            for stmt in statements {
                desugar_statement(stmt);
            }
            if let Some(expr) = final_expr {
                desugar_expression(expr);
            }
        },
        ExprAST::If { condition, block, else_branch, .. } => {
            desugar_expression(condition);
            desugar_expression(block);
            if let Some(else_block) = else_branch {
                desugar_expression(else_block);
            }
        },
        ExprAST::Return(expr, _) => {
            if let Some(expr) = expr {
                desugar_expression(expr);
            }
        },
        ExprAST::IntegerLiteral(_, _)
        | ExprAST::BooleanLiteral(_, _)
        | ExprAST::Variable(_, _) => (),
        ExprAST::Moved => panic!("ExprAST moved"),
    }
}