// Desugaring is the process of removing syntactic sugar. This process happens
// early so that later analysis steps can take place with a somewhat normalized
// format.

use std::rc::Rc;

use crate::{ast::{AST, ExprAST, StatementAST, ASTNodeData, AnyAST}, token::Span};


pub(crate) fn desugar(ast: &mut AST)  {
    desugar_ast(&mut AnyAST::File(ast))
}

fn desugar_ast<'a>(ast: &'a mut AnyAST<'a>) {
    match ast {
        /* Compound assignment simply becomes normal assignment after performing the
         * operation. */
        AnyAST::Statement(statement @ StatementAST::CompoundAssignment(..)) => {
            // We unpack here to appease the borrow checker.
            let StatementAST::CompoundAssignment(left, right, op, ..) = statement
                else { panic!("Known to be variant") };

            let left = std::mem::take(left);
            let right = std::mem::take(right);

            let operation = match op {
                crate::ast::MathOperation::Add => ExprAST::Add(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(tmp_span())),
                crate::ast::MathOperation::Subtract => ExprAST::Subtract(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(tmp_span())),
                crate::ast::MathOperation::Multiply => ExprAST::Multiply(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(tmp_span())),
                crate::ast::MathOperation::Divide => ExprAST::Divide(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(tmp_span())),
                crate::ast::MathOperation::Modulus => ExprAST::Modulus(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(tmp_span())),
            };
            
            _ = std::mem::replace(*statement, StatementAST::Assignment(left, operation, ASTNodeData::new(tmp_span())));

            desugar_ast(&mut AnyAST::Statement(statement))
        }
        _ => {
            for mut child in ast.children() {
                desugar_ast(&mut child)
            }
        }
    }
}

// TODO: Remove
fn tmp_span() -> Span {
    Span {
        file: Rc::new("<Replace Me>".into()),
        start_line: 0,
        end_line: 0,
        start_col: 0,
        end_col: 0,
    }
}

// TODO: Desugar final while
// TODO: Figure out what I meant by this ^. Did I possibly mean desugar final return
// into normal final expression?
