// Desugaring is the process of removing syntactic sugar. This process happens
// early so that later analysis steps can take place with a somewhat normalized
// format.

use crate::ast::{AST, ExprAST, StatementAST, ASTNodeData, AnyAST};


pub(crate) fn desugar(ast: &mut AST)  {
    desugar_ast(AnyAST::File(ast))
}

fn desugar_ast(ast: AnyAST) {
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
                crate::ast::MathOperation::Add => ExprAST::Add(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
                crate::ast::MathOperation::Subtract => ExprAST::Subtract(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
                crate::ast::MathOperation::Multiply => ExprAST::Multiply(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
                crate::ast::MathOperation::Divide => ExprAST::Divide(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
                crate::ast::MathOperation::Modulus => ExprAST::Modulus(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new()),
            };

            _ = std::mem::replace(statement, StatementAST::Assignment(left, operation, ASTNodeData::new()));

            desugar_ast(AnyAST::Statement(statement))
        }
        _ => {
            for child in ast.children() {
                desugar_ast(child)
            }
        }
    }
}

// TODO: Desugar final while
// TODO: Figure out what I meant by this ^. Did I possibly mean desugar final return
// into normal final expression?
