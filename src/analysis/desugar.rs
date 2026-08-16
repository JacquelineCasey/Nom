// Desugaring is the process of removing syntactic sugar. This process happens
// early so that later analysis steps can take place with a somewhat normalized
// format.

use crate::{
    ast::{ASTNodeData, AnyAST, ExprAST, StatementAST, AST},
    token::Span,
};

// Desugaring occurs at several places, so the functions here are divided into separate stages.

/// Desugaring that occurs on the AST immediately after it has been built. Type / scope information has not been built,
/// so these are naive program transformations, essentially turning fancy syntax into simpler syntax.
pub fn desugar_after_ast_build(ast: &mut AST) {
    desugar_after_ast_build_recursive(&mut AnyAST::File(ast));
}

/// Desugaring that occurs on the AST after type checking. Since type information is known, these transformations can
/// do things like "only change this if we know X is a pointer", and so on. One downside to putting a transformation
/// here is that it must inform the rest of the compilation process of the type of the new expression, and the type
/// system has to work with the pre-transformed code even in cases where that otherwise wouldn't be necessary. So the
/// benefit is lower and the cost is higher, but these transformations are still a good idea because custom code-gen
/// is tricky, and having near-duplicate structures there is brittle.
pub fn desugar_after_type_check(ast: &mut AST /* TODO add something for type info */) {
    // desugar_after_type_check_recursive();
    todo!() // Add a typed_desugar step to turn ptr.x into ptr.*.x;
}

fn desugar_after_ast_build_recursive<'a>(ast: &'a mut AnyAST<'a>) {
    match ast {
        /* Compound assignment simply becomes normal assignment after performing the
         * operation. */
        AnyAST::Statement(statement @ StatementAST::CompoundAssignment(..)) => {
            // We unpack here to appease the borrow checker.
            let StatementAST::CompoundAssignment(left, right, op, ..) = statement else {
                panic!("Known to be variant")
            };

            let left = std::mem::take(left);
            let right = std::mem::take(right);

            let span = Span::combine(&left.get_node_data().span, &right.get_node_data().span);

            let operation = match op {
                crate::ast::MathOperation::Add => {
                    ExprAST::Add(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(span.clone()))
                }
                crate::ast::MathOperation::Subtract => {
                    ExprAST::Subtract(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(span.clone()))
                }
                crate::ast::MathOperation::Multiply => {
                    ExprAST::Multiply(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(span.clone()))
                }
                crate::ast::MathOperation::Divide => {
                    ExprAST::Divide(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(span.clone()))
                }
                crate::ast::MathOperation::Modulus => {
                    ExprAST::Modulus(Box::new(left.duplicate()), Box::new(right), ASTNodeData::new(span.clone()))
                }
            };

            _ = std::mem::replace(*statement, StatementAST::Assignment(left, operation, ASTNodeData::new(span)));

            desugar_after_ast_build_recursive(&mut AnyAST::Statement(statement));
        }
        _ => {
            for mut child in ast.children() {
                desugar_after_ast_build_recursive(&mut child);
            }
        }
    }
}

// TODO: Desugar final while
// TODO: Figure out what I meant by this ^. Did I possibly mean desugar final return
// into normal final expression?
