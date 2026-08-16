//! Desugaring is the process of removing syntactic sugar. This process happens early so that later analysis steps can
//! take place with a somewhat normalized format.

use super::Type;
use crate::ast::{ASTNodeData, AnyAST, ExprAST, StatementAST, AST};
use crate::error::AnalysisError;
use crate::token::Span;
use crate::CompilationEnvironment;

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
pub fn desugar_after_type_check(env: &mut CompilationEnvironment, function_name: &str) -> Result<(), AnalysisError> {
    let function = env.functions.get_mut(function_name).ok_or(AnalysisError("Could not find function".into()))?;
    let mut block = std::mem::take(&mut function.ast);

    desugar_after_ast_type_check_recursive(env, &mut AnyAST::Expression(&mut block));

    // Look up function again so that previous `function` variable lifetime doesn't coincide the env borrow above.
    env.functions.get_mut(function_name).expect("known to exist").ast = block;
    Ok(())
}

fn desugar_after_ast_build_recursive<'a>(ast: &'a mut AnyAST<'a>) {
    match ast {
        AnyAST::Statement(statement @ StatementAST::CompoundAssignment(..)) => {
            // Compound assignment simply becomes normal assignment after performing the operation.

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

            **statement = StatementAST::Assignment(left, operation, ASTNodeData::new(span));

            desugar_after_ast_build_recursive(&mut AnyAST::Statement(statement));
        }
        _ => {
            for mut child in ast.children() {
                desugar_after_ast_build_recursive(&mut child);
            }
        }
    }
}

fn desugar_after_ast_type_check_recursive<'a>(env: &mut CompilationEnvironment, ast: &'a mut AnyAST<'a>) {
    match ast {
        AnyAST::Expression(ExprAST::MemberAccess(left, _, _)) => {
            if let Type::Pointer(pointee_type) = env.type_index.get(&left.get_node_data().id).expect("Types known") {
                // "Member access" to a pointer is syntactic sugar for pointer access followed by member access.

                let new_node_data = left.get_node_data().relabel();
                let pointer_access = ExprAST::PointerAccess(std::mem::take(left), new_node_data);
                env.type_index.insert(pointer_access.get_node_data().id, *pointee_type.clone());

                **left = pointer_access;

                // Run again on full tree in case left is still a pointer.
                desugar_after_ast_type_check_recursive(env, ast);
            } else {
                desugar_after_ast_type_check_recursive(env, &mut AnyAST::Expression(left));
            }
        }
        _ => {
            for mut child in ast.children() {
                desugar_after_ast_type_check_recursive(env, &mut child);
            }
        }
    }
}
