
use super::ast::{AST, ExprAST};


#[derive(Debug)]
pub struct InterpretError (String);

pub fn interpret_ast(ast: AST) -> Result<(), InterpretError> {
    println!("{}", evaluate_expr(ast.expression)?);
    Ok(())
}

pub fn evaluate_expr(ast: ExprAST) -> Result<i32, InterpretError> {
    match ast {
        ExprAST::Add(left, right) => Ok(evaluate_expr(*left)? + evaluate_expr(*right)?),
        ExprAST::Subtract(left, right) => Ok(evaluate_expr(*left)? - evaluate_expr(*right)?),
        ExprAST::Multiply(left, right) => Ok(evaluate_expr(*left)? * evaluate_expr(*right)?),
        ExprAST::Divide(left, right) => Ok(evaluate_expr(*left)? / evaluate_expr(*right)?),
        ExprAST::Literal(i) => Ok(i),
    }
}