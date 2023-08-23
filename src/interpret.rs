
use super::ast::{AST, ExprAST};

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;


#[derive(Debug)]
pub struct InterpretError (String);


struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    variables: HashMap<String, i32>,
}

impl Scope {
    /* A completely new scope */
    fn new() -> Scope {
        Scope { parent: None, variables: HashMap::new() }
    }

    fn lookup(&self, name: &str) -> Result<i32, InterpretError> {
        match self.variables.get(name) {
            Some(val) => Ok(*val),
            None => {
                match &self.parent {
                    Some(parent) => parent.borrow().lookup(name), 
                    None => Err(InterpretError(format!("Could not find variable: {}", name))),
                }
            } 
        }
    }
}


pub struct Interpretter {
    global_scope: Rc<RefCell<Scope>>,
}

impl Interpretter {
    pub fn new() -> Interpretter {
        Interpretter { global_scope: Rc::new(RefCell::new(Scope::new())) }
    }

    fn new_subscope(scope: Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
        let mut new_scope = Scope::new();
        new_scope.parent = Some(Rc::clone(&scope));

        Rc::new(RefCell::new(new_scope))
    }

    pub fn run(&mut self, ast: AST) -> Result<(), InterpretError> {
        for declaration in ast.declarations {
            match declaration {
                crate::ast::DeclarationAST::Function { name, block, .. } if name == "main" =>{
                    let value = self.evaluate_expr(block, Self::new_subscope(Rc::clone(&self.global_scope)))?;
                    println!("Main evaluated to {}", value);
                }
                crate::ast::DeclarationAST::Function { .. } => {
                    println!("NotYetImplemented: running code in functions other than main()");
                }
            }
        }
    
        Ok(())
    }

    fn evaluate_expr(&self, ast: ExprAST, scope: Rc<RefCell<Scope>>) -> Result<i32, InterpretError> {
        match ast {
            ExprAST::Add(left, right, ..) => Ok(self.evaluate_expr(*left, Rc::clone(&scope))? + self.evaluate_expr(*right, scope)?),
            ExprAST::Subtract(left, right, ..) => Ok(self.evaluate_expr(*left, Rc::clone(&scope))? - self.evaluate_expr(*right, scope)?),
            ExprAST::Multiply(left, right, ..) => Ok(self.evaluate_expr(*left, Rc::clone(&scope))? * self.evaluate_expr(*right, scope)?),
            ExprAST::Divide(left, right, ..) => Ok(self.evaluate_expr(*left, Rc::clone(&scope))? / self.evaluate_expr(*right, scope)?),
            ExprAST::Literal(i, ..) => Ok(i),
            ExprAST::Block(statements, opt_expr, ..) => {
                let scope = Self::new_subscope(scope);

                /* This needs some work */
                if statements.len() > 0 {
                    println!("NotYetImplemented: {} statement(s) parsed but not run.", statements.len())
                }
                
                if let Some(boxed_expr) = opt_expr {
                    self.evaluate_expr(*boxed_expr, scope)
                }
                else {
                    Err(InterpretError("Not yet implemented: Block without final expression".to_string()))
                }
            }
            ExprAST::Variable(name, ..) => {
                scope.borrow().lookup(&name)
            }
        }
    }
}
