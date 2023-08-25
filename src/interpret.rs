
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

    /* Looks up a variable recursively. */
    fn lookup(&self, name: &str) -> Result<i32, InterpretError> {
        match self.variables.get(name) {
            Some(val) => Ok(*val),
            None => {
                match &self.parent {
                    Some(parent) => parent.borrow().lookup(name), 
                    None => Err(InterpretError(format!("Could not find variable: {name}"))),
                }
            } 
        }
    }
    
    /* Looks up a variable recursively, then sets it. Failure to find the variable
     * yields an error - use add() instead. */
    fn set(&mut self, name: &str, val: i32) -> Result<(), InterpretError> {
        match self.variables.get(name) {
            Some(_) => {
                self.variables.insert(name.to_string(), val);
                Ok(())
            },
            None => {
                match &self.parent {
                    Some(parent) => parent.borrow_mut().set(name, val), 
                    None => Err(InterpretError(format!("Could not find variable: {name}"))),
                }
            } 
        }
    }

    /* Adds and sets a variable in the local scope */
    fn add(&mut self, name: &str, val: i32) -> Result<(), InterpretError> {
        // No shadowing for now.
        if self.variables.contains_key(name) {
            Err(InterpretError(format!("Cannot shadow local variable {name}")))
        }
        else {
            self.variables.insert(name.to_string(), val);
            Ok(())
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

    fn new_subscope(scope: &Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
        let mut new_scope = Scope::new();
        new_scope.parent = Some(Rc::clone(scope));

        Rc::new(RefCell::new(new_scope))
    }

    pub fn run(&mut self, ast: AST) -> Result<(), InterpretError> {
        for declaration in ast.declarations {
            match declaration {
                crate::ast::DeclarationAST::Function { name, block, .. } if name == "main" =>{
                    let value = self.evaluate_expr(block, &Self::new_subscope(&self.global_scope))?;
                    println!("Main evaluated to {value}");
                }
                crate::ast::DeclarationAST::Function { .. } => {
                    println!("NotYetImplemented: running code in functions other than main()");
                }
                _ => {
                    todo!()
                }
            }
        }
    
        Ok(())
    }

    fn evaluate_expr(&self, ast: ExprAST, scope: &Rc<RefCell<Scope>>) -> Result<i32, InterpretError> {
        match ast {
            ExprAST::Add(left, right, ..) => Ok(self.evaluate_expr(*left, scope)? + self.evaluate_expr(*right, scope)?),
            ExprAST::Subtract(left, right, ..) => Ok(self.evaluate_expr(*left, scope)? - self.evaluate_expr(*right, scope)?),
            ExprAST::Multiply(left, right, ..) => Ok(self.evaluate_expr(*left, scope)? * self.evaluate_expr(*right, scope)?),
            ExprAST::Divide(left, right, ..) => Ok(self.evaluate_expr(*left, scope)? / self.evaluate_expr(*right, scope)?),
            ExprAST::Literal(i, ..) => Ok(i),
            ExprAST::Block(statements, opt_expr, ..) => {
                let scope = Self::new_subscope(scope);

                for stmt in statements {
                    self.evaluate_statement(stmt, &scope)?;
                }
                
                if let Some(boxed_expr) = opt_expr {
                    self.evaluate_expr(*boxed_expr, &scope)
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

    fn evaluate_statement(&self, stmt: crate::ast::StatementAST, scope: &Rc<RefCell<Scope>>) -> Result<(), InterpretError> {
        match stmt {
            crate::ast::StatementAST::ExpressionStatement(expr, ..) => {
                self.evaluate_expr(expr, scope)
                    .map(|_| ())
            }
            crate::ast::StatementAST::Assignment(left, right, ..) => {
                let val = self.evaluate_expr(right, scope)?;

                // It needs to be an identifier
                if let ExprAST::Variable(name, ..) = left {
                    scope.borrow_mut().set(&name, val)
                }
                else {
                    Err(InterpretError("Tried to assign to non variable expression".to_string()))
                }
            }
            crate::ast::StatementAST::Declaration(decl, ..) => {
                match decl {
                    crate::ast::DeclarationAST::Function { .. } => 
                        Err(InterpretError("Cannot process function declaration in function".to_string())),
                    crate::ast::DeclarationAST::Variable { name, expr, .. } => {
                        let val = self.evaluate_expr(expr, scope)?;

                        scope.borrow_mut().add(&name, val)
                    }
                }
            }
        }
    }
}

impl Default for Interpretter {
    fn default() -> Self {
        Self::new()
    }
}
