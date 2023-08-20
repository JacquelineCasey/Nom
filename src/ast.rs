
use parsley::{CharToken, SyntaxTree};

/* Currently our program AST is just a single expression */
#[derive(Debug)]
pub struct AST {
    pub expression: ExprAST,
}

#[derive(Debug)]
pub enum ExprAST {
    Add (Box<ExprAST>, Box<ExprAST>),
    Subtract (Box<ExprAST>, Box<ExprAST>),
    Multiply (Box<ExprAST>, Box<ExprAST>),
    Divide (Box<ExprAST>, Box<ExprAST>),
    Literal (i32),
}

#[derive(Debug)]
pub struct ASTError (String);


pub fn build_ast(tree: SyntaxTree<CharToken>) -> Result<AST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Program" => {
            let subexpressions = drop_whitespace(subexpressions);

            if subexpressions.len() == 1 {
                Ok(AST { expression: build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))? })
            }
            else {
                Err(ASTError("Incorrect number of expressions at top level".to_string()))
            }
        }
        _ => Err(ASTError("Expected Expression Node".to_string())),
    }
}

fn build_expr_ast(tree: SyntaxTree<CharToken>) -> Result<ExprAST, ASTError> {
    // TODO - many of the branches are similar. Try to factor out the folding logic,
    // may require a closure.
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Expression" => {
            if subexpressions.len() != 1 {
                Err(ASTError("Expected exactly 1 subtree".to_string()))
            }
            else {
                build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))
            }
        }
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "AdditiveExpression" => {
            let subexpressions = drop_whitespace(subexpressions);
            if subexpressions.len() == 1 {
                build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))
            }
            else {
                let mut iterator = subexpressions.into_iter().rev();  // Right to Left
                
                let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

                while let Some(op) = iterator.next() {
                    let left = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

                    ast = match op {
                        SyntaxTree::TokenNode(tok) if tok.token_type == "+"  => {
                            ExprAST::Add(Box::new(left), Box::new(ast))
                        }
                        SyntaxTree::TokenNode(tok) if tok.token_type == "-"  => {
                            ExprAST::Subtract(Box::new(left), Box::new(ast))
                        },
                        _ => Err(ASTError("Expected + or -".to_string()))?
                    }
                }
                    
                Ok(ast)
            }
        },
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "MultiplicativeExpression" => {
            let subexpressions = drop_whitespace(subexpressions);
            if subexpressions.len() == 1 {
                build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))
            }
            else {
                let mut iterator = subexpressions.into_iter().rev();  // Right to Left
                
                let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

                while let Some(op) = iterator.next() {
                    let left = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

                    ast = match op {
                        SyntaxTree::TokenNode(tok) if tok.token_type == "*"  => {
                            ExprAST::Multiply(Box::new(left), Box::new(ast))
                        }
                        SyntaxTree::TokenNode(tok) if tok.token_type == "/"  => {
                            ExprAST::Divide(Box::new(left), Box::new(ast))
                        },
                        _ => Err(ASTError("Expected + or -".to_string()))?
                    }
                }
                    
                Ok(ast)
            }
        },
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "PrimaryExpression" => {
            let subexpressions = drop_whitespace(subexpressions);
            if subexpressions.len() == 1 {
                Ok(ExprAST::Literal(build_literal(subexpressions.into_iter().next().expect("Known to exist"))?))
            }
            else if subexpressions.len() == 3 {
                if !matches_token(&subexpressions[0], "(") || !matches_token(&subexpressions[2], ")") {
                    Err(ASTError("Expected parentheses".to_string()))
                }
                else {
                    build_expr_ast(subexpressions.into_iter().nth(1).expect("Known to exist"))
                }
            }
            else {
                Err(ASTError("Wrong number of subtrees at PrimaryExpression".to_string()))
            }
        }
        SyntaxTree::RuleNode { rule_name, subexpressions: _ } => 
            Err(ASTError(format!("Expected expression, found {}", rule_name))),
        SyntaxTree::TokenNode (tok) => 
            Err(ASTError(format!("Expected expression, found token {}", tok.token_type)))
    }
}

fn build_literal(tree: SyntaxTree<CharToken>) -> Result<i32, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Literal" => {
            let mut val: i32 = 0;

            for expr in subexpressions {
                if let SyntaxTree::RuleNode { rule_name, subexpressions } = expr {
                    if rule_name != "Digit" {
                        return Err(ASTError("Expected Digit".to_string()));
                    }

                    let subexpr = subexpressions.into_iter().next().expect("Known to exist");

                    if let SyntaxTree::TokenNode(tok) = subexpr {
                        let ch = tok.token_type.chars().next().expect("Known to exist");
                        
                        if ch > '9' || ch < '0' {
                            return Err(ASTError("Expected Digit".to_string())) 
                        }

                        val *= 10;
                        val += (ch as i32) - ('0' as i32);
                    } 


                }
                else { return Err(ASTError("Expected Literal node".to_string())) }
            }
            
            Ok(val)
        }
        _ => Err(ASTError("Expected Literal node".to_string()))
    }
}

fn drop_whitespace(trees: Vec<SyntaxTree<CharToken>>) -> Vec<SyntaxTree<CharToken>> {
    trees.into_iter()
        .filter(|subtree| {
            if let SyntaxTree::RuleNode { rule_name, subexpressions: _ } = subtree {
                rule_name != "WS"
            }
            else { true }
        })
        .collect()
}

fn matches_token(tree: &SyntaxTree<CharToken>, expected: &str) -> bool {
    match tree {
        SyntaxTree::RuleNode { rule_name: _, subexpressions: _ } => false,
        SyntaxTree::TokenNode(tok) => tok.token_type == expected,
    }
}
