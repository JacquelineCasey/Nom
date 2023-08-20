
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
    Block (Vec<StatementAST>, Option<Box<ExprAST>>),
}

#[derive(Debug)]
pub enum StatementAST {
    NotYetImplemented
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
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Expression" => {
            if subexpressions.len() != 1 {
                Err(ASTError("Expected exactly 1 subtree".to_string()))
            }
            else {
                build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))
            }
        }
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "AdditiveExpression" => 
            combine_binary_ops(subexpressions, true, |left, op, right| {
                if matches_token(&op, "+") {
                    Ok(ExprAST::Add(Box::new(left), Box::new(right)))
                }
                else if matches_token(&op, "-") {
                    Ok(ExprAST::Subtract(Box::new(left), Box::new(right)))
                }
                else {
                    Err(ASTError("Expected + or -".to_string()))
                }
            }),
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "MultiplicativeExpression" =>
            combine_binary_ops(subexpressions, true, |left, op, right| {
                if matches_token(&op, "*") {
                    Ok(ExprAST::Multiply(Box::new(left), Box::new(right)))
                }
                else if matches_token(&op, "/") {
                    Ok(ExprAST::Divide(Box::new(left), Box::new(right)))
                }
                else {
                    Err(ASTError("Expected * or /".to_string()))
                }
            }),
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "PrimaryExpression" => {
            let subexpressions = drop_whitespace(subexpressions);
            
            if subexpressions.len() == 1 {  // Literal or Block
                build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))
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
        SyntaxTree::RuleNode { ref rule_name, subexpressions: _ } if rule_name == "Literal" =>
            build_literal_expr(tree),
        SyntaxTree::RuleNode { ref rule_name, subexpressions: _ } if rule_name == "BlockExpression" =>
            build_block_expr(tree),
        SyntaxTree::RuleNode { rule_name, subexpressions: _ } => 
            Err(ASTError(format!("Expected expression, found {}", rule_name))),
        SyntaxTree::TokenNode (tok) => 
            Err(ASTError(format!("Expected expression, found token {}", tok.token_type)))
    }
}


fn build_literal_expr(tree: SyntaxTree<CharToken>) -> Result<ExprAST, ASTError> {
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
            
            Ok(ExprAST::Literal(val))
        }
        _ => Err(ASTError("Expected Literal node".to_string()))
    }
}

fn build_block_expr(tree: SyntaxTree<CharToken>) -> Result<ExprAST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "BlockExpression" => {
            if !matches_token(&subexpressions[0], "{") {
                return Err(ASTError("Expected open bracket before block".to_string()));
            }
            if !matches_token(&subexpressions[subexpressions.len() - 1], "}") {
                return Err(ASTError("Expected closed bracket before block".to_string()));
            }

            let mut subexpressions = subexpressions;
            subexpressions.remove(subexpressions.len() - 1);
            subexpressions.remove(0);

            let subexpressions = drop_whitespace(subexpressions);

            let mut subexpressions: Vec<_> = subexpressions.into_iter()
                .filter(|subtree| !matches_token(subtree, ";"))
                .collect();

            if subexpressions.len() == 0 {
                return Ok(ExprAST::Block(vec![], None));
            }

            let opt_expr = if let SyntaxTree::RuleNode { rule_name, subexpressions: _ } = &subexpressions[subexpressions.len() - 1] {
                if rule_name == "Expression" {
                    let last = subexpressions.remove(subexpressions.len() - 1);
                    
                    Some(Box::new(build_expr_ast(last)?))
                }
                else { None }
            } else { None };

            let statements = subexpressions.into_iter()
                .map(|subtree| build_statement_ast(subtree))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(ExprAST::Block(statements, opt_expr))
        }
        _ => Err(ASTError("Expected Block node".to_string()))
    }
}

fn build_statement_ast(_tree: SyntaxTree<CharToken>) -> Result<StatementAST, ASTError> {
    Ok(StatementAST::NotYetImplemented)
}


fn combine_binary_ops<F>(subtrees: Vec<SyntaxTree<CharToken>>, left_to_right: bool, combine_fn: F) -> Result<ExprAST, ASTError>
    where F: Fn(ExprAST, SyntaxTree<CharToken>, ExprAST) -> Result<ExprAST, ASTError>
{
    let subexpressions = drop_whitespace(subtrees);
    if subexpressions.len() == 1 {
        build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))
    }
    else {
        if left_to_right {
            let mut iterator = subexpressions.into_iter();  // Matches left to right semantics
   
            let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
            while let Some(op) = iterator.next() {
                let right = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
                ast = combine_fn(ast, op, right)?;
            }

            Ok(ast)
        }
        else {
            let mut iterator = subexpressions.into_iter().rev();  // Matches right to left semantics
   
            let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
            while let Some(op) = iterator.next() {
                let left = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
                ast = combine_fn(left, op, ast)?;
            }

            Ok(ast)
        } 
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