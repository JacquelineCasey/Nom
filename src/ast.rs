
use parsley::SyntaxTree;

use crate::token::{Token, TokenBody, Operator, Punctuation, Keyword};


/* Data shared by every AST Node */
#[derive(Debug)]
pub struct ASTNodeData {
    pub id: u32, // Unique id
}

impl ASTNodeData {
    fn new() -> ASTNodeData {
        ASTNodeData { id: crate::util::next_id() }
    }
}


/* Currently our program AST is just a single expression */
#[derive(Debug)]
pub struct AST {
    pub declarations: Vec<DeclarationAST>,
    pub node_data: ASTNodeData,
}

#[derive(Debug)]
pub enum DeclarationAST {
    Function { name: String, block: ExprAST, node_data: ASTNodeData }
}

#[derive(Debug)]
pub enum ExprAST {
    Add (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Subtract (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Multiply (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Divide (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Literal (i32, ASTNodeData),
    Block (Vec<StatementAST>, Option<Box<ExprAST>>, ASTNodeData),
}

#[derive(Debug)]
pub enum StatementAST {
    NotYetImplemented
}

#[derive(Debug)]
pub struct ASTError (String);


pub fn build_ast(tree: SyntaxTree<Token>) -> Result<AST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Program" => {
            Ok(AST { 
                declarations: subexpressions.into_iter()
                    .map(|subtree| build_declaration_ast(subtree))
                    .collect::<Result<Vec<_>, _>>()?,
                node_data: ASTNodeData::new() 
            })
        }
        _ => Err(ASTError("Expected Program Node".to_string())),
    }
}

fn build_declaration_ast(tree: SyntaxTree<Token>) -> Result<DeclarationAST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, mut subexpressions } if rule_name == "Declaration" => {
            if subexpressions.len() != 1 {
                Err(ASTError("Expected single child of declaration node".to_string()))
            }
            else {
                match subexpressions.remove(0) {
                    SyntaxTree::RuleNode { rule_name, mut subexpressions } if rule_name == "FunctionDeclaration" => {
                        if subexpressions.len() != 4 {
                            return Err(ASTError("Incorrect number of subnodes to function node".to_string()));
                        }
                        
                        if !matches!(subexpressions[0], SyntaxTree::TokenNode( Token { body: TokenBody::Keyword(Keyword::Fn) })) {
                            return Err(ASTError("Expected `fn` in function declaration".to_string()));
                        }

                        let name = if let SyntaxTree::TokenNode( Token { body: TokenBody::Identifier(name) }) = &subexpressions[1] {
                            name.clone()
                        } 
                        else { 
                            return Err(ASTError("Expected function name".to_string()));
                        };

                        /* TODO: Parse the arguments */
                        if !matches!(&subexpressions[2], SyntaxTree::RuleNode { rule_name, .. } if rule_name == "ParameterList") {
                            return Err(ASTError("Expected parameter list in function declaration".to_string()));
                        }

                        let block = build_expr_ast(subexpressions.remove(3))?;

                        Ok(DeclarationAST::Function { name, block, node_data: ASTNodeData::new() })
                    },
                    _ => Err(ASTError("Expected Function Declaration Node".to_string()))
                }
            }
        },
        _ => Err(ASTError("Expected Declaration Node".to_string()))
    }
}

fn build_expr_ast(tree: SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
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
                match op {
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Plus) }) 
                        => Ok(ExprAST::Add(Box::new(left), Box::new(right), ASTNodeData::new())),
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Minus) }) 
                        => Ok(ExprAST::Subtract(Box::new(left), Box::new(right), ASTNodeData::new())),
                    _ => Err(ASTError("Expected + or -".to_string()))
                }
            }),
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "MultiplicativeExpression" =>
            combine_binary_ops(subexpressions, true, |left, op, right| {
                match op {
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Times) }) 
                        => Ok(ExprAST::Multiply(Box::new(left), Box::new(right), ASTNodeData::new())),
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Divide) }) 
                        => Ok(ExprAST::Divide(Box::new(left), Box::new(right), ASTNodeData::new())),
                    _ => Err(ASTError("Expected * or /".to_string()))
                }
            }),
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "PrimaryExpression" => {            
            if subexpressions.len() == 1 {  // Literal or Block
                build_expr_ast(subexpressions.into_iter().next().expect("Known to exist"))
            }
            else if subexpressions.len() == 3 {
                if !matches!(&subexpressions[0], SyntaxTree::TokenNode(Token { body: TokenBody::Punctuation(Punctuation::LeftParenthesis)}))
                    || !matches!(&subexpressions[2], SyntaxTree::TokenNode(Token { body: TokenBody::Punctuation(Punctuation::RightParenthesis)})) {
                    
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
            Err(ASTError(format!("Expected expression, found token {}", tok)))
    }
}


fn build_literal_expr(tree: SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Literal" => {
            if subexpressions.len() != 1 {
                return Err(ASTError("Wrong number of subnodes to node Literal".to_string()))
            }

            let child = &subexpressions[0];

            match child {
                SyntaxTree::RuleNode { .. } => Err(ASTError("Rule node under Literal node".to_string())),
                SyntaxTree::TokenNode(Token { body: TokenBody::NumericLiteral(str) }) => 
                    Ok(ExprAST::Literal(str.parse().map_err(|_| ASTError("Integer parse failed".to_string()))?, ASTNodeData::new())),
                _ => Err(ASTError("Non numeric literal under Literal node".to_string()))
            }
        }
        _ => Err(ASTError("Expected Literal node".to_string()))
    }
}

fn build_block_expr(tree: SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "BlockExpression" => {
            if !matches!(subexpressions[0], SyntaxTree::TokenNode(Token {body: TokenBody::Punctuation(Punctuation::LeftCurlyBrace)})) {
                return Err(ASTError("Expected open bracket before block".to_string()));
            }
            if !matches!(subexpressions[subexpressions.len() - 1], SyntaxTree::TokenNode(Token {body: TokenBody::Punctuation(Punctuation::RightCurlyBrace)})) {
                return Err(ASTError("Expected closed bracket before block".to_string()));
            }

            let mut subexpressions = subexpressions;
            subexpressions.remove(subexpressions.len() - 1);
            subexpressions.remove(0);

            let mut subexpressions: Vec<_> = subexpressions.into_iter()
                .filter(|subtree| !matches!(subtree, SyntaxTree::TokenNode(Token {body: TokenBody::Punctuation(Punctuation::Semicolon)})))
                .collect();

            if subexpressions.len() == 0 {
                return Ok(ExprAST::Block(vec![], None, ASTNodeData::new()));
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

            Ok(ExprAST::Block(statements, opt_expr, ASTNodeData::new()))
        }
        _ => Err(ASTError("Expected Block node".to_string()))
    }
}

fn build_statement_ast(_tree: SyntaxTree<Token>) -> Result<StatementAST, ASTError> {
    Ok(StatementAST::NotYetImplemented)
}


fn combine_binary_ops<F>(subtrees: Vec<SyntaxTree<Token>>, left_to_right: bool, combine_fn: F) -> Result<ExprAST, ASTError>
    where F: Fn(ExprAST, SyntaxTree<Token>, ExprAST) -> Result<ExprAST, ASTError>
{
    if subtrees.len() == 1 {
        build_expr_ast(subtrees.into_iter().next().expect("Known to exist"))
    }
    else {
        if left_to_right {
            let mut iterator = subtrees.into_iter();  // Matches left to right semantics
   
            let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
            while let Some(op) = iterator.next() {
                let right = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
                ast = combine_fn(ast, op, right)?;
            }

            Ok(ast)
        }
        else {
            let mut iterator = subtrees.into_iter().rev();  // Matches right to left semantics
   
            let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
            while let Some(op) = iterator.next() {
                let left = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;
    
                ast = combine_fn(left, op, ast)?;
            }

            Ok(ast)
        } 
    }
}
