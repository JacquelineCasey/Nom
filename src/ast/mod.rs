
use parsley::SyntaxTree;

use crate::token::{Token, TokenBody, Operator, Punctuation, Keyword};
use crate::error::ASTError;


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
    Function { name: String, params: Vec<String>, block: ExprAST, node_data: ASTNodeData },
    Variable { mutability: Mutability, name: String, expr: ExprAST, node_data: ASTNodeData }
}

#[derive(Debug, Default)]
pub enum ExprAST {
    Add (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Subtract (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Multiply (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Divide (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Literal (i32, ASTNodeData),
    Variable (String, ASTNodeData),
    Block (Vec<StatementAST>, Option<Box<ExprAST>>, ASTNodeData),
    FunctionCall (String, Vec<ExprAST>, ASTNodeData),  // The vec contains arguments

    #[default]
    Moved,  // This is a hack that allows us to remove an AST, operate on it, and put it back.
            // Blame the borrow checker?
}

impl ExprAST {
    pub fn get_node_data(&self) -> &ASTNodeData {
        use ExprAST as E;

        match self {
            E::Add(_, _, data) 
            | E::Subtract(_, _, data)
            | E::Multiply(_, _, data)
            | E::Divide(_, _, data)
            | E::Literal(_, data)
            | E::Variable(_, data)
            | E::Block(_, _, data) 
            | E::FunctionCall(_, _, data)
            => data,
            E::Moved => panic!("ExprAST was moved"),
        }
    }
}

#[derive(Debug)]
pub enum StatementAST {
    ExpressionStatement (ExprAST, ASTNodeData),  // A expression executed for its side effects
    Assignment (ExprAST, ExprAST, ASTNodeData),  // There are restrictions on wbat goes on the left, but it is ultimately an expression too.
    Declaration (DeclarationAST, ASTNodeData),  // Any declaration will be allowed, but for now only variable declarations work.
}

#[derive(Debug)]
pub enum Mutability {
    Var, 
    Val
}






pub fn build_ast(tree: &SyntaxTree<Token>) -> Result<AST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Program" => {
            Ok(AST { 
                declarations: subexpressions.iter()
                    .map(build_declaration_ast)
                    .collect::<Result<Vec<_>, _>>()?,
                node_data: ASTNodeData::new() 
            })
        }
        _ => Err("Expected Program Node".into()),
    }
}

fn build_declaration_ast(tree: &SyntaxTree<Token>) -> Result<DeclarationAST, ASTError> {
    use SyntaxTree as ST;
    
    match tree {
        ST::RuleNode { rule_name, ref subexpressions } if rule_name == "Declaration" => {
            if subexpressions.len() != 1 {
                return Err("Expected single child of declaration node".into())
            }
                
            match &subexpressions[0] {
                ST::RuleNode { rule_name, ref subexpressions } if rule_name == "FunctionDeclaration" => {
                    if subexpressions.len() != 4 {
                        return Err("Incorrect number of subnodes to function node".into());
                    }
                    
                    if !matches!(subexpressions[0], ST::TokenNode( Token { body: TokenBody::Keyword(Keyword::Fn) })) {
                        return Err("Expected `fn` in function declaration".into());
                    }

                    let name = if let ST::TokenNode( Token { body: TokenBody::Identifier(name) }) = &subexpressions[1] {
                        name.clone()
                    } 
                    else { 
                        return Err("Expected function name".into());
                    };

                    let params = build_parameter_list(&subexpressions[2])?;
                    let block = build_expr_ast(&subexpressions[3])?;

                    Ok(DeclarationAST::Function { name, params, block, node_data: ASTNodeData::new() })
                },
                ST::RuleNode { rule_name, ref subexpressions } if rule_name == "VariableDeclaration" => {
                    match &subexpressions[..] {
                        [ ST::TokenNode(Token { body: TokenBody::Keyword(keyword @ (Keyword::Val | Keyword::Var))})
                        , ST::TokenNode(Token { body: TokenBody::Identifier(name)})
                        , ST::TokenNode(Token { body: TokenBody::Operator(Operator::Equals)})
                        , expr_node @ ST::RuleNode { rule_name: last_rule_name, ..}
                        ] if last_rule_name == "Expression" => {
                            Ok(DeclarationAST::Variable { 
                                mutability: match keyword {
                                    Keyword::Var => Mutability::Var,
                                    Keyword::Val => Mutability::Val,
                                    _ => panic!("Known unreachable")
                                },
                                name: name.clone(), 
                                expr: build_expr_ast(expr_node)?,
                                node_data: ASTNodeData::new()
                            })
                        }
                        _ => Err("Failed to parse variable declaration".into())
                    }
                },
                _ => Err("Expected Function or Variable Declaration Node".into())
            }
        },
        _ => Err("Expected Declaration Node".into())
    }
}

fn build_parameter_list(node: &SyntaxTree<Token>) -> Result<Vec<String>, ASTError> {
    use SyntaxTree as ST;
    use Token as T;
    use TokenBody as TB;

    match node {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "ParameterList" => {
            if !matches!(&subexpressions[0], ST::TokenNode (T {body: TB::Punctuation(Punctuation::LeftParenthesis)})) {
                return Err("Expected left parenthesis".into());
            }
            if !matches!(&subexpressions[subexpressions.len() - 1], ST::TokenNode (T {body: TB::Punctuation(Punctuation::RightParenthesis)})) {
                return Err("Expected right parenthesis".into());
            }

            let list = &subexpressions[1..subexpressions.len() - 1];
            let mut iter = list.iter();
            
            let mut param_names = vec![];
            while let Some(node) = iter.next() {
                let ST::TokenNode(T {body: TB::Identifier(name)}) = node
                    else { return Err("Expected name".into())};
                
                param_names.push(name.clone());

                match iter.next() {
                    Some(ST::TokenNode (T {body: TB::Punctuation(Punctuation::Comma)})) => (),
                    Some(_) => return Err("Expected comma".into()),
                    None => break,  // In case the iterator restarts?
                }
            }

            Ok(param_names)
        }
        _ => Err("Expected parameter list in function declaration".into())
    }
}

fn build_expr_ast(tree: &SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
    use SyntaxTree as ST;

    use Token as T;
    use TokenBody as TB;
    
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == "Expression" => {
            if subexpressions.len() == 1 {
                build_expr_ast(subexpressions.iter().next().expect("Known to exist"))
            }
            else {
                Err("Expected exactly 1 subtree".into())
            }
        }
        ST::RuleNode { rule_name, subexpressions } if rule_name == "AdditiveExpression" => 
            combine_binary_ops(subexpressions, true, |left, op, right| {
                match op {
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Plus) }) 
                        => Ok(ExprAST::Add(Box::new(left), Box::new(right), ASTNodeData::new())),
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Minus) }) 
                        => Ok(ExprAST::Subtract(Box::new(left), Box::new(right), ASTNodeData::new())),
                    _ => Err("Expected + or -".into())
                }
            }),
        ST::RuleNode { rule_name, subexpressions } if rule_name == "MultiplicativeExpression" =>
            combine_binary_ops(subexpressions, true, |left, op, right| {
                match op {
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Times) }) 
                        => Ok(ExprAST::Multiply(Box::new(left), Box::new(right), ASTNodeData::new())),
                    SyntaxTree::TokenNode(Token { body: TokenBody::Operator(Operator::Divide) }) 
                        => Ok(ExprAST::Divide(Box::new(left), Box::new(right), ASTNodeData::new())),
                    _ => Err("Expected * or /".into())
                }
            }),
        ST::RuleNode { rule_name, subexpressions } if rule_name == "PrimaryExpression" => {            
            if subexpressions.len() == 1 {  // Literal, Variable, or Block
                build_expr_ast(subexpressions.iter().next().expect("Known to exist"))
            }
            else if subexpressions.len() == 3 {
                if !matches!(&subexpressions[0], SyntaxTree::TokenNode(Token { body: TokenBody::Punctuation(Punctuation::LeftParenthesis)}))
                    || !matches!(&subexpressions[2], SyntaxTree::TokenNode(Token { body: TokenBody::Punctuation(Punctuation::RightParenthesis)})) {
                    
                    Err("Expected parentheses".into())
                }
                else {
                    build_expr_ast(&subexpressions[1])
                }
            }
            else {
                Err("Wrong number of subtrees at PrimaryExpression".into())
            }
        }
        ST::RuleNode { ref rule_name, subexpressions } if rule_name == "FunctionCall" => {
            let SyntaxTree::TokenNode (T {body: TB::Identifier(name)}) = &subexpressions[0]
                else { return Err("Could not find identifier in FunctionCall".into()) };

            if !matches!(&subexpressions[1], ST::TokenNode (T {body: TB::Punctuation(Punctuation::LeftParenthesis)})) {
                return Err("Expected left parenthesis".into());
            }
            if !matches!(&subexpressions[subexpressions.len() - 1], ST::TokenNode (T {body: TB::Punctuation(Punctuation::RightParenthesis)})) {
                return Err("Expected right parenthesis".into());
            }

            let arg_list = &subexpressions[2..subexpressions.len() - 1];
            let mut iter = arg_list.iter();
            
            let mut expressions = vec![];
            while let Some(node) = iter.next() {
                expressions.push(build_expr_ast(node)?);

                match iter.next() {
                    Some(ST::TokenNode (T {body: TB::Punctuation(Punctuation::Comma)})) => (),
                    Some(_) => return Err("Expected comma".into()),
                    None => break,  // In case the iterator restarts?
                }
            }

            Ok(ExprAST::FunctionCall(name.clone(), expressions, ASTNodeData::new()))
        }
        ST::RuleNode { ref rule_name, subexpressions: _ } if rule_name == "Literal" =>
            build_literal_expr(tree),
        ST::RuleNode { ref rule_name, subexpressions: _ } if rule_name == "BlockExpression" =>
            build_block_expr(tree),
        ST::RuleNode { rule_name, subexpressions: _ } => 
            Err(format!("Expected expression, found {rule_name}").into()),
        ST::TokenNode (T { body: TB::Identifier(name) }) =>
            Ok(ExprAST::Variable(name.clone(), ASTNodeData::new() )),
        ST::TokenNode (tok) => 
            Err(format!("Expected expression, found token {tok}").into())
    }
}


fn build_literal_expr(tree: &SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "Literal" => {
            if subexpressions.len() != 1 {
                return Err("Wrong number of subnodes to node Literal".into())
            }

            let child = &subexpressions[0];

            match child {
                SyntaxTree::RuleNode { .. } => Err("Rule node under Literal node".into()),
                SyntaxTree::TokenNode(Token { body: TokenBody::NumericLiteral(str) }) => 
                    Ok(ExprAST::Literal(str.parse().map_err(|_| ASTError("Integer parse failed".to_string()))?, ASTNodeData::new())),
                SyntaxTree::TokenNode(_) => Err("Non numeric literal under Literal node".into())
            }
        }
        _ => Err("Expected Literal node".into())
    }
}

fn build_block_expr(tree: &SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "BlockExpression" => {
            if !matches!(subexpressions[0], SyntaxTree::TokenNode(Token {body: TokenBody::Punctuation(Punctuation::LeftCurlyBrace)})) {
                return Err("Expected open bracket before block".into());
            }
            if !matches!(subexpressions[subexpressions.len() - 1], SyntaxTree::TokenNode(Token {body: TokenBody::Punctuation(Punctuation::RightCurlyBrace)})) {
                return Err("Expected closed bracket before block".into());
            }

            let subexpressions = &subexpressions[1..subexpressions.len()-1];

            let mut subexpressions: Vec<_> = subexpressions.iter()
                .filter(|subtree| !matches!(subtree, SyntaxTree::TokenNode(Token {body: TokenBody::Punctuation(Punctuation::Semicolon)})))
                .collect();

            if subexpressions.is_empty() {
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
                .map(build_statement_ast)
                .collect::<Result<Vec<_>, _>>()?;

            Ok(ExprAST::Block(statements, opt_expr, ASTNodeData::new()))
        }
        _ => Err("Expected Block node".into())
    }
}

fn build_statement_ast(tree: &SyntaxTree<Token>) -> Result<StatementAST, ASTError> {
    match tree {
        SyntaxTree::RuleNode { rule_name, ref subexpressions } if rule_name == "Statement" => {
            match &subexpressions[..] {
                [SyntaxTree::RuleNode { rule_name, subexpressions }] if rule_name == "Expression" => 
                    Ok(StatementAST::ExpressionStatement(build_expr_ast(&subexpressions[0])?, ASTNodeData::new())),
                [SyntaxTree::RuleNode { rule_name, subexpressions }] if rule_name == "AssignmentStatement" => {
                    match &subexpressions[..] {
                        [ SyntaxTree::RuleNode { rule_name: rule_1, subexpressions: ref sub_expr_1 }
                        , SyntaxTree::TokenNode (Token { body: TokenBody::Operator(Operator::Equals) })
                        , SyntaxTree::RuleNode { rule_name: rule_2, subexpressions: ref sub_expr_2 }
                        ] if rule_1 == "Expression" 
                        && rule_2 == "Expression" 
                        && sub_expr_1.len() == 1 
                        && sub_expr_2.len() == 1 => {
                            let left = build_expr_ast(&sub_expr_1[0])?;
                            let right = build_expr_ast(&sub_expr_2[0])?;

                            // TODO: Validation?

                            Ok(StatementAST::Assignment(left, right, ASTNodeData::new()))
                        }
                    _ => Err("Failed to build AssignmentStatement".into())
                    }
                },
                [SyntaxTree::RuleNode { rule_name, .. }] if rule_name == "Declaration" => {
                    Ok(StatementAST::Declaration(build_declaration_ast(&subexpressions[0])?, ASTNodeData::new()))
                },
                _ => Err("Failed to build AssignmentStatement".into())
            }
        },
        _ => Err("Expected statement node".into())
    }
}


fn combine_binary_ops<F>(subtrees: &[SyntaxTree<Token>], left_to_right: bool, combine_fn: F) -> Result<ExprAST, ASTError>
    where F: Fn(ExprAST, &SyntaxTree<Token>, ExprAST) -> Result<ExprAST, ASTError>
{
    if subtrees.len() == 1 {
        build_expr_ast(subtrees.iter().next().expect("Known to exist"))
    }
    else if left_to_right {
        let mut iterator = subtrees.iter();  // Matches left to right semantics

        let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

        while let Some(op) = iterator.next() {
            let right = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

            ast = combine_fn(ast, op, right)?;
        }

        Ok(ast)
    }
    else {
        let mut iterator = subtrees.iter().rev();  // Matches right to left semantics

        let mut ast = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

        while let Some(op) = iterator.next() {
            let left = build_expr_ast(iterator.next().ok_or(ASTError("Expected subtree".to_string()))?)?;

            ast = combine_fn(left, op, ast)?;
        }

        Ok(ast)
    } 
}
