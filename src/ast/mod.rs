
use parsley::SyntaxTree;

use crate::token::{Token, TokenBody, Operator, Punctuation, Keyword};
use crate::instructions::Comparison;
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
    // The parameters are pairs of names and type ascriptions
    Function { name: String, params: Vec<(String, String)>, block: ExprAST, return_type: String, node_data: ASTNodeData },
    Variable { mutability: Mutability, name: String, expr: ExprAST, type_ascription: Option<String> , node_data: ASTNodeData }
}

#[derive(Debug, Default)]
pub enum ExprAST {
    Add (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Subtract (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Multiply (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Divide (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Comparison (Box<ExprAST>, Box<ExprAST>, Comparison, ASTNodeData),
    IntegerLiteral(i128, ASTNodeData), // i128 can fit all of our literals, up to u64 and i64. Whether a literal fits in a specific type is decided later.
    BooleanLiteral(bool, ASTNodeData),
    Variable (String, ASTNodeData),
    Block (Vec<StatementAST>, Option<Box<ExprAST>>, ASTNodeData),
    FunctionCall (String, Vec<ExprAST>, ASTNodeData),  // The vec contains arguments
    If { condition: Box<ExprAST>, block: Box<ExprAST>, data: ASTNodeData }, // Else to be added later.

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
            | E::Comparison(_, _, _, data)
            | E::IntegerLiteral(_, data)
            | E::Variable(_, data)
            | E::Block(_, _, data) 
            | E::FunctionCall(_, _, data)
            | E::BooleanLiteral(_, data)
            | E::If { data, .. }
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

    let ST::RuleNode { rule_name, subexpressions } = tree
        else { return Err("Expected Rule node".into()); };
    
    if rule_name != "Declaration" {
        return Err("Expected Declaration node".into());
    }

    if subexpressions.len() != 1 {
        return Err("Expected single child of declaration node".into())
    }
        
    match &subexpressions[0] {
        ST::RuleNode { rule_name, ref subexpressions } if rule_name == "FunctionDeclaration" => {
            if subexpressions.len() != 6 {
                return Err("Incorrect number of subnodes to function node".into());
            }
            
            if !matches!(subexpressions[0], ST::TokenNode(Token { body: TokenBody::Keyword(Keyword::Fn) })) {
                return Err("Expected `fn` in function declaration".into());
            }

            let name = if let ST::TokenNode(Token { body: TokenBody::Identifier(name) }) = &subexpressions[1] {
                name.clone()
            } 
            else { 
                return Err("Expected function name".into());
            };

            if !matches!(subexpressions[3], ST::TokenNode(Token {body: TokenBody::Operator(Operator::ThinRightArrow)})) {
                return Err("Expected `->` in function declaration".into());
            }

            let return_type = build_type(&subexpressions[4])?;

            let params = build_parameter_list(&subexpressions[2])?;
            let block = build_expr_ast(&subexpressions[5])?;

            Ok(DeclarationAST::Function { name, params, block, node_data: ASTNodeData::new(), return_type })
        },
        ST::RuleNode { rule_name, ref subexpressions } if rule_name == "VariableDeclaration" => {
            match &subexpressions[..] {
                [ ST::TokenNode(Token { body: TokenBody::Keyword(keyword @ (Keyword::Val | Keyword::Var))})
                , ST::TokenNode(Token { body: TokenBody::Identifier(name)})
                , ..
                , ST::TokenNode(Token { body: TokenBody::Operator(Operator::Equals)})
                , expr_node @ ST::RuleNode { rule_name: last_rule_name, ..}
                ] if last_rule_name == "Expression" => {
                    let mutability = match keyword {
                        Keyword::Var => Mutability::Var,
                        Keyword::Val => Mutability::Val,
                        _ => panic!("Known unreachable")
                    };

                    let type_ascription = if subexpressions.len() == 6 {
                        match &subexpressions[2..4] {
                            [ ST::TokenNode(Token { body: TokenBody::Punctuation(Punctuation::Colon)})
                            , type_node
                            ] => Some(build_type(type_node)?),
                            _ => return Err("Expected type".into()),
                        }
                    }
                    else { None };

                    Ok(DeclarationAST::Variable { 
                        mutability,
                        name: name.clone(), 
                        expr: build_expr_ast(expr_node)?,
                        node_data: ASTNodeData::new(),
                        type_ascription,
                    })
                }
                _ => Err("Failed to parse variable declaration".into())
            }
        },
        _ => Err("Expected Function or Variable Declaration Node".into())
    }
}

fn build_parameter_list(node: &SyntaxTree<Token>) -> Result<Vec<(String, String)>, ASTError> {
    use SyntaxTree as ST;
    use Token as T;
    use TokenBody as TB;

    let SyntaxTree::RuleNode { rule_name, subexpressions } = node
        else { return Err("Expected Rule node".into()); };

    if rule_name != "ParameterList" {
        return Err("Expected parameter list in function declaration".into());
    }


    if !matches!(&subexpressions[0], ST::TokenNode (T {body: TB::Punctuation(Punctuation::LeftParenthesis)})) {
        return Err("Expected left parenthesis".into());
    }
    if !matches!(&subexpressions[subexpressions.len() - 1], ST::TokenNode (T {body: TB::Punctuation(Punctuation::RightParenthesis)})) {
        return Err("Expected right parenthesis".into());
    }

    let list = &subexpressions[1..subexpressions.len() - 1];
    let mut iter = list.iter().peekable();
    
    let mut parameters = vec![];
    while let Some(node) = iter.next() {
        let ST::TokenNode(T {body: TB::Identifier(name)}) = node
            else { return Err("Expected name".into()) };
                
        let Some(ST::TokenNode (T {body: TB::Punctuation(Punctuation::Colon)})) = iter.next()
            else { return Err("Expected colon".into()) };

        let param_type = build_type(iter.next().ok_or(ASTError::from("Expected type node"))?)?;
        
        parameters.push((name.clone(), param_type));

        match iter.next() {
            Some(ST::TokenNode (T {body: TB::Punctuation(Punctuation::Comma)})) => (),
            Some(_) => return Err("Expected comma".into()),
            None => break,  // In case the iterator restarts?
        }
    }

    Ok(parameters)
}

fn build_type(tree: &SyntaxTree<Token>) -> Result<String, ASTError> {
    if let SyntaxTree::RuleNode { rule_name, subexpressions } = tree {
        if rule_name == "Type" {
            if let SyntaxTree::TokenNode(Token {body: TokenBody::Identifier(ident)}) = &subexpressions[0] {
                return Ok(ident.clone());
            }
        }
    }

    Err("Could not build Type node".into())
}

fn build_expr_ast(tree: &SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
    use SyntaxTree as ST;
    use Token as T;
    use TokenBody as TB;
    
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == "Expression" => {
            if subexpressions.len() == 1 {
                build_expr_ast(&subexpressions[0])
            }
            else {
                Err("Expected exactly 1 subtree".into())
            }
        }
        ST::RuleNode { rule_name, subexpressions } if rule_name == "ComparisonExpression" => {
            if subexpressions.len() == 1 {
                build_expr_ast(&subexpressions[0])
            }
            else if subexpressions.len() == 3 {
                let comparison = match &subexpressions[1] {
                    SyntaxTree::TokenNode(T {body: TokenBody::Operator(Operator::DoubleEquals)}) => Comparison::Equals,
                    SyntaxTree::TokenNode(T {body: TokenBody::Operator(Operator::NotEquals)}) => Comparison::NotEquals,
                    SyntaxTree::TokenNode(T {body: TokenBody::Operator(Operator::LessEquals)}) => Comparison::LessEquals,
                    SyntaxTree::TokenNode(T {body: TokenBody::Operator(Operator::GreaterEquals)}) => Comparison::GreaterEquals,
                    SyntaxTree::TokenNode(T {body: TokenBody::Operator(Operator::Less)}) => Comparison::Less,
                    SyntaxTree::TokenNode(T {body: TokenBody::Operator(Operator::Greater)}) => Comparison::Greater,
                    _ => return Err("Expected comparison operator".into()),
                };

                let left = Box::new(build_expr_ast(&subexpressions[0])?);
                let right = Box::new(build_expr_ast(&subexpressions[2])?);

                Ok(ExprAST::Comparison(left, right, comparison, ASTNodeData::new()))
            }
            else {
                Err("Unexpected number of subexpression under ComparisonExpression".into())
            }
        },
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
        ST::RuleNode { ref rule_name, subexpressions } if rule_name == "IfExpression" => {
            if subexpressions.len() != 3 {
                return Err("Expected 3 subexpressions for IfExpression".into());
            }

            if !matches!(subexpressions[0], ST::TokenNode(Token {body: TB::Keyword(Keyword::If)})) {
                return Err("Expected keyword if".into());
            }

            let condition = Box::new(build_expr_ast(&subexpressions[1])?);

            let block = Box::new(build_block_expr(&subexpressions[2])?);

            Ok(ExprAST::If { condition, block, data: ASTNodeData::new() })
        }, 
        ST::RuleNode { rule_name, subexpressions: _ } => 
            Err(format!("Expected Expression. Unknown expression node name: {rule_name}").into()),
        ST::TokenNode (T { body: TB::Identifier(name) }) =>
            Ok(ExprAST::Variable(name.clone(), ASTNodeData::new() )),
        ST::TokenNode (tok) => 
            Err(format!("Expected expression, found token {tok}").into())
    }
}

fn build_literal_expr(tree: &SyntaxTree<Token>) -> Result<ExprAST, ASTError> {
    let SyntaxTree::RuleNode { rule_name, subexpressions } = tree
        else { return Err("Expected Rule node".into()); };

    if rule_name != "Literal" {
        return Err("Expected Literal node".into());
    }

    if subexpressions.len() != 1 {
        return Err("Wrong number of subnodes to node Literal".into())
    }

    let child = &subexpressions[0];

    match child {
        SyntaxTree::RuleNode { rule_name, subexpressions } if rule_name == "BooleanLiteral" => {
            match subexpressions.as_slice() {
                [SyntaxTree::TokenNode(Token { body: TokenBody::Keyword(Keyword::True) })] => 
                    Ok(ExprAST::BooleanLiteral(true, ASTNodeData::new())),
                [SyntaxTree::TokenNode(Token { body: TokenBody::Keyword(Keyword::False) })] => 
                    Ok(ExprAST::BooleanLiteral(false, ASTNodeData::new())),
                _ => Err("Expected true or false under BooleanLiteral node".into()),
            }
        }
        SyntaxTree::RuleNode { .. } =>
            Err("Unexpected rule node under Literal node".into()),
        SyntaxTree::TokenNode(Token { body: TokenBody::NumericLiteral(str) }) => 
            Ok(ExprAST::IntegerLiteral(str.parse().map_err(|_| ASTError("Integer parse failed. Literals must fit in i128".to_string()))?, ASTNodeData::new())),
        SyntaxTree::TokenNode(_) => Err("Non numeric literal under Literal node".into())
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
    let SyntaxTree::RuleNode { rule_name, subexpressions } = tree
        else { return Err("Expected Rule node".into()) };

    if rule_name != "Statement" {
        return Err("Expected Statement node".into());
    }

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
}

fn combine_binary_ops<F>(subtrees: &[SyntaxTree<Token>], left_to_right: bool, combine_fn: F) -> Result<ExprAST, ASTError>
    where F: Fn(ExprAST, &SyntaxTree<Token>, ExprAST) -> Result<ExprAST, ASTError> {
        
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
