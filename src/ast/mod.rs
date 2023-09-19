
use parsley::SyntaxTree as ST;

use crate::error::ASTError;
use crate::instructions::Comparison;
use crate::token::{Token, TokenBody as TB, Operator as Op, Punctuation as Punc, Keyword as Kw};


// ---- AST Definitions ---- //

#[derive(Debug)]
pub struct AST {
    pub declarations: Vec<DeclarationAST>,
    pub node_data: ASTNodeData,
}

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

#[derive(Debug)]
pub enum DeclarationAST {
    // The parameters are pairs of names and type ascriptions
    Function { name: String, params: Vec<(String, String)>, block: ExprAST, return_type: String, node_data: ASTNodeData },
    Variable { mutability: Mutability, name: String, expr: ExprAST, type_ascription: Option<String> , node_data: ASTNodeData }
}

#[derive(Debug)]
pub enum StatementAST {
    ExpressionStatement (ExprAST, ASTNodeData),  // A expression executed for its side effects
    Assignment (ExprAST, ExprAST, ASTNodeData),  // There are restrictions on wbat goes on the left, but it is ultimately an expression too.
    Declaration (DeclarationAST, ASTNodeData),  // Any declaration will be allowed, but for now only variable declarations work.
}

#[derive(Debug, Default)]
pub enum ExprAST {
    Add (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Subtract (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Multiply (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Divide (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Comparison (Box<ExprAST>, Box<ExprAST>, Comparison, ASTNodeData),
    Or (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    And (Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Not (Box<ExprAST>, ASTNodeData),

    // i128 can fit all of our literals, up to u64 and i64. Whether a literal fits in a specific type is decided later.
    IntegerLiteral(i128, ASTNodeData), 
    BooleanLiteral(bool, ASTNodeData),
    Variable (String, ASTNodeData),
    FunctionCall (String, Vec<ExprAST>, ASTNodeData),  // The vec contains arguments
    Block (Vec<StatementAST>, Option<Box<ExprAST>>, ASTNodeData),
    If { condition: Box<ExprAST>, block: Box<ExprAST>, else_branch: Option<Box<ExprAST>>, data: ASTNodeData },
    While { condition: Box<ExprAST>, block: Box<ExprAST>, data: ASTNodeData },
    
    // This is a hack that allows us to remove an AST, operate on it, and put it back. (Blame the borrow checker for this.)
    #[default] 
    Moved,  
}

impl ExprAST {
    pub fn get_node_data(&self) -> &ASTNodeData {
        match self {
            ExprAST::Add(_, _, data)
            | ExprAST::Subtract(_, _, data)
            | ExprAST::Multiply(_, _, data)
            | ExprAST::Divide(_, _, data)
            | ExprAST::Comparison(_, _, _, data)
            | ExprAST::Or(_, _, data)
            | ExprAST::And(_, _, data)
            | ExprAST::Not(_, data)
            | ExprAST::IntegerLiteral(_, data)
            | ExprAST::BooleanLiteral(_, data)
            | ExprAST::Variable(_, data)
            | ExprAST::FunctionCall(_, _, data)
            | ExprAST::Block(_, _, data)
            | ExprAST::If { data, .. }
            | ExprAST::While { data, .. } => data,
            ExprAST::Moved => panic!("ExprAST was moved"),
        }
    }
}


/* Related Types */

#[derive(Debug)]
pub enum Mutability {
    Var, 
    Val
}


/* Functions that build AST Nodes */

pub fn build_ast(tree: &ST<Token>) -> Result<AST, ASTError> {
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == "Program" => {
            Ok(AST { 
                declarations: subexpressions.iter()
                    .map(build_declaration_ast)
                    .collect::<Result<Vec<_>, _>>()?,
                node_data: ASTNodeData::new() 
            })
        }
        _ => Err("Failed to build Program AST".into()),
    }
}

fn build_declaration_ast(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "Declaration")?;
        
    match children {
        [ ST::RuleNode { rule_name, ..  } ] if rule_name == "FunctionDeclaration" => {
            build_function_declaration(&children[0])
        },
        [ decl @ ST::RuleNode { rule_name, .. }
        , ST::TokenNode(Token { body: TB::Punctuation(Punc::Semicolon) })
        ] if rule_name == "VariableDeclaration" => {
            build_variable_declaration(decl)
        },
        _ => Err("Failed to build Declaration AST".into())
    }
}

fn build_statement_ast(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = assert_rule_get_children(tree, "Statement")?;

    match children {
        [ ST::RuleNode { rule_name, subexpressions }
        , ST::TokenNode (Token { body: TB::Punctuation(Punc::Semicolon)})
        ] if rule_name == "Expression" => 
            Ok(StatementAST::ExpressionStatement(build_expr_ast(&subexpressions[0])?, ASTNodeData::new())),
        
        [ stmt @ ST::RuleNode { rule_name, .. }
        , ST::TokenNode (Token { body: TB::Punctuation(Punc::Semicolon)})
        ] if rule_name == "AssignmentStatement" => 
            build_assignment_statement(stmt),
        
        [ stmt @ ST::RuleNode { rule_name, .. } ] if rule_name == "Declaration" => 
            Ok(StatementAST::Declaration(build_declaration_ast(stmt)?, ASTNodeData::new())),
        
        _ => Err("Failed to build Statement AST".into())
    }
}

fn build_expr_ast(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == "Expression" => {
            if subexpressions.len() == 1 {
                build_expr_ast(&subexpressions[0])
            }
            else {
                Err("Expected exactly on child of Expression node".into())
            }
        },
        ST::RuleNode { rule_name, subexpressions } if rule_name == "PrimaryExpression" => {            
            if subexpressions.len() == 1 {  // Literal, Variable, or Block
                build_expr_ast(subexpressions.iter().next().expect("Known to exist"))
            }
            else if subexpressions.len() == 3 {
                if !matches!(&subexpressions[0], ST::TokenNode(Token { body: TB::Punctuation(Punc::LeftParenthesis)}))
                    || !matches!(&subexpressions[2], ST::TokenNode(Token { body: TB::Punctuation(Punc::RightParenthesis)})) {
                    
                    Err("Expected parentheses".into())
                }
                else {
                    build_expr_ast(&subexpressions[1])
                }
            }
            else {
                Err("Wrong number of subtrees at PrimaryExpression".into())
            }
        },
        ST::RuleNode { rule_name, .. } if rule_name == "AdditiveExpression" => 
            build_additive_expr(tree),
        ST::RuleNode { rule_name, .. } if rule_name == "MultiplicativeExpression" =>
            build_multiplicative_expr(tree),
        ST::RuleNode { rule_name, .. } if rule_name == "ComparisonExpression" => 
            build_comparision_expr(tree),
        ST::RuleNode { rule_name, .. } if rule_name == "OrExpression" =>
            build_or_expr(tree),
        ST::RuleNode { rule_name, .. } if rule_name == "AndExpression" =>
            build_and_expr(tree),
        ST::RuleNode { rule_name, .. } if rule_name == "NotExpression" =>
            build_not_expr(tree),
        ST::RuleNode { ref rule_name, .. } if rule_name == "Literal" =>
            build_literal_expr(tree),
        ST::RuleNode { ref rule_name, .. } if rule_name == "FunctionCall" => 
            build_function_call_expr(tree),
        ST::RuleNode { ref rule_name, .. } if rule_name == "BlockExpression" =>
            build_block_expr(tree),
        ST::RuleNode { ref rule_name, .. } if rule_name == "IfExpression" => 
            build_if_expr(tree),
        ST::RuleNode { ref rule_name, .. } if rule_name == "WhileExpression" =>
            build_while_expr(tree),
        ST::RuleNode { rule_name, subexpressions: _ } => 
            Err(format!("Expected Expression. Unknown expression node name: {rule_name}").into()),
        ST::TokenNode (Token { body: TB::Identifier(name) }) =>
            Ok(ExprAST::Variable(name.clone(), ASTNodeData::new() )),
        ST::TokenNode (tok) => 
            Err(format!("Expected expression, found token {tok}").into())
    }
}


/* Functions that build specific kinds of AST Node */

fn build_function_declaration(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "FunctionDeclaration")?;

    if children.len() != 6 {
        return Err("Incorrect number of subnodes to function node".into());
    }
    
    if !matches!(children[0], ST::TokenNode(Token { body: TB::Keyword(Kw::Fn) })) {
        return Err("Expected `fn` in function declaration".into());
    }

    let name = if let ST::TokenNode(Token { body: TB::Identifier(name) }) = &children[1] {
        name.clone()
    } 
    else { 
        return Err("Expected function name".into());
    };

    if !matches!(children[3], ST::TokenNode(Token {body: TB::Operator(Op::ThinRightArrow)})) {
        return Err("Expected `->` in function declaration".into());
    }

    let return_type = build_type(&children[4])?;

    let params = build_parameter_list(&children[2])?;
    let block = build_expr_ast(&children[5])?;

    Ok(DeclarationAST::Function { name, params, block, node_data: ASTNodeData::new(), return_type })
}

fn build_variable_declaration(tree: &ST<Token>) -> Result<DeclarationAST, ASTError> {
    let children = assert_rule_get_children(tree, "VariableDeclaration")?;

    match children {
        [ ST::TokenNode(Token { body: TB::Keyword(keyword @ (Kw::Val | Kw::Var)) })
        , ST::TokenNode(Token { body: TB::Identifier(name) })
        , ..
        , ST::TokenNode(Token { body: TB::Operator(Op::Equals) })
        , expr_node @ ST::RuleNode { rule_name: last_rule_name, ..}
        ] if last_rule_name == "Expression" => {
            let mutability = match keyword {
                Kw::Var => Mutability::Var,
                Kw::Val => Mutability::Val,
                _ => panic!("Known unreachable")
            };

            let type_ascription = if children.len() == 6 {
                match &children[2..4] {
                    [ ST::TokenNode(Token { body: TB::Punctuation(Punc::Colon)})
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
}

fn build_assignment_statement(tree: &ST<Token>) -> Result<StatementAST, ASTError> {
    let children = assert_rule_get_children(tree, "AssignmentStatement")?;

    match children {
        [ ST::RuleNode { rule_name: rule_1, subexpressions: ref sub_expr_1 }
        , ST::TokenNode (Token { body: TB::Operator(Op::Equals) })
        , ST::RuleNode { rule_name: rule_2, subexpressions: ref sub_expr_2 }
        ] if rule_1 == "Expression" 
        && rule_2 == "Expression" 
        && sub_expr_1.len() == 1 
        && sub_expr_2.len() == 1 => {
            let left = build_expr_ast(&sub_expr_1[0])?;
            let right = build_expr_ast(&sub_expr_2[0])?;

            Ok(StatementAST::Assignment(left, right, ASTNodeData::new()))
        },
        _ => Err("Failed to build AssignmentStatement".into())
    }
}

fn build_additive_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "AdditiveExpression")?;
    
    combine_binary_ops(children, true, |left, op, right| {
        match op {
            ST::TokenNode(Token { body: TB::Operator(Op::Plus) }) => 
                Ok(ExprAST::Add(Box::new(left), Box::new(right), ASTNodeData::new())),
            ST::TokenNode(Token { body: TB::Operator(Op::Minus) }) => 
                Ok(ExprAST::Subtract(Box::new(left), Box::new(right), ASTNodeData::new())),
            _ => Err("Expected + or -".into())
        }
    })
}

fn build_multiplicative_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "MultiplicativeExpression")?;
    
    combine_binary_ops(children, true, |left, op, right| {
        match op {
            ST::TokenNode(Token { body: TB::Operator(Op::Times) }) 
                => Ok(ExprAST::Multiply(Box::new(left), Box::new(right), ASTNodeData::new())),
            ST::TokenNode(Token { body: TB::Operator(Op::Divide) }) 
                => Ok(ExprAST::Divide(Box::new(left), Box::new(right), ASTNodeData::new())),
            _ => Err("Expected * or /".into())
        }
    })
}

fn build_comparision_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "ComparisonExpression")?;
    
    if children.len() == 1 {
        build_expr_ast(&children[0])
    }
    else if children.len() == 3 {
        let comparison = match &children[1] {
            ST::TokenNode(Token {body: TB::Operator(Op::DoubleEquals)}) => Comparison::Equals,
            ST::TokenNode(Token {body: TB::Operator(Op::NotEquals)}) => Comparison::NotEquals,
            ST::TokenNode(Token {body: TB::Operator(Op::LessEquals)}) => Comparison::LessEquals,
            ST::TokenNode(Token {body: TB::Operator(Op::GreaterEquals)}) => Comparison::GreaterEquals,
            ST::TokenNode(Token {body: TB::Operator(Op::Less)}) => Comparison::Less,
            ST::TokenNode(Token {body: TB::Operator(Op::Greater)}) => Comparison::Greater,
            _ => return Err("Expected comparison operator".into()),
        };

        let left = Box::new(build_expr_ast(&children[0])?);
        let right = Box::new(build_expr_ast(&children[2])?);

        Ok(ExprAST::Comparison(left, right, comparison, ASTNodeData::new()))
    }
    else {
        Err("Unexpected number of subexpression under ComparisonExpression".into())
    }
}

fn build_or_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "OrExpression")?;
    
    combine_binary_ops(children, true, |left, op, right| {
        match op {
            ST::TokenNode(Token { body: TB::Keyword(Kw::Or) }) => 
                Ok(ExprAST::Or(Box::new(left), Box::new(right), ASTNodeData::new())),
            _ => Err("Expected 'or'".into())
        }
    })
}

fn build_and_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "AndExpression")?;
    
    combine_binary_ops(children, true, |left, op, right| {
        match op {
            ST::TokenNode(Token { body: TB::Keyword(Kw::And) }) => 
                Ok(ExprAST::And(Box::new(left), Box::new(right), ASTNodeData::new())),
            _ => Err("Expected 'and'".into())
        }
    })
}

fn build_not_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "NotExpression")?;
    
    if children.len() == 2 {
        let ST::TokenNode(Token {body: TB::Keyword(Kw::Not)}) = children[0]
            else { return Err("...".into()) };

        let inner = build_expr_ast(&children[1])?;
        Ok(ExprAST::Not(Box::new(inner), ASTNodeData::new()))
    }
    else {
        build_expr_ast(&children[0])
    }
}

fn build_literal_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "Literal")?;

    if children.len() != 1 {
        return Err("Wrong number of subnodes to node Literal".into())
    }

    let child = &children[0];

    match child {
        ST::RuleNode { rule_name, subexpressions } if rule_name == "BooleanLiteral" => {
            match subexpressions.as_slice() {
                [ST::TokenNode(Token { body: TB::Keyword(Kw::True) })] => 
                    Ok(ExprAST::BooleanLiteral(true, ASTNodeData::new())),
                [ST::TokenNode(Token { body: TB::Keyword(Kw::False) })] => 
                    Ok(ExprAST::BooleanLiteral(false, ASTNodeData::new())),
                _ => Err("Expected true or false under BooleanLiteral node".into()),
            }
        },
        ST::RuleNode { .. } =>
            Err("Unexpected rule node under Literal node".into()),
        ST::TokenNode(Token { body: TB::NumericLiteral(str) }) => {
            let num = str.parse()
                .map_err(|_| ASTError("Integer parse failed. Literals must fit in i128".to_string()))?;

            Ok(ExprAST::IntegerLiteral(num, ASTNodeData::new()))
        },
        ST::TokenNode(_) => Err("Non numeric literal under Literal node".into())
    }
}

fn build_function_call_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "FunctionCall")?; 

    let ST::TokenNode (Token {body: TB::Identifier(name)}) = &children[0]
        else { return Err("Could not find identifier in FunctionCall".into()) };

    if !matches!(&children[1], ST::TokenNode (Token {body: TB::Punctuation(Punc::LeftParenthesis)})) {
        return Err("Expected left parenthesis".into());
    }
    if !matches!(&children[children.len() - 1], ST::TokenNode (Token {body: TB::Punctuation(Punc::RightParenthesis)})) {
        return Err("Expected right parenthesis".into());
    }

    let arg_list = &children[2..children.len() - 1];
    let mut iter = arg_list.iter();

    let mut expressions = vec![];
    while let Some(node) = iter.next() {
        expressions.push(build_expr_ast(node)?);

        match iter.next() {
            Some(ST::TokenNode (Token {body: TB::Punctuation(Punc::Comma)})) => (),
            Some(_) => return Err("Expected comma".into()),
            None => break,  // In case the iterator restarts?
        }
    }

    Ok(ExprAST::FunctionCall(name.clone(), expressions, ASTNodeData::new()))
}

fn build_block_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "BlockExpression")?;

    if !matches!(children[0], ST::TokenNode(Token {body: TB::Punctuation(Punc::LeftCurlyBrace)})) {
        return Err("Expected open bracket before block".into());
    }
    if !matches!(children[children.len() - 1], ST::TokenNode(Token {body: TB::Punctuation(Punc::RightCurlyBrace)})) {
        return Err("Expected closed bracket before block".into());
    }

    // &Vec<...> -> Vec<&...>
    let mut subexpressions: Vec<&ST<Token>> = children.iter().collect();
    subexpressions.remove(subexpressions.len()-1); // Discard last_brace

    if subexpressions.is_empty() {
        return Ok(ExprAST::Block(vec![], None, ASTNodeData::new()));
    }

    let opt_expr = if let ST::RuleNode { rule_name, subexpressions: _ } = &subexpressions[subexpressions.len() - 1] {
        if rule_name == "Expression" {
            let last = subexpressions.remove(subexpressions.len() - 1);
            
            Some(Box::new(build_expr_ast(last)?))
        }
        else { None }
    } else { None };

    let statements = subexpressions.into_iter()
        .skip(1)  // Discard first brace
        .map(build_statement_ast)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(ExprAST::Block(statements, opt_expr, ASTNodeData::new()))
}

fn build_if_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "IfExpression")?;

    if children.len() < 3 {
        return Err("Expected 3 subexpressions for IfExpression".into());
    }

    if !matches!(children[0], ST::TokenNode(Token {body: TB::Keyword(Kw::If)})) {
        return Err("Expected keyword if".into());
    }

    let condition = Box::new(build_expr_ast(&children[1])?);

    let block = Box::new(build_block_expr(&children[2])?);

    let else_branch = if children.len() == 5 {
        if !matches!(children[3], ST::TokenNode(Token {body: TB::Keyword(Kw::Else)})) {
            return Err("Expected keyword else".into());
        }

        Some(Box::new(build_expr_ast(&children[4])?))
    }
    else {
        None
    };

    Ok(ExprAST::If { condition, block, else_branch, data: ASTNodeData::new() })
}

fn build_while_expr(tree: &ST<Token>) -> Result<ExprAST, ASTError> {
    let children = assert_rule_get_children(tree, "WhileExpression")?;

    if children.len() != 3 {
        return Err("Expected 3 subexpressions for WhileExpression".into());
    }

    if !matches!(children[0], ST::TokenNode(Token {body: TB::Keyword(Kw::While)})) {
        return Err("Expected keyword while".into());
    }

    let condition = Box::new(build_expr_ast(&children[1])?);

    let block = Box::new(build_block_expr(&children[2])?);

    Ok(ExprAST::While { condition, block, data: ASTNodeData::new() })
}


/* Functions that build components of AST Nodes */

fn build_parameter_list(node: &ST<Token>) -> Result<Vec<(String, String)>, ASTError> {
    let ST::RuleNode { rule_name, subexpressions } = node
        else { return Err("Expected Rule node".into()); };

    if rule_name != "ParameterList" {
        return Err("Expected parameter list in function declaration".into());
    }


    if !matches!(&subexpressions[0], ST::TokenNode (Token {body: TB::Punctuation(Punc::LeftParenthesis)})) {
        return Err("Expected left parenthesis".into());
    }
    if !matches!(&subexpressions[subexpressions.len() - 1], ST::TokenNode (Token {body: TB::Punctuation(Punc::RightParenthesis)})) {
        return Err("Expected right parenthesis".into());
    }

    let list = &subexpressions[1..subexpressions.len() - 1];
    let mut iter = list.iter().peekable();
    
    let mut parameters = vec![];
    while let Some(node) = iter.next() {
        let ST::TokenNode(Token {body: TB::Identifier(name)}) = node
            else { return Err("Expected name".into()) };
                
        let Some(ST::TokenNode (Token {body: TB::Punctuation(Punc::Colon)})) = iter.next()
            else { return Err("Expected colon".into()) };

        let param_type = build_type(iter.next().ok_or(ASTError::from("Expected type node"))?)?;
        
        parameters.push((name.clone(), param_type));

        match iter.next() {
            Some(ST::TokenNode (Token {body: TB::Punctuation(Punc::Comma)})) => (),
            Some(_) => return Err("Expected comma".into()),
            None => break,  // In case the iterator restarts?
        }
    }

    Ok(parameters)
}

fn build_type(tree: &ST<Token>) -> Result<String, ASTError> {
    if let ST::RuleNode { rule_name, subexpressions } = tree {
        if rule_name == "Type" {
            if let ST::TokenNode(Token {body: TB::Identifier(ident)}) = &subexpressions[0] {
                return Ok(ident.clone());
            }
        }
    }

    Err("Could not build Type node".into())
}


/* Helpers for AST build functions */

fn assert_rule_get_children<'a>(tree: &'a ST<Token>, expected_name: &str) -> Result<&'a [ST<Token>], ASTError> {
    match tree {
        ST::RuleNode { rule_name, subexpressions } if rule_name == expected_name => 
            Ok(subexpressions),
        ST::RuleNode { rule_name, .. } => 
            Err(format!("Expected {expected_name} node, found {rule_name} node").into()),
        ST::TokenNode(_) => Err(format!("Expected {expected_name} node, found token node").into()),
    }
}

fn combine_binary_ops<F>(subtrees: &[ST<Token>], left_to_right: bool, combine_fn: F) -> Result<ExprAST, ASTError>
    where F: Fn(ExprAST, &ST<Token>, ExprAST) -> Result<ExprAST, ASTError> {
        
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
