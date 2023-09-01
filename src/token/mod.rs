
use std::str::FromStr;


/* Token Definitions */

#[derive(Debug)]
pub struct Token {
    pub body: TokenBody,
    // TODO: Add Span information for better error messages
    // If comments ever could apply to an element as metadata, maybe that could go here?
}

// Comments (single line only with "//") are stripped out entirely, and act as whitespace.
// Whitespace impacts the split between some other tokens.
#[derive(Debug)]
pub enum TokenBody {
    Identifier (String),
    Keyword (Keyword),
    StringLiteral (String),  // Content, with escapes processed, and no double quotes.
    CharLiteral (char),  // Content, with escapes processed, and no single quotes.
    NumericLiteral (String),  // TODO: Replace with enum for all numeric literal values.
    Operator (Operator),
    Punctuation (Punctuation),
}

#[derive(Debug)]
pub enum Keyword {
    Var,
    Val,
    Fn,
}

impl FromStr for Keyword {
    type Err = TokenError;  // Likely ignored by algorithm.

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Keyword::*;

        Ok(match s {
            "var" => Var,
            "val" => Val,
            "fn" => Fn,
            _ => Err(TokenError("Not a keyword".to_string()))?
        })
    }
}

#[derive(Debug)]
pub enum Operator {  // Operators currently accepted greedily
    Plus,
    Minus,
    Times, 
    Divide,
    Equals
}

impl FromStr for Operator {
    type Err = ();  // Likely ignored by algorithm.

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Operator::Plus),
            "-" => Ok(Operator::Minus),
            "*" => Ok(Operator::Times),
            "/" => Ok(Operator::Divide),           
            "=" => Ok(Operator::Equals),
            _ => Err(())
        }
    }
}

// All punctuation is a single character that cannot be part of another token.
#[derive(Debug)]
pub enum Punctuation {
    Semicolon,
    Comma,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
}

impl TryFrom<char> for Punctuation {
    type Error = ();  // Likely ignored by algorithm.;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            ';' => Ok(Punctuation::Semicolon),
            ',' => Ok(Punctuation::Comma),
            '{' => Ok(Punctuation::LeftCurlyBrace),
            '}' => Ok(Punctuation::RightCurlyBrace),
            '(' => Ok(Punctuation::LeftParenthesis),           
            ')' => Ok(Punctuation::RightParenthesis),
            '[' => Ok(Punctuation::LeftSquareBracket),
            ']' => Ok(Punctuation::RightSquareBracket),
            _ => Err(())
        }
    }
}

#[derive(Debug)]
pub struct TokenError (String);


/* Tokenization Algorithm */

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenError> {
    let mut tokens: Vec<Token> = vec![];

    let mut iter = input.chars().peekable();
    while let Some(ch) = iter.peek() {
        if *ch == '\"' {
            let token = take_string_literal(&mut iter)?;
            tokens.push(Token { body: token });
        }
        else if *ch == '\'' {
            let token = take_char_literal(&mut iter)?;
            tokens.push(Token { body: token });
        }
        else if is_operator_char(*ch) {
            let operators = take_operators(&mut iter)?;
            for op in operators {
                tokens.push(Token { body: TokenBody::Operator(op) });
            }
        }
        else if ch.is_ascii_digit() {
            let token = take_numeric_literal(&mut iter)?;
            tokens.push(Token { body: token });
        }
        else if let Ok(punct) = Punctuation::try_from(*ch) {
            _ = iter.next();
            tokens.push(Token { body: TokenBody::Punctuation (punct) });
        }
        else if is_identifier_char(*ch) {
            let token = take_identifier_or_keyword(&mut iter)?;
            tokens.push(Token { body: token });
        }
        else if ch.is_whitespace() {
            iter.next().expect("Known");
        }
        else {
            return Err(TokenError(format!("Cannot start token with {}", *ch)))
        }
    }

    Ok(tokens)
}


fn take_string_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    let first = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    if first != '\"' {
        return Err(TokenError("Expected character '\"'.".to_string()));
    }

    let mut string = String::new();

    for ch in iter {
        // TODO: Allow [\"] to escape the double quote
        
        if ch == '\"' {
            return Ok(TokenBody::StringLiteral(deliteralize(string)?));
        }

        string.push(ch);
    }

    Err(TokenError("Expected character '\"'. String literal does not terminate.".to_string()))
}

fn take_char_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    let first = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    if first != '\'' {
        return Err(TokenError("Expected character '\''.".to_string()));
    }

    let mut string = String::new();

    for ch in iter {
        // TODO: Allow [\'] to escape the single quote
        
        if ch == '\'' {
            return Ok(TokenBody::CharLiteral(literal_to_char(&string)?));
        }

        string.push(ch);
    }

    Err(TokenError("Expected character '\''. Char literal does not terminate.".to_string()))
}

fn take_numeric_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    if !matches!(iter.peek(), Some(ch) if ch.is_ascii_digit()) {
        return Err(TokenError("Expected Digit.".to_string()))
    }

    let mut string = String::new();
    while let Some(ch) = iter.peek() {
        if is_numeric_literal_char(*ch) {
            string.push(iter.next().expect("Known to exist"));
        }
        else {
            break
        }
    }

    Ok(TokenBody::NumericLiteral(string))
}

fn take_identifier_or_keyword(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    if !matches!(iter.peek(), Some(ch) if is_identifier_char(*ch)) {
        return Err(TokenError("Expected Identifier character.".to_string()))
    }

    let mut string = String::new();
    while let Some(ch) = iter.peek() {
        if is_identifier_char(*ch) {
            string.push(iter.next().expect("Known to exist"));
        }
        else {
            break
        }
    }

    match Keyword::from_str(&string) {
        Ok(keyword) => Ok(TokenBody::Keyword(keyword)),
        Err(_) => Ok(TokenBody::Identifier(string))
    }
}

fn take_operators(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<Vec<Operator>, TokenError> {
    if !matches!(iter.peek(), Some(ch) if is_operator_char(*ch)) {
        return Err(TokenError("Expected operator character.".to_string()))
    }

    let mut string = String::new();
    while let Some(ch) = iter.peek() {
        if is_operator_char(*ch) {
            string.push(iter.next().expect("Known to exist"));
        }
        else {
            break
        }
    }

    // Now we continually remove the largest operator that works. Yes, this is greedy,
    // could improve later or just use sane operators with little overlap.

    let mut operators = vec![];

    let mut slice = &string[..];
    while !slice.is_empty() {
        if &slice[..1] == "+" {
            operators.push(Operator::Plus);
            slice = &slice[1..];
        }
        else if &slice[..1] == "-" {
            operators.push(Operator::Minus);
            slice = &slice[1..];
        }
        else if &slice[..1] == "*" {
            operators.push(Operator::Times);
            slice = &slice[1..];
        }
        else if &slice[..1] == "/" {
            operators.push(Operator::Divide);
            slice = &slice[1..];
        }
        else if &slice[..1] == "=" {
            operators.push(Operator::Equals);
            slice = &slice[1..];
        }
        else {
            return Err(TokenError(format!("Could not split operators: {slice}")));
        }
    }

    Ok(operators)
}


fn is_operator_char(ch: char) -> bool {
    let operators = ['+', '-', '*', '/', '='];

    operators.contains(&ch)
}

fn is_numeric_literal_char(ch: char) -> bool {
    // TODO: Support for floats and numbers of different bases.

    // See ch.is_digit for arbitrary base
    ch.is_ascii_digit()
}

fn is_identifier_char(ch: char) -> bool {
    // Note that digits won't work at start due to algorithm design.
    ch.is_ascii_alphabetic() || ch.is_ascii_digit() || ch == '_'
}

#[allow(clippy::unnecessary_wraps)]
fn deliteralize(string: String) -> Result<String, TokenError> {
    // TODO: Actually turn literal into represented string
    Ok(string)
}

fn literal_to_char(string: &str) -> Result<char, TokenError> {
    // TODO: Add supports for escaped characters. Preferably shared with deliteralize?
    // Be aware of [\']
    if string.len() == 1 {
        Ok(string.chars().next().expect("Known to exist"))
    }
    else {
        Err(TokenError("Bad character literal.".to_string()))
    }
}


/* Make Token work with Parsley */

impl parsley::Token for Token {
    fn matches(token_type: &str, token: &Self) -> Result<bool, parsley::ParseError> {
        use Punctuation::*;
        use Operator::*;

        Ok(match token_type {
            "Identifier" => matches!(token, Token { body: TokenBody::Identifier(_), .. }),
            "NumericLiteral" => matches!(token, Token { body: TokenBody::NumericLiteral(_), .. }),
            "LeftCurlyBrace" => matches!(token, Token { body: TokenBody::Punctuation(LeftCurlyBrace), .. }),
            "RightCurlyBrace" => matches!(token, Token { body: TokenBody::Punctuation(RightCurlyBrace), .. }),
            "LeftParenthesis" => matches!(token, Token { body: TokenBody::Punctuation(LeftParenthesis), .. }),
            "RightParenthesis" => matches!(token, Token { body: TokenBody::Punctuation(RightParenthesis), .. }),
            "LeftSquareBracket" => matches!(token, Token { body: TokenBody::Punctuation(LeftSquareBracket), .. }),
            "RightSquareBracket" => matches!(token, Token { body: TokenBody::Punctuation(RightSquareBracket), .. }),
            "Semicolon" => matches!(token, Token { body: TokenBody::Punctuation(Semicolon), .. }),
            "Comma" => matches!(token, Token { body: TokenBody::Punctuation(Comma), .. }),
            "Plus" => matches!(token, Token { body: TokenBody::Operator(Plus) }),
            "Minus" => matches!(token, Token { body: TokenBody::Operator(Minus) }),
            "Times" => matches!(token, Token { body: TokenBody::Operator(Times) }), 
            "Divide" => matches!(token, Token { body: TokenBody::Operator(Divide) }),
            "Equals" => matches!(token, Token { body: TokenBody::Operator(Equals) }),
            "Var" => matches!(token, Token { body: TokenBody::Keyword(Keyword::Var) }),
            "Val" => matches!(token, Token { body: TokenBody::Keyword(Keyword::Val) }),
            "Fn" => matches!(token, Token { body: TokenBody::Keyword(Keyword::Fn) }),
            _ => Err(parsley::ParseError(format!("Bad token type: \"{token_type}\"")))?
        })
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /* TODO: Actually fill this out */

        f.write_str("{token}")
    }
}
