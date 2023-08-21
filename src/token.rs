
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
}

#[derive(Debug)]
pub enum Operator {  // Operators currently accepted greedily
    Plus,
    Minus,
    Times, 
    Divide,
    Equal
}

impl FromStr for Operator {
    type Err = ();  // Likely ignored by algorithm.

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Operator::Plus),
            "-" => Ok(Operator::Minus),
            "*" => Ok(Operator::Times),
            "/" => Ok(Operator::Divide),           
            "=" => Ok(Operator::Equal),
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
            todo!()
            // Pull operator characters. 
            // Check is its actually a comment
            // If it is a comment, throw away characters until end of line.
            // Otherwise, break up operators somehow.
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
            /* Ignore */
        }
        else {
            return Err(TokenError(format!("Cannot start token with {}", *ch)))
        }
    }

    Ok(tokens)
}


fn take_string_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    todo!()
}

fn take_char_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    todo!()
}

fn take_numeric_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    todo!()
}

fn take_identifier_or_keyword(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    todo!()
}


fn is_operator_char(ch: char) -> bool {
    todo!()
}

fn is_identifier_char(ch: char) -> bool {
    // Note that digits won't work at start due to algorithm design.
    ch.is_ascii_alphabetic() || ch.is_ascii_digit() || ch == '_'
}
