
use std::str::FromStr;

use crate::error::TokenError;

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
        use Keyword as K;

        Ok(match s {
            "var" => K::Var,
            "val" => K::Val,
            "fn" => K::Fn,
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
    Equals,
    ThinRightArrow,
}

impl FromStr for Operator {
    type Err = ();  // Likely ignored by algorithm.

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operator as O;

        match s {
            "+" => Ok(O::Plus),
            "-" => Ok(O::Minus),
            "*" => Ok(O::Times),
            "/" => Ok(O::Divide),           
            "=" => Ok(O::Equals),
            _ => Err(())
        }
    }
}

// All punctuation is a single character that cannot be part of another token.
#[derive(Debug)]
pub enum Punctuation {
    Semicolon,
    Comma,
    Colon,
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
        use Punctuation as P;

        match value {
            ';' => Ok(P::Semicolon),
            ',' => Ok(P::Comma),
            ':' => Ok(P::Colon),
            '{' => Ok(P::LeftCurlyBrace),
            '}' => Ok(P::RightCurlyBrace),
            '(' => Ok(P::LeftParenthesis),           
            ')' => Ok(P::RightParenthesis),
            '[' => Ok(P::LeftSquareBracket),
            ']' => Ok(P::RightSquareBracket),
            _ => Err(())
        }
    }
}

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
            return Err(format!("Cannot start token with {}", *ch).into())
        }
    }

    Ok(tokens)
}


fn take_string_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    let first = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    if first != '\"' {
        return Err("Expected character '\"'.".into());
    }

    let mut string = String::new();

    for ch in iter {
        // TODO: Allow [\"] to escape the double quote
        
        if ch == '\"' {
            return Ok(TokenBody::StringLiteral(deliteralize(string)?));
        }

        string.push(ch);
    }

    Err("Expected character '\"'. String literal does not terminate.".into())
}

fn take_char_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    let first = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    if first != '\'' {
        return Err("Expected character '\''.".into());
    }

    let mut string = String::new();

    for ch in iter {
        // TODO: Allow [\'] to escape the single quote
        
        if ch == '\'' {
            return Ok(TokenBody::CharLiteral(literal_to_char(&string)?));
        }

        string.push(ch);
    }

    Err("Expected character '\''. Char literal does not terminate.".into())
}

fn take_numeric_literal(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<TokenBody, TokenError> {
    if !matches!(iter.peek(), Some(ch) if ch.is_ascii_digit()) {
        return Err("Expected Digit.".into())
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
        return Err("Expected Identifier character.".into())
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

// A / looks like an operator, but if it is followed by another slash then we discard the whole
// line.
fn take_operators(iter: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<Vec<Operator>, TokenError> {
    if !matches!(iter.peek(), Some(ch) if is_operator_char(*ch)) {
        return Err("Expected operator character.".into())
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
        if slice.starts_with("//") {
            // Discard comment
            for ch in iter.by_ref() {
                if ch == '\n' {
                    break;
                }
            }

            return Ok(operators)
        }
        // This is an operator for lexical reasons. Punctuation has to be single characters.
        else if slice.starts_with("->") {
            operators.push(Operator::ThinRightArrow);
            slice = &slice[2..];
        }
        else if slice.starts_with('+') {
            operators.push(Operator::Plus);
            slice = &slice[1..];
        }
        else if slice.starts_with('-') {
            operators.push(Operator::Minus);
            slice = &slice[1..];
        }
        else if slice.starts_with('*') {
            operators.push(Operator::Times);
            slice = &slice[1..];
        }
        else if slice.starts_with('/') {
            operators.push(Operator::Divide);
            slice = &slice[1..];
        }
        else if slice.starts_with('=') {
            operators.push(Operator::Equals);
            slice = &slice[1..];
        }
        else {
            return Err(format!("Could not split operators: {slice}").into());
        }
    }

    Ok(operators)
}


fn is_operator_char(ch: char) -> bool {
    let operators = ['+', '-', '*', '/', '=', '>'];

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
        Err("Bad character literal.".into())
    }
}


/* Make Token work with Parsley */

impl parsley::Token for Token {
    fn matches(token_type: &str, token: &Self) -> Result<bool, parsley::ParseError> {
        use Token as T;
        use TokenBody as TB;
        use Punctuation as P;
        use Operator as O;
        use Keyword as K;

        Ok(match token_type {
            "Identifier"     => matches!(token, T { body: TB::Identifier(_), .. }),
            "NumericLiteral" => matches!(token, T { body: TB::NumericLiteral(_), .. }),

            "LeftCurlyBrace"     => matches!(token, T { body: TB::Punctuation(P::LeftCurlyBrace), .. }),
            "RightCurlyBrace"    => matches!(token, T { body: TB::Punctuation(P::RightCurlyBrace), .. }),
            "LeftParenthesis"    => matches!(token, T { body: TB::Punctuation(P::LeftParenthesis), .. }),
            "RightParenthesis"   => matches!(token, T { body: TB::Punctuation(P::RightParenthesis), .. }),
            "LeftSquareBracket"  => matches!(token, T { body: TB::Punctuation(P::LeftSquareBracket), .. }),
            "RightSquareBracket" => matches!(token, T { body: TB::Punctuation(P::RightSquareBracket), .. }),
            "Semicolon"          => matches!(token, T { body: TB::Punctuation(P::Semicolon), .. }),
            "Comma"              => matches!(token, T { body: TB::Punctuation(P::Comma), .. }),
            "Colon"              => matches!(token, T { body: TB::Punctuation(P::Colon), .. }),

            "Plus"           => matches!(token, T { body: TB::Operator(O::Plus) }),
            "Minus"          => matches!(token, T { body: TB::Operator(O::Minus) }),
            "Times"          => matches!(token, T { body: TB::Operator(O::Times) }), 
            "Divide"         => matches!(token, T { body: TB::Operator(O::Divide) }),
            "Equals"         => matches!(token, T { body: TB::Operator(O::Equals) }),
            "ThinRightArrow" => matches!(token, T { body: TB::Operator(O::ThinRightArrow) }),

            "Var" => matches!(token, T { body: TB::Keyword(K::Var) }),
            "Val" => matches!(token, T { body: TB::Keyword(K::Val) }),
            "Fn"  => matches!(token, T { body: TB::Keyword(K::Fn) }),

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
