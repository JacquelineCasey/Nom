
// use parsley::ParseError;

#[cfg(test)]
mod tests;

use std::cmp::{min, max};
use std::rc::Rc;
use std::str::FromStr;

use crate::error::TokenError;


/* Spans describe contiguous groups of characters in a specific source file (or
 * pseudo source file). Each span represents a half open interval. */

#[derive(Debug, Clone)]
pub struct Span {
    file: Rc<String>,
    start_line: usize,  // 1 based
    end_line: usize,
    start_col: usize,  // 1 based
    end_col: usize
}

impl Span {
    pub fn combine(a: &Span, b: &Span) -> Span {
        assert!(*a.file == *b.file);

        Span {
            file: Rc::clone(&a.file),
            start_line: min(a.start_line, b.start_line),
            end_line: max(a.end_line, b.end_line),
            start_col: min(a.start_col, a.start_col),
            end_col: max(a.end_col, b.end_col)
        }
    }
}

/* Token Definitions */

#[derive(Debug, Clone)]
pub struct Token {
    pub body: TokenBody,
    pub span: Span,

    // TODO: If comments ever could apply to an element as metadata, maybe that could go here?
}

// Comments (single line only with "//") are stripped out entirely, and act as whitespace.
// Whitespace impacts the split between some other tokens.
#[derive(Debug, Clone)]
pub enum TokenBody {
    Identifier (String),
    Keyword (Keyword),
    StringLiteral (String),  // Content, with escapes processed, and no double quotes.
    CharLiteral (char),  // Content, with escapes processed, and no single quotes.
    NumericLiteral (String),  // TODO: Replace with enum for all numeric literal values.
    Operator (Operator),
    Punctuation (Punctuation),
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Var,
    Val,
    Fn,
    True,
    False,
    If, 
    Else,
    Not,
    And,
    Or,
    While,
    Return,
}

impl FromStr for Keyword {
    type Err = TokenError;  // Likely ignored by algorithm.

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Keyword as K;

        Ok(match s {
            "var" => K::Var,
            "val" => K::Val,
            "fn" => K::Fn,
            "true" => K::True,
            "false" => K::False,
            "if" => K::If,
            "else" => K::Else,
            "not" => K::Not,
            "and" => K::And,
            "or" => K::Or,
            "while" => K::While,
            "return" => K::Return,
            _ => Err(TokenError("Not a keyword".to_string()))?
        })
    }
}

#[derive(Debug, Clone)]
pub enum Operator {  // Operators currently accepted greedily
    Plus,
    Minus,
    Times, 
    Divide,
    Modulus, 
    Equals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivideEquals,
    ModulusEquals,
    ThinRightArrow,
    DoubleEquals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
}

// All punctuation is a single character that cannot be part of another token.
#[derive(Debug, Clone)]
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

fn add_span_info<'a>(input: &'a str, file: Rc<String>) -> impl std::iter::Iterator<Item = (char, Span)> + 'a {
    let mut line_num = 1;
    let mut col_num = 1;
    
    input.chars().map(move |ch| {
        let ret_val = (ch, Span { 
            file: Rc::clone(&file), 
            start_line: line_num,
            end_line: line_num,
            start_col: col_num,
            end_col: col_num + 1,
        });

        col_num += 1;

        if ch == '\n' {
            line_num += 1;
            col_num = 1;
        }

        ret_val
    })
}  

// TODO! Delete
fn tmp_span() -> Span {
    Span {
        file: Rc::new("<Replace Me!>".to_owned()),
        start_line: 0,
        end_line: 0,
        start_col: 0,
        end_col: 0,
    }
}

/* Tokenization Algorithm */

/* Accepts the input as a string, and a file path for bookeeping (spans). The file
 * path may also be a pseudo file path, if there is no actual backing file (e.g. 
 * REPL mode). */
pub fn tokenize(input: &str, file_path: &str) -> Result<Vec<Token>, TokenError> {
    let mut tokens: Vec<Token> = vec![];

    let fake_file = Rc::new(file_path.to_owned());

    let mut iter = add_span_info(input, fake_file).peekable();
    while let Some((ch, _)) = iter.peek() {
        if *ch == '\"' {
            let (token, span) = take_string_literal(&mut iter)?;
            tokens.push(Token { body: token, span});
        }
        else if *ch == '\'' {
            let (token, span) = take_char_literal(&mut iter)?;
            tokens.push(Token { body: token, span });
        }
        else if is_operator_char(*ch) {
            let operators = take_operators(&mut iter)?;
            for (op, span) in operators {
                tokens.push(Token { body: TokenBody::Operator(op), span });
            }
        }
        else if ch.is_ascii_digit() {
            let (token, span) = take_numeric_literal(&mut iter)?;
            tokens.push(Token { body: token, span });
        }
        else if let Ok(punct) = Punctuation::try_from(*ch) {
            _ = iter.next();
            tokens.push(Token { body: TokenBody::Punctuation (punct), span: tmp_span() });
        }
        else if is_identifier_char(*ch) {
            let (token, span) = take_identifier_or_keyword(&mut iter)?;
            tokens.push(Token { body: token, span });
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


fn take_string_literal(iter: &mut impl std::iter::Iterator<Item = (char, Span)>) 
        -> Result<(TokenBody, Span), TokenError> {
    
    let (first, _) = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    if first != '\"' {
        return Err("Expected character '\"'.".into());
    }

    let mut string = String::new();

    for (ch, _) in iter {
        // TODO: Allow [\"] to escape the double quote
        
        if ch == '\"' {
            return Ok(
                (TokenBody::StringLiteral(deliteralize(string)?),
                tmp_span(),
            ));
        }

        string.push(ch);
    }

    Err("Expected character '\"'. String literal does not terminate.".into())
}

fn take_char_literal(iter: &mut impl std::iter::Iterator<Item = (char, Span)>) 
        -> Result<(TokenBody, Span), TokenError> {
    
    let (first, _) = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    if first != '\'' {
        return Err("Expected character '\''.".into());
    }

    let mut string = String::new();

    for (ch, _) in iter {
        // TODO: Allow [\'] to escape the single quote
        
        if ch == '\'' {
            return Ok((
                TokenBody::CharLiteral(literal_to_char(&string)?),
                tmp_span()
            ));
        }

        string.push(ch);
    }

    Err("Expected character '\''. Char literal does not terminate.".into())
}

fn take_numeric_literal(iter: &mut std::iter::Peekable<impl std::iter::Iterator<Item = (char, Span)>>) 
        -> Result<(TokenBody, Span), TokenError> {
    
    if !matches!(iter.peek(), Some((ch, _)) if ch.is_ascii_digit()) {
        return Err("Expected Digit.".into())
    }

    let mut string = String::new();
    while let Some((ch, _)) = iter.peek() {
        if is_numeric_literal_char(*ch) {
            string.push(iter.next().expect("Known to exist").0);
        }
        else {
            break
        }   
    }

    Ok((TokenBody::NumericLiteral(string), tmp_span()))
}

fn take_identifier_or_keyword(iter: &mut std::iter::Peekable<impl std::iter::Iterator<Item = (char, Span)>>) 
        -> Result<(TokenBody, Span), TokenError> {
        
    if !matches!(iter.peek(), Some((ch, _)) if is_identifier_char(*ch)) {
        return Err("Expected Identifier character.".into())
    }

    let mut string = String::new();
    while let Some((ch, _)) = iter.peek() {
        if is_identifier_char(*ch) {
            string.push(iter.next().expect("Known to exist").0);
        }
        else {
            break
        }
    }

    match Keyword::from_str(&string) {
        Ok(keyword) => Ok((TokenBody::Keyword(keyword), tmp_span())),
        Err(_) => Ok((TokenBody::Identifier(string), tmp_span()))
    }
}

// A / looks like an operator, but if it is followed by another slash then we discard the whole
// line.
fn take_operators(iter: &mut std::iter::Peekable<impl std::iter::Iterator<Item = (char, Span)>>) 
        -> Result<Vec<(Operator, Span)>, TokenError> {
    
    if !matches!(iter.peek(), Some((ch, _)) if is_operator_char(*ch)) {
        return Err("Expected operator character.".into())
    }

    let mut string = String::new();
    while let Some((ch, _)) = iter.peek() {
        if is_operator_char(*ch) {
            string.push(iter.next().expect("Known to exist").0);
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
        let (op, advance) = if slice.starts_with("//") {
            // Discard comment
            for (ch, _) in iter.by_ref() {
                if ch == '\n' {
                    break;
                }
            }

            return Ok(operators)
        }
        // This is an operator for lexical reasons. Punctuation has to be single characters.
        else if slice.starts_with("->") {
            (Operator::ThinRightArrow, 2)
        }
        else if slice.starts_with("==") {
            (Operator::DoubleEquals, 2)
        }
        else if slice.starts_with("!=") {
            (Operator::NotEquals, 2)
        }
        else if slice.starts_with("<=") {
            (Operator::LessEquals, 2)
        }
        else if slice.starts_with(">=") {
            (Operator::GreaterEquals, 2)
        }
        else if slice.starts_with("+=") {
            (Operator::PlusEquals, 2)
        }
        else if slice.starts_with("-=") {
            (Operator::MinusEquals, 2)
        }
        else if slice.starts_with("*=") {
            (Operator::TimesEquals, 2)
        }
        else if slice.starts_with("/=") {
            (Operator::DivideEquals, 2)
        }
        else if slice.starts_with("%=") {
            (Operator::ModulusEquals, 2)
        }
        else if slice.starts_with('<') {
            (Operator::Less, 1)
        }
        else if slice.starts_with('>') {
            (Operator::Greater, 1)
        }
        else if slice.starts_with('+') {
            (Operator::Plus, 1)
        }
        else if slice.starts_with('-') {
            (Operator::Minus, 1)
        }
        else if slice.starts_with('*') {
            (Operator::Times, 1)
        }
        else if slice.starts_with('/') {
            (Operator::Divide, 1)
        }
        else if slice.starts_with('%') {
            (Operator::Modulus, 1)
        }
        else if slice.starts_with('=') {
            (Operator::Equals, 1)
        }
        else {
            return Err(format!("Could not split operators: {slice}").into());
        };

        operators.push((op, tmp_span()));
        slice = &slice[advance..];
    }

    Ok(operators)
}


fn is_operator_char(ch: char) -> bool {
    let operators = ['+', '-', '*', '/', '=', '>', '<', '!', '%'];

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

            "Plus"           => matches!(token, T { body: TB::Operator(O::Plus), .. }),
            "Minus"          => matches!(token, T { body: TB::Operator(O::Minus), .. }),
            "Times"          => matches!(token, T { body: TB::Operator(O::Times), .. }), 
            "Divide"         => matches!(token, T { body: TB::Operator(O::Divide), .. }),
            "Modulus"        => matches!(token, T { body: TB::Operator(O::Modulus), .. }),
            "Equals"         => matches!(token, T { body: TB::Operator(O::Equals), .. }),
            "ThinRightArrow" => matches!(token, T { body: TB::Operator(O::ThinRightArrow), .. }),
            "DoubleEquals"   => matches!(token, T { body: TB::Operator(O::DoubleEquals), .. }),
            "NotEquals"      => matches!(token, T { body: TB::Operator(O::NotEquals), .. }),
            "LessEquals"     => matches!(token, T { body: TB::Operator(O::LessEquals), .. }),
            "GreaterEquals"  => matches!(token, T { body: TB::Operator(O::GreaterEquals), .. }),
            "Less"           => matches!(token, T { body: TB::Operator(O::Less), .. }),
            "Greater"        => matches!(token, T { body: TB::Operator(O::Greater), .. }),
            "PlusEquals"     => matches!(token, T { body: TB::Operator(O::PlusEquals), .. }),
            "MinusEquals"     => matches!(token, T { body: TB::Operator(O::MinusEquals), .. }),
            "TimesEquals"     => matches!(token, T { body: TB::Operator(O::TimesEquals), .. }),
            "DivideEquals"     => matches!(token, T { body: TB::Operator(O::DivideEquals), .. }),
            "ModulusEquals"     => matches!(token, T { body: TB::Operator(O::ModulusEquals), .. }),

            "Var" => matches!(token, T { body: TB::Keyword(K::Var), .. }),
            "Val" => matches!(token, T { body: TB::Keyword(K::Val), .. }),
            "Fn"  => matches!(token, T { body: TB::Keyword(K::Fn), .. }),
            "True" => matches!(token, T { body: TB::Keyword(K::True), .. }),
            "False" => matches!(token, T { body: TB::Keyword(K::False), .. }),
            "If" => matches!(token, T { body: TB::Keyword(K::If), .. }),
            "Else" => matches!(token, T { body: TB::Keyword(K::Else), .. }),
            "Not" => matches!(token, T { body: TB::Keyword(K::Not), .. }),
            "And" => matches!(token, T { body: TB::Keyword(K::And), .. }),
            "Or" => matches!(token, T { body: TB::Keyword(K::Or), .. }),
            "While" => matches!(token, T { body: TB::Keyword(K::While), .. }),
            "Return" => matches!(token, T { body: TB::Keyword(K::Return), .. }),
            
            _ => return Err(format!("Bad token type: \"{token_type}\"").into())
        })
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /* TODO: Actually fill this out */

        f.write_str("{token}")
    }
}
