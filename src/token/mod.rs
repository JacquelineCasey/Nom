//! Defines the `Token` type and handles tokenization of an input string.
//!
//! The `Token` type is defined to be compatible with Parsley. It is a struct
//! that contains an enum describing the type of the token (and other information,
//! like which operator it is). The outer struct also contains span information,
//! describing where the token starts and ends, for the purpose of nicer error
//! messages.
//!
//! The program is tokenized in a fairly ad-hoc manner. Tokenization is a very
//! local process, this is only a small amount of looking ahead at future characters.
//!
//! The tokenization removes excess whitespace, and removes comments.

#[cfg(test)]
mod tests;

use std::cmp::{max, min};
use std::rc::Rc;
use std::str::FromStr;

use crate::error::TokenError;

/* Span definition and basic functions */

/// A `Span` descibes a contigous group of characters, in a specific source file (or
/// pseudo source file).
///
/// The span is given as a half open interval, though it may be represented differently
/// in error messages.
#[derive(Debug, Clone)]
pub struct Span {
    /// The path to a file, or a name representing a pseudo file, like "\<input\>".
    pub file: Rc<String>,
    /// The line the span starts on (1 based).
    pub start_line: usize,
    /// The line the span ends on (1 based).
    pub end_line: usize,
    /// The column the span starts on (1 based).
    pub start_col: usize,
    /// The column the span ends on (1 based).
    pub end_col: usize,
}

impl Span {
    /// Given two spans, returns a new span that includes both.
    ///
    /// This is used heavily in `ast`, where we determine a span for every `ASTNode`
    /// in the program.
    pub fn combine(a: &Span, b: &Span) -> Span {
        assert!(*a.file == *b.file);

        Span {
            file: Rc::clone(&a.file),
            start_line: min(a.start_line, b.start_line),
            end_line: max(a.end_line, b.end_line),
            start_col: min(a.start_col, a.start_col),
            end_col: max(a.end_col, b.end_col),
        }
    }

    /// Given a nonempty slice of Spans, returns a new Span that includes them all.
    ///
    /// This is used heavily in `ast`, where we determine a span for every `ASTNode`
    /// in the program.
    pub fn combine_all(spans: &[Span]) -> Span {
        assert!(!spans.is_empty());

        let mut final_span = spans[0].clone();

        for span in &spans[1..] {
            final_span = Span::combine(&final_span, span);
        }

        final_span
    }
}

/* Token and related types */

/// Represents a single token in a Nom program.
#[derive(Debug, Clone)]
pub struct Token {
    /// Represents the primary information of the token - its type, and any info
    /// related to that type.
    pub body: TokenBody,
    /// Represents the span of the token, i.e. where in the source file it came from.
    pub span: Span,
    // TODO: If comments ever could apply to an element as metadata, maybe that could go here?
}

/// Describes the information of the token, without any metadata (spans, etc.)
///
/// Each variant is a type of token, and comes with information specific to that
/// type.
#[derive(Debug, Clone)]
pub enum TokenBody {
    /// An identifier, or name, in the program. Used for variables, functions,
    /// struct members, and so on. The string is naturally the name. Not allowed
    /// to be any keyword.
    Identifier(String),
    /// A keyword in the program. Used in various control flow and other constructs.
    Keyword(Keyword),
    /// A string literal in the program. The String is of course the data
    /// of the literal. Escapes are carefully processed during tokenization, so the
    /// string here is the processed version. The quotes that mark the literal are
    /// not included in the data.
    #[allow(unused)]
    StringLiteral(String),
    /// A character literal in the program. The char is of course the character
    /// in the literal. Escapes are processed. The quotes that mark the literal
    /// are not included in the data.
    #[allow(unused)]
    CharLiteral(char),
    /// A numeric literal in the program. The string is the literal, and will be
    /// converted to an actual number later once type information is available.
    NumericLiteral(String),
    /// An operator in the program. We may someday allow users to define operators,
    /// so we handle them separately from punctuation, although they are somewhat
    /// similar.
    Operator(Operator),
    /// A piece of punctuation in the program. Differs from operators by being much
    /// less complicated.
    Punctuation(Punctuation),
}

/// Represents a supported terminal identification. Useful for processing token related errors from
/// parsley. If we fail to convert a string representing a terminal into this type, that is an error
/// on our part (likely an issue in grammar.parsley).
pub enum Terminal {
    Identifier,
    NumericLiteral,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    Semicolon,
    Comma,
    Colon,
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    Equals,
    ThinRightArrow,
    DoubleEquals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivideEquals,
    ModulusEquals,
    Dot,
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
    Struct,
}

/// Performs conversion from the terminal name to the enum.
impl TryFrom<&str> for Terminal {
    type Error = String;

    fn try_from(terminal_name: &str) -> Result<Self, Self::Error> {
        Ok(match terminal_name {
            "Identifier" => Terminal::Identifier,
            "NumericLiteral" => Terminal::NumericLiteral,
            "LeftCurlyBrace" => Terminal::LeftCurlyBrace,
            "RightCurlyBrace" => Terminal::RightCurlyBrace,
            "LeftParenthesis" => Terminal::LeftParenthesis,
            "RightParenthesis" => Terminal::RightParenthesis,
            "LeftSquareBracket" => Terminal::LeftSquareBracket,
            "RightSquareBracket" => Terminal::RightSquareBracket,
            "Semicolon" => Terminal::Semicolon,
            "Comma" => Terminal::Comma,
            "Colon" => Terminal::Colon,
            "Plus" => Terminal::Plus,
            "Minus" => Terminal::Minus,
            "Times" => Terminal::Times,
            "Divide" => Terminal::Divide,
            "Modulus" => Terminal::Modulus,
            "Equals" => Terminal::Equals,
            "ThinRightArrow" => Terminal::ThinRightArrow,
            "DoubleEquals" => Terminal::DoubleEquals,
            "NotEquals" => Terminal::NotEquals,
            "LessEquals" => Terminal::LessEquals,
            "GreaterEquals" => Terminal::GreaterEquals,
            "Less" => Terminal::Less,
            "Greater" => Terminal::Greater,
            "PlusEquals" => Terminal::PlusEquals,
            "MinusEquals" => Terminal::MinusEquals,
            "TimesEquals" => Terminal::TimesEquals,
            "DivideEquals" => Terminal::DivideEquals,
            "ModulusEquals" => Terminal::ModulusEquals,
            "Dot" => Terminal::Dot,
            "Var" => Terminal::Var,
            "Val" => Terminal::Val,
            "Fn" => Terminal::Fn,
            "True" => Terminal::True,
            "False" => Terminal::False,
            "If" => Terminal::If,
            "Else" => Terminal::Else,
            "Not" => Terminal::Not,
            "And" => Terminal::And,
            "Or" => Terminal::Or,
            "While" => Terminal::While,
            "Return" => Terminal::Return,
            "Struct" => Terminal::Struct,
            _ => Err(format!("Bad token type: \"{terminal_name}\""))?,
        })
    }
}

/// Represents the possible keywords in a Nom program.
///
/// Using an enum here is more efficient and type safe compared to just using strings.
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
    Struct,
}

impl FromStr for Keyword {
    type Err = TokenError; // Likely ignored by algorithm.

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
            "struct" => K::Struct,
            _ => Err(TokenError("Not a keyword".to_string()))?,
        })
    }
}

/// Represents an operator in the Nom Programming language.
///
/// Note that there is some overlap in which characters each operator uses (e.g. `+` for `Plus`, but `+=`
/// for `PlusEquals`), so a greedy "longest match" algorithm is used to determine which
/// operators are actually present.
#[derive(Debug, Clone)]
pub enum Operator {
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
    Dot,
}

/// A piece of punctuation in a Nom Program. Each is a single character.
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
    type Error = (); // Likely ignored by algorithm.;

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
            _ => Err(()),
        }
    }
}

/* Tokenization Algorithm */

/// Attaches span information to an input string.
///
/// Given a input string (and a filename), produces an iterator which yields the
/// characters of the string, along with the span representing those characters.
/// During the tokenization process, these spans are combined to produce the spans
/// for the tokens.
fn add_span_info(input: &str, file: Rc<String>) -> impl std::iter::Iterator<Item = (char, Span)> + '_ {
    let mut line_num = 1;
    let mut col_num = 1;

    input.chars().map(move |ch| {
        let ret_val = (
            ch,
            Span {
                file: Rc::clone(&file),
                start_line: line_num,
                end_line: line_num,
                start_col: col_num,
                end_col: col_num + 1,
            },
        );

        col_num += 1;

        if ch == '\n' {
            line_num += 1;
            col_num = 1;
        }

        ret_val
    })
}

/// Tokenizes input, converting a string to a list of tokens.
///
/// Accepts input as a string, and a file path for book-keeping (spans). The file
/// path may also be a pseudo file path, like "\<input\>".
pub fn tokenize(input: &str, file_path: &str) -> Result<Vec<Token>, TokenError> {
    let mut tokens: Vec<Token> = vec![];

    let fake_file = Rc::new(file_path.to_owned());

    let mut iter = add_span_info(input, fake_file).peekable();
    while let Some((ch, _)) = iter.peek() {
        if *ch == '\"' {
            let (token, span) = take_string_literal(&mut iter)?;
            tokens.push(Token { body: token, span });
        } else if *ch == '\'' {
            let (token, span) = take_char_literal(&mut iter)?;
            tokens.push(Token { body: token, span });
        } else if is_operator_char(*ch) {
            let operators = take_operators(&mut iter)?;
            for (op, span) in operators {
                tokens.push(Token { body: TokenBody::Operator(op), span });
            }
        } else if ch.is_ascii_digit() {
            let (token, span) = take_numeric_literal(&mut iter)?;
            tokens.push(Token { body: token, span });
        } else if let Ok(punct) = Punctuation::try_from(*ch) {
            let (_, span) = iter.next().expect("Known to exist.");
            tokens.push(Token { body: TokenBody::Punctuation(punct), span });
        } else if is_identifier_char(*ch) {
            let (token, span) = take_identifier_or_keyword(&mut iter)?;
            tokens.push(Token { body: token, span });
        } else if ch.is_whitespace() {
            iter.next().expect("Known");
        } else {
            return Err(format!("Cannot start token with {}", *ch).into());
        }
    }

    Ok(tokens)
}

/// Given an iterator from `tokenize()`, extracts a string literal token.
///
/// Note that the string literal is escaped here, so the token contains the true
/// data that should appear in the program.
///
/// Caller should be sure that a string literal appears at the front of the iterator,
/// for instance by peeking for a double quote.
fn take_string_literal(
    iter: &mut impl std::iter::Iterator<Item = (char, Span)>,
) -> Result<(TokenBody, Span), TokenError> {
    let mut spans = vec![];
    let (first, first_span) = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    spans.push(first_span);

    if first != '\"' {
        return Err("Expected character '\"'.".into());
    }

    let mut string = String::new();

    for (ch, ch_span) in iter {
        // TODO: Allow [\"] to escape the double quote

        spans.push(ch_span);

        if ch == '\"' {
            return Ok((TokenBody::StringLiteral(deliteralize(string)?), Span::combine_all(&spans)));
        }

        string.push(ch);
    }

    Err("Expected character '\"'. String literal does not terminate.".into())
}

/// Given an iterator from `tokenize()`, extracts a character literal token.
///
/// Note that the character literal is escaped here, so the token contains the true
/// data the should appear in the program.
///
/// Caller should be sure that a character literal appears at the front of the iterator,
/// for instance by peeking for a single quote.
fn take_char_literal(
    iter: &mut impl std::iter::Iterator<Item = (char, Span)>,
) -> Result<(TokenBody, Span), TokenError> {
    let mut spans = vec![];
    let (first, first_span) = iter.next().ok_or(TokenError("Expected character, found nothing".to_string()))?;
    spans.push(first_span);

    if first != '\'' {
        return Err("Expected character '\''.".into());
    }

    let mut string = String::new();

    for (ch, ch_span) in iter {
        // TODO: Allow [\'] to escape the single quote

        spans.push(ch_span);

        if ch == '\'' {
            return Ok((TokenBody::CharLiteral(literal_to_char(&string)?), Span::combine_all(&spans)));
        }

        string.push(ch);
    }

    Err("Expected character '\''. Char literal does not terminate.".into())
}

/// Given an iterator from `tokenize()`, extracts a numeric literal token.
///
/// Note that, at time of writing, the data extracted for the literal remains in
/// string form, until it is processed later around type checking time.
///
/// Caller should be sure that a numeric literal appears at the front of the iterator,
/// for example by peeking the iterator.
fn take_numeric_literal(
    iter: &mut std::iter::Peekable<impl std::iter::Iterator<Item = (char, Span)>>,
) -> Result<(TokenBody, Span), TokenError> {
    if !matches!(iter.peek(), Some((ch, _)) if ch.is_ascii_digit()) {
        return Err("Expected Digit.".into());
    }

    let mut string = String::new();
    let mut spans = vec![];

    while let Some((ch, _)) = iter.peek() {
        if is_numeric_literal_char(*ch) {
            let (ch, ch_span) = iter.next().expect("Known to exist");

            string.push(ch);
            spans.push(ch_span);
        } else {
            break;
        }
    }

    Ok((TokenBody::NumericLiteral(string), Span::combine_all(&spans)))
}

/// Given an iterator from `tokenize()`, extracts an identifier or keyword token.
///
/// Keywords may not be used as identifiers. This function extracts a word, and
/// converts it into the appropriate type based on contents.
///
/// Caller should be sure that a identifier or keyword appears at the front of the
/// iterator, for example by peeking the iterator.
fn take_identifier_or_keyword(
    iter: &mut std::iter::Peekable<impl std::iter::Iterator<Item = (char, Span)>>,
) -> Result<(TokenBody, Span), TokenError> {
    if !matches!(iter.peek(), Some((ch, _)) if is_identifier_char(*ch)) {
        return Err("Expected Identifier character.".into());
    }

    let mut string = String::new();
    let mut span: Option<Span> = None;
    while let Some((ch, _)) = iter.peek() {
        if is_identifier_char(*ch) {
            let (ch, ch_span) = iter.next().expect("Known to exist");

            string.push(ch);
            span = match span {
                Some(old) => Some(Span::combine(&old, &ch_span)),
                None => Some(ch_span),
            }
        } else {
            break;
        }
    }

    match Keyword::from_str(&string) {
        Ok(keyword) => Ok((TokenBody::Keyword(keyword), span.expect("Known to exist"))),
        Err(_) => Ok((TokenBody::Identifier(string), span.expect("Known to exist"))),
    }
}

/// Given an iterator from `tokenize()`, extracts a sequence of operator tokens.
///
/// Important: a single foward slash looks like an operator, but could be a comment.
/// Still, the iterator can be passed to this function, and in the event it is a comment,
/// the full line is consumed and discarded, and an empty vector is returned.
///
/// Caller should be sure that either a comment or a sequence of operators appears
/// at the front of the iterator, for example by peeking the iterator.
fn take_operators(
    iter: &mut std::iter::Peekable<impl std::iter::Iterator<Item = (char, Span)>>,
) -> Result<Vec<(Operator, Span)>, TokenError> {
    if !matches!(iter.peek(), Some((ch, _)) if is_operator_char(*ch)) {
        return Err("Expected operator character.".into());
    }

    let mut string = String::new();
    let mut spans = vec![];
    while let Some((ch, _)) = iter.peek() {
        if is_operator_char(*ch) {
            let (ch, ch_span) = iter.next().expect("Known to exist");
            string.push(ch);
            spans.push(ch_span);
        } else {
            break;
        }
    }

    // Now we continually remove the largest operator that works. Yes, this is greedy,
    // could improve later or just use sane operators with little overlap.

    let mut operators = vec![];

    let mut slice = &string[..];
    let mut span_slice = &spans[..];
    while !slice.is_empty() {
        let (op, advance) = if slice.starts_with("//") {
            // Discard comment
            for (ch, _) in iter.by_ref() {
                if ch == '\n' {
                    break;
                }
            }

            return Ok(operators);
        }
        // This is an operator for lexical reasons. Punctuation has to be single characters.
        else if slice.starts_with("->") {
            (Operator::ThinRightArrow, 2)
        } else if slice.starts_with("==") {
            (Operator::DoubleEquals, 2)
        } else if slice.starts_with("!=") {
            (Operator::NotEquals, 2)
        } else if slice.starts_with("<=") {
            (Operator::LessEquals, 2)
        } else if slice.starts_with(">=") {
            (Operator::GreaterEquals, 2)
        } else if slice.starts_with("+=") {
            (Operator::PlusEquals, 2)
        } else if slice.starts_with("-=") {
            (Operator::MinusEquals, 2)
        } else if slice.starts_with("*=") {
            (Operator::TimesEquals, 2)
        } else if slice.starts_with("/=") {
            (Operator::DivideEquals, 2)
        } else if slice.starts_with("%=") {
            (Operator::ModulusEquals, 2)
        } else if slice.starts_with('<') {
            (Operator::Less, 1)
        } else if slice.starts_with('>') {
            (Operator::Greater, 1)
        } else if slice.starts_with('+') {
            (Operator::Plus, 1)
        } else if slice.starts_with('-') {
            (Operator::Minus, 1)
        } else if slice.starts_with('*') {
            (Operator::Times, 1)
        } else if slice.starts_with('/') {
            (Operator::Divide, 1)
        } else if slice.starts_with('%') {
            (Operator::Modulus, 1)
        } else if slice.starts_with('=') {
            (Operator::Equals, 1)
        } else if slice.starts_with('.') {
            (Operator::Dot, 1)
        } else {
            return Err(format!("Could not split operators: {slice}").into());
        };

        operators.push((op, Span::combine_all(&span_slice[..advance])));

        span_slice = &span_slice[advance..];
        slice = &slice[advance..];
    }

    Ok(operators)
}

/// Helper function that determines if an operator may be part of an operator.
fn is_operator_char(ch: char) -> bool {
    let operators = ['+', '-', '*', '/', '=', '>', '<', '!', '%', '.'];

    operators.contains(&ch)
}

/// Helper function that determines if a character might begin a numeric literal.
///
/// Note: At time of writing, we do not consider negative literals. Instead, we
/// may parse `- 2` as the negation of a literal positive `2`. At time of writing,
/// this actually doesn't work at all, and instead we can get around this by writing
/// `0 - 2`.
fn is_numeric_literal_char(ch: char) -> bool {
    // TODO: Support for floats and numbers of different bases.

    // See ch.is_digit for arbitrary base
    ch.is_ascii_digit()
}

/// Helper function that determines if a character might be part of an identifier.
///
/// Note: While digits are permitted in identifiers, they cannot begin them, because
/// this triggers the numeric literal parsing instead.
fn is_identifier_char(ch: char) -> bool {
    // Note that digits won't work at start due to algorithm design.
    ch.is_ascii_alphabetic() || ch.is_ascii_digit() || ch == '_'
}

/// Helper function that processes literals in strings.
///
/// At time of writing, nothing is actually done, although escaping a quote does
/// work.
#[allow(clippy::unnecessary_wraps)] // When we implement this fully this goes away.
fn deliteralize(string: String) -> Result<String, TokenError> {
    // TODO: Actually turn literal into represented string
    Ok(string)
}

/// Helper function that converts a character literal into a single character.
///
/// At time of writing, almost nothing is done, other than a check that the literal
/// is a single character.
fn literal_to_char(string: &str) -> Result<char, TokenError> {
    // TODO: Add supports for escaped characters. Preferably shared with deliteralize?
    // Be aware of [\']
    if string.len() == 1 {
        Ok(string.chars().next().expect("Known to exist"))
    } else {
        Err("Bad character literal.".into())
    }
}

/* Make Token compatible with parsley. */

impl parsley::Token for Token {
    fn matches(token_type: &str, token: &Self) -> Result<bool, parsley::ParseError> {
        /* This allows the parsley grammar to refer to types of tokens. */

        use Keyword as K;
        use Operator as O;
        use Punctuation as P;
        use Token as T;
        use TokenBody as TB;

        Ok(match Terminal::try_from(token_type)? {
            Terminal::Identifier => matches!(token, T { body: TB::Identifier(_), .. }),

            Terminal::NumericLiteral => matches!(token, T { body: TB::NumericLiteral(_), .. }),

            Terminal::LeftCurlyBrace => matches!(token, T { body: TB::Punctuation(P::LeftCurlyBrace), .. }),
            Terminal::RightCurlyBrace => matches!(token, T { body: TB::Punctuation(P::RightCurlyBrace), .. }),
            Terminal::LeftParenthesis => matches!(token, T { body: TB::Punctuation(P::LeftParenthesis), .. }),
            Terminal::RightParenthesis => matches!(token, T { body: TB::Punctuation(P::RightParenthesis), .. }),
            Terminal::LeftSquareBracket => matches!(token, T { body: TB::Punctuation(P::LeftSquareBracket), .. }),
            Terminal::RightSquareBracket => matches!(token, T { body: TB::Punctuation(P::RightSquareBracket), .. }),

            Terminal::Semicolon => matches!(token, T { body: TB::Punctuation(P::Semicolon), .. }),
            Terminal::Comma => matches!(token, T { body: TB::Punctuation(P::Comma), .. }),
            Terminal::Colon => matches!(token, T { body: TB::Punctuation(P::Colon), .. }),

            Terminal::Plus => matches!(token, T { body: TB::Operator(O::Plus), .. }),
            Terminal::Minus => matches!(token, T { body: TB::Operator(O::Minus), .. }),
            Terminal::Times => matches!(token, T { body: TB::Operator(O::Times), .. }),
            Terminal::Divide => matches!(token, T { body: TB::Operator(O::Divide), .. }),
            Terminal::Modulus => matches!(token, T { body: TB::Operator(O::Modulus), .. }),
            Terminal::Equals => matches!(token, T { body: TB::Operator(O::Equals), .. }),
            Terminal::ThinRightArrow => matches!(token, T { body: TB::Operator(O::ThinRightArrow), .. }),
            Terminal::DoubleEquals => matches!(token, T { body: TB::Operator(O::DoubleEquals), .. }),
            Terminal::NotEquals => matches!(token, T { body: TB::Operator(O::NotEquals), .. }),
            Terminal::LessEquals => matches!(token, T { body: TB::Operator(O::LessEquals), .. }),
            Terminal::GreaterEquals => matches!(token, T { body: TB::Operator(O::GreaterEquals), .. }),
            Terminal::Less => matches!(token, T { body: TB::Operator(O::Less), .. }),
            Terminal::Greater => matches!(token, T { body: TB::Operator(O::Greater), .. }),
            Terminal::PlusEquals => matches!(token, T { body: TB::Operator(O::PlusEquals), .. }),
            Terminal::MinusEquals => matches!(token, T { body: TB::Operator(O::MinusEquals), .. }),
            Terminal::TimesEquals => matches!(token, T { body: TB::Operator(O::TimesEquals), .. }),
            Terminal::DivideEquals => matches!(token, T { body: TB::Operator(O::DivideEquals), .. }),
            Terminal::ModulusEquals => matches!(token, T { body: TB::Operator(O::ModulusEquals), .. }),
            Terminal::Dot => matches!(token, T { body: TB::Operator(O::Dot), .. }),

            Terminal::Var => matches!(token, T { body: TB::Keyword(K::Var), .. }),
            Terminal::Val => matches!(token, T { body: TB::Keyword(K::Val), .. }),
            Terminal::Fn => matches!(token, T { body: TB::Keyword(K::Fn), .. }),
            Terminal::True => matches!(token, T { body: TB::Keyword(K::True), .. }),
            Terminal::False => matches!(token, T { body: TB::Keyword(K::False), .. }),
            Terminal::If => matches!(token, T { body: TB::Keyword(K::If), .. }),
            Terminal::Else => matches!(token, T { body: TB::Keyword(K::Else), .. }),
            Terminal::Not => matches!(token, T { body: TB::Keyword(K::Not), .. }),
            Terminal::And => matches!(token, T { body: TB::Keyword(K::And), .. }),
            Terminal::Or => matches!(token, T { body: TB::Keyword(K::Or), .. }),
            Terminal::While => matches!(token, T { body: TB::Keyword(K::While), .. }),
            Terminal::Return => matches!(token, T { body: TB::Keyword(K::Return), .. }),
            Terminal::Struct => matches!(token, T { body: TB::Keyword(K::Struct), .. }),
        })
    }
}

/* Additional trait implementation. */

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /* TODO: Actually fill this out */

        f.write_str("{token}")
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}:{}", self.file, self.start_line, self.start_col))
    }
}
