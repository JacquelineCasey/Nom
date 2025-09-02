//! Provides the tokenization algorithm.

use super::token_types::{Keyword, Operator, Punctuation, Span, Token, TokenBody};

use crate::error::TokenError;
use crate::FileOrString;

use std::str::FromStr;

/* The Algorithm. */

/// Tokenizes input, converting a string to a list of tokens.
///
/// Accepts input as a string, and a [`FileOrString`] for book-keeping (spans).
pub fn tokenize(input: &str, source: FileOrString) -> Result<Vec<Token>, TokenError> {
    let mut tokens: Vec<Token> = vec![];

    let mut iter = add_span_info(input, source).peekable();
    while let Some((ch, span)) = iter.peek() {
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
            return Err(TokenError::ProblemAtSpan(format!("Cannot start token with '{}'.", *ch), span.clone()));
        }
    }

    Ok(tokens)
}

/* Consruction and Manipulation of the (char, Span) iterator. */

/// Attaches span information to an input string.
///
/// Given a input string (and a source), produces an iterator which yields the
/// characters of the string, along with the span representing those characters.
/// During the tokenization process, these spans are combined to produce the spans
/// for the tokens.
fn add_span_info(input: &str, source: FileOrString) -> impl std::iter::Iterator<Item = (char, Span)> + '_ {
    let mut line_num = 1;
    let mut col_num = 1;

    input.chars().map(move |ch| {
        let ret_val = (
            ch,
            Span {
                source: source.clone(), // Fairly cheap, copies a small string and a pointer at most.
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
    let (first, first_span) = iter.next().ok_or("Expected character, found nothing")?;
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
    let (first, first_span) = iter.next().ok_or("Expected character, found nothing")?;
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
        } else if slice.starts_with("->") {
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
            return Err(TokenError::ProblemAtSpan(
                "Could not split operators.".to_string(),
                Span::combine_all(span_slice),
            ));
        };

        operators.push((op, Span::combine_all(&span_slice[..advance])));

        span_slice = &span_slice[advance..];
        slice = &slice[advance..];
    }

    Ok(operators)
}

/* Helpers that classify certain kinds of characters. */

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

/* Helpers for dealing with literals. */

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
