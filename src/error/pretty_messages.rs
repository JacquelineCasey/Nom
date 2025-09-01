//! Handles the display of pretty messages for errors.

use super::{ASTError, AnalysisError, CompileError, TokenError};

use crate::token::{Span, Terminal, Token};
use crate::CompilationEnvironment;

use std::collections::{BTreeSet, HashSet};

/* Public */

pub fn pretty_error_message(env: &CompilationEnvironment, err: &CompileError) -> String {
    // TODO: Do we really need the environment here?

    // Much more work to be done here, but I'll do it piecemeal for now.
    // I'd like to replace all of the string error messages throughout the code with
    // enums.
    match err {
        CompileError::Direct(msg) => format!("Error occurred during compilation:\n    {msg}"),
        CompileError::TokenError(token_error) => pretty_token_error_message(env, token_error),
        CompileError::ParseError(parse_error, tokens) => pretty_parse_error_message(env, parse_error, tokens),
        CompileError::ASTError(ASTError(msg)) => {
            format!("Error occurred during ast construction:\n    {msg}")
        }
        CompileError::AnalysisError(AnalysisError(msg)) => {
            format!("Error occurred during analysis:\n    {msg}")
        }
    }
}

/* Private Helpers */

fn pretty_token_error_message(_env: &CompilationEnvironment, err: &TokenError) -> String {
    match err {
        TokenError::Problem(description) => {
            format!("Error occurred during tokenization: {description}")
        }
        TokenError::ProblemAtSpan(description, span) => {
            format!("Error occurred during tokenization: {}\n{}", description, annotate(span))
        }
    }
}

fn pretty_parse_error_message(_env: &CompilationEnvironment, err: &parsley::ParseError, tokens: &[Token]) -> String {
    match err {
        parsley::ParseError::Internal(msg) => {
            format!("Internal error occurred during parsing:\n    {msg}")
        }
        parsley::ParseError::IncompleteParse { index, terminals } => {
            format!(
                "Error occurred during parsing:\
                \n    Failed to parse token at {} (token type: {:?})\
                \n    Expected {}.
                \n",
                tokens[*index].span,
                tokens[*index].body, // TODO: Replace with something prettier.
                terminal_choice_description(terminals),
            )
        }
        parsley::ParseError::OutOfInput { terminals } => {
            // TODO: Add the filter logic here as well.

            format!(
                "Error occurred during parsing:\
                \n    Ran out of input, expected more.\
                \n    Expected {}.\
                \n",
                terminal_choice_description(terminals),
            )
        }
    }
}

/// Returns a pretty description of a set of terminals, for use in the case where the user should pick one of them.
/// e.g. "one of ',', '.', an identifier, or an operator". Cases with many operators are identified, and "an
/// operator" is inserted instead.
fn terminal_choice_description(terminal_names: &HashSet<String>) -> String {
    let terminals = terminal_names
        .iter()
        .map(String::as_str)
        .map(Terminal::try_from)
        .collect::<Result<BTreeSet<_>, _>>()
        .expect("All terminal names converted.");

    let (terminals, operator) = if terminals.iter().filter(|terminal| terminal.is_user_operator()).count() > 3 {
        (terminals.into_iter().filter(|terminal| !terminal.is_user_operator()).collect(), Some("an operator"))
    } else {
        (terminals, None)
    };

    let terminals_to_show: Vec<&'static str> =
        terminals.into_iter().map(Terminal::pretty_string).chain(operator).collect();

    if terminals_to_show.len() == 2 {
        terminals_to_show.join(" or ")
    } else if terminals_to_show.len() >= 3 {
        "one of ".to_owned()
            + &terminals_to_show[..terminals_to_show.len() - 1].join(", ")
            + ", or "
            + terminals_to_show[terminals_to_show.len() - 1]
    } else {
        // 1 (or if something went wrong, 0)
        (*terminals_to_show.first().expect("Some terminal")).to_string()
    }
}

/// Produce an annotation, which is a snippet of the input code with certain parts underlined.
fn annotate(span: &Span) -> String {
    // This will likely become more sophisticated over time, but for now we just print the lines of a single span, with
    // the span part underlined.

    let line_number_size = span.end_line.to_string().len();

    let maybe_temp_storage; // Huh, mut not needed... Has Rust gotten smarter, or has it always allowed this.
    let source_view: &str = match &span.source {
        crate::FileOrString::File(path) => match std::fs::read_to_string(path) {
            Ok(str) => {
                maybe_temp_storage = str;
                &maybe_temp_storage
            }
            Err(_) => return "[Failed to open file while rendering this error.]".into(),
        },
        crate::FileOrString::String(_, string) => &string,
    };

    let annotations: Vec<String> = source_view
        .lines()
        .skip(span.start_line - 1)
        .take(span.end_line - span.start_line + 1)
        .zip(span.start_line..=span.end_line)
        .map(|(line, line_number)| {
            // Care is taken not to underline the parts that could be trimmed.
            let mut string_started = false;
            let mut annotation_line = String::new();

            for (i, char) in line.trim().chars().enumerate() {
                if !char.is_whitespace() {
                    string_started = true;
                }

                if !string_started {
                    annotation_line += " ";
                    continue;
                }

                // How much of the line needs to be underlined depends on which line in the span we in.
                let underline_needed = if line_number == span.start_line && line_number == span.end_line {
                    // Note that columns are one based, and half open.
                    i + 1 >= span.start_col && i + 1 < span.end_col
                } else if line_number == span.start_line {
                    i + 1 >= span.start_col
                } else if line_number == span.end_line {
                    i + 1 < span.end_col
                } else {
                    true
                };

                annotation_line += if underline_needed { "^" } else { " " };
            }

            let mut padded_line_number = line_number.to_string();
            padded_line_number += &" ".repeat(line_number_size - padded_line_number.len());

            format!("{} | {}\n{} | {}", padded_line_number, line, &" ".repeat(line_number_size), annotation_line)
        })
        .collect();

    format!("{span}:\n{}\n", annotations.join("\n"))
}
