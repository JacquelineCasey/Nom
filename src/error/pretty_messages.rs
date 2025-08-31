//! Handles the display of pretty messages for errors.

use super::{ASTError, AnalysisError, CompileError, TokenError};

use crate::token::{Terminal, Token};
use crate::CompilationEnvironment;

use std::collections::{BTreeSet, HashSet};

/* Public */

pub fn pretty_error_message(env: &CompilationEnvironment, err: &CompileError) -> String {
    // Much more work to be done here, but I'll do it piecemeal for now.
    // I'd like to replace all of the string error messages throughout the code with
    // enums.
    match err {
        CompileError::Direct(msg) => format!("Error occurred during compilation:\n    {msg}"),
        CompileError::TokenError(TokenError(msg)) => {
            format!("Error occurred during tokenization:\n    {msg}")
        }
        CompileError::ParseError(p_error, tokens) => pretty_parse_error_message(env, p_error, tokens),
        CompileError::ASTError(ASTError(msg)) => {
            format!("Error occurred during ast construction:\n    {msg}")
        }
        CompileError::AnalysisError(AnalysisError(msg)) => {
            format!("Error occurred during analysis:\n    {msg}")
        }
    }
}

/* Private Helpers */

fn pretty_parse_error_message(_env: &CompilationEnvironment, err: &parsley::ParseError, tokens: &[Token]) -> String {
    match err {
        parsley::ParseError::Internal(msg) => {
            format!("Internal error occurred during parsing:\n    {msg}")
        }
        parsley::ParseError::IncompleteParse { index, terminals } => {
            format!(
                "Error occurred during parsing:\
                \n    Failed to parse token at {} (token type: {:?})\
                \n    Expected {}.",
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
                \n    Expected {}.",
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
