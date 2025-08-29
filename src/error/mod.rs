// We do approximately one error type per module.
// For now, errors are really just strings, but I want to have user facing errors
// at some point.

use std::collections::{BTreeSet, HashSet};

use crate::{
    token::{Terminal, Token},
    CompilationEnvironment,
};

#[derive(Debug)]
pub struct ASTError(pub String);

impl From<&str> for ASTError {
    fn from(value: &str) -> Self {
        ASTError(value.to_string())
    }
}

impl From<String> for ASTError {
    fn from(value: String) -> Self {
        ASTError(value)
    }
}

#[derive(Debug)]
pub struct GenerateError(pub String);

impl From<&str> for GenerateError {
    fn from(value: &str) -> Self {
        GenerateError(value.to_string())
    }
}

impl From<String> for GenerateError {
    fn from(value: String) -> Self {
        GenerateError(value)
    }
}

#[derive(Debug)]
pub struct AnalysisError(pub String);

impl From<&str> for AnalysisError {
    fn from(value: &str) -> Self {
        AnalysisError(value.to_string())
    }
}

impl From<String> for AnalysisError {
    fn from(value: String) -> Self {
        AnalysisError(value)
    }
}

#[derive(Debug)]
pub struct TokenError(pub String);

impl From<&str> for TokenError {
    fn from(value: &str) -> Self {
        TokenError(value.to_string())
    }
}

impl From<String> for TokenError {
    fn from(value: String) -> Self {
        TokenError(value)
    }
}

#[derive(Debug)]
pub enum CompileError {
    Direct(String),
    TokenError(TokenError),
    ParseError(parsley::ParseError, Vec<Token>), // We pack the tokens for context.
    ASTError(ASTError),
    AnalysisError(AnalysisError),
}

impl From<&str> for CompileError {
    fn from(value: &str) -> Self {
        CompileError::Direct(value.to_string())
    }
}

impl From<String> for CompileError {
    fn from(value: String) -> Self {
        CompileError::Direct(value)
    }
}

impl From<TokenError> for CompileError {
    fn from(value: TokenError) -> Self {
        CompileError::TokenError(value)
    }
}

// Parse error doesn't have one due to differing structure.

impl From<ASTError> for CompileError {
    fn from(value: ASTError) -> Self {
        CompileError::ASTError(value)
    }
}

impl From<AnalysisError> for CompileError {
    fn from(value: AnalysisError) -> Self {
        CompileError::AnalysisError(value)
    }
}

pub fn pretty_error_msg(env: &CompilationEnvironment, err: &CompileError) -> String {
    // Much more work to be done here, but I'll do it piecemeal for now.
    // I'd like to replace all of the string error messages throughout the code with
    // enums.
    match err {
        CompileError::Direct(msg) => format!("Error occurred during compilation:\n    {msg}"),
        CompileError::TokenError(TokenError(msg)) => {
            format!("Error occurred during tokenization:\n    {msg}")
        }
        CompileError::ParseError(p_error, tokens) => pretty_parse_error_msg(env, p_error, tokens),
        CompileError::ASTError(ASTError(msg)) => {
            format!("Error occurred during ast construction:\n    {msg}")
        }
        CompileError::AnalysisError(AnalysisError(msg)) => {
            format!("Error occurred during analysis:\n    {msg}")
        }
    }
}

/// Returns a pretty description of a set of terminals, for use in the case where the user should pick one of them.
/// e.g. "one of ',', '.', an identifier, or an operator". Cases with many operators are identified, and "an
/// operator" is inserted instead.
pub fn terminal_choice_description(terminal_names: &HashSet<String>) -> String {
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
        "one of ".to_owned() + &terminals_to_show.join(", ")
    }
}

pub fn pretty_parse_error_msg(_env: &CompilationEnvironment, err: &parsley::ParseError, tokens: &[Token]) -> String {
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
