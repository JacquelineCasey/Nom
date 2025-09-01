//! Defines the types of errors that may occur at any stage during compilation.
//! Also defines simple conversions between strings and errors, and the module errors and the overall [`CompileError`]

use crate::token::Token;

/* Module Level Errors */

// We do approximately one error type per module.
// For now, errors are really just strings, but I want to have user facing errors
// at some point.

use crate::token::Span;

#[derive(Debug)]
pub enum TokenError {
    /// A problem described by a string.
    Problem(String),
    /// A problem (described as the string) localized at a particularl span.
    ProblemAtSpan(String, Span),
}

#[derive(Debug)]
pub struct ASTError(pub String);

#[derive(Debug)]
pub struct AnalysisError(pub String);

#[derive(Debug)]
pub struct GenerateError(pub String);

// Note that the Parsing stage is not present, we just user parsley::ParseError.

/* String Conversions. */

impl From<String> for TokenError {
    fn from(value: String) -> Self {
        TokenError::Problem(value)
    }
}

impl From<&str> for TokenError {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

impl From<String> for ASTError {
    fn from(value: String) -> Self {
        ASTError(value)
    }
}

impl From<&str> for ASTError {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

impl From<String> for AnalysisError {
    fn from(value: String) -> Self {
        AnalysisError(value)
    }
}

impl From<&str> for AnalysisError {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

impl From<String> for GenerateError {
    fn from(value: String) -> Self {
        GenerateError(value)
    }
}

impl From<&str> for GenerateError {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

/* Project Level Error */

#[derive(Debug)]
pub enum CompileError {
    /// An error associated with the compilation process itself, not any one step.
    Direct(String),
    TokenError(TokenError),
    // We pack the tokens for context. TODO: We should probably provide that as context instead. Then we can treat this
    // error more normally.
    ParseError(parsley::ParseError, Vec<Token>),
    ASTError(ASTError),
    AnalysisError(AnalysisError),
    // Note that GenerateErrors should always be our fault and not the user's. Not currently included, though we could
    // change that.
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
