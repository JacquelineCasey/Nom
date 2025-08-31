//! Defines error types and utilities, as well as the pretty display of user facing compile errors.

mod error_types;
mod pretty_messages;

pub use error_types::{ASTError, AnalysisError, CompileError, GenerateError, TokenError};
pub use pretty_messages::pretty_error_message;
