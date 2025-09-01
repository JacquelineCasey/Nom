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

mod token_types;
mod tokenize;

#[cfg(test)]
mod tests;

pub use token_types::{Keyword, Operator, Punctuation, Span, Terminal, Token, TokenBody};
pub use tokenize::tokenize;
