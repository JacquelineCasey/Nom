
// I use `cargo clippy -- -D clippy::pedantic`
#![allow(
    clippy::missing_errors_doc,  // Docs? Lol.
    clippy::missing_panics_doc,  // Docs? Lmao.
    clippy::must_use_candidate,  // What?
    clippy::module_name_repetitions,  // Maybe a little weird but I'm bad at naming things.
    clippy::cast_sign_loss,  // I know
    clippy::cast_possible_truncation,  // I know
    clippy::cast_possible_wrap,  // I know
    clippy::if_not_else,  // Actually I like this, its the gaurd pattern
    clippy::upper_case_acronyms,  // Deal with it
    clippy::enum_glob_use,  // This is fine
    clippy::match_wildcard_for_single_variants,  // I do this on purpose
)]


pub mod token;  // Tokenize, instruct parsley how tokens work
pub mod ast;  // Generate AST from parsley's concrete syntax tree
pub mod interpret;  // Interpret the AST. Soon to be deprecated in favor of a virtual machine model.
pub mod instructions;  // Define the instruction set of the VM.
pub mod generate;  // Traverses an AST and returns instructions and other data.
pub mod runtime;  // Runs generated instructions

mod util;  // Utility functions, etc.
