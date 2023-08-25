/* Eventually, this binary will be a tool for compiling (?) or running possibly many
 * Nom files. */


// I use `cargo clippy -- -D clippy::pedantic`
#![allow(
    clippy::missing_errors_doc,  // Docs? Lol.
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

 
mod token;  // Tokenize, instruct parsley how tokens work
mod ast;  // Generate AST from parsley's concrete syntax tree
mod interpret;  // Interpret the AST. Soon to be deprecated in favor of a virtual machine model.
mod instructions;  // Define the instruction set of the VM.
mod generate;  // Traverses an AST and returns instructions and other data.
mod runtime;  // Runs generated instructions

mod util;  // Utility functions, etc.


use std::io::Read;


fn main() {
    let parser_definition = include_str!("grammar.parsley");  // Drops the string right into the binary.
    let parser = parsley::define_parser::<token::Token>(parser_definition).expect("Parser definition should be valid");

    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer).expect("Reading stdin should succeed");
    let tokens = token::tokenize(&buffer).expect("Tokenization should succeed");

    let syntax_tree = parser.parse_tokens(tokens, "Program").expect("Parsing should not fail");
    let abstract_syntax_tree = ast::build_ast(&syntax_tree).expect("Specialization to AST should not fail");

    // println!("{abstract_syntax_tree:?}");

    let mut interpretter = interpret::Interpretter::new();
    interpretter.run(abstract_syntax_tree).expect("Ran to completion");
}
