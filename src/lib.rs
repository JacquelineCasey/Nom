
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
    clippy::match_wildcard_for_single_variants,  // I do this on purpose
)]


mod token;  // Tokenize, instruct parsley how tokens work
mod ast;  // Generate AST from parsley's concrete syntax tree
mod interpret;  // Interpret the AST. Soon to be deprecated in favor of a virtual machine model.
mod analysis;  // Analyze an AST, checking correctness and learning type info.
mod instructions;  // Define the instruction set of the VM.
mod generate;  // Traverses an AST and returns instructions and other data.
pub mod runtime;  // Runs generated instructions

mod util;  // Utility functions, etc.


pub fn compile(input: &str) -> Vec<instructions::Instruction> {
    // TODO: Should these expects turn into yet another error type?

    let parser_definition = include_str!("grammar.parsley");  // Drops the string right into the binary.
    let parser = parsley::define_parser::<token::Token>(parser_definition).expect("Parser definition should be valid");

    let tokens = token::tokenize(input).expect("Tokenization should succeed");

    let syntax_tree = parser.parse_tokens(tokens, "Program").expect("Parsing should not fail");
    let abstract_syntax_tree = ast::build_ast(&syntax_tree).expect("Specialization to AST should not fail");

    let analyzed_ast = analysis::AnalyzedAST::new(abstract_syntax_tree).expect("Analysis succeeded");
    let generator = generate::CodeGenerator::new();
    
    generator.generate(&analyzed_ast).expect("Code should generate successfully")
}
