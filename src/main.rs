/* Eventually, this binary will be a tool for compiling (?) or running possibly many
 * Nom files. */
 
mod token;
mod ast;
mod interpret;

use std::io::Read;


fn main() {
    let parser_definition = include_str!("grammar.parsley").to_string();  // Drops the string right into the binary.
    let parser = parsley::define_parser::<token::Token>(parser_definition).expect("Parser definition should be valid");

    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer).expect("Reading stdin should succeed");
    let tokens = token::tokenize(&buffer).expect("Tokenization should succeed");

    let syntax_tree = parser.parse_tokens(tokens, "Program").expect("Parsing should not fail");
    let abstract_syntax_tree = ast::build_ast(syntax_tree).expect("Specialization to AST should not fail");

    println!("{:?}", abstract_syntax_tree);

    interpret::interpret_ast(abstract_syntax_tree).expect("Ran to completion");
}
