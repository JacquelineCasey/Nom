/* Eventually, this binary will be a tool for compiling (?) or running possibly many
 * Nom files. */
 
use std::io::Read;


fn main() {
    let parser_definition = include_str!("grammar.parsley").to_string();  // Drops the string right into the binary.

    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer).expect("Failed to read stdin");

    let parser = parsley::define_parser::<parsley::CharToken>(parser_definition).expect("Parser definition should be valid");

    let syntax_tree = parser.parse_string(buffer, "Program").expect("Parsing Failed");

    println!("{}", syntax_tree);
}
