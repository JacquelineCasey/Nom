/* Eventually, this binary will be a tool for compiling (?) or running possibly many
 * Nom files. */
 
use std::io::Read;


fn main() {
    let parser_definition = include_str!("grammar.parsley").to_string();  // Drops the string right into the binary.

    let mut buffer = String::new();

    std::io::stdin().read_to_string(&mut buffer).expect("Failed to read stdin");

    println!("{}", buffer);

    println!("{}", parser_definition);
}
