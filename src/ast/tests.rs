
use test_generator::test_resources;

use std::io::Read;

use super::*;

static PARSER_DEFINITION: &str = include_str!("../grammar.parsley");  // Drops the string right into the binary.

fn read_file(resource: &str) -> String {
    let mut file = std::fs::File::open(resource).expect("File opens");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Read successful");
    input
}

#[test_resources("samples/successful/**/*.nom")]
fn ast_has_spans(resource: &str) {
    let input = read_file(resource);

    let tokens = crate::token::tokenize(&input, "<test-input>").unwrap();

    let parser = parsley::define_parser::<crate::token::Token>(PARSER_DEFINITION).expect("Parser definition should be valid");

    let syntax_tree = parser.parse_tokens(&tokens, "Program").unwrap();
    let mut ast = build_ast(&syntax_tree).unwrap();

    /* TODO: Analyze AST. Visitor pattern? */
    assert!(false);

    crate::analysis::desugar(&mut ast);

    /* TODO: Analyze AST again! */

    /* TODO: Could do further tests to ensure no tokens overlap (except between parents
     * and children), etc. */
}
