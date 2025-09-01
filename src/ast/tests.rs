use test_generator::test_resources;

use std::{io::Read, rc::Rc};

use crate::FileOrString;

use super::*;

static PARSER_DEFINITION: &str = include_str!("../grammar.parsley"); // Drops the string right into the binary.

fn read_file(resource: &str) -> String {
    let mut file = std::fs::File::open(resource).expect("File opens");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Read successful");
    input
}

fn validate_spans<'a>(ast: &'a mut AnyAST<'a>) {
    let span = &ast.get_node_data().span;

    assert!(span.pseudo_path() == "<test-input>");
    assert!(span.start_line <= span.end_line);
    assert!(span.start_col < span.end_col);

    for mut child in ast.children() {
        validate_spans(&mut child);
    }
}

#[test_resources("samples/successful/**/*.nom")]
fn ast_has_spans(resource: &str) {
    let input = Rc::new(read_file(resource));

    let tokens =
        crate::token::tokenize(&input, FileOrString::String("<test-input>".to_string(), Rc::clone(&input))).unwrap();

    let parser =
        parsley::define_parser::<crate::token::Token>(PARSER_DEFINITION).expect("Parser definition should be valid");

    let syntax_tree = parser.parse_tokens(&tokens, "Program").unwrap();

    let mut ast = build_ast(&syntax_tree).unwrap();

    validate_spans(&mut AnyAST::File(&mut ast));

    crate::analysis::desugar(&mut ast);

    validate_spans(&mut AnyAST::File(&mut ast));

    /* TODO: Could do further tests to ensure no spans overlap (except between parents
     * and children), etc. */
}
