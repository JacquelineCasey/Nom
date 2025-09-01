//! Tests for the token module.

use super::*;

use std::io::Read;

use test_generator::test_resources;

fn read_file(resource: &str) -> String {
    let mut file = std::fs::File::open(resource).expect("File opens");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Read successful");
    input
}

#[test_resources("samples/successful/**/*.nom")]
fn tokens_have_spans(resource: &str) {
    let input = read_file(resource);

    let tokens = tokenize(&input, "<test-input>").unwrap();

    for token in tokens {
        println!("{:?}", token);

        assert!(*token.span.file == "<test-input>");
        assert!(token.span.start_line <= token.span.end_line);
        assert!(token.span.start_col < token.span.end_col);
    }

    /* TODO: Could do further tests to ensure no tokens overlap, etc. */
}
