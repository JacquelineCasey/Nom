
use test_generator::test_resources;

use std::io::Read;
use std::panic;

use nom::compile_file;
use nom::runtime::Runtime;
use nom::Instruction;

// Retrieves expected output or panic messages etc
// Returns a string with all lines that begin with //!, ignoring the prefix "//! " (note the space)
fn get_marked_comments(input: &str) -> String {
    let mut marked_lines = input.lines()
        .filter_map(|line| if line.starts_with("//! ") {
            Some(Ok(&line[4..]))
        }
        else if line.starts_with("//!") {
            Some(Err("Likely a typo: Put a space after \"//!\""))
        }
        else {
            None
        })
        .collect::<Result<Vec<_>, _>>()
        .expect("No issue with special comments")
        .join("\n");

    marked_lines.push('\n');

    marked_lines
}

fn read_file(resource: &str) -> String {
    let mut file = std::fs::File::open(resource).expect("File opens");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Read successful");
    input
}

// Convenience - dump an array of instructions
pub fn dump_instructions(instrs: &[Instruction]) -> String {
    instrs.iter()
        .enumerate()
        .map(|(i, instr)| format!("{i:<6}  {instr:?}"))
        .collect::<Vec<_>>()
        .join("\n")
}


#[test_resources("samples/successful/**/*.nom")]
fn run_successful(resource: &str) {
    let input = read_file(resource);
    let expected_output = get_marked_comments(&input);
    
    let code: Vec<Instruction> = compile_file(resource.to_string());
    println!("{}", dump_instructions(&code));

    let mut runtime = Runtime::new(code);

    let mut buf = std::io::BufWriter::new(vec![]);
    runtime.run_debug(&mut buf);

    let output = String::from_utf8(buf.into_inner().expect("No IO Error")).expect("Good Conversion");

    assert_eq!(expected_output, output);
}


// Note - The messages returned by such panics are definitely subject to change.
#[test_resources("samples/runtime-panic/**/*.nom")]
fn run_panics(resource: &str) {
    let input = read_file(resource);
    let expected_output = get_marked_comments(&input);
    
    let code: Vec<Instruction> = compile_file(resource.to_string());
    println!("{}", dump_instructions(&code));

    let mut runtime = Runtime::new(code);

    match panic::catch_unwind(move || runtime.run()) {
        Ok(_) => panic!("Success is unexpected"),
        Err(boxed_msg) => {
            let actual_msg = *(boxed_msg.downcast::<&str>().unwrap());
            assert!(actual_msg == expected_output.trim());
        },
    }
}
