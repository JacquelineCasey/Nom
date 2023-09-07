
use test_generator::test_resources;

use std::io::Read;

use nom::compile_file;
use nom::runtime::Runtime;


#[test_resources("samples/**/*.nom")]
fn run_sample(resource: &str) {
    let mut file = std::fs::File::open(resource).expect("File opens");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Read successful");

    let mut expected_output = input.lines()
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
        
    expected_output.push('\n');
    
    let code = compile_file(resource.to_string());
    println!("{}", code.iter().map(|instr| format!("{:?}", instr)).collect::<Vec<_>>().join("\n"));

    let mut runtime = Runtime::new(code);

    let mut buf = std::io::BufWriter::new(vec![]);
    runtime.run_debug(&mut buf);

    let output = String::from_utf8(buf.into_inner().expect("No IO Error")).expect("Good Conversion");

    assert_eq!(expected_output, output);
}
