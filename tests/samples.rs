
use test_generator::test_resources;

use std::io::Read;

use nom::compile;
use nom::runtime::Runtime;


#[test_resources("samples/**/*.nom")]
fn run_sample(resource: &str) {
    let mut file = std::fs::File::open(resource).expect("File opens");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("Read successful");

    let code = compile(&input);

    let mut runtime = Runtime::new(code);
    runtime.run();

    // TODO: Verify expected output
}
