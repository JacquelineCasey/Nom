/* Eventually, this binary will be a tool for compiling (?) or running possibly many
 * Nom files. */

 
use nom::compile_string;
use nom::runtime::Runtime;

use std::io::Read;


fn main() {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer).expect("Reading stdin should succeed");

    let code = compile_string(buffer);

    for (i, instr) in code.iter().enumerate() {
        println!("{i: <5}: {instr:?}");
    }

    println!("\n-*-*-*-*- Running VM -*-*-*-*-\n");

    let mut runtime = Runtime::new(code);
    runtime.run_debug(&mut std::io::stdout());
}
