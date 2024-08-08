
# Nom

Nom is a prototype / toy programming language. I am developing it for fun, and to
learn more about the inner workings of the compilation process.

## Quick Start

Nom should work with the current version of Rust. At time of writing, that is
Rust 1.80.0 (2021 Edition). To build and test this project:
- Obtain an up to date version of Cargo.
- Run `cargo test`. All tests should pass, unless I have recently started (and not finished)
  adding a new feature.

Then, you can have the project compile and run your Nom code. For instance:
- Running `cargo run < samples/successful/example_test.nom` will compile and run one of the sample programs.

## More about Nom

Nom is a compiled programming language, in the same sense that Java is compiled.
A Nom program is transformed into a simple set of instructions, which run on a virtual
machine (the Nom VM, or the Nom runtime.) You could split hairs and say that it is
interpretted, but you can do the same thing for Java, C#, etc (although they do have a JIT, and
Nom certainly does not).

Here is a small program in Nom.

```
// Ruthlessly inefficient function computing fibonacci numbers.
fn fib(n: i32) -> i32 {
    var result: i32 = 0;
    if n == 1 {
        result = 1;
    }
    else if n == 2 {
        result = 1;
    }
    else {
        result = fib(n-2) + fib(n-1);
    };

    result
}

fn main() -> i32 {
    fib(20)
}
```

This and many other programs can be found in the `samples` folder. These are also
the programs used in the tests, so you can verify that they all work and produce 
correct output.

Nom was strongly influenced by Rust and Zig, and to a lesser extent every other
language I know. It intends to offer direct control of memory, as these languages
do, and a rich type system. It is currently a very simple language, but I intend
for it to become more complex over time. The main goal of the project is to let
me learn about implementing a language, so by necessity the language will get more
and more complicated to let me learn more.

Features:
- Imperative programming basics. Variables, arithmetic, conditionals, while loops, and functions
  (even recursive functions). The syntax is much like Rust's, which for this subset
  of features means it should look reasonably familiar those who know any `C` family language.
- Expressional syntax. Blocks for instance are expressions, which run some statements
  before evaluating to a final expression in the block. Function bodies work like
  this as well, so you can omit the final return in the function, like in Rust. If
  statements are really just if expressions, evaluating to whichever block is selected.
  Returns and loops are also expressions, which provides a bit of flexibility.
- Strong typing. Currently no support for type inference (beyond inferring literal
  types), but inference is planned. Built in types include the suite of normal integer
  types (e.g. `i32`, `u8`), as well as `bool`, `unit` (the type with one value, analogous
  to C's `void`) and `bottom` (the type with no value, symbolizing that a function
  never returns, or an expression never finishes evaluating). 
- User defined types, with `struct`. Structs can of course contain other structs.
  Struct expressions let you define an instance of a struct all at once (the only
  way struct values come into existence), but then a struct can be modified via
  the familiar member access notation (`my_struct.my_field`).

Further documentation on the Nom programming language (not documentation for the
project) can be found in `nom_docs.md`. I'll try to avoid letting it get *too* out
of date.

Upcoming Features (probably):
- Pointers are the next major milestone. They will enable comfortably working with
  arrays (without making copies all the time), and then we can do strings and I/O. 
  I eventually want some sort of slice as well.
- Multi-file programs, i.e. a module or import system. Some groundwork has been
  laid, but not fleshed out. With this, writing some basic utilities into a standard
  library could come next (perhaps with some functions being implemented natively
  in Rust?)  
- More user defined types. Tuples / anonymous structs are a huge convenience. Enums 
  / unions that model sum types, completing an algebraic type system when combined
  with structs (product types). Perhaps we can have Zig-like support for optional 
  or nullable values, and likewise for fallible values ("error-union type"). I also
  want to include a generic type system, but that may be implemented much further
  down the line.

This is a fairly high level roadmap. For a much more detailed version, see the
project tab or the issues tab on GitHub.
  
## More about the Project

Documentation for the code of the project is included in the code itself. However,
it can be viewed in HTML form by running `cargo doc`. This only shows the (small) public interface,
but documentation exists for private items as well, which can be viewed by running
`cargo doc --document-private-items`.

Most of the code is contained in the library crate, rooted at `lib.rs`. There is also
a simple binary crate (the single file, `main.rs`) which compiles and runs a program
provided through standard input. In the future, `main.rs` should look more like the frontend
of a compiler, handling flags and options and so on; right now, it is a very thin wrapper
around the library.

The library code is broken up into modules. The first layer of modules is organized primarily
by which stage of the compilation process they implement. This first layer is described
below, but each of these primary modules may be further broken up into submodules.

- `lib.rs` is the top level module, which includes a `CompilationEnvironment` struct
  that tracks all the currently known information about the program; its structure, its
  types, etc. It is important to note that this struct contains different amounts of information
  at different stages of the process, so `env.types` should be understood to reflect
  "currently known types", which means it may be empty early on. This struct also
  determines when different compilation stages take place for various files (so "scope checking"
  one file may trigger imports from another file).
- `token` handles the first step of compilation: tokenization. It defines the `Token`
  struct, which describes a single unit of the program (an identifier, an operator, a brace,
  etc.). This module allows a string to be broken up into these tokens. Tokens also
  track "span" information, which describes where in the source file they came from,
  permitting nicer error messages.
- One stage of compilation is not handled by code in this crate: Parsing. By parsing,
  I mean specifically the process of discovering the structure of the program from
  a string of tokens. This step is handled by a separate, standalone crate I wrote
  called [`Parsley`](https://github.com/JacquelineCasey/Parsley.git#beb70bfb). Parsely
  has evolved in tandem with Nom, but Parsely is a far more general (and far more
  complete) tool. In this stage of compilation, the token string is transformed into
  a `parsley::SyntaxTree<Token>`. This only captures the structure, and almost nothing
  else, of the program. That structure is described in `grammar.parsley`, which
  is given in a format similar to Backus-Naur form (BNF). 
- `ast` handles the process of transforming the `SyntaxTree` into an `AST` or abstract
  syntax tree. This entails validating many local features of the language, and performing
  simple transformations. The intent is to not capture the syntax the programmer typed,
  but the meaning they intended, so that all future stages of analysis proceed more
  easily. Extra information (e.g. the brackets surrounding an if block) are discarded.
  Data is stored in a more easily accessible manner. Finally, AST nodes have some
  additional data which allows them to be identified and associated with information
  discovered later in analysis (e.g. type information, which is computed later).
- `analysis` controls the step where the program (as an `AST`) is repeatedly
  analyzed to determine further information. Further transformations to the code are
  also undertaken. Syntactic sugar is expanded into a standard form (`desugar` submodule). Items used in the programs are checked to ensure that they have been declared, and are in scope (`scope_check` submodule). Finally, the program is type checked (`type_check` submodule), which validates the type
  rules as well as discovering the types for every expression, variable, and function.
- `generate` takes the finalized `AST`, and produces a list of instructions. There
  are some final optimizations made to just this list of instructions (currently
  very simple).
- `instructions` naturally describes the instructions that the virtual machine / 
  Nom runtime understands. A list of instructions fully describes a Nom program,
  so no other information is needed to run it. They can safely be stored somewhere
  to be run by the VM later.
- `runtime` implements the virtual machine. It receives a list of instructions, and
  and runs them. The runtime is based somewhat off of Java's, but like the rest of
  Nom, much of it is developed from first principles. The architecture is stack
  based, so there are no registers beyond an instruction pointer, a stack frame
  base pointer, and a top of stack pointer. Nom's stack lives in Rust's heap, but
  it is actual memory which (in the future) can be manipulated directly by Nom programs,
  and perhaps someday even passed to extenal code (i.e. `C` code).
- The other modules, `error` and `util`, are helper modules. As the language grows
  more complicated, I find I need to add more prettier error reporting facilities in
  `error` in order to successfully write new tests / samples. In time, I hope that
  every possible error would generate a nice user facing message / explanation, but
  right now many error messages are terse and intended for debugging the compiler,
  not the Nom program.

The Nom runtime isn't particularly fast, as one might imagine from a toy language. While I say
there are optimizations, they are currently very simple. That being said, a carefully
written Nom program (written with an understanding of how it compiles) can achieve
performance similar to a translation of that program into Python (even performing
around 10% better), which is a pleasant surprise. Make sure you build the project in
release mode (`cargo build --release`) before doing benchmarks.

I find a test driven approach is very helpful in developing this project. There
are a couple of unit tests for some modules, but the majority of tests are integration
tests for new language features. To extend the language, I recommend writing a test
or two with the new feature, then going through all the modules in order updating
as necessary. Rust's type system is very helpful here - often I need to make just
a few well thought out changes to, for instance, the `AST` type, and after that the
rest of the work is just going through the remaining modules and making sure they
compile. Rust points out all the places that need to consider how the handle the new
`AST` variant, which is very helpful.

When I am done, `cargo test` should pass, with no compilation warnings either. `cargo clippy`
should also emit no warnings (to be fair, I have been known to silence those lints I disagree
with). Finally, `cargo fmt` should be used to fix any formatting issues.

