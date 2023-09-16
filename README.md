
# Nom

Nom is a prototype / toy language that I am writing for fun.

I aim for Nom to be a compiled, statically typed language, with inspiration taken
from Rust (which is the language I am using to implement Nom) and Zig. Nom compiles
down to instructions that run on a virtual machine - the instructions are very
simplified, which makes code generation easy but may cause some major inefficiencies.
The architecture is Stack based, and inspiration was taken from Java's stack based
virtual machine. Nom code is *compiled* the same way Java code is *compiled*, in that
it is transformed into a bytecode which is then subsequently interpretted. Of course,
Java goes further with a JIT that *actually* compiles the code later on, which Nom
does not have.

The benefit to this approach is that I get to write basically everything, which I quite enjoy.
I am writing the parser, the analysis stage, the code generation stage, and the runtime.
While I am of course installing various crates to help out with utility stuff, I am
avoiding those that implement major steps like parsing or code generation. Of course,
this means that Nom's compile times are likely to be much higher than ideal, and 
its runtime efficiency much lower than ideal.

## Features

- Fairly standard basics: Assignment, Math, and Function calls all work the way
  you expect, and have a Rust like syntax. Variable declarations can start with
  `var` for mutable variables and `val` for constant variables.
  - At time of writing, `val` and `var` act identically (creating mutable variables).
  - At time of writing, all variables must be provided a type explicitely. In the
    future, we hope that a (simple) type inference algorithm can make this more 
    ergonomic.
- Expression focused syntax. Most code constructs will be some kind of expression, with
  the primary exception being assignment.
  - Blocks are expressions, which evaluates to the final expression in the block.
    Note that, like in Rust, you must not place a semicolon in order for the final
    line to be considered an expression instead of a statement.
  - Similiarly, functions implicitely return the final expression. At time of writing,
    there is no explicit return statement.
- If statements with Rusty syntax. Else blocks are not here yet, but when complete,
  an if-else block will act like an expression, yielding the final value of whichever
  block was chosen.
- Simple, Rust / Zig like primitive types.
  - Signed and unsigned integer types, from `u8` and `i8` up to `u64` and `i64`.
    (There is currently no plan to add larger primitives, which would require higher
    alignment for every function call.)
  - A boolean type `bool`, which is implemented as a single byte which is either 0 or 1.
    These can be created with numeric comparison operators.
  - A unit type `unit`, which takes a single value. Since the value is known, it is implemented
    as a zero sized type, meaning variables that hold unit effectively vanish in the bytecode.
    - Blocks and functions without a final expression return unit. Constructing a unit value
      can be done by writing the empty block `{}`. This resemble's Zig's unit type (called `void`).
  - There are currently no implicit conversions between primitive types, and there are no plans
    to make numeric conversions free like in C++. Other implicit conversions may be added
    as we get more types. Explicit conversions have not been added yet, but plans have
    been made and the bytecode supports numeric conversions. 

## Successes

We beat python! See the benchmark in `samples/slow/fib_40.nom` - we perform ~14%
better than python. Caveat - the exact algorithm applied is slightly cherry picked; 
Nom has nearly no optimization, so I had to write the algorithm very carefully. Also,
python is approximately the slowest language in common use.

## See also

Parsley - The parser system I wrote and am using for this project: https://github.com/jackcasey067/Parsley.
The needs of Nom are informing the ongoing development of Parsley. 
