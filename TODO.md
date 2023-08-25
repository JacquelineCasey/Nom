
== Immediate TODO ==

- Write tests for the runtime component
- Comments
- Unary Negate



== Medium Term TODO ==

- I'm using a lot of panic! and expect! in Runtime, maybe I should switch to Result<>
- Runtime Overflow checks?



== Roadmap ==

Everything in this list is super tentative, and will likely undergo major changes.
The goal is to get something usable with the basic features all the other languages
have. Then I can start being opinionated and experimental with how things develop
and change.

- Build out syntax and AST generating code for basic syntax. Continue extending
  naive interpretter for now, will likely replace interpretter with something more
  interesting later (considering an intermediate language like CIL).
  - Assignment
    - Done, but still need to implement semantics of var and val. Maybe start an
      analysis phase?
  - Functions, function calls
  - Relational Operators (Boolean type)
  - Boolean Operators (I'm a fan of `and` and `or`, but I also maybe prefer `!` to `not`)
  - if, else, while. (Delay `for` loops, they should support ranges)
  - Comments
  - Strings
  - Link against a standard library. Basic functions for IO.
    - Importing from standard library.

- Type system: Simplest approach to start - everything is a value type.
  - Generalize the type system somewhat. Add more builtin types.
  - Permit type conversion (conversion to wider type)
  - Syntax for type coercion (cast to smaller type. Could be checked?)
  - User defined types:
    - Type Alias
    - Product Type
    - Sum Type
  - Arrays (and slices?)
    - Requires a sort of "builtin generic behavior". Maybe just cheat for now, or skip to actual generics?
    - May want an explicit allocator. Maybe hide in standard library.
  - Exotic types:
    - void / unit
    - unreachable / never / diverging
      - I'm fond of adding an explicit 'unreachable' keyword like Zig. Rust has something similar as a macro.
      - However, its unclear if divergence should be a type or a statement or both. Maybe I need to reconsider
        my stance on statements vs expressions.
    - any? I kinda hate `interface{}` or `anytype` or `void*`, but they can be useful in some places.
  - Convenience types:
    - e.g. tuples, slices, optionals and errors. Records? Newtype?
    - References / pointers / borrows / view / lens


== Runtime ==

At some point we've got to make the ergonomics better - I'd like to get Parsley
to emit better error messages, anything beyound "Parse Failed". The simplest approach
could be simply to return the first token that couldn't be parsed. Then we might
update the tokens (here or in Parsley) to include span information - i.e. which line and
characters of the source file has the issue. We could also want to use tokens that
aren't just characters - this could alleviate some issues involving keywords that
look like identifiers (our parser lacks a greedy parsing feature, though that could
be added - tell it to implicitely prefer the leftmost alternative).


The more I think about, the more I realize I'm either gonna have to go full python
(hashtables everywhere, every object is a hashtable or maybe a clever vector of
boxes) or I'm gonna want to use an actual backend. I saw https://cranelift.dev/
as an interesting, Rust friendly option. (LLVM might be better, but Rust support
for LLVM might not be as good I'm not sure).
- This seems helpful: https://github.com/bytecodealliance/cranelift-jit-demo/blob/main/src/jit.rs

I am also starting to realize that cranelift might be really hard, and I can probably
survive just writing an interpretter using some unsafe Rust. I won't have to go
full python - I can use pointers, Rust just makes it tricky because it wants you
to do something else in any normal code.

Still not much closer to picking a style for the Runtime. I am actually becoming
more fond of a stack based virtual machine, like Java's JVM or C#'s. Python kinda
sorta does this too I think? The benefit of this as opposed to a model which just
tries to simulate how x86 uses the stack is that I will rarely have to track the
explicit locations of temporaries, and chaining together expressions stays relatively
simple. Add becomes "run the left expression that leaves a temporary on the stack,
run the right expression that leaves a temporary on the stack, and run add, which
pops those and puts a new temporary on the stack".
- The major problem is alignment. You can only store an f64 at locations divisible 
  by 8. You can deduce the depth of any computation though, so maybe in the instructions
  we generate we will add buffer to account for this (dynamically!).
- Perhaps as we compile, we track a "conceptual stack" where we can easily see the
  location of any value before or after manipulating the stack, allowing us to handle
  alignment gracefully.
- Alternatively, we can declare that all types, even the lowley u8 or bool, takes
  64 bits of space. This might be acceptable during prototyping. 
- Unlike Java, I want to be able to put arbitrarily large things on the stack, such
  as an array of characters.
- Java's approach feels rather intuitive to me here - put locals at a set place
  at the bottom of the frame, and put temporaries on a stack. Operations should
  be able to access locals (or copy them to the stack at least) without knowing
  where in the stack they currently are.
- Probably I will keep the operations simple - no peeking below the top element
  of the stack, for instance.