

== Roadmap ==

Everything in this list is super tentative, and will likely undergo major changes.
The goal is to get something usable with the basic features all the other languages
have. Then I can start being opinionated and experimental with how things develop
and change.

- Build out syntax and AST generating code for basic syntax. Continue extending
  naive interpretter for now, will likely replace interpretter with something more
  interesting later (considering an intermediate language like CIL).
  - Assignment
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
