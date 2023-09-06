
== Analysis Stage Design ==
- Note that any design I come up with now will likely be flawed, and thats ok. I'll
  discover the flaws later.

- Name resolution? Maybe partial, if we consider member access (requiring some type
  info) and name resolution to be the same.
  - Do skip for now.
- The step before the first step might be to process type declarations, to determine
  if they are valid and to write down any properties realizeable from their local declaration.
  - We definitely won't have this for a while.
- The first step should be to collect function type info. Each function has a name,
  parameters with specific types, and a return type, all of which can be determined
  from the declaration.
  - Note that this may involve validation of any part of the AST that refers to a type.
  - This sort of check should be reuseable. Does the named type exist? If it is composed
    of parts (generic, or some sort of anonymous sum or product type), does it work? 
- Then we should do a simple crawl of the AST to check scope rules, and maybe build
  some sort of scope object. We need to eventually associate with each function a
  list of locals, so this can either be done inline or as a seperate step after.
  I am fond of the seperate step, but this means that child scopes need to be visible
  from parents (vice versa is a given).
  - This is also a reasonable time to check const-ness (var / val). Again, maybe this
    could be seperate?
  - This is also a reasonable time to resolve name shadowing. For now, we should
    definitely detect this and error (Zig forbids shadowing, but Rust loves shadowing), 
    but if we eventually want to permit this, we might want some form of name mangling, 
    so that later steps can just look up pure names.
- At this point, we might like to make "standard transformations." If we decide to
  implement a switch statement as a series of if statements, then we should do that
  transformation here. Hopefully scope information can be carried forwards harmlessly.
  Ideally though, scope information (beyond function scope) might have been forgotten
  due to name mangling.
  - This step might only make since for certain transformations that are made very
    clear to the end user.
- Type checking + type unification.
  - Type checking refers to A: determininigng the types of all entities in a program
    (really just variables) and B: ensuring that all types make sense. We need to have
    some degree of type inference if we want to associate every expression with a type
    (I think we do want this). However, we might skip *real* type inference in favor of
    forcing the programmer to label every variable, at least early on.
  - Type unification (maybe my term?) will refer to the process of adding conversions into
    the AST in order to make it so that every AST node has perfect types. For example,
    it might be legal to add a u8 and a u16, but before we start generating code we want
    the AST to look like { ADD (convert_to_u16 (some u8)) (some u16)}.
    - This is also relevant for functions. At some point, we will have some form of
      overloading, or something like generics, where this will also decide types from
      a set of options and write down requests for functions to instantiate.
    - I am saying unification specifically to invoke Prolog memories of information
      flowing in multiple directions, though this is really more of a type inference
      thing.
    - Perhaps early on, it could be the user's onus to explicitely cast absolutely
      everything, rendering this step a no-op. However, to make the language useable,
      we will have to provide at least some free conversions (even if its just like
      int promotion).
  - Note: These operations might be inseperable, or at least natural to do together.
- Prune. If we want to emit warnings about dead code, this might be the time. We
  should definitely remove functions that are never called, which of course might
  have been the only thing calling other functions...
  - Note that a pre-prune stage (request based model) might also make sense, as we
    don't want to waste time compiling stuff that never gets used. Perhaps importing
    a module should run enough steps to generate data about what exists in the module,
    but then further analysis is done only when that thing is used. It's possible I
    want different rules for compiling all functions that user writes, though...


Implementation notes - analysis is a big stage, with lots of steps. You can definitely
decompose them into individual (though related) parts, and you should. In fact, each
part might be a submodule, since we can expect the work that each part does to increase
over time. The analysis module itself should provide an API to the outside world,
and everything inside can be hidden. The first step - write out all the modules (some
might reasonably disapeer or fuse later), and move existing functionality there while
keeping the current tests. Write dummy functions that are no-ops.
- Ownership is a pain, make sure you don't tie a changing AST to a struct that is
  changing (or being examined, like in a loop) at the same time. We might package
  the AST with the data at the end of the process.
- Try to break things up so they operate on individual functions as much as possible.
  - Suppose you determine a variable v in a function has type X. This allows you to
    realize that v.some_op() refers to a specific function, which you now have to
    pull into consideration (there should definitely be some type of pruning or
    something, definitely we should not pull function in from the standard library
    if they are never requested; however we should also compile unused functions to
    make a progressive coding style feasible...).

  


== Immediate TODO ==
- Update or eliminate the `get_expr_type` function.
- Builtin types beyond unit and i32
  - Likely we will want 
  - Requires an actual type analysis. I'd like to have one that associates a type
    to every expression using the identity in NodeData.
  - Requires a conversion AST node, ergo requires the ability to modify the AST.
  - We need to generate code for this AST node, which cares mostly about alignment.
  - We might need some support from the VM to handle narrowing conversions gracefully.
    - Or maybe all conversions gracefully. Byte order is a nuissance.

== Medium Term TODO ==

- I'm using a lot of panic! and expect! in Runtime, maybe I should switch to Result<>
- Runtime Overflow checks?
- Scope
- Actual analysis of var and val (const checking).
- Unary Negate



== Long Term TODO ==

- Optimize layout of local variables on stack.
- Inline functions? Or at least some of the built in or autogenerated functions like casting.
  - Does this happen at AST level or at bytecode level.
- Global Variables
- Tokens should include span data for better error messages
- See tower-lsp to write a language server. I miss the pretty colors lol.
- Package into reusable binary or file format. Linking?



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