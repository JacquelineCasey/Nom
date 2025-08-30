
## Notes on analysis stage of compilation

Analysis Stage Design
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
