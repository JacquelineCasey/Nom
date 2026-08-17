
# Multiple Item Pointers

Summary
- `* mut T` and `* const T` are single item pointers, and are never null. Note that the `mut` and `const` apply to the
  pointers, not the `T` - constness is not a full part of the type system, a `T` is just a `T` and we only know through
  context whether it is changeable.
- `[] mut T`, `[] const T` is a slice of `T`, meaning any number are there (maybe 0), and the size is carried with it.
  - Still not nullable. The `0` size case is still a slice, just with no elements in it. The pointer will have some
    other value. This is generally just easier to work with, less checks everywhere. A null optional slice won't even
    tell you the size, since there is truly no slice object involved.
- `[*] mut T`, `[*] const T` are multi-item pointers to `T`. There may be `0` (still not null), `1`, or many. Even when
  there are 0, there is still a use as a sentinel.
- `[3; T]` is an array, which is not a pointer at all. The size must be compile time known. We put the size on the right
  for synergy with the other prefix based forms. It also means that `[R; [C; T]]` is accessed like `arr[r][c]` which 
  feels right, it is basically an `R x C` matrix in row-major order. Observe the lack of `const | mut`, since this is
  not a pointer, just a large object.
- `?T` is an optional `T`, where the `T` part might be present or instead it will be `null`. This works well with all
  the pointers / slice types too, so `?* mut T` is a nullable pointer. There's no reason not to allow `?[] const T`, even
  though it's a bit odd. `?[*] mut T` is maybe also a bit odd, but might be somewhat reasonable too in rare circumstances 
  (optional out parameter?).  I might make `?T` just be sugar for some `std::Option<T>` struct, but maybe not.

This is effectively the Zig idea, except we put mut and const in more places (perhaps binding it to the pointer instead
of the object, I'm not sure what Zig does there). The array syntax is a little different too I think. We also forbid
Zig's `N` item pointers, as I think they muddy the waters. Zig says it's the same as a normal pointer to an N sized
array, but I'd prefer making the programmer type that out `* const [4, i32]`.

I like giving options the `.?` syntax. Unlike `.*`, you are not allowed to skip it in the event of having another access
after (so `ptr.*.m` and `ptr.m`, but `opt.?.m` cannot be elide to `opt.m`). I'm neutral about eliding `ptr.*.?` to `ptr.?`,
it might be acceptable, it might not.

We may need to rethink the shortening in the face of multiple pointers... `ptr_ptr.m` currently goes to `ptr_ptr.*.*.m`,
which I suppose is the only way to combine these things, but it is a bit scary. I guess so long as we keep
`ptr_ptr.*` and `ptr_ptr.*.*` different, then we should be fine (we currently do).

We got to think about 0 sized allocations. In general, for simplicity, I think I am fine with avoiding `0` sized types
by saying that empty structs, 0 sized arrays, etc. are 1 byte large. This means that allocating `0` `T`'s still gives
you an allocation, which is nice and uniform. This is actually what `C++` does, at least up until `C++20` which maybe
softened the rules a bit (not clear). (Also with C++, the size can be larger than `1` if that's what the allocator
prefers.) The main benefit of this is that every object has a distinct identity, which is generally a useful property
to have, even if it is a little wasteful on the margins.

The unifying idea behind all this syntax is that prefixes in the type correspond to post-fix operations, which has a
nice feel for human readers going left to right. `* const ?[4, i32]` is a pointer to a const optional array of 4 i32's.
If you have one, you then write `ptr.*.?[2]`, i.e. "dereference the pointer, access the optional, and take the index 2 
entry," giving you an `i32` at the end. Making sure all the type modifiers go on one side also means fewer parentheses
in types. 

As an aside, even though I think I like this optional `?` thing, I'm not sure I care for Zig's `!` for results. It
raises precedence questions, and generally breaks the left-to-right flow. I'd be fine with just Result<> here, even if
I do give it some nice syntactic sugar like `try`. I'm not particularly afraid of syntactic sugar for the "Prelude"
level core language types, even when those types are mostly defined in library-land as structs. But I'm a ways away
from really sorting out the error model, so maybe I'll change my mind.

TODO: Flesh out these ideas. In what order do we implement them. Is it time to hammer out constness?
