
# Describes adding pointers to the language, and related concerns.


```
val fooPtr: *Foo = alloc!(Foo {});  
            ~~~~   ~~~~~~
            |      |
            |      ^- Keyword here like new, lifts a value to the stack.
            |
            ^- Ptr parts comes first, easy to read as "Pointer to Foo".
```

Perhaps someday we will either be able to replace the left or right type annotation.

For now, we'll probably proceed by building the inner value on the stack, then copying it over to the heap.
In the future, we may want to be more careful - e.g. incurring a big array copy here might be annoying.
I see why C++ developed constructors (for all their weird quirks) - you can just invoke the constructor on the pointer
to the new memory, and it will create the object no questions asked.

If desired, users could probably follow this pattern themselves though.

```
val fooPtr: *Foo = Foo::init(alloc!())
```

Do we need seperate syntax to allocate an unitialized pointer?

Perhaps we provide `alloc!( TYPE )` to allocate (buf do nothing with) memory for a type. Then we provide the more user
friendly `heap!( VALUE )` which performs construction on the stack, and lifts the value to heap after?

Or maybe the heap version is always overkill, and we should always stick to a type based alloc?

Perhaps we have to pin down what constructors (if we really want them...) will some day look like.

I favor static functions associated with the type. They might need to be tagged to indicate that they are a constructor,
that way we can generate glue to call them correctly in various contexts.

```
struct Foo {
    mInt: i32,

    // ctor implies static? Drop "-> unit" someday.
    // Well, it's kinda static (intended to be called that way, probably should refuse method calls), but it is kinda
    // like a method in that it receives an implicit this.
    pub ctor default(a: i32) -> unit { // Implicit this: *Foo
        this.mInt = a;  // (Probably want .mInt as sugar for .*.mInt, or -> mInt. Zig pulls this off. I think I want 
                       // postfix * or .*, it makes too much sense.)
    }

    // How to indicate methods?
    pub method get() // implicit (this: *Foo)
    pub method get(foo: *mFoo)
    pub fn get(self) // implicit (self: *Foo)

    // I think I'm leaning towards pub method with implicit this vs pub fn for static functions.
    // Methods would still be callable statically, with the "this" argument provided explicitly.
    // That makes them a bit more friendly for functional programming.
}

...

// The point of marking constructors ctor (even though they are really just static functions) is so that they can be
// called differently in different allocation contexts.

// Construct onto heap.

alloc!(Foo::default(5));
// Same as
{
    temp p = {allocate a ptr of size Foo};
    Foo::default(p, 5);
    p
}

// Construct onto stack as temporary.

expr ... (Foo::default(5)) ... expr
// Same as
{
    Advance stack pointer sufficiently to hold a Foo.
    Call Foo::default(p, 5) where p points at that space.
    Retreat stack pointer to top of foo.
    Continue computation of surrounding expressions.
}

// While not immediately critical, we'd probably want to support construction into stack variable memory.

f: Foo = Foo::default(5);
// Same as
{
    Foo::default(address of f, 5); 
}

// This can become interesting. Could we, in principle, work out construction into stack variable memory from other
// expressions?
val f: Foo = if c { Foo::default(5) } else { Foo::default(5) };

// Implementing that would be hard. Perhaps generate_expr gets an optional "construct into" argument?
// Or perhaps all generate_expr and subfunctions track a Location for the expression to be built (but in most cases that
// location is Location::EvaluatedExpression, which might be renamed to "StackTemporary").
```

Finally, if a struct S has all public fields, we might want to synthesize a constructor for it, to make code generation
for alloc! uniform.

## So how to proceed?

We don't even have methods yet, so hold off on that version for now, though I think we want it eventually.
We could start by copying values from the stack to the heap, though we might not want to allow that later - C++ demands
that a constructor is called I believe, and will only allow copying onto the heap if a copy constructor exists. That 
seems reasonable to me.

At some point, allocating uninitialized memory is reasonable - e.g. if an user wants to do something more memory 
manage-y themselves, or maybe to interface with foreign code. Allocating unitialized memory is usually scarier, so I
wouldn't mind if it was a little clunkier to call. It is easier to implement this form of allocation, and it could 
ultimately be treated as a building block for the constructor based allocation idea later.

Proposed syntax:
```
    // Stage one - allocation of unitialized memory.
    val fooPtr: *Foo = alloc_uninit!(Foo);

    // Stage two (more fuzzy) - allocation of objects on the heap (constructed immediately).
    val fooPtr: *Foo = alloc!(Foo::some_ctor());
                 ... = alloc!(Bar { baz: 4, fizz: true });
                 ... = alloc!(5); // And other literals. 
```

Deferring questions of constness. Val and Var no longer look as appealing as modifiers of pointers. Maybe we should do
let and mut, with mut also being the pointer modifier. Zig has `const` and `var`, which looks pretty good. Rust has 
`let` and `let mut`, which addmitedly is appealing if we are going to treat `mut` as the pointer modifier.

Then there's a question - does constness go on the type, or not? Is `const T` a real thing, or are there only `ptr-to-const<T>`?
For variables, is constness an attribute of the variable, or the value. It seems like Rust is able to make it go on the
bindings, though this may complicate const analysis? It also means lots of methods have a _mut counterpart... which might
be fine. And that may have more to do with the borrow checker making mut trickier as well. And C++ has the same thing, it
just uses overloading to hide it. (Maybe fixable, something like `pub method get() -> *const[this] i32` or similar, 
where the const is inheritted from this?.) And how do we const check expressions like `foo.mInt` if we can't piggyback 
off of the type system for this. (I think I slightly favor constness on bindings, but I am still thinking about it...). 
Deferring for now.

alloc! and alloc_uninit! would not be proper functions - I propose that keywords which look like functions and produce
values be given the `!` suffix to emphasize their keyword-ness. This is extra important since we don't have code 
coloring. It also has the benefit that we don't really have to reserve as many keywords - users could use these terms
in other places without the `!` marker. The fact that they are not proper functions is how `alloc_uninit!` can cheat by 
taking a type as an argument.

Type inference could someday simplify these forms. For instance, `val fooPtr: *Foo = alloc_uninit!()` looks fine to me.
Or alternatively, `val fooPtr = alloc_uninit(Foo)`. Honestly, we might be able to support both. Subtyping would probably
break with one of these though (maybe?).

What do we do with pointers once we have them? Well, we can step through to the underlying value, of course. Postfix
dereference makes too much sense. With the prefix form, you occassional write stuff like `*abc.foo` to dereference the
foo part. But that's silly, it looks like you are dereferencing the abc part. We read left to right, let's develop the
language to work primarily in that direction wherever possible. So we'd try `fooPtr*` to dereference `fooPtr`. The main
concern is multiplication, in fact `*=` is particularly nasty here. We could switch to another symbol (pascal supposedly
would do `fooPtr^`, which I understand, though I'd want to use `^` on the pointer type as well.) It's tempting...
Zig's `.*` seems pretty good now though, keeping `*` from the type and from `C`, and emphasizing the "access"-ness of
the operation. So I think we'll go with that. Zig lets `.` be used in place of `.*.` as well, which we'd want (thankfully
avoiding the annoying `.` `->` distinction in C). Though that can be added later.

No discussion of how to get rid of pointers yet, but its relatively uncontroversial. `free!(fooPtr)` is the natural
choice. Now, destructors (which we are not even close to thinking about for real yet) might make this more interesting.
The only other question is what should be done to the `fooPtr`, using it after free is not allowed. Perhaps it should
be set to all zeros (null) here, with the understanding that this is actually safe. However, the null here will be 
observable, which is undesirable. Best to just leave the pointer alone (Zig does that), and call it an error (undefined
behavior) if it is used. Nom will have the ability to validate that a pointer goes to some allocated memory, though it
will be impossible to gaurantee that the memory isn't reused elsewhere. Rust also leaves the pointer alone, it may be
best to defer to that. An alternative is to use another error value, Zig's undefined is `aaaaaaa` in hex, we might steal
that here. We could then look for that value during pointer comparisons (and say such comparisons are undefined, perhaps
panic-ing a debug build)? That seems a bit more appealing, let's at least give it a shot. How this will interact with
the ineivitable nullable pointer type is unclear - if we build it out of Option<>, we probably just defer to whatever 
option says.

On the other hand, working with const pointers down the line (i.e. const address, maybe mutable value) will be supremely
annoying. Best to have free() not mutate its argument. A user who wants this can write this. No need to shuffle the 
undefined behavior around too much.

Pointer equality and inequality are necessary and generally quite handy. Order comparison is more suspect though, I may
leave it out for now. Also leaving out increment and decrement for now, though those are liable to show up in the future.

No casts yet, we don't even have static casts between scalars yet.

Stage 1 Details:
- Adds keyword `alloc_uninit!` and associated expression.
- Adds type `*T` for every `T` (including unit? What to do there. Do we exclude bottom as well?).
  - Some day, we might differentiate `*T` from `*mut T` and an immutable `*T` (or perhaps `*const T`).
- Adds `free!` and associated expression.
  - We won't nullify for otherwise mangle the pointer after freeing it. It is UB to try to use it.
- Adds pointer dereference `.*`.
  - Some day, we'll have syntactic sugar `.` for `.*.`, this precludes adding methods to the pointer itself though.
- Adds `==` and `!=` on pointers.
- TODO: Figure out how to create references from other objects. `&` is the classic choice (Stage 1.5)?

For nullability... well, alloc_uninit!() is definitely a way to get unitialized data now, so we definitely have to accept
the posibility of there not being anything good in there. However, a "good" Nom program will treat the uninit case very
carefully, and the alloc!() case will ensure that all values are actually initialized (perhaps that should be checked).
So we don't actually have to implement `null` yet. Someday, we'll probably use generics and Optional<*T> to represent 
nullability. Where desirable, we can use other tricks to represnet nullability, like an explicit `nil` linked list
element.

After all of this is implemented, we will be able to express linked lists (with a bit of nil node shenanigans). 
It's interesting that we got those before arrays, though arrays are clearly around the corner. The big program that this
demonstrates this behavior should be some sorting algorithm, probably just one of the O(n^2) ones that works well when 
we don't have random access. 
