
Proposed optional syntax.


Stealling the null forgiving operators from C#
```
    optFoo.! = ...
    // (Note: optFoo! = ... doesn't really work, looks like !=. Or != looks like it.)

    // Shorter form is available when we further access.
    optFoo!.baz (which is optFoo.!.baz) 
    optFooPtr!.* = ...
    optFooPtr!.baz (which is optFoo.!.*.baz)

    All of which unwrap optional, yielding an l-value, or panicing if it is null.
```

We could do something similar to rust with ? then, returning nullopt from an option returning function if the inspected
optional is nullopt.

Maybe ! isn't needed though, if we can successfully have a .unwrap() return an lvalue or perhaps a pointer.
