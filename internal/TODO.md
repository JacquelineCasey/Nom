
Planning takes place on this project: https://github.com/users/jackcasey067/projects/1/views/1

The shortest of short term items (i.e, tasks necessary to complete the next goal) 
can go here:

== Immediate TODO ==

- Document the language in nom_docs.md
- Document the project in the code itself.
  - Top level done. More to follow.
- Add tests for the current token error annotation.
- Add a test for pointer to pointer?
- The tests should eventually test for leaks as well.
- Add handling for .* in ast. We are in the same rule_node as .foo style expressions, but we should likely return a 
  different AST variant. We'll have to desugar `ptr.foo` style expressions, I suppose *during* typechecking, which does
  have a mutable ast in anticipation of inserting implicit conversions.
- Maybe that build_all.zsh script should be converted in build.rs?
- Starting from in build {expr, statement, type}, go through and use the new SyntaxTree helpers to avoid repetive 
  pattern matching and error handling.


== Someday TODO ==

- Use https://doc.rust-lang.org/std/panic/struct.Location.html to write some alternatives to the various expect() or
  unwrap calls strewn about. They could provide both the benefit of not instantly panicking and still providing source
  location for me.
  - E.g. we could write expect_ice("msg") (internal compile error) which returns an Internal Error (which should .into()
    the other error types) which also tracks the source location.
- Syntax Highlighting (based on tokens, not semantics) might be surprisingly in reach. It would require messing with 
  vscode extensions and textmate grammars, but that's about it - no real code needed.
- Going further would require a Language Server, but let's not get ahead of ourselves. Let's get the language first.