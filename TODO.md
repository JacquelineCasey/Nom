
Planning takes place on this project: https://github.com/users/jackcasey067/projects/1/views/1

The shortest of short term items (i.e, tasks necessary to complete the next goal) 
can go here:

== Immediate TODO ==

- Document the language in nom_docs.md
- Document the project in the code itself.
  - Top level done. More to follow.
- Add Span Annotation function somewhere in error code. Errors should print out some code and underline relevant bits.
- Improve the current token error when running the tests. Perhaps now we should get our annotation?
- Allow ! in keywords, possibly by checking for them greedily when we fail to find a Keyword. from_str should return an
  error indicating that ! is needed?
- The tests should eventually test for leaks as well.

== Someday TODO ==

- Syntax Highlighting (based on tokens, not semantics) might be surprisingly in reach. It would require messing with 
  vscode extensions and textmate grammars, but that's about it - no real code needed.
- Going further would require a Language Server, but let's not get ahead of ourselves. Let's get the language first.