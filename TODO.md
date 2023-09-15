
Planning takes place on this project: https://github.com/users/jackcasey067/projects/1/views/1

The shortest of short term items (i.e, tasks necessary to complete the next goal) 
can go here:

== Immediate TODO ==
- Jump instructions (actual, possibly multiple) with /relative/ offsets
- If statement grammar
- If statement AST
- If statement type checking (condition must be bool, body must match expected)
- Code generation creating Temp jump instructions
- Specialize jump instructions into actual instructions, removing the JumpFrom's accordingly (tricky, maybe needs a forward and backwards pass?)

- Q: Can if statements ignore the semicolon? What about standard block expression? Will this create grammar ambiguity?