
Right now, the "documentation" for the language is really justs a list of features.
I'll definitely want to organize documentation better later on.

Features:
- 1 whole type: i32
- Arithemetic Operators with the usual precedence.
- Grouping with parentheses
- Blocks are expressions, they evaluate to the expression written after the
  final statement. No semicolon. It's like Rust.
- Function declaration syntax, but only main() runs right now.
- Assignment. Variable declaration, but only in functions.
  - Declare variables with the `var` or `val` keyword. Currently they are the same, but
    I intend for `var` to mean 'mutable' and `val` to mean 'constant'.