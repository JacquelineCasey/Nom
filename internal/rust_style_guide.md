
# Rust Style Guide

I love bikeshedding, even with myself.

This file describes the Rust style rules that I am developing as I build the project.

Rule 1: `cargo fmt` and `cargo clippy` define most of the rules.

Rule 2: Import Order:
- Do whatever fmt does, but add spaces between blocks of imports with the same top level path.
  - Most specific source first, so `super::...` then `crate::...` then `other_crate::...` then `std::...`.
  - Use the import group braces `{}` at the end of the list if needed. Do not use it at any other level, prefer another
    entry.
  - fmt will take care of alphabetizing.

Rule 3: File Order, from top to bottom:
- Module level `//!` documentation.
- Submodule declarations.
- Using declarations.
- The rest. I still haven't figured out hard rules.
  - Types Precede definitions for sure.
    - Simple trait implementations go with the types, before the interesting functions.
  - Public should Precede private wherever possible.
  - Grouping by idea with `/* ... */` section comments works well for me.
  - All these heuristics are good, but I haven't worked out which ones supercede others yet. 

Rule 4: Project Organiziation.
- I am moving towards strictly distinguishing between root files and leaf files, though the conversion is not yet complete.
  - Root files are `mod.rs` and `lib.rs`, and only contain `mod` declarations and `pub use` declarations. Submodules can
    be `pub`, though I find I've been using that more rarely.
  - Leaf files contain the actual implementation of things, and contain no submodules, except maybe tests. 
    - A module with only a test submodule (even in a different file) doesn't count as a root module.