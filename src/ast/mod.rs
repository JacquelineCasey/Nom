//! Defines the [`AST`] type, and handles specializing [`SyntaxTrees`](parsley::SyntaxTree)
//! into [`ASTs`](AST)
//!
//! The [`AST`] (abstract syntax tree) type is a highly heterogenous tree that represents
//! the meaning of the parsed program. There are closely related types representing
//! declarations (both top level and within other scopes), expressions, and statements.
//! There is no real ordering of these: statements contain expressions, but block
//! expressions contain statements. A statement may contain a declaration, but
//! a function declaration contains expressions and statements. This is a result
//! of Nom's expressional syntax, which mimics Rust's.
//!
//! Each of these related types are implemented as an enum, where each variant represents
//! a type of expression, a type of statement, etc. This allows each variant to contain
//! heterogenous data, from directly relevant data to data held in children AST elements.
//!
//! The [`AST`] is designed with the intent to simplify further operations on it, and
//! to allow a lot of pattern matching. Many functions at later stages of compilation
//! recurse over the tree and split based on a pattern match.
//!
//! Excess information is discarded. While [`AST`] nodes remember a span over the tokens
//! that generated it, those tokens have been discarded. Punctuation does not appear
//! in the tree, though it's location could likely be inferred. Parentheses and grouping
//! are not shown, but is certainly reflected in the structure of the tree.
//!
//! Along with defining the types, this module also defines numerous helper functions
//! to help later stages operate on the tree cleanly. Notably, there is the [`AnyAST`]
//! type that permits recursing over the children without excessive pattern matching,
//! allowing a function to examine just a few enum variants, and recurse over the
//! rest (good for desugaring, and certain types of optimization).
//!
//! Finally, the module implements the conversion of [`parsley's`](parsley) syntax trees (which
//! come from sequences of [`Tokens`][Token] from [`tokenize()`](super::token::tokenize))
//! into abstract syntax trees. [`parsley`] determines the structure of the code,
//! but this module ensures the structure follows more rules, and gathers information
//! into a more manipulatable and typesafe format. While types of nodes are expressed
//! as strings in syntax trees, in AST's they are expressed as typesafe enum variants.
//!
//! While the main way to generate an [`AST`] is by passing it a syntax tree, it is
//! also permitted to modify the tree at later stages. This is done during the desugaring
//! process, and during optimizations. This is where the fact that AST's represent
//! *abstract* syntax, which makes modifying code during these later stages easier
//! and more natural.
//!
//! The fact that certain syntactic constructs are removed later means that stages
//! that occurs can be permitted to ignore some enum variants, which should have
//! been removed during these steps.

mod ast_types;

mod build_ast;
mod build_ast_helpers;
mod build_declaration_ast;
mod build_expr_ast;
mod build_statement_ast;
mod build_type_ast;

#[cfg(test)]
mod tests;

pub use ast_types::{
    ASTNodeData, AnyAST, DeclarationAST, ExprAST, MathOperation, Mutability, StatementAST, TypeAST, AST,
};
pub use build_ast::build_ast;
