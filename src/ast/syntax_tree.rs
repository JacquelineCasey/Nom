//! This module provides a wrapper around Parsely's syntax tree, with convenience functions for querrying and retrieving
//! components.

use std::fmt::Display;

use crate::error::ASTError;
use crate::token::{Keyword, Operator, Punctuation, Span, Token, TokenBody};

/// Common Alias, useful for accessing the enum variants. [`SyntaxTree`] itself is pub, since sometimes spelling it out
/// makes things more readable.
pub(super) use SyntaxTree as ST;

pub(super) type SyntaxTree = parsley::SyntaxTree<Token>;

/// We can't just drop methods into `SyntaxTree`, so we use a trait instead.
pub(super) trait SyntaxTreeExtension {
    /// Retrieves the span of a Token Node. Rule Nodes error instead.
    fn span_of_token(&self) -> Result<&Span, ASTError>;

    /// It is common to want the children of a rule node, and to want to assert its rule, at the start of the
    /// `build_XYZ_ast()` functions. We combine this into one operation.
    fn assert_rule_get_children<'a>(&'a self, expected_name: &str) -> Result<&'a [SyntaxTree], ASTError>;

    /// Tests whether the syntax tree holds a particular component, e.g. a specific keyword, operator, punctuation.
    fn holds<T: SyntaxTreeHoldable>(&self, value: &T) -> bool;

    /// Assertion form of holds, which yields an error if holds is not true.
    fn expect_holds<T: SyntaxTreeHoldable>(&self, value: &T) -> Result<(), ASTError> {
        if self.holds(value) {
            Ok(())
        } else {
            Err(format!("Expected {value}").into())
        }
    }
}

impl SyntaxTreeExtension for SyntaxTree {
    fn span_of_token(&self) -> Result<&Span, ASTError> {
        match self {
            parsley::SyntaxTree::RuleNode { rule_name, .. } => {
                Err(format!("Expected TokenNode, found {rule_name} RuleNode instead").into())
            }
            parsley::SyntaxTree::TokenNode(Token { span, .. }) => Ok(span),
        }
    }

    fn assert_rule_get_children<'a>(&'a self, expected_name: &str) -> Result<&'a [SyntaxTree], ASTError> {
        match self {
            ST::RuleNode { rule_name, subexpressions } if rule_name == expected_name => Ok(subexpressions),
            ST::RuleNode { rule_name, .. } => {
                Err(format!("Expected {expected_name} node, found {rule_name} node").into())
            }
            ST::TokenNode(_) => Err(format!("Expected {expected_name} node, found token node").into()),
        }
    }

    fn holds<T: SyntaxTreeHoldable>(&self, value: &T) -> bool {
        value.is_held_by(self)
    }
}

/// The main kinds of pure enum tokens are holdable.
pub(super) trait SyntaxTreeHoldable: Display {
    fn is_held_by(&self, tree: &SyntaxTree) -> bool;
}

impl SyntaxTreeHoldable for Operator {
    fn is_held_by(&self, tree: &SyntaxTree) -> bool {
        matches!(tree, ST::TokenNode(Token{body: TokenBody::Operator(op), ..}) if op == self)
    }
}

impl SyntaxTreeHoldable for Punctuation {
    fn is_held_by(&self, tree: &SyntaxTree) -> bool {
        matches!(tree, ST::TokenNode(Token{body: TokenBody::Punctuation(punc), ..}) if punc == self)
    }
}

impl SyntaxTreeHoldable for Keyword {
    fn is_held_by(&self, tree: &SyntaxTree) -> bool {
        matches!(tree, ST::TokenNode(Token{body: TokenBody::Keyword(kw), ..}) if kw == self)
    }
}
