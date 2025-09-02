//! Defines data structures related to [`Tokens`](Token)

use crate::error::TokenError;
use crate::FileOrString;

use std::cmp::{max, min};
use std::fmt::Display;
use std::str::FromStr;

/// Represents a single token in a Nom program.
#[derive(Debug, Clone)]
pub struct Token {
    /// Represents the primary information of the token - its type, and any info
    /// related to that type.
    pub body: TokenBody,

    /// Represents the span of the token, i.e. where in the source file it came from.
    pub span: Span,
}

/// Describes the information of the token, without any metadata (spans, etc.)
///
/// Each variant is a type of token, and comes with information specific to that
/// type.
#[derive(Debug, Clone)]
pub enum TokenBody {
    /// An identifier, or name, in the program. Used for variables, functions,
    /// struct members, and so on. The string is naturally the name. Not allowed
    /// to be any keyword.
    Identifier(String),

    /// A string literal in the program. The String is of course the data
    /// of the literal. Escapes are carefully processed during tokenization, so the
    /// string here is the processed version. The quotes that mark the literal are
    /// not included in the data.
    #[allow(unused)]
    StringLiteral(String),

    /// A character literal in the program. The char is of course the character
    /// in the literal. Escapes are processed. The quotes that mark the literal
    /// are not included in the data.
    #[allow(unused)]
    CharLiteral(char),

    /// A numeric literal in the program. The string is the literal, and will be
    /// converted to an actual number later once type information is available.
    NumericLiteral(String),

    /// A keyword in the program. Used in various control flow and other constructs.
    Keyword(Keyword),

    /// An operator in the program. We may someday allow users to define operators,
    /// so we handle them separately from punctuation, although they are somewhat
    /// similar.
    Operator(Operator),

    /// A piece of punctuation in the program. Differs from operators by being much
    /// less complicated.
    Punctuation(Punctuation),
}

/// Represents the possible keywords in a Nom program.
///
/// Using an enum here is more efficient and type safe compared to just using strings.
#[derive(Debug, Clone)]
pub enum Keyword {
    Var,
    Val,
    Fn,
    True,
    False,
    If,
    Else,
    Not,
    And,
    Or,
    While,
    Return,
    Struct,
}

/// Represents an operator in the Nom Programming language.
///
/// Note that there is some overlap in which characters each operator uses (e.g. `+` for `Plus`, but `+=`
/// for `PlusEquals`), so a greedy "longest match" algorithm is used to determine which
/// operators are actually present.
#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    Equals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivideEquals,
    ModulusEquals,
    ThinRightArrow, // This is an operator for lexical reasons. Punctuation has to be single characters.
    DoubleEquals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    Dot,
}

/// A piece of punctuation in a Nom Program. Each is a single character.
#[derive(Debug, Clone)]
pub enum Punctuation {
    Semicolon,
    Comma,
    Colon,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
}

// Make Token compatible with parsley.
impl parsley::Token for Token {
    fn matches(token_type: &str, token: &Self) -> Result<bool, parsley::ParseError> {
        /* This allows the parsley grammar to refer to types of tokens. */

        use Keyword as K;
        use Operator as O;
        use Punctuation as P;
        use Token as T;
        use TokenBody as TB;

        Ok(match Terminal::try_from(token_type)? {
            Terminal::Identifier => matches!(token, T { body: TB::Identifier(_), .. }),

            Terminal::NumericLiteral => matches!(token, T { body: TB::NumericLiteral(_), .. }),

            Terminal::LeftCurlyBrace => matches!(token, T { body: TB::Punctuation(P::LeftCurlyBrace), .. }),
            Terminal::RightCurlyBrace => matches!(token, T { body: TB::Punctuation(P::RightCurlyBrace), .. }),
            Terminal::LeftParenthesis => matches!(token, T { body: TB::Punctuation(P::LeftParenthesis), .. }),
            Terminal::RightParenthesis => matches!(token, T { body: TB::Punctuation(P::RightParenthesis), .. }),
            Terminal::LeftSquareBracket => matches!(token, T { body: TB::Punctuation(P::LeftSquareBracket), .. }),
            Terminal::RightSquareBracket => matches!(token, T { body: TB::Punctuation(P::RightSquareBracket), .. }),

            Terminal::Semicolon => matches!(token, T { body: TB::Punctuation(P::Semicolon), .. }),
            Terminal::Comma => matches!(token, T { body: TB::Punctuation(P::Comma), .. }),
            Terminal::Colon => matches!(token, T { body: TB::Punctuation(P::Colon), .. }),

            Terminal::Plus => matches!(token, T { body: TB::Operator(O::Plus), .. }),
            Terminal::Minus => matches!(token, T { body: TB::Operator(O::Minus), .. }),
            Terminal::Times => matches!(token, T { body: TB::Operator(O::Times), .. }),
            Terminal::Divide => matches!(token, T { body: TB::Operator(O::Divide), .. }),
            Terminal::Modulus => matches!(token, T { body: TB::Operator(O::Modulus), .. }),
            Terminal::Equals => matches!(token, T { body: TB::Operator(O::Equals), .. }),
            Terminal::ThinRightArrow => matches!(token, T { body: TB::Operator(O::ThinRightArrow), .. }),
            Terminal::DoubleEquals => matches!(token, T { body: TB::Operator(O::DoubleEquals), .. }),
            Terminal::NotEquals => matches!(token, T { body: TB::Operator(O::NotEquals), .. }),
            Terminal::LessEquals => matches!(token, T { body: TB::Operator(O::LessEquals), .. }),
            Terminal::GreaterEquals => matches!(token, T { body: TB::Operator(O::GreaterEquals), .. }),
            Terminal::Less => matches!(token, T { body: TB::Operator(O::Less), .. }),
            Terminal::Greater => matches!(token, T { body: TB::Operator(O::Greater), .. }),
            Terminal::PlusEquals => matches!(token, T { body: TB::Operator(O::PlusEquals), .. }),
            Terminal::MinusEquals => matches!(token, T { body: TB::Operator(O::MinusEquals), .. }),
            Terminal::TimesEquals => matches!(token, T { body: TB::Operator(O::TimesEquals), .. }),
            Terminal::DivideEquals => matches!(token, T { body: TB::Operator(O::DivideEquals), .. }),
            Terminal::ModulusEquals => matches!(token, T { body: TB::Operator(O::ModulusEquals), .. }),
            Terminal::Dot => matches!(token, T { body: TB::Operator(O::Dot), .. }),

            Terminal::Var => matches!(token, T { body: TB::Keyword(K::Var), .. }),
            Terminal::Val => matches!(token, T { body: TB::Keyword(K::Val), .. }),
            Terminal::Fn => matches!(token, T { body: TB::Keyword(K::Fn), .. }),
            Terminal::True => matches!(token, T { body: TB::Keyword(K::True), .. }),
            Terminal::False => matches!(token, T { body: TB::Keyword(K::False), .. }),
            Terminal::If => matches!(token, T { body: TB::Keyword(K::If), .. }),
            Terminal::Else => matches!(token, T { body: TB::Keyword(K::Else), .. }),
            Terminal::Not => matches!(token, T { body: TB::Keyword(K::Not), .. }),
            Terminal::And => matches!(token, T { body: TB::Keyword(K::And), .. }),
            Terminal::Or => matches!(token, T { body: TB::Keyword(K::Or), .. }),
            Terminal::While => matches!(token, T { body: TB::Keyword(K::While), .. }),
            Terminal::Return => matches!(token, T { body: TB::Keyword(K::Return), .. }),
            Terminal::Struct => matches!(token, T { body: TB::Keyword(K::Struct), .. }),
        })
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /* TODO: Actually fill this out. Or is this actually needed? */

        f.write_str("{token}")
    }
}

impl FromStr for Keyword {
    type Err = TokenError; // Likely ignored by algorithm.

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Keyword as K;

        Ok(match s {
            "var" => K::Var,
            "val" => K::Val,
            "fn" => K::Fn,
            "true" => K::True,
            "false" => K::False,
            "if" => K::If,
            "else" => K::Else,
            "not" => K::Not,
            "and" => K::And,
            "or" => K::Or,
            "while" => K::While,
            "return" => K::Return,
            "struct" => K::Struct,
            _ => Err("Not a keyword")?,
        })
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Keyword::Var => "var",
            Keyword::Val => "val",
            Keyword::Fn => "fn",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Not => "not",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::While => "while",
            Keyword::Return => "return",
            Keyword::Struct => "struct",
        })
    }
}

impl TryFrom<char> for Punctuation {
    type Error = (); // Likely ignored by algorithm.;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        use Punctuation as P;

        match value {
            ';' => Ok(P::Semicolon),
            ',' => Ok(P::Comma),
            ':' => Ok(P::Colon),
            '{' => Ok(P::LeftCurlyBrace),
            '}' => Ok(P::RightCurlyBrace),
            '(' => Ok(P::LeftParenthesis),
            ')' => Ok(P::RightParenthesis),
            '[' => Ok(P::LeftSquareBracket),
            ']' => Ok(P::RightSquareBracket),
            _ => Err(()),
        }
    }
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Punctuation::Semicolon => ";",
            Punctuation::Comma => ",",
            Punctuation::Colon => ":",
            Punctuation::LeftCurlyBrace => "{",
            Punctuation::RightCurlyBrace => "}",
            Punctuation::LeftParenthesis => "(",
            Punctuation::RightParenthesis => ")",
            Punctuation::LeftSquareBracket => "[",
            Punctuation::RightSquareBracket => "]",
        })
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Times => "*",
            Operator::Divide => "/",
            Operator::Modulus => "%",
            Operator::Equals => "=",
            Operator::PlusEquals => "+=",
            Operator::MinusEquals => "-=",
            Operator::TimesEquals => "*=",
            Operator::DivideEquals => "/=",
            Operator::ModulusEquals => "%=",
            Operator::ThinRightArrow => "->",
            Operator::DoubleEquals => "==",
            Operator::NotEquals => "!=",
            Operator::LessEquals => "<=",
            Operator::GreaterEquals => ">=",
            Operator::Less => "<",
            Operator::Greater => ">",
            Operator::Dot => ".",
        })
    }
}

// Note that Operators don't have a FromStr, since they are often joined together in groups that must be seperateed more
// carefully.

/* Spans */

/// A `Span` descibes a contigous group of characters, in a specific source file (or
/// pseudo source file).
///
/// The span is given as a half open interval, though it may be represented differently
/// in error messages.
#[derive(Debug, Clone)]
pub struct Span {
    /// Program input.
    pub source: FileOrString,
    /// The line the span starts on (1 based).
    pub start_line: usize,
    /// The line the span ends on (1 based).
    pub end_line: usize,
    /// The column the span starts on (1 based).
    pub start_col: usize,
    /// The column the span ends on (1 based).
    pub end_col: usize,
}

impl Span {
    /// Given two spans, returns a new span that includes both.
    ///
    /// This is used heavily in `ast`, where we determine a span for every `ASTNode`
    /// in the program.
    pub fn combine(a: &Span, b: &Span) -> Span {
        assert!(*a.pseudo_path() == *b.pseudo_path());

        Span {
            source: a.source.clone(),
            start_line: min(a.start_line, b.start_line),
            end_line: max(a.end_line, b.end_line),
            start_col: min(a.start_col, a.start_col),
            end_col: max(a.end_col, b.end_col),
        }
    }

    /// Given a nonempty slice of Spans, returns a new Span that includes them all.
    ///
    /// This is used heavily in `ast`, where we determine a span for every `ASTNode`
    /// in the program.
    pub fn combine_all(spans: &[Span]) -> Span {
        assert!(!spans.is_empty());

        let mut final_span = spans[0].clone();

        for span in &spans[1..] {
            final_span = Span::combine(&final_span, span);
        }

        final_span
    }

    /// Returns the path to the held file, or the fake path in the case of string input.
    pub fn pseudo_path(&self) -> &str {
        match &self.source {
            FileOrString::File(path) => path,
            FileOrString::String(fake_path, _) => fake_path,
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}:{}", self.pseudo_path(), self.start_line, self.start_col))
    }
}

/* Terminals */

/// Represents a supported terminal identification. Useful for processing token related errors from
/// parsley. If we fail to convert a string representing a terminal into this type, that is an error
/// on our part (likely an issue in grammar.parsley).
///
/// The order controls the order in which the elements are displayed in some error messages.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum Terminal {
    Identifier,
    NumericLiteral,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    Semicolon,
    Comma,
    Colon,
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    Equals,
    ThinRightArrow,
    DoubleEquals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivideEquals,
    ModulusEquals,
    Dot,
    Var,
    Val,
    Fn,
    True,
    False,
    If,
    Else,
    Not,
    And,
    Or,
    While,
    Return,
    Struct,
}

impl Terminal {
    /// Displays the terminal in a user friendly way. If it represents a single token, then provides that token in single
    /// quotes e.g. "'->'". If it represents a category, then an unquoted description, e.g. "an identifier".
    pub fn pretty_string(self) -> &'static str {
        #[allow(clippy::enum_glob_use)]
        use Terminal::*;

        match self {
            Identifier => "an identifier",
            NumericLiteral => "a numeric literal",
            LeftCurlyBrace => "'{'",
            RightCurlyBrace => "'}'",
            LeftParenthesis => "'('",
            RightParenthesis => "')'",
            LeftSquareBracket => "'['",
            RightSquareBracket => "']'",
            Semicolon => "';'",
            Comma => "','",
            Colon => "':'",
            Plus => "'+'",
            Minus => "'-'",
            Times => "'*'",
            Divide => "'/'",
            Modulus => "'%'",
            Equals => "'='",
            ThinRightArrow => "'->'",
            DoubleEquals => "'=='",
            NotEquals => "'!='",
            LessEquals => "'<='",
            GreaterEquals => "'>='",
            Less => "'<'",
            Greater => "'>'",
            PlusEquals => "'+='",
            MinusEquals => "'-='",
            TimesEquals => "'*='",
            DivideEquals => "'/='",
            ModulusEquals => "'%='",
            Dot => "'.'",
            Var => "'var'",
            Val => "'val'",
            Fn => "'fn'",
            True => "'true'",
            False => "'false'",
            If => "'if'",
            Else => "'else'",
            Not => "'not'",
            And => "'&&'",
            Or => "'||'",
            While => "'while'",
            Return => "'return'",
            Struct => "'struct'",
        }
    }

    /// Indicates whether a terminal is an operator. We use the user understanding of an operator, not the lexical one.
    /// In particular, some things like '.' are internally operators, but are better thought of as punctuation or
    /// something else for user purposes.
    pub fn is_user_operator(self) -> bool {
        #[allow(clippy::enum_glob_use)]
        use Terminal::*;

        match self {
            Plus | Minus | Times | Divide | Modulus | Equals | DoubleEquals | NotEquals | LessEquals
            | GreaterEquals | Less | Greater | PlusEquals | MinusEquals | TimesEquals | DivideEquals
            | ModulusEquals | Not | And | Or => true,
            Identifier | NumericLiteral | LeftCurlyBrace | RightCurlyBrace | LeftParenthesis | RightParenthesis
            | LeftSquareBracket | RightSquareBracket | Semicolon | Comma | Colon | ThinRightArrow | Dot | Var | Val
            | Fn | True | False | If | Else | While | Return | Struct => false,
        }
    }
}

/// Performs conversion from the terminal name to the enum.
impl TryFrom<&str> for Terminal {
    type Error = String;

    fn try_from(terminal_name: &str) -> Result<Self, Self::Error> {
        Ok(match terminal_name {
            "Identifier" => Terminal::Identifier,
            "NumericLiteral" => Terminal::NumericLiteral,
            "LeftCurlyBrace" => Terminal::LeftCurlyBrace,
            "RightCurlyBrace" => Terminal::RightCurlyBrace,
            "LeftParenthesis" => Terminal::LeftParenthesis,
            "RightParenthesis" => Terminal::RightParenthesis,
            "LeftSquareBracket" => Terminal::LeftSquareBracket,
            "RightSquareBracket" => Terminal::RightSquareBracket,
            "Semicolon" => Terminal::Semicolon,
            "Comma" => Terminal::Comma,
            "Colon" => Terminal::Colon,
            "Plus" => Terminal::Plus,
            "Minus" => Terminal::Minus,
            "Times" => Terminal::Times,
            "Divide" => Terminal::Divide,
            "Modulus" => Terminal::Modulus,
            "Equals" => Terminal::Equals,
            "ThinRightArrow" => Terminal::ThinRightArrow,
            "DoubleEquals" => Terminal::DoubleEquals,
            "NotEquals" => Terminal::NotEquals,
            "LessEquals" => Terminal::LessEquals,
            "GreaterEquals" => Terminal::GreaterEquals,
            "Less" => Terminal::Less,
            "Greater" => Terminal::Greater,
            "PlusEquals" => Terminal::PlusEquals,
            "MinusEquals" => Terminal::MinusEquals,
            "TimesEquals" => Terminal::TimesEquals,
            "DivideEquals" => Terminal::DivideEquals,
            "ModulusEquals" => Terminal::ModulusEquals,
            "Dot" => Terminal::Dot,
            "Var" => Terminal::Var,
            "Val" => Terminal::Val,
            "Fn" => Terminal::Fn,
            "True" => Terminal::True,
            "False" => Terminal::False,
            "If" => Terminal::If,
            "Else" => Terminal::Else,
            "Not" => Terminal::Not,
            "And" => Terminal::And,
            "Or" => Terminal::Or,
            "While" => Terminal::While,
            "Return" => Terminal::Return,
            "Struct" => Terminal::Struct,
            _ => Err(format!("Bad token type: \"{terminal_name}\""))?,
        })
    }
}
