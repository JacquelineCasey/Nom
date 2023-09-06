
// We do approximately one error type per module.
// For now, errors are really just strings, but I want to have user facing errors
// at some point.

#[derive(Debug)]
pub struct ASTError (pub String);

impl From<&str> for ASTError {
    fn from(value: &str) -> Self {
        ASTError(value.to_string())
    }
}

impl From<String> for ASTError {
    fn from(value: String) -> Self {
        ASTError(value)
    }
}


#[derive(Debug)]
pub struct GenerateError (pub String);

impl From<&str> for GenerateError {
    fn from(value: &str) -> Self {
        GenerateError(value.to_string())
    }
}

impl From<String> for GenerateError {
    fn from(value: String) -> Self {
        GenerateError(value)
    }
}


#[derive(Debug)]
pub struct AnalysisError (pub String);

impl From<&str> for AnalysisError {
    fn from(value: &str) -> Self {
        AnalysisError(value.to_string())
    }
}

impl From<String> for AnalysisError {
    fn from(value: String) -> Self {
        AnalysisError(value)
    }
}


#[derive(Debug)]
pub struct TokenError (pub String);

impl From<&str> for TokenError {
    fn from(value: &str) -> Self {
        TokenError(value.to_string())
    }
}

impl From<String> for TokenError {
    fn from(value: String) -> Self {
        TokenError(value)
    }
}

#[derive(Debug)]
pub enum CompileError {
    Direct (String),
    TokenError (TokenError),
    ParseError (parsley::ParseError),
    ASTError (ASTError),
    AnalysisError (AnalysisError),
}

impl From<&str> for CompileError {
    fn from(value: &str) -> Self {
        CompileError::Direct(value.to_string())
    }
}

impl From<String> for CompileError {
    fn from(value: String) -> Self {
        CompileError::Direct(value)
    }
}

impl From<TokenError> for CompileError {
    fn from(value: TokenError) -> Self {
        CompileError::TokenError(value)
    }
}

impl From<parsley::ParseError> for CompileError {
    fn from(value: parsley::ParseError) -> Self {
        CompileError::ParseError(value)
    }
}

impl From<ASTError> for CompileError {
    fn from(value: ASTError) -> Self {
        CompileError::ASTError(value)
    }
}

impl From<AnalysisError> for CompileError {
    fn from(value: AnalysisError) -> Self {
        CompileError::AnalysisError(value)
    }
}
