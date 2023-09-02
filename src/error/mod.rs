
// We do approximately one error type per module.
// For now, errors are really just strings, but I want to have user facing errors
// at some point.

#[derive(Debug)]
pub struct ASTError (pub String);

impl From<&str> for ASTError {
    fn from(value: &str) -> Self {
        return ASTError(value.to_string())
    }
}

impl From<String> for ASTError {
    fn from(value: String) -> Self {
        return ASTError(value)
    }
}


#[derive(Debug)]
pub struct GenerateError (pub String);

impl From<&str> for GenerateError {
    fn from(value: &str) -> Self {
        return GenerateError(value.to_string())
    }
}

impl From<String> for GenerateError {
    fn from(value: String) -> Self {
        return GenerateError(value)
    }
}


#[derive(Debug)]
pub struct AnalysisError (pub String);

impl From<&str> for AnalysisError {
    fn from(value: &str) -> Self {
        return AnalysisError(value.to_string())
    }
}

impl From<String> for AnalysisError {
    fn from(value: String) -> Self {
        return AnalysisError(value)
    }
}


#[derive(Debug)]
pub struct TokenError (pub String);

impl From<&str> for TokenError {
    fn from(value: &str) -> Self {
        return TokenError(value.to_string())
    }
}

impl From<String> for TokenError {
    fn from(value: String) -> Self {
        return TokenError(value)
    }
}
