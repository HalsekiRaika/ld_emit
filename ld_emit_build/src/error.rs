use std::fmt;

#[derive(Debug)]
pub enum LDBuildError {
    Network {
        url: String,
        source: Box<dyn std::error::Error>,
    },
    Parse {
        context: String,
        message: String,
        position: Option<usize>,
    },
    CodeGen {
        term: String,
        message: String,
    },
    Io(std::io::Error),
}

impl fmt::Display for LDBuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LDBuildError::Network { url, source } => {
                write!(f, "failed to fetch context from {}: {}", url, source)
            }
            LDBuildError::Parse {
                context,
                message,
                position,
            } => match position {
                Some(pos) => write!(
                    f,
                    "parse error in context '{}' at position {}: {}",
                    context, pos, message
                ),
                None => write!(f, "parse error in context '{}': {}", context, message),
            },
            LDBuildError::CodeGen { term, message } => {
                write!(f, "code generation error for term '{}': {}", term, message)
            }
            LDBuildError::Io(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl std::error::Error for LDBuildError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            LDBuildError::Network { source, .. } => Some(source.as_ref()),
            LDBuildError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for LDBuildError {
    fn from(e: std::io::Error) -> Self {
        LDBuildError::Io(e)
    }
}
