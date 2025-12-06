use std::fmt;

#[derive(Debug, Clone)]
pub struct ScriptError {
    pub message: String,
    pub position: Option<usize>,
}

impl ScriptError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            position: None,
        }
    }

    pub fn with_pos(message: impl Into<String>, position: usize) -> Self {
        Self {
            message: message.into(),
            position: Some(position),
        }
    }
}

impl fmt::Display for ScriptError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(pos) = self.position {
            write!(f, "{} at {}", self.message, pos)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl std::error::Error for ScriptError {}
