use fiber_json::{parse, stringify, GeneratorError, JsValue, JsonError};
use std::fmt;

pub type Value = JsValue;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    message: String,
}

impl Error {
    fn new(msg: impl Into<String>) -> Self {
        Self { message: msg.into() }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for Error {}

impl From<JsonError> for Error {
    fn from(err: JsonError) -> Self {
        Error::new(err.message)
    }
}

impl From<GeneratorError> for Error {
    fn from(err: GeneratorError) -> Self {
        Error::new(err.to_string())
    }
}

pub fn from_str(s: &str) -> Result<Value> {
    parse(s).map_err(Error::from)
}

pub fn to_string(value: &Value) -> Result<String> {
    stringify(value).map_err(Error::from)
}
