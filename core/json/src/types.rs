use fiber_string::JsString;
use indexmap::IndexMap;
use std::fmt;
use std::rc::Rc;

pub type JsArray = Rc<Vec<JsValue>>;
pub type JsObject = Rc<IndexMap<JsString, JsValue>>;

/// Representation of a parsed JSON value.
#[derive(Debug, Clone, PartialEq)]
pub enum JsValue {
    Undefined,
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(JsString),
    Array(JsArray),
    Object(JsObject),
}

/// Error returned when parsing fails.
#[derive(Debug, Clone, PartialEq)]
pub struct JsonError {
    pub offset: usize,
    pub message: String,
}

impl fmt::Display for JsonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at {}: {}", self.offset, self.message)
    }
}

impl std::error::Error for JsonError {}
