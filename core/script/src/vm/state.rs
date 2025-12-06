use fiber_json::JsValue;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmState {
    Running,
    Pending,
    Return,
    Throw,
    EndReturn,
    EndError,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VmError {
    pub message: String,
    pub position: Option<u64>,
}

impl VmError {
    pub fn new(message: impl Into<String>, position: Option<u64>) -> Self {
        Self {
            message: message.into(),
            position,
        }
    }

    pub fn from_js(value: JsValue, position: Option<u64>) -> Self {
        let message = match value {
            JsValue::String(s) => s.to_std_string_lossy(),
            JsValue::Bool(v) => v.to_string(),
            JsValue::Int(v) => v.to_string(),
            JsValue::Float(v) => v.to_string(),
            JsValue::Undefined => "undefined".to_string(),
            JsValue::Null => "null".to_string(),
            JsValue::Exception(exc) => exc.message,
            other => format!("runtime error: {other:?}"),
        };
        Self::new(message, position)
    }
}

#[derive(Debug)]
pub enum PendingOp {
    AsyncConst,
    AsyncFunc { spread: bool },
}
