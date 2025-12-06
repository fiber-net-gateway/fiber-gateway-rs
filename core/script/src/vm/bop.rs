use fiber_json::JsValue;
use fiber_string::JsString;

fn num(val: &JsValue) -> f64 {
    match val {
        JsValue::Int(v) => *v as f64,
        JsValue::Float(v) => *v,
        JsValue::Bool(v) => {
            if *v {
                1.0
            } else {
                0.0
            }
        }
        _ => f64::NAN,
    }
}

fn strict_eq(a: &JsValue, b: &JsValue) -> bool {
    a == b
}

pub fn plus(a: JsValue, b: JsValue) -> JsValue {
    match (a, b) {
        (JsValue::String(sa), JsValue::String(sb)) => JsValue::String(JsString::from(
            sa.to_std_string_lossy() + &sb.to_std_string_lossy(),
        )),
        (JsValue::String(sa), other) => JsValue::String(JsString::from(
            sa.to_std_string_lossy() + &format!("{other:?}"),
        )),
        (other, JsValue::String(sb)) => JsValue::String(JsString::from(
            format!("{other:?}") + &sb.to_std_string_lossy(),
        )),
        (JsValue::Int(a), JsValue::Int(b)) => JsValue::Int(a.saturating_add(b)),
        (left, right) => JsValue::Float(num(&left) + num(&right)),
    }
}

pub fn minus(a: JsValue, b: JsValue) -> JsValue {
    match (a, b) {
        (JsValue::Int(a), JsValue::Int(b)) => JsValue::Int(a.saturating_sub(b)),
        (left, right) => JsValue::Float(num(&left) - num(&right)),
    }
}

pub fn multiply(a: JsValue, b: JsValue) -> JsValue {
    match (a, b) {
        (JsValue::Int(a), JsValue::Int(b)) => JsValue::Int(a.saturating_mul(b)),
        (left, right) => JsValue::Float(num(&left) * num(&right)),
    }
}

pub fn divide(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Float(num(&a) / num(&b))
}

pub fn modulo(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Float(num(&a) % num(&b))
}

pub fn lt(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(num(&a) < num(&b))
}

pub fn lte(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(num(&a) <= num(&b))
}

pub fn gt(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(num(&a) > num(&b))
}

pub fn gte(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(num(&a) >= num(&b))
}

pub fn eq(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(strict_eq(&a, &b))
}

pub fn seq(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(strict_eq(&a, &b))
}

pub fn ne(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(!strict_eq(&a, &b))
}

pub fn sne(a: JsValue, b: JsValue) -> JsValue {
    JsValue::Bool(!strict_eq(&a, &b))
}

pub fn matches(_a: JsValue, _b: JsValue) -> JsValue {
    // Placeholder: regex match not yet implemented.
    JsValue::Bool(false)
}

pub fn contains(lhs: JsValue, rhs: JsValue) -> JsValue {
    let result = match rhs {
        JsValue::Object(obj) => lhs
            .as_string()
            .map(|s| obj.contains_key(s.to_std_string_lossy()))
            .unwrap_or(false),
        JsValue::Array(arr) => {
            let items = arr.borrow();
            items.iter().any(|v| v == &lhs)
        }
        _ => false,
    };
    JsValue::Bool(result)
}

trait AsString {
    fn as_string(&self) -> Option<JsString>;
}

impl AsString for JsValue {
    fn as_string(&self) -> Option<JsString> {
        match self {
            JsValue::String(s) => Some(s.clone()),
            _ => None,
        }
    }
}
