use crate::serde_adapter::serde_error_to_json_error;
#[cfg(test)]
use crate::types::{JsArray, JsObject};
use crate::types::{JsValue, JsonError};
use fiber_string::JsString;
#[cfg(test)]
use indexmap::IndexMap;
use serde::Deserialize;

/// Parse a JSON string into a `JsValue` tree.
pub fn parse(input: &str) -> Result<JsValue, JsonError> {
    let mut deserializer = serde_json::Deserializer::from_str(input);
    let value = JsValue::deserialize(&mut deserializer)
        .map_err(|err| serde_error_to_json_error(err, input))?;
    deserializer
        .end()
        .map_err(|err| serde_error_to_json_error(err, input))?;
    Ok(value)
}

/// Parse JSON from UTF-8 bytes.
pub fn parse_bytes(bytes: &[u8]) -> Result<JsValue, JsonError> {
    let s = std::str::from_utf8(bytes).map_err(|_| JsonError {
        offset: 0,
        message: "input is not valid UTF-8".to_string(),
    })?;
    parse(s)
}

/// Parse JSON from a slice of characters.
pub fn parse_chars(chars: &[char]) -> Result<JsValue, JsonError> {
    let mut buffer = String::with_capacity(chars.len());
    for ch in chars {
        buffer.push(*ch);
    }
    parse(&buffer)
}

/// Parse JSON from a `JsString`.
pub fn parse_js_string(input: &JsString) -> Result<JsValue, JsonError> {
    let content: String = input
        .as_str()
        .code_points()
        .filter_map(|cp| cp.as_char())
        .collect();
    parse(&content)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(val: &str) -> JsValue {
        JsValue::String(JsString::from(val))
    }

    fn arr(items: Vec<JsValue>) -> JsValue {
        JsValue::Array(JsArray::new(items))
    }

    fn obj(map: JsObject) -> JsValue {
        JsValue::Object(map)
    }

    #[test]
    fn parses_primitives() {
        assert_eq!(parse("null").unwrap(), JsValue::Null);
        assert_eq!(parse("true").unwrap(), JsValue::Bool(true));
        assert_eq!(parse("false").unwrap(), JsValue::Bool(false));
        assert_eq!(parse("42").unwrap(), JsValue::Int(42));
        assert_eq!(parse("\"hello\"").unwrap(), s("hello"));
    }

    #[test]
    fn parses_arrays_and_objects() {
        let value = parse(r#"[1, {"a": 2, "b": [true, null]}]"#).unwrap();
        let mut inner = IndexMap::new();
        inner.insert(JsString::from("a"), JsValue::Int(2));
        inner.insert(
            JsString::from("b"),
            arr(vec![JsValue::Bool(true), JsValue::Null]),
        );
        let expected = arr(vec![JsValue::Int(1), obj(JsObject::new(inner))]);
        assert_eq!(value, expected);
    }

    #[test]
    fn parses_strings_with_escapes() {
        let value = parse(r#""line\n\t\"quote\" \u0041""#).unwrap();
        assert_eq!(value, s("line\n\t\"quote\" A"));
    }

    #[test]
    fn parse_js_string_handles_escapes_and_emoji() {
        let json = JsString::from(r#"{"msg":"line\n\t\"q\" \uD83D\uDE03"}"#);
        let parsed = parse_js_string(&json).expect("parse JsString input");
        let expected = s("line\n\t\"q\" 😃");

        let mut map = IndexMap::new();
        map.insert(JsString::from("msg"), expected);
        assert_eq!(parsed, obj(JsObject::new(map)));
    }

    #[test]
    fn rejects_trailing_characters() {
        let err = parse("true false").unwrap_err();
        assert!(err.message.contains("trailing"));
    }

    #[test]
    fn rejects_invalid_number() {
        let err = parse("01").unwrap_err();
        assert!(err.message.contains("invalid number"));
    }

    #[test]
    fn parses_big_int_and_decimal() {
        let big_int = "123456789012345678901234567890";
        let value = parse(big_int).unwrap();
        assert!(matches!(value, JsValue::Float(_)));

        let decimal = "-0.123e10";
        let value = parse(decimal).unwrap();
        assert!(matches!(value, JsValue::Float(_)));
    }

    #[test]
    fn parses_from_chars_slice() {
        let chars: Vec<char> = "[1,2]".chars().collect();
        assert_eq!(
            parse_chars(&chars).unwrap(),
            JsValue::Array(JsArray::new(vec![JsValue::Int(1), JsValue::Int(2)]))
        );
    }
}
