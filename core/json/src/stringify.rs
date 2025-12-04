use crate::types::{JsArray, JsBinary, JsObject, JsValue};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use fiber_string::{CodePoint, JsString};
use itoa::Buffer as ItoaBuffer;
use ryu_js::Buffer as RyuBuffer;
use std::fmt::Write;

/// Convert a `JsValue` into its JSON string representation.
///
/// `Undefined` is treated the same as `Null`.
pub fn stringify(value: &JsValue) -> String {
    let mut out = String::new();
    write_value(value, &mut out);
    out
}

fn write_value(value: &JsValue, out: &mut String) {
    match value {
        JsValue::Undefined | JsValue::Null => out.push_str("null"),
        JsValue::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        JsValue::Int(i) => {
            let mut buf = ItoaBuffer::new();
            out.push_str(buf.format(*i));
        }
        JsValue::Float(f) => {
            if f.is_finite() {
                let mut buf = RyuBuffer::new();
                out.push_str(buf.format_finite(*f));
            } else {
                out.push_str("null");
            }
        }
        JsValue::Binary(bytes) => write_binary(bytes, out),
        JsValue::String(s) => write_string(s, out),
        JsValue::Array(array) => write_array(array, out),
        JsValue::Object(object) => write_object(object, out),
    }
}

fn write_array(array: &JsArray, out: &mut String) {
    let items = array.borrow();
    out.push('[');
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            out.push(',');
        }
        write_value(item, out);
    }
    out.push(']');
}

fn write_object(object: &JsObject, out: &mut String) {
    let entries = object.borrow();
    out.push('{');
    for (idx, (key, value)) in entries.iter().enumerate() {
        if idx > 0 {
            out.push(',');
        }
        write_string(key, out);
        out.push(':');
        write_value(value, out);
    }
    out.push('}');
}

fn write_binary(value: &JsBinary, out: &mut String) {
    out.push('"');
    let encoded = BASE64.encode(value.as_slice());
    out.push_str(&encoded);
    out.push('"');
}

fn write_string(value: &JsString, out: &mut String) {
    out.push('"');
    for cp in value.as_str().code_points() {
        match cp {
            CodePoint::Unicode(c) => match c {
                '"' => out.push_str("\\\""),
                '\\' => out.push_str("\\\\"),
                '\u{08}' => out.push_str("\\b"),
                '\u{0C}' => out.push_str("\\f"),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                c if c <= '\u{1F}' => {
                    let _ = write!(out, "\\u{:04X}", c as u32);
                }
                c => out.push(c),
            },
            CodePoint::UnpairedSurrogate(surr) => {
                let _ = write!(out, "\\u{:04X}", surr);
            }
        }
    }
    out.push('"');
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse, parse_bytes, parse_chars, parse_js_string};
    use fiber_string::JsString;
    use indexmap::IndexMap;

    #[test]
    fn stringify_primitives_and_numbers() {
        assert_eq!(stringify(&JsValue::Null), "null");
        assert_eq!(stringify(&JsValue::Undefined), "null");
        assert_eq!(stringify(&JsValue::Bool(true)), "true");
        assert_eq!(stringify(&JsValue::Int(-7)), "-7");
        assert_eq!(stringify(&JsValue::Float(1.5)), "1.5");
    }

    #[test]
    fn stringify_string_with_escapes() {
        let s = JsValue::String(JsString::from("line\n\t\"quote\"\\"));
        assert_eq!(stringify(&s), "\"line\\n\\t\\\"quote\\\"\\\\\"");
    }

    #[test]
    fn stringify_arrays_and_objects() {
        let array = JsArray::new(vec![
            JsValue::Int(1),
            JsValue::Null,
            JsValue::Undefined,
            JsValue::String(JsString::from("ok")),
        ]);
        assert_eq!(stringify(&JsValue::Array(array)), "[1,null,null,\"ok\"]");

        let mut map = IndexMap::new();
        map.insert(JsString::from("a"), JsValue::Bool(false));
        map.insert(JsString::from("b"), JsValue::Int(2));
        let object = JsObject::new(map);
        assert_eq!(stringify(&JsValue::Object(object)), "{\"a\":false,\"b\":2}");
    }

    #[test]
    fn stringify_binary_as_base64() {
        let bytes = JsBinary::from_slice(b"\x01\x02\x03");
        assert_eq!(stringify(&JsValue::Binary(bytes)), "\"AQID\"");
    }

    #[test]
    fn stringify_and_parse_roundtrip_complex() {
        let source = r#"{"a":[1,2,{"b":"x\n\"y","c":[false,null]}],"num":-0.5,"text":"hi\u00A9"}"#;
        let parsed = parse(source).expect("parse original json");
        let rendered = stringify(&parsed);
        let reparsed = parse(&rendered).expect("parse rendered json");
        assert_eq!(reparsed, parsed);
    }

    #[test]
    fn undefined_becomes_null_on_roundtrip() {
        let array = JsArray::new(vec![JsValue::Undefined, JsValue::Int(1)]);
        let rendered = stringify(&JsValue::Array(array.clone()));
        assert_eq!(rendered, "[null,1]");

        let reparsed = parse(&rendered).expect("parse rendered json");
        let expected = JsArray::new(vec![JsValue::Null, JsValue::Int(1)]);
        assert_eq!(reparsed, JsValue::Array(expected));
    }

    #[test]
    fn parses_from_bytes_and_chars_and_jsstring() {
        assert_eq!(parse_bytes(br" true ").unwrap(), JsValue::Bool(true));
        assert_eq!(parse_chars(&['0']).unwrap(), JsValue::Int(0));

        let js = JsString::from("[1]");
        assert_eq!(
            parse_js_string(&js).unwrap(),
            JsValue::Array(JsArray::new(vec![JsValue::Int(1)]))
        );
    }
}
