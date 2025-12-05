use crate::generator::{GeneratorError, JsonGenerator, SerializeJsonGenerator, StringRef};
use crate::types::{JsArray, JsMap, JsObject, JsValue};
use itoa::Buffer as ItoaBuffer;
use ryu_js::Buffer as RyuBuffer;

/// Convert a `JsValue` into its JSON string representation.
///
/// `Undefined` is treated the same as `Null`.
///
/// The output is emitted through `JsonGenerator`, which in turn writes to a
/// `JsonWriteTarget`. Callers can use the default owned `String` target, or
/// supply alternate buffers such as an existing `String`, `JsString`, or a
/// `Vec<char>`.
pub fn stringify(value: &JsValue) -> Result<String, GeneratorError> {
    let mut generator = SerializeJsonGenerator::<String>::new();
    write_value(&mut generator, value)?;
    generator.into_string()
}

pub fn stringify_into(out: &mut String, value: &JsValue) -> Result<(), GeneratorError> {
    let mut generator = SerializeJsonGenerator::<StringRef>::with_buffer(out);
    write_value(&mut generator, value)?;
    generator.finish_in_place().map(|_| ())
}

fn reserve_estimate<G: JsonGenerator>(generator: &mut G, value: &JsValue) {
    let estimate = estimate_value_size(value);
    if estimate > 0 {
        generator.reserve(estimate);
    }
}

fn write_array<G: JsonGenerator>(generator: &mut G, array: &JsArray) -> Result<(), GeneratorError> {
    let items = array.borrow();
    generator.write_array_start()?;
    for item in items.iter() {
        write_value(generator, item)?;
    }
    generator.write_array_end()?;
    Ok(())
}

fn write_object<G: JsonGenerator>(
    generator: &mut G,
    object: &JsObject,
) -> Result<(), GeneratorError> {
    let entries = object.borrow();
    generator.write_object_start()?;
    for (key, value) in entries.iter() {
        generator.write_text(key)?;
        write_value(generator, value)?;
    }
    generator.write_object_end()?;
    Ok(())
}

fn write_value<G: JsonGenerator>(generator: &mut G, value: &JsValue) -> Result<(), GeneratorError> {
    reserve_estimate(generator, value);
    match value {
        JsValue::Undefined | JsValue::Null => generator.write_null()?,
        JsValue::Bool(b) => generator.write_bool(*b)?,
        JsValue::Int(i) => generator.write_int(*i)?,
        JsValue::Float(f) => generator.write_float(*f)?,
        JsValue::Binary(bytes) => generator.write_binary(bytes.as_slice())?,
        JsValue::String(s) => generator.write_text(s)?,
        JsValue::Array(array) => write_array(generator, array)?,
        JsValue::Object(object) => write_object(generator, object)?,
    }

    Ok(())
}

fn estimate_value_size(value: &JsValue) -> usize {
    match value {
        JsValue::Undefined | JsValue::Null => 4,
        JsValue::Bool(true) => 4,
        JsValue::Bool(false) => 5,
        JsValue::Int(i) => {
            let mut buf = ItoaBuffer::new();
            buf.format(*i).len()
        }
        JsValue::Float(f) => {
            if f.is_finite() {
                let mut buf = RyuBuffer::new();
                buf.format_finite(*f).len()
            } else {
                4
            }
        }
        JsValue::Binary(bytes) => 2 + ((bytes.len() + 2) / 3) * 4,
        JsValue::String(s) => s.as_str().len() + 2,
        JsValue::Array(array) => {
            let items = array.borrow();
            estimate_array_size(&items)
        }
        JsValue::Object(object) => {
            let entries = object.borrow();
            estimate_object_size(&entries)
        }
    }
}

fn estimate_array_size(items: &[JsValue]) -> usize {
    if items.is_empty() {
        2
    } else {
        let mut total = 2 + items.len().saturating_sub(1); // brackets + commas
        for item in items.iter() {
            total += estimate_value_size(item);
        }
        total
    }
}

fn estimate_object_size(entries: &JsMap) -> usize {
    if entries.is_empty() {
        2
    } else {
        let mut total = 2 + entries.len().saturating_sub(1); // braces + commas
        for (key, value) in entries.iter() {
            total += key.as_str().len() + 3; // quotes + colon
            total += estimate_value_size(value);
        }
        total
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{JsBinary, JsMap};
    use crate::{parse, parse_bytes, parse_chars, parse_js_string};
    use fiber_string::JsString;

    #[test]
    fn stringify_primitives_and_numbers() {
        assert_eq!(stringify(&JsValue::Null).unwrap(), "null");
        assert_eq!(stringify(&JsValue::Undefined).unwrap(), "null");
        assert_eq!(stringify(&JsValue::Bool(true)).unwrap(), "true");
        assert_eq!(stringify(&JsValue::Int(-7)).unwrap(), "-7");
        assert_eq!(stringify(&JsValue::Float(1.5)).unwrap(), "1.5");
    }

    #[test]
    fn stringify_string_with_escapes() {
        let s = JsValue::String(JsString::from("line\n\t\"quote\"\\"));
        assert_eq!(stringify(&s).unwrap(), "\"line\\n\\t\\\"quote\\\"\\\\\"");
    }

    #[test]
    fn stringify_arrays_and_objects() {
        let array = JsArray::new(vec![
            JsValue::Int(1),
            JsValue::Null,
            JsValue::Undefined,
            JsValue::String(JsString::from("ok")),
        ]);
        assert_eq!(
            stringify(&JsValue::Array(array)).unwrap(),
            "[1,null,null,\"ok\"]"
        );

        let mut map = JsMap::with_capacity_and_hasher(2, Default::default());
        map.insert(JsString::from("a"), JsValue::Bool(false));
        map.insert(JsString::from("b"), JsValue::Int(2));
        let object = JsObject::new(map);
        assert_eq!(
            stringify(&JsValue::Object(object)).unwrap(),
            "{\"a\":false,\"b\":2}"
        );
    }

    #[test]
    fn stringify_binary_as_base64() {
        let bytes = JsBinary::from_slice(b"\x01\x02\x03");
        assert_eq!(stringify(&JsValue::Binary(bytes)).unwrap(), "\"AQID\"");
    }

    #[test]
    fn stringify_and_parse_roundtrip_complex() {
        let source = r#"{"a":[1,2,{"b":"x\n\"y","c":[false,null]}],"num":-0.5,"text":"hi\u00A9"}"#;
        let parsed = parse(source).expect("parse original json");
        let rendered = stringify(&parsed).unwrap();
        let reparsed = parse(&rendered).expect("parse rendered json");
        assert_eq!(reparsed, parsed);
    }

    #[test]
    fn undefined_becomes_null_on_roundtrip() {
        let array = JsArray::new(vec![JsValue::Undefined, JsValue::Int(1)]);
        let rendered = stringify(&JsValue::Array(array.clone())).unwrap();
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

    #[test]
    fn stringify_into_existing_string() {
        let mut existing = String::from("prefix:");
        let value = JsValue::Object(JsObject::new(JsMap::with_capacity_and_hasher(
            0,
            Default::default(),
        )));

        stringify_into(&mut existing, &value).unwrap();

        assert_eq!(existing, "prefix:{}");
    }

    #[test]
    fn stringify_and_parse_cross_validate() {
        let inputs = [
            "true",
            r#"{"k": [1, -2, 0.5, null, false, "t\u1234\b\\"]}"#,
            r#"["multi", {"nested": ["", []]}]"#,
            r#"{"emoji": "\u20AC"}"#,
        ];

        for original in inputs {
            let parsed = parse(original).expect("parse input");
            let rendered = stringify(&parsed).expect("stringify parsed value");
            let reparsed = parse(&rendered).expect("parse rendered");

            assert_eq!(parsed, reparsed, "roundtrip mismatch for {original}");
        }
    }

    #[test]
    fn stringify_parse_handles_escape_heavy_unicode() {
        let escaped = "Escapes: \"quoted\" \\backslash\n\ttab\r\u{0008}\u{000C} ";
        let emoji = "Emoji: 😀😎🤖🚀☕️🎉 — 多语言混合";
        let repeated = format!("{escaped}{emoji}{escaped}{emoji}{escaped}");

        let value = JsValue::String(JsString::from(repeated.as_str()));
        let rendered = stringify(&value).unwrap();
        let reparsed = parse(&rendered).expect("parse rendered string");
        assert_eq!(reparsed, value);

        let wrapped = format!("{{\"payload\":{rendered}}}");
        let reparsed_object = parse(&wrapped).expect("parse wrapped json");

        let mut expected = JsMap::with_capacity_and_hasher(1, Default::default());
        expected.insert(JsString::from("payload"), value);

        assert_eq!(reparsed_object, JsValue::Object(JsObject::new(expected)));
    }
}
