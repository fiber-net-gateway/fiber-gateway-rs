use crate::types::{JsValue, JsonError};
use fiber_string::{CommonJsStringBuilder, JsString};
use std::io::{self, Write};

/// Convert a `JsValue` into its JSON string representation.
///
/// `Undefined` is treated the same as `Null`.
pub fn stringify(value: &JsValue) -> Result<String, JsonError> {
    serde_json::to_string(value).map_err(|err| JsonError {
        offset: 0,
        message: err.to_string(),
    })
}

/// Convert a `JsValue` into a `JsString`, emitting Latin-1 when possible and
/// upgrading to UTF-16 only when necessary. This is powered by a streaming
/// `io::Write` adapter that consumes UTF-8 output from `serde_json` and
/// incrementally builds the `JsString`.
pub fn stringify_js_string(value: &JsValue) -> Result<JsString, JsonError> {
    let mut writer = JsStringWriter::new();
    serde_json::to_writer(&mut writer, value).map_err(|err| JsonError {
        offset: 0,
        message: err.to_string(),
    })?;
    writer.finish()
}

struct JsStringWriter {
    pending: Vec<u8>,
    builder: CommonJsStringBuilder<'static>,
}

impl JsStringWriter {
    fn new() -> Self {
        Self {
            pending: Vec::new(),
            builder: CommonJsStringBuilder::new(),
        }
    }

    fn process_str(&mut self, s: &str) {
        for ch in s.chars() {
            if ch.is_ascii() {
                self.builder.push(ch as u8);
            } else {
                self.builder.push(ch);
            }
        }
    }

    fn flush_pending(&mut self) -> io::Result<()> {
        if self.pending.is_empty() {
            return Ok(());
        }

        let mut pending = std::mem::take(&mut self.pending);

        match std::str::from_utf8(&pending) {
            Ok(valid) => {
                self.process_str(valid);
                pending.clear();
                self.pending = pending;
                Ok(())
            }
            Err(_) => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "serializer emitted invalid utf-8",
            )),
        }
    }

    fn finish(mut self) -> Result<JsString, JsonError> {
        self.flush_pending().map_err(|err| JsonError {
            offset: 0,
            message: err.to_string(),
        })?;
        Ok(self.builder.build())
    }
}

impl Write for JsStringWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.pending.extend_from_slice(buf);
        let mut pending = std::mem::take(&mut self.pending);

        loop {
            match std::str::from_utf8(&pending) {
                Ok(valid) => {
                    self.process_str(valid);
                    pending.clear();
                    self.pending = pending;
                    return Ok(buf.len());
                }
                Err(err) => {
                    let valid_up_to = err.valid_up_to();
                    if valid_up_to > 0 {
                        let valid = std::str::from_utf8(&pending[..valid_up_to])
                            .expect("valid prefix must be UTF-8");
                        self.process_str(valid);
                        pending.drain(..valid_up_to);
                    }

                    if err.error_len().is_some() {
                        self.pending = pending;
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "serializer emitted invalid utf-8",
                        ));
                    }

                    // Incomplete sequence: wait for more bytes in the next call.
                    self.pending = pending;
                    return Ok(buf.len());
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush_pending()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{JsArray, JsBinary, JsObject};
    use crate::{parse, parse_bytes, parse_chars, parse_js_string};
    use fiber_string::JsString;
    use indexmap::IndexMap;

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

        let mut map = IndexMap::new();
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
    fn stringify_and_parse_roundtrip_with_emoji_and_escapes() {
        let source = JsValue::String(JsString::from("line\n\t\"quote\" 😃"));
        let rendered = stringify(&source).expect("render to json");
        let reparsed = parse(&rendered).expect("parse rendered json");
        assert_eq!(reparsed, source);
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
    fn stringify_js_string_prefers_latin1_and_handles_utf16() {
        let latin_value = JsValue::String(JsString::from("ok"));
        let latin_js = stringify_js_string(&latin_value).unwrap();
        assert!(latin_js.as_str().as_latin1().is_some());
        assert_eq!(latin_js.to_std_string().unwrap(), "\"ok\"");

        let utf16_value = JsValue::String(JsString::from("hi\u{20AC}"));
        let utf16_js = stringify_js_string(&utf16_value).unwrap();
        assert!(utf16_js.as_str().as_latin1().is_none());
        assert_eq!(utf16_js.to_std_string().unwrap(), "\"hi\u{20AC}\"");
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
