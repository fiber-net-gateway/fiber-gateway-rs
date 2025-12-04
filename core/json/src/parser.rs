use fiber_string::JsString;
use indexmap::IndexMap;
use std::str;

use crate::parser_chars::CharParser;
use crate::types::{JsArray, JsObject, JsValue, JsonError};

/// Parse a JSON string into a `JsValue` tree.
pub fn parse(input: &str) -> Result<JsValue, JsonError> {
    Parser::new(input).parse()
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
    CharParser::new(chars).parse()
}

/// Parse JSON from a `JsString`.
pub fn parse_js_string(input: &JsString) -> Result<JsValue, JsonError> {
    let chars: Vec<char> = input
        .as_str()
        .code_points()
        .filter_map(|cp| cp.as_char())
        .collect();
    parse_chars(&chars)
}

struct Parser<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            src: input.as_bytes(),
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<JsValue, JsonError> {
        let value = self.parse_value()?;
        self.skip_ws();
        if self.pos != self.src.len() {
            return Err(self.error("trailing characters"));
        }
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<JsValue, JsonError> {
        self.skip_ws();
        match self.peek() {
            Some(b'n') => self.parse_literal(b"null", JsValue::Null),
            Some(b't') => self.parse_literal(b"true", JsValue::Bool(true)),
            Some(b'f') => self.parse_literal(b"false", JsValue::Bool(false)),
            Some(b'"') => self.parse_string().map(|s| JsValue::String(s.into())),
            Some(b'[') => self.parse_array(),
            Some(b'{') => self.parse_object(),
            Some(b'-') | Some(b'0'..=b'9') => self.parse_number(),
            Some(_) => Err(self.error("unexpected character")),
            None => Err(self.error("unexpected end of input")),
        }
    }

    fn parse_literal(&mut self, expected: &[u8], value: JsValue) -> Result<JsValue, JsonError> {
        if self.remaining().starts_with(expected) {
            self.pos += expected.len();
            Ok(value)
        } else {
            Err(self.error("invalid literal"))
        }
    }

    fn parse_string(&mut self) -> Result<String, JsonError> {
        self.consume(b'"')?;
        let mut out = String::new();

        while let Some(ch) = self.peek() {
            match ch {
                b'"' => {
                    self.pos += 1;
                    return Ok(out);
                }
                b'\\' => {
                    self.pos += 1;
                    out.push(self.parse_escape()?);
                }
                0x00..=0x1F => {
                    return Err(self.error("control characters not allowed in strings"));
                }
                b if b < 0x80 => {
                    self.pos += 1;
                    out.push(b as char);
                }
                _ => {
                    // Decode the next UTF-8 scalar from the source.
                    let ch = self
                        .next_char()
                        .ok_or_else(|| self.error("unterminated string"))?;
                    out.push(ch);
                }
            }
        }

        Err(self.error("unterminated string"))
    }

    fn parse_escape(&mut self) -> Result<char, JsonError> {
        let esc = self.next().ok_or_else(|| self.error("incomplete escape"))?;
        let ch = match esc {
            b'"' => '"',
            b'\\' => '\\',
            b'/' => '/',
            b'b' => '\u{0008}',
            b'f' => '\u{000C}',
            b'n' => '\n',
            b'r' => '\r',
            b't' => '\t',
            b'u' => {
                let code = self.parse_hex_code()?;
                char::from_u32(code).ok_or_else(|| self.error("invalid unicode escape"))?
            }
            _ => return Err(self.error("invalid escape character")),
        };
        Ok(ch)
    }

    fn parse_hex_code(&mut self) -> Result<u32, JsonError> {
        let mut code = 0u32;
        for _ in 0..4 {
            let digit = self
                .next()
                .ok_or_else(|| self.error("incomplete unicode escape"))?;
            let value = match digit {
                b'0'..=b'9' => (digit - b'0') as u32,
                b'a'..=b'f' => (digit - b'a' + 10) as u32,
                b'A'..=b'F' => (digit - b'A' + 10) as u32,
                _ => return Err(self.error("non-hex digit in unicode escape")),
            };
            code = (code << 4) | value;
        }
        Ok(code)
    }

    fn parse_number(&mut self) -> Result<JsValue, JsonError> {
        let start = self.pos;
        if self.peek() == Some(b'-') {
            self.pos += 1;
        }

        let mut is_decimal = false;
        match self.peek() {
            Some(b'0') => {
                self.pos += 1;
                if matches!(self.peek(), Some(b'0'..=b'9')) {
                    return Err(self.error("invalid number"));
                }
            }
            Some(b'1'..=b'9') => {
                self.pos += 1;
                while matches!(self.peek(), Some(b'0'..=b'9')) {
                    self.pos += 1;
                }
            }
            _ => return Err(self.error("invalid number")),
        }

        if self.peek() == Some(b'.') {
            self.pos += 1;
            is_decimal = true;
            let mut digits = 0;
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.pos += 1;
                digits += 1;
            }
            if digits == 0 {
                return Err(self.error("missing digits after decimal point"));
            }
        }

        if matches!(self.peek(), Some(b'e') | Some(b'E')) {
            self.pos += 1;
            is_decimal = true;
            if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                self.pos += 1;
            }
            let mut digits = 0;
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.pos += 1;
                digits += 1;
            }
            if digits == 0 {
                return Err(self.error("missing exponent digits"));
            }
        }

        let slice = &self.src[start..self.pos];
        let s = str::from_utf8(slice).expect("parser only consumes valid UTF-8");
        if is_decimal {
            let value: f64 = s.parse().map_err(|_| self.error("invalid number"))?;
            return Ok(JsValue::Float(value));
        }

        match s.parse::<i64>() {
            Ok(int) => Ok(JsValue::Int(int)),
            Err(_) => {
                let value: f64 = s.parse().map_err(|_| self.error("invalid number"))?;
                Ok(JsValue::Float(value))
            }
        }
    }

    fn parse_array(&mut self) -> Result<JsValue, JsonError> {
        self.consume(b'[')?;
        self.skip_ws();
        let mut items = Vec::new();
        if self.peek() == Some(b']') {
            self.pos += 1;
            let array = JsArray::new(items);
            return Ok(JsValue::Array(array));
        }

        loop {
            let value = self.parse_value()?;
            items.push(value);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some(b']') => {
                    self.pos += 1;
                    break;
                }
                _ => return Err(self.error("expected ',' or ']' in array")),
            }
        }

        let array = JsArray::new(items);
        Ok(JsValue::Array(array))
    }

    fn parse_object(&mut self) -> Result<JsValue, JsonError> {
        self.consume(b'{')?;
        self.skip_ws();
        let mut map = IndexMap::new();
        if self.peek() == Some(b'}') {
            self.pos += 1;
            let object = JsObject::new(map);
            return Ok(JsValue::Object(object));
        }

        loop {
            let key = JsString::from(self.parse_string()?);
            self.skip_ws();
            self.consume(b':')?;
            let value = self.parse_value()?;
            map.insert(key, value);
            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some(b'}') => {
                    self.pos += 1;
                    break;
                }
                _ => return Err(self.error("expected ',' or '}' in object")),
            }
        }

        let object = JsObject::new(map);
        Ok(JsValue::Object(object))
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(b' ' | b'\n' | b'\t' | b'\r')) {
            self.pos += 1;
        }
    }

    fn consume(&mut self, expected: u8) -> Result<(), JsonError> {
        match self.next() {
            Some(ch) if ch == expected => Ok(()),
            _ => Err(self.error("unexpected character")),
        }
    }

    fn remaining(&self) -> &[u8] {
        &self.src[self.pos..]
    }

    fn next(&mut self) -> Option<u8> {
        let ch = self.src.get(self.pos).copied()?;
        self.pos += 1;
        Some(ch)
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let slice = self.src.get(self.pos..)?;
        let rest = str::from_utf8(slice).expect("parser only consumes valid UTF-8");
        let ch = rest.chars().next()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn error(&self, message: &str) -> JsonError {
        JsonError {
            offset: self.pos,
            message: message.to_string(),
        }
    }
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
