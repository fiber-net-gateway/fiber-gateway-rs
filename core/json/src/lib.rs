use std::collections::BTreeMap;
use std::fmt;
use std::str;

/// Representation of a parsed JSON value.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(JsonNumber),
    String(String),
    Array(Vec<JsonValue>),
    Object(BTreeMap<String, JsonValue>),
}

/// Numeric representation with support for large integers and precise decimals.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonNumber {
    Int(i64),
    BigInt(String),
    BigDecimal(String),
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

/// Parse a JSON string into a `JsonValue` tree.
pub fn parse(input: &str) -> Result<JsonValue, JsonError> {
    Parser::new(input).parse()
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

    fn parse(mut self) -> Result<JsonValue, JsonError> {
        let value = self.parse_value()?;
        self.skip_ws();
        if self.pos != self.src.len() {
            return Err(self.error("trailing characters"));
        }
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<JsonValue, JsonError> {
        self.skip_ws();
        match self.peek() {
            Some(b'n') => self.parse_literal(b"null", JsonValue::Null),
            Some(b't') => self.parse_literal(b"true", JsonValue::Bool(true)),
            Some(b'f') => self.parse_literal(b"false", JsonValue::Bool(false)),
            Some(b'"') => self.parse_string().map(JsonValue::String),
            Some(b'[') => self.parse_array(),
            Some(b'{') => self.parse_object(),
            Some(b'-') | Some(b'0'..=b'9') => self.parse_number().map(JsonValue::Number),
            Some(_) => Err(self.error("unexpected character")),
            None => Err(self.error("unexpected end of input")),
        }
    }

    fn parse_literal(&mut self, expected: &[u8], value: JsonValue) -> Result<JsonValue, JsonError> {
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

        while let Some(ch) = self.next() {
            match ch {
                b'"' => return Ok(out),
                b'\\' => out.push(self.parse_escape()?),
                0x00..=0x1F => return Err(self.error("control characters not allowed in strings")),
                _ => out.push(ch as char),
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
            let digit = self.next().ok_or_else(|| self.error("incomplete unicode escape"))?;
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

    fn parse_number(&mut self) -> Result<JsonNumber, JsonError> {
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
            return Ok(JsonNumber::BigDecimal(s.to_string()));
        }

        match s.parse::<i64>() {
            Ok(int) => Ok(JsonNumber::Int(int)),
            Err(_) => Ok(JsonNumber::BigInt(s.to_string())),
        }
    }

    fn parse_array(&mut self) -> Result<JsonValue, JsonError> {
        self.consume(b'[')?;
        self.skip_ws();
        let mut items = Vec::new();
        if self.peek() == Some(b']') {
            self.pos += 1;
            return Ok(JsonValue::Array(items));
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

        Ok(JsonValue::Array(items))
    }

    fn parse_object(&mut self) -> Result<JsonValue, JsonError> {
        self.consume(b'{')?;
        self.skip_ws();
        let mut map = BTreeMap::new();
        if self.peek() == Some(b'}') {
            self.pos += 1;
            return Ok(JsonValue::Object(map));
        }

        loop {
            let key = self.parse_string()?;
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

        Ok(JsonValue::Object(map))
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

    #[test]
    fn parses_primitives() {
        assert_eq!(parse("null").unwrap(), JsonValue::Null);
        assert_eq!(parse("true").unwrap(), JsonValue::Bool(true));
        assert_eq!(parse("false").unwrap(), JsonValue::Bool(false));
        assert_eq!(
            parse("42").unwrap(),
            JsonValue::Number(JsonNumber::Int(42))
        );
        assert_eq!(
            parse("\"hello\"").unwrap(),
            JsonValue::String("hello".to_string())
        );
    }

    #[test]
    fn parses_arrays_and_objects() {
        let value = parse(r#"[1, {"a": 2, "b": [true, null]}]"#).unwrap();
        let mut inner = BTreeMap::new();
        inner.insert("a".into(), JsonValue::Number(JsonNumber::Int(2)));
        inner.insert(
            "b".into(),
            JsonValue::Array(vec![JsonValue::Bool(true), JsonValue::Null]),
        );
        let expected = JsonValue::Array(vec![
            JsonValue::Number(JsonNumber::Int(1)),
            JsonValue::Object(inner),
        ]);
        assert_eq!(value, expected);
    }

    #[test]
    fn parses_strings_with_escapes() {
        let value = parse(r#""line\n\t\"quote\" \u0041""#).unwrap();
        assert_eq!(value, JsonValue::String("line\n\t\"quote\" A".to_string()));
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
        assert_eq!(
            value,
            JsonValue::Number(JsonNumber::BigInt(big_int.to_string()))
        );

        let decimal = "-0.123e10";
        let value = parse(decimal).unwrap();
        assert_eq!(
            value,
            JsonValue::Number(JsonNumber::BigDecimal(decimal.to_string()))
        );
    }
}
