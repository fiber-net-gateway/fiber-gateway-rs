use crate::types::{JsArray, JsObject, JsValue, JsonError};
use fiber_string::JsString;
use indexmap::IndexMap;
use std::fmt;

pub(crate) struct CharParser<'a> {
    src: &'a [char],
    pos: usize,
}

impl<'a> CharParser<'a> {
    pub fn new(src: &'a [char]) -> Self {
        Self { src, pos: 0 }
    }

    pub fn parse(mut self) -> Result<JsValue, JsonError> {
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
            Some('n') => self.parse_literal("null", JsValue::Null),
            Some('t') => self.parse_literal("true", JsValue::Bool(true)),
            Some('f') => self.parse_literal("false", JsValue::Bool(false)),
            Some('"') => self
                .parse_string()
                .map(|s| JsValue::String(JsString::from(s))),
            Some('[') => self.parse_array(),
            Some('{') => self.parse_object(),
            Some('-') | Some('0'..='9') => self.parse_number(),
            Some(_) => Err(self.error("unexpected character")),
            None => Err(self.error("unexpected end of input")),
        }
    }

    fn parse_literal(&mut self, expected: &str, value: JsValue) -> Result<JsValue, JsonError> {
        let exp: Vec<char> = expected.chars().collect();
        if self.remaining().len() < exp.len() {
            return Err(self.error("invalid literal"));
        }

        if self.remaining()[..exp.len()] == exp {
            self.pos += exp.len();
            Ok(value)
        } else {
            Err(self.error("invalid literal"))
        }
    }

    fn parse_string(&mut self) -> Result<String, JsonError> {
        self.consume('"')?;
        let mut out = String::new();

        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    self.pos += 1;
                    return Ok(out);
                }
                '\\' => {
                    self.pos += 1;
                    out.push(self.parse_escape()?);
                }
                c if (c as u32) <= 0x1F => {
                    return Err(self.error("control characters not allowed in strings"));
                }
                c => {
                    self.pos += 1;
                    out.push(c);
                }
            }
        }

        Err(self.error("unterminated string"))
    }

    fn parse_escape(&mut self) -> Result<char, JsonError> {
        let esc = self.next().ok_or_else(|| self.error("incomplete escape"))?;
        let ch = match esc {
            '"' => '"',
            '\\' => '\\',
            '/' => '/',
            'b' => '\u{0008}',
            'f' => '\u{000C}',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'u' => {
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
                '0'..='9' => (digit as u32) - ('0' as u32),
                'a'..='f' => (digit as u32) - ('a' as u32) + 10,
                'A'..='F' => (digit as u32) - ('A' as u32) + 10,
                _ => return Err(self.error("non-hex digit in unicode escape")),
            };
            code = (code << 4) | value;
        }
        Ok(code)
    }

    fn parse_number(&mut self) -> Result<JsValue, JsonError> {
        let start = self.pos;
        if self.peek() == Some('-') {
            self.pos += 1;
        }

        let mut is_decimal = false;
        match self.peek() {
            Some('0') => {
                self.pos += 1;
                if matches!(self.peek(), Some('0'..='9')) {
                    return Err(self.error("invalid number"));
                }
            }
            Some('1'..='9') => {
                self.pos += 1;
                while matches!(self.peek(), Some('0'..='9')) {
                    self.pos += 1;
                }
            }
            _ => return Err(self.error("invalid number")),
        }

        if self.peek() == Some('.') {
            self.pos += 1;
            is_decimal = true;
            let mut digits = 0;
            while matches!(self.peek(), Some('0'..='9')) {
                self.pos += 1;
                digits += 1;
            }
            if digits == 0 {
                return Err(self.error("missing digits after decimal point"));
            }
        }

        if matches!(self.peek(), Some('e') | Some('E')) {
            self.pos += 1;
            is_decimal = true;
            if matches!(self.peek(), Some('+') | Some('-')) {
                self.pos += 1;
            }
            let mut digits = 0;
            while matches!(self.peek(), Some('0'..='9')) {
                self.pos += 1;
                digits += 1;
            }
            if digits == 0 {
                return Err(self.error("missing exponent digits"));
            }
        }

        let slice = &self.src[start..self.pos];
        let mut s = String::new();
        for c in slice {
            s.push(*c);
        }

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
        self.consume('[')?;
        self.skip_ws();
        let mut items = Vec::new();
        if self.peek() == Some(']') {
            self.pos += 1;
            let array = JsArray::new(items);
            return Ok(JsValue::Array(array));
        }

        loop {
            let value = self.parse_value()?;
            items.push(value);
            self.skip_ws();
            match self.peek() {
                Some(',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some(']') => {
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
        self.consume('{')?;
        self.skip_ws();
        let mut map = IndexMap::new();
        if self.peek() == Some('}') {
            self.pos += 1;
            let object = JsObject::new(map);
            return Ok(JsValue::Object(object));
        }

        loop {
            let key = JsString::from(self.parse_string()?);
            self.skip_ws();
            self.consume(':')?;
            let value = self.parse_value()?;
            map.insert(key, value);
            self.skip_ws();
            match self.peek() {
                Some(',') => {
                    self.pos += 1;
                    self.skip_ws();
                }
                Some('}') => {
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
        while matches!(self.peek(), Some(' ' | '\n' | '\t' | '\r')) {
            self.pos += 1;
        }
    }

    fn consume(&mut self, expected: char) -> Result<(), JsonError> {
        match self.next() {
            Some(ch) if ch == expected => Ok(()),
            _ => Err(self.error("unexpected character")),
        }
    }

    fn remaining(&self) -> &[char] {
        &self.src[self.pos..]
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.src.get(self.pos).copied()?;
        self.pos += 1;
        Some(ch)
    }

    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).copied()
    }

    fn error(&self, message: &str) -> JsonError {
        JsonError {
            offset: self.pos,
            message: message.to_string(),
        }
    }
}

impl fmt::Debug for CharParser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CharParser").finish()
    }
}
