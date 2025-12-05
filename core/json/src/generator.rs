use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use fiber_string::{CodePoint, CommonJsStringBuilder, JsString};
use itoa::Buffer as ItoaBuffer;
use ryu_js::Buffer as RyuBuffer;
use std::fmt;
use std::marker::PhantomData;

use crate::types::{JsArray, JsMap, JsObject, JsValue};

/// Abstract sink that accepts JSON output from a generator implementation.
///
/// Implementors provide an intermediate `Buffer` type (often identical to the
/// final type) that can be mutated efficiently while the generator runs.
/// Implementations must be append-only; no rewrites of previously-written data
/// are expected during generation.
pub trait JsonWriteTarget {
    type Buffer;

    fn new_buffer() -> Self::Buffer;
    fn push_char(out: &mut Self::Buffer, ch: char);
    fn push_str(out: &mut Self::Buffer, s: &str);
    fn reserve(_out: &mut Self::Buffer, _additional: usize) {}
    fn finish(out: Self::Buffer) -> Self;
}

impl JsonWriteTarget for String {
    type Buffer = String;

    fn new_buffer() -> Self::Buffer {
        String::new()
    }

    fn push_char(out: &mut Self::Buffer, ch: char) {
        out.push(ch);
    }

    fn push_str(out: &mut Self::Buffer, s: &str) {
        out.push_str(s);
    }

    fn reserve(out: &mut Self::Buffer, additional: usize) {
        out.reserve(additional);
    }

    fn finish(out: Self::Buffer) -> Self {
        out
    }
}

/// `JsonWriteTarget` wrapper that writes directly into an existing `String`.
///
/// `SerializeJsonGenerator::with_buffer` must be used, as `new_buffer` panics to
/// prevent accidental construction without a backing string.
pub struct StringRef<'a> {
    _marker: PhantomData<&'a mut String>,
}

impl<'a> JsonWriteTarget for StringRef<'a> {
    type Buffer = &'a mut String;

    fn new_buffer() -> Self::Buffer {
        panic!("StringRef requires an existing String buffer")
    }

    fn push_char(out: &mut Self::Buffer, ch: char) {
        out.push(ch);
    }

    fn push_str(out: &mut Self::Buffer, s: &str) {
        out.push_str(s);
    }

    fn reserve(out: &mut Self::Buffer, additional: usize) {
        out.reserve(additional);
    }

    fn finish(out: Self::Buffer) -> Self {
        let _ = out;
        Self {
            _marker: PhantomData::<&'a mut String>,
        }
    }
}

impl JsonWriteTarget for JsString {
    type Buffer = CommonJsStringBuilder<'static>;

    fn new_buffer() -> Self::Buffer {
        CommonJsStringBuilder::new()
    }

    fn push_char(out: &mut Self::Buffer, ch: char) {
        out.push(ch);
    }

    fn push_str(out: &mut Self::Buffer, s: &str) {
        out.push(JsString::from(s));
    }

    fn reserve(out: &mut Self::Buffer, additional: usize) {
        out.reserve(additional);
    }

    fn finish(out: Self::Buffer) -> Self {
        out.build()
    }
}

impl JsonWriteTarget for Vec<char> {
    type Buffer = Vec<char>;

    fn new_buffer() -> Self::Buffer {
        Vec::new()
    }

    fn push_char(out: &mut Self::Buffer, ch: char) {
        out.push(ch);
    }

    fn push_str(out: &mut Self::Buffer, s: &str) {
        out.extend(s.chars());
    }

    fn reserve(out: &mut Self::Buffer, additional: usize) {
        out.reserve(additional);
    }

    fn finish(out: Self::Buffer) -> Self {
        out
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GeneratorError {
    MissingContext {
        expected: &'static str,
    },
    UnexpectedContext {
        expected: &'static str,
        found: &'static str,
    },
    UnexpectedState {
        message: &'static str,
    },
}

impl fmt::Display for GeneratorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GeneratorError::MissingContext { expected } => {
                write!(f, "missing {} context for closing delimiter", expected)
            }
            GeneratorError::UnexpectedContext { expected, found } => {
                write!(
                    f,
                    "cannot close {} while current context is {}",
                    expected, found
                )
            }
            GeneratorError::UnexpectedState { message } => f.write_str(message),
        }
    }
}

impl std::error::Error for GeneratorError {}

/// Common interface for emitting JSON structures either into a text buffer or
/// into an in-memory `JsValue` tree.
pub trait JsonGenerator {
    type Output;

    fn reserve(&mut self, additional: usize);
    fn write_null(&mut self) -> Result<(), GeneratorError>;
    fn write_bool(&mut self, value: bool) -> Result<(), GeneratorError>;
    fn write_int(&mut self, value: i64) -> Result<(), GeneratorError>;
    fn write_float(&mut self, value: f64) -> Result<(), GeneratorError>;
    fn write_binary(&mut self, value: &[u8]) -> Result<(), GeneratorError>;
    fn write_text(&mut self, value: &JsString) -> Result<(), GeneratorError>;
    fn write_str_literal(&mut self, value: &str) -> Result<(), GeneratorError>;
    fn write_object_start(&mut self) -> Result<(), GeneratorError>;
    fn write_object_end(&mut self) -> Result<(), GeneratorError>;
    fn write_array_start(&mut self) -> Result<(), GeneratorError>;
    fn write_array_end(&mut self) -> Result<(), GeneratorError>;

    fn finish(self) -> Result<Self::Output, GeneratorError>
    where
        Self: Sized;
}

enum PreparedState {
    TopLevel,
    Array(usize),
    ObjectKey(usize),
    ObjectValue(usize),
}

enum ContextState {
    Array {
        count: usize,
        parent: PreparedState,
    },
    Object {
        count: usize,
        expecting_key: bool,
        parent: PreparedState,
    },
}

impl ContextState {
    fn kind(&self) -> &'static str {
        match self {
            ContextState::Array { .. } => "array",
            ContextState::Object { .. } => "object",
        }
    }
}

/// Serializer-oriented generator that writes JSON text into a `JsonWriteTarget`.
pub struct SerializeJsonGenerator<T: JsonWriteTarget> {
    out: T::Buffer,
    context: Vec<ContextState>,
    _marker: PhantomData<T>,
}

impl<T: JsonWriteTarget> SerializeJsonGenerator<T> {
    pub fn new() -> Self {
        Self {
            out: T::new_buffer(),
            context: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn with_buffer(out: T::Buffer) -> Self {
        Self {
            out,
            context: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn into_string(self) -> Result<T, GeneratorError> {
        self.finish()
    }

    pub fn finish_in_place(self) -> Result<T, GeneratorError> {
        self.finish()
    }

    fn push_unicode_escape(&mut self, code: u32) {
        const HEX: &[u8; 16] = b"0123456789ABCDEF";
        let digits = [
            HEX[((code >> 12) & 0xF) as usize] as char,
            HEX[((code >> 8) & 0xF) as usize] as char,
            HEX[((code >> 4) & 0xF) as usize] as char,
            HEX[(code & 0xF) as usize] as char,
        ];

        self.push_char('\\');
        self.push_char('u');
        for digit in digits {
            self.push_char(digit);
        }
    }

    #[inline]
    fn push_char(&mut self, ch: char) {
        T::push_char(&mut self.out, ch);
    }

    #[inline]
    fn push_str(&mut self, s: &str) {
        T::push_str(&mut self.out, s);
    }

    fn prepare_value(&mut self) -> PreparedState {
        let len = self.context.len();
        if len == 0 {
            return PreparedState::TopLevel;
        }

        let mut needs_comma = false;
        let mut needs_colon = false;
        let prepared = match &self.context[len - 1] {
            ContextState::Array { count, .. } => {
                needs_comma = *count > 0;
                PreparedState::Array(len - 1)
            }
            ContextState::Object {
                count,
                expecting_key,
                ..
            } => {
                if *expecting_key {
                    needs_comma = *count > 0;
                    PreparedState::ObjectKey(len - 1)
                } else {
                    needs_colon = true;
                    PreparedState::ObjectValue(len - 1)
                }
            }
        };

        if needs_comma {
            self.push_char(',');
        }
        if needs_colon {
            self.push_char(':');
        }

        if let PreparedState::ObjectKey(idx) = prepared {
            if let Some(ContextState::Object { expecting_key, .. }) = self.context.get_mut(idx) {
                *expecting_key = false;
            }
        }

        prepared
    }

    fn finish_value(&mut self, prepared: PreparedState) {
        match prepared {
            PreparedState::TopLevel => {}
            PreparedState::Array(idx) => {
                if let Some(ContextState::Array { count, .. }) = self.context.get_mut(idx) {
                    *count += 1;
                }
            }
            PreparedState::ObjectKey(_) => {}
            PreparedState::ObjectValue(idx) => {
                if let Some(ContextState::Object {
                    count,
                    expecting_key,
                    ..
                }) = self.context.get_mut(idx)
                {
                    *count += 1;
                    *expecting_key = true;
                }
            }
        }
    }
}

impl<T: JsonWriteTarget> JsonGenerator for SerializeJsonGenerator<T> {
    type Output = T;

    fn reserve(&mut self, additional: usize) {
        T::reserve(&mut self.out, additional);
    }

    fn write_null(&mut self) -> Result<(), GeneratorError> {
        let prepared = self.prepare_value();
        self.push_str("null");
        self.finish_value(prepared);
        Ok(())
    }

    fn write_bool(&mut self, value: bool) -> Result<(), GeneratorError> {
        let prepared = self.prepare_value();
        self.push_str(if value { "true" } else { "false" });
        self.finish_value(prepared);
        Ok(())
    }

    fn write_int(&mut self, value: i64) -> Result<(), GeneratorError> {
        let prepared = self.prepare_value();
        let mut buf = ItoaBuffer::new();
        self.push_str(buf.format(value));
        self.finish_value(prepared);
        Ok(())
    }

    fn write_float(&mut self, value: f64) -> Result<(), GeneratorError> {
        let prepared = self.prepare_value();
        if value.is_finite() {
            let mut buf = RyuBuffer::new();
            self.push_str(buf.format_finite(value));
        } else {
            self.push_str("null");
        }
        self.finish_value(prepared);
        Ok(())
    }

    fn write_binary(&mut self, value: &[u8]) -> Result<(), GeneratorError> {
        let prepared = self.prepare_value();
        self.push_char('"');
        let encoded = BASE64.encode(value);
        self.push_str(&encoded);
        self.push_char('"');
        self.finish_value(prepared);
        Ok(())
    }

    fn write_text(&mut self, value: &JsString) -> Result<(), GeneratorError> {
        let prepared = self.prepare_value();
        self.push_char('"');
        for cp in value.as_str().code_points() {
            match cp {
                CodePoint::Unicode(c) => match c {
                    '"' => self.push_str("\\\""),
                    '\\' => self.push_str("\\\\"),
                    '\u{08}' => self.push_str("\\b"),
                    '\u{0C}' => self.push_str("\\f"),
                    '\n' => self.push_str("\\n"),
                    '\r' => self.push_str("\\r"),
                    '\t' => self.push_str("\\t"),
                    c if c <= '\u{1F}' => {
                        self.push_unicode_escape(c as u32);
                    }
                    c => self.push_char(c),
                },
                CodePoint::UnpairedSurrogate(surr) => {
                    self.push_unicode_escape(u32::from(surr));
                }
            }
        }
        self.push_char('"');
        self.finish_value(prepared);
        Ok(())
    }

    fn write_str_literal(&mut self, value: &str) -> Result<(), GeneratorError> {
        let prepared = self.prepare_value();
        self.push_char('"');
        for c in value.chars() {
            match c {
                '"' => self.push_str("\\\""),
                '\\' => self.push_str("\\\\"),
                '\u{08}' => self.push_str("\\b"),
                '\u{0C}' => self.push_str("\\f"),
                '\n' => self.push_str("\\n"),
                '\r' => self.push_str("\\r"),
                '\t' => self.push_str("\\t"),
                c if c <= '\u{1F}' => {
                    self.push_unicode_escape(c as u32);
                }
                c => self.push_char(c),
            }
        }
        self.push_char('"');
        self.finish_value(prepared);
        Ok(())
    }

    fn write_object_start(&mut self) -> Result<(), GeneratorError> {
        let parent = self.prepare_value();
        self.push_char('{');
        self.context.push(ContextState::Object {
            count: 0,
            expecting_key: true,
            parent,
        });
        Ok(())
    }

    fn write_object_end(&mut self) -> Result<(), GeneratorError> {
        let ctx = self
            .context
            .pop()
            .ok_or(GeneratorError::MissingContext { expected: "object" })?;

        if let ContextState::Object { parent, .. } = ctx {
            self.push_char('}');
            self.finish_value(parent);
            Ok(())
        } else {
            Err(GeneratorError::UnexpectedContext {
                expected: "object",
                found: ctx.kind(),
            })
        }
    }

    fn write_array_start(&mut self) -> Result<(), GeneratorError> {
        let parent = self.prepare_value();
        self.push_char('[');
        self.context.push(ContextState::Array { count: 0, parent });
        Ok(())
    }

    fn write_array_end(&mut self) -> Result<(), GeneratorError> {
        let ctx = self
            .context
            .pop()
            .ok_or(GeneratorError::MissingContext { expected: "array" })?;

        if let ContextState::Array { parent, .. } = ctx {
            self.push_char(']');
            self.finish_value(parent);
            Ok(())
        } else {
            Err(GeneratorError::UnexpectedContext {
                expected: "array",
                found: ctx.kind(),
            })
        }
    }

    fn finish(self) -> Result<T, GeneratorError> {
        if let Some(ctx) = self.context.last() {
            return Err(GeneratorError::MissingContext {
                expected: ctx.kind(),
            });
        }

        Ok(T::finish(self.out))
    }
}

enum ValueContext {
    Array(Vec<JsValue>),
    Object {
        map: JsMap,
        pending_key: Option<JsString>,
        expecting_key: bool,
    },
}

impl ValueContext {
    fn kind(&self) -> &'static str {
        match self {
            ValueContext::Array(_) => "array",
            ValueContext::Object { .. } => "object",
        }
    }
}

/// Generator that builds an in-memory `JsValue` tree, useful for serializing
/// Rust structs into the dynamic JSON representation.
pub struct ConvertToJsonValueGenerator {
    root: Option<JsValue>,
    context: Vec<ValueContext>,
}

impl ConvertToJsonValueGenerator {
    pub fn new() -> Self {
        Self {
            root: None,
            context: Vec::new(),
        }
    }

    fn push_value(&mut self, value: JsValue) -> Result<(), GeneratorError> {
        if let Some(ctx) = self.context.last_mut() {
            match ctx {
                ValueContext::Array(items) => {
                    items.push(value);
                    return Ok(());
                }
                ValueContext::Object {
                    map,
                    pending_key,
                    expecting_key,
                } => {
                    if *expecting_key {
                        return Err(GeneratorError::UnexpectedContext {
                            expected: "object key",
                            found: "value",
                        });
                    }

                    let key = pending_key.take().ok_or(GeneratorError::UnexpectedState {
                        message: "missing object key before value",
                    })?;
                    map.insert(key, value);
                    *expecting_key = true;
                    return Ok(());
                }
            }
        }

        if self.root.is_some() {
            return Err(GeneratorError::UnexpectedState {
                message: "multiple top-level values are not allowed",
            });
        }

        self.root = Some(value);
        Ok(())
    }

    fn write_string_value(&mut self, value: JsString) -> Result<(), GeneratorError> {
        if let Some(ValueContext::Object {
            pending_key,
            expecting_key,
            ..
        }) = self.context.last_mut()
        {
            if *expecting_key {
                *pending_key = Some(value);
                *expecting_key = false;
                return Ok(());
            }
        }

        self.push_value(JsValue::String(value))
    }
}

impl JsonGenerator for ConvertToJsonValueGenerator {
    type Output = JsValue;

    fn reserve(&mut self, _additional: usize) {}

    fn write_null(&mut self) -> Result<(), GeneratorError> {
        self.push_value(JsValue::Null)
    }

    fn write_bool(&mut self, value: bool) -> Result<(), GeneratorError> {
        self.push_value(JsValue::Bool(value))
    }

    fn write_int(&mut self, value: i64) -> Result<(), GeneratorError> {
        self.push_value(JsValue::Int(value))
    }

    fn write_float(&mut self, value: f64) -> Result<(), GeneratorError> {
        self.push_value(JsValue::Float(value))
    }

    fn write_binary(&mut self, value: &[u8]) -> Result<(), GeneratorError> {
        self.push_value(JsValue::Binary(crate::types::JsBinary::from_slice(value)))
    }

    fn write_text(&mut self, value: &JsString) -> Result<(), GeneratorError> {
        self.write_string_value(value.clone())
    }

    fn write_str_literal(&mut self, value: &str) -> Result<(), GeneratorError> {
        self.write_string_value(JsString::from(value))
    }

    fn write_object_start(&mut self) -> Result<(), GeneratorError> {
        self.context.push(ValueContext::Object {
            map: JsMap::with_capacity_and_hasher(0, Default::default()),
            pending_key: None,
            expecting_key: true,
        });
        Ok(())
    }

    fn write_object_end(&mut self) -> Result<(), GeneratorError> {
        let ctx = self
            .context
            .pop()
            .ok_or(GeneratorError::MissingContext { expected: "object" })?;

        if let ValueContext::Object {
            map,
            pending_key,
            expecting_key,
        } = ctx
        {
            if !expecting_key && pending_key.is_some() {
                return Err(GeneratorError::UnexpectedState {
                    message: "object ended while waiting for value",
                });
            }

            self.push_value(JsValue::Object(JsObject::new(map)))?
        } else {
            return Err(GeneratorError::UnexpectedContext {
                expected: "object",
                found: ctx.kind(),
            });
        }

        Ok(())
    }

    fn write_array_start(&mut self) -> Result<(), GeneratorError> {
        self.context.push(ValueContext::Array(Vec::new()));
        Ok(())
    }

    fn write_array_end(&mut self) -> Result<(), GeneratorError> {
        let ctx = self
            .context
            .pop()
            .ok_or(GeneratorError::MissingContext { expected: "array" })?;

        if let ValueContext::Array(items) = ctx {
            self.push_value(JsValue::Array(JsArray::new(items)))?
        } else {
            return Err(GeneratorError::UnexpectedContext {
                expected: "array",
                found: ctx.kind(),
            });
        }

        Ok(())
    }

    fn finish(self) -> Result<JsValue, GeneratorError> {
        if let Some(ctx) = self.context.last() {
            return Err(GeneratorError::MissingContext {
                expected: ctx.kind(),
            });
        }

        self.root
            .ok_or(GeneratorError::MissingContext { expected: "value" })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct Person {
        name: String,
        age: i32,
    }

    impl Person {
        fn write_json<G: JsonGenerator>(&self, generator: &mut G) -> Result<(), GeneratorError> {
            generator.write_object_start()?;
            generator.write_str_literal("name")?;
            generator.write_str_literal(&self.name)?;
            generator.write_str_literal("age")?;
            generator.write_int(self.age as i64)?;
            generator.write_object_end()
        }
    }

    #[test]
    fn serializes_struct_into_owned_string() {
        let user = Person {
            name: "Ada".to_string(),
            age: 42,
        };

        let mut generator: SerializeJsonGenerator<String> = SerializeJsonGenerator::new();
        user.write_json(&mut generator).unwrap();
        let json = generator.into_string().unwrap();

        assert_eq!(json, "{\"name\":\"Ada\",\"age\":42}");
    }

    #[test]
    fn serializes_struct_into_existing_buffer() {
        let user = Person {
            name: "Grace".to_string(),
            age: 37,
        };

        let mut backing = String::from("prefix:");
        {
            let mut generator: SerializeJsonGenerator<StringRef<'_>> =
                SerializeJsonGenerator::with_buffer(&mut backing);
            user.write_json(&mut generator).unwrap();
            generator.finish_in_place().unwrap();
        }

        assert_eq!(backing, "prefix:{\"name\":\"Grace\",\"age\":37}");
    }

    #[test]
    fn context_mismatch_returns_error_instead_of_panic() {
        let mut generator: SerializeJsonGenerator<String> = SerializeJsonGenerator::new();
        generator.write_array_start().unwrap();

        let err = generator
            .write_object_end()
            .expect_err("object end inside array should error");

        assert_eq!(
            err,
            GeneratorError::UnexpectedContext {
                expected: "object",
                found: "array",
            }
        );

        let missing = generator
            .write_array_end()
            .and_then(|_| generator.write_array_end())
            .expect_err("closing extra array should error");

        assert_eq!(
            missing,
            GeneratorError::MissingContext { expected: "array" }
        );
    }

    #[test]
    fn builds_jsvalue_tree_from_struct() {
        let user = Person {
            name: "Lin".to_string(),
            age: 29,
        };

        let mut generator = ConvertToJsonValueGenerator::new();
        user.write_json(&mut generator).unwrap();
        let value = generator.finish().unwrap();

        assert_eq!(
            value,
            JsValue::Object(JsObject::new({
                let mut map = JsMap::with_capacity_and_hasher(2, Default::default());
                map.insert(
                    JsString::from("name"),
                    JsValue::String(JsString::from("Lin")),
                );
                map.insert(JsString::from("age"), JsValue::Int(29));
                map
            }))
        );
    }
}
