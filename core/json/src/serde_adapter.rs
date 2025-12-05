use crate::types::{JsArray, JsObject, JsValue, JsonError};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use fiber_string::JsString;
use indexmap::IndexMap;
use serde::de::{MapAccess, SeqAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};
use std::fmt;
use std::marker::PhantomData;

impl Serialize for JsValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            JsValue::Undefined | JsValue::Null => serializer.serialize_unit(),
            JsValue::Bool(b) => serializer.serialize_bool(*b),
            JsValue::Int(i) => serializer.serialize_i64(*i),
            JsValue::Float(f) => {
                if f.is_finite() {
                    serializer.serialize_f64(*f)
                } else {
                    serializer.serialize_unit()
                }
            }
            JsValue::String(s) => serializer.serialize_str(&js_string_to_std(s)),
            JsValue::Binary(bytes) => serializer.serialize_str(&BASE64.encode(bytes.as_slice())),
            JsValue::Array(array) => {
                let items = array.borrow();
                let mut seq = serializer.serialize_seq(Some(items.len()))?;
                for value in items.iter() {
                    seq.serialize_element(value)?;
                }
                seq.end()
            }
            JsValue::Object(object) => {
                let entries = object.borrow();
                let mut map = serializer.serialize_map(Some(entries.len()))?;
                for (key, value) in entries.iter() {
                    map.serialize_entry(&js_string_to_std(key), value)?;
                }
                map.end()
            }
        }
    }
}

struct JsValueVisitor {
    marker: PhantomData<fn() -> JsValue>,
}

impl JsValueVisitor {
    fn new() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

impl<'de> Visitor<'de> for JsValueVisitor {
    type Value = JsValue;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a JSON value")
    }

    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E> {
        Ok(JsValue::Bool(v))
    }

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E> {
        Ok(JsValue::Int(v))
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E> {
        if v <= i64::MAX as u64 {
            return Ok(JsValue::Int(v as i64));
        }
        Ok(JsValue::Float(v as f64))
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> {
        Ok(JsValue::Float(v))
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> {
        Ok(JsValue::String(JsString::from(v)))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E> {
        Ok(JsValue::String(JsString::from(v)))
    }

    fn visit_none<E>(self) -> Result<Self::Value, E> {
        Ok(JsValue::Null)
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(JsValue::Null)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut items = Vec::new();
        while let Some(value) = seq.next_element::<JsValue>()? {
            items.push(value);
        }
        Ok(JsValue::Array(JsArray::new(items)))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut entries = IndexMap::new();
        while let Some((key, value)) = map.next_entry::<String, JsValue>()? {
            entries.insert(JsString::from(key), value);
        }
        Ok(JsValue::Object(JsObject::new(entries)))
    }
}

impl<'de> serde::Deserialize<'de> for JsValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(JsValueVisitor::new())
    }
}

pub(crate) fn serde_error_to_json_error(err: serde_json::Error, input: &str) -> JsonError {
    let offset = offset_from_line_and_column(err.line(), err.column(), input);
    JsonError {
        offset,
        message: err.to_string(),
    }
}

fn offset_from_line_and_column(line: usize, column: usize, input: &str) -> usize {
    if line == 0 {
        return 0;
    }

    let zero_based_line = line.saturating_sub(1);
    let mut offset = 0usize;

    for (idx, segment) in input.split_inclusive('\n').enumerate() {
        if idx == zero_based_line {
            let col = column.saturating_sub(1);
            let line_len = segment.trim_end_matches('\n').len();
            return offset + col.min(line_len);
        }
        offset += segment.len();
    }

    let col = column.saturating_sub(1);
    offset + col.min(input.len().saturating_sub(offset))
}

fn js_string_to_std(value: &JsString) -> String {
    value
        .to_std_string()
        .unwrap_or_else(|_| value.to_std_string_lossy())
}
