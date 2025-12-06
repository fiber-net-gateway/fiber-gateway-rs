use crate::gc::GcRef;
use fiber_string::JsString;
use indexmap::IndexMap;
use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

/// GC-managed JSON array with interior mutability for edits.
#[derive(Clone)]
pub struct JsArray {
    ptr: GcRef<RefCell<Vec<JsValue>>>,
}

/// GC-managed JSON object with interior mutability for edits.
#[derive(Clone)]
pub struct JsObject {
    ptr: GcRef<RefCell<IndexMap<JsString, JsValue>>>,
}

/// Binary data representation.
#[derive(Clone, PartialEq, Eq)]
pub struct JsBinary {
    inner: Rc<Vec<u8>>,
}

impl JsArray {
    pub fn new(items: Vec<JsValue>) -> Self {
        Self {
            ptr: GcRef::new(RefCell::new(items)),
        }
    }

    pub fn new_empty() -> Self {
        Self::new(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.borrow().is_empty()
    }

    pub fn push(&self, value: JsValue) -> usize {
        let mut items = self.borrow_mut();
        items.push(value);
        items.len()
    }

    pub fn insert(&self, index: usize, value: JsValue) -> bool {
        let mut items = self.borrow_mut();
        if index > items.len() {
            return false;
        }
        items.insert(index, value);
        true
    }

    pub fn get(&self, index: usize) -> Option<JsValue> {
        self.borrow().get(index).cloned()
    }

    pub fn replace(&self, index: usize, value: JsValue) -> Option<JsValue> {
        let mut items = self.borrow_mut();
        items
            .get_mut(index)
            .map(|slot| std::mem::replace(slot, value))
    }

    pub fn remove(&self, index: usize) -> Option<JsValue> {
        let mut items = self.borrow_mut();
        if index < items.len() {
            Some(items.remove(index))
        } else {
            None
        }
    }
}

impl JsObject {
    pub fn new(map: IndexMap<JsString, JsValue>) -> Self {
        Self {
            ptr: GcRef::new(RefCell::new(map)),
        }
    }

    pub fn new_empty() -> Self {
        Self::new(IndexMap::new())
    }

    pub fn len(&self) -> usize {
        self.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.borrow().is_empty()
    }

    pub fn get_by_key(&self, key: &JsString) -> Option<JsValue> {
        self.borrow().get(key).cloned()
    }
    pub fn get(&self, key: impl AsRef<str>) -> Option<JsValue> {
        let key = JsString::from(key.as_ref());
        self.borrow().get(&key).cloned()
    }

    pub fn insert(&self, key: impl Into<JsString>, value: JsValue) -> Option<JsValue> {
        self.borrow_mut().insert(key.into(), value)
    }

    pub fn remove(&self, key: impl AsRef<str>) -> Option<JsValue> {
        let key = JsString::from(key.as_ref());
        self.borrow_mut().shift_remove(&key)
    }

    pub fn contains_key(&self, key: impl AsRef<str>) -> bool {
        let key = JsString::from(key.as_ref());
        self.borrow().contains_key(&key)
    }
}

impl Deref for JsArray {
    type Target = RefCell<Vec<JsValue>>;

    fn deref(&self) -> &Self::Target {
        &*self.ptr
    }
}

impl Deref for JsObject {
    type Target = RefCell<IndexMap<JsString, JsValue>>;

    fn deref(&self) -> &Self::Target {
        &*self.ptr
    }
}

impl fmt::Debug for JsArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.borrow().iter()).finish()
    }
}

impl fmt::Debug for JsObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.borrow().iter()).finish()
    }
}

impl JsBinary {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self {
            inner: Rc::new(bytes),
        }
    }

    pub fn from_slice(bytes: &[u8]) -> Self {
        Self::new(bytes.to_vec())
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.inner
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

impl fmt::Debug for JsBinary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("JsBinary").field(&self.inner).finish()
    }
}

impl PartialEq for JsArray {
    fn eq(&self, other: &Self) -> bool {
        let (left, right) = (self.borrow(), other.borrow());
        *left == *right
    }
}

impl PartialEq for JsObject {
    fn eq(&self, other: &Self) -> bool {
        let (left, right) = (self.borrow(), other.borrow());
        *left == *right
    }
}

/// Representation of a parsed JSON value.
#[derive(Debug, Clone, PartialEq)]
pub enum JsValue {
    Undefined,
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(JsString),
    Binary(JsBinary),
    Array(JsArray),
    Object(JsObject),
    Iterator(JsIterator),
    Exception(JsException),
}

/// Iterator over a snapshot of keys/values used by the scripting VM.
#[derive(Debug, Clone, PartialEq)]
pub struct JsIterator {
    entries: Vec<(Option<JsValue>, JsValue)>,
    pos: usize,
}

impl JsIterator {
    pub fn new(entries: Vec<(Option<JsValue>, JsValue)>) -> Self {
        Self { entries, pos: 0 }
    }

    pub fn from_value(value: JsValue) -> Self {
        match value {
            JsValue::Array(arr) => {
                let entries = arr
                    .borrow()
                    .iter()
                    .enumerate()
                    .map(|(idx, v)| (Some(JsValue::Int(idx as i64)), v.clone()))
                    .collect();
                Self::new(entries)
            }
            JsValue::Object(obj) => {
                let entries = obj
                    .borrow()
                    .iter()
                    .map(|(k, v)| (Some(JsValue::String(k.clone())), v.clone()))
                    .collect();
                Self::new(entries)
            }
            other => Self::new(vec![(None, other)]),
        }
    }

    pub fn next(&mut self) -> bool {
        if self.pos < self.entries.len() {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    pub fn current_key(&self) -> Option<JsValue> {
        self.entries
            .get(self.pos.saturating_sub(1))
            .and_then(|(k, _)| k.clone())
    }

    pub fn current_value(&self) -> Option<JsValue> {
        self.entries
            .get(self.pos.saturating_sub(1))
            .map(|(_, v)| v.clone())
    }
}

/// Exception payload used by the scripting VM.
#[derive(Debug, Clone, PartialEq)]
pub struct JsException {
    pub message: String,
    pub position: Option<u64>,
}

impl JsException {
    pub fn new(message: impl Into<String>, position: Option<u64>) -> Self {
        Self {
            message: message.into(),
            position,
        }
    }
}

impl JsValue {
    pub fn undefined() -> Self {
        Self::Undefined
    }

    pub fn null() -> Self {
        Self::Null
    }

    pub fn bool(value: bool) -> Self {
        Self::Bool(value)
    }

    pub fn int(value: i64) -> Self {
        Self::Int(value)
    }

    pub fn float(value: f64) -> Self {
        Self::Float(value)
    }

    pub fn string(value: impl Into<JsString>) -> Self {
        Self::String(value.into())
    }

    pub fn binary(bytes: impl Into<Vec<u8>>) -> Self {
        Self::Binary(JsBinary::new(bytes.into()))
    }

    pub fn array(items: Vec<JsValue>) -> Self {
        Self::Array(JsArray::new(items))
    }

    pub fn new_empty_array() -> Self {
        Self::Array(JsArray::new(Vec::new()))
    }

    pub fn object(map: IndexMap<JsString, JsValue>) -> Self {
        Self::Object(JsObject::new(map))
    }

    pub fn new_empty_object() -> Self {
        Self::Object(JsObject::new(IndexMap::new()))
    }

    pub fn iterator(iter: JsIterator) -> Self {
        Self::Iterator(iter)
    }

    pub fn exception(message: impl Into<String>, position: Option<u64>) -> Self {
        Self::Exception(JsException::new(message, position))
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array_crud_operations() {
        let arr = JsArray::new(vec![JsValue::Int(1)]);
        assert_eq!(arr.len(), 1);
        assert!(!arr.is_empty());

        arr.push(JsValue::Bool(true));
        assert_eq!(arr.get(1), Some(JsValue::Bool(true)));

        assert!(arr.insert(1, JsValue::Null));
        assert_eq!(arr.get(1), Some(JsValue::Null));

        assert_eq!(arr.replace(0, JsValue::Int(2)), Some(JsValue::Int(1)));
        assert_eq!(arr.get(0), Some(JsValue::Int(2)));

        assert_eq!(arr.remove(2), Some(JsValue::Bool(true)));
        assert!(arr.remove(5).is_none());
    }

    #[test]
    fn object_crud_operations() {
        let mut initial = IndexMap::new();
        initial.insert(JsString::from("a"), JsValue::Int(1));
        let obj = JsObject::new(initial);

        assert_eq!(obj.len(), 1);
        assert!(obj.contains_key("a"));
        assert_eq!(obj.get("a"), Some(JsValue::Int(1)));

        assert_eq!(obj.insert("b", JsValue::Bool(false)), None);
        assert!(obj.contains_key("b"));

        assert_eq!(obj.insert("b", JsValue::Null), Some(JsValue::Bool(false)));
        assert_eq!(obj.get("b"), Some(JsValue::Null));

        assert_eq!(obj.remove("a"), Some(JsValue::Int(1)));
        assert!(obj.get("a").is_none());
        assert_eq!(obj.len(), 1);
    }

    #[test]
    fn new_empty_helpers() {
        let arr = JsArray::new_empty();
        assert!(arr.is_empty());

        let obj = JsObject::new_empty();
        assert!(obj.is_empty());
    }

    #[test]
    fn binary_helpers() {
        let data = JsBinary::from_slice(&[1, 2, 3]);
        assert_eq!(data.len(), 3);
        assert!(!data.is_empty());
        assert_eq!(data.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn js_value_construction_helpers() {
        assert_eq!(JsValue::undefined(), JsValue::Undefined);
        assert_eq!(JsValue::null(), JsValue::Null);
        assert_eq!(JsValue::bool(true), JsValue::Bool(true));
        assert_eq!(JsValue::int(-7), JsValue::Int(-7));
        assert_eq!(JsValue::float(1.5), JsValue::Float(1.5));
        assert_eq!(JsValue::string("hi"), JsValue::String(JsString::from("hi")));

        let binary = JsValue::binary(vec![1u8, 2, 3]);
        assert_eq!(binary, JsValue::Binary(JsBinary::new(vec![1u8, 2, 3])));

        let array = JsValue::array(vec![JsValue::int(1), JsValue::bool(false)]);
        if let JsValue::Array(arr) = array {
            assert_eq!(arr.len(), 2);
            assert_eq!(arr.get(1), Some(JsValue::Bool(false)));
        } else {
            panic!("expected array JsValue");
        }

        let mut entries = IndexMap::new();
        entries.insert(JsString::from("a"), JsValue::null());
        let object = JsValue::object(entries);
        if let JsValue::Object(obj) = object {
            assert!(obj.contains_key("a"));
            assert_eq!(obj.get("a"), Some(JsValue::Null));
            assert_eq!(obj.len(), 1);
        } else {
            panic!("expected object JsValue");
        }

        if let JsValue::Array(arr) = JsValue::new_empty_array() {
            assert!(arr.is_empty());
        } else {
            panic!("expected empty array JsValue");
        }

        if let JsValue::Object(obj) = JsValue::new_empty_object() {
            assert!(obj.is_empty());
        } else {
            panic!("expected empty object JsValue");
        }
    }
}
