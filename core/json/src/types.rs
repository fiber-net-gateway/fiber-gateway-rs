use crate::gc::GcRef;
use fiber_string::JsString;
use indexmap::IndexMap;
use rustc_hash::FxHasher;
use std::cell::RefCell;
use std::fmt;
use std::hash::BuildHasherDefault;
use std::ops::Deref;
use std::rc::Rc;

pub type JsMap = IndexMap<JsString, JsValue, BuildHasherDefault<FxHasher>>;

/// GC-managed JSON array with interior mutability for edits.
#[derive(Clone)]
pub struct JsArray {
    ptr: GcRef<RefCell<Vec<JsValue>>>,
}

/// GC-managed JSON object with interior mutability for edits.
#[derive(Clone)]
pub struct JsObject {
    ptr: GcRef<RefCell<JsMap>>,
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
    pub fn new(map: JsMap) -> Self {
        Self {
            ptr: GcRef::new(RefCell::new(map)),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::new(JsMap::with_capacity_and_hasher(
            capacity,
            BuildHasherDefault::<FxHasher>::default(),
        ))
    }

    pub fn new_empty() -> Self {
        Self::with_capacity(0)
    }

    pub fn len(&self) -> usize {
        self.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.borrow().is_empty()
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
    type Target = RefCell<JsMap>;

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
    use crate::types::JsMap;

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
        let mut initial = JsMap::with_capacity_and_hasher(1, Default::default());
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
}
