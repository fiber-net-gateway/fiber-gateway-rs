use crate::parse::compile::{ConstRef, FunctionRef, LibraryResolver};
use crate::vm::{VmAsyncConstant, VmAsyncFunction, VmConstant, VmFunction, VmOperand};
use indexmap::IndexMap;
use std::rc::Rc;
use std::sync::Arc;

pub trait Directive: std::fmt::Debug {
    fn find_function(&self, namespace: &str, name: &str) -> Option<FunctionRef>;
}

pub trait DirectiveFactory: std::fmt::Debug {
    fn create_directive(
        &self,
        ty: &str,
        namespace: &str,
        literal: &str,
    ) -> Option<Rc<dyn Directive>>;
}

#[derive(Debug)]
pub struct Library {
    functions: IndexMap<String, FunctionRef>,
    consts: IndexMap<String, ConstRef>,
    directives: IndexMap<String, Rc<dyn DirectiveFactory>>,
}

impl Library {
    /// Create a library preloaded with the full standard library.
    pub fn with_stdlib() -> Self {
        let mut lib = Self::empty();
        lib.register_stdlib();
        lib
    }

    /// Create an empty library with no registered functions or constants.
    pub fn empty() -> Self {
        Self {
            functions: IndexMap::new(),
            consts: IndexMap::new(),
            directives: IndexMap::new(),
        }
    }

    /// Backwards-compatible constructor; defaults to a standard-library instance.
    pub fn new() -> Self {
        Self::with_stdlib()
    }

    pub fn register_function<F>(&mut self, namespace: Option<&str>, name: &str, func: F)
    where
        F: VmFunction + 'static,
    {
        self.insert_function(namespace, name, VmOperand::Function(Arc::new(func)));
    }

    pub fn register_async_function<F>(&mut self, namespace: Option<&str>, name: &str, func: F)
    where
        F: VmAsyncFunction + 'static,
    {
        self.insert_function(namespace, name, VmOperand::AsyncFunction(Arc::new(func)));
    }

    fn insert_function(&mut self, namespace: Option<&str>, name: &str, operand: VmOperand) {
        let key = match namespace {
            Some(ns) => format!("{ns}.{name}"),
            None => name.to_string(),
        };
        let is_async = matches!(operand, VmOperand::AsyncFunction(_));
        self.functions
            .insert(key, FunctionRef { operand, is_async });
    }

    pub fn register_const<C>(&mut self, namespace: Option<&str>, name: &str, constant: C)
    where
        C: VmConstant + 'static,
    {
        self.insert_const(namespace, name, VmOperand::Constant(Arc::new(constant)));
    }

    pub fn register_async_const<C>(&mut self, namespace: Option<&str>, name: &str, constant: C)
    where
        C: VmAsyncConstant + 'static,
    {
        self.insert_const(
            namespace,
            name,
            VmOperand::AsyncConstant(Arc::new(constant)),
        );
    }

    fn insert_const(&mut self, namespace: Option<&str>, name: &str, operand: VmOperand) {
        let key = match namespace {
            Some(ns) => format!("{ns}.{name}"),
            None => name.to_string(),
        };
        let is_async = matches!(operand, VmOperand::AsyncConstant(_));
        self.consts.insert(key, ConstRef { operand, is_async });
    }

    pub fn register_directive_factory(
        &mut self,
        ty: impl Into<String>,
        factory: Rc<dyn DirectiveFactory>,
    ) {
        self.directives.insert(ty.into(), factory);
    }

    fn register_stdlib(&mut self) {}
}

impl Default for Library {
    fn default() -> Self {
        Self::with_stdlib()
    }
}

impl LibraryResolver for Library {
    fn resolve_function(&self, namespace: Option<&str>, name: &str) -> Option<FunctionRef> {
        let key = match namespace {
            Some(ns) => format!("{ns}.{name}"),
            None => name.to_string(),
        };
        self.functions.get(&key).cloned()
    }

    fn resolve_const(&self, namespace: Option<&str>, name: &str) -> Option<ConstRef> {
        let key = match namespace {
            Some(ns) => format!("{ns}.{name}"),
            None => name.to_string(),
        };
        self.consts.get(&key).cloned()
    }

    fn resolve_directive_type(&self, ty: &str) -> Option<Rc<dyn DirectiveFactory>> {
        self.directives.get(ty).cloned()
    }
}
