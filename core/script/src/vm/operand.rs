use super::VmError;
use crate::AttachPtr;
use async_trait::async_trait;
use fiber_json::JsValue;
use std::fmt;
use std::future::Future;
use std::sync::Arc;

pub type VmResult = Result<JsValue, VmError>;

#[derive(Clone)]
pub struct VmCallContext<'a> {
    args: Vec<JsValue>,
    attach: Option<AttachPtr<'a>>,
    position: Option<u64>,
}

impl<'a> VmCallContext<'a> {
    pub fn new(args: Vec<JsValue>, attach: Option<AttachPtr<'a>>, position: Option<u64>) -> Self {
        Self {
            args,
            attach,
            position,
        }
    }

    pub fn arg_count(&self) -> usize {
        self.args.len()
    }

    pub fn arg(&self, idx: usize) -> Option<&JsValue> {
        self.args.get(idx)
    }

    pub fn attach_ptr(&self) -> Option<*const ()> {
        self.attach.map(|a| a.as_ptr())
    }

    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn attach_as<T>(&self) -> Option<&'a T> {
        self.attach.map(|a| unsafe { a.cast::<T>() })
    }

    pub fn position(&self) -> Option<u64> {
        self.position
    }
}

pub trait VmFunction: Send + Sync {
    fn call(&self, ctx: VmCallContext<'_>) -> VmResult;
}

#[async_trait(?Send)]
pub trait VmAsyncFunction: Send + Sync {
    async fn call(self: Arc<Self>, ctx: VmCallContext<'_>) -> VmResult;
}

pub trait VmConstant: Send + Sync {
    fn value(&self, ctx: VmCallContext<'_>) -> VmResult;
}

#[async_trait(?Send)]
pub trait VmAsyncConstant: Send + Sync {
    async fn value(self: Arc<Self>, ctx: VmCallContext<'_>) -> VmResult;
}

impl<F> VmFunction for F
where
    F: for<'attach> Fn(VmCallContext<'attach>) -> VmResult + Send + Sync,
{
    fn call(&self, ctx: VmCallContext<'_>) -> VmResult {
        (self)(ctx)
    }
}

impl<F> VmConstant for F
where
    F: for<'attach> Fn(VmCallContext<'attach>) -> VmResult + Send + Sync,
{
    fn value(&self, ctx: VmCallContext<'_>) -> VmResult {
        (self)(ctx)
    }
}

#[async_trait(?Send)]
impl<F, Fut> VmAsyncFunction for F
where
    F: for<'attach> Fn(VmCallContext<'attach>) -> Fut + Send + Sync,
    Fut: Future<Output = VmResult> + 'static,
{
    async fn call(self: Arc<Self>, ctx: VmCallContext<'_>) -> VmResult {
        (self)(ctx).await
    }
}

#[async_trait(?Send)]
impl<F, Fut> VmAsyncConstant for F
where
    F: for<'attach> Fn(VmCallContext<'attach>) -> Fut + Send + Sync,
    Fut: Future<Output = VmResult> + 'static,
{
    async fn value(self: Arc<Self>, ctx: VmCallContext<'_>) -> VmResult {
        (self)(ctx).await
    }
}

pub type VmFunctionRef = Arc<dyn VmFunction>;
pub type VmAsyncFunctionRef = Arc<dyn VmAsyncFunction>;
pub type VmConstantRef = Arc<dyn VmConstant>;
pub type VmAsyncConstantRef = Arc<dyn VmAsyncConstant>;

/// External values referenced by the bytecode stream.
#[derive(Clone)]
pub enum VmOperand {
    Value(JsValue),
    Text(String),
    Function(VmFunctionRef),
    AsyncFunction(VmAsyncFunctionRef),
    Constant(VmConstantRef),
    AsyncConstant(VmAsyncConstantRef),
}

impl fmt::Debug for VmOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VmOperand::Value(v) => f.debug_tuple("Value").field(v).finish(),
            VmOperand::Text(t) => f.debug_tuple("Text").field(t).finish(),
            VmOperand::Function(_) => f.write_str("Function(..)"),
            VmOperand::AsyncFunction(_) => f.write_str("AsyncFunction(..)"),
            VmOperand::Constant(_) => f.write_str("Constant(..)"),
            VmOperand::AsyncConstant(_) => f.write_str("AsyncConstant(..)"),
        }
    }
}

impl VmOperand {
    pub fn as_value(&self) -> Option<&JsValue> {
        match self {
            VmOperand::Value(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_text(&self) -> Option<&str> {
        match self {
            VmOperand::Text(text) => Some(text.as_str()),
            _ => None,
        }
    }
}
