#![allow(missing_docs, unreachable_pub, missing_debug_implementations)]
mod error;
mod parse;
mod stdlib;
pub mod vm;

use crate::error::ScriptError;
use crate::parse::Parser;
use crate::parse::compile::{CompiledScript, Compiler};
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, RawWaker, RawWakerVTable, Waker};

pub use crate::stdlib::{Directive, DirectiveFactory, Library};
pub use crate::vm::{
    Instruction, InterpreterVm, InterpreterVmFuture, OpCode, VmAsyncConstant, VmAsyncConstantRef,
    VmAsyncFunction, VmAsyncFunctionRef, VmCallContext, VmConstant, VmConstantRef, VmError,
    VmFunction, VmFunctionRef, VmOperand, VmResult, VmState,
};

#[cfg(test)]
mod tests;

#[derive(Clone, Copy)]
pub struct AttachPtr<'a> {
    ptr: *const (),
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> AttachPtr<'a> {
    #[allow(trivial_casts)]
    pub fn from_ref<T>(value: &'a T) -> Self {
        Self {
            ptr: value as *const T as *const (),
            _marker: std::marker::PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *const () {
        self.ptr
    }

    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn cast<T>(&self) -> &'a T {
        unsafe { &*(self.ptr as *const T) }
    }
}

/// Create a library populated with the default standard library.
pub fn new_library() -> Library {
    Library::with_stdlib()
}

/// Create an empty library with no registered functions or constants.
pub fn new_empty_library() -> Library {
    Library::empty()
}

/// Compiled script ready for execution.
#[derive(Debug)]
pub struct Script {
    compiled: CompiledScript,
}

impl Script {
    /// Compile script source with the default standard library.
    pub fn compile(src: &str) -> Result<Self, ScriptError> {
        Self::compile_with(src, Library::with_stdlib())
    }

    /// Compile script source using a custom library.
    pub fn compile_with(src: &str, library: Library) -> Result<Self, ScriptError> {
        let parser = Parser::new(src)?;
        let ast = parser.parse_script()?;
        let compiler = Compiler::new(&library);
        let compiled = compiler.compile(&ast)?;
        Ok(Self { compiled })
    }

    /// Execute script returning a future that resolves to a runtime [`JsValue`].
    pub fn exec<'attach>(
        &self,
        root: fiber_json::JsValue,
        attach: Option<AttachPtr<'attach>>,
    ) -> InterpreterVm<'attach> {
        self.new_vm(root, attach)
    }

    /// Blocking convenience wrapper over [`exec`].
    pub fn block_exec(
        &self,
        root: fiber_json::JsValue,
        attach: Option<AttachPtr<'_>>,
    ) -> Result<fiber_json::JsValue, ScriptError> {
        if self.compiled.has_async {
            return Err(ScriptError::new(
                "script contains async instructions; use async execution",
            ));
        }
        let mut vm = self.new_vm(root, attach);
        block_on_vm(&mut vm).map_err(vm_error_to_script)
    }

    /// Convenience helper accepting [`fiber_json::JsValue`].
    pub fn exec_json(
        &self,
        root: &fiber_json::JsValue,
    ) -> Result<fiber_json::JsValue, ScriptError> {
        self.block_exec(root.clone(), None)
    }
}

impl Script {
    fn new_vm<'attach>(
        &self,
        root: fiber_json::JsValue,
        attach: Option<AttachPtr<'attach>>,
    ) -> InterpreterVm<'attach> {
        InterpreterVm::new(
            self.compiled.codes.clone(),
            self.compiled.operands.clone(),
            self.compiled.stack_size,
            self.compiled.var_table_size,
            root,
            self.compiled.pos.clone(),
            self.compiled.exp_ins.clone(),
            attach,
        )
    }
}

fn block_on_vm(vm: &mut InterpreterVm<'_>) -> VmResult {
    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);
    let pinned = Pin::new(vm);
    let mut fut = pinned.into_future();
    match Pin::new(&mut fut).poll(&mut cx) {
        std::task::Poll::Ready(res) => res,
        std::task::Poll::Pending => Err(VmError::new(
            "blocking execution encountered async instruction",
            None,
        )),
    }
}

fn vm_error_to_script(err: VmError) -> ScriptError {
    let position = err.position.map(|p| (p >> 32) as usize);
    ScriptError {
        message: err.message,
        position,
    }
}

pub(crate) fn noop_waker() -> Waker {
    fn clone(_: *const ()) -> RawWaker {
        RawWaker::new(std::ptr::null(), &VTABLE)
    }
    fn wake(_: *const ()) {}
    fn wake_by_ref(_: *const ()) {}
    fn drop(_: *const ()) {}

    static VTABLE: RawWakerVTable = RawWakerVTable::new(clone, wake, wake_by_ref, drop);
    unsafe { Waker::from_raw(clone(std::ptr::null())) }
}
