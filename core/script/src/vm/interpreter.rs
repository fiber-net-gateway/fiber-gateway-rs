use super::bop;
use super::op::{Instruction, OpCode};
use super::operand::{VmAsyncConstantRef, VmAsyncFunctionRef, VmCallContext, VmOperand, VmResult};
use super::state::{PendingOp, VmError, VmState};
use crate::AttachPtr;
use fiber_json::JsValue;
use fiber_string::JsString;
use std::future::Future;
use std::marker::PhantomData;
use std::pin::Pin;
use std::task::{Context, Poll};

type VmFuture<'a> = Pin<Box<dyn Future<Output = VmResult> + 'a>>;

/// A small interpreter that mirrors the control-flow of the Java VM.
pub struct InterpreterVm<'attach> {
    code: Vec<u32>,
    operands: Vec<VmOperand>,
    stack: Vec<JsValue>,
    var_table: Vec<JsValue>,
    pos: Vec<u64>,
    exp_ins: Vec<u32>,
    root: JsValue,
    attach: Option<AttachPtr<'attach>>,
    _attach: PhantomData<&'attach ()>,
    pub state: VmState,
    pub pc: usize,
    pub sp: usize,
    pub rt_value: Option<JsValue>,
    pub rt_error: Option<VmError>,
    arg_cnt: usize,
    call_args: Vec<JsValue>,
    pending_future: Option<VmFuture<'attach>>,
    pending_op: Option<PendingOp>,
    pending_exception: Option<JsValue>,
}

impl Unpin for InterpreterVm<'_> {}

/// Future wrapper that drives a pinned VM.
pub struct InterpreterVmFuture<'vm, 'attach> {
    vm: Pin<&'vm mut InterpreterVm<'attach>>,
}

impl<'attach> InterpreterVm<'attach> {
    pub fn into_future<'vm>(self: Pin<&'vm mut Self>) -> InterpreterVmFuture<'vm, 'attach> {
        InterpreterVmFuture { vm: self }
    }
}

impl Future for InterpreterVmFuture<'_, '_> {
    type Output = VmResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // SAFETY: `vm` is already pinned; accessing the inner mutably without moving is fine.
        let mut vm = unsafe { self.map_unchecked_mut(|s| s.vm.as_mut().get_mut()) };
        match vm.run(cx) {
            VmState::Pending => Poll::Pending,
            VmState::EndReturn | VmState::Return => {
                let val = vm.rt_value.clone().unwrap_or(JsValue::Null);
                Poll::Ready(Ok(val))
            }
            VmState::EndError | VmState::Throw => {
                let err = vm
                    .rt_error
                    .clone()
                    .unwrap_or_else(|| VmError::new("unknown error", vm.current_pos()));
                Poll::Ready(Err(err))
            }
            VmState::Running => Poll::Pending,
        }
    }
}

impl<'attach> InterpreterVm<'attach> {
    pub const INSTRUMENT_LEN: usize = 8;
    pub const ITERATOR_LEN: usize = 12;
    pub const ITERATOR_OFF: usize = Self::INSTRUMENT_LEN + Self::ITERATOR_LEN;
    pub const MAX_ITERATOR_VAR: usize = (1 << Self::ITERATOR_LEN) - 1;

    pub fn new(
        code: Vec<u32>,
        operands: Vec<VmOperand>,
        stack_size: usize,
        var_table_size: usize,
        root: JsValue,
        pos: Vec<u64>,
        exp_ins: Vec<u32>,
        attach: Option<AttachPtr<'attach>>,
    ) -> Self {
        Self {
            code,
            operands,
            stack: Vec::with_capacity(stack_size),
            var_table: vec![JsValue::Undefined; var_table_size],
            pos,
            exp_ins,
            root,
            attach,
            _attach: PhantomData,
            state: VmState::Running,
            pc: 0,
            sp: 0,
            rt_value: None,
            rt_error: None,
            arg_cnt: 0,
            call_args: Vec::new(),
            pending_future: None,
            pending_op: None,
            pending_exception: None,
        }
    }

    pub fn arg_count(&self) -> usize {
        self.arg_cnt
    }

    pub fn arg(&self, idx: usize) -> Option<&JsValue> {
        if idx >= self.arg_cnt {
            return None;
        }
        self.call_args.get(idx)
    }

    pub fn attach_ptr(&self) -> Option<*const ()> {
        self.attach.map(|a| a.as_ptr())
    }

    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn attach_as<T>(&self) -> Option<&'attach T> {
        self.attach.map(|a| unsafe { a.cast::<T>() })
    }

    fn push(&mut self, value: JsValue) {
        self.stack.push(value);
        self.sp = self.stack.len();
    }

    fn pop(&mut self) -> Option<JsValue> {
        let value = self.stack.pop();
        self.sp = self.stack.len();
        value
    }

    fn peek(&self) -> Option<JsValue> {
        self.stack.last().cloned()
    }

    fn store_var(&mut self, idx: usize, value: JsValue) {
        if idx >= self.var_table.len() {
            self.var_table.resize(idx + 1, JsValue::Undefined);
        }
        self.var_table[idx] = value;
    }

    fn current_pos(&self) -> Option<u64> {
        if self.pc == 0 {
            return None;
        }
        self.pos.get(self.pc - 1).cloned()
    }

    #[allow(unreachable_patterns)]
    pub fn run(&mut self, cx: &mut Context<'_>) -> VmState {
        if self.poll_pending(cx) == Poll::Pending {
            return VmState::Pending;
        }

        while self.pc < self.code.len() && matches!(self.state, VmState::Running) {
            let instr = Instruction(self.code[self.pc]);
            self.pc += 1;

            let Some(opcode) = instr.opcode() else {
                self.fail_unknown(instr);
                break;
            };

            match opcode {
                OpCode::Noop => {}
                OpCode::LoadConst => {
                    let idx = instr.operand_u16() as usize;
                    let value = self
                        .operands
                        .get(idx)
                        .and_then(VmOperand::as_value)
                        .cloned()
                        .unwrap_or(JsValue::Undefined);
                    self.push(value);
                }
                OpCode::LoadRoot => self.push(self.root.clone()),
                OpCode::Dump => {
                    if let Some(value) = self.peek() {
                        self.push(value);
                    }
                }
                OpCode::Pop => {
                    drop(self.pop());
                }
                OpCode::LoadVar => {
                    let idx = instr.operand_u16() as usize;
                    let value = self
                        .var_table
                        .get(idx)
                        .cloned()
                        .unwrap_or(JsValue::Undefined);
                    self.push(value);
                }
                OpCode::StoreVar => {
                    let idx = instr.operand_u16() as usize;
                    if let Some(value) = self.pop() {
                        self.store_var(idx, value);
                    }
                }
                OpCode::UnaryPlus => {
                    if let Some(val) = self.pop() {
                        self.push(self.to_numeric(&val));
                    }
                }
                OpCode::UnaryMinus => {
                    if let Some(val) = self.pop() {
                        match self.to_numeric(&val) {
                            JsValue::Int(i) => self.push(JsValue::Int(-i)),
                            JsValue::Float(f) => self.push(JsValue::Float(-f)),
                            other => self.push(other),
                        }
                    }
                }
                OpCode::UnaryNeg => {
                    if let Some(val) = self.pop() {
                        self.push(JsValue::Bool(!self.is_truthy(&val)));
                    }
                }
                OpCode::UnaryTypeof => {
                    if let Some(val) = self.pop() {
                        self.push(JsValue::String(JsString::from(self.typeof_label(&val))));
                    }
                }
                OpCode::CallFunc => self.exec_call_func(instr),
                OpCode::CallFuncSpread => self.exec_call_func_spread(instr),
                OpCode::CallConst => self.exec_call_const(instr),
                OpCode::CallAsyncConst => self.exec_call_async_const(instr, cx),
                OpCode::CallAsyncFunc => self.exec_call_async_func(instr, cx),
                OpCode::CallAsyncFuncSpread => self.exec_call_async_func_spread(instr, cx),
                OpCode::BopPlus => self.bin(bop::plus),
                OpCode::BopMinus => self.bin(bop::minus),
                OpCode::BopMultiply => self.bin(bop::multiply),
                OpCode::BopDivide => self.bin(bop::divide),
                OpCode::BopMod => self.bin(bop::modulo),
                OpCode::BopMatch => self.bin(bop::matches),
                OpCode::BopLt => self.bin(bop::lt),
                OpCode::BopLte => self.bin(bop::lte),
                OpCode::BopGt => self.bin(bop::gt),
                OpCode::BopGte => self.bin(bop::gte),
                OpCode::BopEq => self.bin(bop::eq),
                OpCode::BopSeq => self.bin(bop::seq),
                OpCode::BopNe => self.bin(bop::ne),
                OpCode::BopSne => self.bin(bop::sne),
                OpCode::BopIn => self.bin(bop::contains),
                OpCode::ExpObject => self.expand_object(),
                OpCode::ExpArray => self.expand_array(),
                OpCode::NewObject => {
                    self.push(JsValue::new_empty_object());
                }
                OpCode::NewArray => {
                    self.push(JsValue::new_empty_array());
                }
                OpCode::PushArray => {
                    let value = self.pop().unwrap_or(JsValue::Undefined);
                    let target = self.pop().unwrap_or(JsValue::Undefined);
                    self.push(self.push_array(target, value));
                }
                OpCode::IdxGet => self.idx_get(),
                OpCode::IdxSet => self.idx_set(true),
                OpCode::IdxSet1 => self.idx_set(false),
                OpCode::PropGet => {
                    let key = instr.operand_u24() as usize;
                    let Some(VmOperand::Text(name)) = self.operands.get(key).cloned() else {
                        self.fail_unknown(instr);
                        break;
                    };
                    self.prop_get(&name);
                }
                OpCode::PropSet => {
                    let key = instr.operand_u24() as usize;
                    let Some(VmOperand::Text(name)) = self.operands.get(key).cloned() else {
                        self.fail_unknown(instr);
                        break;
                    };
                    self.prop_set(&name, true);
                }
                OpCode::PropSet1 => {
                    let key = instr.operand_u24() as usize;
                    let Some(VmOperand::Text(name)) = self.operands.get(key).cloned() else {
                        self.fail_unknown(instr);
                        break;
                    };
                    self.prop_set(&name, false);
                }
                OpCode::IterateInto => {
                    let idx = (instr.0 >> Self::INSTRUMENT_LEN) as usize;
                    let target = self.pop().unwrap_or(JsValue::Undefined);
                    self.store_var(
                        idx,
                        JsValue::Iterator(fiber_json::JsIterator::from_value(target)),
                    );
                }
                OpCode::IterateNext => {
                    let idx = (instr.0 >> Self::INSTRUMENT_LEN) as usize;
                    let progressed = self
                        .iterator_var_mut(idx)
                        .map(|iter| iter.next())
                        .unwrap_or(false);
                    self.push(JsValue::Bool(progressed));
                }
                OpCode::IterateKey => {
                    let iter_idx = (instr.0 >> Self::ITERATOR_OFF) as usize;
                    let dst = ((instr.0 >> Self::INSTRUMENT_LEN) as usize) & Self::MAX_ITERATOR_VAR;
                    if let Some(key) = self
                        .iterator_var_mut(iter_idx)
                        .and_then(|iter| iter.current_key())
                    {
                        self.store_var(dst, key);
                    } else {
                        self.store_var(dst, JsValue::Undefined);
                    }
                }
                OpCode::IterateValue => {
                    let iter_idx = (instr.0 >> Self::ITERATOR_OFF) as usize;
                    let dst = ((instr.0 >> Self::INSTRUMENT_LEN) as usize) & Self::MAX_ITERATOR_VAR;
                    if let Some(val) = self
                        .iterator_var_mut(iter_idx)
                        .and_then(|iter| iter.current_value())
                    {
                        self.store_var(dst, val);
                    } else {
                        self.store_var(dst, JsValue::Undefined);
                    }
                }
                OpCode::IntoCatch => {
                    let idx = (instr.0 >> Self::INSTRUMENT_LEN) as usize;
                    let exc = self.exception_value();
                    self.store_var(idx, exc);
                }
                OpCode::Jump => {
                    self.pc = instr.operand_u24() as usize;
                }
                OpCode::JumpIfFalse => {
                    if let Some(cond) = self.pop() {
                        if !self.is_truthy(&cond) {
                            self.pc = instr.operand_u24() as usize;
                        }
                    }
                }
                OpCode::JumpIfTrue => {
                    if let Some(cond) = self.pop() {
                        if self.is_truthy(&cond) {
                            self.pc = instr.operand_u24() as usize;
                        }
                    }
                }
                OpCode::EndReturn => {
                    self.rt_value = self.peek();
                    self.state = VmState::EndReturn;
                }
                OpCode::ThrowExp => {
                    let epc = self.pc.saturating_sub(1);
                    let error = self.pop().unwrap_or(JsValue::Undefined);
                    self.pending_exception = Some(error.clone());
                    let err = VmError::from_js(error, self.pos.get(epc).cloned());
                    self.handle_error(err, epc);
                }
                _ => self.unimplemented(opcode, instr),
            }

            if self.poll_pending(cx) == Poll::Pending {
                return VmState::Pending;
            }
        }

        if matches!(self.state, VmState::Running) && self.pc >= self.code.len() {
            self.rt_value = self.peek();
            self.state = VmState::EndReturn;
        }

        self.state.clone()
    }

    fn exec_call_func(&mut self, instr: Instruction) {
        let arg_cnt = ((instr.0 >> 8) & 0xFF) as usize;
        let func_idx = (instr.0 >> 16) as usize;
        let Some(VmOperand::Function(func)) = self.operands.get(func_idx).cloned() else {
            self.fail_unknown(instr);
            return;
        };

        if self.stack.len() < arg_cnt {
            self.rt_error = Some(VmError::new(
                format!(
                    "CALL_FUNC expects {arg_cnt} args but stack has {}",
                    self.stack.len()
                ),
                self.current_pos(),
            ));
            self.state = VmState::EndError;
            return;
        }

        let new_sp = self.stack.len() - arg_cnt;
        self.arg_cnt = arg_cnt;
        self.call_args.clear();
        self.call_args.extend(self.stack.drain(new_sp..));
        self.sp = self.stack.len();

        let ctx = VmCallContext::new(self.call_args.clone(), self.attach, self.current_pos());
        match func.call(ctx) {
            Ok(val) => self.push(val),
            Err(err) => {
                let epc = self.pc.saturating_sub(1);
                self.handle_error(self.error_with_pos(err), epc);
            }
        }
    }

    fn exec_call_func_spread(&mut self, instr: Instruction) {
        let func_idx = instr.operand_u24() as usize;
        let Some(VmOperand::Function(func)) = self.operands.get(func_idx).cloned() else {
            self.fail_unknown(instr);
            return;
        };

        let Some(args_val) = self.peek() else {
            self.rt_error = Some(VmError::new(
                "CALL_FUNC_SPREAD missing args array",
                self.current_pos(),
            ));
            self.state = VmState::EndError;
            return;
        };

        let array = match args_val {
            JsValue::Array(arr) => arr,
            _ => {
                self.rt_error = Some(VmError::new(
                    "CALL_FUNC_SPREAD expects array on stack",
                    self.current_pos(),
                ));
                self.state = VmState::EndError;
                return;
            }
        };

        let len = array.len();
        self.arg_cnt = len;
        self.call_args.clear();
        for idx in 0..len {
            self.call_args
                .push(array.get(idx).unwrap_or(JsValue::Undefined));
        }

        let ctx = VmCallContext::new(self.call_args.clone(), self.attach, self.current_pos());
        match func.call(ctx) {
            Ok(val) => {
                if let Some(last) = self.stack.last_mut() {
                    *last = val;
                }
                self.sp = self.stack.len();
            }
            Err(err) => {
                let epc = self.pc.saturating_sub(1);
                self.handle_error(self.error_with_pos(err), epc);
            }
        }
    }

    fn exec_call_const(&mut self, instr: Instruction) {
        let idx = instr.operand_u24() as usize;
        let Some(VmOperand::Constant(constant)) = self.operands.get(idx).cloned() else {
            self.fail_unknown(instr);
            return;
        };

        let ctx = VmCallContext::new(Vec::new(), self.attach, self.current_pos());
        match constant.value(ctx) {
            Ok(val) => self.push(val),
            Err(err) => {
                let epc = self.pc.saturating_sub(1);
                self.handle_error(self.error_with_pos(err), epc);
            }
        }
    }

    fn exec_call_async_const(&mut self, instr: Instruction, cx: &mut Context<'_>) {
        let idx = instr.operand_u24() as usize;
        let Some(VmOperand::AsyncConstant(constant)) = self.operands.get(idx).cloned() else {
            self.fail_unknown(instr);
            return;
        };

        self.schedule_async_const(constant);
        let _ = self.poll_pending(cx);
    }

    fn exec_call_async_func(&mut self, instr: Instruction, cx: &mut Context<'_>) {
        let arg_cnt = ((instr.0 >> 8) & 0xFF) as usize;
        let func_idx = (instr.0 >> 16) as usize;
        let Some(VmOperand::AsyncFunction(func)) = self.operands.get(func_idx).cloned() else {
            self.fail_unknown(instr);
            return;
        };

        if self.stack.len() < arg_cnt {
            self.rt_error = Some(VmError::new(
                format!(
                    "CALL_ASYNC_FUNC expects {arg_cnt} args but stack has {}",
                    self.stack.len()
                ),
                self.current_pos(),
            ));
            self.state = VmState::EndError;
            return;
        }

        let new_sp = self.stack.len() - arg_cnt;
        self.arg_cnt = arg_cnt;
        self.call_args.clear();
        self.call_args.extend(self.stack.drain(new_sp..));
        self.sp = self.stack.len();

        self.schedule_async_func(func, false);
        let _ = self.poll_pending(cx);
    }

    fn exec_call_async_func_spread(&mut self, instr: Instruction, cx: &mut Context<'_>) {
        let func_idx = instr.operand_u24() as usize;
        let Some(VmOperand::AsyncFunction(func)) = self.operands.get(func_idx).cloned() else {
            self.fail_unknown(instr);
            return;
        };

        let Some(args_val) = self.peek() else {
            self.rt_error = Some(VmError::new(
                "CALL_ASYNC_FUNC_SPREAD missing args array",
                self.current_pos(),
            ));
            self.state = VmState::EndError;
            return;
        };

        let array = match args_val {
            JsValue::Array(arr) => arr,
            _ => {
                self.rt_error = Some(VmError::new(
                    "CALL_ASYNC_FUNC_SPREAD expects array on stack",
                    self.current_pos(),
                ));
                self.state = VmState::EndError;
                return;
            }
        };

        let len = array.len();
        self.arg_cnt = len;
        self.call_args.clear();
        for idx in 0..len {
            self.call_args
                .push(array.get(idx).unwrap_or(JsValue::Undefined));
        }

        self.schedule_async_func(func, true);
        let _ = self.poll_pending(cx);
    }

    fn push_array(&self, target: JsValue, value: JsValue) -> JsValue {
        match target {
            JsValue::Array(array) => {
                array.push(value);
                JsValue::Array(array)
            }
            other => other,
        }
    }

    fn iterator_var_mut(&mut self, idx: usize) -> Option<&mut fiber_json::JsIterator> {
        match self.var_table.get_mut(idx) {
            Some(JsValue::Iterator(iter)) => Some(iter),
            _ => None,
        }
    }

    fn schedule_async_const(&mut self, constant: VmAsyncConstantRef) {
        let ctx = VmCallContext::new(Vec::new(), self.attach, self.current_pos());
        self.pending_future = Some(Box::pin(constant.value(ctx)));
        self.pending_op = Some(PendingOp::AsyncConst);
        self.state = VmState::Pending;
    }

    fn schedule_async_func(&mut self, func: VmAsyncFunctionRef, spread: bool) {
        let ctx = VmCallContext::new(self.call_args.clone(), self.attach, self.current_pos());
        self.pending_future = Some(Box::pin(func.call(ctx)));
        self.pending_op = Some(PendingOp::AsyncFunc { spread });
        self.state = VmState::Pending;
    }

    fn bin(&mut self, op: fn(JsValue, JsValue) -> JsValue) {
        let right = self.pop().unwrap_or(JsValue::Undefined);
        let left = self.pop().unwrap_or(JsValue::Undefined);
        self.push(op(left, right));
    }

    fn expand_object(&mut self) {
        let source = self.pop().unwrap_or(JsValue::Undefined);
        let target = self.pop().unwrap_or(JsValue::Undefined);
        if let (JsValue::Object(dst), JsValue::Object(src)) = (target.clone(), source) {
            for (k, v) in src.borrow().iter() {
                dst.insert(k.clone(), v.clone());
            }
            self.push(JsValue::Object(dst));
        } else {
            self.push(target);
        }
    }

    fn expand_array(&mut self) {
        let source = self.pop().unwrap_or(JsValue::Undefined);
        let target = self.pop().unwrap_or(JsValue::Undefined);
        if let JsValue::Array(dst) = target.clone() {
            match source {
                JsValue::Array(src) => {
                    for v in src.borrow().iter() {
                        dst.push(v.clone());
                    }
                }
                JsValue::Object(obj) => {
                    for (_, v) in obj.borrow().iter() {
                        dst.push(v.clone());
                    }
                }
                _ => {}
            }
            self.push(JsValue::Array(dst));
        } else {
            self.push(target);
        }
    }

    fn idx_get(&mut self) {
        let index = self.pop().unwrap_or(JsValue::Undefined);
        let target = self.pop().unwrap_or(JsValue::Undefined);
        let val = match (target, index) {
            (JsValue::Array(arr), JsValue::Int(i)) if i >= 0 => {
                arr.get(i as usize).unwrap_or(JsValue::Undefined)
            }
            (JsValue::Object(obj), JsValue::String(key)) => {
                obj.get_by_key(&key).unwrap_or(JsValue::Undefined)
            }
            _ => JsValue::Undefined,
        };
        self.push(val);
    }

    fn idx_set(&mut self, return_obj: bool) {
        if self.stack.len() < 3 {
            self.rt_error = Some(VmError::new("IDX_SET missing operands", self.current_pos()));
            self.state = VmState::EndError;
            return;
        }
        let val = self.pop().unwrap_or(JsValue::Undefined);
        let index = self.pop().unwrap_or(JsValue::Undefined);
        let target = self.pop().unwrap_or(JsValue::Undefined);

        let updated = match (target.clone(), index, val) {
            (JsValue::Array(arr), JsValue::Int(i), value) if i >= 0 => {
                let idx = i as usize;
                if idx < arr.len() {
                    arr.replace(idx, value);
                } else {
                    arr.insert(idx, value);
                }
                JsValue::Array(arr)
            }
            (JsValue::Object(obj), JsValue::String(key), value) => {
                obj.insert(key, value);
                JsValue::Object(obj)
            }
            (other, _, _) => other,
        };
        if return_obj {
            self.push(updated);
        }
    }

    fn prop_get(&mut self, name: &str) {
        let target = self.pop().unwrap_or(JsValue::Undefined);
        let val = match target.clone() {
            JsValue::Object(obj) => obj.get(name).unwrap_or(JsValue::Undefined),
            _ => JsValue::Undefined,
        };
        self.push(val);
    }

    fn prop_set(&mut self, name: &str, return_obj: bool) {
        let val = self.pop().unwrap_or(JsValue::Undefined);
        let target = self.pop().unwrap_or(JsValue::Undefined);
        let updated = match target.clone() {
            JsValue::Object(obj) => {
                obj.insert(JsString::from(name), val);
                JsValue::Object(obj)
            }
            _ => target.clone(),
        };
        if return_obj {
            self.push(updated);
        }
    }

    fn is_truthy(&self, val: &JsValue) -> bool {
        match val {
            JsValue::Undefined | JsValue::Null => false,
            JsValue::Bool(b) => *b,
            JsValue::Int(i) => *i != 0,
            JsValue::Float(f) => !f.is_nan() && *f != 0.0,
            JsValue::String(s) => !s.is_empty(),
            JsValue::Array(a) => !a.is_empty(),
            JsValue::Object(o) => o.len() > 0,
            JsValue::Binary(b) => !b.is_empty(),
            JsValue::Iterator(_) | JsValue::Exception(_) => true,
        }
    }

    #[allow(dead_code)]
    fn to_number(&self, val: &JsValue) -> f64 {
        match val {
            JsValue::Int(i) => *i as f64,
            JsValue::Float(f) => *f,
            JsValue::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            JsValue::String(s) => s.to_std_string_lossy().parse::<f64>().unwrap_or(f64::NAN),
            JsValue::Null => 0.0,
            _ => f64::NAN,
        }
    }

    fn to_numeric(&self, val: &JsValue) -> JsValue {
        match val {
            JsValue::Int(i) => JsValue::Int(*i),
            JsValue::Float(f) => JsValue::Float(*f),
            JsValue::Bool(b) => JsValue::Int(if *b { 1 } else { 0 }),
            JsValue::String(s) => s
                .to_std_string_lossy()
                .parse::<f64>()
                .map(JsValue::Float)
                .unwrap_or(JsValue::Float(f64::NAN)),
            JsValue::Null => JsValue::Int(0),
            _ => JsValue::Float(f64::NAN),
        }
    }

    fn typeof_label(&self, val: &JsValue) -> &'static str {
        match val {
            JsValue::Undefined => "undefined",
            JsValue::Null => "object",
            JsValue::Bool(_) => "boolean",
            JsValue::Int(_) | JsValue::Float(_) => "number",
            JsValue::String(_) => "string",
            JsValue::Binary(_) => "binary",
            JsValue::Array(_) => "array",
            JsValue::Object(_) => "object",
            JsValue::Iterator(_) => "iterator",
            JsValue::Exception(_) => "exception",
        }
    }

    fn exception_value(&mut self) -> JsValue {
        if let Some(exc) = self.pending_exception.take() {
            return exc;
        }
        if let Some(err) = &self.rt_error {
            JsValue::exception(err.message.clone(), err.position)
        } else {
            JsValue::exception("unknown error", self.current_pos())
        }
    }

    fn fail_unknown(&mut self, instr: Instruction) {
        let epc = self.pc.saturating_sub(1);
        let err = VmError::new(
            format!("unknown opcode: {:#x}", instr.0),
            self.pos.get(epc).cloned(),
        );
        self.handle_error(err, epc);
    }

    fn unimplemented(&mut self, opcode: OpCode, instr: Instruction) {
        let epc = self.pc.saturating_sub(1);
        let err = VmError::new(
            format!("opcode {:?} not implemented (raw {:#x})", opcode, instr.0),
            self.pos.get(epc).cloned(),
        );
        self.handle_error(err, epc);
    }

    fn error_with_pos(&self, mut err: VmError) -> VmError {
        if err.position.is_none() {
            err.position = self.current_pos();
        }
        err
    }

    fn handle_error(&mut self, err: VmError, epc: usize) {
        let mut err = err;
        if err.position.is_none() {
            err.position = self.pos.get(epc).cloned();
        }
        self.rt_error = Some(err);
        self.stack.clear();
        self.sp = 0;
        let catch_pc = self.search_catch(epc);
        if let Some(cpc) = catch_pc {
            self.pc = cpc;
            self.state = VmState::Running;
        } else {
            self.state = VmState::EndError;
        }
    }

    fn search_catch(&self, epc: usize) -> Option<usize> {
        if self.exp_ins.is_empty() {
            return None;
        }

        let mut best: Option<(u32, u32, u32)> = None;
        for chunk in self.exp_ins.chunks_exact(3) {
            let (start, end, catch) = (chunk[0], chunk[1], chunk[2]);
            if (start as usize) <= epc && (end as usize) > epc {
                if best.map_or(true, |b| start >= b.0) {
                    best = Some((start, end, catch));
                }
            }
        }

        best.map(|(_, _, catch)| catch as usize)
    }

    fn poll_pending(&mut self, cx: &mut Context<'_>) -> Poll<()> {
        let Some(mut fut) = self.pending_future.take() else {
            if matches!(self.state, VmState::Pending) {
                self.state = VmState::Running;
            }
            return Poll::Ready(());
        };

        match fut.as_mut().poll(cx) {
            Poll::Pending => {
                self.pending_future = Some(fut);
                self.state = VmState::Pending;
                Poll::Pending
            }
            Poll::Ready(res) => {
                match (self.pending_op.take(), res) {
                    (Some(PendingOp::AsyncConst), Ok(val)) => {
                        self.push(val);
                        self.state = VmState::Running;
                    }
                    (Some(PendingOp::AsyncFunc { spread }), Ok(val)) => {
                        if spread {
                            if let Some(last) = self.stack.last_mut() {
                                *last = val;
                            } else {
                                self.push(val);
                            }
                        } else {
                            self.push(val);
                        }
                        self.state = VmState::Running;
                    }
                    (_, Ok(_)) => {
                        self.state = VmState::Running;
                    }
                    (_, Err(err)) => {
                        let epc = self.pc.saturating_sub(1);
                        self.handle_error(err, epc);
                    }
                }
                Poll::Ready(())
            }
        }
    }
}

impl Unpin for InterpreterVmFuture<'_, '_> {}
