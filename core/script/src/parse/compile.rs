use crate::error::ScriptError;
use crate::parse::ast::*;
use crate::vm::{InterpreterVm, OpCode, VmOperand};
use std::rc::Rc;

#[derive(Debug)]
pub struct CompiledScript {
    pub codes: Vec<u32>,
    pub operands: Vec<VmOperand>,
    pub stack_size: usize,
    pub var_table_size: usize,
    pub pos: Vec<u64>,
    pub exp_ins: Vec<u32>,
    pub has_async: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionRef {
    pub operand: VmOperand,
    pub is_async: bool,
}

#[derive(Debug, Clone)]
pub struct ConstRef {
    pub operand: VmOperand,
    pub is_async: bool,
}

/// Resolve symbols during compilation.
pub trait LibraryResolver {
    fn resolve_function(&self, namespace: Option<&str>, name: &str) -> Option<FunctionRef>;
    fn resolve_const(&self, namespace: Option<&str>, name: &str) -> Option<ConstRef>;
    fn resolve_directive_type(&self, ty: &str) -> Option<Rc<dyn crate::stdlib::DirectiveFactory>>;
}

pub struct Compiler<'a> {
    library: &'a dyn LibraryResolver,
    codes: Vec<u32>,
    operands: Vec<VmOperand>,
    pos: Vec<u64>,
    has_async: bool,
    stack_size: usize,
    var_table_size: usize,
    directives: indexmap::IndexMap<String, Rc<dyn crate::stdlib::Directive>>,
    scopes: Vec<indexmap::IndexMap<String, usize>>,
    loops: Vec<LoopCtx>,
    exceptions: Vec<(u32, u32, u32)>,
}

#[derive(Debug, Default)]
struct LoopCtx {
    #[allow(dead_code)]
    start: u32,
    breaks: Vec<usize>,
    continues: Vec<usize>,
}

impl<'a> Compiler<'a> {
    pub fn new(library: &'a dyn LibraryResolver) -> Self {
        Self {
            library,
            codes: Vec::new(),
            operands: Vec::new(),
            pos: Vec::new(),
            has_async: false,
            stack_size: 8,
            var_table_size: 0,
            directives: indexmap::IndexMap::new(),
            scopes: vec![indexmap::IndexMap::new()],
            loops: Vec::new(),
            exceptions: Vec::new(),
        }
    }

    pub fn compile(mut self, stmts: &[Stmt]) -> Result<CompiledScript, ScriptError> {
        for stmt in stmts {
            self.compile_stmt(stmt)?;
        }
        // ensure termination
        let last_span = stmts.last().map(|s| s.span);
        self.emit(OpCode::EndReturn as u8, 0, span_to_pos(last_span));

        Ok(CompiledScript {
            codes: self.codes,
            operands: self.operands,
            stack_size: self.stack_size,
            var_table_size: self.var_table_size,
            pos: self.pos,
            exp_ins: self
                .exceptions
                .iter()
                .flat_map(|(s, e, c)| [*s, *e, *c])
                .collect(),
            has_async: self.has_async,
        })
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), ScriptError> {
        match &stmt.kind {
            StmtKind::Block(block) => {
                let needs_scope = !matches!(block.ty, BlockType::Script);
                if needs_scope {
                    self.enter_scope();
                }
                for stmt in &block.statements {
                    self.compile_stmt(stmt)?;
                }
                if needs_scope {
                    self.exit_scope();
                }
            }
            StmtKind::Let(bindings) => {
                for binding in bindings {
                    let slot = self.declare_var(&binding.name);
                    if let Some(init) = &binding.init {
                        self.compile_expr(init)?;
                    } else {
                        let idx =
                            self.push_operand(VmOperand::Value(fiber_json::JsValue::Undefined));
                        self.emit(
                            OpCode::LoadConst as u8,
                            idx as u32,
                            span_to_pos(Some(binding.span)),
                        );
                    }
                    // store_var pops the value; duplicate so the assigned value
                    // remains on the stack for potential chained lets.
                    self.emit(OpCode::Dump as u8, 0, span_to_pos(Some(binding.span)));
                    self.emit(
                        OpCode::StoreVar as u8,
                        slot as u32,
                        span_to_pos(Some(binding.span)),
                    );
                    self.emit(OpCode::Pop as u8, 0, span_to_pos(Some(binding.span)));
                }
            }
            StmtKind::Expr(expr) => {
                self.compile_expr(expr)?;
                self.emit(OpCode::Pop as u8, 0, span_to_pos(Some(expr.span)));
            }
            StmtKind::ForOf {
                key,
                value,
                iter,
                body,
            } => {
                self.compile_expr(iter)?;
                // Reserve slots for iterator, key, and value in the current scope.
                let iter_slot = self.declare_var(&format!("__iter{}", self.var_table_size));
                self.emit(
                    OpCode::IterateInto as u8,
                    iter_slot as u32,
                    span_to_pos(Some(iter.span)),
                );

                let loop_start = self.codes.len() as u32;
                self.emit(
                    OpCode::IterateNext as u8,
                    iter_slot as u32,
                    span_to_pos(Some(iter.span)),
                );
                let jmp_exit_pos = self.codes.len();
                self.emit(OpCode::JumpIfFalse as u8, 0, span_to_pos(Some(iter.span)));

                self.loops.push(LoopCtx {
                    start: loop_start,
                    breaks: Vec::new(),
                    continues: Vec::new(),
                });

                if let Some(k) = key {
                    let key_slot = self.declare_var(k);
                    let operand =
                        ((iter_slot as u32) << InterpreterVm::ITERATOR_LEN) | (key_slot as u32);
                    let raw = (OpCode::IterateKey as u8 as u32)
                        | (operand << InterpreterVm::INSTRUMENT_LEN);
                    self.emit_raw(raw, span_to_pos(Some(iter.span)));
                }

                let val_slot = self.declare_var(value);
                let operand =
                    ((iter_slot as u32) << InterpreterVm::ITERATOR_LEN) | (val_slot as u32);
                let raw = (OpCode::IterateValue as u8 as u32)
                    | (operand << InterpreterVm::INSTRUMENT_LEN);
                self.emit_raw(raw, span_to_pos(Some(iter.span)));

                self.compile_stmt(&Stmt {
                    span: body.span,
                    kind: StmtKind::Block(body.clone()),
                })?;

                self.emit(OpCode::Jump as u8, loop_start, span_to_pos(Some(body.span)));
                let loop_end = self.codes.len() as u32;
                self.codes[jmp_exit_pos] |= loop_end << 8;

                if let Some(mut ctx) = self.loops.pop() {
                    for pos in ctx.continues.drain(..) {
                        self.codes[pos] |= loop_start << 8;
                    }
                    for pos in ctx.breaks.drain(..) {
                        self.codes[pos] |= loop_end << 8;
                    }
                }
            }
            StmtKind::Return(Some(expr)) => {
                self.compile_expr(expr)?;
                self.emit(OpCode::EndReturn as u8, 0, span_to_pos(Some(stmt.span)));
            }
            StmtKind::Return(None) => {
                self.emit(OpCode::EndReturn as u8, 0, span_to_pos(Some(stmt.span)));
            }
            StmtKind::Throw(expr) => {
                self.compile_expr(expr)?;
                self.emit(OpCode::ThrowExp as u8, 0, span_to_pos(Some(stmt.span)));
            }
            StmtKind::Break => {
                if self.loops.is_empty() {
                    return Err(ScriptError::with_pos(
                        "break outside of loop",
                        stmt.span.start,
                    ));
                }
                let loop_idx = self.loops.len() - 1;
                let pos = self.codes.len();
                self.emit(OpCode::Jump as u8, 0, span_to_pos(Some(stmt.span)));
                self.loops[loop_idx].breaks.push(pos);
            }
            StmtKind::Continue => {
                if self.loops.is_empty() {
                    return Err(ScriptError::with_pos(
                        "continue outside of loop",
                        stmt.span.start,
                    ));
                }
                let loop_idx = self.loops.len() - 1;
                let pos = self.codes.len();
                self.emit(OpCode::Jump as u8, 0, span_to_pos(Some(stmt.span)));
                self.loops[loop_idx].continues.push(pos);
            }
            StmtKind::Directive { name, ty, literal } => {
                let Some(handler) = self.library.resolve_directive_type(ty) else {
                    return Err(ScriptError::with_pos(
                        format!("unknown directive type {}", ty),
                        stmt.span.start,
                    ));
                };
                let Some(instance) = handler.create_directive(ty, name, literal) else {
                    return Err(ScriptError::with_pos(
                        format!("failed to create directive {}", name),
                        stmt.span.start,
                    ));
                };
                self.directives.insert(name.clone(), instance);
            }
            StmtKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(cond)?;
                // jump placeholder for false branch
                let jmp_false_pos = self.codes.len();
                self.emit(OpCode::JumpIfFalse as u8, 0, span_to_pos(Some(cond.span)));
                self.compile_stmt(&Stmt {
                    span: then_branch.span,
                    kind: StmtKind::Block(then_branch.clone()),
                })?;
                // jump to end after then
                let jmp_end_pos = self.codes.len();
                self.emit(OpCode::Jump as u8, 0, span_to_pos(Some(stmt.span)));

                // patch false jump target
                let else_target = self.codes.len() as u32;
                self.codes[jmp_false_pos] |= else_target << 8;

                if let Some(else_block) = else_branch {
                    self.compile_stmt(&Stmt {
                        span: else_block.span,
                        kind: StmtKind::Block(else_block.clone()),
                    })?;
                }
                let end_target = self.codes.len() as u32;
                self.codes[jmp_end_pos] |= end_target << 8;
            }
            StmtKind::TryCatch { body, err, catch } => {
                let try_start = self.codes.len() as u32;
                self.compile_stmt(&Stmt {
                    span: body.span,
                    kind: StmtKind::Block(body.clone()),
                })?;

                let jmp_end_pos = self.codes.len();
                self.emit(OpCode::Jump as u8, 0, span_to_pos(Some(stmt.span)));

                let catch_pc = self.codes.len() as u32;
                self.exceptions.push((try_start, catch_pc, catch_pc));

                self.enter_scope();
                let err_slot = self.declare_var(err);
                self.emit(
                    OpCode::IntoCatch as u8,
                    err_slot as u32,
                    span_to_pos(Some(catch.span)),
                );
                for stmt in &catch.statements {
                    self.compile_stmt(stmt)?;
                }
                self.exit_scope();

                let end_pc = self.codes.len() as u32;
                self.codes[jmp_end_pos] |= end_pc << 8;
            }
        }
        Ok(())
    }

    #[allow(unreachable_patterns)]
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), ScriptError> {
        match &expr.kind {
            ExprKind::Literal(val) => {
                let idx = self.push_operand(VmOperand::Value(val.clone()));
                self.emit(
                    OpCode::LoadConst as u8,
                    idx as u32,
                    span_to_pos(Some(expr.span)),
                );
            }
            ExprKind::Root => {
                self.emit(OpCode::LoadRoot as u8, 0, span_to_pos(Some(expr.span)));
            }
            ExprKind::Const { namespace, key } => {
                if let Some(c) = self.library.resolve_const(Some(namespace), key) {
                    let idx = self.push_operand(c.operand);
                    let code = if c.is_async {
                        self.has_async = true;
                        OpCode::CallAsyncConst as u8
                    } else {
                        OpCode::CallConst as u8
                    };
                    self.emit(code, idx as u32, span_to_pos(Some(expr.span)));
                } else {
                    return Err(ScriptError::with_pos(
                        format!("unknown const {}.{}", namespace, key),
                        expr.span.start,
                    ));
                }
            }
            ExprKind::Identifier(name) => {
                let Some(slot) = self.resolve_var(name) else {
                    return Err(ScriptError::with_pos(
                        format!("undefined variable {name}"),
                        expr.span.start,
                    ));
                };
                self.emit(
                    OpCode::LoadVar as u8,
                    slot as u32,
                    span_to_pos(Some(expr.span)),
                );
            }
            ExprKind::Call { callee, args } => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                let func = self
                    .library
                    .resolve_function(callee.namespace.as_deref(), &callee.name)
                    .or_else(|| {
                        callee.namespace.as_ref().and_then(|ns| {
                            self.directives
                                .get(ns)
                                .and_then(|dir| dir.find_function(ns, &callee.name))
                        })
                    });
                let Some(fun) = func else {
                    return Err(ScriptError::with_pos(
                        format!("unknown function {}", callee.name),
                        callee.span.start,
                    ));
                };
                let idx = self.push_operand(fun.operand);
                let argc = args.len() as u32;
                let code = if fun.is_async {
                    self.has_async = true;
                    (OpCode::CallAsyncFunc as u8) as u32 | (argc << 8) | ((idx as u32) << 16)
                } else {
                    (OpCode::CallFunc as u8) as u32 | (argc << 8) | ((idx as u32) << 16)
                };
                self.emit_raw(code, span_to_pos(Some(expr.span)));
            }
            ExprKind::Binary { op, left, right } => match op {
                BinaryOp::And => {
                    self.compile_expr(left)?;
                    self.emit(OpCode::Dump as u8, 0, span_to_pos(Some(left.span)));
                    let jump_pos = self.codes.len();
                    self.emit(OpCode::JumpIfFalse as u8, 0, span_to_pos(Some(expr.span)));
                    self.emit(OpCode::Pop as u8, 0, span_to_pos(Some(expr.span)));
                    self.compile_expr(right)?;
                    let end = self.codes.len() as u32;
                    self.codes[jump_pos] |= end << 8;
                }
                BinaryOp::Or => {
                    self.compile_expr(left)?;
                    self.emit(OpCode::Dump as u8, 0, span_to_pos(Some(left.span)));
                    let jump_pos = self.codes.len();
                    self.emit(OpCode::JumpIfTrue as u8, 0, span_to_pos(Some(expr.span)));
                    self.emit(OpCode::Pop as u8, 0, span_to_pos(Some(expr.span)));
                    self.compile_expr(right)?;
                    let end = self.codes.len() as u32;
                    self.codes[jump_pos] |= end << 8;
                }
                _ => {
                    self.compile_expr(left)?;
                    self.compile_expr(right)?;
                    let opcode = match op {
                        BinaryOp::Add => OpCode::BopPlus,
                        BinaryOp::Sub => OpCode::BopMinus,
                        BinaryOp::Mul => OpCode::BopMultiply,
                        BinaryOp::Div => OpCode::BopDivide,
                        BinaryOp::Mod => OpCode::BopMod,
                        BinaryOp::Match => OpCode::BopMatch,
                        BinaryOp::Lt => OpCode::BopLt,
                        BinaryOp::Lte => OpCode::BopLte,
                        BinaryOp::Gt => OpCode::BopGt,
                        BinaryOp::Gte => OpCode::BopGte,
                        BinaryOp::Eq => OpCode::BopEq,
                        BinaryOp::Seq => OpCode::BopSeq,
                        BinaryOp::Ne => OpCode::BopNe,
                        BinaryOp::Sne => OpCode::BopSne,
                        BinaryOp::In => OpCode::BopIn,
                        BinaryOp::And | BinaryOp::Or => unreachable!(),
                    };
                    self.emit(opcode as u8, 0, span_to_pos(Some(expr.span)));
                }
            },
            ExprKind::Unary { op, expr: inner } => {
                self.compile_expr(inner)?;
                let opcode = match op {
                    UnaryOp::Pos => OpCode::UnaryPlus,
                    UnaryOp::Neg => OpCode::UnaryMinus,
                    UnaryOp::Not => OpCode::UnaryNeg,
                    UnaryOp::Typeof => OpCode::UnaryTypeof,
                };
                self.emit(opcode as u8, 0, span_to_pos(Some(expr.span)));
            }
            ExprKind::Array(items) => {
                self.emit(OpCode::NewArray as u8, 0, span_to_pos(Some(expr.span)));
                for item in items {
                    self.compile_expr(item)?;
                    self.emit(OpCode::PushArray as u8, 0, span_to_pos(Some(item.span)));
                }
            }
            ExprKind::Object(props) => {
                self.emit(OpCode::NewObject as u8, 0, span_to_pos(Some(expr.span)));
                for (key, value) in props {
                    match key {
                        ObjectKey::Spread(expr) => {
                            self.compile_expr(expr)?;
                            self.emit(OpCode::ExpObject as u8, 0, span_to_pos(Some(expr.span)));
                        }
                        ObjectKey::Static(name, span) => {
                            self.compile_expr(value)?;
                            let idx = self.push_operand(VmOperand::Text(name.clone()));
                            self.emit(OpCode::PropSet as u8, idx as u32, span_to_pos(Some(*span)));
                        }
                        ObjectKey::Expr(expr) => {
                            self.compile_expr(expr)?;
                            self.compile_expr(value)?;
                            self.emit(OpCode::IdxSet as u8, 0, span_to_pos(Some(value.span)));
                        }
                    }
                }
            }
            ExprKind::Member { object, property } => {
                self.compile_expr(object)?;
                match property {
                    MemberExpr::Named(name, span) => {
                        let idx = self.push_operand(VmOperand::Text(name.clone()));
                        self.emit(OpCode::PropGet as u8, idx as u32, span_to_pos(Some(*span)));
                    }
                    MemberExpr::Computed(expr) => {
                        self.compile_expr(expr)?;
                        self.emit(
                            OpCode::IdxGet as u8,
                            0,
                            span_to_pos(Some(expr.span.union(object.span))),
                        );
                    }
                }
            }
            ExprKind::Assign { target, value } => match &target.kind {
                ExprKind::Identifier(name) => {
                    self.compile_expr(value)?;
                    self.emit(OpCode::Dump as u8, 0, span_to_pos(Some(value.span)));
                    let Some(slot) = self.resolve_var(name) else {
                        return Err(ScriptError::with_pos(
                            format!("undefined variable {name}"),
                            target.span.start,
                        ));
                    };
                    self.emit(
                        OpCode::StoreVar as u8,
                        slot as u32,
                        span_to_pos(Some(target.span)),
                    );
                }
                ExprKind::Member { object, property } => {
                    self.compile_expr(object)?;
                    match property {
                        MemberExpr::Named(name, span) => {
                            self.compile_expr(value)?;
                            let key_idx = self.push_operand(VmOperand::Text(name.clone()));
                            self.emit(
                                OpCode::PropSet as u8,
                                key_idx as u32,
                                span_to_pos(Some(*span)),
                            );
                        }
                        MemberExpr::Computed(expr) => {
                            self.compile_expr(expr)?;
                            self.compile_expr(value)?;
                            self.emit(OpCode::IdxSet as u8, 0, span_to_pos(Some(value.span)));
                        }
                    }
                }
                _ => {
                    return Err(ScriptError::with_pos(
                        "invalid assignment target",
                        target.span.start,
                    ));
                }
            },
            ExprKind::Conditional {
                cond,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(cond)?;
                let jmp_false_pos = self.codes.len();
                self.emit(OpCode::JumpIfFalse as u8, 0, span_to_pos(Some(cond.span)));
                self.compile_expr(then_branch)?;
                let jmp_end_pos = self.codes.len();
                self.emit(OpCode::Jump as u8, 0, span_to_pos(Some(expr.span)));
                let else_target = self.codes.len() as u32;
                self.codes[jmp_false_pos] |= else_target << 8;
                self.compile_expr(else_branch)?;
                let end_target = self.codes.len() as u32;
                self.codes[jmp_end_pos] |= end_target << 8;
            }
            _ => {
                // other expressions not yet compiled
            }
        }
        Ok(())
    }

    fn push_operand(&mut self, op: VmOperand) -> usize {
        let idx = self.operands.len();
        self.operands.push(op);
        idx
    }

    fn declare_var(&mut self, name: &str) -> usize {
        let idx = self.var_table_size;
        self.var_table_size += 1;
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), idx);
        }
        idx
    }

    fn resolve_var(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(idx) = scope.get(name) {
                return Some(*idx);
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.scopes.push(indexmap::IndexMap::new());
    }

    fn exit_scope(&mut self) {
        drop(self.scopes.pop());
    }

    fn emit(&mut self, opcode: u8, operand: u32, pos: u64) {
        let raw = (operand << 8) | opcode as u32;
        self.emit_raw(raw, pos);
    }

    fn emit_raw(&mut self, raw: u32, pos: u64) {
        self.codes.push(raw);
        self.pos.push(pos);
    }
}

fn span_to_pos(span: Option<Span>) -> u64 {
    span.map(|s| ((s.start as u64) << 32) | (s.end as u64))
        .unwrap_or(0)
}
