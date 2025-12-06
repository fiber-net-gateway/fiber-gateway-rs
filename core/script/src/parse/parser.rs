use crate::error::ScriptError;
use crate::parse::ast::*;
use crate::parse::lexer::{Lexer, Token, TokenKind};
use fiber_json::JsValue;
use fiber_string::JsString;

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
    last_pos: usize,
}

impl Parser {
    pub fn new(src: &str) -> Result<Self, ScriptError> {
        let tokens = Lexer::new(src).tokenize()?;
        Ok(Self {
            tokens,
            idx: 0,
            last_pos: 0,
        })
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.idx).expect("token sentinel")
    }

    fn bump(&mut self) -> &Token {
        let tok = self.tokens.get(self.idx).expect("token sentinel");
        self.idx += 1;
        self.last_pos = tok.pos;
        tok
    }

    fn consume(&mut self, kind: &TokenKind) -> Result<Token, ScriptError> {
        let tok = self.bump().clone();
        if std::mem::discriminant(&tok.kind) == std::mem::discriminant(kind) {
            Ok(tok)
        } else {
            Err(ScriptError::with_pos(
                format!("expected {:?}, found {:?}", kind, tok.kind),
                tok.pos,
            ))
        }
    }

    fn matches(&mut self, kind: &TokenKind) -> bool {
        if std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind) {
            self.idx += 1;
            true
        } else {
            false
        }
    }

    pub fn parse_script(mut self) -> Result<Vec<Stmt>, ScriptError> {
        let mut stmts = Vec::new();
        while !matches!(self.peek().kind, TokenKind::Eof) {
            stmts.push(self.parse_stmt()?);
        }
        let span = Span::new(0, self.last_pos);
        Ok(vec![Stmt {
            span,
            kind: StmtKind::Block(Block {
                span,
                ty: BlockType::Script,
                statements: stmts,
            }),
        }])
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ScriptError> {
        match &self.peek().kind {
            TokenKind::Let => self.parse_let(),
            TokenKind::If => self.parse_if(),
            TokenKind::For => self.parse_for(),
            TokenKind::Continue => {
                let start = self.bump().pos;
                let _ = self.matches(&TokenKind::Semicolon);
                Ok(Stmt {
                    span: Span::new(start, self.last_pos),
                    kind: StmtKind::Continue,
                })
            }
            TokenKind::Break => {
                let start = self.bump().pos;
                let _ = self.matches(&TokenKind::Semicolon);
                Ok(Stmt {
                    span: Span::new(start, self.last_pos),
                    kind: StmtKind::Break,
                })
            }
            TokenKind::Return => {
                let start = self.bump().pos;
                if self.matches(&TokenKind::Semicolon) {
                    return Ok(Stmt {
                        span: Span::new(start, self.last_pos),
                        kind: StmtKind::Return(None),
                    });
                }
                if matches!(self.peek().kind, TokenKind::Eof) {
                    return Err(ScriptError::with_pos("unexpected EOF after return", start));
                }
                let expr = self.parse_expression(0)?;
                let _ = self.matches(&TokenKind::Semicolon);
                Ok(Stmt {
                    span: Span::new(start, expr.span.end),
                    kind: StmtKind::Return(Some(expr)),
                })
            }
            TokenKind::Directive => self.parse_directive(),
            TokenKind::Try => self.parse_try(),
            TokenKind::Throw => {
                let start = self.bump().pos;
                let expr = self.parse_expression(0)?;
                let _ = self.matches(&TokenKind::Semicolon);
                Ok(Stmt {
                    span: Span::new(start, expr.span.end),
                    kind: StmtKind::Throw(expr),
                })
            }
            _ => {
                let expr = self.parse_expression(0)?;
                let _ = self.matches(&TokenKind::Semicolon);
                Ok(Stmt {
                    span: expr.span,
                    kind: StmtKind::Expr(expr),
                })
            }
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, ScriptError> {
        let start = self.bump().pos;
        let mut bindings = Vec::new();
        loop {
            let name = match &self.bump().kind {
                TokenKind::Identifier(id) => id.clone(),
                other => {
                    return Err(ScriptError::with_pos(
                        format!("expected identifier, found {:?}", other),
                        self.peek().pos,
                    ));
                }
            };
            let init = if self.matches(&TokenKind::Assign) {
                Some(self.parse_expression(0)?)
            } else {
                None
            };
            let end = init.as_ref().map(|e| e.span.end).unwrap_or(self.last_pos);
            bindings.push(Binding {
                name,
                init,
                span: Span::new(start, end),
            });
            if !self.matches(&TokenKind::Comma) {
                break;
            }
        }
        let _ = self.matches(&TokenKind::Semicolon);
        Ok(Stmt {
            span: Span::new(start, self.last_pos),
            kind: StmtKind::Let(bindings),
        })
    }

    fn parse_block(&mut self, ty: BlockType) -> Result<Block, ScriptError> {
        let start = self.consume(&TokenKind::LBrace)?.pos;
        let mut body = Vec::new();
        while !self.matches(&TokenKind::RBrace) {
            if matches!(self.peek().kind, TokenKind::Eof) {
                return Err(ScriptError::new("unexpected EOF in block"));
            }
            body.push(self.parse_stmt()?);
        }
        Ok(Block {
            span: Span::new(start, self.last_pos),
            ty,
            statements: body,
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, ScriptError> {
        let start = self.bump().pos;
        self.consume(&TokenKind::LParen)?;
        let cond = self.parse_expression(0)?;
        self.consume(&TokenKind::RParen)?;
        let then_branch = self.parse_block(BlockType::If)?;
        let else_branch = if self.matches(&TokenKind::Else) {
            Some(self.parse_block(BlockType::Else)?)
        } else {
            None
        };
        let end = else_branch
            .as_ref()
            .map(|b| b.span.end)
            .unwrap_or(then_branch.span.end);
        Ok(Stmt {
            span: Span::new(start, end),
            kind: StmtKind::If {
                cond,
                then_branch,
                else_branch,
            },
        })
    }

    fn parse_for(&mut self) -> Result<Stmt, ScriptError> {
        let start = self.bump().pos;
        self.consume(&TokenKind::LParen)?;
        self.consume(&TokenKind::Let)?;
        let first = match self.bump().kind.clone() {
            TokenKind::Identifier(id) => id,
            other => {
                return Err(ScriptError::with_pos(
                    format!("expected identifier, found {:?}", other),
                    self.peek().pos,
                ));
            }
        };
        let mut key = None;
        let mut value = first.clone();
        if self.matches(&TokenKind::Comma) {
            key = Some(first);
            if let TokenKind::Identifier(id) = self.bump().kind.clone() {
                value = id;
            }
        }
        self.consume(&TokenKind::Of)?;
        let iter = self.parse_expression(0)?;
        self.consume(&TokenKind::RParen)?;
        let body = self.parse_block(BlockType::For)?;
        Ok(Stmt {
            span: Span::new(start, body.span.end),
            kind: StmtKind::ForOf {
                key,
                value,
                iter,
                body,
            },
        })
    }

    fn parse_directive(&mut self) -> Result<Stmt, ScriptError> {
        let start = self.bump().pos;
        let name = match self.bump().kind.clone() {
            TokenKind::Identifier(id) => id,
            other => {
                return Err(ScriptError::with_pos(
                    format!("expected identifier after directive, got {:?}", other),
                    self.peek().pos,
                ));
            }
        };
        // Accept either legacy `from` keyword or `=` sign.
        if !self.matches(&TokenKind::From) && !self.matches(&TokenKind::Assign) {
            return Err(ScriptError::with_pos(
                "expected '=' or 'from' after directive name",
                self.peek().pos,
            ));
        }
        let ty = match self.bump().kind.clone() {
            TokenKind::Identifier(id) => id,
            other => {
                return Err(ScriptError::with_pos(
                    format!("expected directive type, found {:?}", other),
                    self.peek().pos,
                ));
            }
        };
        let literal = match self.bump().kind.clone() {
            TokenKind::String(s) => s,
            other => {
                return Err(ScriptError::with_pos(
                    format!("expected string literal, found {:?}", other),
                    self.peek().pos,
                ));
            }
        };
        let _ = self.matches(&TokenKind::Semicolon);
        Ok(Stmt {
            span: Span::new(start, self.last_pos),
            kind: StmtKind::Directive { name, ty, literal },
        })
    }

    fn parse_try(&mut self) -> Result<Stmt, ScriptError> {
        let start = self.bump().pos;
        let body = self.parse_block(BlockType::Try)?;
        self.consume(&TokenKind::Catch)?;
        self.consume(&TokenKind::LParen)?;
        let err = match self.bump().kind.clone() {
            TokenKind::Identifier(id) => id,
            _ => return Err(ScriptError::new("expected identifier in catch clause")),
        };
        self.consume(&TokenKind::RParen)?;
        let catch = self.parse_block(BlockType::Catch)?;
        Ok(Stmt {
            span: Span::new(start, catch.span.end),
            kind: StmtKind::TryCatch { body, err, catch },
        })
    }

    fn parse_expression(&mut self, min_bp: u8) -> Result<Expr, ScriptError> {
        let mut lhs = self.parse_unary()?;

        const COND_BP: u8 = 2; // conditional has low precedence, above assignment.
        loop {
            let op = match self.peek().kind.clone() {
                TokenKind::Question => {
                    if COND_BP < min_bp {
                        break;
                    }
                    self.bump();
                    let then_branch = self.parse_expression(0)?;
                    self.consume(&TokenKind::Colon)?;
                    let else_branch = self.parse_expression(0)?;
                    let span = lhs.span.union(else_branch.span);
                    lhs = Expr {
                        span,
                        kind: ExprKind::Conditional {
                            cond: Box::new(lhs),
                            then_branch: Box::new(then_branch),
                            else_branch: Box::new(else_branch),
                        },
                    };
                    continue;
                }
                TokenKind::Assign => {
                    self.bump();
                    let rhs = self.parse_expression(0)?;
                    let span = lhs.span.union(rhs.span);
                    lhs = Expr {
                        span,
                        kind: ExprKind::Assign {
                            target: Box::new(lhs),
                            value: Box::new(rhs),
                        },
                    };
                    continue;
                }
                TokenKind::OrOr => BinaryOp::Or,
                TokenKind::AndAnd => BinaryOp::And,
                TokenKind::EqEq => BinaryOp::Eq,
                TokenKind::EqEqEq => BinaryOp::Seq,
                TokenKind::Neq => BinaryOp::Ne,
                TokenKind::NeqEq => BinaryOp::Sne,
                TokenKind::Lt => BinaryOp::Lt,
                TokenKind::Lte => BinaryOp::Lte,
                TokenKind::Gt => BinaryOp::Gt,
                TokenKind::Gte => BinaryOp::Gte,
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                TokenKind::Tilde => BinaryOp::Match,
                TokenKind::In => BinaryOp::In,
                _ => break,
            };
            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            self.bump();
            let rhs = self.parse_expression(r_bp)?;
            let span = lhs.span.union(rhs.span);
            lhs = Expr {
                span,
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                },
            };
        }

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Expr, ScriptError> {
        match self.peek().kind.clone() {
            TokenKind::Not => {
                let tok = self.bump().clone();
                let expr = self.parse_unary()?;
                Ok(Expr {
                    span: Span::new(tok.pos, expr.span.end),
                    kind: ExprKind::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    },
                })
            }
            TokenKind::Plus => {
                let tok = self.bump().clone();
                let expr = self.parse_unary()?;
                Ok(Expr {
                    span: Span::new(tok.pos, expr.span.end),
                    kind: ExprKind::Unary {
                        op: UnaryOp::Pos,
                        expr: Box::new(expr),
                    },
                })
            }
            TokenKind::Minus => {
                let tok = self.bump().clone();
                let expr = self.parse_unary()?;
                Ok(Expr {
                    span: Span::new(tok.pos, expr.span.end),
                    kind: ExprKind::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(expr),
                    },
                })
            }
            TokenKind::Typeof => {
                let tok = self.bump().clone();
                let expr = self.parse_unary()?;
                Ok(Expr {
                    span: Span::new(tok.pos, expr.span.end),
                    kind: ExprKind::Unary {
                        op: UnaryOp::Typeof,
                        expr: Box::new(expr),
                    },
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, ScriptError> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.peek().kind.clone() {
                TokenKind::LParen => {
                    let open = self.bump().clone();
                    let mut args = Vec::new();
                    if !matches!(self.peek().kind, TokenKind::RParen) {
                        loop {
                            args.push(self.parse_expression(0)?);
                            if !self.matches(&TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    let end = self.consume(&TokenKind::RParen)?.pos;
                    let callee = self.callee_from_expr(expr)?;
                    expr = Expr {
                        span: Span::new(callee.span.start, end),
                        kind: ExprKind::Call { callee, args },
                    };
                    self.last_pos = end.max(open.pos);
                }
                TokenKind::Dot => {
                    self.bump();
                    let name_tok = self.bump().clone();
                    let name = match name_tok.kind {
                        TokenKind::Identifier(id) => id,
                        other => {
                            return Err(ScriptError::with_pos(
                                format!("expected identifier after '.', found {:?}", other),
                                self.peek().pos,
                            ));
                        }
                    };
                    expr = Expr {
                        span: Span::new(expr.span.start, name_tok.pos),
                        kind: ExprKind::Member {
                            object: Box::new(expr),
                            property: MemberExpr::Named(
                                name,
                                Span::new(name_tok.pos, name_tok.pos),
                            ),
                        },
                    };
                }
                TokenKind::LBracket => {
                    let start = self.bump().pos;
                    let prop = self.parse_expression(0)?;
                    let end = self.consume(&TokenKind::RBracket)?.pos;
                    expr = Expr {
                        span: Span::new(expr.span.start, end),
                        kind: ExprKind::Member {
                            object: Box::new(expr),
                            property: MemberExpr::Computed(Box::new(prop)),
                        },
                    };
                    self.last_pos = end.max(start);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ScriptError> {
        let tok = self.bump().clone();
        match tok.kind {
            TokenKind::Identifier(id) => match id.as_str() {
                "true" => Ok(Expr {
                    span: Span::new(tok.pos, tok.pos),
                    kind: ExprKind::Literal(JsValue::Bool(true)),
                }),
                "false" => Ok(Expr {
                    span: Span::new(tok.pos, tok.pos),
                    kind: ExprKind::Literal(JsValue::Bool(false)),
                }),
                "null" => Ok(Expr {
                    span: Span::new(tok.pos, tok.pos),
                    kind: ExprKind::Literal(JsValue::Null),
                }),
                "$" => Ok(Expr {
                    span: Span::new(tok.pos, tok.pos),
                    kind: ExprKind::Root,
                }),
                _ => {
                    if let Some(stripped) = id.strip_prefix('$') {
                        if self.matches(&TokenKind::Dot) {
                            let key_tok = self.bump().clone();
                            if let TokenKind::Identifier(key) = key_tok.kind {
                                return Ok(Expr {
                                    span: Span::new(tok.pos, key_tok.pos),
                                    kind: ExprKind::Const {
                                        namespace: stripped.to_string(),
                                        key,
                                    },
                                });
                            } else {
                                return Err(ScriptError::with_pos(
                                    "expected identifier after const access",
                                    key_tok.pos,
                                ));
                            }
                        }
                    }
                    Ok(Expr {
                        span: Span::new(tok.pos, tok.pos),
                        kind: ExprKind::Identifier(id),
                    })
                }
            },
            TokenKind::Number(n) => {
                let val = if (n.fract() - 0.0).abs() < f64::EPSILON {
                    JsValue::Int(n as i64)
                } else {
                    JsValue::Float(n)
                };
                Ok(Expr {
                    span: Span::new(tok.pos, tok.pos),
                    kind: ExprKind::Literal(val),
                })
            }
            TokenKind::String(s) => Ok(Expr {
                span: Span::new(tok.pos, tok.pos),
                kind: ExprKind::Literal(JsValue::String(JsString::from(s.as_str()))),
            }),
            TokenKind::LParen => {
                let expr = self.parse_expression(0)?;
                let end = self.consume(&TokenKind::RParen)?.pos;
                Ok(Expr {
                    span: Span::new(tok.pos, end),
                    kind: expr.kind,
                })
            }
            TokenKind::LBracket => {
                let mut items = Vec::new();
                if !matches!(self.peek().kind, TokenKind::RBracket) {
                    loop {
                        items.push(self.parse_expression(0)?);
                        if !self.matches(&TokenKind::Comma) {
                            break;
                        }
                    }
                }
                let end = self.consume(&TokenKind::RBracket)?.pos;
                Ok(Expr {
                    span: Span::new(tok.pos, end),
                    kind: ExprKind::Array(items),
                })
            }
            TokenKind::LBrace => {
                let mut props = Vec::new();
                if !matches!(self.peek().kind, TokenKind::RBrace) {
                    loop {
                        let key_tok = self.peek().clone();
                        let key = match key_tok.kind {
                            TokenKind::String(s) => {
                                self.bump();
                                ObjectKey::Static(s, Span::new(key_tok.pos, key_tok.pos))
                            }
                            TokenKind::Identifier(id) => {
                                self.bump();
                                ObjectKey::Static(id, Span::new(key_tok.pos, key_tok.pos))
                            }
                            TokenKind::Spread => {
                                self.bump();
                                ObjectKey::Spread(Box::new(self.parse_expression(0)?))
                            }
                            TokenKind::LBracket => {
                                self.bump();
                                let e = self.parse_expression(0)?;
                                self.consume(&TokenKind::RBracket)?;
                                ObjectKey::Expr(Box::new(e))
                            }
                            _ => {
                                return Err(ScriptError::with_pos(
                                    "unexpected token in object literal",
                                    key_tok.pos,
                                ));
                            }
                        };
                        let value = if matches!(key, ObjectKey::Spread(_)) {
                            Expr {
                                span: Span::new(key_tok.pos, key_tok.pos),
                                kind: ExprKind::Literal(JsValue::Null),
                            }
                        } else if self.matches(&TokenKind::Colon) {
                            self.parse_expression(0)?
                        } else {
                            let name = match &key {
                                ObjectKey::Static(n, _) => n.clone(),
                                _ => return Err(ScriptError::with_pos("invalid key", key_tok.pos)),
                            };
                            Expr {
                                span: Span::new(key_tok.pos, key_tok.pos),
                                kind: ExprKind::Identifier(name),
                            }
                        };
                        props.push((key, value));
                        if !self.matches(&TokenKind::Comma) {
                            break;
                        }
                    }
                }
                let end = self.consume(&TokenKind::RBrace)?.pos;
                Ok(Expr {
                    span: Span::new(tok.pos, end),
                    kind: ExprKind::Object(props),
                })
            }
            _ => Err(ScriptError::with_pos(
                format!("unexpected token {:?}", tok.kind),
                tok.pos,
            )),
        }
    }

    fn callee_from_expr(&self, expr: Expr) -> Result<CallTarget, ScriptError> {
        match expr.kind {
            ExprKind::Identifier(name) => Ok(CallTarget {
                span: expr.span,
                namespace: None,
                name,
            }),
            ExprKind::Member {
                object,
                property: MemberExpr::Named(name, nspan),
            } => match object.kind {
                ExprKind::Identifier(ns) => Ok(CallTarget {
                    span: Span::new(object.span.start, nspan.end),
                    namespace: Some(ns),
                    name,
                }),
                _ => Err(ScriptError::with_pos(
                    "invalid callee (must be identifier or ns.func)",
                    object.span.start,
                )),
            },
            _ => Err(ScriptError::with_pos(
                "invalid callee expression",
                expr.span.start,
            )),
        }
    }
}

fn infix_binding_power(op: BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Or => (1, 2),
        BinaryOp::And => (3, 4),
        BinaryOp::Eq | BinaryOp::Seq | BinaryOp::Ne | BinaryOp::Sne => (5, 6),
        BinaryOp::Lt
        | BinaryOp::Lte
        | BinaryOp::Gt
        | BinaryOp::Gte
        | BinaryOp::Match
        | BinaryOp::In => (7, 8),
        BinaryOp::Add | BinaryOp::Sub => (9, 10),
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => (11, 12),
    }
}
