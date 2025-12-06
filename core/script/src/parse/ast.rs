use fiber_json::JsValue;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn union(self, other: Span) -> Self {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub(crate) enum StmtKind {
    Block(Block),
    Let(Vec<Binding>),
    Expr(Expr),
    If {
        cond: Expr,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    ForOf {
        key: Option<String>,
        value: String,
        iter: Expr,
        body: Block,
    },
    Break,
    Continue,
    Return(Option<Expr>),
    Directive {
        name: String,
        ty: String,
        literal: String,
    },
    TryCatch {
        body: Block,
        err: String,
        catch: Block,
    },
    Throw(Expr),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub span: Span,
    pub ty: BlockType,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BlockType {
    Script,
    Try,
    Catch,
    For,
    If,
    Else,
}

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub name: String,
    pub init: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub(crate) enum ExprKind {
    Literal(JsValue),
    Identifier(String),
    Object(Vec<(ObjectKey, Expr)>),
    Array(Vec<Expr>),
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Conditional {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Call {
        callee: CallTarget,
        args: Vec<Expr>,
    },
    Member {
        object: Box<Expr>,
        property: MemberExpr,
    },
    Root,
    Const {
        namespace: String,
        key: String,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum MemberExpr {
    Named(String, Span),
    Computed(Box<Expr>),
}

#[derive(Debug, Clone)]
pub(crate) struct CallTarget {
    pub span: Span,
    pub namespace: Option<String>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub(crate) enum ObjectKey {
    Static(String, Span),
    Expr(Box<Expr>),
    Spread(Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Match,
    And,
    Or,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Seq,
    Ne,
    Sne,
    In,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnaryOp {
    Not,
    Pos,
    Neg,
    Typeof,
}
