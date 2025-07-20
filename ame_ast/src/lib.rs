use ame_lexer::{LiteralKind, Token, TokenKind};

use ame_common::{Arena, Id};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StmtId(Id<Stmt>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExprId(Id<Expr>);

#[derive(Debug, Clone, Default)]
pub struct Ast {
    stmts: Arena<Stmt>,
    exprs: Arena<Expr>,
}

impl Ast {
    #[inline]
    pub fn new() -> Self {
        Self {
            stmts: Arena::new(),
            exprs: Arena::new(),
        }
    }

    #[inline]
    pub fn alloc_stmt(&mut self, stmt: Stmt) -> StmtId {
        StmtId(self.stmts.alloc(stmt))
    }

    #[inline]
    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        ExprId(self.exprs.alloc(expr))
    }
}

impl std::ops::Index<StmtId> for Ast {
    type Output = Stmt;

    #[inline]
    fn index(&self, index: StmtId) -> &Self::Output {
        &self.stmts[index.0]
    }
}

impl std::ops::Index<ExprId> for Ast {
    type Output = Expr;

    #[inline]
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index.0]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    kind: StmtKind,
}

impl Stmt {
    #[inline]
    pub const fn new(kind: StmtKind) -> Self {
        Self { kind }
    }

    #[inline]
    pub const fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    kind: ExprKind,
}

impl Expr {
    #[inline]
    pub const fn new(kind: ExprKind) -> Self {
        Self { kind }
    }

    #[inline]
    pub const fn kind(&self) -> &ExprKind {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    VarDecl {
        name: String,
        ty: Option<String>,
        init_expr: Option<ExprId>,
    },
    If {
        branches: Vec<(ExprId, Vec<StmtId>)>,
        else_body: Option<Vec<StmtId>>,
    },
    While {
        cond: ExprId,
        body: Vec<StmtId>,
    },
    FnDecl {
        name: String,
        args: Vec<StmtId>,
        body: Option<Vec<StmtId>>,
        return_ty: Option<String>,
        is_extern: bool,
        is_variadic: bool,
    },
    Return(Option<ExprId>),
    ExprStmt(ExprId),
    For {
        init: Option<Box<Vec<StmtId>>>,
        cond: Option<ExprId>,
        action: Option<ExprId>,
        body: Vec<StmtId>,
    },
    ClassDecl {
        name: String,
        fields: Vec<(String, String)>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Literal(LiteralKind),
    Variable(String),
    Unary(UnaryOp, ExprId),
    Binary(BinOp, ExprId, ExprId),
    Assign(AssignOp, ExprId, ExprId),
    FnCall(String, Vec<ExprId>),
    Cast(String, ExprId),
    ClassInst(String, Vec<(String, ExprId)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

impl TryFrom<&Token> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind {
            TokenKind::Plus => Ok(BinOp::Add),
            TokenKind::Minus => Ok(BinOp::Sub),
            TokenKind::Asterisk => Ok(BinOp::Mul),
            TokenKind::Slash => Ok(BinOp::Div),
            TokenKind::Percent => Ok(BinOp::Rem),
            TokenKind::Pipe => Ok(BinOp::BitOr),
            TokenKind::Amp => Ok(BinOp::BitAnd),
            TokenKind::Caret => Ok(BinOp::BitXor),
            TokenKind::Eq => Ok(BinOp::Eq),
            TokenKind::Ne => Ok(BinOp::Ne),
            TokenKind::Le => Ok(BinOp::Le),
            TokenKind::Lt => Ok(BinOp::Lt),
            TokenKind::Ge => Ok(BinOp::Ge),
            TokenKind::Gt => Ok(BinOp::Gt),
            TokenKind::Or => Ok(BinOp::Or),
            TokenKind::And => Ok(BinOp::And),
            TokenKind::Shl => Ok(BinOp::Shl),
            TokenKind::Shr => Ok(BinOp::Shr),

            ref t => todo!("invalid token -> bin op: {t:?}"),
        }
    }
}

impl TryFrom<Token> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        (&token).try_into()
    }
}

// TODO: somehow don't repeat here?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
}

impl TryFrom<&Token> for AssignOp {
    type Error = ();

    #[inline]
    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind {
            TokenKind::Assign => Ok(AssignOp::Assign),
            TokenKind::PlusAssign => Ok(AssignOp::Add),
            TokenKind::MinusAssign => Ok(AssignOp::Sub),
            TokenKind::AsteriskAssign => Ok(AssignOp::Mul),
            TokenKind::SlashAssign => Ok(AssignOp::Div),
            TokenKind::PercentAssign => Ok(AssignOp::Rem),
            TokenKind::PipeAssign => Ok(AssignOp::BitOr),
            TokenKind::AmpAssign => Ok(AssignOp::BitAnd),
            TokenKind::CaretAssign => Ok(AssignOp::BitXor),
            TokenKind::ShlAssign => Ok(AssignOp::Shl),
            TokenKind::ShrAssign => Ok(AssignOp::Shr),

            ref t => panic!("invalid token -> assign op: {t:?}"),
        }
    }
}

impl TryFrom<Token> for AssignOp {
    type Error = ();

    #[inline]
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        (&token).try_into()
    }
}

impl TryFrom<&AssignOp> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(op: &AssignOp) -> Result<Self, Self::Error> {
        match op {
            AssignOp::Assign => Err(()),

            AssignOp::Add => Ok(BinOp::Add),
            AssignOp::Sub => Ok(BinOp::Sub),
            AssignOp::Mul => Ok(BinOp::Mul),
            AssignOp::Div => Ok(BinOp::Div),
            AssignOp::Rem => Ok(BinOp::Rem),
            AssignOp::Shl => Ok(BinOp::Shl),
            AssignOp::Shr => Ok(BinOp::Shr),
            AssignOp::BitOr => Ok(BinOp::BitOr),
            AssignOp::BitAnd => Ok(BinOp::BitAnd),
            AssignOp::BitXor => Ok(BinOp::BitXor),
        }
    }
}

impl TryFrom<AssignOp> for BinOp {
    type Error = ();

    #[inline]
    fn try_from(op: AssignOp) -> Result<Self, Self::Error> {
        (&op).try_into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
    Deref,
}

impl TryFrom<&Token> for UnaryOp {
    type Error = ();

    #[inline]
    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind {
            TokenKind::Minus => Ok(UnaryOp::Neg),
            TokenKind::Bang => Ok(UnaryOp::Not),
            TokenKind::Amp => Ok(UnaryOp::Ref),
            TokenKind::Asterisk => Ok(UnaryOp::Deref),

            ref t => todo!("invalid token -> unary op: {t:?}"),
        }
    }
}

impl TryFrom<Token> for UnaryOp {
    type Error = ();

    #[inline]
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        (&token).try_into()
    }
}
