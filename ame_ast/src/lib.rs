use ame_lexer::{LiteralKind, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    VarDecl {
        name: String,
        ty: Option<String>,
        init_expr: Option<Expr>,
    },
    If {
        branches: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    FnDecl {
        name: String,
        args: Vec<Stmt>,
        body: Option<Vec<Stmt>>,
        return_ty: Option<String>,
        is_extern: bool,
        is_variadic: bool,
    },
    Return(Option<Expr>),
    ExprStmt(Expr),
    For {
        init: Option<Box<Vec<Stmt>>>,
        cond: Option<Expr>,
        action: Option<Expr>,
        body: Vec<Stmt>,
    },
}

impl Expr {
    #[inline]
    pub fn into_stmt(self) -> Stmt {
        Stmt {
            kind: StmtKind::ExprStmt(self),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Literal(LiteralKind),
    Variable(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Assign(AssignOp, Box<Expr>, Box<Expr>),
    FnCall(String, Vec<Expr>),
    Cast(String, Box<Expr>),
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
