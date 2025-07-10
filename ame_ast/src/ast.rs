use ame_lexer::{LiteralKind, Token, TokenKind};
use ame_types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    VarDecl(VarDecl),
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
        return_ty: Type,
        is_extern: bool,
        is_variadic: bool,
    },
    Return(Option<Expr>),
    ExprStmt(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Literal(LiteralKind),
    Variable(String),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Assign {
        op: AssignOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    FnCall(String, Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDecl {
    pub name: String,
    pub ty: Type,
    pub init_expr: Option<Expr>,
}

// TODO: somehow don't repeat here?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
