use ame_lexer::LiteralKind;
use ame_types::Type;

mod inferrer;
pub use ame_ast::{AssignOp, BinOp};
pub use inferrer::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedExprKind {
    Literal(LiteralKind),
    Variable(String),
    Binary(BinOp, Box<TypedExpr>, Box<TypedExpr>),
    Assign(AssignOp, Box<TypedExpr>, Box<TypedExpr>),
    FnCall(String, Vec<TypedExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedStmt {
    pub kind: TypedStmtKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedStmtKind {
    VarDecl {
        name: String,
        ty: Type,
        init_expr: Option<TypedExpr>,
    },
    If {
        branches: Vec<(TypedExpr, Vec<TypedStmt>)>,
        else_body: Option<Vec<TypedStmt>>,
    },
    While {
        cond: TypedExpr,
        body: Vec<TypedStmt>,
    },
    FnDecl {
        name: String,
        args: Vec<TypedStmt>,
        body: Option<Vec<TypedStmt>>,
        return_ty: Type,

        is_extern: bool,
        is_variadic: bool,
    },
    Return(Option<TypedExpr>),
    ExprStmt(TypedExpr),
}
