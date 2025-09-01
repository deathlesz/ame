pub use ame_ast::{AssignOp, BinOp, UnaryOp};
use ame_common::{Arena, Id, Interned};
use ame_lexer::LiteralKind;
use ame_types::Type;

mod inferrer;

pub use crate::inferrer::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypedStmtId(Id<TypedStmt>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypedExprId(Id<TypedExpr>);

#[derive(Debug, Clone, Default)]
pub struct TypedAst {
    stmts: Arena<TypedStmt>,
    exprs: Arena<TypedExpr>,
}

impl TypedAst {
    #[inline]
    pub fn new() -> Self {
        Self {
            stmts: Arena::new(),
            exprs: Arena::new(),
        }
    }

    #[inline]
    pub fn alloc_stmt(&mut self, stmt: TypedStmt) -> TypedStmtId {
        TypedStmtId(self.stmts.alloc(stmt))
    }

    #[inline]
    pub fn alloc_expr(&mut self, expr: TypedExpr) -> TypedExprId {
        TypedExprId(self.exprs.alloc(expr))
    }
}

impl std::ops::Index<TypedStmtId> for TypedAst {
    type Output = TypedStmt;

    #[inline]
    fn index(&self, index: TypedStmtId) -> &Self::Output {
        &self.stmts[index.0]
    }
}

impl std::ops::Index<TypedExprId> for TypedAst {
    type Output = TypedExpr;

    #[inline]
    fn index(&self, index: TypedExprId) -> &Self::Output {
        &self.exprs[index.0]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedExpr {
    kind: TypedExprKind,
    ty: Interned<Type>,
}

impl TypedExpr {
    #[inline]
    pub fn kind(&self) -> &TypedExprKind {
        &self.kind
    }

    #[inline]
    pub fn ty(&self) -> Interned<Type> {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedExprKind {
    Literal(LiteralKind),
    Variable(String),
    Unary(UnaryOp, TypedExprId),
    Binary(BinOp, TypedExprId, TypedExprId),
    Assign(AssignOp, TypedExprId, TypedExprId),
    FnCall(String, Vec<TypedExprId>),
    Cast(Interned<Type>, TypedExprId),
    ClassInst(String, Vec<(String, TypedExprId)>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedStmt {
    kind: TypedStmtKind,
}

impl TypedStmt {
    #[inline]
    pub fn kind(&self) -> &TypedStmtKind {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypedStmtKind {
    VarDecl {
        name: String,
        ty: Interned<Type>,
        init_expr: Option<TypedExprId>,
    },
    If {
        branches: Vec<(TypedExprId, Vec<TypedStmtId>)>,
        else_body: Option<Vec<TypedStmtId>>,
    },
    While {
        cond: TypedExprId,
        body: Vec<TypedStmtId>,
    },
    FnDecl {
        name: String,
        args: Vec<TypedStmtId>,
        body: Option<Vec<TypedStmtId>>,
        return_ty: Interned<Type>,

        is_extern: bool,
        is_variadic: bool,
    },
    Return(Option<TypedExprId>),
    For {
        init: Option<Vec<TypedStmtId>>,
        cond: Option<TypedExprId>,
        action: Option<TypedExprId>,
        body: Vec<TypedStmtId>,
    },
    ClassDecl {
        name: String,
        fields: Vec<(String, Interned<Type>)>,
    },
    ExprStmt(TypedExprId),
}
