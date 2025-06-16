use std::collections::HashMap;

use ame_lexer::LiteralKind;
use ame_types::{unify, FloatKind, IntKind, Type, TypeCtx, TypeError};

use crate::{AssignOp, BinOp, Expr, ExprKind, Stmt, StmtKind};

type Result<T> = std::result::Result<T, TypeError>;

pub struct Inferrer<'a> {
    tcx: &'a mut TypeCtx,
    env: HashMap<String, Type>,
}

impl<'a> Inferrer<'a> {
    pub fn new(tcx: &'a mut TypeCtx) -> Self {
        Self {
            tcx,
            env: HashMap::new(),
        }
    }

    pub fn infer(&mut self, ast: &mut [Stmt]) -> Result<()> {
        for stmt in ast {
            self.infer_stmt(stmt)?;
        }

        Ok(())
    }

    pub fn resolve(&mut self, ast: &mut [Stmt]) {
        for stmt in ast {
            self.resolve_stmt(stmt);
        }
    }

    fn infer_stmt(&mut self, stmt: &mut Stmt) -> Result<()> {
        match &mut stmt.kind {
            StmtKind::VarDecl(decl) => {
                let inferred = if let Some(init) = &mut decl.init_expr {
                    self.infer_expr_type(init)?;
                    Some(init.ty.clone())
                } else {
                    None
                };

                if let Some(ty) = inferred {
                    if decl.ty == Type::Unknown {
                        decl.ty = ty.clone();
                    }

                    unify(&decl.ty, &ty, self.tcx)?;
                    self.env.insert(decl.name.clone(), ty.clone());
                }

                Ok(())
            }
            StmtKind::If {
                branches,
                else_body,
            } => {
                for (cond, body) in branches {
                    self.infer_expr_type(cond)?;
                    unify(&cond.ty, &Type::Bool, self.tcx)?;

                    for stmt in body {
                        self.infer_stmt(stmt)?;
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.infer_stmt(stmt)?;
                    }
                }

                Ok(())
            }
            StmtKind::While { cond, body } => {
                self.infer_expr_type(cond)?;
                unify(&cond.ty, &Type::Bool, self.tcx)?;
                for stmt in body {
                    self.infer_stmt(stmt)?;
                }
                Ok(())
            }
            StmtKind::ExprStmt(expr) => {
                self.infer_expr_type(expr)?;
                Ok(())
            }
        }
    }

    fn infer_expr_type(&mut self, expr: &mut Expr) -> Result<Type> {
        let ty = match &mut expr.kind {
            ExprKind::Literal(lit) => Self::infer_literal_type(lit),
            ExprKind::Variable(name) => self
                .env
                .entry(name.clone())
                .or_insert_with(|| Type::Var(self.tcx.next_id()))
                .clone(),
            ExprKind::Binary(op, lhs, rhs) => {
                let ltype = self.infer_expr_type(lhs)?;
                let rtype = self.infer_expr_type(rhs)?;

                match op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Rem
                    | BinOp::BitAnd
                    | BinOp::BitXor
                    | BinOp::BitOr => {
                        unify(&ltype, &rtype, self.tcx)?;

                        ltype
                    }
                    BinOp::Shl | BinOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match rtype {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        ltype
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Ge | BinOp::Lt | BinOp::Le => {
                        unify(&ltype, &rtype, self.tcx)?;

                        Type::Bool
                    }
                    BinOp::And | BinOp::Or => {
                        unify(&ltype, &Type::Bool, self.tcx)?;
                        unify(&rtype, &Type::Bool, self.tcx)?;

                        Type::Bool
                    }
                }
            }
            ExprKind::Assign { op, lhs, rhs } => {
                let ltype = self.infer_expr_type(lhs)?;
                let rtype = self.infer_expr_type(rhs)?;

                match op {
                    AssignOp::Add
                    | AssignOp::Sub
                    | AssignOp::Mul
                    | AssignOp::Div
                    | AssignOp::Rem
                    | AssignOp::BitAnd
                    | AssignOp::BitXor
                    | AssignOp::BitOr => {
                        unify(&ltype, &rtype, self.tcx)?;

                        ltype
                    }
                    AssignOp::Shl | AssignOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match rtype {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        ltype
                    }
                    AssignOp::Assign => {
                        unify(&ltype, &rtype, self.tcx)?;

                        ltype
                    }
                }
            }
        };

        expr.ty = ty.resolve(self.tcx);
        Ok(ty)
    }

    fn infer_literal_type(lit: &LiteralKind) -> Type {
        match lit {
            LiteralKind::Int { .. } => Type::Int(IntKind::default()),
            // LiteralKind::Bool(_) => Type::Bool,
            LiteralKind::Float { .. } => Type::Float(FloatKind::default()),
            LiteralKind::String { .. } => Type::String,
        }
    }

    fn resolve_stmt(&self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::VarDecl(var_decl) => {
                if let Some(expr) = &mut var_decl.init_expr {
                    self.resolve_expr(expr);
                } else {
                    var_decl.ty = self
                        .env
                        .get(&var_decl.name)
                        .cloned()
                        .unwrap_or(Type::Unknown)
                        .resolve(self.tcx);
                }
            }
            StmtKind::ExprStmt(expr) => self.resolve_expr(expr),
            StmtKind::If {
                branches,
                else_body,
            } => {
                for (cond, body) in branches {
                    self.resolve_expr(cond);
                    for stmt in body {
                        self.resolve_stmt(stmt);
                    }
                }
                if let Some(body) = else_body {
                    for stmt in body {
                        self.resolve_stmt(stmt);
                    }
                }
            }
            StmtKind::While { cond, body } => {
                self.resolve_expr(cond);
                for stmt in body {
                    self.resolve_stmt(stmt);
                }
            }
        }
    }

    fn resolve_expr(&self, expr: &mut Expr) {
        expr.ty = expr.ty.resolve(self.tcx);

        match &mut expr.kind {
            ExprKind::Binary(_, lhs, rhs) => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            ExprKind::Assign { lhs, rhs, .. } => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }
            _ => {}
        }
    }
}
