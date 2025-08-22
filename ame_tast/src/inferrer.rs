use ame_ast::{AssignOp, Ast, BinOp, ExprId, ExprKind, StmtId, StmtKind, UnaryOp};
use ame_common::{Interned, ScopeStack};
use ame_lexer::LiteralKind;
use ame_types::{Constraint, DefKind, Type, TypeCtx, TypeError};

use crate::{
    TypedAst, TypedExpr, TypedExprId, TypedExprKind, TypedStmt, TypedStmtId, TypedStmtKind,
};

type Result<T> = std::result::Result<T, TypeError>;

pub struct Inferrer<'a> {
    tcx: &'a mut TypeCtx,
    env: ScopeStack<String, Interned<Type>>,

    current_return_ty: Option<Interned<Type>>,

    ast: &'a Ast,
    typed_ast: TypedAst,
}

impl<'a> Inferrer<'a> {
    pub fn new(tcx: &'a mut TypeCtx, ast: &'a Ast) -> Self {
        Self {
            tcx,
            env: ScopeStack::new(),

            current_return_ty: None,

            ast,
            typed_ast: TypedAst::new(),
        }
    }

    pub fn infer(mut self, ast: &[StmtId]) -> Result<(TypedAst, Vec<TypedStmtId>)> {
        let stmts = self.infer_stmts(ast)?;

        Ok((self.typed_ast, stmts))
    }

    fn infer_stmts(&mut self, ast: &[StmtId]) -> Result<Vec<TypedStmtId>> {
        self.define_functions(ast)?;

        let mut typed_ast = Vec::with_capacity(ast.len());
        for stmt in ast {
            typed_ast.push(self.infer_stmt(*stmt)?)
        }

        Ok(typed_ast)
    }

    fn define_functions(&mut self, stmts: &[StmtId]) -> Result<()> {
        for stmt in stmts {
            let stmt = &self.ast[*stmt];

            if let StmtKind::FnDecl {
                name,
                args,
                return_ty,
                is_variadic,
                ..
            } = stmt.kind()
            {
                let mut arg_tys = Vec::with_capacity(args.len());
                for arg in args {
                    if let StmtKind::VarDecl { ty, .. } = self.ast[*arg].kind() {
                        let ty = Type::from_str(
                            ty.as_ref()
                                .expect("type must be present in function argument"),
                            self.tcx,
                        );
                        arg_tys.push(self.tcx.intern_type(ty));
                    }
                }

                let return_ty = return_ty
                    .as_ref()
                    .map(|ty| Type::from_str(ty, self.tcx))
                    .unwrap_or(Type::None);
                let return_ty = self.tcx.intern_type(return_ty);
                let fn_def_id = self
                    .tcx
                    .define_fn(name.clone(), arg_tys, return_ty, *is_variadic);

                self.env
                    .define(name.clone(), self.tcx.get_def_ty(fn_def_id));
            }
        }

        Ok(())
    }

    fn infer_stmt(&mut self, stmt: StmtId) -> Result<TypedStmtId> {
        let kind = match self.ast[stmt].kind() {
            StmtKind::VarDecl {
                name,
                ty,
                init_expr,
            } => {
                let typed_init_expr = init_expr
                    .as_ref()
                    .map(|expr| self.infer_expr(*expr))
                    .transpose()?;

                let inferred_ty = if let Some(init) = &typed_init_expr {
                    self.typed_ast[*init].ty
                } else {
                    let var = self.tcx.var();
                    self.tcx.intern_type(var)
                };

                let ty = ty
                    .as_ref()
                    .map(|ty| {
                        let ty = Type::from_str(ty, self.tcx);
                        self.tcx.intern_type(ty)
                    })
                    .unwrap_or(inferred_ty);

                self.tcx.unify(ty, inferred_ty)?;
                self.env.define(name.clone(), ty);

                TypedStmtKind::VarDecl {
                    name: name.clone(),
                    ty,
                    init_expr: typed_init_expr,
                }
            }
            StmtKind::FnDecl {
                name,
                args,
                body,
                return_ty,
                is_extern,
                is_variadic,
            } => {
                self.env.enter();

                let mut typed_args = Vec::with_capacity(args.len());
                for arg in args {
                    if let StmtKind::VarDecl { name, ty, .. } = &self.ast[*arg].kind() {
                        let ty = Type::from_str(
                            ty.as_ref()
                                .expect("type must be present in function argument"),
                            self.tcx,
                        );
                        let ty = self.tcx.intern_type(ty);
                        self.env.define(name.clone(), ty);

                        typed_args.push(self.infer_stmt(*arg)?);
                    }
                }

                let return_ty = return_ty
                    .as_ref()
                    .map(|ty| Type::from_str(ty, self.tcx))
                    .unwrap_or(Type::None);
                let return_ty = self.tcx.intern_type(return_ty);
                let typed_body = if let Some(stmts) = body {
                    let prev_return_ty = self.current_return_ty.replace(return_ty);

                    let typed_stmts = self.infer_stmts(stmts)?;

                    self.current_return_ty = prev_return_ty;
                    Some(typed_stmts)
                } else {
                    None
                };

                self.env.exit();

                TypedStmtKind::FnDecl {
                    name: name.clone(),
                    args: typed_args,
                    body: typed_body,
                    return_ty,
                    is_extern: *is_extern,
                    is_variadic: *is_variadic,
                }
            }
            StmtKind::Return(expr) => {
                let typed_expr = expr
                    .as_ref()
                    .map(|expr| self.infer_expr(*expr))
                    .transpose()?;
                let return_ty = typed_expr
                    .as_ref()
                    .map(|expr| self.typed_ast[*expr].ty)
                    .unwrap_or_else(|| self.tcx.intern_type(Type::None));

                if let Some(expected) = &self.current_return_ty {
                    self.tcx.unify(*expected, return_ty)?;
                }

                TypedStmtKind::Return(typed_expr)
            }
            StmtKind::If {
                branches,
                else_body,
            } => {
                let mut typed_branches = Vec::with_capacity(branches.len());

                for (cond, body) in branches {
                    let typed_cond = self.infer_expr(*cond)?;
                    let bool = self.tcx.intern_type(Type::Bool);
                    self.tcx.unify(self.typed_ast[typed_cond].ty, bool)?;

                    self.env.enter();
                    let typed_body = self.infer_stmts(body)?;
                    self.env.exit();

                    typed_branches.push((typed_cond, typed_body));
                }

                let typed_else_body = else_body
                    .as_ref()
                    .map(|stmts| {
                        self.env.enter();
                        let typed_body = self.infer_stmts(stmts);
                        self.env.exit();

                        typed_body
                    })
                    .transpose()?;

                TypedStmtKind::If {
                    branches: typed_branches,
                    else_body: typed_else_body,
                }
            }
            StmtKind::While { cond, body } => {
                let typed_cond = self.infer_expr(*cond)?;
                let bool = self.tcx.intern_type(Type::Bool);
                self.tcx.unify(self.typed_ast[typed_cond].ty, bool)?;

                self.env.enter();
                let typed_body = self.infer_stmts(body)?;
                self.env.exit();

                TypedStmtKind::While {
                    cond: typed_cond,
                    body: typed_body,
                }
            }
            StmtKind::For {
                init,
                cond,
                action,
                body,
            } => {
                self.env.enter();
                let typed_init = if let Some(stmts) = init {
                    let mut vec = Vec::with_capacity(stmts.len());

                    for stmt in stmts.iter() {
                        vec.push(self.infer_stmt(*stmt)?);
                    }

                    Some(vec)
                } else {
                    None
                };

                let typed_cond = if let Some(expr) = cond {
                    let typed_cond = self.infer_expr(*expr)?;
                    let bool = self.tcx.intern_type(Type::Bool);
                    self.tcx.unify(self.typed_ast[typed_cond].ty, bool)?;

                    Some(typed_cond)
                } else {
                    None
                };

                let typed_action = if let Some(expr) = action {
                    Some(self.infer_expr(*expr)?)
                } else {
                    None
                };

                let typed_body = self.infer_stmts(body)?;

                self.env.exit();

                TypedStmtKind::For {
                    init: typed_init,
                    cond: typed_cond,
                    action: typed_action,
                    body: typed_body,
                }
            }
            StmtKind::ClassDecl {
                name: _name,
                fields: _fields,
            } => todo!(),
            StmtKind::ExprStmt(expr) => TypedStmtKind::ExprStmt(self.infer_expr(*expr)?),
        };

        Ok(self.typed_ast.alloc_stmt(TypedStmt { kind }))
    }

    fn infer_expr(&mut self, expr: ExprId) -> Result<TypedExprId> {
        let (kind, ty) = match &self.ast[expr].kind() {
            ExprKind::Literal(kind) => {
                let ty = match kind {
                    LiteralKind::Int { .. } => self.tcx.var_int(),
                    LiteralKind::Float { .. } => self.tcx.var_float(),
                    LiteralKind::String { .. } => Type::String,
                };

                (
                    TypedExprKind::Literal(kind.clone()),
                    self.tcx.intern_type(ty),
                )
            }
            ExprKind::Variable(name) => {
                if let Some(ty) = self.env.get(name) {
                    (TypedExprKind::Variable(name.clone()), *ty)
                } else {
                    panic!("undefined variable")
                }
            }
            ExprKind::Unary(op, val) => {
                let typed_val = self.infer_expr(*val)?;

                let ty = self.typed_ast[typed_val].ty;
                let result_ty = match op {
                    UnaryOp::Neg => match self.tcx.get_ty(ty) {
                        Type::Int(kind) if !kind.unsigned() => ty,
                        Type::Var(_, Constraint::SignedInteger) => ty,
                        Type::Var(tid, Constraint::Integer) => self
                            .tcx
                            .intern_type(Type::Var(*tid, Constraint::SignedInteger)),
                        Type::Float(_) => ty,
                        Type::Var(_, Constraint::Float) => ty,
                        ty => panic!("cannot negate {ty:?}"),
                    },
                    UnaryOp::Not => {
                        match self.tcx.get_ty(ty) {
                            Type::Bool => {}
                            _ => panic!("cannot not"),
                        }

                        self.tcx.intern_type(Type::Bool)
                    }
                    UnaryOp::Ref => self.tcx.intern_type(Type::Ref(ty)),
                    UnaryOp::Deref => match self.tcx.get_ty(ty) {
                        Type::Ref(ty) => *ty,
                        _ => panic!("cannot deref"),
                    },
                };

                (TypedExprKind::Unary(*op, typed_val), result_ty)
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let typed_lhs = self.infer_expr(*lhs)?;
                let typed_rhs = self.infer_expr(*rhs)?;
                let typed_lhs_ty = self.typed_ast[typed_lhs].ty;
                let typed_rhs_ty = self.typed_ast[typed_rhs].ty;

                let result_ty = match op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Rem
                    | BinOp::BitAnd
                    | BinOp::BitXor
                    | BinOp::BitOr => {
                        self.tcx.unify(typed_lhs_ty, typed_rhs_ty)?;

                        typed_lhs_ty
                    }
                    BinOp::Shl | BinOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match self.tcx.get_ty(typed_lhs_ty) {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        typed_lhs_ty
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Ge | BinOp::Lt | BinOp::Le => {
                        self.tcx.unify(typed_lhs_ty, typed_rhs_ty)?;

                        self.tcx.intern_type(Type::Bool)
                    }
                    BinOp::And | BinOp::Or => {
                        let bool = self.tcx.intern_type(Type::Bool);

                        self.tcx.unify(typed_lhs_ty, bool)?;
                        self.tcx.unify(typed_rhs_ty, bool)?;

                        bool
                    }
                };

                (TypedExprKind::Binary(*op, typed_lhs, typed_rhs), result_ty)
            }
            ExprKind::Assign(op, lhs, rhs) => {
                let typed_lhs = self.infer_expr(*lhs)?;
                let typed_rhs = self.infer_expr(*rhs)?;
                let typed_lhs_ty = self.typed_ast[typed_lhs].ty;
                let typed_rhs_ty = self.typed_ast[typed_rhs].ty;

                let result_ty = match op {
                    AssignOp::Add
                    | AssignOp::Sub
                    | AssignOp::Mul
                    | AssignOp::Div
                    | AssignOp::Rem
                    | AssignOp::BitAnd
                    | AssignOp::BitXor
                    | AssignOp::BitOr => {
                        self.tcx.unify(typed_lhs_ty, typed_rhs_ty)?;

                        typed_lhs_ty
                    }
                    AssignOp::Shl | AssignOp::Shr => {
                        // TODO: add error for this or modify `TypeError::CannotUnify`
                        match self.tcx.get_ty(typed_lhs_ty) {
                            Type::Int(_) => {}
                            _ => panic!("you can shl/shr only by integer values"),
                        }

                        typed_lhs_ty
                    }
                    AssignOp::Assign => {
                        self.tcx.unify(typed_lhs_ty, typed_rhs_ty)?;

                        typed_lhs_ty
                    }
                };

                (TypedExprKind::Assign(*op, typed_lhs, typed_rhs), result_ty)
            }
            ExprKind::FnCall(name, args) => {
                if let Some(ty) = self.env.get(name) {
                    let ty = self.tcx.get_ty(*ty);
                    match ty {
                        Type::Fn(id) => {
                            let DefKind::Fn {
                                args: arg_tys,
                                return_ty,
                                ..
                            } = self.tcx.get_def(*id).clone();

                            let mut typed_args = Vec::with_capacity(args.len());

                            // required to still infer types of variadic arguments, but not unify them
                            for (arg, arg_ty) in args
                                .iter()
                                .zip(arg_tys.iter().map(Some).chain(std::iter::repeat(None)))
                            {
                                let typed_arg = self.infer_expr(*arg)?;

                                if let Some(arg_ty) = arg_ty {
                                    self.tcx.unify(self.typed_ast[typed_arg].ty, *arg_ty)?;
                                }

                                typed_args.push(typed_arg);
                            }

                            (TypedExprKind::FnCall(name.clone(), typed_args), return_ty)
                        }
                        _ => unreachable!("function type is Fn, duh"),
                    }
                } else {
                    panic!("function `{name}` is not defined");
                }
            }
            ExprKind::Cast(ty, expr) => {
                let ty = Type::from_str(ty, self.tcx);
                let ty = self.tcx.intern_type(ty);
                let typed_expr = self.infer_expr(*expr)?;

                (TypedExprKind::Cast(ty, typed_expr), ty)
            }
            ExprKind::ClassInst(_, _) => todo!(),
        };

        Ok(self.typed_ast.alloc_expr(TypedExpr { kind, ty }))
    }
}
