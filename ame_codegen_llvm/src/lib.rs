use std::{collections::HashMap, path::PathBuf};

pub use inkwell::context::Context;
use inkwell::{
    builder::Builder,
    module::Module,
    targets::{Target, TargetMachine, TargetTriple},
    values::{BasicValue, BasicValueEnum, FunctionValue},
    IntPredicate,
};

use ame_ast::{AssignOp, BinOp, Expr, ExprKind, Stmt, StmtKind, VarDecl};
use ame_lexer::LiteralKind;
use ame_types::{Type, TypeCtx};

mod options;
mod ty;

use crate::ty::AsLLVMType;
pub use options::*;

pub struct CodeGen<'a, 'ctx> {
    ast: &'a [Stmt],
    tcx: TypeCtx,

    locals: HashMap<&'a String, BasicValueEnum<'ctx>>,

    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    #[inline]
    pub fn new(
        ast: &'a [Stmt],
        tcx: TypeCtx,

        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
    ) -> Self {
        Self {
            ast,
            tcx,

            locals: HashMap::new(),

            context,
            module,
            builder,
        }
    }

    pub fn generate(&mut self, options: CodeGenOptions) {
        let i32_type = self.context.i32_type();

        let main_fn_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);

        let main_block = self.context.append_basic_block(main_fn, "main");
        self.builder.position_at_end(main_block);

        self.generate_stmts(self.ast, main_fn);

        // NOTE: only for testing purposes while there's no print or something
        // prints out all variable name-value pairs at the end of the program
        // FIXME: add support for floats/bools/etc.
        {
            let printf_fn_type = i32_type.fn_type(&[self.context.ptr_type(0.into()).into()], true);
            let printf_fn = self.module.add_function(
                "printf",
                printf_fn_type,
                Some(inkwell::module::Linkage::External),
            );

            let fmt_str = self.context.const_string(b"%s: %d\n", true);
            let fmt_str_global = self.module.add_global(fmt_str.get_type(), None, "fmt");
            fmt_str_global.set_initializer(&fmt_str);
            fmt_str_global.set_constant(true);
            fmt_str_global.set_linkage(inkwell::module::Linkage::Private);

            let fmt_str_ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    fmt_str.get_type(),
                    fmt_str_global.as_pointer_value(),
                    &[i32_type.const_zero(), i32_type.const_zero()],
                    "fmt_str_ptr",
                )
            }
            .unwrap();

            for (name, val) in &self.locals {
                let var_name = self.context.const_string(name.as_bytes(), true);
                let var_name_global = self.module.add_global(var_name.get_type(), None, "name");
                var_name_global.set_initializer(&var_name);
                var_name_global.set_constant(true);
                var_name_global.set_linkage(inkwell::module::Linkage::Private);

                let var_name_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        var_name.get_type(),
                        var_name_global.as_pointer_value(),
                        &[i32_type.const_zero(), i32_type.const_zero()],
                        "var_name_ptr",
                    )
                }
                .unwrap();

                let var = self
                    .builder
                    .build_load(i32_type, val.into_pointer_value(), "load")
                    .unwrap();

                self.builder
                    .build_call(
                        printf_fn,
                        &[fmt_str_ptr.into(), var_name_ptr.into(), var.into()],
                        "printf",
                    )
                    .unwrap();
            }
        }

        self.builder
            .build_return(Some(&i32_type.const_zero()))
            .unwrap();

        Target::initialize_all(&inkwell::targets::InitializationConfig::default());

        let target_triple = options
            .target
            .as_ref()
            .and_then(|target| Some(TargetTriple::create(target)))
            .unwrap_or_else(|| TargetMachine::get_default_triple());

        let cpu = options
            .target
            .unwrap_or_else(|| TargetMachine::get_host_cpu_name().to_string());

        let features = options
            .features
            .and_then(|features| {
                Some(if features == "native" {
                    TargetMachine::get_host_cpu_features().to_string()
                } else {
                    features
                })
            })
            .unwrap_or_else(|| "+cmov,+cx8,+fxsr,+mmx,+sse,+sse2".into());

        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                if options.optimize {
                    inkwell::OptimizationLevel::Aggressive
                } else {
                    inkwell::OptimizationLevel::None
                },
                inkwell::targets::RelocMode::PIC, // NOTE: needs -fPIE if using ::Default
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();

        self.module.set_triple(&target_triple);
        self.module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());

        if options.optimize {
            self.module.run_passes("annotation2metadata,forceattrs,inferattrs,function<eager-inv>(lower-expect,simplifycfg,early-cse),openmp-opt,ipsccp,called-value-propagation,globalopt,function<eager-inv>(instcombine<no-verify-fixpoint>,simplifycfg),always-inline,require<globals-aa>,function(invalidate<aa>),require<profile-summary>,cgscc(devirt<4>(inline,function-attrs,openmp-opt-cgscc,function<eager-inv;no-rerun>(early-cse<memssa>,speculative-execution,jump-threading,correlated-propagation,simplifycfg,instcombine<no-verify-fixpoint>,aggressive-instcombine,libcalls-shrinkwrap,tailcallelim,simplifycfg,reassociate,constraint-elimination,loop-mssa(loop-instsimplify,loop-simplifycfg,licm<no-allowspeculation>,loop-rotate<header-duplication;no-prepare-for-lto>,licm<allowspeculation>,simple-loop-unswitch<no-nontrivial;trivial>),simplifycfg,instcombine<no-verify-fixpoint>,loop(loop-idiom,indvars,loop-deletion,loop-unroll-full),vector-combine,mldst-motion<no-split-footer-bb>,sccp,bdce,instcombine<no-verify-fixpoint>,jump-threading,correlated-propagation,adce,memcpyopt,dse,move-auto-init,loop-mssa(licm<allowspeculation>),simplifycfg,instcombine<no-verify-fixpoint>),function-attrs,function(require<should-not-run-function-passes>))),deadargelim,globalopt,globaldce,elim-avail-extern,rpo-function-attrs,recompute-globalsaa,function<eager-inv>(lower-constant-intrinsics,loop(loop-rotate<header-duplication;no-prepare-for-lto>,loop-deletion),loop-distribute,inject-tli-mappings,loop-vectorize<no-interleave-forced-only;no-vectorize-forced-only>,infer-alignment,loop-load-elim,instcombine<no-verify-fixpoint>,simplifycfg,slp-vectorizer,vector-combine,instcombine<no-verify-fixpoint>,loop-unroll<O2>,transform-warning,loop-mssa(licm<allowspeculation>),infer-alignment,alignment-from-assumptions,loop-sink,instsimplify,div-rem-pairs,tailcallelim,simplifycfg),globaldce,constmerge,cg-profile,rel-lookup-table-converter,function(annotation-remarks),verify", &target_machine, inkwell::passes::PassBuilderOptions::create()).unwrap();
        }

        let output = options.output.unwrap_or_else(|| {
            "out"
                .parse::<PathBuf>()
                .unwrap()
                .with_extension(options.output_kind.extension())
        });
        match options.output_kind {
            OutputKind::Object => {
                target_machine
                    .write_to_file(&self.module, inkwell::targets::FileType::Object, &output)
                    .unwrap();
            }
            OutputKind::LLVMIr => {
                std::fs::write(output, self.module.to_string()).unwrap();
            }
            OutputKind::Assembly => {
                target_machine
                    .write_to_file(&self.module, inkwell::targets::FileType::Assembly, &output)
                    .unwrap();
            }
        }
    }

    fn generate_stmts(&mut self, stmts: &'a [Stmt], func: FunctionValue<'ctx>) {
        for stmt in stmts {
            match &stmt.kind {
                StmtKind::VarDecl(VarDecl {
                    name,
                    ty,
                    init_expr,
                }) => {
                    let var = self
                        .builder
                        .build_alloca(ty.as_llvm_type(self.context), &name)
                        .unwrap();

                    if let Some(init_expr) = init_expr {
                        let expr = self.generate_expr(init_expr);

                        self.builder.build_store(var, expr).unwrap();
                    }

                    self.locals.insert(name, var.as_basic_value_enum());
                }
                StmtKind::If {
                    branches,
                    else_body,
                } => self.generate_if(func, branches, else_body),
                StmtKind::While { cond, body } => {
                    let cond_generated = self.generate_expr(cond);
                    let cmp = cond_generated.into_int_value();

                    let do_block = self.context.append_basic_block(func, "do");
                    let whilecont_block = self.context.append_basic_block(func, "whilecont");

                    self.builder
                        .build_conditional_branch(cmp, do_block, whilecont_block)
                        .unwrap();

                    self.builder.position_at_end(do_block);
                    self.generate_stmts(&body, func);

                    let cond_generated = self.generate_expr(cond);
                    let cmp = cond_generated.into_int_value();

                    self.builder
                        .build_conditional_branch(cmp, do_block, whilecont_block)
                        .unwrap();

                    self.builder.position_at_end(whilecont_block);
                }
                StmtKind::ExprStmt(expr) => {
                    self.generate_expr(expr);
                }
            }
        }
    }

    fn generate_if(
        &mut self,
        func: FunctionValue<'ctx>,
        branches: &'a Vec<(Expr, Vec<Stmt>)>,
        else_body: &'a Option<Vec<Stmt>>,
    ) {
        let Some(((cond, body), rest)) = branches.split_last() else {
            return;
        };

        for (cond, body) in rest {
            let cond = self.generate_expr(cond);
            let cmp = cond.into_int_value();

            let then_block = self.context.append_basic_block(func, "then");
            let ifcont_block = self.context.append_basic_block(func, "ifcont");

            self.builder
                .build_conditional_branch(cmp, then_block, ifcont_block)
                .unwrap();

            self.builder.position_at_end(then_block);
            self.generate_stmts(&body, func);
            self.builder
                .build_unconditional_branch(ifcont_block)
                .unwrap();

            self.builder.position_at_end(ifcont_block);
        }

        let cond = self.generate_expr(cond);
        let cmp = cond.into_int_value();

        let then_block = self.context.append_basic_block(func, "then");
        let else_block = if else_body.is_some() {
            Some(self.context.append_basic_block(func, "else"))
        } else {
            None
        };
        let ifcont_block = self.context.append_basic_block(func, "ifcont");

        self.builder
            .build_conditional_branch(cmp, then_block, else_block.unwrap_or(ifcont_block))
            .unwrap();

        self.builder.position_at_end(then_block);
        self.generate_stmts(&body, func);
        self.builder
            .build_unconditional_branch(ifcont_block)
            .unwrap();

        if let Some(else_body) = else_body {
            self.builder.position_at_end(else_block.unwrap());
            self.generate_stmts(&else_body, func);

            self.builder
                .build_unconditional_branch(ifcont_block)
                .unwrap();
        }

        self.builder.position_at_end(ifcont_block);
    }

    fn generate_expr(&mut self, expr: &Expr) -> BasicValueEnum<'ctx> {
        let ty = expr.ty.resolve(&self.tcx);
        let llvm_ty = ty.as_llvm_type(&self.context);

        match &expr.kind {
            ExprKind::Literal(kind) => match kind {
                LiteralKind::Int {
                    base,
                    empty: _,
                    value,
                } => self
                    .context
                    .i32_type()
                    .const_int(
                        i32::from_str_radix(value.trim(), *base as u32).unwrap() as u64,
                        false,
                    )
                    .into(),
                LiteralKind::Float {
                    base: _,
                    empty_exp: _,
                    value,
                } => self
                    .context
                    .f64_type()
                    .const_float(value.trim().parse().unwrap())
                    .into(),
                LiteralKind::String { .. } => {
                    todo!("add support for string literals in codegen")
                }
            },
            ExprKind::Variable(name) => self
                .builder
                .build_load(
                    llvm_ty,
                    self.locals.get(name).unwrap().into_pointer_value(),
                    "load",
                )
                .unwrap()
                .into(),
            ExprKind::Binary(op, lhs, rhs) => {
                let l = self.generate_expr(lhs);
                let r = self.generate_expr(rhs);

                match ty {
                    Type::Int(_) => match op {
                        // add uint handling for div/rem
                        BinOp::Add => self.builder.build_int_add(
                            l.into_int_value(),
                            r.into_int_value(),
                            "add",
                        ),
                        BinOp::Sub => self.builder.build_int_sub(
                            l.into_int_value(),
                            r.into_int_value(),
                            "sub",
                        ),
                        BinOp::Mul => self.builder.build_int_mul(
                            l.into_int_value(),
                            r.into_int_value(),
                            "mul",
                        ),
                        BinOp::Div => self.builder.build_int_signed_div(
                            l.into_int_value(),
                            r.into_int_value(),
                            "div",
                        ),
                        BinOp::Rem => self.builder.build_int_signed_rem(
                            l.into_int_value(),
                            r.into_int_value(),
                            "rem",
                        ),
                        _ => todo!("add more binops for ints in codegen"),
                    }
                    .unwrap()
                    .into(),
                    Type::Float(_) => match op {
                        BinOp::Add => self.builder.build_float_add(
                            l.into_float_value(),
                            r.into_float_value(),
                            "add",
                        ),
                        BinOp::Sub => self.builder.build_float_sub(
                            l.into_float_value(),
                            r.into_float_value(),
                            "sub",
                        ),
                        BinOp::Mul => self.builder.build_float_mul(
                            l.into_float_value(),
                            r.into_float_value(),
                            "mul",
                        ),
                        BinOp::Div => self.builder.build_float_div(
                            l.into_float_value(),
                            r.into_float_value(),
                            "div",
                        ),
                        BinOp::Rem => self.builder.build_float_rem(
                            l.into_float_value(),
                            r.into_float_value(),
                            "rem",
                        ),
                        _ => todo!("add more binops for floats in codegen"),
                    }
                    .unwrap()
                    .into(),
                    Type::Bool => match op {
                        // TODO: add support for floats here
                        BinOp::Eq | BinOp::Ne | BinOp::Le | BinOp::Lt | BinOp::Ge | BinOp::Gt => {
                            self.builder
                                .build_int_compare(
                                    binop_to_int_predicate(op),
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "cmp",
                                )
                                .unwrap()
                                .into()
                        }
                        _ => todo!("add more binops for bool in codegen"),
                    },
                    ty => todo!("add binops for {ty:?} in codegen"),
                }
            }
            ExprKind::Assign { op, lhs, rhs } => {
                let ptr = match &lhs.kind {
                    ExprKind::Variable(name) => self.locals.get(&name).unwrap().clone(),
                    _ => unreachable!("assignment lhs can only be variable"),
                };

                let ty = lhs.ty.resolve(&self.tcx);

                match op {
                    AssignOp::Assign => {
                        let rhs = self.generate_expr(rhs);

                        self.builder
                            .build_store(ptr.into_pointer_value(), rhs)
                            .unwrap();

                        ptr
                    }
                    AssignOp::Add | AssignOp::Sub | AssignOp::Mul | AssignOp::Div => {
                        let l = self
                            .builder
                            .build_load(
                                ty.as_llvm_type(self.context),
                                ptr.into_pointer_value(),
                                "load_lhs",
                            )
                            .unwrap();
                        let r = self.generate_expr(rhs);

                        let result: BasicValueEnum<'_> = match ty {
                            Type::Int(_) => match op {
                                // add uint handling for div/rem
                                AssignOp::Add => self.builder.build_int_add(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "add",
                                ),
                                AssignOp::Sub => self.builder.build_int_sub(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "sub",
                                ),
                                AssignOp::Mul => self.builder.build_int_mul(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "mul",
                                ),
                                AssignOp::Div => self.builder.build_int_signed_div(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "div",
                                ),
                                AssignOp::Rem => self.builder.build_int_signed_rem(
                                    l.into_int_value(),
                                    r.into_int_value(),
                                    "rem",
                                ),
                                _ => todo!("add more binops for ints in codegen"),
                            }
                            .unwrap()
                            .into(),
                            Type::Float(_) => match op {
                                AssignOp::Add => self.builder.build_float_add(
                                    l.into_float_value(),
                                    r.into_float_value(),
                                    "add",
                                ),
                                AssignOp::Sub => self.builder.build_float_sub(
                                    l.into_float_value(),
                                    r.into_float_value(),
                                    "sub",
                                ),
                                AssignOp::Mul => self.builder.build_float_mul(
                                    l.into_float_value(),
                                    r.into_float_value(),
                                    "mul",
                                ),
                                AssignOp::Div => self.builder.build_float_div(
                                    l.into_float_value(),
                                    r.into_float_value(),
                                    "div",
                                ),
                                AssignOp::Rem => self.builder.build_float_rem(
                                    l.into_float_value(),
                                    r.into_float_value(),
                                    "rem",
                                ),
                                _ => todo!("add more binops for floats in codegen"),
                            }
                            .unwrap()
                            .into(),
                            ty => todo!("add binops for {ty:?} in codegen"),
                        };

                        self.builder
                            .build_store(ptr.into_pointer_value(), result)
                            .unwrap();
                        ptr
                    }
                    _ => todo!(),
                }
            }
        }
    }
}

#[inline]
fn binop_to_int_predicate(op: &BinOp) -> IntPredicate {
    match op {
        BinOp::Eq => IntPredicate::EQ,
        BinOp::Ne => IntPredicate::NE,
        BinOp::Le => IntPredicate::SLE,
        BinOp::Lt => IntPredicate::SLT,
        BinOp::Ge => IntPredicate::SGE,
        BinOp::Gt => IntPredicate::SGT,

        _ => unreachable!("never called with non-existent op"),
    }
}
