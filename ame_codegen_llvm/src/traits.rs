use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    context::Context,
    types::{AnyTypeEnum, BasicTypeEnum},
};

use ame_codegen::CodeGenCtx;
use ame_common::Interned;
use ame_tast::BinOp;
use ame_types::{Constraint, FloatKind, IntKind, Type};

use crate::backend::LLVMBackend;

pub trait AsLLVMType<'ctx> {
    fn as_any_llvm_type(
        &self,
        context: &'ctx Context,
        ctx: &mut CodeGenCtx<'_, LLVMBackend<'ctx>>,
    ) -> AnyTypeEnum<'ctx>;
    fn as_basic_llvm_type(
        &self,
        context: &'ctx Context,
        ctx: &mut CodeGenCtx<'_, LLVMBackend<'ctx>>,
    ) -> BasicTypeEnum<'ctx> {
        self.as_any_llvm_type(context, ctx)
            .try_into()
            .expect("not a basic type")
    }
}

impl<'ctx> AsLLVMType<'ctx> for Interned<Type> {
    #[inline]
    fn as_any_llvm_type(
        &self,
        context: &'ctx Context,
        ctx: &mut CodeGenCtx<'_, LLVMBackend<'ctx>>,
    ) -> AnyTypeEnum<'ctx> {
        let ty = ctx.tcx.resolve(*self);

        match ty {
            Type::Int(kind) => match kind {
                IntKind::Int8 | IntKind::Uint8 => context.i8_type().into(),
                IntKind::Int16 | IntKind::Uint16 => context.i16_type().into(),
                IntKind::Int32 | IntKind::Uint32 => context.i32_type().into(),
                IntKind::Int64 | IntKind::Uint64 => context.i64_type().into(),
            },
            Type::Float(kind) => match kind {
                FloatKind::Float32 => context.f32_type().into(),
                FloatKind::Float64 => context.f64_type().into(),
            },
            Type::Bool => context.bool_type().into(),
            Type::Var(_, Constraint::Integer | Constraint::SignedInteger) => {
                context.i32_type().into()
            }
            Type::Var(_, Constraint::Float) => context.f64_type().into(),
            Type::String => context.ptr_type(AddressSpace::default()).into(), // temporary for now
            Type::Ref(_) => context.ptr_type(AddressSpace::default()).into(),
            // Type::Fn(id) => {}
            Type::Class(id) => {
                let name = ctx.tcx.get_def(*id).clone().as_class().name;

                (*ctx.classes.get(name.as_str()).expect("no class")).into()
            }
            Type::None => context.void_type().into(),

            other => panic!("cannot lower {other:?} to llvm type"),
        }
    }
}

pub trait AsIntPredicate {
    fn as_int_predicate(&self, signed: bool) -> IntPredicate;
}

impl AsIntPredicate for BinOp {
    #[inline]
    fn as_int_predicate(&self, unsigned: bool) -> IntPredicate {
        match self {
            BinOp::Eq => IntPredicate::EQ,
            BinOp::Ne => IntPredicate::NE,
            BinOp::Le => {
                if unsigned {
                    IntPredicate::ULE
                } else {
                    IntPredicate::SLE
                }
            }
            BinOp::Lt => {
                if unsigned {
                    IntPredicate::ULT
                } else {
                    IntPredicate::SLT
                }
            }
            BinOp::Ge => {
                if unsigned {
                    IntPredicate::UGE
                } else {
                    IntPredicate::SGE
                }
            }
            BinOp::Gt => {
                if unsigned {
                    IntPredicate::UGT
                } else {
                    IntPredicate::SGT
                }
            }

            o => unreachable!("binary operation {o:?} doesn't have correspoding int predicate"),
        }
    }
}

pub trait AsFloatPredicate {
    fn as_float_predicate(&self) -> FloatPredicate;
}

impl AsFloatPredicate for BinOp {
    #[inline]
    fn as_float_predicate(&self) -> FloatPredicate {
        match self {
            BinOp::Eq => FloatPredicate::OEQ,
            BinOp::Ne => FloatPredicate::ONE,
            BinOp::Le => FloatPredicate::OLE,
            BinOp::Lt => FloatPredicate::OLT,
            BinOp::Ge => FloatPredicate::OGE,
            BinOp::Gt => FloatPredicate::OGT,

            o => unreachable!("binary operation {o:?} doesn't have correspoding int predicate"),
        }
    }
}
