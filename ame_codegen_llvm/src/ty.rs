use ame_tast::BinOp;
use ame_types::{Constraint, FloatKind, IntKind, Type};
use inkwell::{context::Context, types::AnyTypeEnum, AddressSpace, FloatPredicate, IntPredicate};

pub trait AsLLVMType<'ctx> {
    type Out;

    fn as_llvm_type(&self, ctx: &'ctx Context) -> Self::Out;
}

impl<'ctx> AsLLVMType<'ctx> for Type {
    type Out = AnyTypeEnum<'ctx>;

    #[inline]
    fn as_llvm_type(&self, ctx: &'ctx Context) -> Self::Out {
        match self {
            Self::Int(kind) => match kind {
                IntKind::Int8 | IntKind::Uint8 => ctx.i8_type().into(),
                IntKind::Int16 | IntKind::Uint16 => ctx.i16_type().into(),
                IntKind::Int32 | IntKind::Uint32 => ctx.i32_type().into(),
                IntKind::Int64 | IntKind::Uint64 => ctx.i64_type().into(),
            },
            Self::Float(kind) => match kind {
                FloatKind::Float32 => ctx.f32_type().into(),
                FloatKind::Float64 => ctx.f64_type().into(),
            },
            Self::Bool => ctx.bool_type().into(),
            Self::Var(_, Constraint::Integer | Constraint::SignedInteger) => ctx.i32_type().into(),
            Self::Var(_, Constraint::Float) => ctx.f64_type().into(),
            Self::String => ctx.ptr_type(AddressSpace::default()).into(), // temporary for now
            Self::Ref(_) => ctx.ptr_type(AddressSpace::default()).into(),
            Self::None => ctx.void_type().into(),

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
