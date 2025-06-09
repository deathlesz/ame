use ame_types::{FloatKind, IntKind, Type};
use inkwell::{context::Context, types::BasicTypeEnum};

pub trait AsLLVMType<'ctx> {
    type Out;

    fn as_llvm_type(&self, ctx: &'ctx Context) -> Self::Out;
}

impl<'ctx> AsLLVMType<'ctx> for Type {
    type Out = BasicTypeEnum<'ctx>;

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

            _ => panic!("cannot lower to llvm type"),
        }
    }
}
