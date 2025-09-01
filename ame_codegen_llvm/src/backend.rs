use inkwell::{
    types::StructType,
    values::{BasicValueEnum, FunctionValue, GlobalValue},
};

use ame_codegen::Backend;

pub struct LLVMBackend<'ctx>(std::marker::PhantomData<&'ctx ()>);

impl<'ctx> Backend for LLVMBackend<'ctx> {
    type LocalValue = BasicValueEnum<'ctx>;
    type FunctionValue = FunctionValue<'ctx>;
    type ClassValue = StructType<'ctx>;
    type StringValue = GlobalValue<'ctx>;
}
