use std::path::PathBuf;

use ame_codegen_llvm::{CodeGenOptions, OutputKind};

#[derive(Debug, Clone, clap::Parser)]
#[command(version, about = "The Ame `雨` compiler.")]
pub struct Args {
    #[arg(help = "Path to the source file")]
    pub source: PathBuf,
    #[arg(short, long, help = "Path to output file")]
    pub output: Option<PathBuf>,
    #[arg(short, long, help = "Target to compile for (e.g. x86_64-pc-linux-gnu)")]
    pub target: Option<String>,
    #[arg(
        short,
        long,
        help = "Features to enable (e.g. +sse2,+cx16,+sahf,-tbm). You can use 'native' to enable all features that current machine supports"
    )]
    pub features: Option<String>,
    #[arg(value_enum, short, long, default_value = "object")]
    pub emit: Emit,
    #[arg(short = 'O', help = "Enable optimization passes")]
    pub optimize: bool,
}

impl From<Args> for CodeGenOptions {
    #[inline]
    fn from(
        Args {
            output,
            target,
            features,
            emit,
            optimize,
            ..
        }: Args,
    ) -> Self {
        Self {
            output,
            output_kind: emit.into(),
            target,
            features,
            optimize,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default, clap::ValueEnum)]
pub enum Emit {
    #[default]
    #[value(help = "Emit object file")]
    Object,
    #[value(help = "Emit generated LLVM IR")]
    LLVMIr,
    #[value(help = "Emit generated assembly")]
    Assembly,
}

impl From<Emit> for OutputKind {
    #[inline]
    fn from(emit: Emit) -> Self {
        match emit {
            Emit::Object => Self::Object,
            Emit::LLVMIr => Self::LLVMIr,
            Emit::Assembly => Self::Assembly,
        }
    }
}
