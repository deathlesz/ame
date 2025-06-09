use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CodeGenOptions {
    pub output: Option<PathBuf>,
    pub output_kind: OutputKind,

    pub target: Option<String>,
    pub features: Option<String>,
    pub optimize: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputKind {
    #[default]
    Object,
    LLVMIr,
    Assembly,
}

impl OutputKind {
    #[inline]
    pub const fn extension(&self) -> &str {
        match self {
            Self::Object => "o",
            Self::LLVMIr => "ll",
            Self::Assembly => "as",
        }
    }
}
