use miette::{miette, IntoDiagnostic, Result, WrapErr};

use ame_codegen_llvm::{CodeGen, Context};
use ame_lexer::tokenize;
use ame_parser::Parser;

fn main() -> Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::default()
                .context_lines(3)
                .build(),
        )
    }))?;

    let source_path = std::env::args().nth(1).ok_or(miette!("no source file"))?;
    let source = std::fs::read_to_string(&source_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read from `{}`", source_path))?;

    let tokens = tokenize(&source).collect::<Vec<_>>();

    let mut parser = Parser::new(&tokens);
    let ast = parser
        .parse()
        .into_diagnostic()
        .wrap_err("failed to parse")?;

    let context = Context::create();
    let module = context.create_module("ame");
    let builder = context.create_builder();
    let mut codegen = CodeGen::new(&ast, &context, module, builder);

    codegen.generate();

    Ok(())
}
