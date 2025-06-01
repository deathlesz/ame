use clap::Parser as _;
use miette::{IntoDiagnostic, Result, WrapErr};

use ame_codegen_llvm::{CodeGen, Context};
use ame_lexer::tokenize;
use ame_parser::Parser;

mod cli;

fn main() -> Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::default()
                .context_lines(3)
                .build(),
        )
    }))?;

    let args = cli::Args::parse();

    let source = std::fs::read_to_string(&args.source)
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read from `{}`", args.source.display()))?;

    let tokens = tokenize(&source).collect::<Vec<_>>();

    let mut parser = Parser::new(&tokens);
    let ast = parser
        .parse()
        .into_diagnostic()
        .wrap_err("failed to parse")?;

    let context = Context::create();
    let module = context.create_module(args.source.file_stem().unwrap().to_str().unwrap());
    let builder = context.create_builder();
    let mut codegen = CodeGen::new(&ast, &context, module, builder);

    codegen.generate(args.into());

    Ok(())
}
