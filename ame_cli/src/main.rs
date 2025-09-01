use clap::Parser as _;
use miette::{IntoDiagnostic, Result, WrapErr};

use ame_codegen::CodeGenCtx;
use ame_codegen_llvm::{CodeGen, Context};
use ame_lexer::tokenize;
use ame_parser::Parser;
use ame_tast::Inferrer;
use ame_types::TypeCtx;

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

    let before = std::time::Instant::now();
    let source = std::fs::read_to_string(&args.source)
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read from `{}`", args.source.display()))?;
    println!(
        "read file in {} ms",
        (std::time::Instant::now() - before).as_millis()
    );

    let before = std::time::Instant::now();
    let tokens = tokenize(&source)
        .collect::<Result<Vec<_>, _>>()
        .into_diagnostic()
        .wrap_err("failed to lex")?;
    // println!("tokens: {tokens:?}");
    println!(
        "tokenized in {} ms",
        (std::time::Instant::now() - before).as_millis()
    );

    let parser = Parser::new(&tokens);
    let before = std::time::Instant::now();
    let (ast, stmts) = parser
        .parse()
        .into_diagnostic()
        .wrap_err("failed to parse")?;
    println!(
        "parsed in {} ms",
        (std::time::Instant::now() - before).as_millis()
    );

    let mut tcx = TypeCtx::new();

    let before = std::time::Instant::now();
    let inferrer = Inferrer::new(&mut tcx, &ast);
    let (typed_ast, typed_stmts) = inferrer
        .infer(&stmts)
        .into_diagnostic()
        .wrap_err("failed to infer types")?;
    println!(
        "type checked in {} ms",
        (std::time::Instant::now() - before).as_millis()
    );

    let context = Context::create();
    let module = context.create_module(args.source.file_stem().unwrap().to_str().unwrap());
    let builder = context.create_builder();

    let ctx = CodeGenCtx::new(&typed_ast, &typed_stmts, &tcx);
    let mut codegen = CodeGen::new(ctx, &context, module, builder);

    let before = std::time::Instant::now();
    codegen.generate(args.into());
    println!(
        "code generated in {} ms",
        (std::time::Instant::now() - before).as_millis()
    );

    Ok(())
}
