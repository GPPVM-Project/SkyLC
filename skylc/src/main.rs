mod cli;

use anyhow::{Context, Error, Result};
use clap::Parser;
use skyl_codegen::BytecodeGenerator;
use skyl_data::{
    bytecode::Bytecode, read_file_without_bom, CompilerConfig, CompilerContext, IntermediateCode,
};
use skyl_ir::IRGenerator;
use skyl_semantics::SemanticAnalyzer;
use skyl_stdlib::StdLibrary;
use skyl_vm::virtual_machine::VirtualMachine;
use skylc::{
    config::load_config,
    decompiler::Decompiler,
    find_stdlib_path,
    version::{CODENAME, VERSION},
};
use std::{cell::RefCell, ffi::OsStr, path::PathBuf, rc::Rc};

use cli::{Cli, Commands, CompileArgs};
use skyl_driver::{
    errors::{handle_errors, CompilerErrorReporter},
    gpp_error, Pipeline,
};
use skyl_lexer::Lexer;

use crate::cli::RunArgs;

#[cfg(debug_assertions)]
fn load_dotenv() {
    #[allow(unused_must_use)]
    dotenvy::dotenv();
}

#[cfg(not(debug_assertions))]
fn load_dotenv() {}

fn main() -> Result<()> {
    load_dotenv();

    let cli_args = Cli::parse();

    if cli_args.show_version {
        version();
        return Ok(());
    }

    if let Some(command) = &cli_args.command {
        match command {
            Commands::Compile(args) => {
                compile(args)?;
            }
            Commands::Run(args) => run(args)?,
        }
    }

    Ok(())
}

fn compile(args: &CompileArgs) -> Result<()> {
    let _skyl_config = load_config().map_err(|e| Error::msg(e.to_string()))?;

    if args.output.extension() != Some(OsStr::new("grc")) {
        return Err(Error::msg("Please specify `.grc` file for output argument"));
    }

    let source_code = Rc::new(
        read_file_without_bom(args.input_file.to_str().unwrap()).with_context(|| {
            format!("Failed to read input file: '{}'", args.input_file.display())
        })?,
    );

    let ctx = Rc::new(RefCell::new(CompilerContext::new()));
    ctx.borrow_mut()
        .push_module(args.input_file.to_str().unwrap().to_string());

    let reporter = Rc::new(RefCell::new(CompilerErrorReporter::new(Some(ctx.clone()))));

    let stdlib_path = find_stdlib_path();

    let stdlib_path = match stdlib_path {
        Err(e) => gpp_error!("{}", e),
        Ok(p) => match p {
            None => gpp_error!("The required environment variable SKYL_LIB is not defined."),
            Some(p) => p,
        },
    };

    let config = CompilerConfig::new(args.clone().input_file, stdlib_path, args.verbose);

    let mut pipeline = Pipeline::new()
        .add_stage(Box::new(Lexer::default()))
        .add_stage(Box::new(skyl_parser::Parser::default()))
        .add_stage(Box::new(SemanticAnalyzer::default()))
        .add_stage(Box::new(IRGenerator::default()));

    let ir = match pipeline.execute(
        source_code.content.clone(),
        &config,
        ctx.clone(),
        Rc::clone(&reporter),
    ) {
        Err(e) => gpp_error!("{}", e.0),
        Ok(ir) => ir,
    };

    if reporter.borrow().has_errors() {
        handle_errors(&reporter.borrow());
    }

    let ir: IntermediateCode = ir.downcast::<IntermediateCode>().unwrap().as_ref().clone();

    if config.verbose {
        Decompiler::decompile(&ir);
    }

    let bytecode_gen = BytecodeGenerator::new();
    let bytecode = bytecode_gen.generate(ir);

    bytecode
        .save_to_file(&args.output)
        .map_err(|e| Error::msg(format!("Compilation Error: {e}")))
        .with_context(|| format!("Failed to save bytecode to '{}'", args.output.display()))?;

    Ok(())
}

fn run(args: &RunArgs) -> anyhow::Result<()> {
    let _skyl_config = load_config().map_err(|e| Error::msg(e.to_string()))?;

    let config = CompilerConfig::new(args.bytecode_file.clone(), PathBuf::new(), args.verbose);

    let bytecode = Bytecode::load_from_file(&args.bytecode_file).context(format!(
        "Failed to load bytecode from '{}'",
        args.bytecode_file.display()
    ))?;

    let mut vm = VirtualMachine::new(&config);
    vm.attach_bytecode(&bytecode);

    StdLibrary::register_std_libraries(&mut vm);

    vm.interpret();

    Ok(())
}

fn version() {
    let _ascii_art = include_str!("../../assets/ascii-art.txt");
    // println!("\n{}", _ascii_art);
    println!("Skyl {VERSION} {CODENAME}");
}
