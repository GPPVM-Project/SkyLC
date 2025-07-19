mod cli;

use anyhow::{Context, Result};
use clap::Parser;
use skyl_codegen::BytecodeGenerator;
use skyl_data::{read_file_without_bom, CompilerConfig, IntermediateCode, SemanticCode};
use skyl_ir::IRGenerator;
use skyl_semantics::SemanticAnalyzer;
use skyl_stdlib::StdLibrary;
use skyl_vm::virtual_machine::VirtualMachine;
use skylc::find_stdlib_path;
use std::{cell::RefCell, rc::Rc};

use cli::{Cli, Commands, CompileArgs};
use skyl_driver::{
    errors::{handle_errors, CompilerErrorReporter},
    gpp_error, Pipeline,
};
use skyl_lexer::Lexer;

fn main() -> Result<()> {
    let cli_args = Cli::parse();

    match &cli_args.command {
        Commands::Compile(args) => {
            compile(args)?;
        }
        _ => {}
    }

    Ok(())
}

fn compile(args: &CompileArgs) -> Result<()> {
    let source_code = Rc::new(
        read_file_without_bom(args.input_file.to_str().unwrap()).with_context(|| {
            format!("Failed to read input file: '{}'", args.input_file.display())
        })?,
    );

    let reporter = Rc::new(RefCell::new(CompilerErrorReporter::new(Rc::clone(
        &source_code,
    ))));

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
        .add_stage(Box::new(IRGenerator::default()))
        .add_stage(Box::new(BytecodeGenerator::default()));

    let bytecode =
        match pipeline.execute(source_code.content.clone(), &config, Rc::clone(&reporter)) {
            Err(e) => gpp_error!("{}", e.0),
            Ok(ir) => ir,
        };

    if reporter.borrow().has_errors() {
        handle_errors(&reporter.borrow());
    }

    let mut vm = VirtualMachine::new(&config);
    vm.attach_bytecode(&bytecode.downcast().unwrap());
    StdLibrary::register_std_libraries(&mut vm);
    vm.interpret();

    Ok(())
}
