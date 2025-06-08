mod cli;

use anyhow::{Context, Result};
use clap::Parser;
use skyl_codegen::BytecodeGenerator;
use skyl_data::{read_file_without_bom, CompilerConfig};
use skyl_ir::IRGenerator;
use skyl_semantics::SemanticAnalyzer;
use skyl_stdlib::StdLibrary;
use skyl_vm::virtual_machine::VirtualMachine;
use skylc::find_stdlib_path;
use std::{cell::RefCell, rc::Rc};

use cli::{Cli, Commands, CompileArgs};
use skyl_driver::{
    errors::{handle_errors, CompilerErrorReporter},
    gpp_error, ExecutablePipeline, PipelineBuilder,
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
    let reporter = Rc::new(RefCell::new(CompilerErrorReporter::new()));

    let source_code =
        read_file_without_bom(args.input_file.to_str().unwrap()).with_context(|| {
            format!(
                "Falha ao ler o arquivo de entrada '{}'",
                args.input_file.display()
            )
        })?;

    let stdlib_path = find_stdlib_path();

    let stdlib_path = match stdlib_path {
        Err(e) => gpp_error!("{}", e),
        Ok(p) => match p {
            None => gpp_error!("The required environment variable SKYL_LIB is not defined."),
            Some(p) => p,
        },
    };

    let config = CompilerConfig::new(args.clone().input_file, stdlib_path);

    let mut pipeline = PipelineBuilder::new::<Lexer>()
        .add_step::<skyl_parser::Parser>()
        .add_step::<SemanticAnalyzer>()
        .add_step::<IRGenerator>()
        .add_step::<BytecodeGenerator>();

    let bytecode = pipeline.execute(source_code, &config, Rc::clone(&reporter));

    if reporter.borrow().has_errors() {
        handle_errors(&reporter.borrow());
    }

    match bytecode {
        Err(e) => gpp_error!("{}", e.0),
        Ok(b) => {
            let mut vm = VirtualMachine::new();
            vm.attach_bytecode(&b);
            StdLibrary::register_std_libraries(&mut vm);
            vm.interpret();
        }
    }

    Ok(())
}
