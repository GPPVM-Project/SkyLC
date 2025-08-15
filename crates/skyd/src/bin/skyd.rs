use std::{cell::RefCell, rc::Rc};

use anyhow::{Context, Error};
use clap::Parser;
use skyd::{
    cli::{BuildOptions, SkydCli, SkydCommand},
    dependencies::{setup::setup_dependencies, validation::validate_all},
    error::SkydError,
    manifest::get_compiler_config,
    Skyd,
};
use skyl_codegen::BytecodeGenerator;
use skyl_data::{read_file_without_bom, CompilerConfig, CompilerContext, IntermediateCode};
use skyl_driver::{
    errors::{handle_errors, CompilerErrorReporter},
    gpp_error, Pipeline,
};
use skyl_ir::IRGenerator;
use skyl_lexer::Lexer;
use skyl_semantics::SemanticAnalyzer;

fn main() -> Result<(), SkydError> {
    let cli_args = SkydCli::parse();

    if let Some(command) = cli_args.command {
        match command {
            SkydCommand::Build(build_options) => {
                build(build_options)?;
            }

            SkydCommand::Check => {
                check()?;
            }
        }
    }

    Ok(())
}

fn build(_build_options: BuildOptions) -> Result<(), SkydError> {
    let path = std::env::current_dir().unwrap();
    let skyd_toml = path.join("Skyd.toml");

    if !skyd_toml.exists() {
        return Err(SkydError::Toml {
            message: "Skyd.toml file not found in this directory".into(),
        });
    }

    let skyd = Skyd::new(skyd_toml)?;
    let compiler_config = get_compiler_config(&skyd.manifest().package)?;
    let ctx = Rc::new(RefCell::new(CompilerContext::new()));
    let reporter = Rc::new(RefCell::new(CompilerErrorReporter::new(Some(ctx.clone()))));
    let entry_path = skyd.manifest().package.entry.clone();

    ctx.borrow_mut().push_module(entry_path.clone());

    match validate_all(skyd.dependencies()) {
        Ok(()) => {}
        Err(errors) => {
            for e in errors {
                eprintln!("{}", e);
            }
        }
    }

    setup_dependencies(ctx.clone(), skyd.dependencies());

    let source_code = read_file_without_bom(&entry_path).map_err(|e| SkydError::IO {
        cause: format!("Error to read `{}`: {}", entry_path, e.to_string()),
    })?;

    let mut pipeline = Pipeline::new()
        .add_stage(Box::new(Lexer::default()))
        .add_stage(Box::new(skyl_parser::Parser::default()))
        .add_stage(Box::new(SemanticAnalyzer::default()))
        .add_stage(Box::new(IRGenerator::default()));

    let ir = match pipeline.execute(
        source_code.content.clone(),
        &compiler_config,
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

    let bytecode_gen = BytecodeGenerator::new();
    let bytecode = bytecode_gen.generate(ir);

    let out_filepath = path
        .join("out")
        .join(format!("{}.grc", skyd.manifest().package.name));

    bytecode
        .save_to_file(&out_filepath)
        .map_err(|e| Error::msg(format!("Compilation Error: {e}")))
        .with_context(|| format!("Failed to save bytecode to '{}'", out_filepath.display()))
        .map_err(|e| SkydError::IO {
            cause: e.to_string(),
        })?;

    Ok(())
}

fn check() -> Result<(), SkydError> {
    let path = std::env::current_dir().unwrap();
    let skyd_toml = path.join("Skyd.toml");

    if !skyd_toml.exists() {
        return Err(SkydError::Toml {
            message: "Skyd.toml file not found in this directory".into(),
        });
    }

    let skyd = Skyd::new(skyd_toml)?;
    let compiler_config = get_compiler_config(&skyd.manifest().package)?;
    let ctx = Rc::new(RefCell::new(CompilerContext::new()));
    let reporter = Rc::new(RefCell::new(CompilerErrorReporter::new(Some(ctx.clone()))));
    let entry_path = skyd.manifest().package.entry.clone();

    ctx.borrow_mut().push_module(entry_path.clone());

    match validate_all(skyd.dependencies()) {
        Ok(()) => {}
        Err(errors) => {
            for e in errors {
                eprintln!("{}", e);
            }
        }
    }

    setup_dependencies(ctx.clone(), skyd.dependencies());

    let source_code = read_file_without_bom(&entry_path).map_err(|e| SkydError::IO {
        cause: format!("Error to read `{}`: {}", entry_path, e.to_string()),
    })?;

    let mut pipeline = Pipeline::new()
        .add_stage(Box::new(Lexer::default()))
        .add_stage(Box::new(skyl_parser::Parser::default()))
        .add_stage(Box::new(SemanticAnalyzer::default()));

    let _semantic_code = match pipeline.execute(
        source_code.content.clone(),
        &compiler_config,
        ctx.clone(),
        Rc::clone(&reporter),
    ) {
        Err(e) => gpp_error!("{}", e.0),
        Ok(ir) => ir,
    };

    if reporter.borrow().has_errors() {
        handle_errors(&reporter.borrow());
    }

    Ok(())
}
