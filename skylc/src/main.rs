mod cli;

use anyhow::{Context, Result};
use clap::Parser;
use skyl_data::{read_file_without_bom, CompilerConfig};
use skyl_semantics::SemanticAnalyzer;
use std::{cell::RefCell, rc::Rc};

use cli::{Cli, Commands, CompileArgs};
use skyl_driver::{errors::CompilerErrorReporter, ExecutablePipeline, PipelineBuilder};
use skyl_lexer::Lexer;

fn main() -> Result<()> {
    let cli_args = Cli::parse();

    match &cli_args.command {
        Commands::Compile(args) => {
            compile(args)?;
        }
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

    let config = CompilerConfig::new(args.clone().input_file);
    let mut pipeline = PipelineBuilder::new::<Lexer>()
        .add_step::<skyl_parser::Parser>()
        .add_step::<SemanticAnalyzer>();

    let ast_result = pipeline.execute(source_code, &config, Rc::clone(&reporter));

    if reporter.borrow().has_errors() {
        return Err(anyhow::anyhow!("A compilação falhou com um ou mais erros."));
    }

    println!("\n--- AST Gerada ---");
    println!("{:#?}", ast_result.unwrap());
    println!("--------------------");

    if let Some(output_path) = &args.output {
        println!(
            "\n(Futuro) Escrevendo resultado para: {}",
            output_path.display()
        );
    }

    println!("\nCompilação concluída com sucesso.");
    Ok(())
}
