use std::path::PathBuf;

use clap::{command, Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Commands {
    #[command(name = "compile", short_flag = 'c')]
    Compile(CompileArgs),
}

#[derive(Parser, Debug, Clone)]
pub struct CompileArgs {
    #[arg(required = true)]
    pub input_file: PathBuf,

    #[arg(short, long)]
    pub output: Option<PathBuf>,
}
