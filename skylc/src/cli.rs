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

    #[command(name = "version", short_flag = 'v')]
    Version,
}

#[derive(Parser, Debug, Clone)]
pub struct CompileArgs {
    #[arg(required = true)]
    pub input_file: PathBuf,

    #[arg(short, long)]
    pub output: Option<PathBuf>,

    #[arg(short, long)]
    pub verbose: bool,
}
