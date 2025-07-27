use std::path::PathBuf;

use clap::{command, Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(author, about, long_about = None, disable_version_flag = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,

    #[arg(short = 'v', long = "version")]
    pub show_version: bool,
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

    #[arg(short, long)]
    pub verbose: bool,
}
