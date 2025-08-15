use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(name = "skyd", author, about = "Skyl project manager")]
pub struct SkydCli {
    #[command(subcommand)]
    pub command: Option<SkydCommand>,
}

#[derive(Subcommand, Debug)]
pub enum SkydCommand {
    Build(BuildOptions),
    Check,
    Fetch,
}

#[derive(Parser, Debug, Clone)]
pub struct BuildOptions {
    #[arg(short, long)]
    pub verbose: bool,
}
