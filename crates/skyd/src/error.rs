#[derive(Debug, Clone)]
pub enum SkydError {
    IO { cause: String },
    Toml { message: String },
}
