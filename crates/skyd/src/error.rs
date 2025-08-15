use std::io;

use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum SkydError {
    #[error("Error to read file: {}", cause)]
    IO { cause: String },

    #[error("Error to read TOML file: {}", message)]
    Toml { message: String },

    #[error("Git error: {0}")]
    Git(String),

    #[error("Unaccessible repository: {0}")]
    UnaccessibleRepository(String),

    #[error("Branch '{0}' not found in remote repository")]
    BranchNotFound(String),
}

impl From<io::Error> for SkydError {
    fn from(e: io::Error) -> Self {
        SkydError::IO {
            cause: e.to_string(),
        }
    }
}

impl From<toml::de::Error> for SkydError {
    fn from(e: toml::de::Error) -> Self {
        SkydError::Toml {
            message: e.message().to_owned(),
        }
    }
}
