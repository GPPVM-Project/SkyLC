use std::path::PathBuf;

use crate::{
    error::SkydError,
    manifest::{Dependencies, Dependency},
};

pub fn validate_all(dependencies: &Dependencies) -> Result<(), Vec<SkydError>> {
    let mut errors = Vec::new();

    for (name, dependency) in &dependencies.deps {
        match validate_dependency(&dependency) {
            Ok(_) => {}
            Err(e) => errors.push(e),
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(())
}

fn validate_dependency(dependency: &Dependency) -> Result<(), SkydError> {
    match dependency {
        Dependency::Git { git, branch } => validate_git_dependency(git, branch),
        Dependency::Path { path } => validate_path_dependency(path),
    }
}

fn validate_path_dependency(path: &str) -> Result<(), SkydError> {
    let path_buf = PathBuf::from(path);

    if !path_buf.exists() {
        return Err(SkydError::IO {
            cause: format!("Dependency path '{}' not exists", path),
        });
    }

    Ok(())
}

fn validate_git_dependency(git: &str, branch: &Option<String>) -> Result<(), SkydError> {
    todo!()
}
