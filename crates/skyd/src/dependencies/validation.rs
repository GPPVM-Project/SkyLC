use std::path::PathBuf;

use git2::{Cred, CredentialType, Direction, Remote, RemoteCallbacks};

use crate::{error::SkydError, manifest::Dependencies};

pub fn validate_all(dependencies: &Dependencies) -> Result<(), Vec<SkydError>> {
    let mut errors = Vec::new();

    if let Some(git_deps) = &dependencies.git {
        for (name, dep) in git_deps {
            if let Err(e) = validate_git_dependency(&dep.git, &dep.branch) {
                errors.push(SkydError::Git(format!(
                    "Failed to validate Git dependency '{name}': {e}"
                )));
            }
        }
    }

    // Iterate through Path dependencies and validate them
    if let Some(path_deps) = &dependencies.path {
        for (name, dep) in path_deps {
            if let Err(e) = validate_path_dependency(&dep.path) {
                errors.push(SkydError::IO {
                    cause: format!("Failed to validate path dependency '{name}': {e}"),
                });
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(())
}

fn validate_path_dependency(path: &str) -> Result<(), SkydError> {
    let path_buf = PathBuf::from(path);

    if !path_buf.exists() {
        return Err(SkydError::IO {
            cause: format!("Dependency path '{path}' does not exist"),
        });
    }

    Ok(())
}

fn validate_git_dependency(git: &str, branch: &Option<String>) -> Result<(), SkydError> {
    let base = dirs::data_dir().unwrap().join(".skyd").join("deps");

    let name = git.split('/').next_back().unwrap().replace(".git", "");
    let path = base.join(&name);

    if path.exists() {
        return Ok(());
    }

    let mut callbacks = RemoteCallbacks::new();
    callbacks.credentials(|url, username_from_url, allowed| {
        // Tenta usar o token HTTPS se permitido
        if allowed.contains(CredentialType::USER_PASS_PLAINTEXT) && url.starts_with("https://") {
            if let Ok(token) = std::env::var("SKYD_GITHUB_TOKEN") {
                return Cred::userpass_plaintext("x-access-token", &token);
            }
        }

        // Tenta usar a chave SSH se permitido
        if allowed.contains(CredentialType::SSH_KEY) {
            if let Some(user) = username_from_url {
                return Cred::ssh_key_from_agent(user);
            }
        }

        // Se nenhuma credencial for encontrada, retorne um erro
        Err(git2::Error::from_str("No authentication method available"))
    });

    let mut remote =
        Remote::create_detached(git).map_err(|e| SkydError::Git(e.message().to_owned()))?;

    // The second argument to `connect_auth` expects a `Option<RemoteCallbacks>`.
    // It is more robust to pass the `callbacks` directly.
    remote
        .connect_auth(Direction::Fetch, Some(callbacks), None)
        .map_err(|e| SkydError::UnaccessibleRepository(format!("{git} ({})", e.message())))?;

    let refs = remote
        .list()
        .map_err(|e| SkydError::Git(e.message().to_owned()))?;

    if let Some(b) = branch {
        let wanted = format!("refs/heads/{b}");
        let exists = refs.iter().any(|h| h.name() == wanted);
        if !exists {
            remote.disconnect().unwrap();
            return Err(SkydError::BranchNotFound(b.clone()));
        }
    }

    remote.disconnect().unwrap();
    Ok(())
}
