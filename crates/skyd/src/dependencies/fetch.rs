use std::fs;

use git2::{Cred, FetchOptions, RemoteCallbacks};

use crate::{error::SkydError, manifest::Dependencies};

pub fn fetch_dependencies(deps: &Dependencies) -> Result<(), SkydError> {
    if let Some(git_deps) = &deps.git {
        for (name, dep) in git_deps {
            let repo_name = dep
                .git
                .split('/')
                .next_back()
                .unwrap_or("")
                .trim_end_matches(".git");

            let base = dirs::data_dir().unwrap().join(".skyd").join("deps");
            let path = base.join(repo_name);

            if path.exists() {
                println!("Updating dependency '{name}' by re-cloning...");
                fs::remove_dir_all(&path).map_err(|e| SkydError::IO {
                    cause: format!(
                        "Failed to remove old dependency directory '{}': {}",
                        path.display(),
                        e
                    ),
                })?;
            } else {
                println!("Cloning dependency '{name}' for the first time...");
            }

            let mut callbacks = RemoteCallbacks::new();
            callbacks.credentials(|url, username_from_url, allowed| {
                if url.starts_with("https://") {
                    if let Ok(token) = std::env::var("SKYD_GITHUB_TOKEN") {
                        return Cred::userpass_plaintext("x-access-token", &token);
                    }
                }
                if allowed.contains(git2::CredentialType::SSH_KEY) {
                    if let Some(user) = username_from_url {
                        return Cred::ssh_key_from_agent(user);
                    }
                }
                Err(git2::Error::from_str("No authentication method available"))
            });

            let mut fo = FetchOptions::new();
            fo.remote_callbacks(callbacks);

            let mut builder = git2::build::RepoBuilder::new();
            builder.fetch_options(fo);

            if let Some(branch) = dep.branch.as_ref() {
                builder.branch(branch);
            }

            builder
                .clone(&dep.git, &path)
                .map_err(|e| SkydError::Git(format!("Failed to clone {name}: {e}")))?;

            println!("Dependency '{name}' successfully updated.");
        }
    }

    Ok(())
}
