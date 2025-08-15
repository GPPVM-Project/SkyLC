use std::{cell::RefCell, path::PathBuf, rc::Rc};

use git2::{Cred, FetchOptions, RemoteCallbacks, Repository};
use skyl_data::CompilerContext;

use crate::{error::SkydError, manifest::Dependencies};

pub fn setup_dependencies(
    ctx: Rc<RefCell<CompilerContext>>,
    dependencies: &Dependencies,
) -> Result<(), Vec<SkydError>> {
    let mut errors = Vec::new();

    if let Some(git_deps) = &dependencies.git {
        for (name, dep) in git_deps {
            if let Err(e) = setup_git_dependency(ctx.clone(), &dep.git, dep.branch.as_ref()) {
                errors.push(SkydError::Git(format!(
                    "Failed to setup git dependency '{}': {}",
                    name, e
                )));
            }
        }
    }

    if let Some(path_deps) = &dependencies.path {
        for (_name, dep) in path_deps {
            setup_path_dependency(ctx.clone(), &dep.path);
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(())
    }
}

fn setup_git_dependency(
    ctx: Rc<RefCell<CompilerContext>>,
    git: &str,
    branch: Option<&String>,
) -> Result<(), SkydError> {
    let base = dirs::data_dir().unwrap().join(".skyd").join("deps");

    let name = git.split('/').last().unwrap().replace(".git", "");
    let path = base.join(&name);

    if path.exists() {
        let src_path = path.join("src");
        ctx.borrow_mut().add_dependency(src_path);
        return Ok(());
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

    if let Some(branch) = branch {
        builder.branch(branch);
    }

    Repository::clone(git, &path).map_err(|e| SkydError::Git(e.to_string()))?;

    println!("Dependency {} cloned into {:?}", name, path);
    let src_path = path.join("src");
    ctx.borrow_mut().add_dependency(src_path);

    Ok(())
}

fn setup_path_dependency(ctx: Rc<RefCell<CompilerContext>>, path: &str) {
    ctx.borrow_mut()
        .add_dependency(PathBuf::from(path).join("src"));
}
