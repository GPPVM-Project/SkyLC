use serde::Deserialize;
use skyl_data::CompilerConfig;
use skyl_driver::gpp_error;
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use crate::{error::SkydError, find_stdlib_path};

#[derive(Debug, Deserialize)]
pub struct SkydManifest {
    pub package: Package,
    pub build: Option<BuildSpecification>,
    pub dependencies: Option<Dependencies>,
}

#[derive(Debug, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub authors: Option<Vec<String>>,
    pub license: Option<String>,
    pub edition: Option<String>,
    pub entry: String,
}

#[derive(Debug, Deserialize)]
pub struct BuildSpecification {
    pub out_dir: Option<String>,
    pub opt_level: Option<u8>,
}

#[derive(Debug, Deserialize)]
pub struct Dependencies {
    pub git: Option<HashMap<String, GitDependency>>,
    pub path: Option<HashMap<String, PathDependency>>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct GitDependency {
    pub git: String,
    pub branch: Option<String>,
    pub private: Option<bool>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct PathDependency {
    pub path: String,
}

impl Dependencies {
    pub fn from_manifest(manifest: &SkydManifest) -> Self {
        let git_deps = match &manifest.dependencies {
            Some(d) => d.git.clone(),
            None => None,
        };
        let path_deps = match &manifest.dependencies {
            Some(d) => d.path.clone(),
            None => None,
        };

        Dependencies {
            git: git_deps,
            path: path_deps,
        }
    }

    pub fn names(&self) -> Vec<String> {
        let mut names = Vec::new();
        if let Some(git_deps) = &self.git {
            names.extend(git_deps.keys().cloned());
        }
        if let Some(path_deps) = &self.path {
            names.extend(path_deps.keys().cloned());
        }
        names
    }
}

impl SkydManifest {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, SkydError> {
        let content = fs::read_to_string(&path)?;
        let manifest = toml::from_str(&content)?;
        Ok(manifest)
    }
}

pub fn get_compiler_config(package: &Package) -> Result<CompilerConfig, SkydError> {
    let stdlib_path = find_stdlib_path();

    let stdlib_path = match stdlib_path {
        Err(e) => gpp_error!("{}", e),
        Ok(p) => match p {
            None => gpp_error!("The required environment variable SKYL_LIB is not defined."),
            Some(p) => p,
        },
    };

    Ok(CompilerConfig {
        entry_point: package.entry.clone().into(),
        root: PathBuf::from(&package.entry)
            .parent()
            .unwrap()
            .to_path_buf(),
        verbose: false,
        batch_stdout: false,
        stdlib_path,
        optimization_level: 0,
        emit_debug_info: false,
    })
}
