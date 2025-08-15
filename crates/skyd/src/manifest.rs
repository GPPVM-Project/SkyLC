use serde::Deserialize;
use skyl_data::CompilerConfig;
use skyl_driver::gpp_error;
use std::{
    collections::HashMap,
    fmt, fs, io,
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
    #[serde(flatten)]
    pub deps: HashMap<String, Dependency>,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(untagged)]
pub enum Dependency {
    Git { git: String, branch: Option<String> },
    Path { path: String },
}

impl Dependencies {
    pub fn from_manifest(manifest: &SkydManifest) -> Self {
        let deps = match &manifest.dependencies {
            Some(d) => d.deps.clone(),
            None => HashMap::new(),
        };

        Dependencies { deps }
    }

    pub fn names(&self) -> Vec<String> {
        self.deps.keys().cloned().collect()
    }
}

impl SkydManifest {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, SkydError> {
        let content = fs::read_to_string(&path)?;
        let manifest = toml::from_str(&content)?;
        Ok(manifest)
    }
}

impl fmt::Display for SkydError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SkydError::IO { cause } => write!(f, "IO Error: {}", cause),
            SkydError::Toml { message } => write!(f, "Error to read TOML file: {}", message),
        }
    }
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

pub fn get_compiler_config(package: &Package) -> Result<CompilerConfig, SkydError> {
    let stdlib_path = find_stdlib_path();

    let stdlib_path = match stdlib_path {
        Err(e) => gpp_error!("{}", e),
        Ok(p) => match p {
            None => gpp_error!("The required environment variable SKYL_LIB is not defined."),
            Some(p) => p,
        },
    };

    return Ok(CompilerConfig {
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
    });
}
