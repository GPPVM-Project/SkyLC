use std::{
    env,
    path::{Path, PathBuf},
};

use anyhow::Error;

use crate::{
    error::SkydError,
    manifest::{Dependencies, SkydManifest},
};

pub mod cli;
pub mod dependencies;
pub mod error;
pub mod manifest;

#[derive(Debug)]
pub struct Skyd {
    manifest: SkydManifest,
    dependencies: Dependencies,
}

impl Skyd {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, SkydError> {
        let manifest = Self::load_manifest(path)?;
        let dependencies = Dependencies::from_manifest(&manifest);

        Ok(Self {
            manifest,
            dependencies,
        })
    }

    pub fn dependencies(&self) -> &Dependencies {
        &self.dependencies
    }

    pub fn manifest(&self) -> &SkydManifest {
        &self.manifest
    }

    fn load_manifest<P: AsRef<Path>>(path: P) -> Result<SkydManifest, SkydError> {
        SkydManifest::new(path)
    }
}

pub fn find_stdlib_path() -> Result<Option<PathBuf>, Error> {
    if let Ok(path_str) = env::var("SKYL_LIB") {
        let path = PathBuf::from(path_str);
        if path.is_dir() {
            return Ok(Some(path));
        } else {
            return Err(anyhow::anyhow!(
                "The environment variable SKYL_LIB value is '{}', but the path not exists or isn't a directory.",
                path.display()
            ));
        }
    }

    if let Ok(exe_path) = env::current_exe() {
        if let Some(install_root) = exe_path.parent().and_then(|p| p.parent()) {
            let lib_path = install_root.join("lib").join("skyl");
            if lib_path.is_dir() {
                return Ok(Some(lib_path));
            }
        }
    }

    Ok(None)
}
