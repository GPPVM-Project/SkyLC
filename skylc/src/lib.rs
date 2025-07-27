use std::{env, path::PathBuf};

use anyhow::Error;

pub mod cli;
pub mod decompiler;
pub mod version;

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
