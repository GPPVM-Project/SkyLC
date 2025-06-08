use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub entry_point: PathBuf,
    pub root: PathBuf,
    pub stdlib_path: PathBuf,
    pub optimization_level: u8,
    pub emit_debug_info: bool,
}

impl CompilerConfig {
    pub fn new(entry_point: PathBuf, stdlib_path: PathBuf) -> Self {
        Self {
            root: entry_point.parent().unwrap().to_path_buf(),
            entry_point,
            stdlib_path,
            optimization_level: 0,
            emit_debug_info: false,
        }
    }
}
