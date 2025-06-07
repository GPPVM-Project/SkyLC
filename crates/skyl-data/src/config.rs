use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub entry_point: PathBuf,
    pub root: PathBuf,
    pub optimization_level: u8,
    pub emit_debug_info: bool,
}

impl CompilerConfig {
    pub fn new(entry_point: PathBuf) -> Self {
        Self {
            entry_point: entry_point.clone(),
            root: entry_point.clone().parent().unwrap().to_path_buf(),
            optimization_level: 0,
            emit_debug_info: false,
        }
    }
}
