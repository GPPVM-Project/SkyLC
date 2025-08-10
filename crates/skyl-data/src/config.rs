use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub entry_point: PathBuf,
    pub root: PathBuf,
    pub verbose: bool,
    pub batch_stdout: bool,
    pub stdlib_path: PathBuf,
    pub optimization_level: u8,
    pub emit_debug_info: bool,
}

impl CompilerConfig {
    pub fn new(
        entry_point: PathBuf,
        stdlib_path: PathBuf,
        verbose: bool,
        batch_stdout: bool,
    ) -> Self {
        Self {
            batch_stdout,
            verbose,
            root: entry_point.parent().unwrap().to_path_buf(),
            entry_point,
            stdlib_path,
            optimization_level: 0,
            emit_debug_info: false,
        }
    }

    pub fn empty() -> Self {
        Self {
            batch_stdout: false,
            emit_debug_info: false,
            entry_point: PathBuf::new(),
            optimization_level: 0,
            root: PathBuf::new(),
            stdlib_path: PathBuf::new(),
            verbose: false,
        }
    }
}
