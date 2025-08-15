use std::{collections::HashMap, path::PathBuf};

use crate::{SourceFile, SourceFileID};

#[derive(Debug)]
pub struct CompilerContext {
    modules: Vec<String>,
    dependencies: Vec<PathBuf>,
    files: HashMap<SourceFileID, SourceFile>,
}

impl Default for CompilerContext {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilerContext {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            modules: Vec::new(),
            dependencies: Vec::new(),
        }
    }

    pub fn dependencies(&self) -> &Vec<PathBuf> {
        &self.dependencies
    }

    pub fn register_file(&mut self, file: SourceFile) {
        self.files.insert(file.id, file);
    }

    pub fn get_file(&mut self, id: &SourceFileID) -> &SourceFile {
        &self.files[id]
    }

    pub fn peek_module(&self) -> String {
        self.modules.last().unwrap().clone()
    }

    pub fn push_module(&mut self, module_name: String) {
        self.modules.push(module_name);
    }

    pub fn pop_module(&mut self) {
        self.modules.pop();
    }

    pub fn add_dependency(&mut self, path: PathBuf) {
        self.dependencies.push(path);
    }
}
