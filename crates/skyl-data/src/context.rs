use std::collections::HashMap;

use crate::{SourceFile, SourceFileID};

#[derive(Debug)]
pub struct CompilerContext {
    modules: Vec<String>,
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
        }
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
}
