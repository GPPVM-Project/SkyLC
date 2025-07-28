#[derive(Debug)]
pub struct CompilerContext {
    modules: Vec<String>,
}

impl Default for CompilerContext {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilerContext {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
        }
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
