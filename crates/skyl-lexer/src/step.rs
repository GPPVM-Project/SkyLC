use std::any::Any;
use std::{cell::RefCell, rc::Rc};

use skyl_data::{CompilerConfig, CompilerContext};
use skyl_driver::{
    PipelineStep,
    errors::{CompilerErrorReporter, PipelineError},
};

use crate::lexer::Lexer;

impl Default for Lexer {
    fn default() -> Self {
        Self {
            source: Default::default(),
            line: Default::default(),
            column: Default::default(),
            start: Default::default(),
            length: Default::default(),
            tokens: Default::default(),
            keywords: Default::default(),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::empty())),
        }
    }
}

impl PipelineStep for Lexer {
    fn run(
        &mut self,
        input: Box<dyn Any>,
        _config: &CompilerConfig,
        _ctx: Rc<RefCell<CompilerContext>>,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Box<dyn Any>, PipelineError> {
        let source = input
            .downcast::<String>()
            .map_err(|_| PipelineError::new("Lexer esperava um String como entrada".into()))?;

        self.reset_internal_state(*source);

        let tokens = self.scan_tokens(reporter);

        Ok(Box::new(tokens))
    }
}
