use std::{cell::RefCell, rc::Rc};

use skyl_data::{CompilerConfig, TokenStream};
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
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::new())),
        }
    }
}

impl PipelineStep for Lexer {
    type Input = String;
    type Output = TokenStream;

    fn run(
        &mut self,
        input: Self::Input,
        _: &CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, PipelineError> {
        self.reset_internal_state(input);
        Ok(self.scan_tokens(reporter))
    }
}
