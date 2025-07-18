use std::{cell::RefCell, rc::Rc};

use skyl_data::{Ast, CompilerConfig, TokenStream};
use skyl_driver::{
    PipelineStep,
    errors::{self, CompilerErrorReporter},
};

use crate::parser::Parser;

impl Default for Parser {
    fn default() -> Self {
        Self {
            stream: Default::default(),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::empty())),
        }
    }
}

impl PipelineStep for Parser {
    type Input = TokenStream;
    type Output = Ast;

    fn run(
        &mut self,
        input: Self::Input,
        _: &CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, errors::PipelineError> {
        Ok(self.parse(reporter, input))
    }
}
