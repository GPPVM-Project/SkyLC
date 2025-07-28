use std::any::Any;
use std::{cell::RefCell, rc::Rc};

use skyl_data::{CompilerConfig, CompilerContext, TokenStream};
use skyl_driver::{
    PipelineStep,
    errors::{CompilerErrorReporter, PipelineError},
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
    fn run(
        &mut self,
        input: Box<dyn Any>,
        _config: &CompilerConfig,
        _ctx: Rc<RefCell<CompilerContext>>,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Box<dyn Any>, PipelineError> {
        let tokens = input
            .downcast::<TokenStream>()
            .map_err(|_| PipelineError::new("Parser esperava TokenStream como entrada".into()))?;

        let ast = self.parse(reporter, *tokens);

        Ok(Box::new(ast))
    }
}
