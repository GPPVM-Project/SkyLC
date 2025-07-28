use skyl_data::IntermediateCode;
use skyl_data::{CompilerConfig, CompilerContext};
use skyl_driver::{errors::CompilerErrorReporter, errors::PipelineError, PipelineStep};
use std::{any::Any, cell::RefCell, rc::Rc};

use crate::bytecode_gen::BytecodeGenerator;

impl PipelineStep for BytecodeGenerator {
    fn run(
        &mut self,
        input: Box<dyn Any>,
        _config: &CompilerConfig,
        _ctx: Rc<RefCell<CompilerContext>>,
        _reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Box<dyn Any>, PipelineError> {
        let intermediate_code = input.downcast::<IntermediateCode>().map_err(|_| {
            PipelineError::new("BytecodeGenerator expected IntermediateCode as input".into())
        })?;

        let bytecode = self.generate(*intermediate_code);

        Ok(Box::new(bytecode))
    }
}
