use std::cell::RefCell;
use std::rc::Rc;

use skyl_data::{CompilerConfig, CompilerContext};

pub mod errors;
pub mod format_err;

use crate::errors::{CompilerErrorReporter, PipelineError};

pub trait PipelineStep {
    fn run(
        &mut self,
        input: Box<dyn std::any::Any>,
        config: &CompilerConfig,
        ctx: Rc<RefCell<CompilerContext>>,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Box<dyn std::any::Any>, PipelineError>;
}

pub struct Pipeline {
    stages: Vec<Box<dyn PipelineStep>>,
}

impl Default for Pipeline {
    fn default() -> Self {
        Self::new()
    }
}

impl Pipeline {
    pub fn new() -> Self {
        Pipeline { stages: Vec::new() }
    }

    pub fn add_stage(mut self, stage: Box<dyn PipelineStep>) -> Self {
        self.stages.push(stage);
        self
    }

    pub fn execute<T: 'static>(
        &mut self,
        input: T,
        config: &CompilerConfig,
        ctx: Rc<RefCell<CompilerContext>>,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Box<dyn std::any::Any>, PipelineError> {
        let mut value: Box<dyn std::any::Any> = Box::new(input);

        for stage in &mut self.stages {
            value = stage.run(value, config, ctx.clone(), Rc::clone(&reporter))?;
        }

        Ok(value)
    }
}
