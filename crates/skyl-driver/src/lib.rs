pub mod errors;

use std::cell::RefCell;
use std::rc::Rc;

use skyl_data::CompilerConfig;

use crate::errors::{handle_errors, CompilerErrorReporter, PipelineError};

pub trait PipelineStep: Default {
    type Input;
    type Output;

    fn run(
        &mut self,
        input: Self::Input,
        config: &CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, PipelineError>;
}

pub trait ExecutablePipeline<Input> {
    type Output;
    fn execute(
        &mut self,
        input: Input,
        config: &CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, PipelineError>;
}

pub struct PipelineBuilder;

impl PipelineBuilder {
    pub fn new<S>() -> ChainedPipeline<InitialStep, S>
    where
        S: PipelineStep<Input = String> + Default,
    {
        ChainedPipeline {
            previous_pipeline: InitialStep,
            step: S::default(),
        }
    }
}

pub struct ChainedPipeline<P, S> {
    previous_pipeline: P,
    step: S,
}

impl<P, S> ChainedPipeline<P, S> {
    pub fn add_step<NextS>(self) -> ChainedPipeline<Self, NextS>
    where
        Self: ExecutablePipeline<String>,
        NextS: PipelineStep<Input = <Self as ExecutablePipeline<String>>::Output> + Default,
    {
        ChainedPipeline {
            previous_pipeline: self,
            step: NextS::default(),
        }
    }
}

impl<P, S, Input> ExecutablePipeline<Input> for ChainedPipeline<P, S>
where
    P: ExecutablePipeline<Input>,
    S: PipelineStep<Input = <P as ExecutablePipeline<Input>>::Output>,
{
    type Output = S::Output;

    fn execute(
        &mut self,
        input: Input,
        config: &CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, PipelineError> {
        let intermediate_result =
            self.previous_pipeline
                .execute(input, &config, Rc::clone(&reporter))?;

        handle_errors(&reporter.borrow());

        self.step.run(intermediate_result, config, reporter)
    }
}

#[derive(Default)]
pub struct InitialStep;

impl<T: Clone> ExecutablePipeline<T> for InitialStep {
    type Output = T;
    fn execute(
        &mut self,
        input: T,
        _config: &CompilerConfig,
        _reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, PipelineError> {
        Ok(input.clone())
    }
}
