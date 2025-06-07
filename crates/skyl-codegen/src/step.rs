use skyl_data::{bytecode::Bytecode, IntermediateCode};
use skyl_driver::PipelineStep;

use crate::bytecode_gen::BytecodeGenerator;

impl Default for BytecodeGenerator {
    fn default() -> Self {
        Self {}
    }
}

impl PipelineStep for BytecodeGenerator {
    type Input = IntermediateCode;

    type Output = Bytecode;

    fn run(
        &mut self,
        input: Self::Input,
        _: &skyl_data::CompilerConfig,
        _: std::rc::Rc<std::cell::RefCell<skyl_driver::errors::CompilerErrorReporter>>,
    ) -> Result<Self::Output, skyl_driver::errors::PipelineError> {
        let bytecode = self.generate(input);

        Ok(bytecode)
    }
}
