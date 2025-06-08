use skyl_driver::{ChainedPipeline, InitialStep, PipelineBuilder};
use skyl_lexer::Lexer;
use skyl_parser::Parser;

pub struct ModuleImportPipeline;

impl ModuleImportPipeline {
    pub fn get() -> ChainedPipeline<ChainedPipeline<InitialStep, Lexer>, Parser> {
        let pipeline: ChainedPipeline<ChainedPipeline<InitialStep, Lexer>, Parser> =
            PipelineBuilder::new::<Lexer>().add_step::<Parser>();
        pipeline
    }
}
