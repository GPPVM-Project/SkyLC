use skyl_driver::Pipeline;
use skyl_lexer::Lexer;
use skyl_parser::Parser;

pub struct ModuleImportPipeline;

impl ModuleImportPipeline {
    pub fn get() -> Pipeline {
        Pipeline::new()
            .add_stage(Box::new(Lexer::without_source()))
            .add_stage(Box::new(Parser::new()))
    }
}
