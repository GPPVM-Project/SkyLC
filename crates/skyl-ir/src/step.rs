use std::{cell::RefCell, rc::Rc};

use skyl_data::{
    AnnotatedAST, CodeGraph, CompileTimeChunk, IntermediateCode, SemanticCode, SymbolTable,
};
use skyl_driver::{errors::CompilerErrorReporter, PipelineStep};

use crate::{ir_generator::CompileTimeStack, IRGenerator};

impl Default for IRGenerator {
    fn default() -> Self {
        Self {
            semantic_code: SemanticCode::new(SymbolTable::new(), AnnotatedAST::new(Vec::new())),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::empty())),
            functions: Default::default(),
            kinds: Default::default(),
            methods: Default::default(),
            top_level_graph: CodeGraph::new(Default::default()),
            current_chunk: CompileTimeChunk::empty(),
            current_depth: 0,
            local_values: CompileTimeStack::new(),
            current_native_id: 0,
            native_functions: Default::default(),
        }
    }
}

impl PipelineStep for IRGenerator {
    type Input = SemanticCode;

    type Output = IntermediateCode;

    fn run(
        &mut self,
        input: Self::Input,
        _: &skyl_data::CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, skyl_driver::errors::PipelineError> {
        let ir_code = self.generate(reporter, input);
        Ok(ir_code)
    }
}
