use std::any::{type_name, Any};
use std::{cell::RefCell, rc::Rc};

use skyl_data::{
    AnnotatedAST, CodeGraph, CompileTimeChunk, CompilerConfig, IntermediateCode, SemanticCode,
    SymbolTable,
};
use skyl_driver::{errors::CompilerErrorReporter, errors::PipelineError, PipelineStep};

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
    fn run(
        &mut self,
        input: Box<dyn Any>,
        config: &CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Box<dyn Any>, PipelineError> {
        let semantic_code = input.downcast::<SemanticCode>().map_err(|_| {
            PipelineError::new("[E1023] IRGenerator expected SemanticCode as input".into())
        })?;

        let ir = self.generate(reporter, *semantic_code);

        Ok(Box::new(ir))
    }
}
