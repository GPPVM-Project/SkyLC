use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use skyl_data::{
    Ast, CompilerConfig, ContextStack, SemanticCode, SymbolKind, SymbolTable, TypeDescriptor,
};
use skyl_driver::{PipelineStep, errors::CompilerErrorReporter};

use crate::SemanticAnalyzer;

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        let mut analyzer = Self {
            modules: Vec::new(),
            current_descriptor_id: None,
            current_return_kind_id: None,
            config: CompilerConfig::empty(),
            statements: Vec::new(),
            context_stack: ContextStack::new(),
            current_stmt: 0,
            current_symbol: String::new(),
            symbol_table: SymbolTable::new(),
            current_static_id: 1u32,
            current_symbol_kind: SymbolKind::None,
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::empty())),
            void_instance: Rc::new(RefCell::new(TypeDescriptor::empty())),
        };

        let archetypes = HashSet::new();
        let fields = HashMap::new();

        analyzer.void_instance = Rc::new(RefCell::new(TypeDescriptor::new(
            "void".to_string(),
            archetypes,
            fields,
            0,
        )));

        analyzer.initialize_predefined_types();

        analyzer
    }
}

impl PipelineStep for SemanticAnalyzer {
    type Input = Ast;

    type Output = SemanticCode;

    fn run(
        &mut self,
        input: Self::Input,
        config: &CompilerConfig,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Self::Output, skyl_driver::errors::PipelineError> {
        let code = self.analyze(reporter, input.statements, config);
        Ok(code)
    }
}
