use std::any::Any;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use skyl_data::{
    Ast, CompilerConfig, CompilerContext, ContextStack, Decorator, SourceFileID, SymbolKind,
    SymbolTable, TypeDescriptor, Visibility,
};
use skyl_driver::{
    PipelineStep,
    errors::{CompilerErrorReporter, PipelineError},
};

use crate::SemanticAnalyzer;

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        let mut analyzer = Self {
            current_file: SourceFileID(0),
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
            void_instance: Rc::new(RefCell::new(TypeDescriptor::empty(
                SourceFileID(0),
                Visibility::Public,
            ))),
            current_decorator: Decorator::from(Vec::new()),
            ctx: None,
        };

        let archetypes = HashSet::new();
        let fields = HashMap::new();

        analyzer.void_instance = Rc::new(RefCell::new(TypeDescriptor::new(
            SourceFileID(0),
            Visibility::Public,
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
    fn run(
        &mut self,
        input: Box<dyn Any>,
        config: &CompilerConfig,
        ctx: Rc<RefCell<CompilerContext>>,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
    ) -> Result<Box<dyn Any>, PipelineError> {
        self.ctx = Some(ctx);

        let ast = input
            .downcast::<Ast>()
            .map_err(|_| PipelineError::new("SemanticAnalyzer esperava Ast como input".into()))?;

        let code = self.analyze(Rc::clone(&reporter), ast.statements, config);

        Ok(Box::new(code))
    }
}
