use std::{cell::RefCell, rc::Rc};

use skyl_data::{CompilerConfig, ContextStack, Statement, SymbolKind, SymbolTable, TypeDescriptor};
use skyl_driver::errors::CompilerErrorReporter;

pub struct SemanticContext {
    pub(crate) statements: Vec<Statement>,
    pub(crate) context_stack: ContextStack,
    pub(crate) config: CompilerConfig,
    pub(crate) symbol_table: SymbolTable,
    pub(crate) current_stmt: usize,
    pub(crate) current_symbol: String,
    pub(crate) current_descriptor_id: Option<u32>,
    pub(crate) current_return_kind_id: Option<u32>,
    pub(crate) current_static_id: u32,
    pub(crate) current_symbol_kind: SymbolKind,
    pub(crate) reporter: Rc<RefCell<CompilerErrorReporter>>,
    pub(crate) void_instance: Rc<RefCell<TypeDescriptor>>,
    pub(crate) modules: Vec<String>,
}

impl SemanticContext {
    pub fn new(
        statements: Vec<Statement>,
        context_stack: ContextStack,
        config: CompilerConfig,
        symbol_table: SymbolTable,
        current_stmt: usize,
        current_symbol: String,
        current_descriptor_id: Option<u32>,
        current_return_kind_id: Option<u32>,
        current_static_id: u32,
        current_symbol_kind: SymbolKind,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
        void_instance: Rc<RefCell<TypeDescriptor>>,
        modules: Vec<String>,
    ) -> Self {
        Self {
            statements,
            context_stack,
            config,
            symbol_table,
            current_stmt,
            current_symbol,
            current_descriptor_id,
            current_return_kind_id,
            current_static_id,
            current_symbol_kind,
            reporter,
            void_instance,
            modules,
        }
    }
}
