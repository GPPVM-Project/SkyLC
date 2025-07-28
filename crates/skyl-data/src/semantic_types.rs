use std::{cell::RefCell, collections::HashMap, default::Default, rc::Rc};

use crate::{
    AnnotatedAST,
    descriptors::{TypeDecl, TypeDescriptor},
    statements::FieldDeclaration,
};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeComposition {
    mask: Vec<TypeDecl>,
}

#[derive(Debug, Clone)]
pub struct ObjectDescriptor {
    pub fields: HashMap<String, TypeComposition>,
}

#[derive(Debug, Clone)]
pub enum ValueWrapper {
    Int(i32),
    Float(f32),
    Boolean(bool),
    String(String),
    Object(ObjectDescriptor),
    Kind,
    Internal,
}

impl ValueWrapper {
    pub fn boolean(value: bool) -> Self {
        ValueWrapper::Boolean(value)
    }

    pub fn float(value: f32) -> Self {
        ValueWrapper::Float(value)
    }

    pub fn int(value: i32) -> Self {
        ValueWrapper::Int(value)
    }

    pub fn as_int(&self) -> Option<i32> {
        if let ValueWrapper::Int(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<String> {
        if let ValueWrapper::String(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn as_float(&self) -> Option<f32> {
        if let ValueWrapper::Float(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        if let ValueWrapper::Boolean(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_object(&self) -> Option<ObjectDescriptor> {
        if let ValueWrapper::Object(v) = self {
            Some(v.clone())
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct SemanticValue {
    pub kind: Option<Rc<RefCell<TypeDescriptor>>>,
    pub value: ValueWrapper,
    pub line: usize,
}

impl SemanticValue {
    pub fn new(
        kind: Option<Rc<RefCell<TypeDescriptor>>>,
        value: ValueWrapper,
        line: usize,
    ) -> Self {
        Self { kind, value, line }
    }
}

#[derive(Clone, Debug)]
pub struct StaticValue {
    pub kind: Rc<RefCell<TypeDescriptor>>,
    pub value: ValueWrapper,
}

impl StaticValue {
    pub fn new(kind: Rc<RefCell<TypeDescriptor>>, value: ValueWrapper) -> Self {
        Self { kind, value }
    }
}

#[derive(Debug, Clone)]
pub struct ContextScope {
    pub names: HashMap<String, SemanticValue>,
}

impl Default for ContextScope {
    fn default() -> Self {
        Self::new()
    }
}

impl ContextScope {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
        }
    }

    pub fn contains_name(&self, name: &String) -> bool {
        self.names.contains_key(name)
    }

    pub fn name(&self, name: &String) -> Option<SemanticValue> {
        if self.contains_name(name) {
            Some(self.names.get(name).unwrap().clone())
        } else {
            None
        }
    }

    pub fn set_infered_kind(&mut self, name: &String, kind: Rc<RefCell<TypeDescriptor>>) {
        self.names.get_mut(name).unwrap().kind = Some(kind);
    }

    pub fn declare_name(&mut self, name: &str, value: SemanticValue) {
        self.names.insert(name.to_string().clone(), value);
    }
}

#[derive(Debug, Clone, Default)]
pub struct ContextStack {
    pub scopes: Vec<ContextScope>,
}

impl ContextStack {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn push_empty(&mut self) {
        self.scopes.push(ContextScope::new());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn peek(&mut self) -> &mut ContextScope {
        self.scopes.last_mut().unwrap()
    }

    pub fn get(&mut self, i: usize) -> &mut ContextScope {
        self.scopes.get_mut(i).unwrap()
    }
}

#[derive(Eq, PartialEq, Default)]
pub enum SymbolKind {
    #[default]
    None,

    Function,
    Kind,
    InternalDefinition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionPrototype {
    pub name: String,
    pub params: Vec<FieldDeclaration>,
    pub arity: usize,
    pub return_kind: Rc<RefCell<TypeDescriptor>>,
}

impl FunctionPrototype {
    pub fn new(
        name: String,
        params: Vec<FieldDeclaration>,
        arity: usize,
        return_kind: Rc<RefCell<TypeDescriptor>>,
    ) -> Self {
        Self {
            name,
            params,
            arity,
            return_kind,
        }
    }
}

impl std::hash::Hash for FunctionPrototype {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl std::hash::Hash for BuiltinAttribute {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct BuiltinAttribute {
    pub name: String,
    pub args: Vec<Rc<RefCell<TypeDescriptor>>>,
}

impl BuiltinAttribute {
    pub fn new(name: String, args: Vec<Rc<RefCell<TypeDescriptor>>>) -> Self {
        Self { name, args }
    }
}

#[derive(Copy, Hash, Debug, Clone, PartialEq, Eq)]
pub enum CoersionKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub struct Operator {
    pub this: u32,
    pub other: u32,
    pub kind: CoersionKind,
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    pub names: HashMap<String, StaticValue>,
    pub functions: HashMap<String, FunctionPrototype>,
    pub native_functions: HashMap<String, FunctionPrototype>,
    pub attributes: HashMap<String, BuiltinAttribute>,
    pub operators: HashMap<Operator, String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            functions: HashMap::new(),
            native_functions: HashMap::new(),
            attributes: HashMap::new(),
            operators: HashMap::new(),
        }
    }

    pub fn get_attribute(&self, name: String) -> Option<&BuiltinAttribute> {
        self.attributes.get(&name)
    }

    pub fn define_attribute(&mut self, name: String, args: Vec<Rc<RefCell<TypeDescriptor>>>) {
        self.attributes
            .insert(name.clone(), BuiltinAttribute::new(name, args));
    }

    pub fn define(&mut self, name: String, value: StaticValue) {
        self.names.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&StaticValue> {
        self.names.get(name)
    }

    pub fn get_function(&mut self, name: &str) -> Option<&mut FunctionPrototype> {
        self.functions.get_mut(name)
    }

    pub fn define_function(&mut self, name: String, value: FunctionPrototype) {
        self.functions.insert(name, value);
    }

    pub fn get_type_by_id(&self, id: u32) -> Option<Rc<RefCell<TypeDescriptor>>> {
        let types = self.names.iter().find(|(_, v)| v.kind.borrow().id == id);
        types.map(|(_, s)| s.kind.clone())
    }
}

#[derive(Debug, Clone)]
pub struct SemanticCode {
    pub table: SymbolTable,
    pub ast: AnnotatedAST,
}

impl SemanticCode {
    pub fn new(table: SymbolTable, ast: AnnotatedAST) -> Self {
        SemanticCode { table, ast }
    }

    pub fn get_table(&self) -> &SymbolTable {
        &self.table
    }
}
