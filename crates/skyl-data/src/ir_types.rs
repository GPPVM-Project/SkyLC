use std::collections::HashMap;

use crate::{CompileTimeChunk, TypeDescriptor};

#[derive(Debug, Clone)]
pub struct NativeFunctionInfo {
    pub arity: u8,
    pub id: u32,
}

impl NativeFunctionInfo {
    pub fn new(arity: u8, id: u32) -> Self {
        Self { arity, id }
    }
}

#[derive(Debug, Clone)]
pub struct IRType {
    pub id: u32,
    pub fields: HashMap<String, u8>,
    pub chunk: CompileTimeChunk,
}

#[derive(Debug, Clone)]
pub struct IRFunction {
    pub id: u32,
    pub name: String,
    pub chunk: CompileTimeChunk,
    pub arity: u8,
}

impl IRFunction {
    pub fn new(id: u32, name: String, chunk: CompileTimeChunk, arity: u8) -> Self {
        Self {
            id,
            name,
            chunk,
            arity,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodeGraph {
    pub connections: HashMap<String, u32>,
    pub inverse_connections: HashMap<u32, String>,
    pub current_id: u32,
}

impl CodeGraph {
    pub fn new(connections: HashMap<String, u32>) -> Self {
        Self {
            connections,
            inverse_connections: HashMap::new(),
            current_id: 0,
        }
    }

    pub fn get_id_for_new_edge(&mut self, name: String) -> u32 {
        let id = self.current_id;
        self.current_id += 1;

        self.connections.insert(name.clone(), id);
        self.inverse_connections.insert(id, name);

        return id;
    }

    pub fn get_function_id(&self, name: &str) -> u32 {
        self.connections[name]
    }
}

#[derive(Debug, Clone)]
pub struct IntermediateCode {
    pub functions: HashMap<String, IRFunction>,
    pub native_functions: HashMap<String, NativeFunctionInfo>,
    pub methods: HashMap<TypeDescriptor, Vec<IRFunction>>,
    pub kinds: HashMap<String, IRType>,
    pub graph: CodeGraph,
}

impl IntermediateCode {
    pub fn new(
        functions: HashMap<String, IRFunction>,
        native_functions: HashMap<String, NativeFunctionInfo>,
        methods: HashMap<TypeDescriptor, Vec<IRFunction>>,
        kinds: HashMap<String, IRType>,
        graph: CodeGraph,
    ) -> Self {
        Self {
            functions,
            native_functions,
            methods,
            kinds,
            graph,
        }
    }
}
