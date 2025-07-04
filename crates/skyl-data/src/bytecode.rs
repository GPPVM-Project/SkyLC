use std::{collections::HashMap, rc::Rc};

use crate::{NativeFunctionInfo, objects::Value};

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new(code: Vec<u8>, constants: Vec<Value>) -> Self {
        Self { code, constants }
    }
}

#[derive(Clone, Debug)]
pub struct VirtualFunction {
    pub id: u32,
    pub chunk: Rc<Chunk>,
}

impl VirtualFunction {
    pub fn new(id: u32, chunk: Rc<Chunk>) -> Self {
        Self { id, chunk }
    }
}

#[derive(Clone, Debug)]
pub struct Bytecode {
    pub functions: HashMap<u32, VirtualFunction>,
    pub native_functions: HashMap<String, NativeFunctionInfo>,
    pub v_tables: HashMap<u32, Vec<VirtualFunction>>,
    pub main: Option<VirtualFunction>,
}

impl Bytecode {
    pub fn new(
        functions: HashMap<u32, VirtualFunction>,
        native_functions: HashMap<String, NativeFunctionInfo>,
        v_tables: HashMap<u32, Vec<VirtualFunction>>,
        main: Option<VirtualFunction>,
    ) -> Self {
        Self {
            functions,
            native_functions,
            v_tables,
            main,
        }
    }

    pub fn get_function(&self, function_id: u32) -> Rc<Chunk> {
        self.functions[&function_id].chunk.clone()
    }
}
