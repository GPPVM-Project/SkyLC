use std::{collections::HashMap, rc::Rc};

use skyl_data::{
    bytecode::{Bytecode, Chunk, VirtualFunction},
    objects::Value,
    CompileTimeValue, IntermediateCode,
};
use skyl_driver::gpp_error;

#[derive(Default)]
pub struct BytecodeGenerator;

impl BytecodeGenerator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn generate(&self, ir: IntermediateCode) -> Bytecode {
        let mut functions: HashMap<u32, VirtualFunction> = HashMap::new();
        let mut main: Option<VirtualFunction> = None;
        let mut v_tables: HashMap<u32, Vec<VirtualFunction>> = HashMap::new();

        for function in &ir.functions {
            let code = function.1.chunk.code.clone();
            let mut constants = Vec::new();

            let mut c: Value;
            for constant in &function.1.chunk.constants {
                match constant {
                    CompileTimeValue::Int(v) => {
                        c = Value::Int(*v);
                    }
                    CompileTimeValue::Float(v) => {
                        c = Value::Float(*v);
                    }
                    CompileTimeValue::String(v) => {
                        c = Value::String(Rc::new(v.clone()));
                    }
                    CompileTimeValue::Boolean(v) => {
                        c = Value::Bool(*v);
                    }
                    CompileTimeValue::Object(_) => {
                        gpp_error!("Cannot create complex object constants.");
                    }
                }

                constants.push(c);
            }

            let chunk = Chunk::new(code, constants);
            let virtual_function = VirtualFunction::new(function.1.id, Rc::new(chunk));

            if function.0 == "main" {
                main = Some(virtual_function);
            } else {
                functions.insert(function.1.id, virtual_function);
            }
        }

        for (desc, v_table) in &ir.methods {
            for method in v_table {
                let code = method.chunk.code.clone();
                let mut constants = Vec::new();

                let mut c: Value;

                for constant in &method.chunk.constants {
                    match constant {
                        CompileTimeValue::Int(v) => {
                            c = Value::Int(*v);
                        }
                        CompileTimeValue::Float(v) => {
                            c = Value::Float(*v);
                        }
                        CompileTimeValue::String(v) => {
                            c = Value::String(Rc::new(v.clone()));
                        }
                        CompileTimeValue::Boolean(v) => {
                            c = Value::Bool(*v);
                        }
                        CompileTimeValue::Object(_) => {
                            gpp_error!("Cannot create complex object constants.");
                        }
                    }

                    constants.push(c);
                }

                let chunk = Chunk::new(code, constants);
                let virtual_function = VirtualFunction::new(method.id, Rc::new(chunk));

                if let std::collections::hash_map::Entry::Vacant(e) = v_tables.entry(desc.id) {
                    e.insert(vec![virtual_function]);
                } else {
                    v_tables.get_mut(&desc.id).unwrap().push(virtual_function);
                }
            }
        }

        Bytecode::new(functions, ir.native_functions.clone(), v_tables, main)
    }
}
