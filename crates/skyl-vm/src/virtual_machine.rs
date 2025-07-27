#![allow(dead_code)]

use core::fmt::Debug;
use std::{cell::RefCell, rc::Rc};

use skyl_data::{
    CompilerConfig, Instruction,
    bytecode::{Bytecode, Chunk},
    memory::GcRef,
    objects::{Instance, List, Object, Value},
};
use skyl_driver::gpp_error;
use skyl_ffi::{NativeBridge, NativeFnPtr, NativeFunction, NativeLibrary};

use crate::{frame::Frame, heap::Heap};

const INITIAL_HEAP_SIZE: usize = 1024; // 1KB

#[derive(Debug)]
pub struct VirtualMachine {
    pub ip: usize,
    pub sp: usize,
    pub fp: usize,
    pub stack: Vec<Value>,
    pub bytecode: Option<Bytecode>,
    config: CompilerConfig,
    native_functions: Vec<NativeFunction>,
    frame_stack: Vec<RefCell<Frame>>,
    chunk: Rc<Chunk>,
    unsafe_mode: bool,
    heap: Heap,
    halted: bool,
}

impl NativeBridge for VirtualMachine {
    fn bind(&mut self, name: &str, func: NativeFnPtr) {
        let func_info = &self.bytecode.as_ref().unwrap().native_functions.get(name);

        match func_info {
            Some(info) => {
                let index = info.id;

                if self.config.verbose {
                    println!("Linking: {} function.", name);
                }

                self.native_functions[index as usize] = NativeFunction::new(func, info.arity);
            }

            None => {
                if !self.unsafe_mode {
                    gpp_error!(
                        "Unsafe config: {}. Linkage of '{}' function failed. Can't found native definition.",
                        self.unsafe_mode,
                        name
                    )
                }
            }
        }
    }
}

impl VirtualMachine {
    pub fn new(config: &CompilerConfig) -> Self {
        let heap = Heap::new(INITIAL_HEAP_SIZE);

        let vm = Self {
            ip: 0,
            sp: 0,
            fp: 0,
            chunk: Rc::new(Chunk::new(vec![], vec![])),
            config: config.clone(),
            stack: vec![Value::Void; 255],
            frame_stack: Vec::new(),
            native_functions: Vec::new(),
            bytecode: None,
            unsafe_mode: true,
            heap,
            halted: false,
        };

        vm
    }

    #[inline]
    pub fn handle_add(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Float(a + b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Int(a + b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Float((a as f32) + b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Float(a + (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_and(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Bool(a), Value::Bool(b)) => {
                self.push(Value::Bool(a && b));
            }

            _ => {
                gpp_error!("Attempt to handle 'and' in not bool values.");
            }
        }
    }

    #[inline]
    fn handle_eq(&mut self) {
        let a = self.pop();
        let b = self.pop();

        self.push(Value::Bool(a == b));
    }

    #[inline]
    fn handle_neq(&mut self) {
        let a = self.pop();
        let b = self.pop();

        self.push(Value::Bool(a != b));
    }

    #[inline]
    pub fn handle_or(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Bool(a), Value::Bool(b)) => {
                self.push(Value::Bool(a || b));
            }

            _ => {
                gpp_error!("Attempt to handle 'or' in not bool values.");
            }
        }
    }

    #[inline]
    pub fn handle_sub(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Float(a - b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Int(a - b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Float((a as f32) - b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Float(a - (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_div(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Float(a / b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Int(a / b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Float((a as f32) / b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Float(a / (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_mul(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Float(a * b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Int(a * b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Float((a as f32) * b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Float(a * (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_push(&mut self) {
        let constant_index = self.read_u16();
        let constant = self.chunk.constants[constant_index as usize].clone();

        self.push(constant);
    }

    #[inline]
    pub fn handle_pop(&mut self) {
        self.pop();
    }

    #[inline]
    pub fn handle_void(&mut self) {
        self.push(Value::Void);
    }

    #[inline]
    pub fn handle_greater(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Bool(a > b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Bool(a > b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Bool((a as f32) > b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Bool(a > (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_less(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Bool(a < b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Bool(a < b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Bool((a as f32) < b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Bool(a < (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_greater_equal(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Bool(a >= b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Bool(a >= b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Bool((a as f32) >= b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Bool(a >= (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_less_equal(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Bool(a <= b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Bool(a <= b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Bool((a as f32) <= b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Bool(a <= (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_cmp(&mut self) {
        let a = self.pop();
        let b = self.pop();

        match (a, b) {
            (Value::Float(a), Value::Float(b)) => {
                self.push(Value::Bool(a == b));
            }
            (Value::Int(a), Value::Int(b)) => {
                self.push(Value::Bool(a == b));
            }
            (Value::Int(a), Value::Float(b)) => {
                self.push(Value::Bool((a as f32) == b));
            }
            (Value::Float(a), Value::Int(b)) => {
                self.push(Value::Bool(a == (b as f32)));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_true(&mut self) {
        self.push(Value::Bool(true));
    }

    #[inline]
    pub fn handle_false(&mut self) {
        self.push(Value::Bool(false));
    }

    #[inline]
    pub fn handle_get_field(&mut self) {
        let object = self.pop();
        let index = self.read_byte();

        if let Value::Object(obj_ptr) = object {
            self.push(
                obj_ptr
                    .borrow()
                    .as_any()
                    .downcast_ref::<Instance>()
                    .unwrap()
                    .fields[index as usize]
                    .clone(),
            );
        }
    }

    pub fn handle_set_field(&mut self) {
        let value = self.pop();
        let mut object = self.pop();
        let index = self.read_byte();

        if let Value::Object(obj_ptr) = &mut object {
            obj_ptr
                .borrow_mut()
                .as_any_mut()
                .downcast_mut::<Instance>()
                .unwrap()
                .fields[index as usize] = value;
        }
    }

    #[inline]
    pub fn handle_new(&mut self) {
        let arity = self.read_byte() as usize;

        self.sp -= arity;

        let fields = self.stack[self.sp..self.sp + arity]
            .iter()
            .cloned()
            .collect::<Vec<_>>();

        let obj = Value::Object(Rc::new(RefCell::new(Instance::new(fields))));
        self.push(obj);
    }

    #[inline]
    pub fn handle_print(&mut self) {
        let value = self.pop();

        match value {
            Value::Bool(b) => println!("{}", b),
            Value::Int(i) => println!("{}", i),
            Value::Float(f) => println!("{}", f),
            Value::String(s) => println!("{}", s),
            Value::Object(obj) => {
                println!("{}", obj.borrow().to_string());
            }
            _ => todo!(),
        }
    }

    #[inline]
    pub fn handle_set_local(&mut self) {
        let value = self.pop();
        let index = self.fp + (self.read_byte() as usize);
        self.stack[index as usize] = value;
    }

    #[inline]
    pub fn handle_get_local(&mut self) {
        let index = self.read_byte();
        let value = &self.stack[self.fp + (index as usize)];
        self.push(value.clone());
    }

    #[inline]
    pub fn handle_get_local0(&mut self) {
        let value = &self.stack[self.fp];
        self.push(value.clone());
    }

    #[inline]
    pub fn handle_get_local1(&mut self) {
        let value = &self.stack[self.fp + 1];
        self.push(value.clone());
    }

    #[inline]
    pub fn handle_get_local2(&mut self) {
        let value = &self.stack[self.fp + 2];
        self.push(value.clone());
    }

    #[inline]
    pub fn handle_get_local3(&mut self) {
        let value = &self.stack[self.fp + 3];
        self.push(value.clone());
    }

    #[inline]
    pub fn handle_get_local4(&mut self) {
        let value = &self.stack[self.fp + 4];
        self.push(value.clone());
    }

    #[inline]
    pub fn handle_increment_local(&mut self) {
        let index = self.fp + (self.read_byte() as usize);
        let value = &self.stack[index];

        if let Value::Int(i) = value {
            self.stack[index as usize] = Value::Int(i + 1);
        }
    }

    #[inline]
    pub fn handle_decrement_local(&mut self) {
        let index = self.fp + (self.read_byte() as usize);
        let value = &self.stack[index];

        if let Value::Int(i) = value {
            self.stack[index] = Value::Int(i - 1);
        } else {
            print!("VM Debug Data: ");
            self.print_stack();
            panic!("Cannot decrement '{}' value.", value);
        }
    }

    #[inline]
    pub fn handle_call(&mut self) {
        let index = self.read_u32();
        let arity = self.read_byte();

        self.attach_fn(index, arity);
    }

    #[inline]
    pub fn handle_invoke_virtual(&mut self) {
        let v_table_index = self.read_u32();
        let function_index = self.read_u32();
        let arity = self.read_byte();

        self.attach_method(v_table_index, function_index, arity);
    }

    #[inline]
    pub fn handle_invoke_native(&mut self) {
        let index = self.read_u32();
        let arity = self.read_byte();

        let mut args: Vec<Value> = Vec::new();

        self.sp -= arity as usize;

        for i in 0..arity as usize {
            args.push(self.stack[self.sp + i].clone());
        }

        let function = &self.native_functions[index as usize];
        let value = (function.handler)(args);

        if let Value::Void = value {
        } else {
            self.push(value);
        }
    }

    #[inline]
    pub fn handle_return(&mut self) {
        if self.frame_stack.len() == 1 {
            self.handle_halt();
            return;
        }

        let ret_value = self.pop();

        if let Value::Void = ret_value {
            self.detach_fn();
        } else {
            self.detach_fn();
            self.push(ret_value);
        }
    }

    #[inline]
    pub fn handle_loop(&mut self) {
        let offset = self.read_u32();
        self.ip -= offset as usize;
    }

    #[inline]
    pub fn handle_jump(&mut self) {
        let offset = self.read_u32();
        self.ip += offset as usize;
    }

    #[inline]
    pub fn handle_jfalse(&mut self) {
        let offset = self.read_u32();
        let value = self.pop();

        if let Value::Bool(b) = value {
            if !b {
                self.ip += offset as usize;
            }
        }
    }

    #[inline]
    pub fn handle_jtrue(&mut self) {
        let offset = self.read_u32();
        let value = self.pop();

        if let Value::Bool(b) = value {
            if b {
                self.ip += offset as usize;
            }
        }
    }

    #[inline]
    pub fn handle_not(&mut self) {
        let value = self.pop();

        match value {
            Value::Float(f) => {
                self.push(Value::Float(-f));
            }
            Value::Int(i) => {
                self.push(Value::Int(-i));
            }
            Value::Bool(b) => {
                self.push(Value::Bool(!b));
            }
            _ => {}
        }
    }

    #[inline]
    pub fn handle_list_get(&mut self) {
        let index = self.pop();
        let list = self.pop();

        if let Value::Object(obj_ptr) = list {
            let instance = obj_ptr.borrow();
            let list = instance.as_any().downcast_ref::<List>().unwrap();

            if let Value::Int(i) = index {
                if (i as usize) >= list.elements.len() {
                    println!(
                        "error: index {} out of bounds for {:?} with length {}.",
                        i,
                        list.elements,
                        list.elements.len()
                    );
                    std::process::exit(0);
                } else {
                    self.push(list.elements[i as usize].clone());
                }
            }
        }
    }

    #[inline]
    pub fn handle_array(&mut self) {
        let arity = self.read_byte();

        let mut elements: Vec<Value> = Vec::new();

        self.sp -= arity as usize;

        for i in 0..arity as usize {
            elements.push(self.stack[self.sp + i].clone());
        }

        let list = List::new(elements);
        self.push(Value::Object(Rc::new(RefCell::new(list))));
    }

    fn allocate_object<T: Object + 'static>(&mut self, obj: T) -> GcRef {
        self.heap.allocate(obj, |heap| {
            heap.collect_garbage(&mut self.stack);
        })
    }

    fn invalidate_native_call(_: Vec<Value>) -> Value {
        println!("Invalid native call index detected!");
        std::process::exit(0);
    }

    pub fn attach_bytecode(&mut self, bytecode: &Bytecode) {
        self.bytecode = Some(bytecode.clone());
        self.attach_main_fn();

        let native_fn_max_id = bytecode
            .native_functions
            .iter()
            .max_by_key(|n| n.1.id)
            .unwrap()
            .1
            .id;

        // TODO: Fix the native functions detection
        self.native_functions = vec![
            NativeFunction::new(Self::invalidate_native_call, 0);
            native_fn_max_id as usize + 1
        ];
    }

    pub fn interpret(&mut self) {
        self.attach_main_fn();

        if self.config.verbose {
            println!("{}", "=".repeat(60));
            println!("Running code");
            println!("{}", "=".repeat(60));
        }

        let timer = std::time::Instant::now();

        self.execution_loop();

        self.heap.clear();

        if self.config.verbose {
            println!("Virtual machine took: {:?}", timer.elapsed());
            println!(
                "Heap finished with: {} allocated bytes.",
                self.heap.allocated_bytes()
            );
        }
    }

    #[inline]
    fn execution_loop(&mut self) {
        loop {
            let byte = self.read_byte();
            let instruction = unsafe { std::mem::transmute::<u8, Instruction>(byte) };

            if self.halted {
                println!(
                    "Process finished with code {}.",
                    self.stack[self.fp].to_string()
                );
                break;
            }

            match instruction {
                Instruction::Add => self.handle_add(),
                Instruction::And => self.handle_and(),
                Instruction::Or => self.handle_or(),
                Instruction::Sub => self.handle_sub(),
                Instruction::Mul => self.handle_mul(),
                Instruction::Div => self.handle_div(),
                Instruction::Greater => self.handle_greater(),
                Instruction::Less => self.handle_less(),
                Instruction::Eq => self.handle_eq(),
                Instruction::Neq => self.handle_neq(),
                Instruction::GreaterEqual => self.handle_greater_equal(),
                Instruction::LessEqual => self.handle_less_equal(),
                Instruction::Cmp => self.handle_cmp(),
                Instruction::Push => self.handle_push(),
                Instruction::Pop => self.handle_pop(),
                Instruction::Jump => self.handle_jump(),
                Instruction::JTrue => self.handle_jtrue(),
                Instruction::JFalse => self.handle_jfalse(),
                Instruction::Ret => self.handle_return(),
                Instruction::Print => self.handle_print(),
                Instruction::Call => self.handle_call(),
                Instruction::InvokeNative => self.handle_invoke_native(),
                Instruction::True => self.handle_true(),
                Instruction::False => self.handle_false(),
                Instruction::GetLocal => self.handle_get_local(),
                Instruction::GetLocal0 => self.handle_get_local0(),
                Instruction::GetLocal1 => self.handle_get_local1(),
                Instruction::GetLocal2 => self.handle_get_local2(),
                Instruction::GetLocal3 => self.handle_get_local3(),
                Instruction::GetLocal4 => self.handle_get_local4(),
                Instruction::SetLocal => self.handle_set_local(),
                Instruction::Void => self.handle_void(),
                Instruction::DecrementLocal => self.handle_decrement_local(),
                Instruction::IncrementLocal => self.handle_increment_local(),
                Instruction::Loop => self.handle_loop(),
                Instruction::New => self.handle_new(),
                Instruction::GetField => self.handle_get_field(),
                Instruction::SetField => self.handle_set_field(),
                Instruction::Array => self.handle_array(),
                Instruction::ListGet => self.handle_list_get(),
                Instruction::Not => self.handle_not(),
                Instruction::InvokeVirtual => self.handle_invoke_virtual(),
                Instruction::Halt => {
                    self.handle_halt();
                    break;
                }
                _ => panic!("Unimplemented instruction: {:?}", instruction),
            }
        }
    }

    fn pop(&mut self) -> Value {
        self.sp -= 1;
        std::mem::replace(&mut self.stack[self.sp as usize], Value::Void)
    }

    fn read_u16(&mut self) -> u16 {
        let byte1 = self.read_byte();
        let byte2 = self.read_byte();
        ((byte1 as u16) << 8) | (byte2 as u16)
    }

    fn read_u32(&mut self) -> u32 {
        let byte1 = self.read_byte();
        let byte2 = self.read_byte();
        let byte3 = self.read_byte();
        let byte4 = self.read_byte();

        let value = ((byte1 as u32) << 24)
            | ((byte2 as u32) << 16)
            | ((byte3 as u32) << 8)
            | (byte4 as u32);
        value
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code[self.ip];
        self.ip += 1;

        byte
    }

    fn push(&mut self, value: Value) {
        self.stack[self.sp as usize] = value;
        self.sp += 1;
    }

    #[allow(dead_code)]
    fn print_stack(&self) {
        print!("Stack [");

        for i in 0..self.sp {
            if let Value::Object(obj_ptr) = &self.stack[i] {
                print!("{}, ", obj_ptr.borrow().to_string());
            } else {
                print!("{:?}, ", self.stack[i]);
            }
        }

        println!("]");
    }

    fn attach_main_fn(&mut self) {
        let main = Frame::new(self.bytecode.clone().unwrap().main.unwrap().chunk);
        self.chunk = main.chunk.clone();
        self.frame_stack.push(RefCell::new(main));
        self.ip = 0;
    }

    fn attach_fn(&mut self, function_id: u32, arity: u8) {
        let code = self.bytecode.clone();
        let chunk = code.unwrap().get_function(function_id);

        self.chunk = chunk.clone();

        let frame = Frame::new(chunk);

        self.frame_stack
            .last()
            .unwrap()
            .borrow_mut()
            .set_ip(self.ip);
        self.frame_stack
            .last()
            .unwrap()
            .borrow_mut()
            .set_sp(self.sp - (arity as usize));
        self.frame_stack.push(RefCell::new(frame));
        self.frame_stack
            .last()
            .unwrap()
            .borrow_mut()
            .set_fp(self.sp - (arity as usize));
        self.ip = 0;
        self.fp = self.sp - (arity as usize);
    }

    fn attach_method(&mut self, v_table_id: u32, method_id: u32, arity: u8) {
        let code = self.bytecode.clone();
        let chunk = code.unwrap().v_tables[&v_table_id][method_id as usize]
            .chunk
            .clone();

        self.chunk = chunk.clone();

        let frame = Frame::new(chunk);

        self.frame_stack
            .last()
            .unwrap()
            .borrow_mut()
            .set_ip(self.ip);
        self.frame_stack
            .last()
            .unwrap()
            .borrow_mut()
            .set_sp(self.sp - (arity as usize));
        self.frame_stack.push(RefCell::new(frame));
        self.frame_stack
            .last()
            .unwrap()
            .borrow_mut()
            .set_fp(self.sp - (arity as usize));
        self.ip = 0;
        self.fp = self.sp - (arity as usize);
    }

    fn detach_fn(&mut self) {
        self.frame_stack.pop();

        if self.frame_stack.is_empty() {
            self.handle_halt();
            return;
        }

        self.chunk = self.frame_stack.last().unwrap().borrow().chunk.clone();

        self.ip = self.frame_stack.last().unwrap().borrow().ip;
        self.sp = self.frame_stack.last().unwrap().borrow().sp;
        self.fp = self.frame_stack.last().unwrap().borrow().fp;
    }

    pub fn load_library(&mut self, lib: &mut dyn NativeLibrary) {
        lib.register_functions(self);
    }

    fn print_info(&self) {
        println!("{:?}", self.frame_stack);
    }

    fn handle_halt(&mut self) {
        self.halted = true;
    }
}
