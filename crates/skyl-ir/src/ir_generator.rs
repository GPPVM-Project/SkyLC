use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use skyl_data::{
    AnnotatedAST, AnnotatedExpression, AnnotatedStatement, CodeGraph, CompileTimeChunk,
    CompileTimeValue, FunctionPrototype, IRFunction, IRType, Instruction, IntermediateCode,
    Literal, MethodDescriptor, NativeFunctionInfo, OperatorKind, SemanticCode, SymbolTable, Token,
    TokenKind, TypeDescriptor,
};
use skyl_driver::{errors::CompilerErrorReporter, gpp_error};

#[derive(Debug, Clone)]
struct LocalValue {
    name: String,
    depth: u32,
    _is_initialized: bool,
}

impl LocalValue {
    fn new(name: String, depth: u32, is_initialized: bool) -> Self {
        Self {
            name,
            depth,
            _is_initialized: is_initialized,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompileTimeStack {
    values: Vec<LocalValue>,
}

impl CompileTimeStack {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    fn push(&mut self, value: LocalValue) {
        self.values.push(value);
    }

    // fn pop(&mut self) -> LocalValue {
    //     self.values.pop().unwrap()
    // }

    // pub fn count(&self) -> usize {
    //     self.values.len()
    // }
}

pub struct IRGenerator {
    pub(crate) semantic_code: SemanticCode,
    pub(crate) reporter: Rc<RefCell<CompilerErrorReporter>>,
    pub(crate) top_level_graph: CodeGraph,
    pub(crate) functions: HashMap<String, IRFunction>,
    pub(crate) native_functions: HashMap<String, NativeFunctionInfo>,
    pub(crate) kinds: HashMap<String, IRType>,
    pub(crate) methods: HashMap<TypeDescriptor, Vec<IRFunction>>,
    pub(crate) current_chunk: CompileTimeChunk,
    pub(crate) local_values: CompileTimeStack,
    pub(crate) current_depth: u32,
    pub(crate) current_native_id: u32,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            semantic_code: SemanticCode::new(SymbolTable::new(), AnnotatedAST::new(Vec::new())),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::empty())),
            functions: HashMap::new(),
            kinds: HashMap::new(),
            methods: HashMap::new(),
            top_level_graph: CodeGraph::new(HashMap::new()),
            current_chunk: CompileTimeChunk::empty(),
            current_depth: 0,
            local_values: CompileTimeStack::new(),
            current_native_id: 0,
            native_functions: HashMap::new(),
        }
    }

    pub fn generate(
        &mut self,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
        semantic_code: SemanticCode,
    ) -> IntermediateCode {
        self.semantic_code = semantic_code;
        self.reporter = reporter;

        for annotated_stmt in self.semantic_code.ast.statements.clone() {
            self.generate_ir_for(&annotated_stmt);
        }

        IntermediateCode::new(
            self.functions.clone(),
            self.native_functions.clone(),
            self.methods.clone(),
            self.kinds.clone(),
            self.top_level_graph.clone(),
        )
    }

    fn generate_ir_for(&mut self, annotated_stmt: &AnnotatedStatement) -> Vec<u8> {
        match annotated_stmt {
            AnnotatedStatement::Decorator(_, _, _, _) => {
                vec![]
            }
            AnnotatedStatement::EndCode => {
                vec![]
            }
            AnnotatedStatement::Expression(expression, _, _) => {
                let code = self.generate_expr_ir(expression);
                code
            }
            AnnotatedStatement::While(condition, body, _, _) => {
                self.generate_while_ir(condition, body)
            }
            AnnotatedStatement::ForEach(_, _, _, _, _) => {
                vec![]
            }
            AnnotatedStatement::Function(prototype, body, _, _) => {
                self.generate_function_ir(prototype, body)
            }
            AnnotatedStatement::NativeFunction(prototype, _, _) => {
                self.generate_native_function_ir(prototype)
            }
            AnnotatedStatement::Global => {
                vec![]
            }
            AnnotatedStatement::If(keyword, condition, then_branch, else_branch, _, _) => {
                self.generate_if_else(keyword, condition, then_branch, else_branch)
            }
            AnnotatedStatement::Return(value, _, _) => self.generate_return_ir(value),
            AnnotatedStatement::Scope(statements, _, _) => self.generate_scope_ir(statements),
            AnnotatedStatement::Type(descriptor, _, _) => {
                self.generate_type_ir(&descriptor.borrow())
            }
            AnnotatedStatement::Variable(name, value, _, _) => {
                self.generate_variable_decl_ir(name, value)
            }

            AnnotatedStatement::BuiltinAttribute(_, _, _, _) => {
                // println!("Attribute: {}", name.lexeme);
                vec![]
            }
            AnnotatedStatement::InternalDefinition(target, definition, body, _, _) => {
                self.generate_internal_definition_ir(&target.borrow(), definition, body)
            }
        }
    }

    // fn is_valid_ir_statement(&self, statement: &AnnotatedStatement) -> bool {
    //     matches!(
    //         statement,
    //         AnnotatedStatement::Expression(_, _, _)
    //             | AnnotatedStatement::If(_, _, _, _, _, _)
    //             | AnnotatedStatement::ForEach(_, _, _, _, _)
    //             | AnnotatedStatement::Return(_, _, _)
    //             | AnnotatedStatement::Scope(_, _, _)
    //             | AnnotatedStatement::While(_, _, _, _)
    //             | AnnotatedStatement::Variable(_, _, _, _)
    //     )
    // }

    fn generate_function_ir(
        &mut self,
        prototype: &FunctionPrototype,
        body: &AnnotatedStatement,
    ) -> Vec<u8> {
        self.current_chunk = CompileTimeChunk::empty();
        let mut code = Vec::new();

        self.begin_scope();

        for (_, param) in prototype.params.iter().enumerate() {
            self.declare_local(param.name.lexeme.clone(), true);
        }

        if let AnnotatedStatement::Scope(stmts, _, _) = body {
            for stmt in stmts {
                let stmt_code = self.generate_ir_for(stmt);

                for byte in stmt_code {
                    self.emit_byte(&mut code, byte);
                }
            }
        }

        self.end_scope(&mut code);

        if prototype.name.cmp(&String::from("main")) == Ordering::Equal {
            self.emit_instruction(&mut code, Instruction::Halt);
        } else {
            self.emit_instruction(&mut code, Instruction::Void);
            self.emit_instruction(&mut code, Instruction::Ret);
        }

        self.current_chunk.code = code.clone();

        let ir_function = IRFunction::new(
            self.top_level_graph
                .get_id_for_new_edge(prototype.name.clone()),
            prototype.name.clone(),
            self.current_chunk.clone(),
            prototype.arity as u8,
        );

        self.functions.insert(prototype.name.clone(), ir_function);

        code
    }

    fn generate_variable_decl_ir(
        &mut self,
        name: &Token,
        value: &Option<AnnotatedExpression>,
    ) -> Vec<u8> {
        self.declare_local(name.lexeme.clone(), true);

        let mut code = Vec::new();

        match value {
            Some(v) => {
                let value_code = self.generate_expr_ir(v);

                for byte in value_code {
                    self.emit_byte(&mut code, byte);
                }
            }
            None => {
                self.emit_instruction(&mut code, Instruction::Void);
            }
        }

        code
    }

    // fn get_value_in_stack(&mut self, depth: u32) -> &LocalValue {
    //     &self.local_values.values[depth as usize]
    // }

    fn get_in_depth(&self, name: String) -> u32 {
        for (index, value) in self.local_values.values.iter().enumerate() {
            if value.name == name {
                return index as u32;
            }
        }

        return 0;
    }

    fn declare_local(&mut self, name: String, is_initialized: bool) {
        self.local_values
            .push(LocalValue::new(name, self.current_depth, is_initialized));
    }

    fn generate_scope_ir(&mut self, statements: &[Box<AnnotatedStatement>]) -> Vec<u8> {
        self.begin_scope();

        let mut code = Vec::new();

        for stmt in statements {
            let stmt_code = self.generate_ir_for(stmt);

            for byte in stmt_code {
                self.emit_byte(&mut code, byte);
            }
        }

        self.end_scope(&mut code);

        code
    }

    fn generate_expr_ir(&mut self, expr: &AnnotatedExpression) -> Vec<u8> {
        match expr {
            AnnotatedExpression::Arithmetic(left, op, right, kind) => {
                self.generate_arithmetic_expr_ir(left, op, right, &kind.borrow())
            }

            AnnotatedExpression::Logical(left, op, right, kind) => {
                self.generate_logical_expr_ir(left, op, right, &kind.borrow())
            }

            AnnotatedExpression::PostFix(operator, variable) => {
                self.generate_postfix_expr_ir(operator, variable)
            }

            AnnotatedExpression::Call(proto, callee, args, kind) => {
                self.generate_call_expr_ir(proto, callee, args, &kind.borrow())
            }

            AnnotatedExpression::CallMethod(object, method, args) => {
                self.generate_method_call_ir(object, method, args)
            }

            AnnotatedExpression::CallNative(proto, callee, args, kind) => {
                self.generate_call_native_expr_ir(proto, callee, args, &kind.borrow())
            }

            AnnotatedExpression::Assign(name, value, kind) => {
                self.generate_assign_expr_ir(name, value, &kind.borrow())
            }

            AnnotatedExpression::Unary(operator, expression, kind) => {
                self.generate_unary_expr_ir(operator, expression, &kind.borrow())
            }

            AnnotatedExpression::Variable(name, kind) => {
                self.generate_variable_expr_ir(name, &kind.borrow())
            }

            AnnotatedExpression::Literal(token, kind) => {
                self.generate_literal_ir(token, &kind.borrow())
            }

            AnnotatedExpression::Get(target, name, kind) => {
                self.generate_get_expr_ir(target, name, &kind.borrow())
            }

            AnnotatedExpression::Set(target, name, value, kind) => {
                self.generate_set_ir(target, name, value, &kind.borrow())
            }

            AnnotatedExpression::List(elements, kind) => {
                self.generate_list_ir(elements, &kind.borrow())
            }

            AnnotatedExpression::ListGet(list, index) => self.generate_list_get_ir(list, index),

            _ => todo!("Expression IR: {:?} not implemented.", expr),
        }
    }

    fn generate_arithmetic_expr_ir(
        &mut self,
        left: &AnnotatedExpression,
        op: &Token,
        right: &AnnotatedExpression,
        _kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut right_bytes = self.generate_expr_ir(left);
        let mut left_bytes = self.generate_expr_ir(right);
        let operator = self.convert_operator_to_instruction(op) as u8;

        let mut all_bytes: Vec<u8> = Vec::new();

        all_bytes.append(&mut left_bytes);
        all_bytes.append(&mut right_bytes);
        all_bytes.push(operator);

        all_bytes
    }

    fn generate_literal_ir(&mut self, token: &Token, kind: &TypeDescriptor) -> Vec<u8> {
        let constant = self.get_constant(token, kind);
        let mut bytes = Vec::new();

        if constant.is_bool() {
            if let CompileTimeValue::Boolean(b) = constant {
                if b {
                    bytes.push(Instruction::True as u8);
                } else {
                    bytes.push(Instruction::False as u8);
                }
            }
        } else {
            let index = self.current_chunk.add_constant(constant);
            bytes.push(Instruction::Push as u8);

            let (low, high) = self.split_u16(index);
            bytes.push(low);
            bytes.push(high);
        }

        bytes
    }

    fn get_constant(&self, token: &Token, _kind: &TypeDescriptor) -> CompileTimeValue {
        if let TokenKind::Literal(l) = token.kind {
            match l {
                Literal::Boolean => {
                    if token.lexeme.cmp(&String::from("true")) == Ordering::Equal {
                        CompileTimeValue::Boolean(true)
                    } else {
                        CompileTimeValue::Boolean(false)
                    }
                }
                Literal::String => CompileTimeValue::String(token.lexeme.clone()),
                Literal::Int => CompileTimeValue::Int(token.lexeme.parse::<i32>().unwrap()),
                Literal::Float => CompileTimeValue::Float(token.lexeme.parse::<f32>().unwrap()),
            }
        } else {
            gpp_error!("Invalid literal kind.");
        }
    }

    fn generate_assign_expr_ir(
        &mut self,
        name: &Token,
        value: &AnnotatedExpression,
        _kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut code = Vec::new();
        let index = self.get_in_depth(name.lexeme.clone());

        let mut value_code = self.generate_expr_ir(value);
        code.append(&mut value_code);

        code.push(Instruction::SetLocal as u8);
        code.push(index as u8);

        code
    }

    fn split_u16(&self, short: u16) -> (u8, u8) {
        let high = (short >> 8) as u8;
        let low = (short & 0xff) as u8;

        (high, low)
    }

    fn split_u32(&self, value: u32) -> (u8, u8, u8, u8) {
        let byte1 = (value >> 24) as u8;
        let byte2 = (value >> 16) as u8;
        let byte3 = (value >> 8) as u8;
        let byte4 = value as u8;

        (byte1, byte2, byte3, byte4)
    }

    fn convert_operator_to_instruction(&self, op: &Token) -> Instruction {
        if let TokenKind::Operator(operator) = op.kind {
            match operator {
                OperatorKind::And => Instruction::And,
                OperatorKind::Or => Instruction::Or,
                OperatorKind::Star => Instruction::Mul,
                OperatorKind::Slash => Instruction::Div,
                OperatorKind::Plus => Instruction::Add,
                OperatorKind::Minus => Instruction::Sub,
                OperatorKind::Not => Instruction::Not,
                OperatorKind::EqualEqual => Instruction::Eq,
                OperatorKind::NotEqual => Instruction::Neq,
                OperatorKind::BitwiseAnd => Instruction::BitAnd,
                OperatorKind::BitwiseOr => Instruction::BitOr,
                OperatorKind::DoubleStar => Instruction::Pow,
                OperatorKind::Less => Instruction::Less,
                OperatorKind::LessEqual => Instruction::LessEqual,
                OperatorKind::Greater => Instruction::Greater,
                OperatorKind::GreaterEqual => Instruction::GreaterEqual,
                OperatorKind::Equal => Instruction::Eq,
                OperatorKind::Arrow => todo!(),
                _ => {
                    todo!();
                }
            }
        } else {
            gpp_error!("Invalid operator to instruction conversion.");
        }
    }

    fn emit_byte(&mut self, code: &mut Vec<u8>, byte: u8) {
        code.push(byte);
    }

    fn emit_instruction(&mut self, code: &mut Vec<u8>, instruction: Instruction) {
        code.push(instruction.into());
    }

    fn begin_scope(&mut self) {
        self.current_depth += 1;
    }

    fn end_scope(&mut self, code: &mut Vec<u8>) {
        let mut count = 0;
        for value in self.local_values.values.iter().rev() {
            if value.depth < self.current_depth {
                break;
            }

            code.push(Instruction::Pop as u8);
            count += 1;
        }

        for _ in 0..count {
            self.local_values.values.pop();
        }

        self.current_depth -= 1;
    }

    fn generate_variable_expr_ir(&mut self, name: &Token, _kind: &TypeDescriptor) -> Vec<u8> {
        let index = self.get_in_depth(name.lexeme.clone());

        let mut code: Vec<u8> = Vec::new();

        match index {
            0 => self.emit_instruction(&mut code, Instruction::GetLocal0),
            1 => self.emit_instruction(&mut code, Instruction::GetLocal1),
            2 => self.emit_instruction(&mut code, Instruction::GetLocal2),
            3 => self.emit_instruction(&mut code, Instruction::GetLocal3),
            4 => self.emit_instruction(&mut code, Instruction::GetLocal4),
            _ => {
                self.emit_instruction(&mut code, Instruction::GetLocal);
                self.emit_byte(&mut code, index as u8);
            }
        }

        code
    }

    fn generate_return_ir(&mut self, value: &Option<AnnotatedExpression>) -> Vec<u8> {
        let mut code = Vec::new();

        match value {
            Some(v) => {
                code = self.generate_expr_ir(v);
            }
            None => self.emit_instruction(&mut code, Instruction::Void),
        }

        self.emit_instruction(&mut code, Instruction::Ret);

        code
    }

    fn generate_call_expr_ir(
        &mut self,
        proto: &FunctionPrototype,
        _callee: &Token,
        args: &[Box<AnnotatedExpression>],
        kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut code = Vec::new();
        let function_table_index = self.top_level_graph.get_function_id(&proto.name);

        for arg in args {
            let mut arg_code = self.generate_expr_ir(arg);
            code.append(&mut arg_code);
        }

        if self.is_constructor(proto, kind) {
            code.push(Instruction::New as u8);
        } else {
            code.push(Instruction::Call as u8);

            let index_bytes = self.split_u32(function_table_index);

            code.push(index_bytes.0);
            code.push(index_bytes.1);
            code.push(index_bytes.2);
            code.push(index_bytes.3);
        }

        code.push(proto.arity as u8);

        code
    }

    fn generate_call_native_expr_ir(
        &mut self,
        proto: &FunctionPrototype,
        _callee: &Token,
        args: &[Box<AnnotatedExpression>],
        _kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut code = Vec::new();
        let function_table_index = self.native_functions[&proto.name].id;

        for arg in args {
            let mut arg_code = self.generate_expr_ir(arg);
            code.append(&mut arg_code);
        }

        code.push(Instruction::InvokeNative as u8);

        let index_bytes = self.split_u32(function_table_index);

        code.push(index_bytes.0);
        code.push(index_bytes.1);
        code.push(index_bytes.2);
        code.push(index_bytes.3);

        code.push(proto.arity as u8);

        code
    }

    fn generate_if_else(
        &mut self,
        _keyword: &Token,
        condition: &AnnotatedExpression,
        then_branch: &AnnotatedStatement,
        else_branch: &Option<Box<AnnotatedStatement>>,
    ) -> Vec<u8> {
        let mut code = Vec::new();

        let mut condition_code = self.generate_expr_ir(condition);
        code.append(&mut condition_code);

        self.emit_instruction(&mut code, Instruction::JFalse);
        let jfalse_offset_pos = code.len();
        code.extend(&[0xff, 0xff, 0xff, 0xff]);

        let mut then_branch_code = self.generate_ir_for(then_branch);
        code.append(&mut then_branch_code);

        let mut jump_to_end_offset_pos = None;
        if else_branch.is_some() {
            self.emit_instruction(&mut code, Instruction::Jump);
            jump_to_end_offset_pos = Some(code.len());
            code.extend(&[0xff, 0xff, 0xff, 0xff]);
        }

        let else_start_pos = code.len();

        let jfalse_jump_target = else_start_pos;
        let jfalse_offset = (jfalse_jump_target - (jfalse_offset_pos + 4)) as u32;
        let (byte1, byte2, byte3, byte4) = self.split_u32(jfalse_offset);

        code[jfalse_offset_pos] = byte1;
        code[jfalse_offset_pos + 1] = byte2;
        code[jfalse_offset_pos + 2] = byte3;
        code[jfalse_offset_pos + 3] = byte4;

        if let Some(else_branch) = else_branch {
            let mut else_code = self.generate_ir_for(else_branch);
            code.append(&mut else_code);

            if let Some(jump_offset_pos) = jump_to_end_offset_pos {
                let jump_end_target = code.len();
                let jump_offset = (jump_end_target - (jump_offset_pos + 4)) as u32;
                let (byte1, byte2, byte3, byte4) = self.split_u32(jump_offset);

                code[jump_offset_pos] = byte1;
                code[jump_offset_pos + 1] = byte2;
                code[jump_offset_pos + 2] = byte3;
                code[jump_offset_pos + 3] = byte4;
            }
        }

        code
    }

    // fn get_current_chunk_offset(&self) -> usize {
    //     self.current_chunk.code.len()
    // }

    // fn generate_standard_native_functions(&mut self) {
    //     let id = self
    //         .top_level_graph
    //         .get_id_for_new_edge("print".to_string());

    //     self.current_chunk = CompileTimeChunk::empty();

    //     let mut code = Vec::new();

    //     self.emit_instruction(&mut code, Instruction::Print);
    //     self.emit_instruction(&mut code, Instruction::Void);
    //     self.emit_instruction(&mut code, Instruction::Ret);

    //     self.current_chunk.code = code;

    //     self.functions.insert(
    //         "print".to_string(),
    //         IRFunction::new(id, "print".to_string(), self.current_chunk.clone(), 1),
    //     );
    // }

    fn generate_unary_expr_ir(
        &mut self,
        _operator: &Token,
        expression: &AnnotatedExpression,
        _kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut code: Vec<u8> = Vec::new();
        let mut expr_code = self.generate_expr_ir(expression);
        code.append(&mut expr_code);

        self.emit_instruction(&mut code, Instruction::Not);
        code
    }

    fn generate_while_ir(
        &mut self,
        condition: &AnnotatedExpression,
        body: &AnnotatedStatement,
    ) -> Vec<u8> {
        let mut code: Vec<u8> = Vec::new();
        let mut condition_code = self.generate_expr_ir(condition);
        let condition_len = condition_code.len();

        self.begin_scope();

        code.append(&mut condition_code);
        self.emit_instruction(&mut code, Instruction::JFalse);
        let end_jump_offset = code.len();

        code.extend(&[0xff, 0xff, 0xff, 0xff]);
        let mut body_code = self.generate_ir_for(body);
        code.append(&mut body_code);

        let offset = code.len() + 5;
        self.emit_instruction(&mut code, Instruction::Loop);
        let (byte1, byte2, byte3, byte4) = self.split_u32(offset as u32);

        self.emit_byte(&mut code, byte1);
        self.emit_byte(&mut code, byte2);
        self.emit_byte(&mut code, byte3);
        self.emit_byte(&mut code, byte4);

        let offset = code.len() - condition_len - 5;
        let (byte1, byte2, byte3, byte4) = self.split_u32(offset as u32);

        code[end_jump_offset] = byte1;
        code[end_jump_offset + 1] = byte2;
        code[end_jump_offset + 2] = byte3;
        code[end_jump_offset + 3] = byte4;

        self.end_scope(&mut code);

        code
    }

    fn generate_postfix_expr_ir(
        &mut self,
        operator: &Token,
        variable: &AnnotatedExpression,
    ) -> Vec<u8> {
        let mut code: Vec<u8> = Vec::new();

        if let AnnotatedExpression::Variable(name, _) = variable {
            let index = self.get_in_depth(name.lexeme.clone());

            match operator.kind {
                TokenKind::Operator(op) => match op {
                    OperatorKind::PostFixIncrement => {
                        self.emit_instruction(&mut code, Instruction::IncrementLocal);
                        self.emit_byte(&mut code, index as u8);
                    }
                    OperatorKind::PostFixDecrement => {
                        self.emit_instruction(&mut code, Instruction::DecrementLocal);
                        self.emit_byte(&mut code, index as u8);
                    }

                    _ => {}
                },
                _ => {}
            }
        }

        code
    }

    fn is_constructor(&self, proto: &FunctionPrototype, return_kind: &TypeDescriptor) -> bool {
        proto.name == return_kind.name
    }

    fn generate_type_ir(&mut self, descriptor: &TypeDescriptor) -> Vec<u8> {
        self.top_level_graph
            .get_id_for_new_edge(descriptor.name.clone());

        vec![]
    }

    fn generate_get_expr_ir(
        &mut self,
        target: &AnnotatedExpression,
        name: &Token,
        kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut code: Vec<u8> = Vec::new();
        code.append(&mut self.generate_expr_ir(target));

        let field_index = kind.fields[&name.lexeme].id;
        self.emit_instruction(&mut code, Instruction::GetField);
        self.emit_byte(&mut code, field_index);

        code
    }

    fn generate_set_ir(
        &mut self,
        target: &AnnotatedExpression,
        name: &Token,
        value: &AnnotatedExpression,
        kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut code: Vec<u8> = Vec::new();
        let field_index = kind.fields[&name.lexeme].id;

        code.append(&mut self.generate_expr_ir(target));
        code.append(&mut self.generate_expr_ir(value));

        self.emit_instruction(&mut code, Instruction::SetField);
        self.emit_byte(&mut code, field_index);

        code
    }

    fn generate_list_ir(
        &mut self,
        elements: &[Box<AnnotatedExpression>],
        _kind: &TypeDescriptor,
    ) -> Vec<u8> {
        let mut code: Vec<u8> = Vec::new();

        for element in elements {
            code.append(&mut self.generate_expr_ir(&element));
        }

        self.emit_instruction(&mut code, Instruction::Array);
        self.emit_byte(&mut code, elements.len() as u8);

        code
    }

    fn generate_list_get_ir(
        &mut self,
        list: &AnnotatedExpression,
        index: &AnnotatedExpression,
    ) -> Vec<u8> {
        let mut code: Vec<u8> = Vec::new();

        code.append(&mut self.generate_expr_ir(list));
        code.append(&mut self.generate_expr_ir(index));
        self.emit_instruction(&mut code, Instruction::ListGet);

        code
    }

    fn generate_native_function_ir(&mut self, prototype: &FunctionPrototype) -> Vec<u8> {
        let id = self.get_native_id();

        self.native_functions.insert(
            prototype.name.clone(),
            NativeFunctionInfo::new(prototype.arity as u8, id.clone()),
        );

        vec![]
    }

    fn get_native_id(&mut self) -> u32 {
        let id = self.current_native_id;
        self.current_native_id += 1;

        id
    }

    fn generate_internal_definition_ir(
        &mut self,
        target: &TypeDescriptor,
        definition: &FunctionPrototype,
        body: &AnnotatedStatement,
    ) -> Vec<u8> {
        self.current_chunk = CompileTimeChunk::empty();
        let mut code = Vec::new();

        self.begin_scope();

        for param in &definition.params {
            self.declare_local(param.name.lexeme.clone(), true);
        }

        if let AnnotatedStatement::Scope(stmts, _, _) = body {
            for stmt in stmts {
                let stmt_code = self.generate_ir_for(stmt);
                for byte in stmt_code {
                    self.emit_byte(&mut code, byte);
                }
            }
        }

        self.end_scope(&mut code);

        self.emit_instruction(&mut code, Instruction::Void);
        self.emit_instruction(&mut code, Instruction::Ret);

        self.current_chunk.code = code.clone();

        let id = match self.methods.contains_key(&target) {
            true => self.methods[&target].len() as u32,
            false => 0,
        };

        let ir_function = IRFunction::new(
            id,
            definition.name.clone(),
            self.current_chunk.clone(),
            definition.arity as u8,
        );

        if self.methods.contains_key(&target) {
            self.methods.get_mut(&target).unwrap().push(ir_function);
        } else {
            self.methods.insert(target.clone(), vec![ir_function]);
        }

        code
    }

    fn generate_method_call_ir(
        &mut self,
        _object: &AnnotatedExpression,
        method: &MethodDescriptor,
        args: &Vec<Box<AnnotatedExpression>>,
    ) -> Vec<u8> {
        let mut code = Vec::new();

        let owner_type = &self
            .semantic_code
            .table
            .get_type_by_id(method.owner_type_id)
            .unwrap();

        let method_table = self.methods.get(&owner_type.borrow()).unwrap();
        let ir_method = method_table
            .iter()
            .find(|m| m.name == method.name)
            .unwrap()
            .clone();

        for arg in args {
            let mut arg_code = self.generate_expr_ir(arg);
            code.append(&mut arg_code);
        }

        code.push(Instruction::InvokeVirtual as u8);

        let method_index = ir_method.id;
        let method_index_bytes = self.split_u32(method_index);

        let v_table_index = owner_type.borrow().id;
        let v_table_index_bytes = self.split_u32(v_table_index);

        code.push(v_table_index_bytes.0);
        code.push(v_table_index_bytes.1);
        code.push(v_table_index_bytes.2);
        code.push(v_table_index_bytes.3);

        code.push(method_index_bytes.0);
        code.push(method_index_bytes.1);
        code.push(method_index_bytes.2);
        code.push(method_index_bytes.3);

        code.push(ir_method.arity as u8);

        code
    }

    fn generate_logical_expr_ir(
        &mut self,
        left: &AnnotatedExpression,
        op: &Token,
        right: &AnnotatedExpression,
        _borrow: &std::cell::Ref<'_, TypeDescriptor>,
    ) -> Vec<u8> {
        let mut right_bytes = self.generate_expr_ir(left);
        let mut left_bytes = self.generate_expr_ir(right);
        let operator = self.convert_operator_to_instruction(op) as u8;

        let mut all_bytes: Vec<u8> = Vec::new();

        all_bytes.append(&mut left_bytes);
        all_bytes.append(&mut right_bytes);
        all_bytes.push(operator);

        all_bytes
    }
}
