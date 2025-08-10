#![allow(warnings)]

use std::{
    cell::RefCell,
    clone,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    env,
    fmt::{Display, format, write},
    path::PathBuf,
    process,
    rc::Rc,
    string,
};

use skyl_data::{
    AnnotatedAST, AnnotatedExpression, AnnotatedStatement, Archetype, Ast, BuiltinAttribute,
    CoersionKind, CompilerConfig, CompilerContext, ContextScope, ContextStack, Decorator,
    Expression, FieldDeclaration, FieldDescriptor, FunctionPrototype, Literal, MethodDescriptor,
    MethodDescriptorParams, MethodParameter, SemanticCode, SemanticValue, SourceFileID, Span,
    Statement, StaticValue, SymbolKind, SymbolTable, Token, TokenKind, TypeDecl, TypeDescriptor,
    ValueWrapper, Visibility, read_file_without_bom,
};
use skyl_driver::{
    errors::{CompilationError, CompilationErrorKind, CompilerErrorReporter},
    gpp_error,
};
use skyl_lexer::Lexer;
use skyl_parser::Parser;

use crate::{
    import_pipeline::ModuleImportPipeline,
    result::{TyResult, TyStmts},
};

pub struct SemanticAnalyzer {
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
    pub(crate) current_decorator: Decorator,
    pub(crate) reporter: Rc<RefCell<CompilerErrorReporter>>,
    pub(crate) void_instance: Rc<RefCell<TypeDescriptor>>,
    pub(crate) modules: Vec<String>,
    pub(crate) ctx: Option<Rc<RefCell<CompilerContext>>>,
    pub(crate) current_file: SourceFileID,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
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
            void_instance: Rc::new(RefCell::new(TypeDescriptor::empty(
                SourceFileID(0),
                Visibility::Public,
            ))),
            ..Default::default()
        };
        let mut archetypes = HashSet::new();
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

    pub fn report_error(&mut self, error: CompilationError) {
        self.reporter.borrow_mut().report_error(error);
    }

    /// Returns the next available id for a new static type.
    /// # Returns
    /// A `u32` value with new given value.
    pub fn get_static_id(&mut self) -> u32 {
        self.current_static_id += 1;
        self.current_static_id
    }

    /// Initialize all native types that the language has
    /// by default in any compiled program, including `object`,
    /// `bool`, `number`, `float`, `int`, `iterator`, `str`,
    /// `tuple`, `list`.
    pub fn initialize_predefined_types(&mut self) {
        self.create_and_define_type(SourceFileID(0), "object", vec![]);
        self.create_and_define_type(SourceFileID(0), "bool", vec![]);
        self.create_and_define_type(SourceFileID(0), "number", vec![]);

        let number_descriptor = self
            .get_static_kind_by_name("number", &Expression::Void)
            .unwrap();
        self.add_field_to_defined_type("mod", number_descriptor.clone(), number_descriptor);

        self.create_and_define_type(SourceFileID(0), "float", vec!["number"]);
        self.create_and_define_type(SourceFileID(0), "int", vec!["number"]);

        let int_descriptor = self
            .get_static_kind_by_name("int", &Expression::Void)
            .unwrap();

        self.create_and_define_type(SourceFileID(0), "iterator", vec![]);
        let iterator_descriptor = self
            .get_static_kind_by_name("iterator", &Expression::Void)
            .unwrap();

        self.add_field_to_defined_type("length", iterator_descriptor, int_descriptor);

        self.create_and_define_type(SourceFileID(0), "str", vec!["iterator"]);
        self.create_and_define_type(SourceFileID(0), "tuple", vec!["iterator"]);
        self.create_and_define_type(SourceFileID(0), "list", vec!["iterator"]);

        let kind = self.get_void_instance();

        // self.create_and_define_function(
        //     "print",
        //     vec![
        //         FieldDeclaration::new(
        //             Token::new(TokenKind::Identifier, "print".to_string(), 0, 0),
        //             Expression::Void
        //         )
        //     ],
        //     kind.clone()
        // );
    }

    /// Creates valid semantic data for standard native functions,
    /// ensuring that the semantic analyzer can complete the analysis
    /// without reporting errors for the defined native functions.
    ///
    /// # Arguments
    ///
    /// * `name` - A string slice containing the function name.
    /// * `params` - A `Vec` containing the description of function params names and kinds.
    /// * `kind` - The descriptor for function return kind.
    pub fn create_and_define_function(
        &mut self,
        file: SourceFileID,
        visibility: Visibility,
        name: &str,
        params: Vec<FieldDeclaration>,
        kind: Rc<RefCell<TypeDescriptor>>,
    ) {
        let arity = params.len();

        self.symbol_table.native_functions.insert(
            name.to_string(),
            FunctionPrototype::new(file, visibility, name.to_string(), params, arity, kind),
        );
    }

    /// Adds new field for native or user declared kind.
    ///
    /// # Arguments
    /// * `name` - A string slice with new field name.
    /// * `target_descriptor` - The descriptor of kind to be changed.
    /// * `field_descriptor` - The descriptor of new field.
    pub fn add_field_to_defined_type(
        &mut self,
        name: &str,
        target_descriptor: Rc<RefCell<TypeDescriptor>>,
        field_descriptor: Rc<RefCell<TypeDescriptor>>,
    ) {
        let field_len = target_descriptor.borrow().fields.len();
        let field_name = {
            let fd = field_descriptor.borrow();
            fd.name.clone()
        };

        target_descriptor.borrow_mut().fields.insert(
            name.to_string(),
            FieldDescriptor::new(field_name, field_descriptor.clone(), field_len as u8),
        );
    }

    pub fn add_method_to_defined_type(
        &mut self,
        file: SourceFileID,
        visibility: Visibility,
        name: String,
        target: &str,
        params: Vec<MethodParameter>,
        arity: usize,
        owner_type_id: u32,
        return_kind_id: u32,
        is_native: bool,
    ) {
        let method = {
            MethodDescriptor::new(MethodDescriptorParams {
                file,
                visibility,
                name: name.clone(),
                params,
                arity,
                owner_type_id,
                return_kind_id,
                is_native,
            })
        };

        let methods = &mut self
            .symbol_table
            .names
            .get_mut(target)
            .unwrap()
            .kind
            .borrow_mut()
            .methods;

        methods.insert(name, method);
    }

    /// Defines a new native kind to existing kinds.
    ///
    /// # Arguments
    ///
    /// * `name` - A string slice with name of kind to be defined.
    /// * `archetypes` - A `Vec` with names of archetypes to
    /// compound new kind mask.
    pub fn create_and_define_type(
        &mut self,
        file: SourceFileID,
        name: &str,
        archetypes: Vec<&str>,
    ) {
        let mut type_decl = TypeDecl::new(
            file,
            Visibility::Public,
            name.to_string(),
            self.get_static_id(),
        );

        if "object".cmp(&type_decl.name) != Ordering::Equal {
            type_decl.add_archetype(Archetype::new("object".to_string()));
        }

        type_decl.add_archetype(Archetype::new(name.to_string().clone()));

        for archetype_name in &archetypes {
            type_decl.add_archetype(Archetype::new(archetype_name.to_string()));
        }

        let mut type_descriptor = Rc::new(RefCell::new(TypeDescriptor::from_type_decl(type_decl)));

        for archetype_name in &archetypes {
            let kind = self
                .get_static_kind_by_name(&archetype_name, &Expression::Void)
                .unwrap();

            for (name, field_descriptor) in &kind.borrow().fields {
                type_descriptor
                    .borrow_mut()
                    .fields
                    .insert(name.clone(), field_descriptor.clone());
            }
        }

        let static_value = StaticValue::new(type_descriptor, ValueWrapper::Kind);
        self.define_symbol(name.to_string(), static_value);
    }

    /// Defines new user defined symbol
    ///
    /// # Arguments
    ///
    /// * `name` - A String containing the name of the symbol.
    /// * `value` - A fixed value with descriptor and `Value`
    /// for literals and instances.
    pub fn define_symbol(&mut self, name: String, value: StaticValue) {
        self.symbol_table.define(name, value);
    }

    /// Performs semantic analysis on the given abstract syntax tree (AST).
    ///
    /// This function traverses the AST and checks for semantic errors, such as
    /// undefined variables, type mismatches, and other rule violations. If any
    /// issues are found, they are reported accordingly.
    ///
    /// # Arguments
    ///
    /// * `ast` - A reference to the AST to be analyzed.
    ///
    /// # Returns
    ///
    /// A result indicating success or containing a list of semantic errors.
    pub fn analyze(
        &mut self,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
        statements: Vec<Statement>,
        config: &CompilerConfig,
    ) -> SemanticCode {
        self.reset_internal_state(statements, config);

        self.reporter = reporter;

        let mut stmt: Statement;

        let mut annotated_statements: TyStmts = Vec::new();

        let mut prelude = match self.setup_prelude() {
            Err(e) => {
                self.report_error(e);
                vec![]
            }
            Ok(p) => p,
        };

        annotated_statements.append(&mut prelude);

        while !self.is_at_end() {
            stmt = self.current().unwrap().clone();
            let ty_stmt = self.analyze_stmt(&stmt, &stmt.span(), &stmt.line());

            match ty_stmt {
                Err(e) => {
                    self.report_error(e);
                }
                Ok(mut stmt) => {
                    annotated_statements.append(&mut stmt);
                }
            }

            self.advance();
        }

        let main_fn = self.get_function("main");

        if main_fn == None {
            self.report_error(CompilationError::new(
                CompilationErrorKind::MissingMainFunction,
                None,
            ));
        } else {
            if main_fn.unwrap().return_kind.borrow().name != "void" {
                self.report_error(CompilationError::new(
                    CompilationErrorKind::MainFunctionReturnKind,
                    None,
                ));
            }
        }

        SemanticCode::new(
            self.symbol_table.clone(),
            AnnotatedAST::new(annotated_statements),
        )
    }

    /// Defines a new type in the symbol table.
    ///
    /// This function adds a new type to the symbol table, creating an entry with the type's name
    /// and an associated static value. The type is represented by a `TypeDescriptor`, and the static
    /// value is initialized as `Internal`.
    ///
    /// # Parameters
    /// - `descriptor`: The `TypeDescriptor` containing information about the type to be defined,
    /// including the type's name and other related details.
    ///
    /// # Examples
    /// ```rust
    /// let descriptor = TypeDescriptor::new("MyType");
    /// analyzer.define_type(descriptor);
    /// ```
    ///
    /// # Panics
    /// This function may panic if the type is already defined in the symbol table.
    ///
    /// # Notes
    /// - The symbol table manages the definition of types to ensure that defined types are not duplicated.
    pub(super) fn define_type(&mut self, descriptor: Rc<RefCell<TypeDescriptor>>) {
        self.symbol_table.define(
            descriptor.clone().borrow().name.clone(),
            StaticValue::new(descriptor, ValueWrapper::Internal),
        );
    }

    /// Ensures that the current depth satisfies a given condition.
    ///
    /// This function compares the current scope depth with a specified value and reports an error
    /// if the depth does not meet the expected condition. The comparison is made based on the provided
    /// `comparator` and `depth`. If the condition is not satisfied, an error message is generated.
    ///
    /// # Parameters
    /// - `comparator`: An `Ordering` value that determines whether the current depth should be less than,
    ///   greater than, or equal to the specified depth.
    /// - `depth`: The expected scope depth that will be compared against the current depth.
    /// - `message`: The error message to be reported if the comparison condition is not met.
    ///
    /// # Returns
    /// This function does not return any value. It will report an error if the condition is not satisfied.
    pub(super) fn require_depth(
        &mut self,
        comparator: Ordering,
        location: &Token,
        depth: usize,
        message: String,
    ) -> TyResult<()> {
        let comparison_result = self.depth().cmp(&depth);

        if comparison_result != comparator {
            return Err(CompilationError::with_span(
                CompilationErrorKind::DepthError { msg: message },
                Some(location.line),
                location.span,
            ));
        }

        Ok(())
    }

    /// Starts a new scope by pushing an empty context onto the context stack.
    ///
    /// This function is used to create a new scope for the current analysis, allowing for local
    /// declarations and symbol management to be isolated within that scope. It adds an empty context
    /// to the `context_stack` to indicate the beginning of a new scope.
    ///
    /// # Parameters
    /// This function does not take any parameters.
    ///
    /// # Returns
    /// This function does not return any value. It only modifies the internal state by adding a new
    /// context to the `context_stack`.
    pub(super) fn begin_scope(&mut self) {
        self.context_stack.push_empty();
    }

    /// Ends the current scope by popping the top context from the context stack.
    ///
    /// This function is used to close the current scope, removing the local declarations and symbols
    /// associated with it. It pops the top context off the `context_stack`, effectively ending the scope
    /// and returning to the previous one.
    ///
    /// # Parameters
    /// This function does not take any parameters.
    ///
    /// # Returns
    /// This function does not return any value. It only modifies the internal state by removing the
    /// top context from the `context_stack`.
    pub(super) fn end_scope(&mut self) {
        self.context_stack.pop();
    }

    /// Retrieves the next statement in the sequence of statements.
    ///
    /// This function returns the next statement in the list of statements. If there is no next statement
    /// (i.e., the end of the list is reached), it returns a `Statement::EndCode` to indicate the end of code.
    /// It is used to navigate through a series of statements in sequence.
    ///
    /// # Returns
    /// - If there is a next statement, it returns a cloned reference to the next statement.
    /// - If there is no next statement, it returns `Statement::EndCode` to indicate the end of the code sequence.
    pub(super) fn next(&self) -> Statement {
        match self.statements.get(self.current_stmt + 1) {
            None => Statement::EndCode,
            Some(stmt) => stmt.clone(),
        }
    }

    /// Checks if the current statement is the last statement.
    ///
    /// This function determines if the current statement is the last statement in the sequence
    /// by checking if the current statement is `Statement::EndCode`. It returns `true` if the
    /// current statement is the last one, indicating the end of the code sequence, and `false` otherwise.
    ///
    /// # Returns
    /// - `true` if the current statement is the last one (`Statement::EndCode`).
    /// - `false` if there are more statements to process.
    fn is_at_end(&self) -> bool {
        match self.current() {
            None => true,
            Some(stmt) => match stmt {
                Statement::EndCode => true,
                _ => false,
            },
        }
    }

    /// Retrieves the current statement.
    ///
    /// This function returns a reference to the current statement in the sequence. If no statement
    /// is available at the current position, it returns `None`.
    ///
    /// # Returns
    /// - `Some(&Statement)` if there is a current statement.
    /// - `None` if there is no statement at the current position.
    fn current(&self) -> Option<&Statement> {
        self.statements.get(self.current_stmt)
    }

    /// Retrieves the previous statement.
    ///
    /// This function returns a reference to the previous statement in the sequence. If no previous
    /// statement exists (i.e., if the current statement is the first one), it returns `None`.
    ///
    /// # Returns
    /// - `Some(&Statement)` if there is a previous statement.
    /// - `None` if there is no previous statement.
    fn previous(&self) -> Option<&Statement> {
        self.statements.get(self.current_stmt - 1)
    }

    /// Advances to the next statement and returns the previous one.
    ///
    /// This function increments the `current_stmt` index and returns the previous statement in the sequence
    /// (the one before the new current statement). If there is no previous statement, it returns `None`.
    ///
    /// # Returns
    /// - `Some(&Statement)` if there is a previous statement after advancing.
    /// - `None` if there is no previous statement after advancing (i.e., if at the beginning).
    pub(crate) fn advance(&mut self) -> Option<&Statement> {
        self.current_stmt += 1;
        self.previous()
    }

    /// Resets the internal state of the analysis context.
    ///
    /// This function resets the analysis context, including the list of statements, the context stack,
    /// and the current statement index. It is typically used to reinitialize the state before starting
    /// a new analysis or after a major error that requires a fresh start.
    ///
    /// # Parameters
    /// - `statements`: A vector of statements to initialize the internal statements list with.
    fn reset_internal_state(&mut self, statements: Vec<Statement>, config: &CompilerConfig) {
        self.statements = statements;
        self.config = config.clone();
        self.context_stack = ContextStack::new();
        self.current_stmt = 0;
    }

    /// Returns the current depth of the context stack.
    ///
    /// The context stack tracks the current scope of analysis. This function returns the number of levels
    /// in the context stack, which corresponds to the current depth of nested scopes.
    ///
    /// # Returns
    /// - The current depth (i.e., the number of levels in the context stack).
    fn depth(&self) -> usize {
        self.context_stack.len()
    }

    /// Retrieves the current context scope.
    ///
    /// This function returns a mutable reference to the topmost scope in the context stack, which is
    /// useful for analyzing or modifying the symbols within the current scope.
    ///
    /// # Returns
    /// - A mutable reference to the current `ContextScope` at the top of the context stack.
    pub(crate) fn context(&mut self) -> &mut ContextScope {
        self.context_stack.peek()
    }

    /// Returns a reference to the predefined "void" type instance.
    ///
    /// This function retrieves a predefined instance of the "void" type. It is used when an expression
    /// or function is expected to return no value.
    ///
    /// # Returns
    /// - A clone of the "void" type instance.
    pub(super) fn get_void_instance(&mut self) -> Rc<RefCell<TypeDescriptor>> {
        self.void_instance.clone()
    }

    /// Retrieves a symbol from the symbol table by name.
    ///
    /// This function looks up a symbol in the symbol table by its name. It returns an `Option` containing
    /// the symbol if it exists, or `None` if the symbol is not found.
    ///
    /// # Parameters
    /// - `name`: The name of the symbol to look up.
    ///
    /// # Returns
    /// - An `Option` containing the `StaticValue` associated with the symbol if it exists, or `None` if not.
    pub(super) fn get_symbol(&self, name: &str) -> Option<&StaticValue> {
        self.symbol_table.get(name)
    }

    /// Retrieves the ID of a symbol's kind from the symbol table by name.
    ///
    /// This function looks up a symbol in the symbol table by its name and retrieves the ID of its kind.
    /// If the symbol does not exist, it panics.
    ///
    /// # Parameters
    /// - `name`: The name of the symbol whose kind ID is to be retrieved.
    ///
    /// # Returns
    /// - The ID of the kind associated with the symbol.
    ///
    /// # Panics
    /// - Panics if the symbol does not exist in the symbol table.
    fn get_static_kind_id(&self, name: &str) -> u32 {
        self.symbol_table.get(name).unwrap().kind.borrow().id
    }

    /// Resolves the type of an identifier in the current context.
    ///
    /// This function looks up the type of an identifier by traversing the context stack from the current
    /// scope up to the global scope. It checks if the identifier is declared and returns its type. If the
    /// identifier is not found or if its type is unknown, it raises an error.
    ///
    /// # Parameters
    /// - `token`: The token representing the identifier whose type is to be resolved.
    ///
    /// # Returns
    /// - The `TypeDescriptor` representing the type of the identifier.
    ///
    /// # Errors
    /// - Raises an error if the identifier is not found or if its type is unknown.
    pub(crate) fn resolve_identifier_type(
        &mut self,
        token: &Token,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        self.require_depth(
            Ordering::Greater,
            token,
            0,
            format!(
                "Get identifier value is only allowed inside functions. At line {}.",
                token.line
            ),
        )?;

        let mut i = self.context_stack.len() - 1;

        loop {
            match self.context_stack.get(i).name(&token.lexeme) {
                Some(symbol) => match symbol.kind {
                    Some(kind) => {
                        return Ok(kind);
                    }
                    None => gpp_error!(
                        "The kind of '{}' are not known here. At line {}.",
                        token.lexeme,
                        token.line
                    ),
                },
                None => {
                    if i == 0 {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::SymbolNotFound {
                                symbol_kind: format!("variable"),
                                symbol_name: token.lexeme.clone(),
                            },
                            Some(token.line),
                            token.span,
                        ));
                    }

                    if self.check_type_exists(&token.lexeme) {
                        return self.get_static_kind_by_name(
                            &token.lexeme,
                            &Expression::Literal(token.clone(), token.span),
                        );
                    }

                    i -= 1;
                    continue;
                }
            }
        }

        gpp_error!(
            "The name '{}' are not declared here. At line {}.",
            token.lexeme,
            token.line
        );
    }

    /// Retrieves a symbol from the context stack by name, checking all levels of scope.
    ///
    /// This function attempts to find a symbol by name starting from the most recent scope and working
    /// backwards through previous scopes. If the symbol is found, it returns it. If the symbol is not found
    /// in any scope, it raises an error.
    ///
    /// # Parameters
    /// - `name`: The token representing the name of the symbol to look for.
    ///
    /// # Returns
    /// - An `Option<SemanticValue>` representing the symbol found in the context stack, or `None` if not found.
    ///
    /// # Errors
    /// - Raises an error if the symbol is not found in the current context or any parent contexts.
    pub(crate) fn get_name_in_depth(&mut self, name: &Token) -> TyResult<Option<SemanticValue>> {
        let mut i = self.context_stack.len() - 1;

        loop {
            match self.context_stack.get(i).name(&name.lexeme) {
                Some(symbol) => {
                    return Ok(Some(symbol));
                }
                None => {
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                    continue;
                }
            }
        }

        Err(CompilationError::with_span(
            CompilationErrorKind::UsageOfUndeclaredVariable {
                name: name.lexeme.clone(),
            },
            Some(name.line),
            name.span,
        ))
    }

    /// Asserts that the given expression matches the expected type.
    ///
    /// This function checks if the type of the given expression matches the expected type. If the types do
    /// not match, it raises an error with an appropriate message.
    ///
    /// # Parameters
    /// - `expr`: The expression whose type is being checked.
    /// - `expected_kind`: The expected type for the expression.
    /// - `location`: The token representing the location of the expression in the code.
    ///
    /// # Errors
    /// - Raises an error if the type of the expression does not match the expected type.
    pub(crate) fn assert_expression_kind(
        &mut self,
        expr: &Expression,
        expected_kind: Rc<RefCell<TypeDescriptor>>,
        location: &Expression,
    ) -> TyResult<()> {
        let expr_kind = self.resolve_expr_type(expr)?;

        if expr_kind.borrow().id != expected_kind.borrow().id {
            return Err(CompilationError::with_span(
                CompilationErrorKind::ExpectType {
                    expect: expected_kind.borrow().name.clone(),
                    found: expr_kind.borrow().name.clone(),
                    compiler_msg: None,
                },
                Some(location.line()),
                location.span(),
            ));
        }

        Ok(())
    }

    /// Retrieves the static type descriptor for a given name from the symbol table.
    ///
    /// This function looks up a type descriptor in the symbol table by name. If the symbol is found, it returns
    /// the corresponding type descriptor. If the symbol does not exist, it raises an error.
    ///
    /// # Parameters
    /// - `name`: The name of the type to retrieve.
    ///
    /// # Returns
    /// - The `TypeDescriptor` corresponding to the name.
    ///
    /// # Panics
    /// - Panics if the symbol does not exist in the symbol table.
    pub(crate) fn get_static_kind_by_name(
        &self,
        name: &str,
        location: &Expression,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        if self.check_type_exists(&name.to_string()) {
            Ok(self.symbol_table.get(name).unwrap().kind.clone())
        } else {
            Err(CompilationError::with_span(
                CompilationErrorKind::InexistentType {
                    r#type: name.clone().to_string(),
                },
                Some(location.line()),
                location.span(),
            ))
        }
    }

    pub(super) fn get_static_kind_by_id(&self, id: u32) -> Rc<RefCell<TypeDescriptor>> {
        match self.symbol_table.get_type_by_id(id) {
            Some(desc) => desc,
            None => {
                println!("Current data: {}", self.current_symbol);
                println!(
                    "Current symbols: {}",
                    self.symbol_table
                        .names
                        .values()
                        .map(|t| format!("{}:{}", t.kind.borrow().id, t.kind.borrow().name))
                        .collect::<Vec<_>>()
                        .join("\n")
                );
                gpp_error!("Type with id `{}` not exists", id)
            }
        }
    }

    /// Checks whether two types are the same kind (i.e., have the same type ID).
    ///
    /// This function compares two `TypeDecl` values and raises an error with a custom message if their
    /// `kind_id` values are not equal. It is useful for ensuring that two types are compatible or
    /// match in a certain context.
    ///
    /// # Parameters
    /// - `a`: The first `TypeDecl` to compare.
    /// - `b`: The second `TypeDecl` to compare.
    /// - `msg`: A custom error message to be included if the types do not match.
    ///
    /// # Errors
    /// - Raises an error with the provided message if the types are not the same kind.
    fn is_same_kind(&self, a: TypeDecl, b: TypeDecl, msg: String) {
        if a.kind_id != b.kind_id {
            gpp_error!("{}", msg);
        }
    }

    /// Asserts that an expression's type conforms to a given archetype.
    ///
    /// # Parameters
    /// - `expr`: The expression whose type needs to be checked.
    /// - `archetype_source`: A `TypeDecl` representing the expected archetype(s).
    /// - `msg`: A custom error message to be included if the assertion fails.
    ///
    /// # Behavior
    /// - Resolves the type of the given expression.
    /// - Checks if the expression's type implements all required archetypes from `archetype_source`.
    /// - If the type does not conform, an error is raised with a detailed message.
    ///
    /// This function ensures that expressions match the expected type constraints, enforcing type safety.
    pub(crate) fn assert_archetype_kind(
        &mut self,
        expr: &Expression,
        archetype_source: Rc<RefCell<TypeDescriptor>>,
        msg: &str,
    ) -> TyResult<()> {
        let expr_kind = self.resolve_expr_type(expr)?;

        let mut is_same = true;

        for archtype in archetype_source.borrow().archetypes.iter() {
            if !expr_kind.borrow().implements_archetype(archtype) {
                is_same = false;
            }
        }

        if !is_same {
            return Err(CompilationError::with_span(
                CompilationErrorKind::ExpectType {
                    expect: archetype_source.borrow().name.clone(),
                    found: expr_kind.borrow().name.clone(),
                    compiler_msg: Some(msg.to_string()),
                },
                Some(expr.line()),
                expr.span(),
            ));
        }

        Ok(())
    }

    /// Defines a new function in the symbol table.
    ///
    /// This function adds a function prototype to the symbol table, associating it with the specified
    /// function name. This enables later lookups of the function by its name.
    ///
    /// # Parameters
    /// - `name`: The name of the function being defined.
    /// - `value`: The `FunctionPrototype` representing the function's signature.
    ///
    /// # Example
    /// ```rust
    /// define_function("my_function".to_string(), my_function_prototype);
    /// ```
    pub(super) fn define_function(&mut self, name: String, value: FunctionPrototype) {
        self.symbol_table.define_function(name, value);
    }

    /// Resolves the return type of a function call.
    ///
    /// This function checks the return type of a function when it is called, based on the function's
    /// prototype stored in the symbol table. It ensures that the function is defined and retrieves
    /// its return type.
    ///
    /// # Parameters
    /// - `callee`: The expression representing the function being called.
    /// - `paren`: The token representing the opening parenthesis of the function call.
    /// - `args`: The arguments passed to the function call.
    ///
    /// # Returns
    /// - A `TypeDescriptor` representing the return type of the function.
    ///
    /// # Errors
    /// - Raises an error if the function is not declared in the current scope or if the callee is not a valid function call.
    pub(super) fn resolve_function_return_type(
        &mut self,
        callee: &Expression,
        paren: &Token,
        args: &Vec<Expression>,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        match callee {
            Expression::Variable(name, span) => {
                let mut function = self.symbol_table.get_function(&name.lexeme.clone());

                if let None = function {
                    function = self.symbol_table.native_functions.get_mut(&name.lexeme);
                }

                match function {
                    Some(prototype) => {
                        return Ok(prototype.return_kind.clone());
                    }
                    None => gpp_error!(
                        "Function '{}' are not declared in this scope.",
                        name.lexeme.clone()
                    ),
                }
            }
            Expression::Get(callee, name, span) => self.resolve_get_expr(&callee, name),
            _ => {
                gpp_error!(
                    "Call functions inside modules are currently not allowed {}.",
                    callee
                );
            }
        }
    }

    /// Asserts that the arguments passed to a function call match the expected types.
    ///
    /// This function compares the types of the arguments passed to a function call against the expected
    /// types specified in the function's prototype. If the types don't match, an error is raised.
    ///
    /// # Parameters
    /// - `prototype`: The `FunctionPrototype` representing the expected signature of the function.
    /// - `args`: A vector of expressions representing the arguments passed to the function call.
    ///
    /// # Errors
    /// - Raises an error if any of the argument types do not match the expected types.
    pub(crate) fn assert_function_args(
        &mut self,
        prototype: FunctionPrototype,
        args: &Vec<Expression>,
        paren: &Token,
    ) -> TyResult<()> {
        for (index, arg) in args.iter().enumerate() {
            let proto_arg_kind = self.resolve_expr_type(&prototype.params[index].kind)?;
            let passed_arg_kind = self.resolve_expr_type(arg)?;

            self.assert_archetype_kind(
                arg,
                proto_arg_kind.clone(),
                format!(
                    "Expect '{}' to '{}' param, but got '{}'. At line {}.",
                    proto_arg_kind.borrow().name,
                    prototype.params[index].name.lexeme,
                    passed_arg_kind.borrow().name,
                    paren.line
                )
                .as_str(),
            )?;
        }

        Ok(())
    }

    /// Retrieves a function prototype by its name.
    ///
    /// This function looks up the function prototype in the symbol table by its name, allowing
    /// access to the function's signature, such as its parameters and return type.
    ///
    /// # Parameters
    /// - `name`: The name of the function being retrieved.
    ///
    /// # Returns
    /// - An `Option<&mut FunctionPrototype>` containing a mutable reference to the function's prototype
    ///   if it exists, or `None` if the function is not defined.
    ///
    /// # Example
    /// ```rust
    /// let function = get_function("my_function");
    /// ```
    pub(crate) fn get_function(&mut self, name: &str) -> Option<&mut FunctionPrototype> {
        self.symbol_table.get_function(name)
    }

    /// Defines a local variable in the current context.
    ///
    /// This function declares a local variable by adding it to the current context, associating
    /// the variable's name with its semantic value (type, scope, etc.).
    ///
    /// # Parameters
    /// - `name`: The name of the variable being declared.
    /// - `value`: The `SemanticValue` associated with the variable, containing type and other details.
    pub(super) fn define_local(&mut self, name: &str, value: SemanticValue) {
        self.context().declare_name(name, value);
    }

    /// Retrieves a `TypeDescriptor` that matches a set of archetypes.
    ///
    /// This function checks the symbol table to find a `TypeDescriptor` whose archetypes match
    /// the provided set of archetypes.
    ///
    /// # Parameters
    /// - `sets`: A slice of `Archetype` values to match against the types in the symbol table.
    ///
    /// # Returns
    /// - An `Option<TypeDescriptor>` representing the matching type, or `None` if no match is found.
    ///
    /// # Example
    /// ```rust
    /// let result = get_by_archetype(&[Archetype::new("object".to_string())]);
    /// ```
    pub(super) fn get_by_archetype(
        &mut self,
        sets: &[Archetype],
    ) -> Option<Rc<RefCell<TypeDescriptor>>> {
        let target_set: HashSet<_> = sets.iter().cloned().collect();

        match self
            .symbol_table
            .names
            .iter()
            .find(|decl| decl.1.kind.borrow().archetypes == target_set)
        {
            Some((name, value)) => Some(value.kind.clone()),
            None => None,
        }
    }

    /// Asserts that two types are equal.
    ///
    /// This function compares two `TypeDescriptor` values and raises an error if they are not equal
    /// in terms of their archetypes.
    ///
    /// # Parameters
    /// - `source`: The source `TypeDescriptor` to check.
    /// - `target`: The target `TypeDescriptor` to compare against.
    /// - `msg`: The error message to display if the types do not match.
    ///
    /// # Errors
    /// - Raises an error if the archetypes of the `source` and `target` types do not match.
    pub(super) fn assert_kind_equals(
        &self,
        source: Rc<RefCell<TypeDescriptor>>,
        target: Rc<RefCell<TypeDescriptor>>,
        msg: String,
    ) -> TyResult<()> {
        for i in &target.borrow().archetypes {
            if !source.borrow().archetypes.contains(&i) {
                return Err(CompilationError::new(
                    CompilationErrorKind::TypeAssertion {
                        msg: format!("{}", msg),
                    },
                    None,
                ));
            }
        }

        Ok(())
    }

    /// Retrieves the type descriptor of a user-defined symbol.
    ///
    /// This function looks up a symbol's name in the symbol table and retrieves the `TypeDescriptor`
    /// associated with it. It assumes the symbol is user-defined (i.e., not a built-in type).
    ///
    /// # Parameters
    /// - `name`: The name of the symbol whose type descriptor is being retrieved.
    ///
    /// # Returns
    /// - The `TypeDescriptor` of the user-defined symbol.
    ///
    /// # Errors
    /// - Panics if the symbol is not found in the symbol table.
    pub(super) fn get_user_defined_kind(&self, name: String) -> Rc<RefCell<TypeDescriptor>> {
        self.symbol_table.names.get(&name).unwrap().kind.clone()
    }

    /// Checks whether a type or symbol exists in the symbol table.
    ///
    /// This function checks if the given name is present in the symbol table, indicating that a type
    /// or symbol has been defined with that name.
    ///
    /// # Parameters
    /// - `name`: The name of the symbol or type to check for.
    ///
    /// # Returns
    /// - `true` if the symbol exists, `false` otherwise.
    pub(super) fn check_type_exists(&self, name: &String) -> bool {
        self.symbol_table.names.contains_key(name)
    }

    /// Retrieves a native function's prototype by its name.
    ///
    /// This function looks up a native function by its name in the symbol table and returns its prototype
    /// if it exists. Native functions are predefined in the language.
    ///
    /// # Parameters
    /// - `name`: The name of the native function.
    ///
    /// # Returns
    /// - An `Option<FunctionPrototype>` that represents the native function's prototype, or `None`
    ///   if the function is not found.
    ///
    /// # Example
    /// ```rust
    /// let native_fn = get_native_function("print");
    /// ```
    pub(crate) fn get_native_function(&self, name: &str) -> Option<&FunctionPrototype> {
        self.symbol_table.native_functions.get(name)
    }

    pub(super) fn define_native_function(&mut self, name: String, value: FunctionPrototype) {
        self.symbol_table.native_functions.insert(name, value);
    }

    pub(super) fn resolve_module_path(&self, relative_path: &PathBuf) -> Option<PathBuf> {
        if let stdlib_root = &self.config.stdlib_path {
            let potential_path = stdlib_root.join(relative_path);
            if potential_path.exists() {
                return Some(potential_path);
            }
        }

        let potential_path = self.config.root.join(relative_path);
        if potential_path.exists() {
            return Some(potential_path);
        }

        None
    }

    fn setup_prelude(&mut self) -> TyResult<TyStmts> {
        let mut stmts: TyStmts = Vec::new();

        let std_core = self.build_import_path(&["stdlib", "prelude"]);

        self.import(&std_core, &mut stmts)?;

        Ok(stmts)
    }

    fn import(&mut self, path: &Vec<Token>, stmts: &mut TyStmts) -> TyResult<()> {
        let mut import_stmts = match self.analyze_import(path) {
            Err(e) => {
                self.report_error(e);
                vec![]
            }
            Ok(s) => s,
        };
        stmts.append(&mut import_stmts);
        Ok(())
    }

    fn build_import_path(&self, path: &[&str]) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        for p in path {
            tokens.push(Token::new(
                TokenKind::Identifier,
                (*p).into(),
                1,
                0,
                Span { start: 0, end: 0 },
            ));
        }

        tokens
    }
}
