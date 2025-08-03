#![allow(clippy::result_large_err)]

use std::{cell::RefCell, cmp::Ordering, collections::HashMap, path::PathBuf, rc::Rc};

use skyl_data::{
    AnnotatedExpression, AnnotatedStatement, Archetype, Ast, BuiltinAttributeUsage, Decorator,
    Expression, FieldDeclaration, FieldDescriptor, FunctionPrototype, MethodParameter,
    SemanticValue, Span, Statement, SymbolKind, Token, TypeDecl, TypeDescriptor, ValueWrapper,
    read_file_without_bom,
};
use skyl_driver::{
    errors::{CompilationError, CompilationErrorKind},
    gpp_error,
};

use crate::{
    SemanticAnalyzer,
    import_pipeline::ModuleImportPipeline,
    result::{TyResult, TyStmts},
};

impl SemanticAnalyzer {
    /// Analyzes a statement and performs semantic validation.
    ///
    /// This function processes different types of statements and ensures they adhere
    /// to semantic rules. It delegates specific checks to corresponding handlers
    /// based on the statement type.
    ///
    /// # Arguments
    ///
    /// * `stmt` - A reference to the statement to be analyzed.
    ///
    /// # Returns
    ///
    /// An `AnnotatedStatement` containing the analyzed and validated statement.
    pub(super) fn analyze_stmt(
        &mut self,
        stmt: &Statement,
        _span: &Span,
        _line: &usize,
    ) -> Result<TyStmts, CompilationError> {
        match stmt {
            Statement::Return(keyword, value, _, _) => {
                Ok(vec![self.analyze_return(keyword, value)?])
            }
            Statement::Expression(expr, span, line) => Ok(vec![AnnotatedStatement::Expression(
                self.analyze_expr(expr)?,
                *span,
                *line,
            )]),
            Statement::Decorator(hash_token, attribs, _, _) => {
                Ok(vec![self.analyze_decorator(hash_token, attribs)?])
            }
            Statement::Type(name, archetypes, fields, _, _) => {
                let result = self.analyze_type(name, archetypes, fields);
                Ok(vec![result?])
            }
            Statement::Function(name, params, body, return_kind, _, _) => {
                let result = self.analyze_function(name, params, body, return_kind)?;
                self.analyze_control_flow(&result);
                Ok(vec![result])
            }
            Statement::NativeFunction(name, params, return_kind, _, _) => {
                Ok(vec![self.analyze_native_function(
                    name,
                    params,
                    return_kind,
                )?])
            }
            Statement::Variable(name, value, _, _) => {
                Ok(vec![self.analyze_variable_declaration(name, value)?])
            }

            Statement::While(condition, body, _, _) => {
                Ok(vec![self.analyze_while_stmt(condition, body)?])
            }
            Statement::If(keyword, condition, body, else_branch, _, _) => {
                Ok(vec![self.analyze_if_stmt(
                    keyword,
                    condition,
                    body,
                    else_branch,
                )?])
            }
            Statement::BuiltinAttribute(name, kinds, _, _) => {
                Ok(vec![self.analyze_builtin_attribute(name, kinds)?])
            }
            Statement::InternalDefinition(name, params, body, return_kind, _, _) => {
                let result = self.analyze_internal_definition(name, params, body, return_kind)?;
                self.analyze_control_flow(&result);
                Ok(vec![result])
            }
            Statement::DestructurePattern(fields, value, _, _) => {
                Ok(self.analyze_destructure_pattern(fields, value)?)
            }
            Statement::Scope(stmts, span, line) => {
                let stmts = self.analyze_scope(stmts)?;
                let mut boxed_statements: Vec<Box<AnnotatedStatement>> = Vec::new();

                for stmt in stmts {
                    boxed_statements.push(Box::new(stmt));
                }

                Ok(vec![AnnotatedStatement::Scope(
                    boxed_statements,
                    *span,
                    *line,
                )])
            }
            Statement::Import(module, _, _) => {
                let result = self.analyze_import(module);

                match result {
                    Err(e) => {
                        self.report_error(e);
                        Ok(vec![])
                    }
                    Ok(s) => Ok(s),
                }
            }
            Statement::EndCode => Ok(vec![]),
            _ => gpp_error!("Statement {:?} not supported.", stmt),
        }
    }

    fn analyze_destructure_pattern(
        &mut self,
        fields: &Vec<Token>,
        value: &Expression,
    ) -> TyResult<TyStmts> {
        let value_kind = self.resolve_expr_type(value)?;
        let mut declarations: TyStmts = Vec::new();

        for field in fields {
            if value_kind.borrow().fields.contains_key(&field.lexeme) {
                let field_kind = value_kind.borrow().fields[&field.lexeme].kind.clone();
                if self.context().contains_name(&field.lexeme) {
                    return Err(CompilationError::with_span(
                        CompilationErrorKind::DuplicatedVariable {
                            name: field.lexeme.clone(),
                            previous: self.context().name(&field.lexeme).unwrap().line,
                        },
                        Some(field.line),
                        field.span,
                    ));
                }

                self.define_local(
                    &field.lexeme,
                    SemanticValue::new(Some(field_kind), ValueWrapper::Internal, field.line),
                );

                let _get_kind = self.resolve_expr_type(&Expression::Get(
                    Rc::new(value.clone()),
                    field.clone(),
                    field.span,
                ));

                let field_get = AnnotatedExpression::Get(
                    Box::new(self.analyze_expr(value)?),
                    field.clone(),
                    value_kind.clone(),
                );

                declarations.push(AnnotatedStatement::Variable(
                    field.clone(),
                    Some(field_get),
                    fields.first().unwrap().span.merge(value.span()),
                    fields.first().unwrap().line,
                ));
            } else {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::NotFoundField {
                        field: field.lexeme.clone(),
                        r#type: value_kind.borrow().name.clone(),
                    },
                    Some(field.line),
                    field.span,
                ));
            }
        }

        Ok(declarations)
    }

    fn analyze_scope(&mut self, stmts: &Vec<Rc<Statement>>) -> TyResult<TyStmts> {
        let mut annotated_stmts: TyStmts = Vec::new();

        self.begin_scope();

        for stmt in stmts {
            let annotated_stmt = self.analyze_stmt(stmt, &stmt.span(), &stmt.line());
            match annotated_stmt {
                Err(e) => {
                    self.end_scope();
                    return Err(e);
                }
                Ok(mut s) => {
                    annotated_stmts.append(&mut s);
                }
            }
        }

        self.end_scope();

        Ok(annotated_stmts)
    }

    pub(super) fn analyze_import(&mut self, module: &Vec<Token>) -> TyResult<TyStmts> {
        let mut relative_path = PathBuf::new();
        for token in module {
            relative_path.push(&token.lexeme);
        }
        relative_path.set_extension("gpp");

        let full_path = match self.resolve_module_path(&relative_path) {
            Some(path) => path,
            None => {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::ModuleNotFound {
                        path: module.iter().map(|t| t.lexeme.clone()).collect(),
                    },
                    Some(module.last().unwrap().line),
                    module.last().unwrap().span,
                ));
            }
        };

        if let Ok(canonical_path) = full_path.canonicalize() {
            if self
                .modules
                .contains(&canonical_path.to_str().unwrap().into())
            {
                return Ok(vec![]);
            }
            self.modules.push(canonical_path.to_str().unwrap().into());
        } else {
            return Err(CompilationError::with_span(
                CompilationErrorKind::ModuleAccessDenied {
                    path: module.iter().map(|t| t.lexeme.clone()).collect(),
                    full_path: full_path.display().to_string(),
                },
                Some(module.last().unwrap().line),
                module.last().unwrap().span,
            ));
        }

        let source = match read_file_without_bom(full_path.to_str().unwrap()) {
            Ok(s) => s,
            Err(_) => {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::ModuleReadError {
                        path: module.iter().map(|t| t.lexeme.clone()).collect(),
                        error: "Error to read module".into(),
                        full_path: full_path.display().to_string(),
                    },
                    Some(module.last().unwrap().line),
                    module.last().unwrap().span,
                ));
            }
        };

        if self.config.verbose {
            println!("Importing: {}", full_path.display());
        }

        if let Some(ctx) = &mut self.ctx {
            ctx.borrow_mut()
                .push_module(full_path.to_str().unwrap().into());
        }

        let mut import_pipeline = ModuleImportPipeline::get();
        let ast = import_pipeline
            .execute(
                source.content.clone(),
                &self.config.clone(),
                self.ctx.clone().unwrap(),
                Rc::clone(&self.reporter),
            )
            .unwrap()
            .downcast::<Ast>()
            .unwrap();

        let mut imported_annotated_stmts = Vec::new();

        let old_stmts = self.statements.clone();
        let old_index = self.current_stmt;

        self.current_stmt = 0;
        self.statements = ast.statements.clone();

        for stmt in ast.statements {
            let mut s = match self.analyze_stmt(
                &stmt,
                &module
                    .first()
                    .unwrap()
                    .span
                    .merge(module.last().unwrap().span),
                &module.first().unwrap().line,
            ) {
                Err(e) => {
                    self.report_error(e);
                    vec![]
                }

                Ok(s) => s,
            };

            imported_annotated_stmts.append(&mut s);
            self.advance();
        }

        if let Some(ctx) = &mut self.ctx {
            ctx.borrow_mut().pop_module();
        }

        self.statements = old_stmts;
        self.current_stmt = old_index;

        Ok(imported_annotated_stmts)
    }

    fn analyze_internal_definition(
        &mut self,
        name: &Token,
        params: &Vec<FieldDeclaration>,
        body: &Statement,
        return_kind: &Expression,
    ) -> TyResult<AnnotatedStatement> {
        self.require_depth(
            Ordering::Less,
            name,
            1,
            format!(
                "Definitions are only allowed in top level code. At line {}.",
                name.line
            ),
        )?;

        self.current_return_kind_id = Some(self.resolve_expr_type(return_kind)?.borrow().id);
        self.current_symbol_kind = SymbolKind::InternalDefinition;

        if let Expression::TypeComposition(mask, _) = return_kind {
            self.resolve_type_composition(mask)?;
        } else {
            self.get_static_kind_by_name("void", return_kind)?;
            return Err(CompilationError::new(
                CompilationErrorKind::MissingConstruction {
                    construction: "function return kind".into(),
                },
                Some(name.line),
            ));
        }

        let target = self.resolve_expr_type(&params[0].kind)?;

        if target.borrow().methods.contains_key(&name.lexeme) {
            return Err(CompilationError::with_span(
                CompilationErrorKind::DuplicatedDefinition {
                    kind: "internal".into(),
                    definition: name.lexeme.clone(),
                    target: target.borrow().name.clone(),
                },
                Some(name.line),
                name.span,
            ));
        }

        let target_name = target.borrow().name.clone();
        let target_id = target.borrow().id;
        self.current_descriptor_id = Some(target_id);

        let mut method_params: Vec<MethodParameter> = Vec::new();
        for field_decl in params {
            let param_kind = self.resolve_expr_type(&field_decl.kind)?;
            method_params.push(MethodParameter::new(
                field_decl.name.lexeme.clone(),
                param_kind,
            ));
        }

        self.current_symbol = name.lexeme.clone();

        self.begin_scope();
        for arg in params {
            let kind = match self.resolve_expr_type(&arg.kind) {
                Err(e) => {
                    self.end_scope();
                    return Err(e);
                }
                Ok(k) => k,
            };
            self.define_local(
                &arg.name.lexeme,
                SemanticValue::new(Some(kind), ValueWrapper::Internal, arg.name.line),
            );
        }

        let mut annotated_body = Vec::new();
        match body {
            Statement::Scope(stmts, span, line) => {
                for stmt in stmts {
                    let inner_stmts = self.analyze_stmt(stmt, span, line);

                    match inner_stmts {
                        Err(e) => self.report_error(e),
                        Ok(stmts) => {
                            for s in stmts {
                                annotated_body.push(Box::new(s));
                            }
                        }
                    }
                }
            }
            _ => gpp_error!("Statement {:?} is not allowed here.", body),
        }
        self.end_scope();

        let arity = method_params.len();
        let return_kind_type = self.resolve_expr_type(return_kind)?;
        let return_kind_id = return_kind_type.borrow().id;

        self.add_method_to_defined_type(
            name.lexeme.clone(),
            &target_name,
            method_params,
            arity,
            target_id,
            return_kind_id,
            false,
        );

        let function_definition = FunctionPrototype::new(
            name.lexeme.clone(),
            params.clone(),
            params.len(),
            return_kind_type,
        );

        self.process_internal_definition(name, &function_definition)?;

        Ok(AnnotatedStatement::InternalDefinition(
            target,
            function_definition,
            Box::new(AnnotatedStatement::Scope(
                annotated_body,
                body.span(),
                body.line(),
            )),
            name.span.merge(body.span()),
            name.line,
        ))
    }

    fn analyze_builtin_attribute(
        &mut self,
        name: &Token,
        kinds: &Vec<Token>,
    ) -> TyResult<AnnotatedStatement> {
        let att_name = name.lexeme.clone();
        let mut att_kinds: Vec<Rc<RefCell<TypeDescriptor>>> = Vec::new();

        for kind in kinds {
            if !self.check_type_exists(&kind.lexeme) {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::NotFoundType {
                        name: kind.lexeme.clone(),
                    },
                    Some(kind.line),
                    kind.span,
                ));
            }

            let att_kind = self.get_static_kind_by_name(
                &kind.lexeme,
                &Expression::Literal(kind.clone(), kind.span),
            )?;
            att_kinds.push(att_kind);
        }

        self.symbol_table
            .define_attribute(att_name, att_kinds.clone());

        Ok(AnnotatedStatement::BuiltinAttribute(
            name.clone(),
            att_kinds,
            name.span,
            name.line,
        ))
    }

    /// Analyzes an `if` statement and generates an annotated statement.
    ///
    /// This function processes an `if` statement by analyzing its condition, body, and optional
    /// else branch. It ensures that the condition is of the correct type (boolean), and the bodies
    /// of the `if` and `else` branches are valid statements. The function also manages scope creation
    /// and closure during the analysis of the statement.
    ///
    /// # Parameters
    /// - `keyword`: A reference to the `Token` representing the `if` keyword. It is used to track
    ///   the statement's location for error reporting.
    /// - `condition`: A reference to the `Expression` representing the condition of the `if` statement.
    ///   This expression is validated to ensure it is of type `bool`.
    /// - `body`: A reference to the `Statement` representing the body of the `if` statement.
    /// - `else_branch`: An optional reference to a `Box<Statement>` representing the body of the
    ///   `else` branch. If present, this branch is analyzed as well.
    ///
    /// # Returns
    /// This function returns an `AnnotatedStatement` representing the analyzed `if` statement. The
    /// returned statement contains the annotated condition, the body of the `if` branch, and the
    /// body of the `else` branch (if present).
    ///
    /// # Errors
    /// - If the condition is not of type `bool`, an error is reported.
    /// - If the bodies of the `if` or `else` branches are not valid statements, an error is reported.
    /// - If the `else` branch contains an invalid statement, an error is reported.
    fn analyze_if_stmt(
        &mut self,
        keyword: &Token,
        condition: &Expression,
        body: &Statement,
        else_branch: &Option<Box<Statement>>,
    ) -> TyResult<AnnotatedStatement> {
        let annotated_condition = self.analyze_expr(condition)?;
        self.assert_expression_kind(
            condition,
            self.get_static_kind_by_name("bool", condition)?,
            condition,
        )?;

        let annotated_body;

        match body {
            Statement::Scope(stmts, _, _) => {
                annotated_body = self
                    .analyze_scope(stmts)?
                    .into_iter()
                    .map(|s| Box::new(s))
                    .collect()
            }
            _ => {
                return Err(CompilationError::new(
                    CompilationErrorKind::InvalidStatementScope {
                        statement: format!("{body:?}"),
                    },
                    None,
                ));
            }
        }

        let annotated_else;

        match else_branch {
            Some(stmt) => match stmt.as_ref() {
                Statement::Scope(stmts, span, line) => {
                    annotated_else = self
                        .analyze_scope(stmts)?
                        .into_iter()
                        .map(|s| Box::new(s))
                        .collect();

                    Ok(AnnotatedStatement::If(
                        keyword.clone(),
                        annotated_condition,
                        Box::new(AnnotatedStatement::Scope(
                            annotated_body,
                            body.span(),
                            body.line(),
                        )),
                        Some(Box::new(AnnotatedStatement::Scope(
                            annotated_else,
                            *span,
                            *line,
                        ))),
                        *span,
                        *line,
                    ))
                }
                Statement::If(keyword, condition, body, else_branch, span, line) => {
                    let annotated_else_branch =
                        self.analyze_if_stmt(keyword, condition, body, else_branch)?;
                    Ok(AnnotatedStatement::If(
                        keyword.clone(),
                        annotated_condition,
                        Box::new(AnnotatedStatement::Scope(annotated_body, *span, *line)),
                        Some(Box::new(annotated_else_branch)),
                        *span,
                        *line,
                    ))
                }
                _ => gpp_error!("Statement {:?} is not allowed here.", stmt),
            },

            None => Ok(AnnotatedStatement::If(
                keyword.clone(),
                annotated_condition,
                Box::new(AnnotatedStatement::Scope(
                    annotated_body,
                    body.span(),
                    body.line(),
                )),
                None,
                body.span(),
                body.line(),
            )),
        }
    }

    fn analyze_while_stmt(
        &mut self,
        condition: &Expression,
        body: &Statement,
    ) -> TyResult<AnnotatedStatement> {
        let annotated_condition = self.analyze_expr(condition)?;

        let kind = self.resolve_expr_type(condition)?;

        if !kind
            .borrow()
            .implements_archetype(&Archetype::new("bool".to_string()))
        {
            return Err(CompilationError::with_span(
                CompilationErrorKind::ExpectType {
                    expect: "bool".into(),
                    found: kind.borrow().name.clone(),
                    compiler_msg: None,
                },
                Some(condition.line()),
                condition.span(),
            ));
        }

        let mut annotated_body: Vec<Box<AnnotatedStatement>> = Vec::new();

        match body {
            Statement::Scope(statements, span, line) => {
                for stmt in statements {
                    for s in self.analyze_stmt(stmt, span, line)? {
                        annotated_body.push(Box::new(s));
                    }
                }
            }

            _ => {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::UsageOfNotRequiredStatement {
                        statement: "scopes".into(),
                        place: "while".into(),
                    },
                    Some(condition.line()),
                    condition.span(),
                ));
            }
        }

        Ok(AnnotatedStatement::While(
            annotated_condition,
            Box::new(AnnotatedStatement::Scope(
                annotated_body,
                body.span(),
                body.line(),
            )),
            condition.span().merge(body.span()),
            condition.line(),
        ))
    }

    /// Analyzes a for-each loop and ensures semantic correctness.
    ///
    /// This function verifies that the loop condition is iterable and processes
    /// the loop body within a new scope to ensure proper variable handling.
    ///
    /// # Arguments
    ///
    /// * `variable` - The loop variable.
    /// * `condition` - The iterable expression.
    /// * `body` - The statement representing the loop body.
    ///
    /// # Returns
    ///
    /// An `AnnotatedStatement::ForEach` containing the analyzed loop structure.
    pub(super) fn _analyze_iterator(
        &mut self,
        variable: &Token,
        condition: &Expression,
        body: &Statement,
    ) -> TyResult<AnnotatedStatement> {
        self.begin_scope();

        let annotated_iterator: AnnotatedExpression;
        let iterator_kind: Rc<RefCell<TypeDescriptor>>;

        match condition {
            Expression::Variable(variable, _) => {
                self.assert_archetype_kind(
                    condition,
                    self.get_static_kind_by_name("iterator", condition)?,
                    "Expect iterator in 'for' loop.",
                )?;

                // iterator_kind =
                self.resolve_identifier_type(variable)?;
                annotated_iterator = self.analyze_expr(condition)?;
            }

            Expression::Call(callee, paren, args, _) => {
                iterator_kind = self.resolve_function_return_type(callee, paren, args)?;
                self.assert_kind_equals(
                    iterator_kind.clone(),
                    self.get_static_kind_by_name("iterator", callee)?,
                    "Expect iterator in for each declaration.".to_string(),
                )?;

                annotated_iterator = self.analyze_expr(condition)?;
            }

            _ => {
                // iterator_kind =
                self.resolve_iterator_kind(condition)?;
                annotated_iterator = self.analyze_expr(condition)?;
            }
        }

        let mut annotated_body = Vec::new();

        match body {
            Statement::Scope(stmts, span, line) => {
                for stmt in stmts {
                    let stmt_vec = self.analyze_stmt(stmt, span, line)?;

                    for s in stmt_vec {
                        annotated_body.push(Box::new(s));
                    }
                }
            }
            _ => gpp_error!("Statement {:?} is not allowed here.", body),
        }

        self.end_scope();

        Ok(AnnotatedStatement::ForEach(
            variable.clone(),
            annotated_iterator,
            Box::new(AnnotatedStatement::Scope(
                annotated_body,
                body.span(),
                body.line(),
            )),
            variable.span.merge(body.span()),
            variable.line,
        ))
    }

    /// Analyzes a variable declaration and ensures it adheres to semantic rules.
    ///
    /// This function checks if the variable name is already declared within the
    /// current scope and processes its initialization expression if provided.
    ///
    /// # Arguments
    ///
    /// * `name` - The token representing the variable name.
    /// * `value` - An optional expression representing the variable's initial value.
    ///
    /// # Returns
    ///
    /// An `AnnotatedStatement::Variable` containing the analyzed variable declaration.
    fn analyze_variable_declaration(
        &mut self,
        name: &Token,
        value: &Option<Expression>,
    ) -> TyResult<AnnotatedStatement> {
        let ctx_name = self.context().name(&name.lexeme);

        match ctx_name {
            Some(sm) => Err(CompilationError::with_span(
                CompilationErrorKind::DuplicatedVariable {
                    name: name.lexeme.clone(),
                    previous: sm.line,
                },
                Some(name.line),
                name.span,
            )),
            None => match value {
                Some(expr) => {
                    let annotated_value = self.analyze_expr(expr)?;

                    let value = SemanticValue::new(
                        Some(self.resolve_expr_type(expr)?),
                        ValueWrapper::Internal,
                        name.line,
                    );

                    if value.kind.as_ref().unwrap().borrow().name == "void" {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::UsingVoidToAssignVariableOrParam,
                            Some(name.line),
                            name.span,
                        ));
                    }

                    let context = &mut self.context();
                    context.declare_name(&name.lexeme, value);
                    Ok(AnnotatedStatement::Variable(
                        name.clone(),
                        Some(annotated_value),
                        name.span.merge(expr.span()),
                        name.line,
                    ))
                }
                None => {
                    let value = SemanticValue::new(None, ValueWrapper::Internal, name.line);
                    let context = self.context();
                    context.declare_name(&name.lexeme, value);
                    Ok(AnnotatedStatement::Variable(
                        name.clone(),
                        None,
                        name.span,
                        name.line,
                    ))
                }
            },
        }
    }

    pub(super) fn analyze_native_function(
        &mut self,
        name: &Token,
        params: &[FieldDeclaration],
        return_kind: &Expression,
    ) -> TyResult<AnnotatedStatement> {
        self.require_depth(
            Ordering::Less,
            name,
            1,
            format!(
                "Functions are only allowed in top level code. At line {}.",
                name.line
            ),
        )?;

        if self.get_native_function(&name.lexeme).is_some() {
            return Err(CompilationError::with_span(
                CompilationErrorKind::DuplicatedNativeFunction {
                    name: name.lexeme.clone(),
                },
                Some(name.line),
                name.span,
            ));
        }

        self.current_symbol_kind = SymbolKind::Function;

        let kind: Rc<RefCell<TypeDescriptor>>;

        if let Expression::TypeComposition(mask, _) = return_kind {
            kind = self.resolve_type_composition(mask)?;
        } else {
            // kind = self.get_static_kind_by_name("void", return_kind)?;

            return Err(CompilationError::new(
                CompilationErrorKind::MissingConstruction {
                    construction: "function return kind".into(),
                },
                Some(name.line),
            ));
        }

        let function_definition = FunctionPrototype::new(
            name.lexeme.clone(),
            params.to_owned(),
            params.len(),
            kind.clone(),
        );

        self.define_native_function(name.lexeme.clone(), function_definition.clone());

        self.current_symbol = name.lexeme.clone();

        Ok(AnnotatedStatement::NativeFunction(
            function_definition,
            name.span.merge(return_kind.span()),
            name.line,
        ))
    }

    /// Analyzes a function definition and generates an annotated statement.
    ///
    /// This function processes the function's name, parameters, body, and return type, ensuring
    /// that the function is correctly defined within the top-level scope. It validates the function's
    /// return type, parameters, and checks for any semantic errors. The function body is analyzed
    /// and converted into an annotated scope containing the function's statements.
    ///
    /// # Parameters
    /// - `name`: A reference to the `Token` representing the function's name. It is used to track
    ///   the function's location for error reporting.
    /// - `params`: A reference to a vector of `FieldDeclaration` representing the function's
    ///   parameters. Each parameter has a name and a type.
    /// - `body`: A reference to the `Statement` representing the function's body. It contains the
    ///   actual code of the function.
    /// - `return_kind`: A reference to an `Expression` representing the return type of the function.
    ///   It defines the expected return type, such as a basic type or a composition of types.
    ///
    /// # Returns
    /// This function returns an `AnnotatedStatement` that represents the analyzed function definition.
    /// The returned statement contains the function prototype along with its annotated body.
    fn analyze_function(
        &mut self,
        name: &Token,
        params: &[FieldDeclaration],
        body: &Statement,
        return_kind: &Expression,
    ) -> TyResult<AnnotatedStatement> {
        self.require_depth(
            Ordering::Less,
            name,
            1,
            format!(
                "Functions are only allowed in top level code. At line {}.",
                name.line
            ),
        )?;

        self.current_symbol_kind = SymbolKind::Function;

        let kind: Rc<RefCell<TypeDescriptor>>;

        if let Expression::TypeComposition(mask, _) = return_kind {
            kind = self.resolve_type_composition(mask)?;
        } else {
            // kind = self.get_static_kind_by_name("void", return_kind)?;

            return Err(CompilationError::with_span(
                CompilationErrorKind::MissingConstruction {
                    construction: "function return kind".into(),
                },
                Some(name.line),
                return_kind.span(),
            ));
        }

        let function_definition = FunctionPrototype::new(
            name.lexeme.clone(),
            params.to_owned(),
            params.len(),
            kind.clone(),
        );

        self.define_function(name.lexeme.clone(), function_definition.clone());

        self.current_symbol = name.lexeme.clone();

        self.begin_scope();

        for arg in &function_definition.params {
            let kind = self.resolve_expr_type(&arg.kind)?;
            self.define_local(
                &arg.name.lexeme,
                SemanticValue::new(Some(kind), ValueWrapper::Internal, arg.name.line),
            );
        }

        let mut annotated_body = Vec::new();

        match body {
            Statement::Scope(stmts, span, line) => {
                for stmt in stmts {
                    let inner_stmts = self.analyze_stmt(stmt, span, line);

                    match inner_stmts {
                        Err(e) => self.report_error(e),
                        Ok(stmts) => {
                            for s in stmts {
                                annotated_body.push(Box::new(s));
                            }
                        }
                    }
                }
            }
            _ => {
                return Err(CompilationError::new(
                    CompilationErrorKind::InvalidStatementScope {
                        statement: format!("{body:?}"),
                    },
                    None,
                ));
            }
        }

        self.end_scope();

        self.process_function(name, &function_definition)?;

        Ok(AnnotatedStatement::Function(
            function_definition,
            Box::new(AnnotatedStatement::Scope(
                annotated_body,
                body.span(),
                body.line(),
            )),
            name.span.merge(body.span()),
            name.line,
        ))
    }

    /// Analyzes a return statement and ensures that the return type matches the function signature.
    ///
    /// This function checks if the return statement is within a function context and validates that
    /// the type of the returned expression matches the function's return type.
    ///
    /// # Parameters
    /// - `value`: The expression being returned.
    ///
    /// # Returns
    /// - An `AnnotatedStatement` representing the return statement, with the annotated return value.
    ///
    /// # Errors
    /// - Raises an error if the return statement is outside a function or if the return type does not match the function's signature.
    fn analyze_return(
        &mut self,
        keyword: &Token,
        value: &Option<Expression>,
    ) -> TyResult<AnnotatedStatement> {
        self.require_depth(
            Ordering::Greater,
            keyword,
            0,
            "Return statement are only allowed inside functions.".to_string(),
        )?;

        if self.current_symbol_kind != SymbolKind::Function
            && self.current_symbol_kind != SymbolKind::InternalDefinition
        {
            return Err(CompilationError::with_span(
                CompilationErrorKind::InvalidStatementUsage {
                    error: "Returns is only allowed inside functions or internal definitions."
                        .into(),
                },
                Some(keyword.line),
                keyword.span,
            ));
        }

        if let SymbolKind::Function = self.current_symbol_kind {
            let function = self.current_symbol.clone();
            let function_signature = self.get_function(&function).unwrap().clone();

            match value {
                Some(v) => {
                    if function_signature.return_kind.borrow().name == "void" {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::ExpectReturnType {
                                expect: "void".into(),
                                found: self.resolve_expr_type(v)?.borrow().name.clone(),
                            },
                            Some(v.line()),
                            v.span(),
                        ));
                    }

                    let annotated_value = self.analyze_expr(v)?;

                    self.assert_archetype_kind(
                        v,
                        function_signature.return_kind.clone(),
                        format!(
                            "Return of '{}' does not match with function signature.",
                            function.clone()
                        )
                        .as_str(),
                    )?;

                    Ok(AnnotatedStatement::Return(
                        Some(annotated_value),
                        keyword.span.merge(v.span()),
                        keyword.line,
                    ))
                }
                None => {
                    if function_signature.return_kind.borrow().name != "void" {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::ExpectReturnType {
                                expect: function_signature.return_kind.borrow().name.clone(),
                                found: "void".into(),
                            },
                            Some(keyword.line),
                            keyword.span,
                        ));
                    }

                    Ok(AnnotatedStatement::Return(None, keyword.span, keyword.line))
                }
            }
        } else if let SymbolKind::InternalDefinition = self.current_symbol_kind {
            let id = self.current_return_kind_id.unwrap();
            let expected_return = self.get_static_kind_by_id(id);

            match value {
                None => {
                    if let Ordering::Equal = expected_return.borrow().name.cmp(&"void".to_string())
                    {
                        Ok(AnnotatedStatement::Return(None, keyword.span, keyword.line))
                    } else {
                        Err(CompilationError::with_span(
                            CompilationErrorKind::UnexpectedReturnValue {
                                found: self.get_static_kind_by_id(id).borrow().name.clone(),
                            },
                            Some(keyword.line),
                            keyword.span,
                        ))
                    }
                }
                Some(v) => {
                    let return_kind = self.resolve_expr_type(v)?;

                    if return_kind.borrow().id != expected_return.borrow().id {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::ExpectReturnType {
                                expect: expected_return.borrow().name.clone(),
                                found: return_kind.borrow().name.clone(),
                            },
                            Some(v.line()),
                            v.span(),
                        ));
                    } else {
                        Ok(AnnotatedStatement::Return(
                            Some(self.analyze_expr(v)?),
                            keyword.span.merge(v.span()),
                            keyword.line,
                        ))
                    }
                }
            }
        } else {
            gpp_error!("return statement are only allowed inside functions or definitions.");
        }
    }

    /// Analyzes a type declaration and ensures it adheres to semantic rules.
    ///
    /// This function validates that the type is declared at the top level,
    /// ensures there are no duplicate type definitions, and processes archetypes
    /// and fields associated with the type.
    ///
    /// # Arguments
    ///
    /// * `name` - The token representing the type name.
    /// * `archetypes` - A list of archetypes associated with the type.
    /// * `fields` - A list of field declarations associated with the type.
    ///
    /// # Returns
    ///
    /// An `AnnotatedStatement::Type` containing the analyzed type declaration.
    fn analyze_type(
        &mut self,
        name: &Token,
        archetypes: &Vec<Token>,
        fields: &[FieldDeclaration],
    ) -> TyResult<AnnotatedStatement> {
        self.require_depth(
            Ordering::Less,
            name,
            1,
            format!(
                "Type declarations are only allowed in top level code. At line {}.",
                name.line
            ),
        )?;

        if self.symbol_table.names.contains_key(&name.lexeme) {
            return Err(CompilationError::with_span(
                CompilationErrorKind::DuplicatedTypeDefinition {
                    r#type: name.lexeme.clone(),
                },
                Some(name.line),
                name.span,
            ));
        }

        self.current_symbol_kind = SymbolKind::Kind;

        let mut decl = TypeDecl::new(name.lexeme.clone(), self.get_static_id());
        decl.add_archetype(Archetype::new("object".to_string()));
        decl.add_archetype(Archetype::new(name.lexeme.clone()));

        for archetype in archetypes {
            let kind = self
                .get_static_kind_by_name(
                    &archetype.lexeme,
                    &Expression::Literal(archetype.clone(), archetype.span),
                )?
                .borrow()
                .archetypes
                .clone();

            for kind_arch in kind {
                decl.add_archetype(kind_arch);
            }
        }

        let mut type_fields: HashMap<String, FieldDescriptor> = HashMap::new();

        for (index, field) in fields.iter().enumerate() {
            if let Expression::TypeComposition(mask, _) = field.kind.clone() {
                let kind = self.resolve_type_composition(&mask)?;
                let archetypes: Vec<Archetype> =
                    kind.borrow().archetypes.clone().into_iter().collect();

                for archetype in archetypes {
                    self.get_static_kind_by_name(&archetype.name, &field.kind.clone())?;
                }

                if type_fields.contains_key(&field.name.lexeme) {
                    return Err(CompilationError::with_span(
                        CompilationErrorKind::DuplicatedField {
                            field: field.name.lexeme.clone(),
                        },
                        Some(field.name.line),
                        field.kind.span().merge(field.name.span),
                    ));
                }

                type_fields.insert(
                    field.name.lexeme.clone(),
                    FieldDescriptor::new(field.name.lexeme.clone(), kind.clone(), index as u8),
                );
            }
        }

        let type_descriptor = Rc::new(RefCell::new(TypeDescriptor::from_type_decl_with_fields(
            decl,
            type_fields.clone(),
        )));

        self.define_type(type_descriptor.clone());

        let constructor = FunctionPrototype::new(
            name.lexeme.clone(),
            fields.to_owned(),
            type_fields.len(),
            self.get_user_defined_kind(name.lexeme.clone()),
        );

        self.define_function(name.lexeme.clone(), constructor);

        Ok(AnnotatedStatement::Type(
            type_descriptor,
            name.span,
            name.line,
        ))
    }

    /// Analyzes a decorator and its associated attributes.
    ///
    /// This function processes a decorator (preceded by a hash token) and ensures it is applied
    /// correctly to a function signature. It checks if the decorator is used in a valid context (only
    /// allowed in function signatures) and annotates the decorator with its attributes. If the decorator
    /// is not used correctly, an error is reported.
    ///
    /// # Parameters
    /// - `hash_token`: A reference to the `Token` representing the `#` symbol used in the decorator.
    ///   It is used to track the decorator's location for error reporting.
    /// - `attributes`: A reference to a vector of `Expression` representing the attributes passed
    ///   to the decorator. These are analyzed and annotated.
    ///
    /// # Returns
    /// This function returns an `AnnotatedStatement` that represents the analyzed decorator. The
    /// decorator is associated with the `hash_token` and its annotated attributes.
    ///
    /// # Errors
    /// If the decorator is used outside a function signature, an error is reported, indicating that
    /// decorators are only allowed in function definitions.
    fn analyze_decorator(
        &mut self,
        hash_token: &Token,
        attributes: &Vec<Expression>,
    ) -> TyResult<AnnotatedStatement> {
        let next = self.next();

        let mut annotated_attributes = Vec::new();
        let mut attribute_usages = Vec::new();

        for attribute in attributes {
            annotated_attributes.push(self.analyze_expr(attribute)?);

            if let Expression::Attribute(name, args, span) = attribute {
                attribute_usages.push(BuiltinAttributeUsage {
                    args: args.iter().map(Rc::clone).collect(),
                    name: name.lexeme.clone(),
                    span: *span,
                });
            } else {
                unreachable!();
            }
        }

        match next {
            Statement::Function(_, _, _, _, span, line) => {
                self.current_decorator = Decorator::new(attribute_usages);
                Ok(AnnotatedStatement::Decorator(
                    hash_token.clone(),
                    annotated_attributes,
                    span,
                    line,
                ))
            }

            Statement::NativeFunction(_, _, _, span, line) => {
                self.current_decorator = Decorator::new(attribute_usages);
                Ok(AnnotatedStatement::Decorator(
                    hash_token.clone(),
                    annotated_attributes,
                    span,
                    line,
                ))
            }

            Statement::InternalDefinition(_, _, _, _, span, line) => {
                self.current_decorator = Decorator::new(attribute_usages);
                Ok(AnnotatedStatement::Decorator(
                    hash_token.clone(),
                    annotated_attributes,
                    span,
                    line,
                ))
            }

            Statement::EndCode => Ok(AnnotatedStatement::EndCode),

            _ => Err(CompilationError::with_span(
                CompilationErrorKind::InvalidStatementUsage {
                    error: "Decorators are only accepted in function signatures.".to_string(),
                },
                Some(hash_token.line),
                hash_token.span,
            )),
        }
    }
}
