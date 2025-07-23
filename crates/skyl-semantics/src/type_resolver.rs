use std::{cell::RefCell, collections::HashSet, rc::Rc};

use skyl_data::{
    Archetype, Expression, Literal, Operator, OperatorKind, Token, TokenKind, TypeDescriptor,
};
use skyl_driver::{
    errors::{CompilationError, CompilationErrorKind},
    gpp_error,
};

use crate::{SemanticAnalyzer, result::TyResult};

impl SemanticAnalyzer {
    /// Resolves and returns the type descriptor for the given expression.
    ///
    /// This function inspects the expression and resolves its type, returning a `TypeDescriptor` that
    /// corresponds to the type of the expression. It handles various expression types, including literals,
    /// unary operations, and arithmetic operations. If the expression's type cannot be resolved, an error is raised.
    ///
    /// # Parameters
    /// - `expression`: A reference to the expression whose type is being resolved.
    ///
    /// # Returns
    /// - A `TypeDescriptor` representing the type of the given expression.
    ///
    /// # Errors
    /// - If the expression type cannot be determined or is unsupported, an error is raised.
    pub(super) fn resolve_expr_type(
        &mut self,
        expression: &Expression,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        match expression {
            Expression::List(elements, span) => self.get_static_kind_by_name("list", expression),
            Expression::Literal(token, span) => match token.kind {
                TokenKind::Identifier => self.resolve_identifier_type(token),
                TokenKind::Literal(literal) => match literal {
                    Literal::String => Ok(self.get_symbol("str").unwrap().kind.clone()),
                    Literal::Float => Ok(self.get_symbol("float").unwrap().kind.clone()),
                    Literal::Int => Ok(self.get_symbol("int").unwrap().kind.clone()),
                    Literal::Boolean => Ok(self.get_symbol("bool").unwrap().kind.clone()),
                },
                _ => gpp_error!("Expect literal in line {}.", token.line),
            },
            Expression::Unary(_, expression, span) => self.resolve_expr_type(&expression),
            Expression::Arithmetic(left, op, right, span) => {
                if let TokenKind::Operator(operator) = op.kind {
                    match operator {
                        OperatorKind::Plus
                        | OperatorKind::Minus
                        | OperatorKind::Star
                        | OperatorKind::Slash => {
                            let left_type = self.resolve_expr_type(&left)?;
                            let right_type = self.resolve_expr_type(right)?;

                            let number_arch = Archetype {
                                name: "number".into(),
                            };

                            if left_type.borrow().implements_archetype(&number_arch)
                                && right_type.borrow().implements_archetype(&number_arch)
                            {
                                return Ok(left_type);
                            }

                            let operator = &Operator {
                                kind: operator.into(),
                                this: left_type.borrow().id,
                                other: right_type.borrow().id,
                            };

                            if let Some(op) = self.symbol_table.operators.get(&operator) {
                                let left_type_borrow = left_type.borrow();
                                let operator_function = left_type_borrow.methods.get(op).unwrap();

                                return Ok(
                                    self.get_static_kind_by_id(operator_function.return_kind_id)
                                );
                            } else {
                                return Err(CompilationError::with_span(
                                    CompilationErrorKind::OperatorOverloadNotFound {
                                        this: left_type.borrow().name.clone(),
                                        other: right_type.borrow().name.clone(),
                                        operator: self
                                            .operator_kind_to_name(&operator.kind)
                                            .to_string(),
                                    },
                                    Some(expression.line()),
                                    expression.span(),
                                ));
                            }
                        }

                        OperatorKind::Greater
                        | OperatorKind::GreaterEqual
                        | OperatorKind::Less
                        | OperatorKind::NotEqual
                        | OperatorKind::EqualEqual => {
                            return self.get_static_kind_by_name("bool", expression);
                        }

                        _ => gpp_error!("Invalid arithmetic operator."),
                    }
                }

                gpp_error!("Invalid arithmetic operator.");
            }
            Expression::Logical(left, _, _, span) => {
                let left_type = self.resolve_expr_type(&left)?;

                if left_type != self.get_symbol("bool").unwrap().kind {
                    gpp_error!("Expected boolean type for logical expression.");
                }
                Ok(left_type)
            }
            Expression::Ternary(cond, true_expr, false_expr, span) => {
                let cond_type = self.resolve_expr_type(&cond)?;
                let true_type = self.resolve_expr_type(&true_expr)?;
                let false_type = self.resolve_expr_type(&false_expr)?;

                if true_type != false_type {
                    gpp_error!("Types of both branches of the ternary expression must match.");
                }
                Ok(true_type)
            }
            Expression::Variable(name, span) => self.resolve_identifier_type(name),
            Expression::Assign(_, expr, span) => self.resolve_expr_type(expr),
            Expression::Lambda => {
                gpp_error!("Lambda expressions are currently not supported.")
            }
            Expression::TypeComposition(mask, span) => self.resolve_type_composition(mask),
            Expression::Call(callee, paren, args, span) => {
                self.resolve_function_return_type(callee, paren, args)
            }
            Expression::Get(object, token, span) => self.resolve_get_expr(object, token),
            Expression::Group(expression, span) => self.resolve_expr_type(&expression),
            Expression::Void => Ok(self.get_void_instance()),
            Expression::ListGet(list, index, span) => self.resolve_list_get_type(list, index),
            _ => gpp_error!("Expression {expression:?} are not supported."),
        }
    }

    /// Resolves the type of an expression with field access (e.g., `obj.field`).
    ///
    /// This function resolves the type of an expression with one or more field accesses, ensuring that
    /// each field in the path exists and is valid for the type.
    ///
    /// # Parameters
    /// - `expression`: The expression representing the object whose fields are being accessed.
    /// - `token`: The token representing the field being accessed.
    ///
    /// # Returns
    /// - A `TypeDescriptor` representing the type of the accessed field.
    ///
    /// # Errors
    /// - Raises an error if any field in the expression path does not exist or if the type is invalid.
    pub(super) fn resolve_get_expr(
        &mut self,
        expression: &Expression,
        token: &Token,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        let mut current_kind: Option<Rc<RefCell<TypeDescriptor>>> = None;
        let mut current_expression = expression;
        let mut is_literal = false;

        let mut path = vec![token.clone()];

        while let Expression::Get(expr, name, span) = current_expression {
            path.push(name.clone());
            current_expression = expr;
        }

        if let Expression::Variable(name, span) = current_expression {
            path.push(name.clone());
        } else {
            current_kind = Some(Rc::clone(&self.resolve_expr_type(&current_expression)?));
            is_literal = true;
        }

        let path: Vec<&Token> = path.iter().rev().collect();

        if is_literal {
            for (index, field) in path[0..].iter().enumerate() {
                current_kind = match current_kind.clone() {
                    None => {
                        gpp_error!(
                            "{} cannot have '{}' field.",
                            path[index - 1],
                            field.lexeme.clone()
                        );
                    }

                    Some(type_descriptor) => {
                        match type_descriptor.borrow().fields.get(&field.lexeme) {
                            None => match type_descriptor.borrow().methods.get(&field.lexeme) {
                                Some(method_decl) => {
                                    return Ok(self
                                        .get_static_kind_by_id(method_decl.return_kind_id)
                                        .clone());
                                }
                                None => {
                                    gpp_error!(
                                        "Variable '{}' is a '{}' instance and not have '{}' field.",
                                        path[index].lexeme.clone(),
                                        current_kind.unwrap().borrow().name,
                                        field.lexeme.clone()
                                    );
                                }
                            },
                            Some(field_decl) => Some(field_decl.kind.clone()),
                        }
                    }
                };
            }
        } else {
            current_kind = match self.get_name_in_depth(&path[0])? {
                None => {
                    gpp_error!("The kind of {} is not known here.", &path[0].lexeme);
                }
                Some(semantic_value) => semantic_value.kind,
            };

            for (index, field) in path[1..].iter().enumerate() {
                current_kind = match &current_kind {
                    None => {
                        gpp_error!(
                            "{} cannot have '{}' field.",
                            path[index - 1],
                            field.lexeme.clone()
                        );
                    }

                    Some(type_descriptor) => {
                        match type_descriptor.borrow().fields.get(&field.lexeme) {
                            Some(field_decl) => Some(field_decl.kind.clone()),
                            None => match type_descriptor.borrow().methods.get(&field.lexeme) {
                                Some(method_decl) => Some(
                                    self.get_static_kind_by_id(method_decl.return_kind_id)
                                        .clone(),
                                ),
                                None => {
                                    gpp_error!(
                                        "Variable '{}' is a '{}' instance and not have '{}' field.",
                                        path[index].lexeme.clone(),
                                        current_kind.clone().unwrap().borrow().name,
                                        field.lexeme.clone()
                                    );
                                }
                            },
                        }
                    }
                };
            }
        }

        match &current_kind {
            None => gpp_error!("Not have field with name."),
            Some(kind) => Ok(self.get_static_kind_by_name(&kind.borrow().name, expression)?),
        }
    }

    fn resolve_list_get_type(
        &mut self,
        list: &Expression,
        index: &Expression,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        match list {
            Expression::List(elements, span) => self.resolve_list_type(elements),
            Expression::Variable(name, span) => self.resolve_identifier_type(name),
            _ => gpp_error!("Cannot resolve list type for {}.", list),
        }
    }

    /// Infers the type of a list based on its elements.
    ///
    /// # Parameters
    /// - `elements`: A slice of Rced expressions representing the elements of the list.
    ///
    /// # Returns
    /// - A `TypeDecl` representing the inferred type of the list.
    ///
    /// # Type Inference Process
    /// 1. If the list is empty, an error is raised because type inference is impossible.
    /// 2. If the list contains only one element, the type of that element is used as the list type.
    /// 3. Otherwise, the function:
    ///    - Resolves the type of each element.
    ///    - Collects all unique archetypes found across the elements.
    ///    - Identifies archetypes that are common to all elements.
    ///    - Determines the final list type based on these common archetypes.
    ///
    /// The inferred type is printed for debugging purposes before being returned.
    pub(super) fn resolve_list_type(
        &mut self,
        elements: &[Rc<Expression>],
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        if elements.is_empty() {
            gpp_error!("Cannot infer type of empty list. At least one element is required.");
        }

        let first_type = self.resolve_expr_type(&elements[0])?;

        if elements.len() == 1 {
            return Ok(first_type);
        }

        let mut common_archetypes: HashSet<Archetype> = first_type.borrow().archetypes.clone();

        for element in &elements[1..] {
            let element_type = self.resolve_expr_type(&element)?;
            common_archetypes.retain(|arch| element_type.borrow().archetypes.contains(arch));
        }

        if common_archetypes.is_empty() {
            gpp_error!("Cannot infer list kind. No common archetypes found.");
        }

        let archetypes_vec: Vec<Archetype> = common_archetypes.into_iter().collect();

        let infered_type = self.get_by_archetype(&archetypes_vec);

        match infered_type {
            Some(kind) => {
                println!("Infered list kind: {}.", kind.borrow().name);
                Ok(kind)
            }
            None => gpp_error!("Cannot find type with specified archetypes: {archetypes_vec:?}."),
        }
    }

    /// Resolves the type of an expression based on a path of tokens.
    ///
    /// This function resolves a type by following a sequence of tokens, ensuring that modules are
    /// not used, as they are currently unsupported. The path should contain a single token representing
    /// the type's name.
    ///
    /// # Parameters
    /// - `path`: A vector of tokens representing the path to the type.
    ///
    /// # Returns
    /// - A `TypeDescriptor` representing the resolved type.
    ///
    /// # Errors
    /// - Raises an error if the path has more than one token, indicating unsupported module usage.
    pub(super) fn resolve_type(&self, path: Vec<Token>) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        if path.len() != 1 {
            gpp_error!(
                "Modules are currently not supported. At line {}.",
                path[0].line
            );
        } else {
            self.get_static_kind_by_name(
                &path.first().unwrap().lexeme,
                &Expression::Literal(path.first().unwrap().clone(), path.first().unwrap().span),
            )
        }
    }

    /// Resolves the type of an iterator expression (e.g., for lists or function calls).
    ///
    /// This function determines the type of an iterator expression. It handles different types of
    /// iterator expressions, such as lists and function calls, and ensures that the correct type
    /// is inferred based on the expression's context.
    ///
    /// # Parameters
    /// - `iterator`: The iterator expression whose type is to be resolved.
    ///
    /// # Returns
    /// - A `TypeDescriptor` representing the type of the iterator expression.
    ///
    /// # Errors
    /// - Raises an error if the iterator expression is not a list or a function call.
    pub(crate) fn resolve_iterator_kind(
        &mut self,
        iterator: &Expression,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        let expr_kind = self.resolve_expr_type(iterator);

        match iterator {
            Expression::List(elements, span) => self.resolve_list_type(&elements),
            Expression::Call(callee, paren, args, span) => {
                self.analyze_call_expression(callee, paren, args);
                self.resolve_function_return_type(callee, paren, args)
            }
            _ => {
                gpp_error!("Expect list, but got {:?}.", iterator);
            }
        }
    }

    /// Resolves a type composition from a mask of tokens.
    ///
    /// This function builds a set of archetypes from the given tokens and attempts to find a matching
    /// `TypeDescriptor` that satisfies all the archetypes.
    ///
    /// # Parameters
    /// - `mask`: A vector of `Token`s representing the names of types or modules.
    ///
    /// # Returns
    /// - A `TypeDescriptor` representing the resolved type based on the mask.
    ///
    /// # Errors
    /// - Raises an error if no matching type is found for the given archetypes.
    pub(super) fn resolve_type_composition(
        &mut self,
        mask: &Vec<Token>,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        let mut archetypes = HashSet::<Archetype>::new();

        if mask[0].lexeme == "void" {
            return Ok(self.get_void_instance());
        }

        archetypes.insert(Archetype::new("object".to_string()));

        for name in mask {
            let matched: Vec<Archetype> = self
                .get_static_kind_by_name(
                    &name.lexeme,
                    &Expression::Literal(name.clone(), name.span),
                )?
                .borrow()
                .archetypes
                .clone()
                .into_iter()
                .collect();

            for archetype in matched {
                archetypes.insert(archetype.clone());
            }
        }

        let archetypes: Vec<Archetype> = archetypes.into_iter().collect();

        match self.get_by_archetype(&archetypes) {
            None => gpp_error!("Cannot find type to match with specified archetype."),
            Some(kind) => Ok(kind),
        }
    }

    /// Resolves the type of a literal value.
    ///
    /// This function takes a literal token and determines its type (e.g., `int`, `float`, `bool`, `str`).
    /// It retrieves the appropriate `TypeDescriptor` for the literal's type from the symbol table.
    ///
    /// # Parameters
    /// - `literal`: The token representing the literal value.
    ///
    /// # Returns
    /// - The `TypeDescriptor` corresponding to the literal's type.
    ///
    /// # Errors
    /// - Raises an error if the token does not represent a valid literal.
    pub(crate) fn resolve_literal_kind(
        &self,
        literal: &Token,
    ) -> TyResult<Rc<RefCell<TypeDescriptor>>> {
        if let TokenKind::Literal(l) = literal.kind {
            match l {
                Literal::Boolean => Ok(self.symbol_table.get("bool").unwrap().kind.clone()),
                Literal::Float => Ok(self.symbol_table.get("float").unwrap().kind.clone()),
                Literal::Int => Ok(self.symbol_table.get("int").unwrap().kind.clone()),
                Literal::String => Ok(self.symbol_table.get("str").unwrap().kind.clone()),
            }
        } else {
            return Err(CompilationError::with_span(
                CompilationErrorKind::InvalidLiteral { line: literal.line },
                Some(literal.line),
                literal.span,
            ));
        }
    }
}
