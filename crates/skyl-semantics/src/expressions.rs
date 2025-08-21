#![allow(clippy::result_large_err)]

use skyl_data::{
    AnnotatedExpression, Archetype, Expression, MethodDescriptor, Operator, OperatorKind, Span,
    Token, TokenKind,
};
use skyl_driver::{
    errors::{CompilationError, CompilationErrorKind},
    gpp_error,
};

use crate::{SemanticAnalyzer, result::TyResult};

impl SemanticAnalyzer {
    /// Analyzes an expression and generates an annotated expression.
    ///
    /// This function processes various types of expressions, including literals, unary operations,
    /// arithmetic expressions, logical expressions, assignments, and function calls. Depending on the
    /// expression type, it delegates the analysis to the appropriate helper function for more specific
    /// processing. Unsupported expression types are marked as TODO for future implementation.
    ///
    /// # Parameters
    /// - `expr`: A reference to the `Expression` that needs to be analyzed. The expression can be
    ///   of various types, such as a literal, unary operation, or function call.
    ///
    /// # Returns
    /// This function returns an `AnnotatedExpression` that represents the analyzed expression. The
    /// return type depends on the specific type of expression being processed.
    ///
    /// # Supported Expression Types
    /// - `Expression::Void`: Returns `AnnotatedExpression::Void`.
    /// - `Expression::Literal`: Processes a literal expression and returns the result of `analyze_literal`.
    /// - `Expression::Unary`: Analyzes a unary expression and returns the result of `analyze_unary_expr`.
    /// - `Expression::Arithmetic`: Analyzes an arithmetic expression and returns the result of `analyze_arithmetic_expr`.
    /// - `Expression::Logical`: Analyzes a logical expression and returns the result of `analyze_logical_expr`.
    /// - `Expression::Assign`: Analyzes an assignment expression and returns the result of `analyze_assignment_expr`.
    /// - `Expression::Call`: Analyzes a function call expression and returns the result of `analyze_call_expression`.
    /// - `Expression::List`: Analyzes a list expression and returns the result of `analyze_collection`.
    /// - `Expression::Group`: Recursively analyzes a grouped expression using `analyze_expr`.
    ///
    /// # Unsupported Expression Types
    /// - `Expression::Ternary`: Not yet implemented.
    /// - `Expression::Lambda`: Not yet implemented.
    /// - `Expression::Tuple`: Not yet implemented.
    /// - `Expression::TypeComposition`: Not yet implemented.
    /// - `Expression::Attribute`: Not yet implemented.
    /// - `Expression::Set`: Not yet implemented.
    ///
    /// # Errors
    /// This function does not return an error itself, but it delegates the analysis to other functions
    /// that may report errors depending on the expression type.
    pub(crate) fn analyze_expr(&mut self, expr: &Expression) -> TyResult<AnnotatedExpression> {
        match expr.clone() {
            Expression::Void => Ok(AnnotatedExpression::Void),
            Expression::Literal(token, _) => self.analyze_literal(token),
            Expression::PostFix(operator, variable, _) => {
                self.analyze_postfix_expr(&operator, &variable)
            }
            Expression::Unary(token, expression, _) => self.analyze_unary_expr(token, &expression),
            Expression::Arithmetic(left, op, right, _) => {
                self.analyze_arithmetic_expr(&left, &op, &right)
            }
            Expression::Logical(left, op, right, _) => {
                self.analyze_logical_expr(&left, &op, &right)
            }
            Expression::Ternary(_, _, _, _) => todo!(),
            Expression::Assign(token, expression, _) => {
                self.analyze_assignment_expr(token, &expression)
            }
            Expression::Lambda => todo!(),
            Expression::Get(expression, token, _) => self.analyze_get_expr(&expression, token),
            Expression::Variable(token, _) => self.analyze_variable_get_expr(token),
            Expression::Set(target, name, value, _) => self.analyze_set_expr(target, name, value),
            Expression::Call(callee, paren, args, _) => {
                self.analyze_call_expression(&callee, &paren, &args)
            }
            Expression::Tuple(_, _) => todo!(),
            Expression::List(_elements, span) => self.analyze_list(expr, span),
            Expression::TypeComposition(_, _) => todo!(),
            Expression::Attribute(token, expressions, _) => {
                self.analyze_attribute(token, expressions)
            }
            Expression::Group(expression, _) => self.analyze_expr(&expression),
            Expression::ListGet(expression, index, _) => {
                self.analyze_list_get_expr(expression, index)
            }
            Expression::ListSet(list, index, value, _) => {
                self.analyze_list_set_expr(list, index, value)
            }
            Expression::Closure(token, field_declarations, expression, body, span) => todo!(),
            Expression::ClosureSignature(token, expressions, expression, span) => todo!(),
        }
    }

    /// Analyzes a literal expression and resolves its type.
    ///
    /// This function creates an annotated expression for a literal, determining its type based on the token
    /// representing the literal.
    ///
    /// # Parameters
    /// - `token`: The token representing the literal value.
    ///
    /// # Returns
    /// - An `AnnotatedExpression` representing the literal, including its type.
    fn analyze_literal(&self, token: Token) -> TyResult<AnnotatedExpression> {
        Ok(AnnotatedExpression::Literal(
            token.clone(),
            self.resolve_literal_kind(&token)?,
        ))
    }

    fn analyze_postfix_expr(
        &mut self,
        operator: &Token,
        variable: &Expression,
    ) -> TyResult<AnnotatedExpression> {
        if let Expression::Variable(name, _) = variable {
            let kind = self.resolve_identifier_type(name)?;

            if !kind
                .borrow()
                .implements_archetype(&Archetype::new("int".to_string()))
            {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::InvalidPostfixOperatorUsage {
                        msg: "Only 'int' instances can use postfix operators.".into(),
                    },
                    Some(operator.line),
                    operator.span.merge(variable.span()),
                ));
            }

            Ok(AnnotatedExpression::PostFix(
                operator.clone(),
                Box::new(self.analyze_expr(variable)?),
            ))
        } else {
            Err(CompilationError::with_span(
                CompilationErrorKind::InvalidPostfixOperatorUsage {
                    msg: "Only variables can use postfix operators.".into(),
                },
                Some(operator.line),
                operator.span.merge(variable.span()),
            ))
        }
    }

    /// Analyzes a unary expression and returns the corresponding annotated expression.
    ///
    /// This function handles unary operators such as `-` (negation) and `!` (logical negation). It checks
    /// the type of the operand and applies the appropriate checks. If the operator is applied to an invalid
    /// operand type, an error is raised.
    ///
    /// # Parameters
    /// - `token`: The token representing the operator (`-` or `!`).
    /// - `expression`: The expression to which the unary operator is applied.
    ///
    /// # Returns
    /// - An `AnnotatedExpression` representing the analyzed unary expression.
    fn analyze_unary_expr(
        &mut self,
        token: Token,
        expression: &Expression,
    ) -> TyResult<AnnotatedExpression> {
        match token.kind {
            TokenKind::Operator(op) => match op {
                OperatorKind::Minus => {
                    let expr_type = self.resolve_expr_type(expression)?;

                    self.assert_archetype_kind(
                        expression,
                        self.get_static_kind_by_name("number", expression)?,
                        "'-' operator only be applyed in numbers.",
                    )?;

                    Ok(AnnotatedExpression::Unary(
                        token.clone(),
                        Box::new(self.analyze_expr(expression)?),
                        expr_type,
                    ))
                }

                OperatorKind::Not => {
                    let expr_type = self.resolve_expr_type(expression)?;

                    self.assert_archetype_kind(
                        expression,
                        self.get_static_kind_by_name("bool", expression)?,
                        "'not' operator only be applyed in booleans",
                    )?;

                    Ok(AnnotatedExpression::Unary(
                        token.clone(),
                        Box::new(self.analyze_expr(expression)?),
                        expr_type,
                    ))
                }
                _ => {
                    gpp_error!("Invalid unary operation at line {}.", token.line);
                }
            },

            _ => gpp_error!("Invalid unary operation at line {}.", token.line),
        }
    }

    /// Analyzes an arithmetic expression (binary operation) involving two operands.
    ///
    /// This function checks the types of the left and right operands, ensuring they are valid for the
    /// given arithmetic operator. It verifies that both operands are of compatible types (e.g., numbers)
    /// and raises an error if the types do not match the expected kind. The operator kind (e.g., plus, minus)
    /// is also validated for its compatibility with the operand types.
    ///
    /// # Parameters
    /// - `left`: The left operand of the arithmetic expression.
    /// - `token`: The token representing the operator.
    /// - `right`: The right operand of the arithmetic expression.
    ///
    /// # Returns
    /// - An `AnnotatedExpression` representing the result of the arithmetic operation, with the operator
    ///   and types validated.
    ///
    /// # Errors
    /// - Raises an error if the operands are incompatible with the operation, or if the operator is invalid
    ///   for the operands' types.
    fn analyze_arithmetic_expr(
        &mut self,
        left: &Expression,
        token: &Token,
        right: &Expression,
    ) -> TyResult<AnnotatedExpression> {
        let annotated_left;
        let annotated_right;

        if !matches!(left, Expression::Literal(_, _)) {
            annotated_left = self.analyze_expr(left)?;
        } else if let Expression::Literal(l, _) = left {
            annotated_left = self.analyze_literal(l.clone())?;
        } else {
            gpp_error!(
                "Invalid literal '{}' kind. At line {}.",
                token.lexeme,
                token.line
            );
        }

        if !matches!(right, Expression::Literal(_, _)) {
            annotated_right = self.analyze_expr(right)?;
        } else if let Expression::Literal(l, _) = right {
            annotated_right = self.analyze_literal(l.clone())?;
        } else {
            gpp_error!(
                "Invalid literal '{}' kind. At line {}.",
                token.lexeme,
                token.line
            );
        }

        let left_kind = self.resolve_expr_type(left)?;
        let right_kind = self.resolve_expr_type(right)?;

        if let TokenKind::Operator(op) = token.kind {
            match op {
                OperatorKind::Plus
                | OperatorKind::Minus
                | OperatorKind::Star
                | OperatorKind::Slash
                | OperatorKind::Greater
                | OperatorKind::GreaterEqual
                | OperatorKind::Less
                | OperatorKind::LessEqual => {
                    let _msg = format!(
                        "Cannot apply arithmetic operation '{}' to '{}' and '{}'. At line {}.",
                        token.lexeme,
                        left_kind.borrow().name,
                        right_kind.borrow().name,
                        token.line
                    );

                    if !left_kind.borrow().implements_archetype(&Archetype {
                        name: "number".into(),
                    }) || !right_kind.borrow().implements_archetype(&Archetype {
                        name: "number".into(),
                    }) {
                        if !self.symbol_table.operators.contains_key(&Operator {
                            this: left_kind.borrow().id,
                            other: right_kind.borrow().id,
                            kind: op.into(),
                        }) {
                            return Err(CompilationError::with_span(
                                CompilationErrorKind::OperatorOverloadNotFound {
                                    operator: token.lexeme.clone(),
                                    this: left_kind.borrow().name.clone(),
                                    other: right_kind.borrow().name.clone(),
                                },
                                Some(left.line()),
                                left.span().merge(right.span()),
                            ));
                        }

                        let method_name = self
                            .symbol_table
                            .operators
                            .get(&Operator {
                                this: left_kind.borrow().id,
                                other: right_kind.borrow().id,
                                kind: op.into(),
                            })
                            .unwrap();

                        return Ok(AnnotatedExpression::CallMethod(
                            Box::new(annotated_left.clone()),
                            left_kind.borrow().methods.get(method_name).unwrap().clone(),
                            vec![Box::new(annotated_left), Box::new(annotated_right)],
                        ));
                    }

                    Ok(AnnotatedExpression::Arithmetic(
                        Box::new(annotated_left),
                        token.clone(),
                        Box::new(annotated_right),
                        left_kind,
                    ))
                }

                OperatorKind::EqualEqual | OperatorKind::NotEqual => {
                    let expected_kind = self.resolve_expr_type(left)?;
                    self.assert_expression_kind(right, expected_kind, left)?;

                    Ok(AnnotatedExpression::Arithmetic(
                        Box::new(annotated_left),
                        token.clone(),
                        Box::new(annotated_right),
                        left_kind,
                    ))
                }

                _ => Err(CompilationError::with_span(
                    CompilationErrorKind::InvalidExpression {
                        msg: format!("Invalid arithmetic operator '{}'", token.lexeme),
                    },
                    Some(token.line),
                    token.span,
                )),
            }
        } else {
            Err(CompilationError::with_span(
                CompilationErrorKind::InvalidExpression {
                    msg: format!("Invalid arithmetic operator '{}'", token.lexeme),
                },
                Some(token.line),
                token.span,
            ))
        }
    }

    /// Analyzes a logical expression (e.g., `&&`, `||`) and ensures both operands are boolean.
    ///
    /// This function checks that both operands of the logical expression are of type `bool` and
    /// then annotates the expression accordingly.
    ///
    /// # Parameters
    /// - `left`: The left operand of the logical expression.
    /// - `op`: The operator (`&&` or `||`).
    /// - `right`: The right operand of the logical expression.
    ///
    /// # Returns
    /// - An `AnnotatedExpression` representing the logical expression, including the operator and operands.
    ///
    /// # Errors
    /// - Raises an error if either operand is not of type `bool`.
    fn analyze_logical_expr(
        &mut self,
        left: &Expression,
        op: &Token,
        right: &Expression,
    ) -> TyResult<AnnotatedExpression> {
        self.assert_expression_kind(left, self.get_static_kind_by_name("bool", left)?, left)?;
        self.assert_expression_kind(right, self.get_static_kind_by_name("bool", right)?, right)?;

        let left_kind = self.resolve_expr_type(left)?;

        Ok(AnnotatedExpression::Logical(
            Box::new(self.analyze_expr(left)?),
            op.clone(),
            Box::new(self.analyze_expr(right)?),
            left_kind,
        ))
    }

    /// Analyzes an assignment expression, ensuring the assigned value matches the variable's type.
    ///
    /// This function checks that the variable being assigned a value has been declared, and that the type
    /// of the value being assigned matches the type of the variable. If the types do not match, an error is raised.
    /// If the variable's type is not yet inferred, it infers the type of the variable based on the assigned value.
    ///
    /// # Parameters
    /// - `token`: The token representing the variable being assigned to.
    /// - `expression`: The expression on the right-hand side of the assignment.
    ///
    /// # Returns
    /// - An `AnnotatedExpression` representing the assignment expression, including the assigned value and its type.
    ///
    /// # Errors
    /// - Raises an error if the variable is not declared or if the types of the variable and the assigned value do not match.
    fn analyze_assignment_expr(
        &mut self,
        token: Token,
        expression: &Expression,
    ) -> TyResult<AnnotatedExpression> {
        let symbol = self.get_name_in_depth(&token)?;

        match symbol {
            Some(sv) => {
                let value = self.analyze_expr(expression)?;

                let value_type = self.resolve_expr_type(expression)?;
                let symbol_type = sv.kind;

                if let Some(kind) = &symbol_type
                    && kind.borrow().name == "void"
                {
                    gpp_error!("Cannot assign 'void' to variables. At line {}.", token.line);
                }

                if value_type.borrow().name == "void" {
                    gpp_error!("Cannot assign 'void' to variables. At line {}.", token.line);
                }

                match symbol_type {
                    Some(kind) => {
                        if kind.borrow().id != value_type.borrow().id {
                            return Err(CompilationError::with_span(
                                CompilationErrorKind::AssignTypeError {
                                    kind: kind.borrow().name.clone(),
                                    found: value_type.borrow().name.clone(),
                                },
                                Some(token.line),
                                token.span.merge(expression.span()),
                            ));
                        }

                        Ok(AnnotatedExpression::Assign(
                            token.clone(),
                            Box::new(value),
                            kind,
                        ))
                    }
                    None => {
                        self.context()
                            .set_infered_kind(&token.lexeme, value_type.clone());
                        Ok(AnnotatedExpression::Assign(
                            token.clone(),
                            Box::new(value),
                            value_type,
                        ))
                    }
                }
            }
            None => gpp_error!("The name '{}' are not declared here.", token.lexeme),
        }
    }

    /// Analyzes an expression involving field or member access (e.g., `obj.field`).
    ///
    /// This function first analyzes the base expression and then constructs an `AnnotatedExpression`
    /// for the field access. It resolves the type of the field access expression as well.
    ///
    /// # Parameters
    /// - `expression`: The expression that represents the base object.
    /// - `token`: The token representing the field being accessed.
    ///
    /// # Returns
    /// - An `AnnotatedExpression::Get` that represents the field access expression, including the
    ///   base expression, the field's token, and the resolved type of the field.
    ///
    /// # Example
    /// ```rust
    /// let expr = analyze_get_expr(&expression, token);
    /// ```
    fn analyze_get_expr(
        &mut self,
        expression: &Expression,
        token: Token,
    ) -> TyResult<AnnotatedExpression> {
        match expression {
            Expression::Get(_, _, _) => Ok(AnnotatedExpression::Get(
                Box::new(self.analyze_expr(expression)?),
                token.clone(),
                self.resolve_expr_type(expression)?,
            )),

            Expression::Variable(_, _) => Ok(AnnotatedExpression::Get(
                Box::new(self.analyze_expr(expression)?),
                token.clone(),
                self.resolve_expr_type(expression)?,
            )),

            Expression::Call(callee, _, _, _) => {
                let kind = self.resolve_expr_type(callee)?;

                if kind.borrow().fields.contains_key(&token.lexeme) {
                    Ok(AnnotatedExpression::Get(
                        Box::new(self.analyze_expr(expression)?),
                        token.clone(),
                        self.resolve_expr_type(expression)?,
                    ))
                } else {
                    gpp_error!(
                        "Type '{}' has no property named '{}'.",
                        kind.borrow().name,
                        token.lexeme
                    );
                }
            }

            _ => {
                panic!("Unsupported get expression: {expression:?}");
            }
        }
    }

    /// Analyzes a variable reference expression.
    ///
    /// This function resolves the type of a variable reference by looking up the variable in the context
    /// stack. It returns an annotated expression for the variable reference, including the variable's type.
    ///
    /// # Parameters
    /// - `token`: The token representing the variable being referenced.
    ///
    /// # Returns
    /// - An `AnnotatedExpression` representing the variable reference, including its type.
    ///
    /// # Errors
    /// - Raises an error if the variable is not declared or if its type is unknown.
    fn analyze_variable_get_expr(&mut self, token: Token) -> TyResult<AnnotatedExpression> {
        let kind = match self.get_name_in_depth(&token)? {
            Some(v) => match v.kind {
                Some(k) => k,
                None => {
                    return Err(CompilationError::with_span(
                        CompilationErrorKind::UsageOfNotInferredVariable {
                            name: token.lexeme.clone(),
                        },
                        Some(token.line),
                        token.span,
                    ));
                }
            },
            None => {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::UsageOfNotInferredVariable {
                        name: token.lexeme.clone(),
                    },
                    Some(token.line),
                    token.span,
                ));
            }
        };
        Ok(AnnotatedExpression::Variable(token, kind))
    }

    fn analyze_set_expr(
        &mut self,
        target: Box<Expression>,
        name: Token,
        value: Box<Expression>,
    ) -> TyResult<AnnotatedExpression> {
        let annotated_target = self.analyze_expr(&target)?;
        let annotated_value = self.analyze_expr(&value)?;
        let target_kind = self.resolve_expr_type(&target)?;
        let _value_kind = self.resolve_expr_type(&value)?;

        // self.assert_kind_equals(
        //     value_kind,
        //     target_kind,
        //     "Cannot assign instance field with different kind.".to_string()
        // );

        Ok(AnnotatedExpression::Set(
            Box::new(annotated_target),
            name,
            Box::new(annotated_value),
            target_kind,
        ))
    }

    /// Analyzes a function call expression.
    ///
    /// This function processes a function call expression, ensuring that the callee is a valid function
    /// and that the correct number of arguments is passed. It checks the arity of the function, verifies
    /// that the argument types match the function's parameter types, and returns an `AnnotatedExpression`
    /// representing the function call. If the callee is recursive or not declared, an error is raised.
    ///
    /// # Parameters
    /// - `callee`: The expression representing the function being called.
    /// - `paren`: The token representing the opening parenthesis of the function call.
    /// - `args`: A vector of expressions representing the arguments of the function call.
    ///
    /// # Returns
    /// - An `AnnotatedExpression` representing the function call with annotated arguments.
    ///
    /// # Errors
    /// - Raises an error if the function is recursive, not declared, or if the wrong number of arguments
    ///   is passed.
    pub(crate) fn analyze_call_expression(
        &mut self,
        callee: &Expression,
        paren: &Token,
        args: &Vec<Expression>,
    ) -> TyResult<AnnotatedExpression> {
        let mut annotated_args = Vec::new();

        for arg in args {
            annotated_args.push(Box::new(self.analyze_expr(arg)?));
        }

        if let Expression::Variable(name, _) = callee {
            if self.current_symbol.clone() == name.lexeme.clone() {
                gpp_error!(
                    "Recursive calls are not allowed in current version. At line {}.",
                    name.line
                );
            }
            match self.get_function(&name.lexeme.clone()) {
                Some(prototype) => {
                    let prototype = prototype.clone();

                    let function_visibility = prototype.visibility;
                    let file = prototype.file;

                    if !function_visibility.can_access(self.current_file, file, None, None) {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::VisibilityError {
                                location: self
                                    .ctx
                                    .clone()
                                    .unwrap()
                                    .borrow_mut()
                                    .get_file(&file)
                                    .path
                                    .display()
                                    .to_string(),
                                symbol: prototype.name.clone(),
                                visibilty: prototype.visibility.to_string(),
                                requested: "pub".into(),
                            },
                            Some(name.line),
                            name.span,
                        ));
                    }

                    if prototype.arity != args.len() {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::MismatchArgumentCount {
                                expected: prototype.arity,
                                found: args.len(),
                                function_name: prototype.name,
                            },
                            Some(name.line),
                            name.span,
                        ));
                    }

                    self.assert_function_args(prototype.clone(), args, paren)?;
                    Ok(AnnotatedExpression::Call(
                        prototype.clone(),
                        paren.clone(),
                        annotated_args,
                        prototype.return_kind.clone(),
                    ))
                }
                None => match self.get_native_function(&name.lexeme.clone()) {
                    Some(prototype) => {
                        let prototype = prototype.clone();

                        if prototype.arity != args.len() {
                            gpp_error!(
                                "Expect {} arguments, but got {}. At line {}.",
                                prototype.arity,
                                args.len(),
                                paren.line
                            );
                        }

                        self.assert_function_args(prototype.clone(), args, paren)?;
                        Ok(AnnotatedExpression::CallNative(
                            prototype.clone(),
                            paren.clone(),
                            annotated_args,
                            prototype.return_kind.clone(),
                        ))
                    }

                    None => Err(CompilationError::with_span(
                        CompilationErrorKind::SymbolNotFound {
                            symbol_kind: "function".to_string(),
                            symbol_name: name.lexeme.clone(),
                        },
                        Some(name.line),
                        name.span,
                    )),
                },
            }
        } else {
            match callee {
                Expression::Get(callee, token, _) => {
                    let method = self.analyze_method_get(callee, token.clone())?;
                    let mut annotated_args: Vec<Box<AnnotatedExpression>> = Vec::new();

                    if args.len() != method.params.len() - 1 {
                        println!(
                            "Expect {} arguments, but got {}. At line {}.",
                            method.arity,
                            args.len(),
                            paren.line
                        );
                    }

                    annotated_args.push(Box::new(self.analyze_expr(callee)?));

                    if method.arity > 1 {
                        for (i, _) in args.iter().enumerate() {
                            let param_kind = &method.params[i + 1];
                            let _arg_kind = self.resolve_expr_type(&args[i])?;

                            self.assert_archetype_kind(
                                &args[i],
                                param_kind.kind.clone(),
                                format!(
                                    "Expect '{}' to '{}' param, but got '{}'. At line {}.",
                                    param_kind.name,
                                    method.params[i + 1].name,
                                    param_kind.name,
                                    paren.line,
                                )
                                .as_str(),
                            )?;

                            let annotated_arg = self.analyze_expr(&args[i])?;
                            annotated_args.push(Box::new(annotated_arg));
                        }
                    }

                    return Ok(AnnotatedExpression::CallMethod(
                        Box::new(self.analyze_expr(callee)?),
                        method,
                        annotated_args,
                    ));
                }

                Expression::Literal(token, _) => {
                    let literal_kind = self.resolve_literal_kind(token)?;
                    println!("{literal_kind:?}");
                }

                _ => {
                    gpp_error!("Call expression used in {:?} value.", dbg!(callee));
                }
            }

            gpp_error!(
                "Call functions inside modules are currently not allowed {}.",
                dbg!(callee)
            );
        }
    }

    fn analyze_attribute(
        &mut self,
        token: Token,
        expressions: Vec<Expression>,
    ) -> TyResult<AnnotatedExpression> {
        let attrib = &self.symbol_table.get_attribute(token.lexeme.clone());

        match attrib {
            Some(att) => {
                if att.args.len() != expressions.len() {
                    return Err(CompilationError::with_span(
                        CompilationErrorKind::InvalidAttributeExpression {
                            msg: format!(
                                "Expect {} arguments, but got {}",
                                att.args.len(),
                                expressions.len()
                            ),
                        },
                        Some(token.line),
                        token.span,
                    ));
                }

                for (index, kind) in att.args.clone().iter().enumerate() {
                    let expr = expressions[index].clone();
                    let expr_kind = self.resolve_expr_type(&expr)?;

                    if kind.borrow().id != expr_kind.borrow().id {
                        return Err(CompilationError::with_span(
                            CompilationErrorKind::ExpectType {
                                expect: kind.borrow().name.clone(),
                                found: expr_kind.borrow().name.clone(),
                                compiler_msg: None,
                            },
                            Some(expr.line()),
                            expr.span(),
                        ));
                    }
                }
            }
            None => {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::InvalidAttributeExpression {
                        msg: format!("Attribute '{}' not found", token.lexeme),
                    },
                    Some(token.line),
                    token.span,
                ));
            }
        }

        Ok(AnnotatedExpression::Void)
    }

    fn analyze_list_get_expr(
        &mut self,
        expression: Box<Expression>,
        index: Box<Expression>,
    ) -> TyResult<AnnotatedExpression> {
        let annotated_expression = self.analyze_expr(&expression)?;
        let annotated_index = self.analyze_expr(&index)?;

        Ok(AnnotatedExpression::ListGet(
            Box::new(annotated_expression),
            Box::new(annotated_index),
        ))
    }

    fn analyze_list_set_expr(
        &mut self,
        list: Box<Expression>,
        index: Box<Expression>,
        value: Box<Expression>,
    ) -> TyResult<AnnotatedExpression> {
        let annotated_list = self.analyze_expr(&list)?;
        let annotated_index = self.analyze_expr(&index)?;
        let annotated_value = self.analyze_expr(&value)?;
        let kind = self.resolve_expr_type(&list)?;

        Ok(AnnotatedExpression::ListSet(
            Box::new(annotated_list),
            Box::new(annotated_index),
            Box::new(annotated_value),
            kind,
        ))
    }

    pub(crate) fn analyze_method_get(
        &mut self,
        callee: &Expression,
        token: Token,
    ) -> TyResult<MethodDescriptor> {
        let callee_kind = self.resolve_expr_type(callee)?;

        if callee_kind.borrow().methods.contains_key(&token.lexeme) {
            let prototype = callee_kind.borrow().methods[&token.lexeme].clone();

            let function_visibility = prototype.visibility;
            let file = prototype.file;

            if !function_visibility.can_access(
                self.current_file,
                file,
                Some(callee_kind.borrow().id),
                Some(prototype.owner_type_id),
            ) {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::VisibilityError {
                        location: self
                            .ctx
                            .clone()
                            .unwrap()
                            .borrow_mut()
                            .get_file(&file)
                            .path
                            .display()
                            .to_string(),
                        symbol: prototype.name.clone(),
                        visibilty: prototype.visibility.to_string(),
                        requested: "pub".into(),
                    },
                    Some(token.line),
                    token.span,
                ));
            }

            return Ok(callee_kind.borrow().methods[&token.lexeme].clone());
        }

        Err(CompilationError::with_span(
            CompilationErrorKind::SymbolNotFound {
                symbol_kind: "method".into(),
                symbol_name: format!("{}.{}", callee_kind.borrow().name, token.lexeme),
            },
            Some(token.line),
            token.span,
        ))
    }

    pub(crate) fn analyze_list(
        &self,
        _expr: &Expression,
        _span: Span,
    ) -> Result<AnnotatedExpression, CompilationError> {
        todo!()
    }
}
