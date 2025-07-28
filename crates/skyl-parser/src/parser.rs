use std::{cell::RefCell, rc::Rc};

use skyl_data::{
    Ast, CollectionKind, Expression, FieldDeclaration, KeywordKind, Literal, OperatorKind,
    PunctuationKind, Statement, Token, TokenKind, TokenStream,
};
use skyl_driver::errors::{
    CompilationError, CompilationErrorKind, CompilerErrorReporter, ParseError,
};

pub struct Parser {
    pub(crate) stream: TokenStream,
    pub(crate) reporter: Rc<RefCell<CompilerErrorReporter>>,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            stream: Default::default(),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::empty())),
        }
    }

    pub fn report_error(&mut self, error: CompilationError) {
        self.reporter.borrow_mut().report_error(error);
    }

    pub fn parse(
        &mut self,
        reporter: Rc<RefCell<CompilerErrorReporter>>,
        tokens: TokenStream,
    ) -> Ast {
        self.reset_internal_state(tokens);

        self.reporter = reporter.clone();

        let mut statements: Vec<Statement> = Vec::new();

        while !self.is_at_end() {
            let stmt = self.declaration();
            match stmt {
                Ok(s) => statements.push(s),
                Err(e) => {
                    self.report_error(CompilationError::with_span(e.kind, Some(e.line), e.span));
                    self.synchronize();
                }
            }
        }

        statements.push(Statement::EndCode);

        Ast::new(statements)
    }

    fn declaration(&mut self) -> Result<Statement, ParseError> {
        match self.advance().kind {
            TokenKind::Keyword(keyword) => match keyword {
                KeywordKind::Return => self.return_statement(),
                KeywordKind::Type => self.type_declaration(),
                KeywordKind::Def => self.function_declaration(),
                KeywordKind::Native => self.parse_native_decl(),
                KeywordKind::Builtin => self.parse_builtin_decl(),
                KeywordKind::Internal => self.parse_internal_function(),
                _ => {
                    self.backtrack();
                    self.statement()
                }
            },
            TokenKind::Punctuation(punctuation) => match punctuation {
                PunctuationKind::Hash => self.decorator_declaration(),
                _ => {
                    self.backtrack();
                    self.statement()
                }
            },
            _ => {
                self.backtrack();
                self.statement()
            }
        }
    }

    fn parse_internal_function(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        self.eat(
            TokenKind::Keyword(KeywordKind::Def),
            CompilationErrorKind::ExpectedToken {
                expect: "def".into(),
                found: self.peek().lexeme.clone(),
                after: None,
            },
        )?;

        let function_name = self.eat(
            TokenKind::Identifier,
            CompilationErrorKind::ExpectedToken {
                expect: "identifier".into(),
                found: self.peek().lexeme.clone(),
                after: None,
            },
        )?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftParen),
            CompilationErrorKind::ExpectedToken {
                expect: "(".into(),
                found: self.peek().lexeme.clone(),
                after: None,
            },
        )?;

        let mut params: Vec<FieldDeclaration> = Vec::new();

        let param_name = self.eat(
            TokenKind::Identifier,
            CompilationErrorKind::ExpectedConstruction {
                expect: "'self' param".into(),
                found: self.peek().lexeme,
            },
        )?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::Colon),
            CompilationErrorKind::ExpectedToken {
                expect: "':'".into(),
                found: self.peek().lexeme.clone(),
                after: None,
            },
        )?;

        let param_type = self.type_composition()?;

        params.push(FieldDeclaration::new(param_name, param_type));

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                let param_name = self.eat(
                    TokenKind::Identifier,
                    CompilationErrorKind::ExpectedConstruction {
                        expect: "param name".into(),
                        found: self.peek().lexeme,
                    },
                )?;

                self.eat(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    CompilationErrorKind::ExpectedToken {
                        expect: "':'".into(),
                        found: self.peek().lexeme.clone(),
                        after: None,
                    },
                )?;

                let param_type = self.type_composition()?;

                params.push(FieldDeclaration::new(param_name, param_type));
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            CompilationErrorKind::ExpectedToken {
                expect: ")".into(),
                found: self.peek().lexeme.clone(),
                after: None,
            },
        )?;

        

        self.eat(
            TokenKind::Operator(OperatorKind::Arrow),
            CompilationErrorKind::ExpectedConstruction {
                expect: "function return kind".into(),
                found: self.peek().lexeme,
            },
        )?;

        let return_kind: Expression = self.type_composition()?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            CompilationErrorKind::ExpectedConstruction {
                expect: "'{' before function body".into(),
                found: self.peek().lexeme,
            },
        )?;

        let body = self.parse_scope()?;
        let end_span = body.span();

        Ok(Statement::InternalDefinition(
            function_name,
            params,
            Rc::new(body),
            return_kind,
            start_token.span.merge(end_span),
            start_token.line,
        ))
    }

    fn parse_native_decl(&mut self) -> Result<Statement, ParseError> {
        if self.try_eat(&[TokenKind::Keyword(KeywordKind::Def)]) {
            return self.native_function();
        } else if self.try_eat(&[TokenKind::Keyword(KeywordKind::Type)]) {
            return self.native_type();
        }

        Err(ParseError::new(
            CompilationErrorKind::InvalidNativeDeclaration,
            self.peek().line,
            self.peek().span,
        ))
    }

    fn parse_builtin_decl(&mut self) -> Result<Statement, ParseError> {
        if self.try_eat(&[TokenKind::Keyword(KeywordKind::Attribute)]) {
            return self.builtin_attribute();
        }

        Err(ParseError::new(
            CompilationErrorKind::InvalidBuiltinDeclaration,
            self.peek().line,
            self.peek().span,
        ))
    }

    fn builtin_attribute(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        let name = self.eat(
            TokenKind::Identifier,
            CompilationErrorKind::ExpectedToken {
                expect: "identifier".into(),
                found: self.peek().lexeme,
                after: Some("'attribute' keyword".into()),
            },
        )?;

        let mut kinds: Vec<Token> = Vec::new();

        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftParen)]) {
            if self.try_eat(&[TokenKind::Identifier]) {
                kinds.push(self.previous());

                while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                    let kind = self.eat(
                        TokenKind::Identifier,
                        CompilationErrorKind::ExpectedToken {
                            expect: "identifier".into(),
                            found: self.peek().lexeme,
                            after: Some("'.'".into()),
                        },
                    )?;

                    kinds.push(kind);
                }
            }

            self.eat(
                TokenKind::Punctuation(PunctuationKind::RightParen),
                CompilationErrorKind::ExpectedToken {
                    expect: ")".into(),
                    found: self.peek().lexeme,
                    after: Some("attribute args".into()),
                },
            )?;
        }

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::SemiColon),
                "';'",
                "attribute declaration",
            )?
            .span;

        Ok(Statement::BuiltinAttribute(
            name,
            kinds,
            end_span.merge(start_token.span),
            start_token.line,
        ))
    }

    fn native_type(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        let type_name = self.expect_token(TokenKind::Identifier, "type name", "'type' keyword")?;

        let mut archetypes: Vec<Token> = Vec::new();

        if self.try_eat(&[TokenKind::Keyword(KeywordKind::With)]) {
            archetypes.push(self.expect_token(
                TokenKind::Identifier,
                "archetype name",
                "'with' keyword",
            )?);

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                archetypes.push(self.expect_token(
                    TokenKind::Identifier,
                    "archetype name",
                    "','",
                )?);
            }
        }

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            "'{'",
            "type name",
        )?;

        let mut fields: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
            let mut field_name = self.eat(
                TokenKind::Identifier,
                CompilationErrorKind::ExpectedToken {
                    expect: "field name".into(),
                    found: self.peek().lexeme,
                    after: None,
                },
            )?;

            self.expect_token(
                TokenKind::Punctuation(PunctuationKind::Colon),
                "':'",
                "field name",
            )?;

            let mut field_type: Expression = self.type_composition()?;
            fields.push(FieldDeclaration::new(field_name, field_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                if self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
                    break;
                }

                field_name = self.eat(
                    TokenKind::Identifier,
                    CompilationErrorKind::ExpectedToken {
                        expect: "field name".into(),
                        found: self.peek().lexeme,
                        after: None,
                    },
                )?;

                self.expect_token(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    "':'",
                    "field name",
                )?;

                field_type = self.type_composition()?;
                fields.push(FieldDeclaration::new(field_name, field_type));
            }
        }

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::RightBrace),
                "'}'",
                "type fields",
            )?
            .span;

        Ok(Statement::Type(
            type_name,
            archetypes,
            fields,
            end_span.merge(start_token.span),
            start_token.line,
        ))
    }

    fn native_function(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        let function_name = self.expect_token(TokenKind::Identifier, "function name", "'def'")?;

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftParen),
            "'('",
            "function name",
        )?;

        let mut params: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            let param_name = self.eat(
                TokenKind::Identifier,
                CompilationErrorKind::ExpectedToken {
                    expect: "param name".into(),
                    found: self.peek().lexeme,
                    after: None,
                },
            )?;

            self.expect_token(
                TokenKind::Punctuation(PunctuationKind::Colon),
                "':'",
                "param name",
            )?;

            let param_type = self.type_composition()?;

            params.push(FieldDeclaration::new(param_name, param_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                let param_name = self.eat(
                    TokenKind::Identifier,
                    CompilationErrorKind::ExpectedToken {
                        expect: "param name".into(),
                        found: self.peek().lexeme,
                        after: None,
                    },
                )?;

                self.expect_token(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    "':'",
                    "param name",
                )?;

                let param_type = self.type_composition()?;

                params.push(FieldDeclaration::new(param_name, param_type));
            }
        }

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            "')'",
            "function params",
        )?;

        

        self.eat(
            TokenKind::Operator(OperatorKind::Arrow),
            CompilationErrorKind::ExpectedToken {
                expect: "function return kind".into(),
                found: self.peek().lexeme,
                after: Some("->".into()),
            },
        )?;

        let return_kind: Expression = self.type_composition()?;

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::SemiColon),
                "';'",
                "return kind",
            )?
            .span;

        Ok(Statement::NativeFunction(
            function_name,
            params,
            return_kind,
            end_span.merge(start_token.span),
            start_token.line,
        ))
    }

    fn decorator_declaration(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        let hash_token = self.previous();

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftBracket),
            "'['",
            "'#'",
        )?;

        let mut decorators: Vec<Expression> = Vec::new();

        let mut decorator = self.parse_decorator()?;
        decorators.push(decorator);

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
            decorator = self.parse_decorator()?;
            decorators.push(decorator);
        }

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::RightBracket),
                "']'",
                "attributes",
            )?
            .span;

        Ok(Statement::Decorator(
            hash_token,
            decorators,
            end_span.merge(start_token.span),
            start_token.line,
        ))
    }

    fn parse_decorator(&mut self) -> Result<Expression, ParseError> {
        let decorator_name = self.eat(
            TokenKind::Identifier,
            CompilationErrorKind::ExpectedToken {
                expect: "attribute name".into(),
                found: self.peek().lexeme,
                after: None,
            },
        )?;

        let start_span = decorator_name.span;

        let mut args: Vec<Rc<Expression>> = Vec::new();
        let mut end_span = start_span;

        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftParen)]) {
            let mut arg = self.expression()?;
            args.push(Rc::new(arg));

            if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
                while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                    arg = self.expression()?;
                    args.push(Rc::new(arg));
                }
            }

            let right_paren = self.expect_token(
                TokenKind::Punctuation(PunctuationKind::RightParen),
                "')'",
                "attribute arguments",
            )?;
            end_span = right_paren.span;
        }

        let result = Expression::Attribute(decorator_name, args, start_span.merge(end_span));

        Ok(result)
    }

    fn function_declaration(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        let function_name = self.expect_token(TokenKind::Identifier, "function name", "'def'")?;

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftParen),
            "'('",
            "function name",
        )?;

        let mut params: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            let param_name = self.eat(
                TokenKind::Identifier,
                CompilationErrorKind::ExpectedToken {
                    expect: "param name".into(),
                    found: self.peek().lexeme,
                    after: None,
                },
            )?;

            self.expect_token(
                TokenKind::Punctuation(PunctuationKind::Colon),
                "':'",
                "param name",
            )?;

            let param_type = self.type_composition()?;

            params.push(FieldDeclaration::new(param_name, param_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                let param_name = self.eat(
                    TokenKind::Identifier,
                    CompilationErrorKind::ExpectedToken {
                        expect: "param name".into(),
                        found: self.peek().lexeme,
                        after: None,
                    },
                )?;

                self.expect_token(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    "':'",
                    "param name",
                )?;

                let param_type = self.type_composition()?;

                params.push(FieldDeclaration::new(param_name, param_type));
            }
        }

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            "')'",
            "function params",
        )?;

        

        self.eat(
            TokenKind::Operator(OperatorKind::Arrow),
            CompilationErrorKind::ExpectedToken {
                expect: "function return kind".into(),
                found: self.peek().lexeme,
                after: None,
            },
        )?;

        let return_kind: Expression = self.type_composition()?;

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            "'{'",
            "function return kind",
        )?;

        let body = self.parse_scope()?;
        let end_span = body.span();

        Ok(Statement::Function(
            function_name,
            params,
            Rc::new(body),
            return_kind,
            end_span.merge(start_token.span),
            start_token.line,
        ))
    }

    fn type_composition(&mut self) -> Result<Expression, ParseError> {
        let mut names: Vec<Token> = Vec::new();

        names.push(self.eat(
            TokenKind::Identifier,
            CompilationErrorKind::ExpectedToken {
                expect: "type name".into(),
                found: self.peek().lexeme,
                after: None,
            },
        )?);

        let start_span = names.first().unwrap().span;

        while self.try_eat(&[TokenKind::Operator(OperatorKind::BitwiseAnd)]) {
            names.push(self.eat(
                TokenKind::Identifier,
                CompilationErrorKind::ExpectedToken {
                    expect: "type name".into(),
                    found: self.peek().lexeme,
                    after: None,
                },
            )?);
        }

        let end_span = names.last().unwrap().span;

        Ok(Expression::TypeComposition(
            names,
            start_span.merge(end_span),
        ))
    }

    fn type_declaration(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        let type_name = self.expect_token(TokenKind::Identifier, "type name", "'type' keyword")?;

        let mut archetypes: Vec<Token> = Vec::new();

        if self.try_eat(&[TokenKind::Keyword(KeywordKind::With)]) {
            archetypes.push(self.expect_token(
                TokenKind::Identifier,
                "archetype name",
                "'with' keyword",
            )?);

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                archetypes.push(self.expect_token(
                    TokenKind::Identifier,
                    "archetype name",
                    "','",
                )?);
            }
        }

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            "'{'",
            "type name",
        )?;

        let mut fields: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
            let mut field_name = self.eat(
                TokenKind::Identifier,
                CompilationErrorKind::ExpectedToken {
                    expect: "field name".into(),
                    found: self.peek().lexeme,
                    after: None,
                },
            )?;

            self.expect_token(
                TokenKind::Punctuation(PunctuationKind::Colon),
                "':'",
                "field name",
            )?;

            let mut field_type: Expression = self.type_composition()?;
            fields.push(FieldDeclaration::new(field_name, field_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                if self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
                    break;
                }

                field_name = self.eat(
                    TokenKind::Identifier,
                    CompilationErrorKind::ExpectedToken {
                        expect: "field name".into(),
                        found: self.peek().lexeme,
                        after: None,
                    },
                )?;

                self.expect_token(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    "':'",
                    "field name",
                )?;

                field_type = self.type_composition()?;
                fields.push(FieldDeclaration::new(field_name, field_type));
            }
        }

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::RightBrace),
                "'}'",
                "type fields",
            )?
            .span;

        Ok(Statement::Type(
            type_name,
            archetypes,
            fields,
            end_span.merge(start_token.span),
            start_token.line,
        ))
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        match self.advance().kind {
            TokenKind::Keyword(keyword) => match keyword {
                KeywordKind::If => self.if_statement(),
                KeywordKind::While => self.while_statement(),
                KeywordKind::For => self.for_statement(),
                KeywordKind::Let => self.variable_declaration(),
                KeywordKind::Import => self.import_statement(),
                _ => Err(ParseError::new(
                    CompilationErrorKind::InvalidKeyword {
                        keyword: self.peek().lexeme.clone(),
                    },
                    self.peek().line,
                    self.peek().span,
                )),
            },
            TokenKind::Punctuation(punctuation) => match punctuation {
                PunctuationKind::LeftBrace => self.parse_scope(),

                _ => {
                    self.backtrack();
                    self.expression_statement()
                }
            },
            _ => {
                self.backtrack();
                self.expression_statement()
            }
        }
    }

    fn for_statement(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();

        let variable_name = self.eat(
            TokenKind::Identifier,
            CompilationErrorKind::ExpectedToken {
                expect: "identifier".into(),
                found: self.peek().lexeme.clone(),
                after: Some("'for' keyword".into()),
            },
        )?;

        self.eat(
            TokenKind::Keyword(KeywordKind::In),
            CompilationErrorKind::ExpectedToken {
                expect: "'in'".into(),
                found: self.peek().lexeme.clone(),
                after: Some("variable name".into()),
            },
        )?;

        let iterator_expr = self.expression()?;
        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            "'{'",
            "iterator expression",
        )?;

        let scope = self.parse_scope()?;

        if let Statement::Scope(stmts, _, _) = &scope
            && stmts.is_empty() {
                return Ok(scope);
            }

        let full_loop_span = start_token.span.merge(scope.span());

        let iterator_variable_name = Token::new(
            TokenKind::Identifier,
            format!("${}", variable_name.lexeme),
            variable_name.line,
            variable_name.column,
            variable_name.span,
        );
        let iterator_variable = Statement::Variable(
            iterator_variable_name.clone(),
            Some(iterator_expr.clone()),
            iterator_expr.span(),
            start_token.line,
        );

        let user_variable = Statement::Variable(
            variable_name.clone(),
            None,
            variable_name.span,
            variable_name.line,
        );

        let paren = Token::new_synthetic(")");
        let has_next_token = Token::new_synthetic("has_next");
        let next_token = Token::new_synthetic("next");

        let iterator_var_expr = || {
            Rc::new(Expression::Variable(
                iterator_variable_name.clone(),
                iterator_variable_name.span,
            ))
        };

        let while_condition = Expression::Call(
            Rc::new(Expression::Get(
                iterator_var_expr(),
                has_next_token,
                iterator_variable_name.span,
            )),
            paren.clone(),
            Vec::new(),
            iterator_variable_name.span,
        );

        let next_call = Expression::Call(
            Rc::new(Expression::Get(
                iterator_var_expr(),
                next_token,
                iterator_variable_name.span,
            )),
            paren,
            Vec::new(),
            iterator_variable_name.span,
        );
        let assignment_expr = Expression::Assign(
            variable_name.clone(),
            Rc::new(next_call),
            variable_name.span.merge(iterator_variable_name.span),
        );
        let assignment_stmt = Statement::Expression(
            assignment_expr.clone(),
            assignment_expr.span(),
            assignment_expr.line(),
        );

        let while_body = Statement::Scope(
            vec![Rc::new(assignment_stmt), Rc::new(scope.clone())],
            scope.span(),
            scope.line(),
        );

        let while_stmt = Statement::While(
            while_condition,
            Box::new(while_body),
            scope.span(),
            scope.line(),
        );

        Ok(Statement::Scope(
            vec![
                Rc::new(iterator_variable),
                Rc::new(user_variable),
                Rc::new(while_stmt),
            ],
            full_loop_span,
            scope.line(),
        ))
    }

    fn import_statement(&mut self) -> Result<Statement, ParseError> {
        let root = self.expect_token(TokenKind::Identifier, "module name", "'import' keyword")?;
        let start_token = root.clone();
        let start_span = root.span;

        let mut path: Vec<Token> = Vec::new();
        path.push(root);

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
            let part = self.expect_token(TokenKind::Identifier, "module name", "'.'")?;
            path.push(part);
        }

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::SemiColon),
                "';'",
                "module import",
            )?
            .span;

        Ok(Statement::Import(
            path,
            start_span.merge(end_span),
            start_token.line,
        ))
    }

    fn variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftBrace)]) {
            return self.parse_destructure_variables();
        }

        let name = self.expect_token(TokenKind::Identifier, "identifier", "'let'")?;

        let mut value: Option<Expression> = None;

        if self.try_eat(&[TokenKind::Operator(OperatorKind::Equal)]) {
            value = Some(self.expression()?);
        }

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::SemiColon),
                "';'",
                "variable declaration",
            )?
            .span;

        Ok(Statement::Variable(
            name,
            value,
            start_token.span.merge(end_span),
            start_token.line,
        ))
    }

    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();

        let condition = self.expression()?;
        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            "'{'",
            "'while' condition",
        )?;

        let body = self.parse_scope()?;
        let end_span = body.span();

        Ok(Statement::While(
            condition,
            Box::new(body),
            start_token.span.merge(end_span),
            start_token.line,
        ))
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        let keyword = self.previous();
        let start_token = keyword.clone();
        let start_span = keyword.span;

        let condition = self.expression()?;
        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            "'{'",
            "'if' condition",
        )?;

        let then_branch = self.parse_scope()?;
        let mut end_span = then_branch.span();

        let mut else_branch: Option<Box<Statement>> = None;

        if self.try_eat(&[TokenKind::Keyword(KeywordKind::Else)]) {
            let else_stmt = self.statement()?;
            end_span = else_stmt.span();
            else_branch = Some(Box::new(else_stmt));
        }

        let combined_span = start_span.merge(end_span);

        Ok(Statement::If(
            keyword,
            condition,
            Box::new(then_branch),
            else_branch,
            combined_span,
            start_token.line,
        ))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.peek();

        let expr = self.expression()?;
        let start_span = expr.span();

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::SemiColon),
                "';'",
                "expression",
            )?
            .span;

        Ok(Statement::Expression(
            expr,
            start_span.merge(end_span),
            start_token.line,
        ))
    }

    fn parse_scope(&mut self) -> Result<Statement, ParseError> {
        let start_token = self.previous();
        let start_span = start_token.span;

        let mut statements = Vec::<Rc<Statement>>::new();

        while !self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)])
            && !self.is_at_end()
        {
            match self.declaration() {
                Ok(stmt) => statements.push(Rc::new(stmt)),
                Err(e) => {
                    self.report_error(CompilationError::with_span(e.kind, Some(e.line), e.span));
                    self.synchronize();
                }
            }
        }

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::RightBrace),
                "'}'",
                "scope statements",
            )?
            .span;

        Ok(Statement::Scope(
            statements,
            start_span.merge(end_span),
            start_token.line,
        ))
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.ternary()?;

        if self.try_eat(&[TokenKind::Operator(OperatorKind::Equal)]) {
            let equals = self.previous();
            let value = self.assignment()?;

            let combined_span = expr.span().merge(value.span());

            match expr {
                Expression::Variable(name, _) => {
                    return Ok(Expression::Assign(name, Rc::new(value), combined_span));
                }
                Expression::Get(object, name, _) => {
                    return Ok(Expression::Set(object, name, Rc::new(value), combined_span));
                }
                Expression::ListGet(target, index, _) => {
                    return Ok(Expression::ListSet(
                        target,
                        index,
                        Rc::new(value),
                        combined_span,
                    ));
                }
                _ => {
                    return Err(ParseError::new(
                        CompilationErrorKind::InvalidAssignmentTarget,
                        equals.line,
                        combined_span,
                    ));
                }
            }
        }
        Ok(expr)
    }

    fn ternary(&mut self) -> Result<Expression, ParseError> {
        let if_branch = self.or()?;

        if self.try_eat(&[TokenKind::Keyword(KeywordKind::If)]) {
            let condition = self.expression()?;

            self.expect_token(
                TokenKind::Keyword(KeywordKind::Else),
                "'else' keyword",
                "condition",
            )?;

            let else_branch = self.expression()?;
            let combined_span = if_branch.span().merge(else_branch.span());

            return Ok(Expression::Ternary(
                Rc::new(condition),
                Rc::new(if_branch),
                Rc::new(else_branch),
                combined_span,
            ));
        }

        Ok(if_branch)
    }

    fn or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.and()?;

        while self.try_eat(&[TokenKind::Operator(OperatorKind::Or)]) {
            let operator = self.previous();
            let right = self.and()?;
            let combined_span = expr.span().merge(right.span());
            expr = Expression::Logical(Rc::new(expr), operator, Rc::new(right), combined_span);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.equality()?;

        while self.try_eat(&[TokenKind::Operator(OperatorKind::And)]) {
            let operator = self.previous();
            let right = self.equality()?;
            let combined_span = expr.span().merge(right.span());
            expr = Expression::Logical(Rc::new(expr), operator, Rc::new(right), combined_span);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;

        while self.try_eat(&[
            TokenKind::Operator(OperatorKind::EqualEqual),
            TokenKind::Operator(OperatorKind::NotEqual),
        ]) {
            let operator = self.previous();
            let right = self.comparison()?;
            let combined_span = expr.span().merge(right.span());
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right), combined_span);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.term()?;

        if self.try_eat(&[
            TokenKind::Operator(OperatorKind::Less),
            TokenKind::Operator(OperatorKind::LessEqual),
            TokenKind::Operator(OperatorKind::Greater),
            TokenKind::Operator(OperatorKind::GreaterEqual),
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            let combined_span = expr.span().merge(right.span());
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right), combined_span);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.factor()?;

        while self.try_eat(&[
            TokenKind::Operator(OperatorKind::Minus),
            TokenKind::Operator(OperatorKind::Plus),
        ]) {
            let operator = self.previous();
            let right = self.factor()?;
            let combined_span = expr.span().merge(right.span());
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right), combined_span);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.unary()?;

        while self.try_eat(&[
            TokenKind::Operator(OperatorKind::Star),
            TokenKind::Operator(OperatorKind::Slash),
        ]) {
            let operator = self.previous();
            let right = self.unary()?;
            let combined_span = expr.span().merge(right.span());
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right), combined_span);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParseError> {
        if self.try_eat(&[
            TokenKind::Operator(OperatorKind::Minus),
            TokenKind::Operator(OperatorKind::Plus),
            TokenKind::Operator(OperatorKind::Not),
        ]) {
            let operator = self.previous();
            let expression = self.unary()?;
            let combined_span = operator.span.merge(expression.span());
            return Ok(Expression::Unary(
                operator,
                Rc::new(expression),
                combined_span,
            ));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.literal()?;

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftBracket)]) {
            let index = self.expression()?;
            let right_bracket = self.expect_token(
                TokenKind::Punctuation(PunctuationKind::RightBracket),
                "']'",
                "index expression",
            )?;
            let combined_span = expr.span().merge(right_bracket.span);
            expr = Expression::ListGet(Box::new(expr), Box::new(index), combined_span);
        }

        if let Expression::Variable(_, _) = &expr
            && self.try_eat(&[
                TokenKind::Operator(OperatorKind::PostFixIncrement),
                TokenKind::Operator(OperatorKind::PostFixDecrement),
            ]) {
                let op = self.previous();
                let combined_span = expr.span().merge(op.span);
                return Ok(Expression::PostFix(op, Rc::new(expr), combined_span));
            }

        loop {
            if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftParen)]) {
                expr = self.finish_call(expr)?;
            } else if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
                let name = self.expect_token(TokenKind::Identifier, "property name", "'.'")?;
                let combined_span = expr.span().merge(name.span);
                expr = Expression::Get(Rc::new(expr), name, combined_span);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        let mut arguments = Vec::<Expression>::new();
        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            loop {
                if arguments.len() >= 255 {
                    return Err(ParseError::new(
                        CompilationErrorKind::ArgumentLimitOverflow,
                        self.peek().line,
                        self.peek().span,
                    ));
                }
                arguments.push(self.expression()?);
                if !self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                    break;
                }
            }
        }
        let right_paren = self.expect_token(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            "')'",
            "arguments",
        )?;
        let combined_span = callee.span().merge(right_paren.span);
        Ok(Expression::Call(
            Rc::new(callee),
            right_paren,
            arguments,
            combined_span,
        ))
    }

    fn literal_get(&mut self, target: Expression) -> Result<Expression, ParseError> {
        let mut expr = target;

        let name = self.expect_token(TokenKind::Identifier, "field name", "'.'")?;
        let combined_span = expr.span().merge(name.span);
        expr = Expression::Get(Rc::new(expr), name, combined_span);

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
            let name = self.expect_token(TokenKind::Identifier, "field name", "'.'")?;
            let combined_span = expr.span().merge(name.span);
            expr = Expression::Get(Rc::new(expr), name, combined_span);
        }

        Ok(expr)
    }

    fn literal(&mut self) -> Result<Expression, ParseError> {
        if self.try_eat(&[
            TokenKind::Literal(Literal::Int),
            TokenKind::Literal(Literal::Float),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Literal(Literal::String),
        ]) {
            let token = self.previous();
            let expr = Expression::Literal(token.clone(), token.span);

            if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
                return self.literal_get(expr);
            } else {
                return Ok(expr);
            }
        }

        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftParen)]) {
            let expr = self.group()?;

            if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
                return self.literal_get(expr);
            } else {
                return Ok(expr);
            }
        }

        if self.try_eat(&[TokenKind::Identifier]) {
            let id = self.previous();
            let expr = Expression::Variable(id.clone(), id.span);
            if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
                return self.literal_get(expr);
            }
            return Ok(expr);
        }

        match self.advance().kind {
            TokenKind::Punctuation(punctuation) => match punctuation {
                PunctuationKind::LeftBracket => self.collection_expression(
                    PunctuationKind::RightBracket,
                    CompilationErrorKind::ExpectedToken {
                        expect: "Expect ']'".into(),
                        found: self.peek().lexeme,
                        after: Some("list values".into()),
                    },
                    CollectionKind::List,
                ),
                PunctuationKind::LeftParen => self.collection_expression(
                    PunctuationKind::RightParen,
                    CompilationErrorKind::ExpectedToken {
                        expect: "Expect ')'".into(),
                        found: self.peek().lexeme,
                        after: Some("tuple values".into()),
                    },
                    CollectionKind::Tuple,
                ),
                _ => Err(ParseError::new(
                    CompilationErrorKind::UnexpectedToken {
                        token: self.previous().lexeme.clone(),
                    },
                    self.previous().line,
                    self.previous().span,
                )),
            },
            _ => Err(ParseError::new(
                CompilationErrorKind::UnexpectedToken {
                    token: self.peek().lexeme.clone(),
                },
                self.peek().line,
                self.peek().span,
            )),
        }
    }

    fn group(&mut self) -> Result<Expression, ParseError> {
        let start_span = self.previous().span;
        let expr = self.expression()?;
        let right_paren = self.expect_token(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            "')'",
            "group expression",
        )?;
        let combined_span = start_span.merge(right_paren.span);
        Ok(Expression::Group(Rc::new(expr), combined_span))
    }

    fn collection_expression(
        &mut self,
        closing: PunctuationKind,
        err: CompilationErrorKind,
        kind: CollectionKind,
    ) -> Result<Expression, ParseError> {
        let start_span = self.previous().span;
        let mut values: Vec<Rc<Expression>> = Vec::new();
        

        if !self.check(&[TokenKind::Punctuation(closing)]) {
            loop {
                values.push(Rc::new(self.expression()?));
                if !self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                    break;
                }
                if self.check(&[TokenKind::Punctuation(closing)]) {
                    break;
                }
            }
        }

        let end_token = self.eat(TokenKind::Punctuation(closing), err)?;

        let combined_span = start_span.merge(end_token.span);

        match kind {
            CollectionKind::List => Ok(Expression::List(values, combined_span)),
            CollectionKind::Tuple => Ok(Expression::Tuple(values, combined_span)),
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::EndOfFile
    }

    fn previous(&self) -> Token {
        self.stream.previous()
    }

    fn reset_internal_state(&mut self, stream: TokenStream) {
        self.stream = stream;
    }

    fn eat(&mut self, kind: TokenKind, err: CompilationErrorKind) -> Result<Token, ParseError> {
        match self.try_eat(&[kind]) {
            true => Ok(self.previous()),
            false => Err(ParseError::new(err, self.peek().line, self.peek().span)),
        }
    }

    fn expect_token(
        &mut self,
        kind: TokenKind,
        expected: &str,
        after: &str,
    ) -> Result<Token, ParseError> {
        match self.try_eat(&[kind]) {
            true => Ok(self.previous()),
            false => Err(ParseError::new(
                CompilationErrorKind::ExpectedToken {
                    expect: expected.to_string(),
                    found: self.peek().lexeme,
                    after: Some(after.to_string()),
                },
                self.peek().line,
                self.peek().span,
            )),
        }
    }

    fn try_eat(&mut self, kind: &[TokenKind]) -> bool {
        for k in kind {
            if *k == self.peek().kind {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&mut self, kind: &[TokenKind]) -> bool {
        for k in kind {
            if *k == self.peek().kind {
                return true;
            }
        }

        false
    }

    fn advance(&mut self) -> Token {
        self.stream.advance()
    }

    fn peek(&self) -> Token {
        self.stream.current().clone()
    }

    fn backtrack(&mut self) {
        self.stream.backtrack();
    }

    fn return_statement(&mut self) -> Result<Statement, ParseError> {
        let keyword = self.previous();
        let start_token = keyword.clone();
        let start_span = start_token.span;

        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::SemiColon)]) {
            return Ok(Statement::Return(
                keyword,
                None,
                start_span.merge(self.previous().span),
                start_token.line,
            ));
        }

        let value = self.expression()?;
        let end_span = value.span();

        let return_stmt = Statement::Return(
            keyword,
            Some(value),
            start_span.merge(end_span),
            start_token.line,
        );

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            "';'",
            "return value",
        )?;

        Ok(return_stmt)
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            match self.peek().kind {
                TokenKind::Punctuation(p) => match p {
                    PunctuationKind::SemiColon
                    | PunctuationKind::Hash
                    | PunctuationKind::RightBrace
                    | PunctuationKind::RightParen => return,
                    _ => {
                        self.advance();
                    }
                },

                TokenKind::EndOfFile => {
                    return;
                }

                TokenKind::Keyword(keyword) => match keyword {
                    KeywordKind::Def
                    | KeywordKind::Type
                    | KeywordKind::For
                    | KeywordKind::If
                    | KeywordKind::Let
                    | KeywordKind::While => {
                        return;
                    }

                    _ => {
                        self.advance();
                    }
                },

                _ => {
                    self.advance();
                }
            }
        }
    }

    fn parse_destructure_variables(&mut self) -> Result<Statement, ParseError> {
        let mut names: Vec<Token> = Vec::new();
        let start_token = self.previous();
        let start_span = start_token.span;

        let first = self.expect_token(TokenKind::Identifier, "'{'", "destructure declaration")?;

        names.push(first);

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
            let name = self.expect_token(TokenKind::Identifier, "field name", "','")?;
            names.push(name);
        }

        self.expect_token(
            TokenKind::Punctuation(PunctuationKind::RightBrace),
            "'}'",
            "fields in destructure declaration",
        )?;

        self.expect_token(
            TokenKind::Operator(OperatorKind::Equal),
            "'='",
            "destructure declaration fields",
        )?;

        let value = self.expression()?;

        let end_span = self
            .expect_token(
                TokenKind::Punctuation(PunctuationKind::SemiColon),
                "';'",
                "destructure declaration value",
            )?
            .span;

        Ok(Statement::DestructurePattern(
            names,
            value,
            start_span.merge(end_span),
            start_token.line,
        ))
    }
}
