use std::{cell::RefCell, rc::Rc};

use skyl_data::{
    Ast, CollectionKind, Expression, FieldDeclaration, KeywordKind, Literal, OperatorKind,
    PunctuationKind, Statement, Token, TokenKind, TokenStream,
};
use skyl_driver::errors::{CompilationError, CompilerErrorReporter, ParseError};

pub struct Parser {
    pub(crate) stream: TokenStream,
    pub(crate) reporter: Rc<RefCell<CompilerErrorReporter>>,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            stream: Default::default(),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::new())),
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
                    self.report_error(CompilationError::new(e.message, Some(e.line)));
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
        self.eat(
            TokenKind::Keyword(KeywordKind::Def),
            format!(
                "Expect 'def' after 'internal' keyword. At line {}",
                self.peek().line
            ),
        )?;

        let function_name = self.eat(
            TokenKind::Identifier,
            format!(
                "Expect internal definition name after 'def' keyword. At line {}.",
                self.peek().line
            ),
        )?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftParen),
            format!(
                "Expect '(' after definition name, but got {}. At line {}",
                self.peek().lexeme,
                self.peek().line
            ),
        )?;

        let mut params: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            let param_name = self.eat(TokenKind::Identifier, String::from("Expect param name."))?;

            self.eat(
                TokenKind::Punctuation(PunctuationKind::Colon),
                String::from("Expect ':' after param name."),
            )?;

            let param_type = self.type_composition()?;

            params.push(FieldDeclaration::new(param_name, param_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                let param_name =
                    self.eat(TokenKind::Identifier, String::from("Expect param name."))?;

                self.eat(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    String::from("Expect ':' after param name."),
                )?;

                let param_type = self.type_composition()?;

                params.push(FieldDeclaration::new(param_name, param_type));
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            String::from("Expect ')' after function params."),
        )?;

        let return_kind: Expression;

        self.eat(
            TokenKind::Operator(OperatorKind::Arrow),
            format!(
                "Expect function return kind. At line {}",
                self.previous().line
            ),
        )?;

        return_kind = self.type_composition()?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            String::from(format!(
                "Expect '{{' before function body, but got {}.",
                self.peek().lexeme
            )),
        )?;

        let body = self.parse_scope()?;

        Ok(Statement::InternalDefinition(
            function_name,
            params,
            Rc::new(body),
            return_kind,
        ))
    }

    fn parse_native_decl(&mut self) -> Result<Statement, ParseError> {
        if self.try_eat(&[TokenKind::Keyword(KeywordKind::Def)]) {
            return self.native_function();
        } else if self.try_eat(&[TokenKind::Keyword(KeywordKind::Type)]) {
            return self.native_type();
        }

        Err(ParseError::new(
            "Invalid native declaration".to_string(),
            self.peek().line,
        ))
    }

    fn parse_builtin_decl(&mut self) -> Result<Statement, ParseError> {
        if self.try_eat(&[TokenKind::Keyword(KeywordKind::Attribute)]) {
            return self.builtin_attribute();
        }

        Err(ParseError::new(
            "Invalid builtin declaration".to_string(),
            self.peek().line,
        ))
    }

    fn builtin_attribute(&mut self) -> Result<Statement, ParseError> {
        let name = self.eat(
            TokenKind::Identifier,
            "Expect identifier after 'attribute' keyword.".to_string(),
        )?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftParen),
            "Expect '(' after builtin attribute name.".to_string(),
        )?;

        let mut kinds: Vec<Token> = Vec::new();

        if self.try_eat(&[TokenKind::Identifier]) {
            kinds.push(self.previous());

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                let kind = self.eat(
                    TokenKind::Identifier,
                    "Expect identifier after ','.".to_string(),
                )?;

                kinds.push(kind);
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            "Expect ')' after attribute args.".to_string(),
        )?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            "Expect ';' after ')'.".to_string(),
        )?;

        Ok(Statement::BuiltinAttribute(name, kinds))
    }

    fn native_type(&mut self) -> Result<Statement, ParseError> {
        let type_name = self.eat(
            TokenKind::Identifier,
            String::from("Expect type name after 'type' keyword."),
        )?;

        let mut archetypes: Vec<Token> = Vec::new();

        if self.try_eat(&[TokenKind::Keyword(KeywordKind::With)]) {
            archetypes.push(self.eat(
                TokenKind::Identifier,
                "Expect archetype name after 'with' keyword.".to_string(),
            )?);

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                archetypes.push(self.eat(
                    TokenKind::Identifier,
                    "Expect archetype name after ','.".to_string(),
                )?);
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            String::from("Expect '{' after type name."),
        )?;

        let mut fields: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
            let mut field_name = self.eat(
                TokenKind::Identifier,
                String::from(format!(
                    "Expect field name, but got '{}'.",
                    self.peek().lexeme
                )),
            )?;

            self.eat(
                TokenKind::Punctuation(PunctuationKind::Colon),
                String::from("Expect ':' after field name."),
            )?;

            let mut field_type: Expression = self.type_composition()?;
            fields.push(FieldDeclaration::new(field_name, field_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                if self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
                    break;
                }

                field_name = self.eat(
                    TokenKind::Identifier,
                    String::from(format!(
                        "Expect field name, but got {}.",
                        self.peek().lexeme
                    )),
                )?;

                self.eat(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    String::from("Expect ':' after field name."),
                )?;

                field_type = self.type_composition()?;
                fields.push(FieldDeclaration::new(field_name, field_type));
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightBrace),
            String::from("Expect '}' after type fields."),
        )?;

        Ok(Statement::Type(type_name, archetypes, fields))
    }

    fn native_function(&mut self) -> Result<Statement, ParseError> {
        let function_name = self.eat(
            TokenKind::Identifier,
            String::from("Expect function name after 'def'."),
        )?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftParen),
            String::from(format!(
                "Expect '(' after function name, but got {}.",
                self.peek().lexeme
            )),
        )?;

        let mut params: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            let param_name = self.eat(TokenKind::Identifier, String::from("Expect param name."))?;

            self.eat(
                TokenKind::Punctuation(PunctuationKind::Colon),
                String::from("Expect ':' after param name."),
            )?;

            let param_type = self.type_composition()?;

            params.push(FieldDeclaration::new(param_name, param_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                let param_name =
                    self.eat(TokenKind::Identifier, String::from("Expect param name."))?;

                self.eat(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    String::from("Expect ':' after param name."),
                )?;

                let param_type = self.type_composition()?;

                params.push(FieldDeclaration::new(param_name, param_type));
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            String::from("Expect ')' after function params."),
        )?;

        let return_kind: Expression;

        self.eat(
            TokenKind::Operator(OperatorKind::Arrow),
            format!(
                "Expect function return kind. At line {}",
                self.previous().line
            ),
        )?;

        return_kind = self.type_composition()?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            format!(
                "Expect ';' after return kind. At line {}",
                self.previous().line
            ),
        )?;

        Ok(Statement::NativeFunction(
            function_name,
            params,
            return_kind,
        ))
    }

    fn decorator_declaration(&mut self) -> Result<Statement, ParseError> {
        let hash_token = self.previous();

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBracket),
            String::from("Expect '[' after '#'."),
        )?;

        let mut decorators: Vec<Expression> = Vec::new();

        let mut decorator = self.parse_decorator()?;
        decorators.push(decorator);

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
            decorator = self.parse_decorator()?;
            decorators.push(decorator);
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightBracket),
            String::from("Expect ']' after attributues."),
        )?;

        Ok(Statement::Decorator(hash_token, decorators))
    }

    fn parse_decorator(&mut self) -> Result<Expression, ParseError> {
        let decorator_name = self.eat(
            TokenKind::Identifier,
            String::from("Expect attribute name."),
        )?;

        let mut args: Vec<Rc<Expression>> = Vec::new();

        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftParen)]) {
            let mut arg = self.expression()?;
            args.push(Rc::new(arg));

            if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
                while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                    arg = self.expression()?;
                    args.push(Rc::new(arg));
                }
            }

            self.eat(
                TokenKind::Punctuation(PunctuationKind::RightParen),
                String::from("Expect ')' after attribute arguments."),
            )?;
        }

        Ok(Expression::Attribute(decorator_name, args))
    }

    fn function_declaration(&mut self) -> Result<Statement, ParseError> {
        let function_name = self.eat(
            TokenKind::Identifier,
            String::from("Expect function name after 'def'."),
        )?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftParen),
            String::from(format!(
                "Expect '(' after function name, but got {}.",
                self.peek().lexeme
            )),
        )?;

        let mut params: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            let param_name = self.eat(TokenKind::Identifier, String::from("Expect param name."))?;

            self.eat(
                TokenKind::Punctuation(PunctuationKind::Colon),
                String::from("Expect ':' after param name."),
            )?;

            let param_type = self.type_composition()?;

            params.push(FieldDeclaration::new(param_name, param_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                let param_name =
                    self.eat(TokenKind::Identifier, String::from("Expect param name."))?;

                self.eat(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    String::from("Expect ':' after param name."),
                )?;

                let param_type = self.type_composition()?;

                params.push(FieldDeclaration::new(param_name, param_type));
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            String::from("Expect ')' after function params."),
        )?;

        let return_kind: Expression;

        self.eat(
            TokenKind::Operator(OperatorKind::Arrow),
            format!(
                "Expect function return kind. At line {}",
                self.previous().line
            ),
        )?;

        return_kind = self.type_composition()?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            String::from(format!(
                "Expect '{{' before function body, but got {}.",
                self.peek().lexeme
            )),
        )?;

        let body = self.parse_scope()?;

        Ok(Statement::Function(
            function_name,
            params,
            Rc::new(body),
            return_kind,
        ))
    }

    fn type_composition(&mut self) -> Result<Expression, ParseError> {
        let mut names: Vec<Token> = Vec::new();

        names.push(self.eat(
            TokenKind::Identifier,
            format!(
                "Expect type name, but got '{}'. At line {}.",
                self.peek().lexeme,
                self.peek().line
            ),
        )?);

        while self.try_eat(&[TokenKind::Operator(OperatorKind::BitwiseAnd)]) {
            names.push(self.eat(TokenKind::Identifier, String::from("Expect type name."))?);
        }

        Ok(Expression::TypeComposition(names))
    }

    fn type_declaration(&mut self) -> Result<Statement, ParseError> {
        let type_name = self.eat(
            TokenKind::Identifier,
            String::from("Expect type name after 'type' keyword."),
        )?;

        let mut archetypes: Vec<Token> = Vec::new();

        if self.try_eat(&[TokenKind::Keyword(KeywordKind::With)]) {
            archetypes.push(self.eat(
                TokenKind::Identifier,
                "Expect archetype name after 'with' keyword.".to_string(),
            )?);

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                archetypes.push(self.eat(
                    TokenKind::Identifier,
                    "Expect archetype name after ','.".to_string(),
                )?);
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            String::from("Expect '{' after type name."),
        )?;

        let mut fields: Vec<FieldDeclaration> = Vec::new();

        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
            let mut field_name = self.eat(
                TokenKind::Identifier,
                String::from(format!(
                    "Expect field name, but got '{}'.",
                    self.peek().lexeme
                )),
            )?;

            self.eat(
                TokenKind::Punctuation(PunctuationKind::Colon),
                String::from("Expect ':' after field name."),
            )?;

            let mut field_type: Expression = self.type_composition()?;
            fields.push(FieldDeclaration::new(field_name, field_type));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                if self.check(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
                    break;
                }

                field_name = self.eat(
                    TokenKind::Identifier,
                    String::from(format!(
                        "Expect field name, but got {}.",
                        self.peek().lexeme
                    )),
                )?;

                self.eat(
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    String::from("Expect ':' after field name."),
                )?;

                field_type = self.type_composition()?;
                fields.push(FieldDeclaration::new(field_name, field_type));
            }
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightBrace),
            String::from("Expect '}' after type fields."),
        )?;

        Ok(Statement::Type(type_name, archetypes, fields))
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
                    format!("Invalid keyword '{}'.", self.peek().lexeme),
                    self.peek().line,
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
        let variable_name = self.eat(
            TokenKind::Identifier,
            String::from("Expect variable name after 'for'."),
        )?;

        let index_name = Token::new(
            TokenKind::Identifier,
            "_".into(),
            variable_name.line,
            variable_name.column,
        );

        self.eat(
            TokenKind::Keyword(KeywordKind::In),
            String::from("Expect 'in' after variable name."),
        )?;

        let iterator = self.expression()?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            String::from("Expect '{' after iterator expression."),
        )?;

        let body = self.parse_scope()?;
        let body = vec![
            Rc::new(Statement::Expression(Expression::Assign(
                variable_name.clone(),
                Rc::new(Expression::ListGet(
                    Box::new(iterator.clone()),
                    Box::new(Expression::Variable(index_name.clone())),
                )),
            ))),
            Rc::new(body),
            Rc::new(Statement::Expression(Expression::PostFix(
                Token::new(
                    TokenKind::Operator(OperatorKind::PostFixIncrement),
                    "++".into(),
                    index_name.line,
                    index_name.column,
                ),
                Rc::new(Expression::Variable(index_name.clone())),
            ))),
        ];

        Ok(Statement::Scope(vec![
            Rc::new(Statement::Variable(variable_name.clone(), None)),
            Rc::new(Statement::Variable(
                index_name.clone(),
                Some(Expression::Literal(Token::new(
                    TokenKind::Literal(Literal::Int),
                    "0".into(),
                    index_name.line,
                    index_name.column,
                ))),
            )),
            Rc::new(Statement::While(
                Expression::Arithmetic(
                    Rc::new(Expression::Variable(index_name.clone())),
                    Token::new(
                        TokenKind::Operator(OperatorKind::Less),
                        "<".into(),
                        index_name.line,
                        index_name.column,
                    ),
                    Rc::new(Expression::Call(
                        Rc::new(Expression::Get(
                            Rc::new(iterator.clone()),
                            Token::new(
                                TokenKind::Identifier,
                                "length".into(),
                                variable_name.line,
                                variable_name.column,
                            ),
                        )),
                        Token::new(
                            TokenKind::Punctuation(PunctuationKind::RightParen),
                            ")".into(),
                            variable_name.line,
                            variable_name.column,
                        ),
                        vec![],
                    )),
                ),
                Box::new(Statement::Scope(body)),
            )),
        ]))
    }

    fn import_statement(&mut self) -> Result<Statement, ParseError> {
        let root = self.eat(
            TokenKind::Identifier,
            String::from("Expect module name after 'import'."),
        )?;

        let mut path: Vec<Token> = Vec::new();
        path.push(root);

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
            let part = self.eat(TokenKind::Identifier, "Expect module name after '.'".into())?;
            path.push(part);
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            String::from("Expect ';' after module import."),
        )?;

        Ok(Statement::Import(path))
    }

    fn variable_declaration(&mut self) -> Result<Statement, ParseError> {
        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftBrace)]) {
            return self.parse_destructure_variables();
        }

        let name = self.eat(
            TokenKind::Identifier,
            String::from("Expect identifier after 'let'."),
        )?;

        let mut value: Option<Expression> = None;

        if self.try_eat(&[TokenKind::Operator(OperatorKind::Equal)]) {
            value = Some(self.expression()?);
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            String::from(format!(
                "Expect ';' after variable declaration, but got '{}'.",
                self.peek().lexeme
            )),
        )?;

        Ok(Statement::Variable(name, value))
    }

    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        let condition = self.expression()?;
        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            String::from("Expect '{' after 'while' condition."),
        )?;

        let body = self.parse_scope()?;

        Ok(Statement::While(condition, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        let keyword = self.previous();
        let condition = self.expression()?;
        self.eat(
            TokenKind::Punctuation(PunctuationKind::LeftBrace),
            format!(
                "Expect '{{' after 'if' condition, but got {:?}. At line {}.",
                self.peek(),
                self.peek().line
            ),
        )?;

        let then_branch = self.parse_scope()?;

        let mut else_branch: Option<Box<Statement>> = None;

        if self.try_eat(&[TokenKind::Keyword(KeywordKind::Else)]) {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Statement::If(
            keyword,
            condition,
            Box::new(then_branch),
            else_branch,
        ))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            String::from(format!(
                "Expect ';' after expression. At line {}.",
                self.previous().line
            )),
        )?;

        Ok(Statement::Expression(expr))
    }

    fn parse_scope(&mut self) -> Result<Statement, ParseError> {
        let mut statements = Vec::<Rc<Statement>>::new();

        while !self.try_eat(&[TokenKind::Punctuation(PunctuationKind::RightBrace)]) {
            statements.push(Rc::new(self.declaration()?));
        }

        Ok(Statement::Scope(statements))
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.ternary()?;

        if self.try_eat(&[TokenKind::Operator(OperatorKind::Equal)]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expression::Variable(name) => {
                    return Ok(Expression::Assign(name, Rc::new(value)));
                }

                Expression::Get(object, name) => {
                    return Ok(Expression::Set(object, name, Rc::new(value)));
                }

                Expression::ListGet(target, index) => {
                    return Ok(Expression::ListSet(target, index, Rc::new(value)));
                }

                _ => {
                    return Err(ParseError::new(
                        "Invalid assignment target.".to_string(),
                        equals.line,
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

            self.eat(
                TokenKind::Keyword(KeywordKind::Else),
                String::from("Expect 'else' keyword after condition."),
            )?;

            let else_branch = self.expression()?;
            return Ok(Expression::Ternary(
                Rc::new(condition),
                Rc::new(if_branch),
                Rc::new(else_branch),
            ));
        }

        Ok(if_branch)
    }

    fn or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.and()?;

        while self.try_eat(&[TokenKind::Operator(OperatorKind::Or)]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expression::Logical(Rc::new(expr), operator, Rc::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.equality()?;

        while self.try_eat(&[TokenKind::Operator(OperatorKind::And)]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expression::Logical(Rc::new(expr), operator, Rc::new(right));
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
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right));
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
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right));
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
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right));
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
            let right = self.factor()?;
            expr = Expression::Arithmetic(Rc::new(expr), operator, Rc::new(right));
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
            return Ok(Expression::Unary(operator, Rc::new(expression)));
        }

        Ok(self.call()?)
    }

    fn call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.literal()?;

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftBracket)]) {
            let index = self.expression()?;
            self.eat(
                TokenKind::Punctuation(PunctuationKind::RightBracket),
                "Expect ']' after index expression.".to_string(),
            )?;
            expr = Expression::ListGet(Box::new(expr), Box::new(index));
        }

        if let Expression::Variable(_) = &expr {
            if self.try_eat(&[
                TokenKind::Operator(OperatorKind::PostFixIncrement),
                TokenKind::Operator(OperatorKind::PostFixDecrement),
            ]) {
                return Ok(Expression::PostFix(self.previous(), Rc::new(expr)));
            }
        }

        loop {
            if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::LeftParen)]) {
                expr = self.finish_call(expr)?;
            } else if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
                let name = self.eat(
                    TokenKind::Identifier,
                    String::from("Expect property name after '.'."),
                )?;

                expr = Expression::Get(Rc::new(expr), name);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        let mut arguments = Vec::<Expression>::new();
        if !self.check(&[TokenKind::Punctuation(PunctuationKind::RightParen)]) {
            let mut has_args = true;

            while has_args {
                if arguments.iter().count() >= 255 {
                    return Err(ParseError::new(
                        "Can't have more than 255 arguments.".to_string(),
                        self.peek().line,
                    ));
                }

                arguments.push(self.expression()?);

                if !self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                    has_args = false;
                }
            }
        }

        let paren = self.eat(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            format!("Expect ')' after arguments. At line {}.", self.peek().line),
        )?;

        Ok(Expression::Call(Rc::new(callee), paren, arguments))
    }

    fn literal_get(&mut self, target: Expression) -> Result<Expression, ParseError> {
        let mut expr = target;

        expr = Expression::Get(
            Rc::new(expr),
            self.eat(
                TokenKind::Identifier,
                format!("Expect field name after '.'. At line {}.", self.peek().line),
            )?,
        );

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
            expr = Expression::Get(
                Rc::new(expr),
                self.eat(
                    TokenKind::Identifier,
                    format!("Expect field name after '.'. At line {}.", self.peek().line),
                )?,
            );
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
            let expr = Expression::Literal(self.previous());

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
            if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Dot)]) {
                return self.literal_get(Expression::Variable(id));
            }
            return Ok(Expression::Variable(self.previous()));
        }

        match self.advance().kind {
            TokenKind::Punctuation(punctuation) => match punctuation {
                PunctuationKind::LeftBracket => self.collection_expression(
                    PunctuationKind::RightBracket,
                    "Expect ']' after list values.",
                    CollectionKind::List,
                ),
                PunctuationKind::LeftParen => self.collection_expression(
                    PunctuationKind::RightParen,
                    "Expect ')' after tuple values.",
                    CollectionKind::List,
                ),
                _ => Err(ParseError::new(
                    format!("Invalid punctuation {:?}.", self.previous()),
                    self.previous().line,
                )),
            },
            _ => Err(ParseError::new(
                format!("Invalid expression {:?}.", self.peek()),
                self.peek().line,
            )),
        }
    }

    fn group(&mut self) -> Result<Expression, ParseError> {
        let expr = self.expression()?;
        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightParen),
            String::from("Expect ')' after group expression."),
        )?;

        Ok(Expression::Group(Rc::new(expr)))
    }

    fn collection_expression(
        &mut self,
        closing: PunctuationKind,
        error_msg: &str,
        kind: CollectionKind,
    ) -> Result<Expression, ParseError> {
        let mut values: Vec<Rc<Expression>> = Vec::new();

        if !self.try_eat(&[TokenKind::Punctuation(PunctuationKind::RightBracket)]) {
            values.push(Rc::new(self.expression()?));

            while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
                values.push(Rc::new(self.expression()?));
            }

            self.eat(TokenKind::Punctuation(closing), error_msg.to_string())?;
        }

        match kind {
            CollectionKind::List => Ok(Expression::List(values)),
            CollectionKind::Tuple => Ok(Expression::Tuple(values)),
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

    fn eat(&mut self, kind: TokenKind, msg: String) -> Result<Token, ParseError> {
        match self.try_eat(&[kind]) {
            true => Ok(self.previous()),
            false => Err(ParseError::new(msg, self.peek().line)),
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
        if self.try_eat(&[TokenKind::Punctuation(PunctuationKind::SemiColon)]) {
            return Ok(Statement::Return(None));
        }

        let value = Statement::Return(Some(self.expression()?));

        self.eat(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            "Expect ';' after return value.".to_string(),
        )?;

        Ok(value)
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if let TokenKind::Punctuation(kind) = self.previous().kind {
                if let PunctuationKind::SemiColon = kind {
                    return;
                }
            }

            match self.peek().kind {
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

        let first = self.eat(
            TokenKind::Identifier,
            "Expect identifier after '{' in destructure declaration".into(),
        )?;

        names.push(first);

        while self.try_eat(&[TokenKind::Punctuation(PunctuationKind::Comma)]) {
            let name = self.eat(TokenKind::Identifier, "Expect field name after ','".into())?;
            names.push(name);
        }

        self.eat(
            TokenKind::Punctuation(PunctuationKind::RightBrace),
            format!(
                "Expect '}}' after fields in destructure declaration, but got '{}'",
                self.peek().lexeme
            )
            .into(),
        )?;

        self.eat(
            TokenKind::Operator(OperatorKind::Equal),
            "Expect '=' after destructure declaration fields".into(),
        )?;

        let value = self.expression()?;

        self.eat(
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            "Expect ';' after destructure declaration value.".into(),
        )?;

        return Ok(Statement::DestructurePattern(names, value));
    }
}
