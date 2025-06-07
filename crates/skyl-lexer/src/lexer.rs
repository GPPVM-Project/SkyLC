use std::{cell::RefCell, collections::HashMap, rc::Rc};

use skyl_data::{
    KeywordKind, Literal, OperatorKind, PunctuationKind, Token, TokenKind, TokenStream,
};
use skyl_driver::errors::{CompilationError, CompilerErrorReporter};

#[derive(Debug)]
pub struct Lexer {
    pub(crate) source: Vec<char>,
    pub(crate) line: usize,
    pub(crate) column: usize,
    pub(crate) start: usize,
    pub(crate) length: usize,
    pub(crate) tokens: Vec<Token>,
    pub(crate) keywords: HashMap<String, TokenKind>,
    pub(crate) reporter: Rc<RefCell<CompilerErrorReporter>>,
}

pub fn create_keywords() -> HashMap<String, TokenKind> {
    let mut keywords = HashMap::new();
    keywords.insert("def".to_string(), TokenKind::Keyword(KeywordKind::Def));
    keywords.insert(
        "global".to_string(),
        TokenKind::Keyword(KeywordKind::Global),
    );
    keywords.insert("let".to_string(), TokenKind::Keyword(KeywordKind::Let));
    keywords.insert("true".to_string(), TokenKind::Literal(Literal::Boolean));
    keywords.insert("false".to_string(), TokenKind::Literal(Literal::Boolean));
    keywords.insert("type".to_string(), TokenKind::Keyword(KeywordKind::Type));
    keywords.insert(
        "import".to_string(),
        TokenKind::Keyword(KeywordKind::Import),
    );
    keywords.insert("not".to_string(), TokenKind::Operator(OperatorKind::Not));
    keywords.insert("and".to_string(), TokenKind::Operator(OperatorKind::And));
    keywords.insert("or".to_string(), TokenKind::Operator(OperatorKind::Or));
    keywords.insert("if".to_string(), TokenKind::Keyword(KeywordKind::If));
    keywords.insert("else".to_string(), TokenKind::Keyword(KeywordKind::Else));
    keywords.insert("elif".to_string(), TokenKind::Keyword(KeywordKind::Elif));
    keywords.insert("def".to_string(), TokenKind::Keyword(KeywordKind::Def));
    keywords.insert("while".to_string(), TokenKind::Keyword(KeywordKind::While));
    keywords.insert("for".to_string(), TokenKind::Keyword(KeywordKind::For));
    keywords.insert("in".to_string(), TokenKind::Keyword(KeywordKind::In));
    keywords.insert("with".to_string(), TokenKind::Keyword(KeywordKind::With));
    keywords.insert(
        "return".to_string(),
        TokenKind::Keyword(KeywordKind::Return),
    );
    keywords.insert(
        "native".to_string(),
        TokenKind::Keyword(KeywordKind::Native),
    );
    keywords.insert(
        "builtin".to_string(),
        TokenKind::Keyword(KeywordKind::Builtin),
    );
    keywords.insert(
        "attribute".to_string(),
        TokenKind::Keyword(KeywordKind::Attribute),
    );
    keywords.insert(
        "internal".to_string(),
        TokenKind::Keyword(KeywordKind::Internal),
    );

    keywords
}

#[allow(dead_code)]
impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer {
            source: source.chars().collect(),
            line: 1,
            column: 1,
            start: 0,
            length: 0,
            tokens: Vec::new(),
            keywords: create_keywords(),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::new())),
        }
    }

    pub fn without_source() -> Self {
        Lexer {
            source: Vec::new(),
            column: 1,
            length: 0,
            line: 1,
            start: 0,
            tokens: Vec::new(),
            keywords: create_keywords(),
            reporter: Rc::new(RefCell::new(CompilerErrorReporter::new())),
        }
    }

    pub fn reset_internal_state(&mut self, source: String) {
        self.source = source.chars().collect();
        self.column = 1;
        self.length = 0;
        self.start = 0;
        self.line = 1;
        self.tokens = Vec::new();
        self.keywords = create_keywords();
    }

    pub fn scan_tokens(&mut self, reporter: Rc<RefCell<CompilerErrorReporter>>) -> TokenStream {
        self.reporter = reporter.clone();

        while !self.is_at_end() {
            self.scan_token();
        }

        self.make_token_with_lexeme(TokenKind::EndOfFile, String::from("\0"));

        TokenStream::new(self.tokens.clone())
    }

    fn sync_cursors(&mut self) {
        self.start += self.length;
        self.length = 0;
    }

    fn scan_token(&mut self) {
        self.sync_cursors();

        let c: char = match self.advance() {
            None => '\0',
            Some(character) => character,
        };

        match c {
            '\n' => {
                self.column = 1;
                self.line += 1;
            }
            ' ' | '\t' => {}
            '\r' => {}
            '#' => self.make_token(TokenKind::Punctuation(PunctuationKind::Hash)),
            '[' => self.make_token(TokenKind::Punctuation(PunctuationKind::LeftBracket)),
            ']' => self.make_token(TokenKind::Punctuation(PunctuationKind::RightBracket)),
            '(' => self.make_token(TokenKind::Punctuation(PunctuationKind::LeftParen)),
            ')' => self.make_token(TokenKind::Punctuation(PunctuationKind::RightParen)),
            '{' => self.make_token(TokenKind::Punctuation(PunctuationKind::LeftBrace)),
            '}' => self.make_token(TokenKind::Punctuation(PunctuationKind::RightBrace)),
            '+' => {
                if self.try_eat('+') {
                    self.make_token(TokenKind::Operator(OperatorKind::PostFixIncrement))
                } else {
                    self.make_token(TokenKind::Operator(OperatorKind::Plus))
                }
            }
            '-' => {
                if self.try_eat('>') {
                    self.make_token(TokenKind::Operator(OperatorKind::Arrow))
                } else if self.try_eat('-') {
                    self.make_token(TokenKind::Operator(OperatorKind::PostFixDecrement))
                } else {
                    self.make_token(TokenKind::Operator(OperatorKind::Minus))
                }
            }
            '*' => {
                if self.try_eat('*') {
                    if self.try_eat('=') {
                        self.make_token(TokenKind::Operator(OperatorKind::DoubleStarEqual));
                    } else {
                        self.make_token(TokenKind::Operator(OperatorKind::DoubleStar));
                    }
                } else {
                    self.make_token(TokenKind::Operator(OperatorKind::Star));
                }
            }
            '|' => self.make_token(TokenKind::Operator(OperatorKind::BitwiseOr)),
            '&' => self.make_token(TokenKind::Operator(OperatorKind::BitwiseAnd)),
            '/' => {
                if self.try_eat('/') {
                    self.comment();
                } else {
                    self.make_token(TokenKind::Operator(OperatorKind::Slash))
                }
            }
            '!' => {
                if self.try_eat('=') {
                    self.make_token(TokenKind::Operator(OperatorKind::NotEqual));
                }
            }
            ',' => self.make_token(TokenKind::Punctuation(PunctuationKind::Comma)),
            ':' => self.make_token(TokenKind::Punctuation(PunctuationKind::Colon)),
            ';' => self.make_token(TokenKind::Punctuation(PunctuationKind::SemiColon)),
            '>' => {
                if self.try_eat('=') {
                    self.make_token(TokenKind::Operator(OperatorKind::GreaterEqual));
                } else {
                    self.make_token(TokenKind::Operator(OperatorKind::Greater));
                }
            }
            '<' => {
                if self.try_eat('=') {
                    self.make_token(TokenKind::Operator(OperatorKind::LessEqual));
                } else {
                    self.make_token(TokenKind::Operator(OperatorKind::Less));
                }
            }
            '=' => {
                if self.try_eat('=') {
                    self.make_token(TokenKind::Operator(OperatorKind::EqualEqual));
                } else {
                    self.make_token(TokenKind::Operator(OperatorKind::Equal));
                }
            }
            '"' => self.string('"').expect("Error in string."),
            '\'' => self.string('\'').expect("Error in string."),
            '.' => self.make_token(TokenKind::Punctuation(PunctuationKind::Dot)),

            _ => match c {
                '_' => {
                    if self.is_alpha_numeric(self.peek_next()) {
                        self.identifier().expect("Error in identifier.");
                    } else {
                        self.make_token(TokenKind::Underscore);
                    }
                }
                _ if self.is_digit(c) => self.number(),
                _ if self.is_alpha(c) => self.identifier().expect("Error in identifier."),
                _ => {
                    self.reporter
                        .borrow_mut()
                        .report_error(CompilationError::new(
                            format!("Invalid character '{}'", c),
                            Some(self.line),
                        ));
                }
            },
        }
    }

    fn identifier(&mut self) -> Result<(), String> {
        loop {
            if !(self.is_alpha_numeric(self.peek()) || self.peek() == '_') {
                break;
            }
            self.advance();
        }

        let lexeme: String = self.source[self.start..self.start + self.length]
            .iter()
            .collect();
        let kind = self
            .keywords
            .get(&lexeme)
            .cloned()
            .unwrap_or(TokenKind::Identifier);

        self.make_token_with_lexeme(kind, lexeme);
        Ok(())
    }

    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        let mut is_float = false;

        if self.check('.') && self.is_digit(self.peek_next()) {
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }

            is_float = true;
        }

        let lexeme: String = self.source[self.start..self.start + self.length]
            .iter()
            .collect();

        if is_float {
            self.make_token_with_lexeme(TokenKind::Literal(Literal::Float), lexeme);
        } else {
            self.make_token_with_lexeme(TokenKind::Literal(Literal::Int), lexeme);
        }
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_digit(c) || self.is_alpha(c)
    }

    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    }

    fn string(&mut self, end: char) -> Result<(), String> {
        let mut value = String::new();
        let mut escaped = false;

        while !self.is_at_end() {
            let c = self.peek();

            if escaped {
                let escape_char = match c {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '"' => '"',
                    '\'' => '\'',
                    other => other,
                };
                value.push(escape_char);
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == end {
                break;
            } else {
                value.push(c);
            }

            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(format!("Unterminated string at line {}.", self.line));
        }

        self.advance();

        self.make_token_with_lexeme(TokenKind::Literal(Literal::String), value);

        Ok(())
    }

    fn make_token(&mut self, kind: TokenKind) {
        let lexeme: String = self.source[self.start..self.start + self.length]
            .iter()
            .collect();
        self.make_token_with_lexeme(kind, lexeme);
    }

    fn make_token_with_lexeme(&mut self, kind: TokenKind, lexeme: String) {
        let token = Token::new(kind, lexeme, self.line, self.column);
        self.tokens.push(token);
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_at_end() {
            return None;
        }

        let c = self.source.get(self.start + self.length).cloned();
        self.length += 1;
        self.column += 1;
        c
    }

    fn try_eat(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if !self.check(c) {
            return false;
        }

        self.advance();
        return true;
    }

    fn check(&self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        c.eq(&self.peek())
    }

    fn peek(&self) -> char {
        self.source
            .get(self.start + self.length)
            .cloned()
            .unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.source
            .get(self.start + self.length + 1)
            .cloned()
            .unwrap_or('\0')
    }

    fn is_at_end(&self) -> bool {
        return self.start + self.length >= self.source.len();
    }

    fn comment(&mut self) {
        while !(self.try_eat('\n') || self.is_at_end()) {
            self.advance();
        }
    }
}
