use crate::{Token, TokenKind};

#[derive(Default)]
pub struct TokenStream {
    tokens: Vec<Token>,
    position: usize,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    pub fn previous(&self) -> Token {
        if self.position > self.tokens.len() {
            let tk = self.tokens.last().unwrap().clone();
            return Token::new(
                TokenKind::EndOfFile,
                "\0".into(),
                tk.line,
                tk.column,
                tk.span,
            );
        }

        self.tokens[self.position - 1].clone()
    }

    pub fn current(&self) -> Token {
        if self.position >= self.tokens.len() {
            let tk = self.tokens.last().unwrap().clone();
            return Token::new(
                TokenKind::EndOfFile,
                "\0".into(),
                tk.line,
                tk.column,
                tk.span,
            );
        }

        self.tokens[self.position].clone()
    }

    pub fn advance(&mut self) -> Token {
        self.position += 1;
        self.previous()
    }

    pub fn look_ahead(&self, k: usize) -> Token {
        if self.position + k >= self.tokens.len() {
            return Token::new(
                TokenKind::EndOfFile,
                "\0".into(),
                self.tokens.last().unwrap().line,
                self.tokens.last().unwrap().column,
                self.tokens.last().unwrap().span,
            );
        }

        self.tokens[self.position + k].clone()
    }

    pub fn backtrack(&mut self) {
        self.position -= 1;
    }
}

impl From<TokenStream> for Vec<Token> {
    fn from(value: TokenStream) -> Self {
        value.tokens
    }
}
