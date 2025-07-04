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
        self.tokens[self.position - 1].clone()
    }

    pub fn current(&self) -> Token {
        self.tokens[self.position].clone()
    }

    pub fn advance(&mut self) -> Token {
        self.position += 1;
        self.tokens[self.position - 1].clone()
    }

    pub fn look_ahead(&self, k: usize) -> Token {
        if self.position + k >= self.tokens.len() {
            return Token::new(
                TokenKind::EndOfFile,
                "\0".into(),
                self.tokens.last().unwrap().line,
                self.tokens.last().unwrap().column,
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
