use std::fmt::{self, Display};

use crate::CoersionKind;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum TokenKind {
    Operator(OperatorKind),
    Keyword(KeywordKind),
    Punctuation(PunctuationKind),
    Literal(Literal),
    Identifier,
    EndOfFile,
    Underscore,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum OperatorKind {
    Plus,
    Minus,
    Star,
    Slash,
    EqualEqual,
    Less,
    Greater,
    NotEqual,
    And,
    BitwiseAnd,
    Or,
    BitwiseOr,
    DoubleStarEqual,
    DoubleStar,
    GreaterEqual,
    LessEqual,
    Equal,
    Not,
    Arrow,
    PostFixIncrement,
    PostFixDecrement,
}

impl Into<CoersionKind> for OperatorKind {
    fn into(self) -> CoersionKind {
        match self {
            OperatorKind::Plus => CoersionKind::Add,
            OperatorKind::Minus => CoersionKind::Sub,
            OperatorKind::Star => CoersionKind::Mul,
            OperatorKind::Slash => CoersionKind::Div,
            OperatorKind::EqualEqual => todo!(),
            OperatorKind::Less => todo!(),
            OperatorKind::Greater => todo!(),
            OperatorKind::NotEqual => todo!(),
            OperatorKind::And => todo!(),
            OperatorKind::BitwiseAnd => todo!(),
            OperatorKind::Or => todo!(),
            OperatorKind::BitwiseOr => todo!(),
            OperatorKind::DoubleStarEqual => todo!(),
            OperatorKind::DoubleStar => todo!(),
            OperatorKind::GreaterEqual => todo!(),
            OperatorKind::LessEqual => todo!(),
            OperatorKind::Equal => todo!(),
            OperatorKind::Not => todo!(),
            OperatorKind::Arrow => todo!(),
            OperatorKind::PostFixIncrement => todo!(),
            OperatorKind::PostFixDecrement => todo!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum KeywordKind {
    If,
    Else,
    Elif,
    For,
    While,
    Return,
    Def,
    Import,
    Lambda,
    Try,
    Except,
    Finally,
    Global,
    Type,
    Or,
    And,
    Let,
    In,
    With,
    Native,
    Builtin,
    Attribute,
    Internal,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum PunctuationKind {
    Comma,
    Dot,
    Colon,
    SemiColon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Hash,
    LeftBracket,
    RightBracket,
    Slash,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Literal {
    String,
    Int,
    Float,
    Boolean,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn merge(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.kind, self.lexeme)
    }
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize, column: usize, span: Span) -> Self {
        Token {
            kind,
            lexeme,
            line,
            column,
            span,
        }
    }
}
