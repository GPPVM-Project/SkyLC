use std::rc::Rc;

use crate::{Span, expressions::Expression, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDeclaration {
    pub name: Token,
    pub kind: Expression,
}

impl FieldDeclaration {
    pub fn new(name: Token, kind: Expression) -> Self {
        FieldDeclaration { name, kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // region:   --- Statements
    If(
        Token,
        Expression,
        Box<Statement>,
        Option<Box<Statement>>,
        Span,
        usize,
    ),
    While(Expression, Box<Statement>, Span, usize),
    // ForEach é desaçucarado para um While, então não é um nó direto na AST final do parser
    Expression(Expression, Span, usize),
    Scope(Vec<Rc<Statement>>, Span, usize),
    Import(Vec<Token>, Span, usize),
    Return(Token, Option<Expression>, Span, usize),
    // endregion:   --- Statements

    // region:   --- Declarations
    Decorator(Token, Vec<Expression>, Span, usize),
    BuiltinAttribute(Token, Vec<Token>, Span, usize),
    Type(Token, Vec<Token>, Vec<FieldDeclaration>, Span, usize),
    Function(
        Token,
        Vec<FieldDeclaration>,
        Rc<Statement>,
        Expression,
        Span,
        usize,
    ),
    Variable(Token, Option<Expression>, Span, usize),
    DestructurePattern(Vec<Token>, Expression, Span, usize),
    InternalDefinition(
        Token,
        Vec<FieldDeclaration>,
        Rc<Statement>,
        Expression,
        Span,
        usize,
    ),
    // endregion:   --- Declarations

    // region:   --- For Compiler
    EndCode,
    NativeFunction(Token, Vec<FieldDeclaration>, Expression, Span, usize),
    // endregion:   --- For Compiler
    Match,
    Global,
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::If(_, _, _, _, span, _) => *span,
            Statement::While(_, _, span, _) => *span,
            Statement::Expression(_, span, _) => *span,
            Statement::Scope(_, span, _) => *span,
            Statement::Import(_, span, _) => *span,
            Statement::Return(_, _, span, _) => *span,
            Statement::Decorator(_, _, span, _) => *span,
            Statement::BuiltinAttribute(_, _, span, _) => *span,
            Statement::Type(_, _, _, span, _) => *span,
            Statement::Function(_, _, _, _, span, _) => *span,
            Statement::Variable(_, _, span, _) => *span,
            Statement::DestructurePattern(_, _, span, _) => *span,
            Statement::InternalDefinition(_, _, _, _, span, _) => *span,
            Statement::NativeFunction(_, _, _, span, _) => *span,

            _ => Span { start: 0, end: 1 },
        }
    }

    pub fn line(&self) -> usize {
        match self {
            Statement::If(_, _, _, _, _, line) => *line,
            Statement::While(_, _, _, line) => *line,
            Statement::Expression(_, _, line) => *line,
            Statement::Scope(_, _, line) => *line,
            Statement::Import(_, _, line) => *line,
            Statement::Return(_, _, _, line) => *line,
            Statement::Decorator(_, _, _, line) => *line,
            Statement::BuiltinAttribute(_, _, _, line) => *line,
            Statement::Type(_, _, _, _, line) => *line,
            Statement::Function(_, _, _, _, _, line) => *line,
            Statement::Variable(_, _, _, line) => *line,
            Statement::DestructurePattern(_, _, _, line) => *line,
            Statement::InternalDefinition(_, _, _, _, _, line) => *line,
            Statement::NativeFunction(_, _, _, _, line) => *line,

            _ => 0,
        }
    }
}
