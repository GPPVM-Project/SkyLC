use std::fmt::Display;

use crate::{SourceFileID, Span, expressions::Expression, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDeclaration {
    pub access: Visibility,
    pub name: Token,
    pub kind: Expression,
}

impl FieldDeclaration {
    pub fn new(access: Visibility, name: Token, kind: Expression) -> Self {
        FieldDeclaration { access, name, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Visibility {
    Public,
    Private,
    Module,
}

impl Visibility {
    pub fn can_access(
        &self,
        visitor_file: SourceFileID,
        owner_file: SourceFileID,
        visitor_type: Option<u32>,
        owner_type: Option<u32>,
    ) -> bool {
        match self {
            Visibility::Public => true,
            Visibility::Module => visitor_file == owner_file,
            Visibility::Private => match (visitor_type, owner_type) {
                (Some(v_type), Some(o_type)) => v_type == o_type,
                _ => visitor_file == owner_file,
            },
        }
    }
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Visibility::Public => "pub",
            Visibility::Private => "private",
            Visibility::Module => "mod",
        })
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
    Scope(Vec<Statement>, Span, usize),
    Import(Vec<Token>, Span, usize),
    Return(Token, Option<Expression>, Span, usize),
    // endregion:   --- Statements

    // region:   --- Declarations
    Decorator(Token, Vec<Expression>, Span, usize),
    BuiltinAttribute(Token, Vec<Token>, Span, usize),
    Type(
        Visibility,
        Token,
        Vec<Token>,
        Vec<FieldDeclaration>,
        Span,
        usize,
    ),
    Function(
        Visibility,
        Token,
        Vec<FieldDeclaration>,
        Box<Statement>,
        Expression,
        Span,
        usize,
    ),
    Variable(Token, Option<Expression>, Span, usize),
    DestructurePattern(Vec<Token>, Expression, Span, usize),
    InternalDefinition(
        Visibility,
        Token,
        Vec<FieldDeclaration>,
        Box<Statement>,
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
            Statement::Type(_, _, _, _, span, _) => *span,
            Statement::Function(_, _, _, _, _, span, _) => *span,
            Statement::Variable(_, _, span, _) => *span,
            Statement::DestructurePattern(_, _, span, _) => *span,
            Statement::InternalDefinition(_, _, _, _, _, span, _) => *span,
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
            Statement::Type(_, _, _, _, _, line) => *line,
            Statement::Function(_, _, _, _, _, _, line) => *line,
            Statement::Variable(_, _, _, line) => *line,
            Statement::DestructurePattern(_, _, _, line) => *line,
            Statement::InternalDefinition(_, _, _, _, _, _, line) => *line,
            Statement::NativeFunction(_, _, _, _, line) => *line,

            _ => 0,
        }
    }
}
