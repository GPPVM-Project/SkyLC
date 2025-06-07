use std::rc::Rc;

use crate::{expressions::Expression, token::Token};

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
    // region:  --- Statements
    If(Token, Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    ForEach(Token, Expression, Box<Statement>),
    Expression(Expression),
    Match,
    Scope(Vec<Rc<Statement>>),
    Import(Vec<Token>),
    Return(Option<Expression>),
    // endregion:  --- Statements

    // region:  --- Declarations
    Decorator(Token, Vec<Expression>),
    BuiltinAttribute(Token, Vec<Token>),
    Type(Token, Vec<Token>, Vec<FieldDeclaration>),
    Function(Token, Vec<FieldDeclaration>, Rc<Statement>, Expression),
    Global,
    Variable(Token, Option<Expression>),
    DestructurePattern(Vec<Token>, Expression),
    InternalDefinition(Token, Vec<FieldDeclaration>, Rc<Statement>, Expression),
    // endregion:  --- Statements

    // region:  --- For Compiler
    EndCode,
    NativeFunction(Token, Vec<FieldDeclaration>, Expression), // endregion:  --- For Compiler
}
