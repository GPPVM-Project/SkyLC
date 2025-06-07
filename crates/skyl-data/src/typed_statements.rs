use std::{cell::RefCell, rc::Rc};

use crate::{
    descriptors::TypeDescriptor, semantic_types::FunctionPrototype, token::Token,
    typed_expression::AnnotatedExpression,
};

#[derive(Debug, Clone)]
pub enum AnnotatedStatement {
    If(
        Token,
        AnnotatedExpression,
        Box<AnnotatedStatement>,
        Option<Box<AnnotatedStatement>>,
    ),
    BuiltinAttribute(Token, Vec<Rc<RefCell<TypeDescriptor>>>),
    ForEach(Token, AnnotatedExpression, Box<AnnotatedStatement>),
    Variable(Token, Option<AnnotatedExpression>),
    Type(Rc<RefCell<TypeDescriptor>>),
    Function(FunctionPrototype, Box<AnnotatedStatement>),
    InternalDefinition(
        Rc<RefCell<TypeDescriptor>>,
        FunctionPrototype,
        Box<AnnotatedStatement>,
    ),
    Scope(Vec<Box<AnnotatedStatement>>),
    Return(Option<AnnotatedExpression>),
    Decorator(Token, Vec<AnnotatedExpression>),
    Expression(AnnotatedExpression),
    While(AnnotatedExpression, Box<AnnotatedStatement>),
    Global,
    EndCode,
    NativeFunction(FunctionPrototype),
}
