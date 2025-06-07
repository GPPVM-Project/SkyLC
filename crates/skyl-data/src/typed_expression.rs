use std::{cell::RefCell, rc::Rc};

use crate::{
    descriptors::{MethodDescriptor, TypeDescriptor},
    semantic_types::FunctionPrototype,
    token::Token,
};

#[derive(Debug, Clone)]
pub enum AnnotatedExpression {
    Literal(Token, Rc<RefCell<TypeDescriptor>>),
    Unary(Token, Box<AnnotatedExpression>, Rc<RefCell<TypeDescriptor>>),
    Group(Box<AnnotatedExpression>, Rc<RefCell<TypeDescriptor>>),
    Arithmetic(
        Box<AnnotatedExpression>,
        Token,
        Box<AnnotatedExpression>,
        Rc<RefCell<TypeDescriptor>>,
    ),
    Logical(
        Box<AnnotatedExpression>,
        Token,
        Box<AnnotatedExpression>,
        Rc<RefCell<TypeDescriptor>>,
    ),
    Assign(Token, Box<AnnotatedExpression>, Rc<RefCell<TypeDescriptor>>),
    Get(Box<AnnotatedExpression>, Token, Rc<RefCell<TypeDescriptor>>),
    Variable(Token, Rc<RefCell<TypeDescriptor>>),
    Call(
        FunctionPrototype,
        Token,
        Vec<Box<AnnotatedExpression>>,
        Rc<RefCell<TypeDescriptor>>,
    ),
    CallNative(
        FunctionPrototype,
        Token,
        Vec<Box<AnnotatedExpression>>,
        Rc<RefCell<TypeDescriptor>>,
    ),
    List(Vec<Box<AnnotatedExpression>>, Rc<RefCell<TypeDescriptor>>),
    TypeComposition(Rc<RefCell<TypeDescriptor>>),
    Attribute(Token, Vec<Box<AnnotatedExpression>>),
    Void,
    PostFix(Token, Box<AnnotatedExpression>),
    Set(
        Box<AnnotatedExpression>,
        Token,
        Box<AnnotatedExpression>,
        Rc<RefCell<TypeDescriptor>>,
    ),
    ListGet(Box<AnnotatedExpression>, Box<AnnotatedExpression>),
    CallMethod(
        Box<AnnotatedExpression>,
        MethodDescriptor,
        Vec<Box<AnnotatedExpression>>,
    ),
    CallNativeMethod(
        Box<AnnotatedExpression>,
        Token,
        Vec<Box<AnnotatedExpression>>,
        Rc<RefCell<TypeDescriptor>>,
    ),
    ListSet(
        Box<AnnotatedExpression>,
        Box<AnnotatedExpression>,
        Box<AnnotatedExpression>,
        Rc<RefCell<TypeDescriptor>>,
    ),
}
