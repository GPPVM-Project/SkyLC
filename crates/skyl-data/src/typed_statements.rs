use std::{cell::RefCell, rc::Rc};

use crate::{
    descriptors::TypeDescriptor,
    semantic_types::FunctionPrototype,
    token::{Span, Token},
    typed_expression::AnnotatedExpression,
};

#[derive(Debug, Clone)]
pub enum AnnotatedStatement {
    If(
        Token,
        AnnotatedExpression,
        Box<AnnotatedStatement>,
        Option<Box<AnnotatedStatement>>,
        Span,
        usize,
    ),
    BuiltinAttribute(Token, Vec<Rc<RefCell<TypeDescriptor>>>, Span, usize),
    ForEach(
        Token,
        AnnotatedExpression,
        Box<AnnotatedStatement>,
        Span,
        usize,
    ),
    Variable(Token, Option<AnnotatedExpression>, Span, usize),
    Type(Rc<RefCell<TypeDescriptor>>, Span, usize),
    Function(FunctionPrototype, Box<AnnotatedStatement>, Span, usize),
    InternalDefinition(
        Rc<RefCell<TypeDescriptor>>,
        FunctionPrototype,
        Box<AnnotatedStatement>,
        Span,
        usize,
    ),
    Scope(Vec<Box<AnnotatedStatement>>, Span, usize),
    Return(Option<AnnotatedExpression>, Span, usize),
    Decorator(Token, Vec<AnnotatedExpression>, Span, usize),
    Expression(AnnotatedExpression, Span, usize),
    While(AnnotatedExpression, Box<AnnotatedStatement>, Span, usize),
    NativeFunction(FunctionPrototype, Span, usize),

    Global,
    EndCode,
}

impl AnnotatedStatement {
    pub fn line(&self) -> usize {
        match self {
            Self::If(_, _, _, _, _, line)
            | Self::BuiltinAttribute(_, _, _, line)
            | Self::ForEach(_, _, _, _, line)
            | Self::Variable(_, _, _, line)
            | Self::Type(_, _, line)
            | Self::Function(_, _, _, line)
            | Self::InternalDefinition(_, _, _, _, line)
            | Self::Scope(_, _, line)
            | Self::Return(_, _, line)
            | Self::Decorator(_, _, _, line)
            | Self::Expression(_, _, line)
            | Self::While(_, _, _, line)
            | Self::NativeFunction(_, _, line) => *line,

            Self::Global | Self::EndCode => 0,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::If(_, _, _, _, span, _)
            | Self::BuiltinAttribute(_, _, span, _)
            | Self::ForEach(_, _, _, span, _)
            | Self::Variable(_, _, span, _)
            | Self::Type(_, span, _)
            | Self::Function(_, _, span, _)
            | Self::InternalDefinition(_, _, _, span, _)
            | Self::Scope(_, span, _)
            | Self::Return(_, span, _)
            | Self::Decorator(_, _, span, _)
            | Self::Expression(_, span, _)
            | Self::While(_, _, span, _)
            | Self::NativeFunction(_, span, _) => *span,

            _ => Span { end: 1, start: 0 },
        }
    }

    pub fn end_line(&self) -> usize {
        match self {
            Self::If(_, _, then_branch, else_branch_opt, _, _) => {
                if let Some(else_branch) = else_branch_opt {
                    else_branch.end_line()
                } else {
                    then_branch.end_line()
                }
            }

            Self::Scope(statements, _, start_line) => {
                if let Some(last_stmt) = statements.last() {
                    last_stmt.end_line()
                } else {
                    *start_line
                }
            }

            Self::ForEach(_, _, body, _, _)
            | Self::While(_, body, _, _)
            | Self::Function(_, body, _, _)
            | Self::InternalDefinition(_, _, body, _, _) => body.end_line(),

            Self::Expression(expr, _, _) => 0,

            _ => self.line(),
        }
    }
}
