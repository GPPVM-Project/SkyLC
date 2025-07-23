use std::rc::Rc;

use crate::{Expression, Span};

#[derive(Debug, Clone)]
pub struct BuiltinAttributeUsage {
    pub args: Vec<Rc<Expression>>,
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Decorator {
    pub attributes: Vec<BuiltinAttributeUsage>,
}

impl Decorator {
    pub fn new(attributes: Vec<BuiltinAttributeUsage>) -> Self {
        Self { attributes }
    }
}

impl From<Vec<BuiltinAttributeUsage>> for Decorator {
    fn from(attributes: Vec<BuiltinAttributeUsage>) -> Self {
        Self { attributes }
    }
}
