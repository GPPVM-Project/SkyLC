use crate::{AnnotatedStatement, Statement};

#[derive(Debug, Clone)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

impl Ast {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, Clone)]
pub struct AnnotatedAST {
    pub statements: Vec<AnnotatedStatement>,
}

impl AnnotatedAST {
    pub fn new(statements: Vec<AnnotatedStatement>) -> Self {
        Self { statements }
    }
}
