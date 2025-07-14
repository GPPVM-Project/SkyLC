use std::{cell::RefCell, rc::Rc};

use skyl_data::{Expression, SemanticValue, Token, TypeDescriptor};

pub trait SemanticProvider {
    /// Returns the TypeDescritor that describes the expression inside TypeOf instance.
    fn get_type_of(&self, target: TypeOf) -> Rc<RefCell<TypeDescriptor>>;

    /// Returns the TypeDescritor of variable, if exists, if the variable is not already initialized, returns None
    fn get_variable_type(&self, target: TypeOfVariable) -> Option<Rc<RefCell<TypeDescriptor>>>;

    fn query_type(&self, query: TypeQuery) -> Rc<RefCell<TypeDescriptor>>;

    fn look_at(&self, name: &Token) -> Option<SemanticValue>;
}

pub enum TypeQuery<'a> {
    ByName(&'a str),
    ById(u32),
}

pub struct TypeOf<'ctx>(pub &'ctx Expression);
pub struct TypeOfVariable<'ctx>(pub &'ctx Token);
