use std::{cell::RefCell, rc::Rc};

use skyl_data::{Expression, Token};

use crate::provider::SemanticProvider;

pub trait ExpressionVisitor {
    type Output: Sized;

    fn visit_literal_expression(
        &self,
        literal: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_unary_expression(
        &self,
        operator: &Token,
        right: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_arithmetic_expression(
        &self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_logical_expression(
        &self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_postfix_expression(
        &self,
        left: &Expression,
        operator: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_ternary_expression(
        &self,
        condition: &Expression,
        then_branch: &Expression,
        else_branch: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_assign_expression(
        &self,
        target: &Token,
        value: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_get_expression(
        &self,
        expression: &Expression,
        getter: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_variable_expression(
        &self,
        name: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_set_expression(
        &self,
        expression: &Expression,
        getter: &Token,
        value: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_call_expression(
        &self,
        callee: &Expression,
        paren: &Token,
        args: &Vec<Expression>,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_list_expression(
        &self,
        elements: &Vec<Rc<Expression>>,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_attribute(
        &self,
        name: &Token,
        args: &Vec<Rc<Expression>>,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;

    fn visit_group_expression(
        &self,
        expression: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output;
}
