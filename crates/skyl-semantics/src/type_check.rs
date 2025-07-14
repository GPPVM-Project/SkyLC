use std::{cell::RefCell, rc::Rc};

use skyl_data::{Expression, Literal, LiteralKind, OperatorKind, Token, TokenKind, TypeDescriptor};
use skyl_driver::{errors::CompilationError, gpp_error};

use crate::{
    expression_visitor::ExpressionVisitor,
    provider::{SemanticProvider, TypeOf, TypeQuery},
};

pub struct TypeChecker;

impl TypeChecker {
    pub fn type_of(
        &self,
        expression: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Result<Rc<RefCell<TypeDescriptor>>, CompilationError> {
        match expression {
            Expression::Literal(l) => self.visit_literal_expression(&l.token, provider),
            Expression::Unary(u) => {
                self.visit_unary_expression(&u.operator, &u.expression, provider)
            }
            Expression::PostFix(p) => todo!(),
            Expression::Arithmetic(a) => {
                self.visit_arithmetic_expression(&a.left, &a.operator, &a.right, provider)
            }
            Expression::Logical(l) => todo!(),
            Expression::Ternary(t) => todo!(),
            Expression::Assign(a) => todo!(),
            Expression::Lambda => todo!(),
            Expression::Get(g) => self.visit_get_expression(&g.target, &g.field, provider),
            Expression::Variable(v) => self.visit_variable_expression(&v.name, provider),
            Expression::Set(s) => todo!(),
            Expression::Call(c) => todo!(),
            Expression::Tuple(expressions) => todo!(),
            Expression::List(expressions) => todo!(),
            Expression::TypeComposition(tokens) => todo!(),
            Expression::Attribute(a) => todo!(),
            Expression::Group(expression) => todo!(),
            Expression::Void => todo!(),
            Expression::ListGet(expression, expression1) => todo!(),
            Expression::ListSet(expression, expression1, expression2) => todo!(),
        }
    }
}

impl ExpressionVisitor for TypeChecker {
    type Output = Result<Rc<RefCell<TypeDescriptor>>, CompilationError>;

    fn visit_literal_expression(
        &self,
        literal: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        // println!("Visit Unary");
        if let TokenKind::Literal(l) = literal.kind {
            match l {
                LiteralKind::Boolean => Ok(provider.query_type(TypeQuery::ByName("bool"))),
                LiteralKind::Int => Ok(provider.query_type(TypeQuery::ByName("int"))),
                LiteralKind::Float => Ok(provider.query_type(TypeQuery::ByName("float"))),
                LiteralKind::String => Ok(provider.query_type(TypeQuery::ByName("str"))),
            }
        } else {
            Err(CompilationError::new(
                "Invalid literal kind".into(),
                Some(literal.line),
            ))
        }
    }

    fn visit_unary_expression(
        &self,
        operator: &Token,
        right: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        // println!("Visit Literal");
        self.type_of(right, provider)
    }

    fn visit_arithmetic_expression(
        &self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        // println!("Visit Arithmetic");

        if let TokenKind::Operator(operator) = operator.kind {
            match operator {
                OperatorKind::Plus
                | OperatorKind::Minus
                | OperatorKind::Star
                | OperatorKind::Slash => {
                    return self.type_of(&left, provider);
                }

                OperatorKind::Greater
                | OperatorKind::GreaterEqual
                | OperatorKind::Less
                | OperatorKind::NotEqual
                | OperatorKind::EqualEqual => {
                    return Ok(provider.query_type(TypeQuery::ByName("bool")));
                }

                _ => gpp_error!("Invalid arithmetic operator."),
            }
        }

        gpp_error!("Invalid arithmetic operator.");
    }

    fn visit_logical_expression(
        &self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        Ok(provider.query_type(TypeQuery::ByName("bool")))
    }

    fn visit_postfix_expression(
        &self,
        left: &Expression,
        operator: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        Ok(provider.query_type(TypeQuery::ByName("int")))
    }

    fn visit_ternary_expression(
        &self,
        condition: &Expression,
        then_branch: &Expression,
        else_branch: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        todo!()
    }

    fn visit_assign_expression(
        &self,
        target: &Token,
        value: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        Ok(provider.get_type_of(TypeOf(value)))
    }

    fn visit_get_expression(
        &self,
        expression: &Expression,
        getter: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        let mut current_kind: Option<Rc<RefCell<TypeDescriptor>>> = None;
        let mut current_expression = expression;
        let mut is_literal = false;

        let mut path = vec![getter.clone()];

        while let Expression::Get(g) = current_expression {
            path.push(g.field.clone());
            current_expression = &g.target;
        }

        if let Expression::Variable(v) = current_expression {
            path.push(v.name.clone());
        } else {
            current_kind = Some(self.type_of(&current_expression, provider)?.clone());
            is_literal = true;
        }

        let path: Vec<&Token> = path.iter().rev().collect();

        if is_literal {
            for (index, field) in path[0..].iter().enumerate() {
                current_kind = match current_kind.clone() {
                    None => {
                        gpp_error!(
                            "{} cannot have '{}' field.",
                            path[index - 1],
                            field.lexeme.clone()
                        );
                    }

                    Some(type_descriptor) => {
                        match type_descriptor.borrow().fields.get(&field.lexeme) {
                            None => match type_descriptor.borrow().methods.get(&field.lexeme) {
                                Some(method_decl) => {
                                    return Ok(provider
                                        .query_type(TypeQuery::ById(method_decl.return_kind_id))
                                        .clone());
                                }
                                None => {
                                    gpp_error!(
                                        "Variable '{}' is a '{}' instance and not have '{}' field.",
                                        path[index].lexeme.clone(),
                                        current_kind.unwrap().borrow().name,
                                        field.lexeme.clone()
                                    );
                                }
                            },
                            Some(field_decl) => Some(field_decl.kind.clone()),
                        }
                    }
                };
            }
        } else {
            current_kind = match provider.look_at(&path[0]) {
                None => {
                    gpp_error!("The kind of {} is not known here.", &path[0].lexeme);
                }
                Some(semantic_value) => semantic_value.kind,
            };

            for (index, field) in path[1..].iter().enumerate() {
                current_kind = match &current_kind {
                    None => {
                        gpp_error!(
                            "{} cannot have '{}' field.",
                            path[index - 1],
                            field.lexeme.clone()
                        );
                    }

                    Some(type_descriptor) => {
                        match type_descriptor.borrow().fields.get(&field.lexeme) {
                            Some(field_decl) => Some(field_decl.kind.clone()),
                            None => match type_descriptor.borrow().methods.get(&field.lexeme) {
                                Some(method_decl) => Some(
                                    provider
                                        .query_type(TypeQuery::ById(method_decl.return_kind_id))
                                        .clone(),
                                ),
                                None => {
                                    gpp_error!(
                                        "Variable '{}' is a '{}' instance and not have '{}' field.",
                                        path[index].lexeme.clone(),
                                        current_kind.clone().unwrap().borrow().name,
                                        field.lexeme.clone()
                                    );
                                }
                            },
                        }
                    }
                };
            }
        }

        match &current_kind {
            None => gpp_error!("Not have field with name."),
            Some(kind) => Ok(provider.query_type(TypeQuery::ByName(&kind.borrow().name))),
        }
    }

    fn visit_variable_expression(
        &self,
        name: &Token,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        // println!("Visit Variable");

        let variable = provider.look_at(name);

        match variable {
            None => Err(CompilationError::new(
                format!("The variable '{}' is not declared", name.lexeme).into(),
                Some(name.line),
            )),
            Some(s) => match s.kind {
                None => gpp_error!("The variable '{}' is not initialized.", name.lexeme),
                Some(desc) => Ok(desc),
            },
        }
    }

    fn visit_set_expression(
        &self,
        expression: &Expression,
        getter: &Token,
        value: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        todo!()
    }

    fn visit_call_expression(
        &self,
        callee: &Expression,
        paren: &Token,
        args: &Vec<Expression>,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        todo!()
    }

    fn visit_list_expression(
        &self,
        elements: &Vec<Rc<Expression>>,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        todo!()
    }

    fn visit_attribute(
        &self,
        name: &Token,
        args: &Vec<Rc<Expression>>,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        todo!()
    }

    fn visit_group_expression(
        &self,
        expression: &Expression,
        provider: &dyn SemanticProvider,
    ) -> Self::Output {
        todo!()
    }
}
