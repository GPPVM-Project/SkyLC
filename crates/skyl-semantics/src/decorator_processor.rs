use std::rc::Rc;

use skyl_data::{
    BuiltinAttributeUsage, CoersionKind, Decorator, Expression, FunctionPrototype, Literal,
    Operator, Span, Token, TokenKind, ValueWrapper,
};
use skyl_driver::{
    errors::{CompilationError, CompilationErrorKind},
    gpp_error,
};

use crate::{SemanticAnalyzer, result::TyResult};

pub const ADD_NAME: &'static str = "+";
pub const SUB_NAME: &'static str = "-";
pub const MULTIPLY_NAME: &'static str = "*";
pub const DIV_NAME: &'static str = "/";

impl SemanticAnalyzer {
    pub(super) fn process_internal_definition(
        &mut self,
        name: &Token,
        definition: &FunctionPrototype,
    ) -> TyResult<()> {
        if self.current_decorator.attributes.is_empty() {
            return Ok(());
        }

        let decorator = self.current_decorator.clone();
        self.current_decorator = Decorator::new(Vec::new());

        for attribute in decorator.attributes {
            self.process_attribute(name, attribute, definition)?;
        }

        Ok(())
    }

    pub(crate) fn process_attribute(
        &mut self,
        name: &Token,
        attribute: BuiltinAttributeUsage,
        definition: &FunctionPrototype,
    ) -> TyResult<()> {
        match attribute.name.as_str() {
            "operator" => {
                if definition.arity != 2 {
                    return Err(CompilationError::with_span(
                        CompilationErrorKind::InvalidOperatorOverload(
                            "Operator overload must be two arguments".into(),
                        ),
                        Some(name.line),
                        attribute.span,
                    ));
                }

                let first = self.resolve_expr_type(&definition.params[0].kind)?;
                let second = self.resolve_expr_type(&definition.params[1].kind)?;

                self.create_operator_overload(
                    name,
                    &attribute.args[0],
                    first.borrow().id,
                    second.borrow().id,
                    definition.name.clone(),
                )?;
            }

            _ => {
                return Err(CompilationError::with_span(
                    CompilationErrorKind::InvalidAttributeExpression {
                        msg: format!("Attribute '{}' not found", name.lexeme),
                    },
                    Some(name.line),
                    attribute.span,
                ));
            }
        }
        Ok(())
    }

    pub(crate) fn create_operator_overload(
        &mut self,
        location: &Token,
        operator: &Rc<Expression>,
        from: u32,
        to: u32,
        function_name: String,
    ) -> TyResult<()> {
        let operator_name = self.evaluate(operator)?.as_string().unwrap();
        let operator_kind = self.operator_name_to_kind(&operator_name, location)?;

        if self.symbol_table.operators.contains_key(&Operator {
            other: to,
            this: from,
            kind: operator_kind,
        }) {
            return Err(CompilationError::with_span(
                CompilationErrorKind::DuplicatedDefinition {
                    kind: format!(
                        "operator `{} {} {}` overload",
                        self.get_static_kind_by_id(from).borrow().name,
                        self.coersion_to_string(&operator_name, operator.line(), operator.span())?,
                        self.get_static_kind_by_id(to).borrow().name,
                    ),
                    definition: format!(
                        "{operator_name}_{}_{}",
                        self.get_static_kind_by_id(from).borrow().name,
                        self.get_static_kind_by_id(to).borrow().name,
                    ),
                    target: self.get_static_kind_by_id(from).borrow().name.clone(),
                },
                Some(operator.line()),
                operator.span(),
            ));
        } else {
            self.symbol_table.operators.insert(
                Operator {
                    this: from,
                    other: to,
                    kind: operator_kind,
                },
                function_name,
            );
        }

        Ok(())
    }

    fn coersion_to_string(&self, operator: &str, line: usize, span: Span) -> TyResult<String> {
        match operator {
            ADD_NAME => Ok("+".into()),
            SUB_NAME => Ok("-".into()),
            MULTIPLY_NAME => Ok("*".into()),
            DIV_NAME => Ok("-".into()),

            _ => Err(CompilationError::with_span(
                CompilationErrorKind::MismatchAttrbuteArgument {
                    arg: operator.into(),
                    accepted: vec![
                        ADD_NAME.into(),
                        SUB_NAME.into(),
                        MULTIPLY_NAME.into(),
                        DIV_NAME.into(),
                    ],
                },
                Some(line),
                span,
            )),
        }
    }

    fn evaluate(&mut self, constant: &Rc<Expression>) -> TyResult<ValueWrapper> {
        if let Expression::Literal(lt, _) = constant.as_ref() {
            if let TokenKind::Literal(l) = lt.kind {
                match l {
                    Literal::String => return Ok(ValueWrapper::String(lt.lexeme.clone())),
                    Literal::Int => {
                        return Ok(ValueWrapper::Int(lt.lexeme.parse::<i32>().unwrap()));
                    }
                    Literal::Float => {
                        return Ok(ValueWrapper::Float(lt.lexeme.parse::<f32>().unwrap()));
                    }
                    Literal::Boolean => {
                        return Ok(ValueWrapper::Boolean(lt.lexeme.parse::<bool>().unwrap()));
                    }
                }
            }

            return Err(CompilationError::with_span(
                CompilationErrorKind::InvalidConstantEvaluation(
                    "Cannot evaluate this expression as constant",
                ),
                Some(constant.line()),
                constant.span(),
            ));
        }

        Err(CompilationError::with_span(
            CompilationErrorKind::InvalidConstantEvaluation(
                "Cannot evaluate this expression as constant",
            ),
            Some(constant.line()),
            constant.span(),
        ))
    }

    pub(crate) fn operator_name_to_kind(
        &self,
        operator_name: &str,
        location: &Token,
    ) -> TyResult<CoersionKind> {
        match operator_name {
            MULTIPLY_NAME => Ok(CoersionKind::Mul),
            DIV_NAME => Ok(CoersionKind::Div),
            ADD_NAME => Ok(CoersionKind::Add),
            SUB_NAME => Ok(CoersionKind::Sub),
            _ => Err(CompilationError::with_span(
                CompilationErrorKind::MismatchAttrbuteArgument {
                    arg: operator_name.to_string(),
                    accepted: vec![
                        ADD_NAME.into(),
                        SUB_NAME.into(),
                        MULTIPLY_NAME.into(),
                        DIV_NAME.into(),
                    ],
                },
                Some(location.line),
                location.span,
            )),
        }
    }

    pub(crate) fn operator_kind_to_name(&self, operator: &CoersionKind) -> &'static str {
        match operator {
            CoersionKind::Mul => MULTIPLY_NAME,
            CoersionKind::Div => DIV_NAME,
            CoersionKind::Add => ADD_NAME,
            CoersionKind::Sub => SUB_NAME,
        }
    }
}
