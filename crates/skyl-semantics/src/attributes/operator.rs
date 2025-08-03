use std::rc::Rc;

use skyl_data::{
    BuiltinAttributeUsage, CoersionKind, Expression, FunctionPrototype, Operator, Span, Token,
};
use skyl_driver::errors::{CompilationError, CompilationErrorKind};

use crate::{SemanticAnalyzer, attribute_processor::AttributeProcessor, result::TyResult};

pub const ADD_NAME: &str = "+";
pub const SUB_NAME: &str = "-";
pub const MULTIPLY_NAME: &str = "*";
pub const DIV_NAME: &str = "/";

impl SemanticAnalyzer {
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

impl AttributeProcessor for SemanticAnalyzer {
    fn process_one_attribute<OperatorAttribute>(
        &mut self,
        location: &Token,
        attribute: BuiltinAttributeUsage,
        definition: &FunctionPrototype,
    ) -> TyResult<()> {
        if definition.arity != 2 {
            return Err(CompilationError::with_span(
                CompilationErrorKind::InvalidOperatorOverload(
                    "Operator overload must be two arguments".into(),
                ),
                Some(location.line),
                attribute.span,
            ));
        }

        let first = self.resolve_expr_type(&definition.params[0].kind)?;
        let second = self.resolve_expr_type(&definition.params[1].kind)?;

        self.create_operator_overload(
            location,
            &attribute.args[0],
            first.borrow().id,
            second.borrow().id,
            definition.name.clone(),
        )?;

        Ok(())
    }
}
