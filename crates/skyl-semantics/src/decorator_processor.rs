#![allow(clippy::result_large_err)]

use skyl_data::{
    BuiltinAttributeUsage, Decorator, Expression, FunctionPrototype, Literal, Token, TokenKind,
    ValueWrapper,
};
use skyl_driver::errors::{CompilationError, CompilationErrorKind};

use crate::{
    SemanticAnalyzer, attribute_processor::AttributeProcessor, attributes::OperatorAttribute,
    result::TyResult,
};

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

    pub(super) fn process_function(
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
                if let Err(e) =
                    self.process_one_attribute::<OperatorAttribute>(name, attribute, definition)
                {
                    self.report_error(e);
                }
            }

            _ => {
                self.report_error(CompilationError::with_span(
                    CompilationErrorKind::InvalidAttributeExpression {
                        msg: format!("Attribute processor for '{}' not found", attribute.name),
                    },
                    Some(name.line),
                    attribute.span,
                ));
            }
        }
        Ok(())
    }

    pub(crate) fn evaluate(&mut self, constant: &Expression) -> TyResult<ValueWrapper> {
        if let Expression::Literal(lt, _) = constant {
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
}
