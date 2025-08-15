use skyl_data::{AnnotatedExpression, CompileTimeValue, OperatorKind};

use crate::IRGenerator;

impl IRGenerator {
    pub(crate) fn try_evaluate_constant_expression(
        &self,
        expr: &AnnotatedExpression,
    ) -> Option<CompileTimeValue> {
        match expr {
            AnnotatedExpression::Literal(token, descriptor) => {
                Some(self.get_constant(token, &descriptor.borrow()))
            }
            AnnotatedExpression::Variable(name, descriptor) => {
                for scope in self.constant_values.iter().rev() {
                    if let Some(value) = scope.get(&name.lexeme) {
                        return Some(value.clone());
                    }
                }
                None
            }
            // AnnotatedExpression::Arithmetic(left, op, right, _) => {
            //     let left_val = self.try_evaluate_constant_expression(left)?;
            //     let right_val = self.try_evaluate_constant_expression(right)?;

            //     match op.kind {
            //         OperatorKind::Plus => match (left_val, right_val) {
            //             (CompileTimeValue::Int(a), CompileTimeValue::Int(b)) => {
            //                 Some(CompileTimeValue::Int(a + b))
            //             }
            //             (CompileTimeValue::Float(a), CompileTimeValue::Float(b)) => {
            //                 Some(CompileTimeValue::Float(a + b))
            //             }
            //             _ => None,
            //         },
            //         OperatorKind::Minus => match (left_val, right_val) {
            //             (CompileTimeValue::Int(a), CompileTimeValue::Int(b)) => {
            //                 Some(CompileTimeValue::Int(a - b))
            //             }
            //             (CompileTimeValue::Float(a), CompileTimeValue::Float(b)) => {
            //                 Some(CompileTimeValue::Float(a - b))
            //             }
            //             _ => None,
            //         },
            //         OperatorKind::Star => match (left_val, right_val) {
            //             (CompileTimeValue::Int(a), CompileTimeValue::Int(b)) => {
            //                 Some(CompileTimeValue::Int(a * b))
            //             }
            //             (CompileTimeValue::Float(a), CompileTimeValue::Float(b)) => {
            //                 Some(CompileTimeValue::Float(a * b))
            //             }
            //             _ => None,
            //         },
            //         OperatorKind::Slash => match (left_val, right_val) {
            //             (CompileTimeValue::Int(a), CompileTimeValue::Int(b)) => {
            //                 Some(CompileTimeValue::Int(a / b))
            //             }
            //             (CompileTimeValue::Float(a), CompileTimeValue::Float(b)) => {
            //                 Some(CompileTimeValue::Float(a / b))
            //             }
            //             _ => None,
            //         },
            //         _ => None,
            //     }
            // }
            ,
            _ => None,
        }
    }
}
