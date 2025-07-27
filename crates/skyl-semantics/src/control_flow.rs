use skyl_data::AnnotatedStatement;
use skyl_driver::errors::{CompilationError, CompilationErrorKind};

use crate::{SemanticAnalyzer, result::TyResult};

impl SemanticAnalyzer {
    pub(crate) fn analyze_control_flow(&mut self, stmt: &AnnotatedStatement) {
        if let AnnotatedStatement::Function(proto, body, _, _) = stmt {
            if !proto.return_kind.borrow().is_void() {
                match self.returns_in_all_paths(body) {
                    Ok(false) => {
                        self.report_error(CompilationError::with_span(
                            CompilationErrorKind::FunctionMayNotReturn {
                                function_kind: "function".into(),
                                name: proto.name.clone(),
                            },
                            Some(body.line()),
                            body.span(),
                        ));
                    }
                    Err(e) => self.report_error(e),
                    Ok(true) => {}
                }
            }
        }
    }

    fn returns_in_all_paths(&self, stmt: &AnnotatedStatement) -> TyResult<bool> {
        match stmt {
            AnnotatedStatement::Return(_, _, _) => Ok(true),
            AnnotatedStatement::Scope(stmts, _, _) => {
                for s in stmts {
                    if self.returns_in_all_paths(s)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            AnnotatedStatement::If(_, _, then_branch, maybe_else, _, _) => {
                let then_returns = self.returns_in_all_paths(then_branch)?;
                let else_returns = match maybe_else {
                    Some(else_stmt) => self.returns_in_all_paths(else_stmt)?,
                    None => false,
                };
                Ok(then_returns && else_returns)
            }
            AnnotatedStatement::While(_, _, _, _) => Ok(false),
            _ => Ok(false),
        }
    }
}
