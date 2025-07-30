use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{
    errors::{CompilationError, CompilationErrorKind},
    format_err::{hintify_error, stringify_error},
};

#[derive(Debug, Error, Clone, Diagnostic)]
#[error("{message}")]
pub struct MietteCompilationError {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("{label}")]
    pub span: Option<SourceSpan>,
    pub label: String,
    #[help]
    pub hint: Option<String>,
}

impl From<&CompilationError> for Option<MietteCompilationError> {
    fn from(error: &CompilationError) -> Option<MietteCompilationError> {
        let file_path = error.file.as_ref()?;
        let span = error.span?;
        let content = std::fs::read_to_string(file_path).ok()?;

        let offset = span.start;
        let len = span.end.saturating_sub(span.start);
        let label = match &error.kind {
            CompilationErrorKind::FunctionMayNotReturn { .. } => "Missing return statement",
            CompilationErrorKind::IllegalCharacter(_) => "Illegal character",
            CompilationErrorKind::InvalidNativeDeclaration => "Invalid native declaration",
            CompilationErrorKind::InvalidBuiltinDeclaration => "Invalid builtin declaration",
            CompilationErrorKind::InvalidKeyword { keyword } => {
                &format!("Invalid keyword '{}'", keyword)
            }
            CompilationErrorKind::InvalidAssignmentTarget => "Invalid assignment target",
            CompilationErrorKind::ArgumentLimitOverflow => "Argument limit overflow",
            CompilationErrorKind::UnexpectedToken { token } => "Unexpected token",
            CompilationErrorKind::ExpectedToken { expect, found, .. } => "Expected token",
            CompilationErrorKind::ExpectedConstruction { expect, found } => "Expected construction",
            CompilationErrorKind::MissingMainFunction => "Missing main function",
            CompilationErrorKind::DuplicatedVariable { name, previous } => "Duplicated variable",
            CompilationErrorKind::UsingVoidToAssignVariableOrParam => "Void used in assignment",
            CompilationErrorKind::DuplicatedTypeDefinition { r#type } => {
                "Duplicated type definition"
            }
            CompilationErrorKind::DuplicatedField { field } => "Duplicated field",
            CompilationErrorKind::DuplicatedDefinition { kind, .. } => "Duplicated definition",
            CompilationErrorKind::MissingConstruction { construction } => "Missing construction",
            CompilationErrorKind::InvalidStatementScope { statement } => "Invalid statement scope",
            CompilationErrorKind::DepthError { msg } => "Depth error",
            CompilationErrorKind::InvalidStatementUsage { error } => "Invalid statement usage",
            CompilationErrorKind::ExpectType { expect, found, .. } => "Type mismatch",
            CompilationErrorKind::ExpectReturnType { expect, found } => "Return type mismatch",
            CompilationErrorKind::UnexpectedReturnValue { found } => "Unexpected return value",
            CompilationErrorKind::TypeAssertion { msg } => "Type assertion error",
            CompilationErrorKind::UsageOfNotRequiredStatement { statement, place } => {
                "Unused statement"
            }
            CompilationErrorKind::DuplicatedNativeFunction { name } => "Duplicated native function",
            CompilationErrorKind::NotFoundType { name } => "Type not found",
            CompilationErrorKind::NotFoundField { r#type, field } => "Field not found",
            CompilationErrorKind::SymbolNotFound { symbol_kind, .. } => "Symbol not found",
            CompilationErrorKind::ModuleNotFound { path } => "Module not found",
            CompilationErrorKind::ModuleAccessDenied { path, .. } => "Module access denied",
            CompilationErrorKind::ModuleReadError { path, error, .. } => "Module read error",
            CompilationErrorKind::UnsupportedFeature { feature } => "Unsupported feature",
            CompilationErrorKind::InvalidLiteral { line } => "Invalid literal",
            CompilationErrorKind::InvalidPostfixOperatorUsage { msg } => {
                "Invalid postfix operator usage"
            }
            CompilationErrorKind::InvalidExpression { msg } => "Invalid expression",
            CompilationErrorKind::InvalidAttributeExpression { msg } => {
                "Invalid attribute expression"
            }
            CompilationErrorKind::InexistentType { r#type } => "Inexistent type",
            CompilationErrorKind::NotFoundArchetypeMask(_) => "Archetype mask not found",
            CompilationErrorKind::UsageOfNotInferredVariable { name } => "Variable not inferred",
            CompilationErrorKind::UsageOfUndeclaredVariable { name } => "Undeclared variable",
            CompilationErrorKind::InvalidOperatorOverload(_) => "Invalid operator overload",
            CompilationErrorKind::MismatchAttrbuteArgument { arg, .. } => {
                "Attribute argument mismatch"
            }
            CompilationErrorKind::InvalidConstantEvaluation(_) => "Invalid constant evaluation",
            CompilationErrorKind::OperatorOverloadNotFound { operator, .. } => {
                "Operator overload not found"
            }
            CompilationErrorKind::MismatchArgumentCount {
                expected, found, ..
            } => "Argument count mismatch",
            CompilationErrorKind::AssignTypeError { kind, .. } => "Assign type error",
            CompilationErrorKind::MainFunctionReturnKind => "Invalid main function return kind",
        };

        Some(MietteCompilationError {
            message: stringify_error(error),
            src: NamedSource::new(file_path.clone(), content),
            span: Some((offset, len).into()),
            label: label.to_string(),
            hint: Some(hintify_error(error)),
        })
    }
}
