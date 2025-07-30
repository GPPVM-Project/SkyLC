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
                &format!("Invalid keyword '{keyword}'")
            }
            CompilationErrorKind::InvalidAssignmentTarget => "Invalid assignment target",
            CompilationErrorKind::ArgumentLimitOverflow => "Argument limit overflow",
            CompilationErrorKind::UnexpectedToken { .. } => "Unexpected token",
            CompilationErrorKind::ExpectedToken { .. } => "Expected token",
            CompilationErrorKind::ExpectedConstruction { .. } => "Expected construction",
            CompilationErrorKind::MissingMainFunction => "Missing main function",
            CompilationErrorKind::DuplicatedVariable { .. } => "Duplicated variable",
            CompilationErrorKind::UsingVoidToAssignVariableOrParam => "Void used in assignment",
            CompilationErrorKind::DuplicatedTypeDefinition { .. } => "Duplicated type definition",
            CompilationErrorKind::DuplicatedField { .. } => "Duplicated field",
            CompilationErrorKind::DuplicatedDefinition { .. } => "Duplicated definition",
            CompilationErrorKind::MissingConstruction { .. } => "Missing construction",
            CompilationErrorKind::InvalidStatementScope { .. } => "Invalid statement scope",
            CompilationErrorKind::DepthError { .. } => "Depth error",
            CompilationErrorKind::InvalidStatementUsage { .. } => "Invalid statement usage",
            CompilationErrorKind::ExpectType { .. } => "Type mismatch",
            CompilationErrorKind::ExpectReturnType { .. } => "Return type mismatch",
            CompilationErrorKind::UnexpectedReturnValue { .. } => "Unexpected return value",
            CompilationErrorKind::TypeAssertion { .. } => "Type assertion error",
            CompilationErrorKind::UsageOfNotRequiredStatement { .. } => "Unused statement",
            CompilationErrorKind::DuplicatedNativeFunction { .. } => "Duplicated native function",
            CompilationErrorKind::NotFoundType { .. } => "Type not found",
            CompilationErrorKind::NotFoundField { .. } => "Field not found",
            CompilationErrorKind::SymbolNotFound { .. } => "Symbol not found",
            CompilationErrorKind::ModuleNotFound { .. } => "Module not found",
            CompilationErrorKind::ModuleAccessDenied { .. } => "Module access denied",
            CompilationErrorKind::ModuleReadError { .. } => "Module read error",
            CompilationErrorKind::UnsupportedFeature { .. } => "Unsupported feature",
            CompilationErrorKind::InvalidLiteral { .. } => "Invalid literal",
            CompilationErrorKind::InvalidPostfixOperatorUsage { .. } => {
                "Invalid postfix operator usage"
            }
            CompilationErrorKind::InvalidExpression { .. } => "Invalid expression",
            CompilationErrorKind::InvalidAttributeExpression { .. } => {
                "Invalid attribute expression"
            }
            CompilationErrorKind::InexistentType { .. } => "Inexistent type",
            CompilationErrorKind::NotFoundArchetypeMask(_) => "Archetype mask not found",
            CompilationErrorKind::UsageOfNotInferredVariable { .. } => "Variable not inferred",
            CompilationErrorKind::UsageOfUndeclaredVariable { .. } => "Undeclared variable",
            CompilationErrorKind::InvalidOperatorOverload(_) => "Invalid operator overload",
            CompilationErrorKind::MismatchAttrbuteArgument { .. } => "Attribute argument mismatch",
            CompilationErrorKind::InvalidConstantEvaluation(_) => "Invalid constant evaluation",
            CompilationErrorKind::OperatorOverloadNotFound { .. } => "Operator overload not found",
            CompilationErrorKind::MismatchArgumentCount { .. } => "Argument count mismatch",
            CompilationErrorKind::AssignTypeError { .. } => "Assign type error",
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
