use skyl_data::SourceFile;

use crate::errors::{CompilationError, CompilationErrorKind};

pub fn format_err(error: &CompilationError, file: &SourceFile) -> String {
    let mut output = String::new();

    if let (Some(span), Some(line)) = (error.span, error.line) {
        let lines: Vec<&str> = file.content.lines().collect();
        let line_idx = line - 1;
        let source_line = lines.get(line_idx).unwrap_or(&"");

        let content_before = &file.content[..span.start];
        let last_line_start = content_before.rfind('\n').map(|pos| pos + 1).unwrap_or(0);
        let col_start = span.start - last_line_start;
        let underline_len = span.end.saturating_sub(span.start).max(1);

        let max_line_digits = lines.len().to_string().len();
        let line_number_pad = max_line_digits.max(2);

        output += &format!("\x1b[31mError\x1b[0m: {}\n", stringify_error(error));
        output += &format!(
            " {arrow:>width$} {file}:{line}:{col}\n",
            arrow = "-->",
            file = file.name,
            line = line,
            col = col_start + 1,
            width = line_number_pad + 1
        );
        output += &format!(" {bar:>width$}\n", bar = "|", width = line_number_pad + 1);
        output += &format!(
            "{line_number:>width$} | {line_content}\n",
            line_number = line,
            line_content = source_line,
            width = line_number_pad
        );
        output += &format!(" {bar:>width$} ", bar = "|", width = line_number_pad + 1);
        output += &format!(
            "\x1b[31m{marker:>offset$}{carets}\x1b[0m\n",
            marker = "",
            offset = col_start,
            carets = "^".repeat(underline_len)
        );
    } else if let Some(line) = error.line {
        output += &format!(
            "\x1b[31mError\x1b[0m: {}. At line {}.\n",
            stringify_error(error),
            line
        );
    } else {
        output += &format!("\x1b[31mError\x1b[0m: {}.", stringify_error(error));
    }

    output
}

fn stringify_error(err: &CompilationError) -> String {
    match &err.kind {
        CompilationErrorKind::IllegalCharacter(c) => format_illegal_character_err(c),
        CompilationErrorKind::InvalidNativeDeclaration => format_invalid_native_declaration(),
        CompilationErrorKind::UnsupportedFeature { feature } => format_unsupported_feature(feature),
        CompilationErrorKind::InvalidBuiltinDeclaration => todo!(),
        CompilationErrorKind::InvalidKeyword { keyword } => todo!(),
        CompilationErrorKind::InvalidAssignmentTarget => todo!(),
        CompilationErrorKind::ArgumentLimitOverflow => todo!(),
        CompilationErrorKind::UnexpectedToken { token } => format_unexpected_token(token),
        CompilationErrorKind::ExpectedToken {
            expect,
            found,
            after,
        } => format_expected_token(expect, found, after),
        CompilationErrorKind::ExpectedConstruction { expect, found } => todo!(),
        CompilationErrorKind::MissingMainFunction => todo!(),
        CompilationErrorKind::DuplicatedVariable { name, previous } => {
            format_duplicated_variable(name, previous)
        }
        CompilationErrorKind::UsingVoidToAssignVariableOrParam => todo!(),
        CompilationErrorKind::DuplicatedTypeDefinition { r#type } => todo!(),
        CompilationErrorKind::DuplicatedField { field } => todo!(),
        CompilationErrorKind::MissingConstruction { construction } => todo!(),
        CompilationErrorKind::InvalidStatementScope { statement } => todo!(),
        CompilationErrorKind::DepthError { msg } => todo!(),
        CompilationErrorKind::InvalidStatementUsage { error } => todo!(),
        CompilationErrorKind::ExpectType {
            expect,
            found,
            compiler_msg,
        } => format_expect_type(expect, found, compiler_msg),
        CompilationErrorKind::ExpectReturnType { expect, found } => todo!(),
        CompilationErrorKind::UnexpectedReturnValue { found } => todo!(),
        CompilationErrorKind::TypeAssertion { msg } => todo!(),
        CompilationErrorKind::UsageOfNotRequiredStatement { statement, place } => todo!(),
        CompilationErrorKind::DuplicatedNativeFunction { name } => todo!(),
        CompilationErrorKind::NotFoundType { name } => todo!(),
        CompilationErrorKind::NotFoundField { r#type, field } => todo!(),
        CompilationErrorKind::ModuleNotFound { path } => todo!(),
        CompilationErrorKind::ModuleAccessDenied { path, full_path } => todo!(),
        CompilationErrorKind::ModuleReadError {
            path,
            error,
            full_path,
        } => todo!(),
        CompilationErrorKind::InvalidLiteral { line } => todo!(),
        CompilationErrorKind::InvalidPostfixOperatorUsage { msg } => todo!(),
        CompilationErrorKind::InvalidExpression { msg } => todo!(),
        CompilationErrorKind::InexistentType { r#type } => format_inexistent_type(r#type),
        CompilationErrorKind::UsageOfNotInferredVariable { name } => todo!(),
        CompilationErrorKind::NotFoundArchetypeMask(not_found_archetype_mask) => todo!(),
        CompilationErrorKind::UsageOfUndeclaredVariable { name } => {
            format_usage_of_undeclared_variable(name)
        }
    }
}

fn format_inexistent_type(r#type: &str) -> String {
    format!("Type '{}' is not declared here", r#type)
}

fn format_expect_type(expect: &str, found: &str, compiler_msg: &Option<String>) -> String {
    match compiler_msg {
        None => format!("Expect '{}' instance, but got '{}'.", expect, found),
        Some(msg) => format!(
            "Expect '{}' instance, but got '{}'. Compiler message: {}",
            expect, found, msg
        ),
    }
}

fn format_usage_of_undeclared_variable(name: &str) -> String {
    format!("The variable '{}' are not declared here.", name)
}

fn format_duplicated_variable(name: &str, previous: &usize) -> String {
    format!(
        "The name '{}' was previous declared in current scope (at line {}).",
        name, previous
    )
}

fn format_unexpected_token(token: &str) -> String {
    format!("Unexpected token '{}'.", token)
}

fn format_expected_token(expect: &str, found: &str, after: &Option<String>) -> String {
    match after {
        None => {
            format!("Expect {}, but got {}.", expect, found)
        }
        Some(a) => format!("Expect {} after {}, but got {}.", expect, a, found),
    }
}

fn format_unsupported_feature(feature: &str) -> String {
    format!("Unsupported feature '{}'.", feature)
}

fn format_invalid_native_declaration() -> String {
    return "Invalid use of 'native' keyword.".into();
}

fn format_illegal_character_err(c: &char) -> String {
    match c {
        '\n' => "\\n".into(),
        '\r' => "\\r".into(),
        '\t' => "\\t".into(),
        _ => c.to_string(),
    }
}
