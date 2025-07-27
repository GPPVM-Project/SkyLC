use std::{fmt::format, path::Path};

use skyl_data::SourceFile;

use crate::errors::{CompilationError, CompilationErrorKind};

pub fn format_err(error: &CompilationError, file: &SourceFile) -> String {
    let mut output = String::new();

    if let (Some(span), Some(line)) = (error.span, error.line) {
        let lines: Vec<&str> = file.content.lines().collect();
        let line_idx = line - 1;
        let source_line = lines.get(line_idx).unwrap_or(&"");

        // let content_before = &file.content[0..20];
        let content_before = &file.content[..span.start];
        let last_line_start = content_before.rfind('\n').map(|pos| pos + 1).unwrap_or(0);
        let col_start = span.start - last_line_start;
        let underline_len = span.end.saturating_sub(span.start).max(1);

        let max_line_digits = lines.len().to_string().len();
        let line_number_pad = max_line_digits.max(2);

        output += &format!("\x1b[31mError\x1b[0m: {}\n", stringify_error(error));
        let relative_path = file
            .path
            .strip_prefix(std::env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf()))
            .unwrap_or_else(|_| file.path.as_path());

        output += &format!(
            " {arrow:>width$} {file}:{line}:{col}\n",
            arrow = "-->",
            file = relative_path.display(),
            line = line,
            col = col_start + 1,
            width = line_number_pad + 1
        );
        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m\n",
            bar = "|",
            width = line_number_pad + 1
        );
        output += &format!(
            "\x1b[34m{line_number:>width$}\x1b[0m \x1b[34m|\x1b[0m {line_content}\n",
            line_number = line,
            line_content = source_line,
            width = line_number_pad
        );
        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m ",
            bar = "|",
            width = line_number_pad + 1
        );
        output += &format!(
            "\x1b[31m{marker:>offset$}{carets}\x1b[0m\n",
            marker = "",
            offset = col_start,
            carets = "^".repeat(underline_len)
        );
        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m\n",
            bar = "|",
            width = line_number_pad + 1
        );
        output += &format!(
            "\x1b[34m{bar:>width$}\x1b[0m \x1b[92mHint:\x1b[0m {hint}.\n",
            bar = "╰─",
            width = line_number_pad + 3,
            hint = hintify_error(&error)
        );

        let notes = notefy_error(error);
        let note_count = notes.len();

        for i in 0..note_count {
            let note_bar = if i == note_count - 1 {
                "└─"
            } else {
                "├─"
            };
            output += &format!(
                "\x1b[34m{bar:>width$}\x1b[0m \x1b[93mNote:\x1b[0m {hint}.\n",
                bar = note_bar,
                width = line_number_pad + 6,
                hint = notes[i]
            );
        }
    } else if let Some(line) = error.line {
        output += &format!(
            "\x1b[31mError\x1b[0m: {}. At line {}.\n",
            stringify_error(error),
            line
        );
    } else {
        output += &format!("\x1b[31mError\x1b[0m: {}.\n", stringify_error(error));
    }

    output
}

fn notefy_error(error: &CompilationError) -> Vec<String> {
    match &error.kind {
        CompilationErrorKind::IllegalCharacter(_) => vec![format!("For Skyl constructions, use ASCII characters and valid symbols, other characters can be used inside strings"),
                                            format!("For more informations consult the Skyl documentation")],
        CompilationErrorKind::InvalidNativeDeclaration => todo!(),
        CompilationErrorKind::InvalidBuiltinDeclaration => todo!(),
        CompilationErrorKind::InvalidKeyword { keyword } => todo!(),
        CompilationErrorKind::InvalidAssignmentTarget => todo!(),
        CompilationErrorKind::ArgumentLimitOverflow => todo!(),
        CompilationErrorKind::UnexpectedToken { token } => vec![],
        CompilationErrorKind::ExpectedToken {
                                                expect,
                                                found,
                                                after,
                                            } => vec![],
        CompilationErrorKind::ExpectedConstruction { expect, found } => todo!(),
        CompilationErrorKind::MissingMainFunction => todo!(),
        CompilationErrorKind::DuplicatedVariable { name, previous } => todo!(),
        CompilationErrorKind::UsingVoidToAssignVariableOrParam => todo!(),
        CompilationErrorKind::DuplicatedTypeDefinition { r#type } => todo!(),
        CompilationErrorKind::DuplicatedField { field } => todo!(),
        CompilationErrorKind::MissingConstruction { construction } => todo!(),
        CompilationErrorKind::InvalidStatementScope { statement } => todo!(),
        CompilationErrorKind::DepthError { msg } => vec![],
        CompilationErrorKind::InvalidStatementUsage { error } => vec![],
        CompilationErrorKind::ExpectType {
                                                expect,
                                                found,
                                                compiler_msg,
                                            } => vec![],
        CompilationErrorKind::ExpectReturnType { expect, found } => vec![],
        CompilationErrorKind::UnexpectedReturnValue { found } => todo!(),
        CompilationErrorKind::TypeAssertion { msg } => todo!(),
        CompilationErrorKind::UsageOfNotRequiredStatement { statement, place } => todo!(),
        CompilationErrorKind::DuplicatedNativeFunction { name } => todo!(),
        CompilationErrorKind::NotFoundType { name } => todo!(),
        CompilationErrorKind::NotFoundField { r#type, field } => vec![],
        CompilationErrorKind::ModuleNotFound { path } => vec![],
        CompilationErrorKind::ModuleAccessDenied { path, full_path } => todo!(),
        CompilationErrorKind::ModuleReadError {
                                                path,
                                                full_path,
                                                error,
                                            } => todo!(),
        CompilationErrorKind::UnsupportedFeature { feature } => vec![],
        CompilationErrorKind::InvalidLiteral { line } => todo!(),
        CompilationErrorKind::InvalidPostfixOperatorUsage { msg } => todo!(),
        CompilationErrorKind::InvalidExpression { msg } => vec![],
        CompilationErrorKind::InexistentType { r#type } => vec![],
        CompilationErrorKind::NotFoundArchetypeMask(not_found_archetype_mask) => todo!(),
        CompilationErrorKind::UsageOfNotInferredVariable { name } => todo!(),
        CompilationErrorKind::UsageOfUndeclaredVariable { name } => vec![],
        CompilationErrorKind::InvalidAttributeExpression { msg } => vec![],
        CompilationErrorKind::InvalidOperatorOverload(_) => vec![],
        CompilationErrorKind::MismatchAttrbuteArgument { arg, accepted } => vec![],
        CompilationErrorKind::InvalidConstantEvaluation(_) => todo!(),
        CompilationErrorKind::OperatorOverloadNotFound { this, other, operator } => {
                        vec![format!("#[coersion(op)]"), format!("def overload_name(self: {}, other: {}) -> SomeKind", this, other)]
                },
        CompilationErrorKind::DuplicatedDefinition { kind, definition, target } => vec![],
        CompilationErrorKind::SymbolNotFound { symbol_kind, symbol_name } => vec![],
        CompilationErrorKind::MismatchArgumentCount { expected, found, function_name } => vec![],
CompilationErrorKind::AssignTypeError { kind, found } => vec![],
    }
}

fn hintify_error(err: &CompilationError) -> String {
    match &err.kind {
        CompilationErrorKind::SymbolNotFound {
                        symbol_kind,
                        symbol_name,
            } => format!(
                "Consider to creating a `{}` {} declaration",
                symbol_name, symbol_kind
            ),
        CompilationErrorKind::IllegalCharacter(c) => {
                format!("Consider removing '{c}' from source code")
            }
        CompilationErrorKind::InvalidNativeDeclaration => format_invalid_native_declaration(),
        CompilationErrorKind::UnsupportedFeature { feature } => format_unsupported_feature(feature),
        CompilationErrorKind::InvalidBuiltinDeclaration => todo!(),
        CompilationErrorKind::InvalidKeyword { keyword } => todo!(),
        CompilationErrorKind::InvalidAssignmentTarget => todo!(),
        CompilationErrorKind::ArgumentLimitOverflow => todo!(),
        CompilationErrorKind::UnexpectedToken { token } => format!("Consider removing this token"),
        CompilationErrorKind::ExpectedToken {
                expect,
                found,
                after,
            } => hint_expected_token(expect, found, after),
        CompilationErrorKind::ExpectedConstruction { expect, found } => {
                format_expected_construction(expect, found)
            }
        CompilationErrorKind::MissingMainFunction => todo!(),
        CompilationErrorKind::DuplicatedVariable { name, previous } => {
                format_duplicated_variable(name, previous)
            }
        CompilationErrorKind::UsingVoidToAssignVariableOrParam => todo!(),
        CompilationErrorKind::DuplicatedTypeDefinition { r#type } => todo!(),
        CompilationErrorKind::DuplicatedField { field } => format_duplicated_field(field),
        CompilationErrorKind::MissingConstruction { construction } => todo!(),
        CompilationErrorKind::InvalidStatementScope { statement } => todo!(),
        CompilationErrorKind::DepthError { msg } => {
                format!("Move the declaration to other scope level.")
            }
        CompilationErrorKind::InvalidStatementUsage { error } => {
                format!("Consider removing the statement")
            }
        CompilationErrorKind::ExpectType {
                expect,
                found,
                compiler_msg,
            } => format_expect_type(expect, found, compiler_msg),
        CompilationErrorKind::ExpectReturnType { expect, found } => {
                format!("Consider change the return type of the function or returned value")
            }
        CompilationErrorKind::UnexpectedReturnValue { found } => todo!(),
        CompilationErrorKind::TypeAssertion { msg } => todo!(),
        CompilationErrorKind::UsageOfNotRequiredStatement { statement, place } => todo!(),
        CompilationErrorKind::DuplicatedNativeFunction { name } => todo!(),
        CompilationErrorKind::NotFoundType { name } => todo!(),
        CompilationErrorKind::NotFoundField { r#type, field } => {
                format!("Consider removing `{}.{}` from your code.", r#type, field)
            }
        CompilationErrorKind::ModuleNotFound { path } => format!(
                "Consider to create module '{}' before use it",
                path.join(".")
            ),
        CompilationErrorKind::ModuleAccessDenied { path, full_path } => todo!(),
        CompilationErrorKind::ModuleReadError {
                path,
                error,
                full_path,
            } => todo!(),
        CompilationErrorKind::InvalidLiteral { line } => todo!(),
        CompilationErrorKind::InvalidPostfixOperatorUsage { msg } => todo!(),
        CompilationErrorKind::InvalidExpression { msg } => {
                format!("Consider removing this expression")
            }
        CompilationErrorKind::InexistentType { r#type } => {
                format!("Consider declare the '{}' type before use it", r#type)
            }
        CompilationErrorKind::UsageOfNotInferredVariable { name } => todo!(),
        CompilationErrorKind::NotFoundArchetypeMask(not_found_archetype_mask) => todo!(),
        CompilationErrorKind::UsageOfUndeclaredVariable { name } => {
                format_usage_of_undeclared_variable(name)
            }
        CompilationErrorKind::InvalidAttributeExpression { msg } => {
                format!("Consider change or remove the attribute declaration")
            }
        CompilationErrorKind::InvalidOperatorOverload(msg) => {
                format!("Consider changing function structure to match with desired overload.")
            }
        CompilationErrorKind::MismatchAttrbuteArgument { arg, accepted } => {
                let formatted = accepted
                    .iter()
                    .map(|s| format!("\"{}\"", s))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("The valid arguments here are: {}", formatted)
            }
        CompilationErrorKind::InvalidConstantEvaluation(_) => todo!(),
        CompilationErrorKind::OperatorOverloadNotFound {
                this,
                other,
                operator,
            } => format!("Consider implement correspondent operator overload"),
        CompilationErrorKind::DuplicatedDefinition {
                definition,
                target,
                kind,
            } => format!(
                "Consider removing one of the definitions of `{}.{}`",
                target, definition
            ),
        CompilationErrorKind::MismatchArgumentCount {
                expected,
                found,
                function_name,
            } => format!("Consider adding the missing argument"),
CompilationErrorKind::AssignTypeError { kind, found } => format!("Consider change the type of variable or assign with other value that match expected type"),
    }
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
        CompilationErrorKind::ExpectedConstruction { expect, found } => {
            format_expected_construction(expect, found)
        }
        CompilationErrorKind::MissingMainFunction => format!("Missing `main` function"),
        CompilationErrorKind::DuplicatedVariable { name, previous } => {
            format_duplicated_variable(name, previous)
        }
        CompilationErrorKind::UsingVoidToAssignVariableOrParam => todo!(),
        CompilationErrorKind::DuplicatedTypeDefinition { r#type } => todo!(),
        CompilationErrorKind::DuplicatedField { field } => format_duplicated_field(field),
        CompilationErrorKind::MissingConstruction { construction } => todo!(),
        CompilationErrorKind::InvalidStatementScope { statement } => todo!(),
        CompilationErrorKind::DepthError { msg } => format!("{}", msg),
        CompilationErrorKind::InvalidStatementUsage { error } => format!("{}", error),
        CompilationErrorKind::ExpectType {
            expect,
            found,
            compiler_msg,
        } => format_expect_type(expect, found, compiler_msg),
        CompilationErrorKind::ExpectReturnType { expect, found } => format!(
            "Expect '{}' instance in return, but got '{}' instance.",
            expect, found
        ),
        CompilationErrorKind::UnexpectedReturnValue { found } => todo!(),
        CompilationErrorKind::TypeAssertion { msg } => todo!(),
        CompilationErrorKind::UsageOfNotRequiredStatement { statement, place } => todo!(),
        CompilationErrorKind::DuplicatedNativeFunction { name } => todo!(),
        CompilationErrorKind::NotFoundType { name } => todo!(),
        CompilationErrorKind::NotFoundField { r#type, field } => {
            format!("Type '{}' has not field named '{}'.", r#type, field)
        }
        CompilationErrorKind::ModuleNotFound { path } => {
            format!("Module '{}' not found", path.join("."))
        }
        CompilationErrorKind::ModuleAccessDenied { path, full_path } => todo!(),
        CompilationErrorKind::ModuleReadError {
            path,
            error,
            full_path,
        } => todo!(),
        CompilationErrorKind::InvalidLiteral { line } => todo!(),
        CompilationErrorKind::InvalidPostfixOperatorUsage { msg } => todo!(),
        CompilationErrorKind::InvalidExpression { msg } => format!("{msg}"),
        CompilationErrorKind::InexistentType { r#type } => format_inexistent_type(r#type),
        CompilationErrorKind::UsageOfNotInferredVariable { name } => todo!(),
        CompilationErrorKind::NotFoundArchetypeMask(not_found_archetype_mask) => todo!(),
        CompilationErrorKind::UsageOfUndeclaredVariable { name } => {
            format_usage_of_undeclared_variable(name)
        }
        CompilationErrorKind::InvalidAttributeExpression { msg } => format!("{}.", msg),
        CompilationErrorKind::InvalidOperatorOverload(msg) => {
            format!("Invalid operator overload: {}.", msg)
        }
        CompilationErrorKind::MismatchAttrbuteArgument { arg, accepted: _ } => {
            format!("Mismatch attribute argument: '{}'.", arg)
        }
        CompilationErrorKind::InvalidConstantEvaluation(_) => todo!(),
        CompilationErrorKind::OperatorOverloadNotFound {
            this,
            other,
            operator,
        } => format!(
            "Operator overload `{} {} {}` not found.",
            this, operator, other
        ),
        CompilationErrorKind::DuplicatedDefinition {
            definition,
            target,
            kind,
        } => format!("Duplicated {} definition of '{}'.", kind, definition),
        CompilationErrorKind::SymbolNotFound {
            symbol_kind,
            symbol_name,
        } => format!("{} `{}` not found", symbol_kind, symbol_name),
        CompilationErrorKind::MismatchArgumentCount {
            expected,
            found,
            function_name,
        } => format!(
            "Expect {} arguments for '{}' call, but got {}.",
            expected, function_name, found
        ),
        CompilationErrorKind::AssignTypeError { kind, found } => {
            format!("Cannot assign '{}' instance with '{}' value.", kind, found)
        }
    }
}

fn hint_expected_token(expect: &str, found: &str, after: &Option<String>) -> String {
    match after {
        None => {
            format!("Consider adding {} to fix this error", expect)
        }
        Some(a) => format!("Consider adding {} after {} to fix this error", expect, a),
    }
}

fn format_expected_construction(expect: &str, found: &str) -> String {
    format!("Expect {}, but got '{}'.", expect, found)
}

fn format_duplicated_field(field: &str) -> String {
    format!("Duplicated field '{}'.", field)
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
    format!("The variable '{}' are not declared here", name)
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
            format!("Expect {}, but got '{}'.", expect, found)
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
        '\n' => format!("Illegal character '\\n'"),
        '\r' => format!("Illegal character '\\r'"),
        '\t' => format!("Illegal character '\\t'"),
        _ => format!("Illegal character '{c}'."),
    }
}
