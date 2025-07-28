use std::path::Path;

use skyl_data::SourceFile;

use crate::errors::{CompilationError, CompilationErrorKind};

pub fn format_err(error: &CompilationError, file: &SourceFile) -> String {
    match &error.kind {
        CompilationErrorKind::FunctionMayNotReturn { .. } => {
            format_function_may_not_return(error, file)
        }
        _ => format_generic_error(error, file),
    }
}

fn format_function_may_not_return(error: &CompilationError, file: &SourceFile) -> String {
    let mut output = String::new();

    if let Some(span) = error.span {
        let lines: Vec<&str> = file.content.lines().collect();
        let max_line_digits = lines.len().to_string().len();
        let line_number_pad = max_line_digits.max(2);

        let content_before_end = &file.content[..span.end];
        let end_line_num = content_before_end.matches('\n').count() + 1;
        let end_col = (span.end - 1) - content_before_end.rfind('\n').map_or(0, |p| p + 1);

        output += &format!("\x1b[31mError\x1b[0m: {}\n", stringify_error(error));

        let relative_path = file
            .path
            .strip_prefix(std::env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf()))
            .unwrap_or(file.path.as_path());

        output += &format!(
            " {arrow:>width$} {file}:{line}:{col}\n",
            arrow = "-->",
            file = relative_path.display(),
            line = end_line_num,
            col = end_col + 1,
            width = line_number_pad + 1
        );

        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m\n",
            bar = "|",
            width = line_number_pad + 1
        );

        let source_line = lines.get(end_line_num - 1).unwrap_or(&"");
        output += &format!(
            "\x1b[34m{end_line_num:>line_number_pad$}\x1b[0m \x1b[34m|\x1b[0m {source_line}\n"
        );

        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m \x1b[31m{marker:>offset$}^\x1b[0m\n",
            bar = "|",
            width = line_number_pad + 1,
            marker = "",
            offset = end_col
        );

        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m\n",
            bar = "|",
            width = line_number_pad + 1
        );

        let hint = "Consider adding a return statement before this closing brace".to_string();
        output += &format!(
            "\x1b[34m{bar:>width$}\x1b[0m \x1b[92mHint:\x1b[0m {hint}.\n",
            bar = "╰─",
            width = line_number_pad + 3,
            hint = hint
        );
    } else {
        output += &format!("\x1b[31mError\x1b[0m: {}.\n", stringify_error(error));
    }

    output
}

fn format_generic_error(error: &CompilationError, file: &SourceFile) -> String {
    let mut output = String::new();

    if let (Some(span), Some(start_line_num)) = (error.span, error.line) {
        let lines: Vec<&str> = file.content.lines().collect();
        let max_line_digits = lines.len().to_string().len();
        let line_number_pad = max_line_digits.max(2);

        output += &format!("\x1b[31mError\x1b[0m: {}\n", stringify_error(error));

        let relative_path = file
            .path
            .strip_prefix(std::env::current_dir().unwrap_or_else(|_| Path::new(".").to_path_buf()))
            .unwrap_or(file.path.as_path());

        let content_before_start = &file.content[..span.start];
        let start_col = span.start - content_before_start.rfind('\n').map_or(0, |p| p + 1);

        let content_before_end = &file.content[..span.end];
        let end_line_num = content_before_end.matches('\n').count() + 1;
        let end_col = span.end - content_before_end.rfind('\n').map_or(0, |p| p + 1);

        output += &format!(
            " {arrow:>width$} {file}:{line}:{col}\n",
            arrow = "-->",
            file = relative_path.display(),
            line = start_line_num,
            col = start_col + 1,
            width = line_number_pad + 1
        );

        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m\n",
            bar = "|",
            width = line_number_pad + 1
        );

        if start_line_num == end_line_num {
            let source_line = lines.get(start_line_num - 1).unwrap_or(&"");
            let underline_len = span.end.saturating_sub(span.start).max(1);

            output += &format!(
                "\x1b[34m{start_line_num:>line_number_pad$}\x1b[0m \x1b[34m|\x1b[0m {source_line}\n"
            );
            output += &format!(
                " \x1b[34m{bar:>width$}\x1b[0m \x1b[31m{marker:>offset$}{carets}\x1b[0m\n",
                bar = "|",
                width = line_number_pad + 1,
                marker = "",
                offset = start_col,
                carets = "^".repeat(underline_len)
            );
        } else {
            for i in start_line_num..=end_line_num {
                let line_idx = i - 1;
                let line_content = lines.get(line_idx).unwrap_or(&"");

                output += &format!(
                    "\x1b[34m{i:>line_number_pad$}\x1b[0m \x1b[34m|\x1b[0m {line_content}\n"
                );

                let (offset, underline_len) = if i == start_line_num {
                    (start_col, line_content.len().saturating_sub(start_col))
                } else if i == end_line_num {
                    (0, end_col)
                } else {
                    (0, line_content.len())
                };

                if underline_len > 0 {
                    output += &format!(
                        " \x1b[34m{bar:>width$}\x1b[0m \x1b[31m{marker:>offset$}{carets}\x1b[0m\n",
                        bar = "|",
                        width = line_number_pad + 1,
                        marker = "",
                        offset = offset,
                        carets = "^".repeat(underline_len)
                    );
                }
            }
        }

        output += &format!(
            " \x1b[34m{bar:>width$}\x1b[0m\n",
            bar = "|",
            width = line_number_pad + 1
        );

        let hint = hintify_error(error);
        if !hint.is_empty() {
            output += &format!(
                "\x1b[34m{bar:>width$}\x1b[0m \x1b[92mHint:\x1b[0m {hint}.\n",
                bar = "╰─",
                width = line_number_pad + 3,
                hint = hint
            );
        }

        let notes = notefy_error(error);
        for (i, note) in notes.iter().enumerate() {
            let note_bar = if i == notes.len() - 1 {
                "└─"
            } else {
                "├─"
            };
            output += &format!(
                "\x1b[34m{bar:>width$}\x1b[0m \x1b[93mNote:\x1b[0m {hint}.\n",
                bar = note_bar,
                width = line_number_pad + 6,
                hint = note
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
        CompilationErrorKind::InvalidKeyword { keyword: _ } => todo!(),
        CompilationErrorKind::InvalidAssignmentTarget => todo!(),
        CompilationErrorKind::ArgumentLimitOverflow => todo!(),
        CompilationErrorKind::UnexpectedToken { token: _ } => vec![],
        CompilationErrorKind::ExpectedToken {
                                                        expect: _,
                                                        found: _,
                                                        after: _,
                                                    } => vec![],
        CompilationErrorKind::ExpectedConstruction { expect: _, found: _ } => todo!(),
        CompilationErrorKind::MissingMainFunction => todo!(),
        CompilationErrorKind::DuplicatedVariable { name: _, previous: _ } => todo!(),
        CompilationErrorKind::UsingVoidToAssignVariableOrParam => todo!(),
        CompilationErrorKind::DuplicatedTypeDefinition { r#type: _ } => todo!(),
        CompilationErrorKind::DuplicatedField { field: _ } => todo!(),
        CompilationErrorKind::MissingConstruction { construction: _ } => todo!(),
        CompilationErrorKind::InvalidStatementScope { statement: _ } => todo!(),
        CompilationErrorKind::DepthError { msg: _ } => vec![],
        CompilationErrorKind::InvalidStatementUsage { error: _ } => vec![],
        CompilationErrorKind::ExpectType {
                                                        expect: _,
                                                        found: _,
                                                        compiler_msg: _,
                                                    } => vec![],
        CompilationErrorKind::ExpectReturnType { expect: _, found: _ } => vec![],
        CompilationErrorKind::UnexpectedReturnValue { found: _ } => todo!(),
        CompilationErrorKind::TypeAssertion { msg: _ } => todo!(),
        CompilationErrorKind::UsageOfNotRequiredStatement { statement: _, place: _ } => todo!(),
        CompilationErrorKind::DuplicatedNativeFunction { name: _ } => todo!(),
        CompilationErrorKind::NotFoundType { name: _ } => todo!(),
        CompilationErrorKind::NotFoundField { r#type: _, field: _ } => vec![],
        CompilationErrorKind::ModuleNotFound { path: _ } => vec![],
        CompilationErrorKind::ModuleAccessDenied { path: _, full_path: _ } => todo!(),
        CompilationErrorKind::ModuleReadError {
                                                        path: _,
                                                        full_path: _,
                                                        error: _,
                                                    } => todo!(),
        CompilationErrorKind::UnsupportedFeature { feature: _ } => vec![],
        CompilationErrorKind::InvalidLiteral { line: _ } => todo!(),
        CompilationErrorKind::InvalidPostfixOperatorUsage { msg: _ } => todo!(),
        CompilationErrorKind::InvalidExpression { msg: _ } => vec![],
        CompilationErrorKind::InexistentType { r#type: _ } => vec![],
        CompilationErrorKind::NotFoundArchetypeMask(_not_found_archetype_mask) => todo!(),
        CompilationErrorKind::UsageOfNotInferredVariable { name: _ } => todo!(),
        CompilationErrorKind::UsageOfUndeclaredVariable { name: _ } => vec![],
        CompilationErrorKind::InvalidAttributeExpression { msg: _ } => vec![],
        CompilationErrorKind::InvalidOperatorOverload(_) => vec![],
        CompilationErrorKind::MismatchAttrbuteArgument { arg: _, accepted: _ } => vec![],
        CompilationErrorKind::InvalidConstantEvaluation(_) => todo!(),
        CompilationErrorKind::OperatorOverloadNotFound { this, other, operator: _ } => {
                                vec![format!("#[coersion(op)]"), format!("def overload_name(self: {}, other: {}) -> SomeKind", this, other)]
                        },
        CompilationErrorKind::DuplicatedDefinition { kind: _, definition: _, target: _ } => vec![],
        CompilationErrorKind::SymbolNotFound { symbol_kind: _, symbol_name: _ } => vec![],
        CompilationErrorKind::MismatchArgumentCount { expected: _, found: _, function_name: _ } => vec![],
        CompilationErrorKind::AssignTypeError { kind: _, found: _ } => vec![],
        CompilationErrorKind::FunctionMayNotReturn { function_kind: _, name: _ } => vec![],
CompilationErrorKind::MainFunctionReturnKind => vec![],
    }
}

fn hintify_error(err: &CompilationError) -> String {
    match &err.kind {
        CompilationErrorKind::SymbolNotFound {
                                symbol_kind,
                                symbol_name,
                    } => format!(
                        "Consider to creating a `{symbol_name}` {symbol_kind} declaration"
                    ),
        CompilationErrorKind::IllegalCharacter(c) => {
                        format!("Consider removing '{c}' from source code")
                    }
        CompilationErrorKind::InvalidNativeDeclaration => format_invalid_native_declaration(),
        CompilationErrorKind::UnsupportedFeature { feature } => format_unsupported_feature(feature),
        CompilationErrorKind::InvalidBuiltinDeclaration => todo!(),
        CompilationErrorKind::InvalidKeyword { keyword: _ } => todo!(),
        CompilationErrorKind::InvalidAssignmentTarget => todo!(),
        CompilationErrorKind::ArgumentLimitOverflow => todo!(),
        CompilationErrorKind::UnexpectedToken { token: _ } => "Consider removing this token".to_string(),
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
        CompilationErrorKind::DuplicatedTypeDefinition { r#type: _ } => todo!(),
        CompilationErrorKind::DuplicatedField { field } => format_duplicated_field(field),
        CompilationErrorKind::MissingConstruction { construction: _ } => todo!(),
        CompilationErrorKind::InvalidStatementScope { statement: _ } => todo!(),
        CompilationErrorKind::DepthError { msg: _ } => {
                        "Move the declaration to other scope level.".to_string()
                    }
        CompilationErrorKind::InvalidStatementUsage { error: _ } => {
                        "Consider removing the statement".to_string()
                    }
        CompilationErrorKind::ExpectType {
                        expect,
                        found,
                        compiler_msg,
                    } => format_expect_type(expect, found, compiler_msg),
        CompilationErrorKind::ExpectReturnType { expect: _, found: _ } => {
                        "Consider change the return type of the function or returned value".to_string()
                    }
        CompilationErrorKind::UnexpectedReturnValue { found: _ } => todo!(),
        CompilationErrorKind::TypeAssertion { msg: _ } => todo!(),
        CompilationErrorKind::UsageOfNotRequiredStatement { statement: _, place: _ } => todo!(),
        CompilationErrorKind::DuplicatedNativeFunction { name: _ } => todo!(),
        CompilationErrorKind::NotFoundType { name: _ } => todo!(),
        CompilationErrorKind::NotFoundField { r#type, field } => {
                        format!("Consider removing `{type}.{field}` from your code.")
                    }
        CompilationErrorKind::ModuleNotFound { path } => format!(
                        "Consider to create module '{}' before use it",
                        path.join(".")
                    ),
        CompilationErrorKind::ModuleAccessDenied { path: _, full_path: _ } => todo!(),
        CompilationErrorKind::ModuleReadError {
                        path: _,
                        error: _,
                        full_path: _,
                    } => todo!(),
        CompilationErrorKind::InvalidLiteral { line: _ } => todo!(),
        CompilationErrorKind::InvalidPostfixOperatorUsage { msg: _ } => todo!(),
        CompilationErrorKind::InvalidExpression { msg: _ } => {
                        "Consider removing this expression".to_string()
                    }
        CompilationErrorKind::InexistentType { r#type } => {
                        format!("Consider declare the '{type}' type before use it")
                    }
        CompilationErrorKind::UsageOfNotInferredVariable { name: _ } => todo!(),
        CompilationErrorKind::NotFoundArchetypeMask(_not_found_archetype_mask) => todo!(),
        CompilationErrorKind::UsageOfUndeclaredVariable { name } => {
                        format_usage_of_undeclared_variable(name)
                    }
        CompilationErrorKind::InvalidAttributeExpression { msg: _ } => {
                        "Consider change or remove the attribute declaration".to_string()
                    }
        CompilationErrorKind::InvalidOperatorOverload(_) => {
                        "Consider changing function structure to match with desired overload.".to_string()
                    }
        CompilationErrorKind::MismatchAttrbuteArgument { arg: _, accepted } => {
                        let formatted = accepted
                            .iter()
                            .map(|s| format!("\"{s}\""))
                            .collect::<Vec<_>>()
                            .join(", ");

                        format!("The valid arguments here are: {formatted}")
                    }
        CompilationErrorKind::InvalidConstantEvaluation(_) => todo!(),
        CompilationErrorKind::OperatorOverloadNotFound {
                        this: _,
                        other: _,
                        operator: _,
                    } => "Consider implement correspondent operator overload".to_string(),
        CompilationErrorKind::DuplicatedDefinition {
                        definition,
                        target,
                        kind: _,
                    } => format!(
                        "Consider removing one of the definitions of `{target}.{definition}`"
                    ),
        CompilationErrorKind::MismatchArgumentCount {
                        expected: _,
                        found: _,
                        function_name: _,
                    } => "Consider adding the missing argument".to_string(),
        CompilationErrorKind::AssignTypeError { kind: _, found: _ } => "Consider change the type of variable or assign with other value that match expected type".to_string(),
        CompilationErrorKind::FunctionMayNotReturn { function_kind: _, name: _ } => "Consider adding return statements to all control flow paths".to_string(),
CompilationErrorKind::MainFunctionReturnKind => "Consider changing the return type of main function to `void`.".to_string(),
    }
}

fn stringify_error(err: &CompilationError) -> String {
    match &err.kind {
        CompilationErrorKind::IllegalCharacter(c) => format_illegal_character_err(c),
        CompilationErrorKind::InvalidNativeDeclaration => format_invalid_native_declaration(),
        CompilationErrorKind::UnsupportedFeature { feature } => format_unsupported_feature(feature),
        CompilationErrorKind::InvalidBuiltinDeclaration => todo!(),
        CompilationErrorKind::InvalidKeyword { keyword: _ } => todo!(),
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
        CompilationErrorKind::MissingMainFunction => "Missing `main` function".to_string(),
        CompilationErrorKind::DuplicatedVariable { name, previous } => {
            format_duplicated_variable(name, previous)
        }
        CompilationErrorKind::UsingVoidToAssignVariableOrParam => todo!(),
        CompilationErrorKind::DuplicatedTypeDefinition { r#type: _ } => todo!(),
        CompilationErrorKind::DuplicatedField { field } => format_duplicated_field(field),
        CompilationErrorKind::MissingConstruction { construction: _ } => todo!(),
        CompilationErrorKind::InvalidStatementScope { statement: _ } => todo!(),
        CompilationErrorKind::DepthError { msg } => msg.to_string(),
        CompilationErrorKind::InvalidStatementUsage { error } => error.to_string(),
        CompilationErrorKind::ExpectType {
            expect,
            found,
            compiler_msg,
        } => format_expect_type(expect, found, compiler_msg),
        CompilationErrorKind::ExpectReturnType { expect, found } => {
            format!("Expect '{expect}' instance in return, but got '{found}' instance.")
        }
        CompilationErrorKind::UnexpectedReturnValue { found: _ } => todo!(),
        CompilationErrorKind::TypeAssertion { msg: _ } => todo!(),
        CompilationErrorKind::UsageOfNotRequiredStatement {
            statement: _,
            place: _,
        } => todo!(),
        CompilationErrorKind::DuplicatedNativeFunction { name: _ } => todo!(),
        CompilationErrorKind::NotFoundType { name: _ } => todo!(),
        CompilationErrorKind::NotFoundField { r#type, field } => {
            format!("Type '{type}' has not field named '{field}'.")
        }
        CompilationErrorKind::ModuleNotFound { path } => {
            format!("Module '{}' not found", path.join("."))
        }
        CompilationErrorKind::ModuleAccessDenied {
            path: _,
            full_path: _,
        } => todo!(),
        CompilationErrorKind::ModuleReadError {
            path: _,
            error: _,
            full_path: _,
        } => todo!(),
        CompilationErrorKind::InvalidLiteral { line: _ } => todo!(),
        CompilationErrorKind::InvalidPostfixOperatorUsage { msg: _ } => todo!(),
        CompilationErrorKind::InvalidExpression { msg } => msg.to_string(),
        CompilationErrorKind::InexistentType { r#type } => format_inexistent_type(r#type),
        CompilationErrorKind::UsageOfNotInferredVariable { name: _ } => todo!(),
        CompilationErrorKind::NotFoundArchetypeMask(_not_found_archetype_mask) => todo!(),
        CompilationErrorKind::UsageOfUndeclaredVariable { name } => {
            format_usage_of_undeclared_variable(name)
        }
        CompilationErrorKind::InvalidAttributeExpression { msg } => format!("{msg}."),
        CompilationErrorKind::InvalidOperatorOverload(msg) => {
            format!("Invalid operator overload: {msg}.")
        }
        CompilationErrorKind::MismatchAttrbuteArgument { arg, accepted: _ } => {
            format!("Mismatch attribute argument: '{arg}'.")
        }
        CompilationErrorKind::InvalidConstantEvaluation(_) => todo!(),
        CompilationErrorKind::OperatorOverloadNotFound {
            this,
            other,
            operator,
        } => format!("Operator overload `{this} {operator} {other}` not found."),
        CompilationErrorKind::DuplicatedDefinition {
            definition,
            target: _,
            kind,
        } => format!("Duplicated {kind} definition of '{definition}'."),
        CompilationErrorKind::SymbolNotFound {
            symbol_kind,
            symbol_name,
        } => format!("{symbol_kind} `{symbol_name}` not found"),
        CompilationErrorKind::MismatchArgumentCount {
            expected,
            found,
            function_name,
        } => format!("Expect {expected} arguments for '{function_name}' call, but got {found}."),
        CompilationErrorKind::AssignTypeError { kind, found } => {
            format!("Cannot assign '{kind}' instance with '{found}' value.")
        }
        CompilationErrorKind::FunctionMayNotReturn {
            function_kind,
            name,
        } => format!("{function_kind} `{name}` may not return in all control flow paths."),
        CompilationErrorKind::MainFunctionReturnKind => {
            "Main function must be not return value".to_string()
        }
    }
}

fn hint_expected_token(expect: &str, _found: &str, after: &Option<String>) -> String {
    match after {
        None => {
            format!("Consider adding {expect} to fix this error")
        }
        Some(a) => format!("Consider adding {expect} after {a} to fix this error"),
    }
}

fn format_expected_construction(expect: &str, found: &str) -> String {
    format!("Expect {expect}, but got '{found}'.")
}

fn format_duplicated_field(field: &str) -> String {
    format!("Duplicated field '{field}'.")
}

fn format_inexistent_type(r#type: &str) -> String {
    format!("Type '{type}' is not declared here")
}

fn format_expect_type(expect: &str, found: &str, compiler_msg: &Option<String>) -> String {
    match compiler_msg {
        None => format!("Expect '{expect}' instance, but got '{found}'."),
        Some(msg) => {
            format!("Expect '{expect}' instance, but got '{found}'. Compiler message: {msg}")
        }
    }
}

fn format_usage_of_undeclared_variable(name: &str) -> String {
    format!("The variable '{name}' are not declared here")
}

fn format_duplicated_variable(name: &str, previous: &usize) -> String {
    format!("The name '{name}' was previous declared in current scope (at line {previous}).")
}

fn format_unexpected_token(token: &str) -> String {
    format!("Unexpected token '{token}'.")
}

fn format_expected_token(expect: &str, found: &str, after: &Option<String>) -> String {
    match after {
        None => {
            format!("Expect {expect}, but got '{found}'.")
        }
        Some(a) => format!("Expect {expect} after {a}, but got {found}."),
    }
}

fn format_unsupported_feature(feature: &str) -> String {
    format!("Unsupported feature '{feature}'.")
}

fn format_invalid_native_declaration() -> String {
    "Invalid use of 'native' keyword.".into()
}

fn format_illegal_character_err(c: &char) -> String {
    match c {
        '\n' => "Illegal character '\\n'".to_string(),
        '\r' => "Illegal character '\\r'".to_string(),
        '\t' => "Illegal character '\\t'".to_string(),
        _ => format!("Illegal character '{c}'."),
    }
}
