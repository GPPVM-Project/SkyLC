use std::{path::PathBuf, rc::Rc};

use skyl_data::{SourceFile, Span};

#[macro_export]
macro_rules! gpp_error {
    ($($arg:tt)*) => {
        {
            eprintln!("\x1b[31mError\x1b[0m: {}", format_args!($($arg)*));
            std::process::exit(1);
        }
    };
}

#[derive(Debug)]
pub enum CompilationErrorKind {
    IllegalCharacter,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: String, line: usize, span: Span) -> Self {
        Self {
            message,
            line,
            span,
        }
    }
}

#[derive(Debug)]
pub struct CompilationError {
    pub msg: String,
    pub kind: CompilationErrorKind,
    pub line: Option<usize>,
    pub span: Option<Span>,
}

impl CompilationError {
    pub fn new(msg: String, line: Option<usize>) -> Self {
        Self {
            msg,
            kind: CompilationErrorKind::IllegalCharacter,
            line,
            span: None,
        }
    }

    pub fn with_span(msg: String, line: Option<usize>, span: Span) -> Self {
        Self {
            msg,
            kind: CompilationErrorKind::IllegalCharacter,
            line,
            span: Some(span),
        }
    }
}

#[derive(Debug)]
pub struct CompilerErrorStack {
    errors: Vec<CompilationError>,
}

impl CompilerErrorStack {
    pub fn new() -> Self {
        CompilerErrorStack { errors: Vec::new() }
    }

    pub fn push(&mut self, error: CompilationError) {
        self.errors.push(error);
    }
}

#[derive(Debug)]
pub struct CompilerErrorReporter {
    stack: CompilerErrorStack,
    file: Option<Rc<SourceFile>>,
}

impl CompilerErrorReporter {
    pub fn new(file: Rc<SourceFile>) -> Self {
        Self {
            stack: CompilerErrorStack::new(),
            file: Some(file),
        }
    }

    pub fn empty() -> Self {
        Self {
            stack: CompilerErrorStack::new(),
            file: None,
        }
    }

    pub fn report_error(&mut self, error: CompilationError) {
        self.stack.push(error);
    }

    pub fn get_errors(&self) -> &Vec<CompilationError> {
        &self.stack.errors
    }

    pub fn has_errors(&self) -> bool {
        self.stack.errors.len() > 0
    }
}

#[derive(Debug)]
pub struct PipelineError(pub String);

pub fn handle_errors(reporter: &CompilerErrorReporter) {
    if reporter.has_errors() {
        let file = reporter.file.as_ref().map(Rc::clone);

        for error in reporter.get_errors() {
            if let (Some(file), Some(span), Some(line)) = (file.as_ref(), error.span, error.line) {
                let lines: Vec<&str> = file.content.lines().collect();
                let line_idx = line - 1;
                let source_line = lines.get(line_idx).unwrap_or(&"");

                // Cálculo da coluna correta
                let content_before = &file.content[..span.start];
                let last_line_start = content_before.rfind('\n').map(|pos| pos + 1).unwrap_or(0);
                let col_start = span.start - last_line_start;
                let underline_len = span.end.saturating_sub(span.start).max(1);

                // Largura do número de linha para alinhar corretamente
                let max_line_digits = lines.len().to_string().len();
                let line_number_pad = max_line_digits.max(2);

                // Cabeçalho do erro
                println!(
                    " {arrow:>width$} {file}:{line}:{col}",
                    arrow = "-->",
                    file = file.name,
                    line = line,
                    col = col_start + 1,
                    width = line_number_pad + 1
                );

                // Separador visual
                println!(" {bar:>width$}", bar = "|", width = line_number_pad + 1);

                // Linha com o erro
                println!(
                    "{line_number:>width$} | {line_content}",
                    line_number = line,
                    line_content = source_line,
                    width = line_number_pad
                );

                // Seta indicando o local do erro
                print!(" {bar:>width$} ", bar = "|", width = line_number_pad + 1);

                println!(
                    "\x1b[31m{marker:>offset$}{carets}\x1b[0m",
                    marker = "",
                    offset = col_start,
                    carets = "^".repeat(underline_len)
                );

                // Mensagem
                println!("\x1b[31mError\x1b[0m: {}\n", error.msg);
            } else if let Some(line) = error.line {
                println!("\x1b[31mError\x1b[0m: {}. At line {}.", error.msg, line);
            } else {
                println!("\x1b[31mError\x1b[0m: {}.", error.msg);
            }
        }

        gpp_error!(
            "The compiler stopped because an error occurred during one of the compilation phases."
        );
    }
}
