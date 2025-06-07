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
}

impl ParseError {
    pub fn new(message: String, line: usize) -> Self {
        Self { message, line }
    }
}

#[derive(Debug)]
pub struct CompilationError {
    pub msg: String,
    pub kind: CompilationErrorKind,
    pub line: Option<usize>,
}

impl CompilationError {
    pub fn new(msg: String, line: Option<usize>) -> Self {
        Self {
            msg,
            kind: CompilationErrorKind::IllegalCharacter,
            line,
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
}

impl CompilerErrorReporter {
    pub fn new() -> Self {
        Self {
            stack: CompilerErrorStack::new(),
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
