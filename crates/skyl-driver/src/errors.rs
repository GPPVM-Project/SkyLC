use crate::format_err::*;
use skyl_data::{Archetype, SourceFile, Span};
use std::rc::Rc;

#[macro_export]
macro_rules! gpp_error {
    ($($arg:tt)*) => {
        {
            eprintln!("\x1b[31mGPP Error\x1b[0m: {}", format_args!($($arg)*));
            std::process::exit(1);
        }
    };
}

#[derive(Debug, Clone)]
pub struct NotFoundArchetypeMask{
    pub arch: Vec<Archetype>
}

#[derive(Debug, Clone)]
pub enum CompilationErrorKind {
    // Lexer
    IllegalCharacter(char),

    // Parser
    InvalidNativeDeclaration,
    InvalidBuiltinDeclaration,
    InvalidKeyword { keyword: String },
    InvalidAssignmentTarget,
    ArgumentLimitOverflow,
    UnexpectedToken { token: String },
    ExpectedToken { expect: String, found: String, after: Option<String> = None },
    ExpectedConstruction { expect: String, found: String },

    // Semantic
    MissingMainFunction,
    DuplicatedVariable {
        name: String, previous: usize
    },
    UsingVoidToAssignVariableOrParam,
    DuplicatedTypeDefinition {
        r#type: String
    },
    DuplicatedField{
        field: String
    },
    MissingConstruction {
        construction: String
    },
    InvalidStatementScope {
        statement: String
    },
    DepthError {
        msg: String,
    },
    InvalidStatementUsage {
        error: String
    },
    ExpectType {
        expect: String, found: String, compiler_msg: Option<String>
    },
    ExpectReturnType {
        expect: String, found: String
    },
    UnexpectedReturnValue {
        found: String
    },
    TypeAssertion {
        msg: String
    },
    UsageOfNotRequiredStatement {
        statement: String, place: String
    },
    DuplicatedNativeFunction {
        name: String
    },
    NotFoundType {
        name: String,
    },
    NotFoundField {
        r#type: String,
        field: String
    },

    ModuleNotFound {
        path: Vec<String>
    },
     
    ModuleAccessDenied {
        path: Vec<String>,
        full_path: String,
    },
    ModuleReadError {
        path: Vec<String>,
        full_path: String
        , error: String    },

    // General
    UnsupportedFeature { feature: &'static str },
    InvalidLiteral {
        line: usize,
    },
    InvalidPostfixOperatorUsage {
        msg: String,
    },
    InvalidExpression {
        msg: String
    },
    InexistentType {
        r#type: String,
    },
    NotFoundArchetypeMask(NotFoundArchetypeMask),
    UsageOfNotInferredVariable {
        name: String
    },
    UsageOfUndeclaredVariable {
        name: String
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: CompilationErrorKind,
    pub line: usize,
    pub span: Span,
}

impl ParseError {
    pub fn new(kind: CompilationErrorKind, line: usize, span: Span) -> Self {
        Self { kind, line, span }
    }
}

#[derive(Debug)]
pub struct CompilationError {
    pub kind: CompilationErrorKind,
    pub line: Option<usize>,
    pub span: Option<Span>,
}

impl CompilationError {
    pub fn new(kind: CompilationErrorKind, line: Option<usize>) -> Self {
        Self {
            kind,
            line,
            span: None,
        }
    }

    pub fn with_span(kind: CompilationErrorKind, line: Option<usize>, span: Span) -> Self {
        Self {
            kind,
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
        let file = reporter.file.as_ref().map(Rc::clone).unwrap().clone();

        for error in reporter.get_errors() {
            let formated_error = format_err(error, &file);
            println!("{}", formated_error);
        }

        gpp_error!(
            "The compiler stopped because an error occurred during one of the compilation phases."
        );
    }
}
