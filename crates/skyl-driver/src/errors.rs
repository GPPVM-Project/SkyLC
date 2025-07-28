use crate::format_err::*;
use skyl_data::{read_file_without_bom, Archetype, CompilerContext, SourceFile, Span};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
pub struct NotFoundArchetypeMask {
    pub arch: Vec<Archetype>,
}

#[derive(Debug, Clone)]
pub enum CompilationErrorKind {
    // Lexer
    IllegalCharacter(char),

    // Parser
    InvalidNativeDeclaration,
    InvalidBuiltinDeclaration,
    InvalidKeyword {
        keyword: String,
    },
    InvalidAssignmentTarget,
    ArgumentLimitOverflow,
    UnexpectedToken {
        token: String,
    },
    ExpectedToken {
        expect: String,
        found: String,
        after: Option<String>,
    },
    ExpectedConstruction {
        expect: String,
        found: String,
    },

    // Semantic
    MissingMainFunction,
    DuplicatedVariable {
        name: String,
        previous: usize,
    },
    UsingVoidToAssignVariableOrParam,
    DuplicatedTypeDefinition {
        r#type: String,
    },
    DuplicatedField {
        field: String,
    },
    DuplicatedDefinition {
        kind: String,
        definition: String,
        target: String,
    },
    MissingConstruction {
        construction: String,
    },
    InvalidStatementScope {
        statement: String,
    },
    DepthError {
        msg: String,
    },
    InvalidStatementUsage {
        error: String,
    },
    ExpectType {
        expect: String,
        found: String,
        compiler_msg: Option<String>,
    },
    ExpectReturnType {
        expect: String,
        found: String,
    },
    UnexpectedReturnValue {
        found: String,
    },
    TypeAssertion {
        msg: String,
    },
    UsageOfNotRequiredStatement {
        statement: String,
        place: String,
    },
    DuplicatedNativeFunction {
        name: String,
    },
    NotFoundType {
        name: String,
    },
    NotFoundField {
        r#type: String,
        field: String,
    },

    SymbolNotFound {
        symbol_kind: String,
        symbol_name: String,
    },

    ModuleNotFound {
        path: Vec<String>,
    },

    ModuleAccessDenied {
        path: Vec<String>,
        full_path: String,
    },
    ModuleReadError {
        path: Vec<String>,
        full_path: String,
        error: String,
    },

    // General
    UnsupportedFeature {
        feature: &'static str,
    },
    InvalidLiteral {
        line: usize,
    },
    InvalidPostfixOperatorUsage {
        msg: String,
    },
    InvalidExpression {
        msg: String,
    },
    InvalidAttributeExpression {
        msg: String,
    },
    InexistentType {
        r#type: String,
    },
    NotFoundArchetypeMask(NotFoundArchetypeMask),
    UsageOfNotInferredVariable {
        name: String,
    },
    UsageOfUndeclaredVariable {
        name: String,
    },
    InvalidOperatorOverload(String),
    MismatchAttrbuteArgument {
        arg: String,
        accepted: Vec<String>,
    },
    InvalidConstantEvaluation(&'static str),
    OperatorOverloadNotFound {
        this: String,
        other: String,
        operator: String,
    },
    MismatchArgumentCount {
        expected: usize,
        found: usize,
        function_name: String,
    },
    AssignTypeError {
        kind: String,
        found: String,
    },
    FunctionMayNotReturn {
        function_kind: String,
        name: String,
    },
    MainFunctionReturnKind,
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

#[derive(Debug, Clone)]
pub struct CompilationError {
    pub kind: CompilationErrorKind,
    pub line: Option<usize>,
    pub span: Option<Span>,
    pub file: Option<String>,
}

impl CompilationError {
    pub fn new(kind: CompilationErrorKind, line: Option<usize>) -> Self {
        Self {
            kind,
            line,
            span: None,
            file: None,
        }
    }

    pub fn with_span(kind: CompilationErrorKind, line: Option<usize>, span: Span) -> Self {
        Self {
            kind,
            line,
            span: Some(span),
            file: None,
        }
    }
}

#[derive(Debug)]
pub struct CompilerErrorStack {
    errors: Vec<CompilationError>,
}

impl Default for CompilerErrorStack {
    fn default() -> Self {
        Self::new()
    }
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
    ctx: Option<Rc<RefCell<CompilerContext>>>,
    error_files: HashMap<String, SourceFile>,
}

impl CompilerErrorReporter {
    pub fn new(file: Rc<SourceFile>, ctx: Option<Rc<RefCell<CompilerContext>>>) -> Self {
        Self {
            stack: CompilerErrorStack::new(),
            file: Some(file),
            ctx,
            error_files: HashMap::new(),
        }
    }

    pub fn empty() -> Self {
        Self {
            stack: CompilerErrorStack::new(),
            file: None,
            ctx: None,
            error_files: HashMap::new(),
        }
    }

    pub fn report_error(&mut self, mut error: CompilationError) {
        if let Some(ctx) = &self.ctx {
            error.file = Some(ctx.borrow().peek_module());
            if let std::collections::hash_map::Entry::Vacant(e) =
                self.error_files.entry(error.clone().file.unwrap())
            {
                let file = read_file_without_bom(error.clone().file.unwrap().as_str());
                let file = match file {
                    Err(e) => {
                        gpp_error!("File not found: {}. OS Message: {}", error.file.unwrap(), e)
                    }
                    Ok(f) => f,
                };

                e.insert(file);
            }

            self.stack.push(error);
        } else {
            self.stack.push(error);
        }
    }

    pub fn get_errors(&self) -> &Vec<CompilationError> {
        &self.stack.errors
    }

    pub fn has_errors(&self) -> bool {
        !self.stack.errors.is_empty()
    }
}

#[derive(Debug)]
pub struct PipelineError(pub String);

impl PipelineError {
    pub fn new(msg: String) -> Self {
        Self(msg)
    }
}

pub fn handle_errors(reporter: &CompilerErrorReporter) {
    if reporter.has_errors() {
        let _file = reporter.file.as_ref().map(Rc::clone).unwrap().clone();

        for error in reporter.get_errors() {
            let formated_error =
                format_err(error, &reporter.error_files[&error.file.clone().unwrap()]);
            println!("{formated_error}");
        }

        gpp_error!(
            "The compiler stopped because an error occurred during one of the compilation phases."
        );
    }
}
