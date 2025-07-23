use skyl_data::AnnotatedStatement;
use skyl_driver::errors::CompilationError;

pub type TyStmts = Vec<AnnotatedStatement>;
pub type TyResult<T> = Result<T, CompilationError>;
