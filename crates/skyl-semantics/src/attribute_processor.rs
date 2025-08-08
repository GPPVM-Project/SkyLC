#![allow(clippy::result_large_err)]

use skyl_data::{BuiltinAttributeUsage, FunctionPrototype, Token};

use crate::result::TyResult;

pub trait AttributeProcessor {
    fn process_one_attribute<T>(
        &mut self,
        location: &Token,
        attribute: BuiltinAttributeUsage,
        definition: &FunctionPrototype,
    ) -> TyResult<()>;
}
