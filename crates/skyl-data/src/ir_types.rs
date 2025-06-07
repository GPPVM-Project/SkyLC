#[derive(Debug, Clone)]
pub struct NativeFunctionInfo {
    pub arity: u8,
    pub id: u32,
}

impl NativeFunctionInfo {
    pub fn new(arity: u8, id: u32) -> Self {
        Self { arity, id }
    }
}
