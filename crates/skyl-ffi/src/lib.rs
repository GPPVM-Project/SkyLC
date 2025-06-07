use skyl_data::objects::Value;

pub type NativeFnPtr = fn(Vec<Value>) -> Value;

#[derive(Clone, Debug)]
pub struct NativeFunction {
    pub handler: NativeFnPtr,
    pub arity: u8,
    pub name: String,
}

impl NativeFunction {
    pub fn new(handler: NativeFnPtr, arity: u8) -> Self {
        Self {
            handler,
            arity,
            name: String::from("unamed_native_fn"),
        }
    }

    pub fn named(handler: NativeFnPtr, arity: u8, name: String) -> Self {
        Self {
            handler,
            arity,
            name,
        }
    }
}

pub trait NativeBridge {
    fn bind(&mut self, name: &str, func: NativeFnPtr);
}

pub trait NativeLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge);
}

#[macro_export]
macro_rules! register_native_funcs {
    ($bridge:expr, [$($name:ident),* $(,)?]) => {
        $(
            $bridge.bind(stringify!($name), Self::$name);
        )*
    };
}
