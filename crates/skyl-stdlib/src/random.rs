use skyl_data::objects::Value;
use skyl_ffi::{register_native_funcs, NativeBridge, NativeLibrary};

pub struct GPPRandomLibrary;

impl GPPRandomLibrary {
    fn random_range(args: Vec<Value>) -> Value {
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => Value::Int(rand::random_range(*a..*b)),
            _ => unreachable!(),
        }
    }
}

impl NativeLibrary for GPPRandomLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        register_native_funcs!(bridge, [random_range])
    }
}
