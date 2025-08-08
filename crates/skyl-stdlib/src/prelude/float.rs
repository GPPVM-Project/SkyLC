use std::rc::Rc;

use skyl_data::objects::Value;
use skyl_ffi::{register_native_funcs, NativeBridge, NativeLibrary};
pub struct GPPFloatLibrary;

impl GPPFloatLibrary {
    fn float_sqrt(args: &[Value]) -> Value {
        if let Value::Float(a) = args[0] {
            Value::Float(a.sqrt())
        } else {
            println!("{}", args[0]);
            unreachable!()
        }
    }

    fn float_to_int(args: &[Value]) -> Value {
        if let Value::Float(f) = &args[0] {
            return Value::Int(*f as i32);
        }

        unreachable!("Found value '{}'.", &args[0]);
    }

    fn float_to_string(args: &[Value]) -> Value {
        if let Value::Float(f) = &args[0] {
            return Value::String(Rc::new(f.to_string()));
        }

        unreachable!("Found value '{}'.", &args[0]);
    }
}

impl NativeLibrary for GPPFloatLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        register_native_funcs!(bridge, [float_to_int, float_sqrt, float_to_string]);
    }
}
