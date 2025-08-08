use std::rc::Rc;

use skyl_data::objects::Value;
use skyl_ffi::{register_native_funcs, NativeBridge, NativeLibrary};

pub struct GPPIntLibrary;

impl GPPIntLibrary {
    fn int_clamp(args: &[Value]) -> Value {
        if let (Value::Int(x), Value::Int(min), Value::Int(max)) = (&args[0], &args[1], &args[2]) {
            Value::Int(*x.clamp(min, max))
        } else {
            unreachable!()
        }
    }

    fn int_abs(args: &[Value]) -> Value {
        if let Value::Int(x) = args[0] {
            Value::Int(x.abs())
        } else {
            unreachable!()
        }
    }

    fn int_is_even(args: &[Value]) -> Value {
        if let Value::Int(x) = args[0] {
            Value::Bool(x % 2 == 0)
        } else {
            unreachable!()
        }
    }

    fn int_is_odd(args: &[Value]) -> Value {
        if let Value::Int(x) = args[0] {
            Value::Bool(x % 2 != 0)
        } else {
            unreachable!()
        }
    }

    fn int_sign(args: &[Value]) -> Value {
        if let Value::Int(x) = args[0] {
            Value::Int(x.signum())
        } else {
            unreachable!()
        }
    }

    fn int_max(args: &[Value]) -> Value {
        if let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) {
            Value::Int(std::cmp::max(*a, *b))
        } else {
            unreachable!()
        }
    }

    fn int_min(args: &[Value]) -> Value {
        if let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) {
            Value::Int(std::cmp::min(*a, *b))
        } else {
            unreachable!()
        }
    }

    fn int_sqrt(args: &[Value]) -> Value {
        if let Value::Int(a) = args[0] {
            Value::Float((a as f32).sqrt())
        } else {
            unreachable!()
        }
    }

    fn int_to_float(args: &[Value]) -> Value {
        if let Value::Int(i) = &args[0] {
            return Value::Float(*i as f32);
        }

        unreachable!("Found value '{}'.", &args[0]);
    }

    fn int_to_string(args: &[Value]) -> Value {
        if let Value::Int(i) = &args[0] {
            return Value::String(Rc::new(i.to_string()));
        }

        unreachable!("Found value '{}'.", &args[0]);
    }
}

impl NativeLibrary for GPPIntLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        register_native_funcs!(
            bridge,
            [
                int_abs,
                int_is_even,
                int_to_string,
                int_is_odd,
                int_sign,
                int_max,
                int_min,
                int_clamp,
                int_sqrt,
                int_to_float,
            ]
        );
    }
}
