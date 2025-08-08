use std::{
    thread::sleep,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use skyl_data::objects::Value;
use skyl_ffi::{register_native_funcs, NativeBridge, NativeLibrary};

pub struct GPPDefaultFunctionsNativeLibrary;

impl GPPDefaultFunctionsNativeLibrary {
    fn int(args: &[Value]) -> Value {
        match &args[0] {
            Value::String(s) => Value::Int(s.trim().parse().unwrap_or_else(|_| panic!("str to int parse error. Input was '{s}'"))),
            Value::Int(i) => Value::Int(*i),
            _ => {
                unreachable!()
            }
        }
    }

    fn float(args: &[Value]) -> Value {
        if let Value::String(s) = &args[0] {
            Value::Float(s.trim().parse().expect("str to float parse error."))
        } else {
            unreachable!()
        }
    }

    fn bool(args: &[Value]) -> Value {
        if let Value::Bool(b) = &args[0] {
            Value::Bool(*b)
        } else {
            unreachable!()
        }
    }

    fn exception(args: &[Value]) -> Value {
        if let Value::String(s) = &args[0] {
            eprintln!("{s}");
            std::process::exit(-1);
        }

        unreachable!();
    }

    fn exit(args: &[Value]) -> Value {
        if let Value::Int(i) = &args[0] {
            std::process::exit(*i);
        }

        unreachable!()
    }

    fn sleep(args: &[Value]) -> Value {
        if let Value::Int(i) = args[0] {
            sleep(Duration::from_millis(i as u64));
        }

        Value::Void
    }

    fn now_ms(_args: &[Value]) -> Value {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("System time before UNIX EPOCH");

        Value::Int(now.as_millis() as i32)
    }
}

impl NativeLibrary for GPPDefaultFunctionsNativeLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        register_native_funcs!(bridge, [exception, int, float, bool, exit, now_ms, sleep,]);
    }
}
