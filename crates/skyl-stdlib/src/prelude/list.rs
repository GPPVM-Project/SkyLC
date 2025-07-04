use skyl_data::objects::{List, Value};
use skyl_ffi::{register_native_funcs, NativeBridge, NativeLibrary};

pub struct GPPListLibrary;

impl GPPListLibrary {
    fn list_len(args: Vec<Value>) -> Value {
        if let Value::Object(obj_ptr) = &args[0] {
            let len = unsafe { obj_ptr.borrow() }
                .as_any()
                .downcast_ref::<List>()
                .unwrap()
                .elements
                .len();
            return Value::Int(len as i32);
        }

        unreachable!();
    }

    fn list_append(args: Vec<Value>) -> Value {
        if let Value::Object(obj_ptr) = &args[0] {
            let value = &args[1];
            unsafe { obj_ptr.obj.as_mut().unwrap() }
                .as_any_mut()
                .downcast_mut::<List>()
                .unwrap()
                .elements
                .push(value.clone());

            return Value::Void;
        }

        unreachable!()
    }

    fn list_pop(args: Vec<Value>) -> Value {
        if let Value::Object(obj_ptr) = &args[0] {
            let value = &args[1];

            if let Value::Int(i) = value {
                unsafe { obj_ptr.obj.as_mut().unwrap() }
                    .as_any_mut()
                    .downcast_mut::<List>()
                    .unwrap()
                    .elements
                    .remove(*i as usize);
            }

            return Value::Void;
        }

        unreachable!()
    }
}

impl NativeLibrary for GPPListLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        register_native_funcs!(bridge, [list_len, list_append, list_pop]);
    }
}
