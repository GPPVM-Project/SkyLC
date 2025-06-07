use std::rc::Rc;

use skyl_data::objects::Value;
use skyl_ffi::{register_native_funcs, NativeBridge, NativeLibrary};

pub struct GPPHttpLibrary;

impl GPPHttpLibrary {
    pub fn native_http_request(args: Vec<Value>) -> Value {
        if args.len() < 2 {
            return Value::String(Rc::new("Invalid arguments".to_string()));
        }

        let method = if let Value::String(m) = &args[0] {
            m.to_lowercase()
        } else {
            return Value::String(Rc::new("Invalid method".to_string()));
        };

        let url = if let Value::String(u) = &args[1] {
            u.clone()
        } else {
            return Value::String(Rc::new("Invalid URL".to_string()));
        };

        let body = if args.len() > 2 {
            if let Value::String(b) = &args[2] {
                b.clone()
            } else {
                Rc::new(String::new())
            }
        } else {
            Rc::new(String::new())
        };

        let response = match method.as_str() {
            "get" => ureq::get(&url.to_string()).call(),
            "post" => ureq::post(&url.to_string()).send(&body.to_string()),
            _ => return Value::String(Rc::new("Unsupported method".to_string())),
        };

        match response {
            Ok(mut resp) => {
                let status = resp.status().as_u16() as i32;
                match resp.body_mut().read_to_string() {
                    Ok(response_body_str) => {
                        let full_response = format!("{}\n{}", status, response_body_str);
                        Value::String(Rc::new(full_response))
                    }
                    Err(e) => {
                        Value::String(Rc::new(format!("Failed to read response body: {}", e)))
                    }
                }
            }
            Err(e) => {
                let message = format!("Request error: {}", e);
                Value::String(Rc::new(message))
            }
        }
    }
}

impl NativeLibrary for GPPHttpLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        register_native_funcs!(bridge, [native_http_request]);
    }
}
