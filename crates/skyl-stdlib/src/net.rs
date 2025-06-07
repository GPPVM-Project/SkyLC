use skyl_ffi::{NativeBridge, NativeLibrary};

use crate::net::http::GPPHttpLibrary;

pub mod http;

pub struct GPPNetLibrary;

impl NativeLibrary for GPPNetLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        let mut native_libs: Vec<Box<dyn NativeLibrary>> = Vec::new();

        native_libs.push(Box::new(GPPHttpLibrary {}));

        for lib in native_libs {
            lib.register_functions(bridge);
        }
    }
}
