use skyl_ffi::{NativeBridge, NativeLibrary};

use crate::prelude::{
    default::GPPDefaultFunctionsNativeLibrary, float::GPPFloatLibrary, int::GPPIntLibrary,
    str::GPPStringNativeLibrary,
};

pub mod default;
pub mod float;
pub mod int;
pub mod list;
pub mod str;

pub struct GPPPreludeLibrary;

impl NativeLibrary for GPPPreludeLibrary {
    fn register_functions(&self, bridge: &mut dyn NativeBridge) {
        let mut native_libs: Vec<Box<dyn NativeLibrary>> = Vec::new();

        native_libs.push(Box::new(GPPFloatLibrary {}));
        native_libs.push(Box::new(GPPIntLibrary {}));
        // native_libs.push(Box::new(GPPListLibrary {}));
        native_libs.push(Box::new(GPPStringNativeLibrary {}));
        native_libs.push(Box::new(GPPDefaultFunctionsNativeLibrary {}));

        for lib in native_libs {
            lib.register_functions(bridge);
        }
    }
}
