use skyl_ffi::{NativeBridge, NativeLibrary};

use crate::{
    io::GPPStdIOLibrary, net::GPPNetLibrary, prelude::GPPPreludeLibrary, random::GPPRandomLibrary,
};

pub mod io;
pub mod net;
pub mod prelude;
pub mod random;

pub struct StdLibrary;

impl StdLibrary {
    pub fn register_std_libraries(bridge: &mut dyn NativeBridge) {
        let mut native_libs: Vec<Box<dyn NativeLibrary>> = Vec::new();

        native_libs.push(Box::new(GPPNetLibrary {}));
        native_libs.push(Box::new(GPPPreludeLibrary {}));
        native_libs.push(Box::new(GPPStdIOLibrary {}));
        native_libs.push(Box::new(GPPRandomLibrary {}));

        for lib in native_libs {
            lib.register_functions(bridge);
        }
    }
}
