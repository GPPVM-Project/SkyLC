use std::{cell::Cell, fmt::Debug};

use crate::objects::Object;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RefColor {
    Grey,
    White,
    Black,
}

pub struct GcRef {
    pub obj: *mut dyn Object,
    pub color: Cell<RefColor>,
    pub age: Cell<usize>,
}

impl Debug for GcRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GcRef")
            .field("obj", &self.obj)
            .field("color", &self.color.get())
            .field("age", &self.age.get())
            .finish()
    }
}

impl Clone for GcRef {
    fn clone(&self) -> Self {
        Self {
            obj: self.obj,
            color: Cell::new(self.color.get()),
            age: Cell::new(self.age.get()),
        }
    }
}

impl GcRef {
    pub fn new(obj: *mut dyn Object) -> Self {
        Self {
            obj,
            color: Cell::new(RefColor::White),
            age: Cell::new(0),
        }
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn borrow(&self) -> &dyn Object {
        unsafe { &*self.obj }
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn borrow_mut(&mut self) -> &mut dyn Object {
        unsafe { &mut *self.obj }
    }
}
