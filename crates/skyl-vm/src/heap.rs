use skyl_data::{
    memory::{GcRef, RefColor},
    objects::{Object, Value},
};
use std::fmt::Debug;

pub struct Heap {
    pub objects: Vec<GcRef>,
    allocated_bytes: usize,
    gc_threshold: usize,
    next_gc: usize,
}

impl Debug for Heap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Heap")
            .field("objects", &self.objects)
            .field("allocated_bytes", &self.allocated_bytes)
            .field("next_gc", &self.next_gc)
            .finish()
    }
}

impl Heap {
    pub fn new(initial_size: usize) -> Self {
        Self {
            objects: Vec::new(),
            allocated_bytes: 0,
            next_gc: initial_size,
            gc_threshold: initial_size,
        }
    }

    pub fn allocated_bytes(&self) -> usize {
        self.allocated_bytes
    }

    pub fn allocate<T: Object + 'static>(
        &mut self,
        obj: T,
        mut gc_callback: impl FnMut(&mut Heap),
    ) -> GcRef {
        let size = obj.get_size();
        self.allocated_bytes += size;

        if self.allocated_bytes > self.next_gc {
            gc_callback(self);
            self.next_gc = (self.allocated_bytes * 2).max(self.gc_threshold);
            println!(
                "GC has been executed, now with {} allocated bytes. Next in {}",
                self.allocated_bytes, self.next_gc
            );
        }

        let boxed: Box<dyn Object> = Box::new(obj);
        let raw = Box::into_raw(boxed);
        let gc_ref = GcRef::new(raw);
        self.objects.push(gc_ref.clone());

        gc_ref
    }

    pub fn collect_garbage(&mut self, _roots: &mut [Value]) {
        todo!();
        // for gc_ref in &self.objects {
        //     gc_ref.color.set(RefColor::White);
        // }

        // let mut grey_stack: Vec<GcRef> = Vec::new();

        // for value in roots.iter() {
        // if let Value::Object(obj) = value {
        //     if obj.color.get() == RefColor::White {
        //         obj.color.set(RefColor::Grey);
        //         obj.age.set(obj.age.get() + 1);
        //         grey_stack.push(obj.clone());
        //     }
        // }
        // }

        // while let Some(gc_ref) = grey_stack.pop() {
        //     gc_ref.color.set(RefColor::Black);

        //     unsafe {
        //         gc_ref.borrow().trace_references(&mut |child: &GcRef| {
        //             if child.color.get() == RefColor::White {
        //                 child.color.set(RefColor::Grey);
        //                 grey_stack.push(child.clone());
        //             }
        //         });
        //     }
        // }

        // self.objects.retain(|gc_ref| {
        //     if gc_ref.color.get() == RefColor::White {
        //         let size = unsafe { gc_ref.borrow() }.get_size();

        //         unsafe { drop(Box::from_raw(gc_ref.obj)) };
        //         self.allocated_bytes -= size;
        //         false
        //     } else {
        //         true
        //     }
        // });
    }

    pub fn clear(&mut self) {
        while let Some(gc_ref) = self.objects.pop() {
            gc_ref.color.set(RefColor::Black);
            let size = unsafe { gc_ref.borrow() }.get_size();
            self.allocated_bytes -= size;
            unsafe { drop(Box::from_raw(gc_ref.obj)) };
        }
    }
}
