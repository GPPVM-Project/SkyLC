use std::rc::Rc;

use skyl_data::bytecode::Chunk;

#[derive(Debug)]
pub struct Frame {
    pub chunk: Rc<Chunk>,
    pub sp: usize,
    pub ip: usize,
    pub fp: usize,
}

impl Frame {
    pub fn new(chunk: Rc<Chunk>) -> Self {
        Self {
            chunk,
            sp: 0,
            ip: 0,
            fp: 0,
        }
    }

    pub fn set_ip(&mut self, ip: usize) {
        self.ip = ip;
    }

    pub fn set_sp(&mut self, sp: usize) {
        self.sp = sp;
    }

    pub fn set_fp(&mut self, fp: usize) {
        self.fp = fp;
    }
}
