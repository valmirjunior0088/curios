use crate::{cont, core};

pub struct ModuleEmitter<'a> {
    module: &'a mut cont::Module,
}

impl<'a> ModuleEmitter<'a> {
    pub fn new(module: &'a mut cont::Module) -> Self {
        Self { module }
    }

    pub fn emit_term(&mut self, term: &core::Term) {}
}
