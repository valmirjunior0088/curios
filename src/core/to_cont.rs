mod entropy;
use entropy::*;

mod module_emitter;
use module_emitter::*;

use crate::{cont, core};

pub fn to_cont(core_term: &core::Term) -> cont::Module {
    let mut cont_module = cont::Module::new();

    ModuleEmitter::new(&mut cont_module).emit_term(core_term);

    cont_module
}
