mod table;
use table::*;

mod frame;
use frame::*;

mod context;
use context::*;

mod expr_emitter;
use expr_emitter::*;

mod module_emitter;
use module_emitter::*;

use crate::{cont, wasm};

pub fn to_wasm(cont_module: &cont::Module) -> wasm::Module {
    let mut wasm_module = wasm::Module::new("module");

    ModuleEmitter::new(&Table::new(cont_module), &mut wasm_module).emit_module(cont_module);

    wasm_module
}

#[cfg(test)]
mod tests;
