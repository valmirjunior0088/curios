mod data;
use data::*;

mod scope;
use scope::*;

mod context;
use context::*;

mod emitter;
use emitter::*;

mod builder;
use builder::*;

use crate::{cont, wasm};

pub fn to_wasm(cont_module: &cont::Module) -> wasm::Module {
    let mut wasm_module = wasm::Module::new("module");

    Builder::new(&ModuleData::new(cont_module), &mut wasm_module).emit_module(cont_module);

    wasm_module
}
