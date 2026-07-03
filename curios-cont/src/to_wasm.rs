mod table;
use table::*;

mod frame;
use frame::*;

mod context;
use context::*;

mod code_emitter;
use code_emitter::*;

mod expr_emitter;
use expr_emitter::*;

mod module_emitter;
use module_emitter::*;

mod rope_emitter;
use rope_emitter::*;

mod types;
pub use types::*;

use curios_wasm::Module;

pub fn to_wasm(cont_module: &crate::Module) -> Module {
    let mut wasm_module = Module::new("module");

    ModuleEmitter::new(&Table::new(cont_module), &mut wasm_module).emit_module(cont_module);

    wasm_module
}
