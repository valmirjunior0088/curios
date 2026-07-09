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
use types::*;

use curios_wasm::Module;

/// Emit the (optimized) cont module as a wasm-GC module — the pipeline's final lowering. A `Table` is computed over the whole module first (the name maps, the closure type per `clsr_arities` arity, tuple arities, rope helpers), then `ModuleEmitter` declares the host imports and emits every const, closure, and function, exporting the entry under its emitted name (`func/main` — the entry is always `main`).
pub fn into_wasm(cont_module: &crate::Module) -> Module {
    let mut wasm_module = Module::new("module");

    ModuleEmitter::new(&Table::new(cont_module), &mut wasm_module).emit_module(cont_module);

    wasm_module
}
