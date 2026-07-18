mod symbols;
pub(crate) use symbols::*;

mod model;
pub(crate) use model::*;

mod table;
pub(crate) use table::*;

mod frame;
pub(crate) use frame::*;

mod context;
pub(crate) use context::*;

mod code_emitter;
pub(crate) use code_emitter::*;

mod structure;
pub(crate) use structure::*;

mod expr_emitter;
pub(crate) use expr_emitter::*;

mod module_emitter;
pub(crate) use module_emitter::*;

mod rope_emitter;
pub(crate) use rope_emitter::*;

mod types;
pub(crate) use types::*;

#[cfg(test)]
mod emit_tests;

/// Emit the (optimized) cont module as a wasm-GC module — the pipeline's final lowering. A `Table` is computed over the whole module first (the name maps, the closure type per `clsr_arities` arity, tuple arities, rope helpers), then `ModuleEmitter` declares the host imports and emits every const, closure, and function, exporting the entry under its emitted name (`func/main` — the entry is always `main`).
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub(crate) fn emit(cont_module: &EmissionModule) -> curios_wasm::Module {
    let mut wasm_module = curios_wasm::Module::new("module");

    ModuleEmitter::new(&Table::new(cont_module), &mut wasm_module).emit_module(cont_module);

    wasm_module
}
