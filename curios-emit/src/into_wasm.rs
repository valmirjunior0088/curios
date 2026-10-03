//! The stage's door: the machine graph built, structurized and hoisted, then emitted.

use {
    super::{
        EmissionValueName, ModuleEmitter, Table, hoist_consts, lower, structurize, value_id,
        value_name,
    },
    curios_utilities::grown,
    std::collections::HashMap,
};

/// Lower an optimized CPS module to a wasm-GC module — the pipeline's final stage. The private machine CFG is built, its reducible control structurized into blocks and loops and its constant data hoisted, then a `Table` is computed over the whole module (the name maps, the closure type per `clsr_arities` arity, tuple arities, rope helpers) and `ModuleEmitter` declares the host imports and emits every const, closure, and function, exporting the entry under its emitted name (`func/main` — the entry is always `main`).
pub fn into_wasm(module: &curios_cont::Module) -> curios_wasm::Module {
    // On a segment of its own, as every other stage enters: the emitter below recurses per nested region, and its frames are large enough that a knot's few thousand lines of CPS would outgrow the default test-thread stack.
    grown(|| into_wasm_within(module))
}

fn into_wasm_within(module: &curios_cont::Module) -> curios_wasm::Module {
    curios_profile::profile!("into_wasm");
    let raw = raw_locals(module);
    let machine = lower(module);
    let mut structured = structurize(&machine);
    hoist_consts(&mut structured);
    let mut wasm_module = curios_wasm::Module::new("module");
    ModuleEmitter::new(&Table::new(&structured, &raw), &mut wasm_module).emit_module(&structured);

    wasm_module
}

/// The emission names the representation analysis decided to hold raw, and at which carrier.
///
/// Translated here rather than threaded through the lowerings because both hops are total functions of an index: a machine value *is* its CPS value, and its emission name is that index spelled. Nothing has to be carried along to reconstruct it. A name codegen mints for itself — a hoisted literal, a wrapper argument, a closure shell — is not a CPS value, is therefore absent, and is held behind a reference, which is the correct default.
fn raw_locals(module: &curios_cont::Module) -> HashMap<EmissionValueName, curios_cont::Repr> {
    curios_cont::storage(module)
        .into_iter()
        .filter_map(|(value, storage)| {
            storage
                .raw_carrier()
                .map(|carrier| (value_name(value_id(value)), carrier))
        })
        .collect()
}
