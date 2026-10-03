//! The Curios WebAssembly emission: [`into_wasm`](into_wasm()) lowers an optimized `curios_cont::Module` to a WebAssembly-GC module, performing delayed closure conversion, verifying a private closed machine CFG, structurizing reducible control into Wasm blocks and loops, and localizing dispatcher fallback to irreducible scopes.
//!
//! Machine lowering recognizes a function's bodyless return continuation in the current-function context, so an ordinary return, `ApplyCont(function.return_cont, [value])`, is emitted as `Return` without allocating a block.
//!
//! Every program value this crate emits lives in a GC reference — a struct, an array, or an `i31` — and never in linear memory. That is [WebAssembly-GC is the only target](../../documentation/design/compilation/webassembly-gc-is-the-only-target.md), and this crate is where it is decided: `curios-wasm` models the whole envelope's linear-memory surface and refuses nothing, so a module emitted here declares no memory at all rather than being kept out of one.
//!
//! The crate owns neither representation it works between, so every name from `curios_cont` and `curios_wasm` is written with its crate.

use {curios_utilities::grown, std::collections::HashMap};

mod machine;
use machine::*;

mod emission;
pub(crate) use emission::*;

mod symbols;
pub(crate) use symbols::*;

mod table;
use table::*;

mod frame;
use frame::*;

mod context;
use context::*;

mod code_emitter;
use code_emitter::*;

mod structure;
use structure::*;

mod expr_emitter;
use expr_emitter::*;

mod hoist;
use hoist::*;

mod immediate;
use immediate::*;

mod refusal;
use refusal::*;

mod module_emitter;
use module_emitter::*;

mod rope_emitter;
use rope_emitter::*;

mod big_emitter;
use big_emitter::*;

mod flt_emitter;
use flt_emitter::*;

mod shorthand;
use shorthand::*;

mod types;
pub use types::*;

#[cfg(test)]
mod aggregate_tests;
#[cfg(test)]
mod foreign_tests;
#[cfg(test)]
mod hoist_tests;
#[cfg(test)]
mod intrinsic_tests;
#[cfg(test)]
mod module_tests;
#[cfg(test)]
mod rope_tests;
#[cfg(test)]
mod test_support;

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
