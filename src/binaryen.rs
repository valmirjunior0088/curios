//! Wasm-level optimization via the statically linked Binaryen library.
//!
//! This is deliberately the last stage of the pipeline: it consumes and
//! produces serialized module bytes, after `wasm::to_bytes`, and knows
//! nothing about any Curios IR. Semantic optimization belongs in `optm`.

mod sys;

#[cfg(test)]
mod tests;

use std::{ptr, slice, sync::Mutex};

pub fn optimize(mut bytes: Vec<u8>) -> Vec<u8> {
    // Binaryen's optimize/shrink/closed-world settings are process-global
    // and its optimizer is not thread-safe across modules, so the whole
    // sequence is serialized.
    static LOCK: Mutex<()> = Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());

    // Binaryen aborts the process on a malformed binary instead of
    // returning an error; the input always comes from `wasm::to_bytes`,
    // so a crash here means an emitter bug that should be loud anyway.
    unsafe {
        // Exactly the features the pipeline targets and Wasmtime's engine
        // enables — not `BinaryenFeatureAll`, which lets the optimizer emit
        // post-GC proposals (e.g. exact reference types) that the runtime
        // does not accept.
        let features = sys::BinaryenFeatureMutableGlobals()
            | sys::BinaryenFeatureNontrappingFPToInt()
            | sys::BinaryenFeatureBulkMemory()
            | sys::BinaryenFeatureSignExt()
            | sys::BinaryenFeatureTailCall()
            | sys::BinaryenFeatureReferenceTypes()
            | sys::BinaryenFeatureMultivalue()
            | sys::BinaryenFeatureGC();

        let module =
            sys::BinaryenModuleReadWithFeatures(bytes.as_mut_ptr().cast(), bytes.len(), features);

        // The module neither escapes references nor is dynamically linked,
        // which closed-world GC optimizations require to be effective.
        sys::BinaryenSetClosedWorld(true);
        sys::BinaryenSetOptimizeLevel(2);
        sys::BinaryenSetShrinkLevel(1);

        sys::BinaryenModuleOptimize(module);

        assert!(
            sys::BinaryenModuleValidate(module),
            "Binaryen produced an invalid module"
        );

        let result = sys::BinaryenModuleAllocateAndWrite(module, ptr::null());
        let optimized = slice::from_raw_parts(result.binary.cast(), result.binary_bytes).to_vec();

        sys::free(result.binary);

        if !result.source_map.is_null() {
            sys::free(result.source_map.cast());
        }

        sys::BinaryenModuleDispose(module);

        optimized
    }
}
