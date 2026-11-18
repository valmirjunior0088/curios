//! Wasm-level optimization via the statically linked Binaryen library.
//!
//! It is the pipeline's last stage: it consumes and produces serialized module bytes, after `curios_wasm::to_bytes`, and knows nothing about any Curios IR, as `README.md` decides.

mod sys;
use sys::*;

use std::{ptr, slice, sync::Mutex};

/// Run Binaryen's whole-module optimizer over serialized module bytes (optimize level 2, shrink level 1, closed world) and return the re-encoded binary. The feature set is pinned to exactly what the emitter produces and Wasmtime's engine enables, so the optimizer can never introduce a post-GC proposal the runtime rejects. Safe to call concurrently — Binaryen's settings are process-global and its optimizer is not thread-safe, so calls serialize behind an internal lock — but `bytes` must be a well-formed module: Binaryen aborts the process on malformed input instead of returning an error, which is acceptable only because the input always comes from `curios_wasm::to_bytes`.
///
/// `names` keeps the name section, which a profile of the optimized module reads its functions' names off.
pub fn optimize(mut bytes: Vec<u8>, names: bool) -> Vec<u8> {
    static LOCK: Mutex<()> = Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());

    unsafe {
        // Exactly the features the pipeline targets and Wasmtime's engine enables — not `BinaryenFeatureAll`, which lets the optimizer emit post-GC proposals (e.g. exact reference types) that the runtime does not accept.
        //
        // `BulkMemoryOpt` is not a choice (see its binding): a set holding bulk memory without it aborts the process the first time a pass asks.
        let features = BinaryenFeatureMutableGlobals()
            | BinaryenFeatureNontrappingFPToInt()
            | BinaryenFeatureBulkMemory()
            | BinaryenFeatureBulkMemoryOpt()
            | BinaryenFeatureSignExt()
            | BinaryenFeatureTailCall()
            | BinaryenFeatureReferenceTypes()
            | BinaryenFeatureMultivalue()
            | BinaryenFeatureMultiMemory()
            | BinaryenFeatureMemory64()
            | BinaryenFeatureGC();

        let module =
            BinaryenModuleReadWithFeatures(bytes.as_mut_ptr().cast(), bytes.len(), features);

        // The module neither escapes references nor is dynamically linked, which closed-world GC optimizations require to be effective.
        BinaryenSetClosedWorld(true);
        BinaryenSetOptimizeLevel(2);
        BinaryenSetShrinkLevel(1);
        // Off by default, and deliberately: the name section is 22 KB on a program the size of `trees`, which a shipped binary should not carry. A runtime profile without it shows bare addresses, so the caller that is profiling asks for it.
        BinaryenSetDebugInfo(names);

        BinaryenModuleOptimize(module);

        assert!(
            BinaryenModuleValidate(module),
            "Binaryen produced an invalid module"
        );

        let result = BinaryenModuleAllocateAndWrite(module, ptr::null());
        let optimized = slice::from_raw_parts(result.binary.cast(), result.binary_bytes).to_vec();

        free(result.binary);

        if !result.source_map.is_null() {
            free(result.source_map.cast());
        }

        BinaryenModuleDispose(module);

        optimized
    }
}
