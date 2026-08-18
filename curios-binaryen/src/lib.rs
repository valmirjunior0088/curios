//! Wasm-level optimization via the statically linked Binaryen library.
//!
//! This is deliberately the last stage of the pipeline: it consumes and produces serialized module bytes, after `wasm::to_bytes`, and knows nothing about any Curios IR. Semantic optimization belongs in `optimize`.
//!
//! [`optimize_with_text`] additionally renders the optimized module through Binaryen's own text writer — the `--print wasm-optm` payload. The text is eyes-only: nothing in the workspace parses it, and the folded s-expression dialect is Binaryen's to change.

mod sys;

use std::{ffi::CStr, ptr, slice, sync::Mutex};

/// Run Binaryen's whole-module optimizer over serialized module bytes (optimize level 2, shrink level 1, closed world) and return the re-encoded binary. The feature set is pinned to exactly what the emitter produces and Wasmtime's engine enables, so the optimizer can never introduce a post-GC proposal the runtime rejects. Safe to call concurrently — Binaryen's settings are process-global and its optimizer is not thread-safe, so calls serialize behind an internal lock — but `bytes` must be a well-formed module: Binaryen aborts the process on malformed input instead of returning an error, which is acceptable only because the input always comes from `wasm::to_bytes`.
pub fn optimize(bytes: Vec<u8>, names: bool) -> Vec<u8> {
    let (optimized, _) = run_optimizer(bytes, names, false);

    optimized
}

/// [`optimize`], additionally returning the optimized module rendered by Binaryen's own text writer — captured from the same in-memory module the optimizer just rewrote, so the text is the optimizer's ground truth rather than any reader's reconstruction of its output. Callers observing the result pass `names: true`, or the dump reads as bare indices.
pub fn optimize_with_text(bytes: Vec<u8>, names: bool) -> (Vec<u8>, String) {
    let (optimized, text) = run_optimizer(bytes, names, true);

    (optimized, text.expect("requested text"))
}

/// The one optimizer session behind both entry points: read, configure, optimize, render the text while the module is still alive when asked, re-encode, dispose.
fn run_optimizer(mut bytes: Vec<u8>, names: bool, want_text: bool) -> (Vec<u8>, Option<String>) {
    static LOCK: Mutex<()> = Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());

    unsafe {
        // Exactly the features the pipeline targets and Wasmtime's engine enables — not `BinaryenFeatureAll`, which lets the optimizer emit post-GC proposals (e.g. exact reference types) that the runtime does not accept.
        //
        // `BulkMemoryOpt` is not a choice: Binaryen carves `memory.copy`/`memory.fill` out of bulk memory and asserts that a set holding bulk memory holds the carve-out too. Setting one without the other aborts the process the first time a pass asks — which stayed latent for as long as nothing emitted either instruction.
        let features = sys::BinaryenFeatureMutableGlobals()
            | sys::BinaryenFeatureNontrappingFPToInt()
            | sys::BinaryenFeatureBulkMemory()
            | sys::BinaryenFeatureBulkMemoryOpt()
            | sys::BinaryenFeatureSignExt()
            | sys::BinaryenFeatureTailCall()
            | sys::BinaryenFeatureReferenceTypes()
            | sys::BinaryenFeatureMultivalue()
            | sys::BinaryenFeatureMultiMemory()
            | sys::BinaryenFeatureMemory64()
            | sys::BinaryenFeatureGC();

        let module =
            sys::BinaryenModuleReadWithFeatures(bytes.as_mut_ptr().cast(), bytes.len(), features);

        // The module neither escapes references nor is dynamically linked, which closed-world GC optimizations require to be effective.
        sys::BinaryenSetClosedWorld(true);
        sys::BinaryenSetOptimizeLevel(2);
        sys::BinaryenSetShrinkLevel(1);
        // Off by default, and deliberately: the name section is 22 KB on a program the size of `trees`, which a shipped binary should not carry. But dropping it is what left every runtime profile of a Curios program showing bare addresses, so the caller that is profiling asks for it back.
        sys::BinaryenSetDebugInfo(names);
        // The buffered text writer never reaches a terminal, so this should already be moot — but it is a process-global setting like every other one above, so it is pinned rather than left to a tty probe.
        sys::BinaryenSetColorsEnabled(false);

        sys::BinaryenModuleOptimize(module);

        assert!(
            sys::BinaryenModuleValidate(module),
            "Binaryen produced an invalid module"
        );

        let text = want_text.then(|| {
            let pointer = sys::BinaryenModuleAllocateAndWriteText(module);
            let text = CStr::from_ptr(pointer).to_string_lossy().into_owned();

            sys::free(pointer.cast());

            text
        });

        let result = sys::BinaryenModuleAllocateAndWrite(module, ptr::null());
        let optimized = slice::from_raw_parts(result.binary.cast(), result.binary_bytes).to_vec();

        sys::free(result.binary);

        if !result.source_map.is_null() {
            sys::free(result.source_map.cast());
        }

        sys::BinaryenModuleDispose(module);

        (optimized, text)
    }
}
