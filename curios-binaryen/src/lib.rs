//! Wasm-level optimization via the statically linked Binaryen library.
//!
//! This is deliberately the last stage of the pipeline: it consumes and produces serialized module bytes, after `curios_wasm::to_bytes`, and knows nothing about any Curios IR. Semantic optimization belongs to `curios-ersd`'s and `curios-cont`'s optimizers.
//!
//! [`optimize`] hands its observer the optimized module while it is alive, as an [`Optimized`] that renders through Binaryen's own text writer when it is formatted — the `wonder stage wasm-optm` payload. The text is eyes-only: nothing in the workspace parses it, and the folded s-expression dialect is Binaryen's to change.

mod sys;

use std::{ffi::CStr, fmt, marker::PhantomData, ptr, slice, sync::Mutex};

/// Run Binaryen's whole-module optimizer over serialized module bytes (optimize level 2, shrink level 1, closed world) and return the re-encoded binary. The feature set is pinned to exactly what the emitter produces and Wasmtime's engine enables, so the optimizer can never introduce a post-GC proposal the runtime rejects. Safe to call concurrently — Binaryen's settings are process-global and its optimizer is not thread-safe, so calls serialize behind an internal lock — but `bytes` must be a well-formed module: Binaryen aborts the process on malformed input instead of returning an error, which is acceptable only because the input always comes from `curios_wasm::to_bytes`.
///
/// `observe` is handed the optimized module between the optimizer and its disposal. A caller that wants to see what the optimizer did formats the [`Optimized`] it is given, and passes `names: true`, or the dump reads as bare indices.
pub fn optimize(mut bytes: Vec<u8>, names: bool, observe: impl FnOnce(&Optimized<'_>)) -> Vec<u8> {
    static LOCK: Mutex<()> = Mutex::new(());
    let _guard = LOCK.lock().unwrap_or_else(|poisoned| poisoned.into_inner());

    unsafe {
        // Exactly the features the pipeline targets and Wasmtime's engine enables — not `BinaryenFeatureAll`, which lets the optimizer emit post-GC proposals (e.g. exact reference types) that the runtime does not accept.
        //
        // `BulkMemoryOpt` is not a choice (see its binding): a set holding bulk memory without it aborts the process the first time a pass asks.
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
        // Off by default, and deliberately: the name section is 22 KB on a program the size of `trees`, which a shipped binary should not carry. A runtime profile without it shows bare addresses, so the caller that is profiling asks for it.
        sys::BinaryenSetDebugInfo(names);
        // The buffered text writer never reaches a terminal, but colour is a process-global setting like every other one above, so it is pinned rather than left to a tty probe.
        sys::BinaryenSetColorsEnabled(false);

        sys::BinaryenModuleOptimize(module);

        assert!(
            sys::BinaryenModuleValidate(module),
            "Binaryen produced an invalid module"
        );

        observe(&Optimized {
            module,
            session: PhantomData,
        });

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

/// The optimized module while its optimizer session is still open: what [`optimize`] hands its observer. Formatting it renders the module through Binaryen's own text writer, from the in-memory module the optimizer just rewrote, so an observer that does not look pays nothing.
pub struct Optimized<'a> {
    module: sys::BinaryenModuleRef,
    session: PhantomData<&'a ()>,
}

impl fmt::Display for Optimized<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        // The module is alive for as long as this view is, and the session's lock is held around it.
        unsafe {
            let pointer = sys::BinaryenModuleAllocateAndWriteText(self.module);
            let written = formatter.write_str(&CStr::from_ptr(pointer).to_string_lossy());

            sys::free(pointer.cast());

            written
        }
    }
}
