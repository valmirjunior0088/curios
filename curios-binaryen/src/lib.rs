//! Wasm-level optimization via the statically linked Binaryen library.
//!
//! It is the pipeline's last stage: it consumes and produces serialized module bytes, after `curios_wasm::to_bytes`, and knows nothing about any Curios IR, as `README.md` decides.

mod sys;
use sys::*;

use std::{ptr, slice, sync::Mutex};

/// Run Binaryen's whole-module optimizer over serialized module bytes (optimize level 2, shrink level 1, closed world) and return the re-encoded binary. The feature set is pinned to exactly what the emitter produces and Wasmtime's engine enables, so the optimizer can never introduce a post-GC proposal the runtime rejects. Safe to call concurrently — Binaryen's settings are process-global and its optimizer is not thread-safe, so calls serialize behind an internal lock — but `bytes` must be a well-formed module: Binaryen aborts the process on malformed input instead of returning an error, which is acceptable only because the input always comes from `curios_wasm::to_bytes`.
///
/// A name section in `bytes` is kept, as far as the optimizer keeps what it names, and none is written where `bytes` has none: which names an artifact carries is decided where it is written (`curios_wasm::Names`).
pub fn optimize(mut bytes: Vec<u8>) -> Vec<u8> {
    curios_tracing::profile!("binaryen_optimize");
    let names = has_name_section(&bytes);
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
        // Binaryen writes names only under its debug flag, and writes an empty section under it where it was handed none, so the flag follows the input.
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

/// Whether `bytes`, a well-formed module, holds a custom section named `name`: read off the section headers, each its id and its length.
fn has_name_section(bytes: &[u8]) -> bool {
    fn leb(bytes: &[u8], mut at: usize) -> Option<(usize, usize)> {
        let (mut value, mut shift) = (0usize, 0);
        loop {
            let byte = *bytes.get(at)?;
            at += 1;
            value |= usize::from(byte & 0x7f) << shift;
            shift += 7;
            if byte < 0x80 {
                return Some((value, at));
            }
        }
    }

    let mut at = 8;
    while at < bytes.len() {
        let id = bytes[at];
        let Some((size, start)) = leb(bytes, at + 1) else {
            return false;
        };
        if id == 0
            && let Some((length, name)) = leb(bytes, start)
            && bytes.get(name..name + length) == Some(b"name".as_slice())
        {
            return true;
        }
        at = start + size;
    }

    false
}
