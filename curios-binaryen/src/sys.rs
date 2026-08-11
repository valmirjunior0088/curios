//! Raw bindings to the subset of Binaryen's C API (`binaryen-c.h`) that [`optimize`](super::optimize) needs. The static library is built by `build.rs` from a downloaded source release — see its module doc for the download/verify/extract/CMake flow.

use std::ffi::{c_char, c_int, c_void};

/// Opaque handle to a Binaryen module (`BINARYEN_REF(Module)`).
#[repr(C)]
pub(crate) struct BinaryenModule {
    _private: [u8; 0],
}

pub(crate) type BinaryenModuleRef = *mut BinaryenModule;

pub(crate) type BinaryenFeatures = u32;

/// Buffers are allocated with `malloc()` and must be released with [`free`].
#[repr(C)]
pub(crate) struct BinaryenModuleAllocateAndWriteResult {
    pub(crate) binary: *mut c_void,
    pub(crate) binary_bytes: usize,
    pub(crate) source_map: *mut c_char,
}

unsafe extern "C" {
    pub(crate) fn BinaryenFeatureMutableGlobals() -> BinaryenFeatures;

    pub(crate) fn BinaryenFeatureNontrappingFPToInt() -> BinaryenFeatures;

    pub(crate) fn BinaryenFeatureBulkMemory() -> BinaryenFeatures;

    pub(crate) fn BinaryenFeatureSignExt() -> BinaryenFeatures;

    pub(crate) fn BinaryenFeatureTailCall() -> BinaryenFeatures;

    pub(crate) fn BinaryenFeatureReferenceTypes() -> BinaryenFeatures;

    pub(crate) fn BinaryenFeatureMultivalue() -> BinaryenFeatures;

    pub(crate) fn BinaryenFeatureGC() -> BinaryenFeatures;

    pub(crate) fn BinaryenModuleReadWithFeatures(
        input: *mut c_char,
        input_size: usize,
        feature_set: BinaryenFeatures,
    ) -> BinaryenModuleRef;

    /// Applies to all modules, globally.
    pub(crate) fn BinaryenSetOptimizeLevel(level: c_int);

    /// Applies to all modules, globally.
    pub(crate) fn BinaryenSetShrinkLevel(level: c_int);

    /// Applies to all modules, globally.
    pub(crate) fn BinaryenSetClosedWorld(on: bool);

    /// Whether the writer preserves the name section. Applies to all modules, globally, and defaults to *off* — which silently discards the names `curios-wasm` emitted, and with them any hope of a readable runtime profile.
    pub(crate) fn BinaryenSetDebugInfo(on: bool);

    pub(crate) fn BinaryenModuleOptimize(module: BinaryenModuleRef);

    pub(crate) fn BinaryenModuleValidate(module: BinaryenModuleRef) -> bool;

    pub(crate) fn BinaryenModuleAllocateAndWrite(
        module: BinaryenModuleRef,
        source_map_url: *const c_char,
    ) -> BinaryenModuleAllocateAndWriteResult;

    pub(crate) fn BinaryenModuleDispose(module: BinaryenModuleRef);

    /// libc `free`, for the buffers returned by [`BinaryenModuleAllocateAndWrite`].
    pub(crate) fn free(pointer: *mut c_void);
}
