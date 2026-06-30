//! Raw bindings to the subset of Binaryen's C API (`binaryen-c.h`) that
//! [`optimize`](super::optimize) needs. The static library is built and
//! linked by `build.rs` from the vendored sources in `binaryen/`.

use std::ffi::{c_char, c_int, c_void};

/// Opaque handle to a Binaryen module (`BINARYEN_REF(Module)`).
#[repr(C)]
pub struct BinaryenModule {
    _private: [u8; 0],
}

pub type BinaryenModuleRef = *mut BinaryenModule;

pub type BinaryenFeatures = u32;

/// Buffers are allocated with `malloc()` and must be released with [`free`].
#[repr(C)]
pub struct BinaryenModuleAllocateAndWriteResult {
    pub binary: *mut c_void,
    pub binary_bytes: usize,
    pub source_map: *mut c_char,
}

unsafe extern "C" {
    pub fn BinaryenFeatureMutableGlobals() -> BinaryenFeatures;

    pub fn BinaryenFeatureNontrappingFPToInt() -> BinaryenFeatures;

    pub fn BinaryenFeatureBulkMemory() -> BinaryenFeatures;

    pub fn BinaryenFeatureSignExt() -> BinaryenFeatures;

    pub fn BinaryenFeatureTailCall() -> BinaryenFeatures;

    pub fn BinaryenFeatureReferenceTypes() -> BinaryenFeatures;

    pub fn BinaryenFeatureMultivalue() -> BinaryenFeatures;

    pub fn BinaryenFeatureGC() -> BinaryenFeatures;

    pub fn BinaryenModuleReadWithFeatures(
        input: *mut c_char,
        input_size: usize,
        feature_set: BinaryenFeatures,
    ) -> BinaryenModuleRef;

    /// Applies to all modules, globally.
    pub fn BinaryenSetOptimizeLevel(level: c_int);

    /// Applies to all modules, globally.
    pub fn BinaryenSetShrinkLevel(level: c_int);

    /// Applies to all modules, globally.
    pub fn BinaryenSetClosedWorld(on: bool);

    pub fn BinaryenModuleOptimize(module: BinaryenModuleRef);

    pub fn BinaryenModuleValidate(module: BinaryenModuleRef) -> bool;

    pub fn BinaryenModuleAllocateAndWrite(
        module: BinaryenModuleRef,
        source_map_url: *const c_char,
    ) -> BinaryenModuleAllocateAndWriteResult;

    pub fn BinaryenModuleDispose(module: BinaryenModuleRef);

    /// libc `free`, for the buffers returned by [`BinaryenModuleAllocateAndWrite`].
    pub fn free(pointer: *mut c_void);
}
