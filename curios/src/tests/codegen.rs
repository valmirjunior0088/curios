//! End-to-end codegen tests: build a `curios_cont::Module` directly, lower it to
//! wasm via `curios_cont::into_wasm`, and execute it through the compiler's run path
//! (`crate::run_wasm`). Executing emitted wasm needs the runtime; these tests
//! live here, alongside the rest of the integration suite, because `curios`
//! can depend on `curios-rt` without a cycle (`curios-rt` depends only on
//! `curios-abi`).

mod code_bin;
mod code_flt;
mod code_int;
mod code_lst;
mod code_nat;
mod code_rope;
mod module;
mod parity;

use {
    curios_abi::{ForeignFunction, sys_io},
    curios_cont::into_wasm,
    curios_rt::{ForeignBindings, MockHost},
    std::sync::Arc,
};

/// Shared between `code_int`'s boundary tests and `code_flt`'s `Flt/to_int` trap test.
const MAX_INT: i32 = (1 << 30) - 1;

/// The `io_write` row of the builtin foreign store, for fixtures that
/// hand-build host calls.
pub(super) fn foreign_write() -> Arc<ForeignFunction> {
    sys_io()
        .get("io_write")
        .expect("sys_io defines io_write")
        .clone()
}

pub(super) fn printed(module: &curios_cont::Module) -> String {
    let (system, io) = MockHost::builder().build();
    crate::run_wasm(&into_wasm(module), system, ForeignBindings::empty()).expect("run failed");
    String::from_utf8(io.output()).unwrap()
}

/// Run `module` and return the value it exits with via `Proc/exit`. Codegen
/// fixtures surface a computed `Nat` by exiting with it; the i31 payload crosses
/// the exit code unsigned.
pub(super) fn i32_result(module: &curios_cont::Module) -> i32 {
    let (system, _io) = MockHost::builder().build();
    crate::run_wasm(&into_wasm(module), system, ForeignBindings::empty()).expect("run failed")
}

/// Like [`i32_result`] but for a signed `Int` result: the exit code carries the
/// unsigned 31-bit payload, so sign-extend bit 30 back into the full `i32`.
pub(super) fn int_result(module: &curios_cont::Module) -> i32 {
    (i32_result(module) << 1) >> 1
}

/// Run `module` and decode the four little-endian bytes it writes to stdout as
/// an `f32`. Fixtures surface a computed `Flt` via `Flt/to_le_bin`.
pub(super) fn f32_result(module: &curios_cont::Module) -> f32 {
    let (system, io) = MockHost::builder().build();
    crate::run_wasm(&into_wasm(module), system, ForeignBindings::empty()).expect("run failed");
    let bytes = io.output();
    f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
}

pub(super) fn traps(module: &curios_cont::Module) -> bool {
    let (system, _io) = MockHost::builder().build();

    crate::run_wasm(&into_wasm(module), system, ForeignBindings::empty()).is_err()
}
