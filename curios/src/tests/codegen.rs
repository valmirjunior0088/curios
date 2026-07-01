//! End-to-end codegen tests: build a `cont::Module` directly, lower it to wasm
//! via `crate::cont::to_wasm`, and execute it through the compiler's run path
//! (`crate::run_wasm`). Executing emitted wasm needs the runtime; these tests
//! live here, alongside the rest of the integration suite, because `curios`
//! can depend on `curios-rt` without a cycle (`curios-rt` depends only on
//! `curios-abi`).

use crate::cont::{self, to_wasm};

mod code;
mod module;

pub fn printed(module: &cont::Module) -> String {
    let (system, io) = curios_rt::MockHost::builder().build();
    crate::run_wasm(&to_wasm(module), system).expect("run failed");
    String::from_utf8(io.output()).unwrap()
}

/// Run `module` and return the value it exits with via `Proc/exit`. Codegen
/// fixtures surface a computed `Nat` by exiting with it; the i31 payload crosses
/// the exit code unsigned.
pub fn i32_result(module: &cont::Module) -> i32 {
    let (system, _io) = curios_rt::MockHost::builder().build();
    crate::run_wasm(&to_wasm(module), system).expect("run failed")
}

/// Like [`i32_result`] but for a signed `Int` result: the exit code carries the
/// unsigned 31-bit payload, so sign-extend bit 30 back into the full `i32`.
pub fn int_result(module: &cont::Module) -> i32 {
    (i32_result(module) << 1) >> 1
}

/// Run `module` and decode the four little-endian bytes it writes to stdout as
/// an `f32`. Fixtures surface a computed `Flt` via `Flt/to_le_bin`.
pub fn f32_result(module: &cont::Module) -> f32 {
    let (system, io) = curios_rt::MockHost::builder().build();
    crate::run_wasm(&to_wasm(module), system).expect("run failed");
    let bytes = io.output();
    f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
}

pub fn traps(module: &cont::Module) -> bool {
    let (system, _io) = curios_rt::MockHost::builder().build();

    crate::run_wasm(&to_wasm(module), system).is_err()
}
