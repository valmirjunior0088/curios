//! Optimize and precompile — the native back end, and the reason this crate exists.
//!
//! `curios` is the only product that links Cranelift (through `curios-cranelift`) and the only one that links Binaryen (via `curios-binaryen`); `curios-native` links neither, and `curios-pipeline` stays clear of both. Everything here is downstream of a `curios_wasm::Module` and indifferent to where one came from — the compiling half is `curios_pipeline::compile_with_prelude` and its siblings, next to the fold it configures.
//!
//! No wasmtime operation below is spelled here. `curios_native::validate`, `curios_cranelift::precompile` and `curios_cranelift::compile` are, because they belong with the engine whose configuration decides what a valid module is and what a `.cwasm` is compatible with; this crate names no wasmtime type at all.

use {
    curios_common::{Fingerprint, Report},
    curios_package::Refused,
    std::{fmt, sync::LazyLock},
};

/// What decides whether a payload compiled on this machine runs on another, as a key part — computed once, since it is a property of the process, and handed to `curios-verdict`'s payload family beside every program it files.
///
/// **The payload is the cabinet's first machine-dependent artifact.** A unit is a judgment and travels wherever the compiler binary does; a payload is machine code Cranelift emitted for the host's ISA — the one input neither the compiler digest nor any recorded source file covers. `curios-cranelift` describes the engine, `curios-common`'s `Fingerprint` turns the description into a digest, and `curios-verdict` files under it; this is the one crate that names all three, so the crate that names wasmtime and the crate that names `sha2` stay apart.
pub fn engine() -> &'static str {
    static ENGINE: LazyLock<String> = LazyLock::new(|| {
        let mut fingerprint = Fingerprint::new();
        curios_cranelift::engine_compatibility(&mut fingerprint);

        fingerprint.hex()
    });

    &ENGINE
}

/// Reject a malformed module before Binaryen is handed it.
///
/// Binaryen answers a module it cannot parse with a C++ `assert`, which aborts the *process*. Under `cargo test` that takes down the whole run: no failing test is named and every other test's result is lost with it. The validator is a linear pass over bytes that are about to be optimized and compiled anyway, and it names the offending function and byte offset.
///
/// A panic rather than an error: emitting a module that does not validate is the compiler's own broken contract rather than any program's fault, and no caller has a use for it. The check is wasmtime's rather than a second WasmGC validator on [`curios_wasm::Module`] — writing one would mean writing WasmGC subtyping, precisely the part a hand-rolled validator gets permissively wrong, and a validator that certifies wrongly is worse than none. The gap that leaves is `curios-js`, whose browser build links neither Binaryen nor wasmtime (its dependency on the latter is a dev-dependency, for the bridge tests) and so carries a malformed module all the way to `WebAssembly.compile`.
fn validate(bytes: &[u8]) {
    if let Err(error) = curios_native::validate(bytes) {
        panic!("`into_wasm` emitted a module that does not validate: {error}");
    }
}

/// Optimize `module` with Binaryen and return its bytes, carrying the names every product carries ([`curios_wasm::Names`]'s default, the module's and its functions'): what a sampler, a browser's stacks and a debugger read the emitted code by.
pub fn optimize(module: &curios_wasm::Module) -> Vec<u8> {
    let raw = curios_wasm::to_bytes(module, &curios_wasm::Names::default());
    validate(&raw);

    curios_binaryen::optimize(raw)
}

/// Optimize (Binaryen) and AOT-compile (Cranelift) a module to the `.cwasm` payload a bundled executable carries and a cabinet files, which leaves this process and is read back by [`Compiled::deserialize`](curios_native::Compiled::deserialize). Uses `curios-native`'s shared engine so the precompiled artifact matches the configuration it is deserialized against.
pub fn to_cwasm(module: &curios_wasm::Module) -> Result<Vec<u8>, Box<Report>> {
    let bytes = optimize(module);

    curios_cranelift::precompile(&bytes).map_err(|error| {
        fault(
            "the emitted module could not be compiled to machine code",
            error,
        )
    })
}

/// A fault of the compiler's own, about the program it was building: `what` failed, in the `words` of whatever refused it. Nothing a program wrote earns one, which is why it names no place in the program.
pub fn fault(what: &str, words: impl fmt::Display) -> Box<Report> {
    Refused::of("compiler fault")
        .stating(what)
        .stating(words)
        .about_named("the program")
}
