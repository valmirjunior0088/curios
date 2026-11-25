//! The launcher's guards' positive control: the size of the image every bundled executable is built from.

use {
    crate::{Measure, read},
    curios::LAUNCHER,
};

/// The positive control for the two guards beside the binary's bundle, `the_embedded_launcher_carries_no_native_backend` and `the_embedded_launcher_stays_slim` (`curios/src/bundle/tests.rs`) — the evidence that they can fail.
///
/// A guard that has never been shown to bite is decoration. A failing image is a build of its own, so this measurement reads the *current* image's size, `embedded launcher`, and records what a compiler-carrying one measured; compare the two. That the current image holds no marker is `the_embedded_launcher_carries_no_native_backend`'s, at every run. `cargo xbench measure launcher_guard_positive_control` takes it.
///
/// To rebuild the failing image (about a minute):
///
/// 1. `cargo build --release -p curios-native --target <triple> --features wasmtime/cranelift`
/// 2. inspect `target/<triple>/release/curios-native`
/// 3. re-run `cargo xtask compiler launcher`, which rebuilds that path without the backend
///
/// **x86_64-unknown-linux-gnu, release profile (no `[profile]` section, so no stripping), counted at `00627ea06`.** Slim: 4,944,856 bytes, zero occurrences of every marker. Cranelift-linked: **17,197,648 bytes**, `cranelift-codegen` ×77 and `cranelift_codegen` ×6,579, so both guards fail against it, which is the evidence this measurement exists to record. `curios` itself, built at the same commit, is 138,664,896 bytes. Note that dead-code elimination does *not* remove the backend even though the launcher calls no compiling API — enabling the feature is enough.
///
/// Re-run this after bumping wasmtime or changing the release profile: the string markers depend on panic-location paths surviving, which `strip = true` would not change (they are `.rodata` literals, not symbols) but a toolchain change might.
pub fn launcher_guard_positive_control() {
    read(
        "embedded launcher",
        Measure::Bytes,
        None,
        LAUNCHER.len() as u64,
    );
}
