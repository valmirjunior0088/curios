use super::LAUNCHER;

/// Markers of a native backend, as they appear in a linked image: `cranelift-codegen`'s panic-location paths, its Rust symbol names, and Binaryen's.
///
/// `cranelift-codegen` rather than `cranelift`: the slim launcher legitimately links `cranelift-bitset`, `-bforest` and `-entity`, which are data structures wasmtime's *runtime* uses. The compiler backend proper is what must never be here.
const BACKEND_MARKERS: [&[u8]; 3] = [b"cranelift-codegen", b"cranelift_codegen", b"binaryen"];

/// The launcher must not contain a native compiler, checked against the bytes that actually ship.
///
/// This is deliberately not a `cargo tree` assertion. The graph answers a question about dependencies; [`LAUNCHER`] is the exact image appended into every bundled executable, so checking it holds however the feature and crate graph are later rearranged — including the case where the launcher's crate comes to reach a backend.
#[test]
fn the_embedded_launcher_carries_no_native_backend() {
    for marker in BACKEND_MARKERS {
        assert!(
            !LAUNCHER
                .windows(marker.len())
                .any(|window| window == marker),
            "the embedded launcher contains `{}` — it was not built by `cargo xtask launcher build`, or its crate now reaches a native backend",
            String::from_utf8_lossy(marker),
        );
    }
}

/// The size backstop, independent of whether the profile retains any string at all.
///
/// The ceiling is calibrated against a *Cranelift-linked launcher*, not against the `curios` binary — that distinction is the whole point of the number. `curios` is 132 MiB because it also carries Binaryen and the compiler, and a ceiling drawn from it sits above the 16 MiB a Cranelift-linked launcher weighs, so it would never fire. See [`launcher_guard_positive_control`] for the measurement.
#[test]
fn the_embedded_launcher_stays_slim() {
    const CEILING: usize = 6 * 1024 * 1024;

    assert!(
        LAUNCHER.len() < CEILING,
        "the embedded launcher is {} bytes, over the {CEILING}-byte ceiling",
        LAUNCHER.len(),
    );
}

/// The positive control for the two guards above — the evidence that they can fail.
///
/// A guard that has never been shown to bite is decoration. A failing image is a build of its own, so this test reports the *current* image's figures and records what a compiler-carrying one measured; compare the two.
///
/// To rebuild the failing image (about a minute):
///
/// 1. `cargo build --release -p curios-native --target <triple> --features wasmtime/cranelift`
/// 2. inspect `target/<triple>/release/curios-native`
/// 3. re-run `cargo xtask launcher build`, which rebuilds that path without the backend
///
/// **x86_64-unknown-linux-gnu, release profile (no `[profile]` section, so no stripping), counted at `00627ea06`.** Slim: 4,944,856 bytes, zero occurrences of every marker. Cranelift-linked: **17,197,648 bytes**, `cranelift-codegen` ×77 and `cranelift_codegen` ×6,579, so both guards fail against it, which is the evidence this test exists to record. `curios` itself, built at the same commit, is 138,664,896 bytes. Note that dead-code elimination does *not* remove the backend even though the launcher calls no compiling API — enabling the feature is enough.
///
/// Re-run this after bumping wasmtime or changing the release profile: the string markers depend on panic-location paths surviving, which `strip = true` would not change (they are `.rodata` literals, not symbols) but a toolchain change might.
#[test]
#[ignore = "measurement, counted: reports the launcher's figures rather than asserting"]
fn launcher_guard_positive_control() {
    let counts = BACKEND_MARKERS.map(|marker| {
        let hits = LAUNCHER
            .windows(marker.len())
            .filter(|window| *window == marker)
            .count();

        format!("{}={hits}", String::from_utf8_lossy(marker))
    });

    println!(
        "embedded launcher: {} bytes ({:.1} MiB), {}",
        LAUNCHER.len(),
        LAUNCHER.len() as f64 / (1024.0 * 1024.0),
        counts.join(", "),
    );
}
