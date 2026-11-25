use super::*;

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
            "the embedded launcher contains `{}` — it was not built by `cargo xtask compiler launcher`, or its crate now reaches a native backend",
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
