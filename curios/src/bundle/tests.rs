use super::LAUNCHER;

/// Markers of a native backend, as they appear in a linked image: `cranelift-codegen`'s panic-location paths, its Rust symbol names, and Binaryen's.
///
/// `cranelift-codegen` rather than `cranelift`: the slim launcher legitimately links `cranelift-bitset`, `-bforest` and `-entity`, which are data structures wasmtime's *runtime* uses. The compiler backend proper is what must never be here.
const BACKEND_MARKERS: [&[u8]; 3] = [b"cranelift-codegen", b"cranelift_codegen", b"binaryen"];

/// The launcher must not contain a native compiler, checked against the bytes that actually ship.
///
/// This is deliberately not a `cargo tree` assertion. The graph answers a question about dependencies; [`LAUNCHER`] is the exact image appended into every bundled executable, so checking it holds however the feature and crate graph are later rearranged — including the case where a `cranelift` feature is added to `curios-runtime` and something enables it by default.
#[test]
fn the_embedded_launcher_carries_no_native_backend() {
    for marker in BACKEND_MARKERS {
        assert!(
            !LAUNCHER
                .windows(marker.len())
                .any(|window| window == marker),
            "the embedded launcher contains `{}` — it was not built by `cargo x runtime`, or a default feature now reaches a native backend",
            String::from_utf8_lossy(marker),
        );
    }
}

/// The size backstop, independent of whether the profile retains any string at all.
///
/// The ceiling is calibrated against a *Cranelift-linked launcher*, not against the `curios` binary — that distinction is the whole point of the number. `curios` is 57 MiB because it also carries Binaryen and the compiler, and a ceiling drawn from it (16 MiB was the first guess) sits above the 11 MiB a Cranelift-only launcher weighs, so it would never fire. See [`launcher_guard_positive_control`] for the measurement.
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
/// A guard that has never been shown to bite is decoration, and the size ceiling in particular was wrong on its first calibration. Reproducing a failing image needs a manifest edit, so this test reports the *current* image's figures and records what a compiler-carrying one measured; compare the two.
///
/// To rebuild the failing image (about 35 seconds):
///
/// 1. add `cranelift = ["wasmtime/cranelift"]` to `curios-runtime`'s `[features]`
/// 2. `cargo build --release -p curios-runtime --target <triple> --features cranelift`
/// 3. inspect `target/<triple>/release/curios-runtime`
/// 4. revert the manifest and re-run `cargo x runtime`
///
/// **2026-08-14, aarch64-apple-darwin, release profile (no `[profile]` section, so no stripping).** Slim: 3,633,792 bytes, zero occurrences of every marker. Cranelift-linked: **11,693,168 bytes**, `cranelift-codegen` ×113 and `cranelift_codegen` ×8. Both guards were run against that image and both failed, which is the evidence this test exists to record. Note that dead-code elimination does *not* remove the backend even though the launcher calls no compiling API — enabling the feature is enough.
///
/// The ceiling was set *after* that measurement, not before: the first guess of 16 MiB came from the 57 MiB `curios` binary and sat above the 11 MiB a Cranelift-only launcher weighs, so it would have passed. A ceiling calibrated against the wrong image is the failure mode this whole test guards against.
///
/// Re-run this after bumping wasmtime or changing the release profile: the string markers depend on panic-location paths surviving, which `strip = true` would not change (they are `.rodata` literals, not symbols) but a toolchain change might.
#[test]
#[ignore = "measurement: reports the launcher's figures rather than asserting"]
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
