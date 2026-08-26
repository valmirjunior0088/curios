//! Emitting a self-contained native executable: the embedded slim launcher with the program's `.cwasm` payload appended. The trailing footer layout lives in `curios_runtime::bundle` (shared with the launcher that recovers it); this module only embeds the launcher image and writes the result to disk.

use {
    curios_runtime::append_payload,
    std::{fs, os::unix::fs::PermissionsExt, path::Path},
};

/// The slim `curios-runtime` launcher stub, embedded at build time. Produced by `cargo xtask runtime` (an isolated `--package curios-runtime` build, kept Cranelift/Binaryen-free) into `curios/.artifacts/<triple>`, which is outside Cargo's target tree so `cargo clean` cannot remove it. Absence is caught before this line: `build.rs` emits a `cargo::error` naming the command to run. So `compile` needs no launcher lookup at runtime.
const LAUNCHER: &[u8] = include_bytes!(env!("CURIOS_RUNTIME_BIN"));

/// Build a self-contained executable: the embedded launcher stub with the `.cwasm` payload and its footer appended to the tail (see [`curios_runtime::append_payload`]).
pub(crate) fn emit_exe(cwasm: &[u8], output: &Path) -> Result<(), String> {
    let mut bytes = LAUNCHER.to_vec();

    append_payload(&mut bytes, cwasm);

    // A declared executable lands inside the store, nested under its package, so the directories below it are this write's to create — nothing else has had reason to.
    if let Some(directory) = output.parent()
        && !directory.as_os_str().is_empty()
    {
        fs::create_dir_all(directory)
            .map_err(|error| format!("failed to create {}: {error}", directory.display()))?;
    }

    fs::write(output, &bytes)
        .map_err(|error| format!("failed to write {}: {error}", output.display()))?;

    let mut perms = fs::metadata(output)
        .map_err(|error| format!("failed to stat {}: {error}", output.display()))?
        .permissions();
    perms.set_mode(0o755);
    fs::set_permissions(output, perms)
        .map_err(|error| format!("failed to chmod {}: {error}", output.display()))?;

    // On macOS we do NOT re-sign. The embedded launcher image carries an ad-hoc signature from the linker whose code-limit covers the original image; our payload is appended *past* that limit, so the signature stays valid and the loader ignores the trailing bytes (the launcher reads them via `current_exe`). `codesign --force` would in fact reject the result ("data after signature"), so leaving the original signature in place is both correct and necessary.

    Ok(())
}

#[cfg(test)]
mod tests {
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
                "the embedded launcher contains `{}` — it was not built by `cargo xtask runtime`, or a default feature now reaches a native backend",
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
    /// 4. revert the manifest and re-run `cargo xtask runtime`
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
}
