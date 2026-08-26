//! Supplies the runtime launcher's path to the `curios` binary.
//!
//! The launcher is produced by `cargo xtask runtime`, which builds `curios-runtime` in its own Cargo invocation so that workspace feature unification cannot reach it — this crate enables `curios-runtime/cranelift`, and a launcher built alongside it would carry a compiler. That isolation needs a second invocation, so it cannot happen here; this script only locates what that invocation left behind, and refuses clearly when it is absent or stale.
//!
//! The path is `.artifacts/<triple>` beside this crate rather than anywhere under Cargo's target tree, so `cargo clean` does not delete it and no build script has to reconstruct Cargo's internal directory layout to find it. `CARGO_MANIFEST_DIR` is a documented interface; `OUT_DIR`'s ancestry is not.

use std::{env, fs, path::Path, path::PathBuf, time::SystemTime};

/// The newest modification time under `path`, or `None` if nothing there can be read.
///
/// Used to decide whether the launcher predates the sources it was built from. Read failures answer `None` rather than panicking: this drives a *warning*, and a build script that cannot stat a file has no business failing a build over it.
fn newest_modification(path: &Path) -> Option<SystemTime> {
    let metadata = fs::metadata(path).ok()?;

    if metadata.is_file() {
        return metadata.modified().ok();
    }

    fs::read_dir(path)
        .ok()?
        .filter_map(|entry| newest_modification(&entry.ok()?.path()))
        .max()
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let target_triple = env::var("TARGET").unwrap();
    let launcher = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .join(".artifacts")
        .join(&target_triple);

    // The triple is the *file name* so two cross-target builds in one checkout cannot overwrite each other. A launcher of the wrong architecture passes both of `bundle.rs`'s guards — it is slim and carries no backend marker — so the failure would reach a user as a bundled executable that does not run, rather than as a build error here.
    println!("cargo:rerun-if-changed={}", launcher.display());

    // `cargo::error` needs the *two*-colon form. `cargo:error` is parsed as an unknown metadata key and discarded without a word, so the single-colon spelling everywhere else in this file is not a style this line may be made to match.
    if !launcher.is_file() {
        println!(
            "cargo::error=the {target_triple} runtime launcher is missing: run `cargo xtask build` to build it and this crate together, or `cargo xtask runtime` for the launcher alone"
        );
        return;
    }

    // Nothing rebuilds the launcher when `curios-runtime` changes, because it is produced by a separate Cargo invocation this build cannot trigger. Without this check that staleness is *silent*: the file is unchanged, so this script does not re-run, and the old bytes are embedded again. The guards do not catch it either — a stale launcher is still slim and still marker-free.
    let runtime = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .expect("the workspace root is this crate's parent")
        .join("curios-runtime");

    for input in ["src", "Cargo.toml"] {
        println!("cargo:rerun-if-changed={}", runtime.join(input).display());
    }

    let built = newest_modification(&launcher);
    let sources = newest_modification(&runtime);

    // A warning rather than an error, because modification times are approximate — a fresh checkout or a `touch` can order these wrongly, and refusing the build on that would be worse than the staleness it guards against. The direction that matters is that a real edit stops being invisible.
    if let (Some(built), Some(sources)) = (built, sources)
        && built < sources
    {
        println!(
            "cargo::warning=the {target_triple} runtime launcher is older than `curios-runtime`'s sources; run `cargo xtask runtime` to rebuild it"
        );
    }

    println!("cargo:rustc-env=CURIOS_RUNTIME_BIN={}", launcher.display());
}
