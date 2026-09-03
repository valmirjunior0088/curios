//! Supplies the runtime launcher's path to the `curios` binary.
//!
//! The launcher is produced by `cargo x runtime`, which builds `curios-runtime` in its own Cargo invocation so that workspace feature unification cannot reach it — this crate enables `curios-runtime/cranelift`, and a launcher built alongside it would carry a compiler. That isolation needs a second invocation, so it cannot happen here; this script only locates what that invocation left behind, and refuses clearly when it is absent or stale.
//!
//! The path is `.artifacts/<triple>` beside this crate rather than anywhere under Cargo's target tree, so `cargo clean` does not delete it and no build script has to reconstruct Cargo's internal directory layout to find it. `CARGO_MANIFEST_DIR` is a documented interface; `OUT_DIR`'s ancestry is not.

use std::{env, fs, path::Path, path::PathBuf, time::SystemTime};

/// When `path` was last modified, or `None` if it cannot be read.
///
/// Used to decide whether the launcher predates the sources it was built from. Read failures answer `None` rather than panicking: this drives a *warning*, and a build script that cannot stat a file has no business failing a build over it.
fn modified(path: &Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let target_triple = env::var("TARGET").unwrap();
    let artifacts = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join(".artifacts");
    let launcher = artifacts.join(&target_triple);
    let inputs = artifacts.join(format!("{target_triple}.inputs"));

    // The triple is the *file name* so two cross-target builds in one checkout cannot overwrite each other. A launcher of the wrong architecture passes both of `bundle.rs`'s guards — it is slim and carries no backend marker — so the failure would reach a user as a bundled executable that does not run, rather than as a build error here.
    println!("cargo:rerun-if-changed={}", launcher.display());
    println!("cargo:rerun-if-changed={}", inputs.display());

    // `cargo::error` needs the *two*-colon form. `cargo:error` is parsed as an unknown metadata key and discarded without a word, so the single-colon spelling everywhere else in this file is not a style this line may be made to match.
    if !launcher.is_file() {
        println!(
            "cargo::error=the {target_triple} runtime launcher is missing: run `cargo x build` to build it and this crate together, or `cargo x runtime` for the launcher alone"
        );
        return;
    }

    // Nothing rebuilds the launcher when its sources change, because it is produced by a separate Cargo invocation this build cannot trigger. Without this check that staleness is *silent*: the file is unchanged, so this script does not re-run, and the old bytes are embedded again. The guards do not catch it either — a stale launcher is still slim and still marker-free.
    //
    // The sources are what `cargo x runtime` filed beside the launcher: cargo's dep-info for the binary — every file rustc read for it, across every workspace crate it embeds, and nothing rustc did not read — and the lock file, for a dependency bump. A list this script kept by hand named whole crates, so an edit to a runtime *test* file left a warning that `cargo x runtime` could not clear, having nothing to rebuild; the recipe now refreshes the launcher's timestamp in exactly that case, which is why the comparison below is against the listed files and the recipe's, and not some third set.
    let workspace = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .expect("the workspace root is this crate's parent")
        .to_path_buf();
    let Ok(listed) = fs::read_to_string(&inputs) else {
        println!(
            "cargo::warning=the {target_triple} runtime launcher's inputs are not filed beside it; run `cargo x runtime` to file them"
        );
        println!("cargo:rustc-env=CURIOS_RUNTIME_BIN={}", launcher.display());
        return;
    };
    let sources = listed
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| workspace.join(line))
        .collect::<Vec<_>>();
    for source in &sources {
        println!("cargo:rerun-if-changed={}", source.display());
    }

    let built = modified(&launcher);
    let newest = sources.iter().filter_map(|source| modified(source)).max();

    // A warning rather than an error, because modification times are approximate — a fresh checkout or a `touch` can order these wrongly, and refusing the build on that would be worse than the staleness it guards against. The direction that matters is that a real edit stops being invisible.
    if let (Some(built), Some(newest)) = (built, newest)
        && built < newest
    {
        println!(
            "cargo::warning=the {target_triple} runtime launcher is older than the sources it embeds; run `cargo x runtime` to rebuild it"
        );
    }

    println!("cargo:rustc-env=CURIOS_RUNTIME_BIN={}", launcher.display());
}
