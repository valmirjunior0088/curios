//! Supplies the runtime launcher's path to the `curios` binary.
//!
//! The launcher is produced by `cargo xtask launcher build`, which builds `curios-native` in its own Cargo invocation so that workspace feature unification cannot reach it — this crate enables `curios-native/cranelift`, and a launcher built alongside it would carry a compiler. That isolation needs a second invocation, so it cannot happen here; this script only locates what that invocation left behind, and refuses clearly when it is absent or stale.
//!
//! The path is `.artifacts/<triple>` beside this crate rather than anywhere under Cargo's target tree, so `cargo clean` does not delete it and no build script has to reconstruct Cargo's internal directory layout to find it. `CARGO_MANIFEST_DIR` is a documented interface; `OUT_DIR`'s ancestry is not.

use std::{
    env, fs,
    path::{Path, PathBuf},
    time::SystemTime,
};

/// When `path` was last modified, or `None` if it cannot be read.
///
/// Used to decide whether the launcher predates the sources it was built from. Read failures answer `None` rather than panicking: this drives a *warning*, and a build script that cannot stat a file has no business failing a build over it.
fn modified(path: &Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

/// The sources a dep-info file lists, or `None` where it is not one.
///
/// The format is Makefile syntax — `<binary>: <source> <source> …` on its first line, a space inside a path escaped as `\ ` — and it is a documented interface: cargo writes the file beside what it built for exactly this kind of reader. A relative path is the workspace's, as cargo writes one under `build.dep-info-basedir`.
fn dep_info_sources(text: &str, workspace: &Path) -> Option<Vec<PathBuf>> {
    let (_, listed) = text.lines().next()?.split_once(": ")?;

    let mut sources = Vec::new();
    let mut current = String::new();
    let mut characters = listed.chars();
    while let Some(character) = characters.next() {
        match character {
            '\\' => match characters.next() {
                Some(' ') => current.push(' '),
                Some(other) => {
                    current.push('\\');
                    current.push(other);
                }
                None => current.push('\\'),
            },
            ' ' if !current.is_empty() => sources.push(std::mem::take(&mut current)),
            ' ' => {}
            _ => current.push(character),
        }
    }
    if !current.is_empty() {
        sources.push(current);
    }

    Some(
        sources
            .into_iter()
            .map(|source| workspace.join(source))
            .collect(),
    )
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let target_triple = env::var("TARGET").unwrap();
    let artifacts = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join(".artifacts");
    let launcher = artifacts.join(&target_triple);
    let dep_info = artifacts.join(format!("{target_triple}.d"));

    // The triple is the *file name* so two cross-target builds in one checkout cannot overwrite each other. A launcher of the wrong architecture passes both of `bundle.rs`'s guards — it is slim and carries no backend marker — so the failure would reach a user as a bundled executable that does not run, rather than as a build error here.
    //
    // The dep-info is read and not watched: cargo writes it again on every build of the launcher, fresh or not, so watching it would run this script after every `launcher build`; the launcher's own time moves whenever what the dep-info lists does.
    println!("cargo:rerun-if-changed={}", launcher.display());

    // `cargo::error` needs the *two*-colon form. `cargo:error` is parsed as an unknown metadata key and discarded without a word, so the single-colon spelling everywhere else in this file is not a style this line may be made to match.
    if !launcher.is_file() {
        println!(
            "cargo::error=the {target_triple} runtime launcher is missing: run `cargo xtask compiler build` to build it and this crate together, or `cargo xtask launcher build` for the launcher alone"
        );
        return;
    }

    // Nothing rebuilds the launcher when its sources change, because it is produced by a separate Cargo invocation this build cannot trigger. Without this check that staleness is *silent*: the file is unchanged, so this script does not re-run, and the old bytes are embedded again. The guards do not catch it either — a stale launcher is still slim and still marker-free.
    //
    // The sources are what cargo's dep-info for the binary lists — every file rustc read for it, across every workspace crate it embeds, and nothing rustc did not read — filed beside the launcher by `cargo xtask launcher build`. Both are filed with the times cargo gave them, so a launcher cargo built again after an edit is newer than what it read, whether or not its bytes moved, and the warning below is one `cargo xtask launcher build` always clears. The dep-info names no registry crate, so a dependency bump is not watched here: it reaches the launcher through `xtask`, where every step that builds this crate runs the launcher's build first, and only a bare `cargo build` of this crate after a bump embeds the old launcher without a word.
    let workspace = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .expect("the workspace root is this crate's parent")
        .to_path_buf();
    let Some(sources) = fs::read_to_string(&dep_info)
        .ok()
        .and_then(|text| dep_info_sources(&text, &workspace))
    else {
        println!(
            "cargo::warning=the {target_triple} runtime launcher's dep-info is not filed beside it; run `cargo xtask launcher build` to file it"
        );
        println!("cargo:rustc-env=CURIOS_NATIVE_BIN={}", launcher.display());
        return;
    };
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
            "cargo::warning=the {target_triple} runtime launcher is older than the sources it embeds; run `cargo xtask launcher build` to rebuild it"
        );
    }

    println!("cargo:rustc-env=CURIOS_NATIVE_BIN={}", launcher.display());
}
