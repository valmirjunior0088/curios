//! Where the workspace and its build products live: the root every other path is derived from, what a `--release` build left behind, where a filed product and the list of its inputs go, and the triples a recipe builds for.
//!
//! Nothing here runs anything or touches a file. A path is a fact about the tree; acting on one is a recipe's business.

use std::{
    env, fs,
    path::{Path, PathBuf},
    time::SystemTime,
};

/// The workspace root: this crate's manifest directory is one level below it.
pub(crate) fn root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("xtask sits one level below the workspace root")
}

/// Where cargo puts what it builds, honoring the same override cargo itself honors.
pub(crate) fn target_directory() -> PathBuf {
    match env::var_os("CARGO_TARGET_DIR") {
        Some(directory) => root().join(directory),
        None => root().join("target"),
    }
}

/// What a `--release --target <triple>` build left for `name` — the file a recipe files or feeds onward.
pub(crate) fn built(target: &str, name: &str) -> PathBuf {
    target_directory().join(target).join("release").join(name)
}

/// A crate's filed build product for one triple: `<crate>/.artifacts/<triple>`, the triple as the file name, as `curios/build.rs` expects.
pub(crate) fn artifact(crate_dir: &str, target: &str) -> PathBuf {
    root().join(crate_dir).join(".artifacts").join(target)
}

/// What a filed build product was built from, beside it: `<crate>/.artifacts/<triple>.inputs`, one workspace-relative path per line, as `curios/build.rs` reads it.
pub(crate) fn inputs(crate_dir: &str, target: &str) -> PathBuf {
    root()
        .join(crate_dir)
        .join(".artifacts")
        .join(format!("{target}.inputs"))
}

/// When `path` was last written, or `None` for a path that is not there — a recipe deciding whether a filed product is older than what it was made from.
pub(crate) fn modified(path: &Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

/// The triple this tool was built for: the host, which is the one triple every recipe builds for.
pub(crate) const HOST_TRIPLE: &str = env!("CURIOS_HOST_TRIPLE");

/// The triple the browser bundle is built for: the bare Wasm target `wasm-bindgen` binds, which no host is.
pub(crate) const BROWSER_TRIPLE: &str = "wasm32-unknown-unknown";
