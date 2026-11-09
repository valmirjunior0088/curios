//! Where the workspace and its build products live: the root every other path is derived from, cargo's target directory, and the triple this tool was built for — the two values the runner fills in a command without a declaration, and the root a relative path is read from.
//!
//! Nothing here runs anything or touches a file. A path is a fact about the tree; acting on one is a step's business.

use std::{
    env,
    path::{Path, PathBuf},
};

/// The workspace root: this crate's manifest directory is one level below it.
pub fn root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("xtask sits one level below the workspace root")
}

/// Where cargo puts what it builds, honoring the same override cargo itself honors: the task files' `{target}`.
pub fn target_directory() -> PathBuf {
    match env::var_os("CARGO_TARGET_DIR") {
        Some(directory) => root().join(directory),
        None => root().join("target"),
    }
}

/// The triple this tool was built for, which is the host: the task files' `{host}`.
pub const HOST_TRIPLE: &str = env!("CURIOS_HOST_TRIPLE");
