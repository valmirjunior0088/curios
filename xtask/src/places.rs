//! Where the workspace and its build products live: the root every other path is derived from, cargo's target directory, Cargo's home, and the triple this tool was built for — values the runner fills in a command without a declaration — and the root a relative path is read from.
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

/// Where this crate keeps what outlives a run: the tools the steps run, and the installer a release renders.
pub fn artifacts() -> PathBuf {
    root().join("xtask").join(".artifacts")
}

/// Where cargo puts what it builds, honoring the same override cargo itself honors: the task files' `{!cargo_target_dir}`.
pub fn target_directory() -> PathBuf {
    match env::var_os("CARGO_TARGET_DIR") {
        Some(directory) => root().join(directory),
        None => root().join("target"),
    }
}

/// Cargo's home, where it fetches what a build depends on and builds it from, honoring the same override cargo itself honors: the task files' `{!cargo_home}`.
pub fn cargo_home() -> PathBuf {
    match env::var_os("CARGO_HOME") {
        Some(home) => PathBuf::from(home),
        None => env::home_dir().unwrap_or_default().join(".cargo"),
    }
}

/// The triple this tool was built for, which is the host: the task files' `{!host_triple}`.
pub const HOST_TRIPLE: &str = env!("CURIOS_HOST_TRIPLE");
