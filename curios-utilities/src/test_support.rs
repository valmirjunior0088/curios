//! What a test that touches the disk shares: a directory of its own that goes away with the test, however the test ends.

#[cfg(test)]
mod tests;

use std::{
    env, fs,
    ops::Deref,
    path::{Path, PathBuf},
    process,
    time::{SystemTime, UNIX_EPOCH},
};

/// A directory of its own, shared with no other test, removed when dropped.
///
/// Removed by `Drop` rather than by a line at the end of the test, because a failing assertion never reaches that line: unwinding runs destructors, so a test that fails leaves nothing behind either.
pub struct Temporary(PathBuf);

impl Temporary {
    /// A fresh path under the system's temporary directory, named for the `family` of tests and the `name` of this one, unique per process and millisecond.
    ///
    /// **Canonical by construction.** The code under test canonicalizes the locations it compares — the governance walk, the dependency walk, the roots the server publishes — so a path built under the temporary directory as spelled would compare two spellings of one directory, equal only on a host whose temporary directory already happens to be canonical. Resolving it here, once, is what keeps every fixture from having to remember to.
    pub fn new(family: &str, name: &str) -> Self {
        let base = env::temp_dir();
        let base = base.canonicalize().unwrap_or(base);

        Self(base.join(format!(
            "curios-{family}-{name}-{}-{}",
            process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_millis()
        )))
    }
}

impl Deref for Temporary {
    type Target = Path;

    fn deref(&self) -> &Path {
        &self.0
    }
}

/// For the callers that take a generic path — `fs::create_dir_all` and its kin — which a `Deref` alone does not reach.
impl AsRef<Path> for Temporary {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl Drop for Temporary {
    fn drop(&mut self) {
        // Best effort, deliberately: a panic here during an unwind aborts the process and hides the assertion that failed, and a directory the test never created is nothing to report.
        let _ = fs::remove_dir_all(&self.0);
    }
}
