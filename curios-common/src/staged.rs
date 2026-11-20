//! A file written whole or not at all: its bytes go to a staging name beside it, and a rename puts them in its place.
//!
//! **A reader holds the file it opened.** A rename over a file replaces its directory entry at once: whoever holds the old one keeps reading it, a running executable among them, which a write in place is refused as a busy text file, and whoever opens the name after reads the new bytes whole, never a file being written. Two writers of one file each put a whole file in its place, and the last stands. The staging name is the file's own with the writing process's id and `part` after it, in its directory, so the rename stays on one file system and two processes never stage over each other.

#[cfg(test)]
mod tests;

use std::{
    fs, io,
    path::{Path, PathBuf},
};

/// Write `bytes` to `path` whole: to a staging name beside it, then renamed into its place.
pub fn write_whole(path: &Path, bytes: &[u8]) -> io::Result<()> {
    write_whole_prepared(path, bytes, |_| Ok(()))
}

/// Write `bytes` to `path` whole, `prepare` handed the staged file before it takes the name: where an executable is made executable, so the name never names a file that cannot run.
pub fn write_whole_prepared(
    path: &Path,
    bytes: &[u8],
    prepare: impl FnOnce(&Path) -> io::Result<()>,
) -> io::Result<()> {
    let staged = staging(path);
    let written = fs::write(&staged, bytes)
        .and_then(|()| prepare(&staged))
        .and_then(|()| fs::rename(&staged, path));
    if written.is_err() {
        let _ = fs::remove_file(&staged);
    }

    written
}

/// The name `path`'s bytes are staged under: its own, beside it, with this process's id and `part` after it.
fn staging(path: &Path) -> PathBuf {
    let mut name = path.file_name().unwrap_or_default().to_os_string();
    name.push(format!(".{}.part", std::process::id()));

    path.with_file_name(name)
}
