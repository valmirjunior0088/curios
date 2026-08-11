//! The criterion a delivered source tree is accepted against, and the key the store files it under.

#[cfg(test)]
mod tests;

use {
    sha2::{Digest, Sha256},
    std::{
        fmt, fs,
        path::{Path, PathBuf},
    },
};

/// The scheme this compiler computes and verifies.
const SCHEME: &str = "c1:";

/// The digits a SHA-256 digest spells in hex.
const DIGITS: usize = 64;

/// A hash over a delivered source tree, doing two jobs at once: the criterion a delivery is accepted against, and the key the shared content-addressed store files it under, uniformly across source kinds.
///
/// `c1:` is SHA-256 over the tree's regular files sorted by relative path, each contributing its path and its contents. Permissions and timestamps do not exist for it, and a symlink in a delivered tree is refused. The scheme prefix is carried rather than assumed, because a hash outlives any implementation: a successor is `c2:`, and the prefix is what lets both verify during a transition.
///
/// Nobody writes one by hand — `curate` computes it on first materialization, and a wrong or missing hash is refused stating the correct one. This type only ever holds a well-formed spelling, so everything downstream compares rather than validates.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TreeHash(String);

impl TreeHash {
    /// The hash `spelling` states, or why it states none.
    pub fn parse(spelling: &str) -> Result<Self, String> {
        let Some(digest) = spelling.strip_prefix(SCHEME) else {
            return Err(format!(
                "{spelling:?} names no hash scheme this compiler knows; the scheme is `{SCHEME}`"
            ));
        };

        // Lowercase is part of the spelling rather than a normalization, because the hash is a store key: two spellings of one digest would be two directories.
        match digest.len() == DIGITS
            && digest
                .chars()
                .all(|digit| matches!(digit, '0'..='9' | 'a'..='f'))
        {
            true => Ok(Self(spelling.to_string())),
            false => Err(format!(
                "{spelling:?} is no `{SCHEME}` hash: the scheme takes {DIGITS} lowercase hex digits"
            )),
        }
    }

    /// The scheme and the digest, apart.
    ///
    /// The store files a tree under its scheme as a directory of its own, which is what lets a successor scheme sit beside `c1` during a transition rather than replacing it. Splitting here rather than at the store keeps the spelling's shape this type's business.
    pub fn split(&self) -> (&str, &str) {
        self.0
            .split_once(':')
            .expect("a well-formed hash carries its scheme")
    }

    /// The hash of the tree rooted at `directory`.
    ///
    /// **What goes into it, exactly.** Every regular file the tree holds, sorted by relative path, each contributing its path and then its contents. Paths are spelled with `/` whatever the platform, so a tree delivered to Windows hashes as it did on the machine that published it. Nothing else exists for the scheme: not permissions, not timestamps, not directories — an empty one leaves no trace, because a tree is its files.
    ///
    /// **Both halves are length-framed**, which the scheme has to do and this is where it is said: without it a file `ab` holding `c` and a file `a` holding `bc` feed the digest identical bytes, and two different trees would share a store key. The frame is the byte length as a little-endian `u64` before each half.
    ///
    /// A symlink is refused rather than followed or recorded. Following one lets a delivered tree reach outside itself; recording one puts a path in the hash whose meaning depends on where it is unpacked. Neither is a criterion a delivery can be accepted against.
    pub fn of(directory: &Path) -> Result<Self, String> {
        let mut files = Vec::new();
        collect(directory, &mut PathBuf::new(), &mut files)?;
        files.sort();

        let mut digest = Sha256::new();
        for (path, contents) in &files {
            digest.update((path.len() as u64).to_le_bytes());
            digest.update(path.as_bytes());
            digest.update((contents.len() as u64).to_le_bytes());
            digest.update(contents);
        }

        Ok(Self(format!(
            "{SCHEME}{}",
            digest
                .finalize()
                .iter()
                .map(|byte| format!("{byte:02x}"))
                .collect::<String>()
        )))
    }
}

/// Every regular file under `directory`, as its `/`-spelled path relative to the tree root and its contents.
fn collect(
    directory: &Path,
    at: &mut PathBuf,
    files: &mut Vec<(String, Vec<u8>)>,
) -> Result<(), String> {
    let entries =
        fs::read_dir(directory).map_err(|error| format!("{}: {error}", directory.display()))?;

    for entry in entries {
        let entry = entry.map_err(|error| format!("{}: {error}", directory.display()))?;
        let path = entry.path();

        // `symlink_metadata` rather than `metadata`, because the question is what the entry *is*, not what it points at.
        let kind = fs::symlink_metadata(&path)
            .map_err(|error| format!("{}: {error}", path.display()))?
            .file_type();

        at.push(entry.file_name());

        if kind.is_symlink() {
            return Err(format!(
                "{} is a symlink, and a delivered tree may hold none: following one reaches outside the tree, and recording one hashes a path whose meaning depends on where it is unpacked",
                path.display()
            ));
        }

        match kind.is_dir() {
            true => collect(&path, at, files)?,
            false => files.push((
                at.components()
                    .map(|component| component.as_os_str().to_string_lossy())
                    .collect::<Vec<_>>()
                    .join("/"),
                fs::read(&path).map_err(|error| format!("{}: {error}", path.display()))?,
            )),
        }

        at.pop();
    }

    Ok(())
}

impl fmt::Display for TreeHash {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}
