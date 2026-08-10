//! The criterion a delivered source tree is accepted against, and the key the store files it under.

#[cfg(test)]
mod tests;

use std::fmt;

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
}

impl fmt::Display for TreeHash {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}
