//! Where everything a build generates or fetches is kept.
//!
//! `.curios/` sits beside the governing root — the umbrella's manifest when one governs the invocation, the package's otherwise — and it is **the only generated directory in the tree**: member directories hold user files and nothing else.
//!
//! It holds three families, each in its own subtree rather than sharing one namespace:
//!
//! ```text
//! .curios/
//!   bin/    myorg/json/serve      what `curios compile` emits
//!   src/    c1/<digest>/          materialized source trees, keyed by their manifest hash
//!   unit/   <key>/                compiled units, keyed by their terms and the certifier
//! ```
//!
//! Separated because the alternative re-invites a collision that nesting otherwise removes: a hash has to be transformed to sit in a directory name at all — `c1:<digest>` most naturally becoming `c1/<digest>` — and a package legitimately named `c1` would then land on top of it.
//!
//! Above this sits a shared content-addressed cache keyed by the same hash, so two projects pinning one revision materialize and compile it once. Content-derived keys are what make that upper layer shareable at all; a path-keyed store could only ever have been local.

#[cfg(test)]
mod tests;

use {
    crate::TreeHash,
    curios_base::Qualifier,
    sha2::{Digest, Sha256},
    std::path::{Path, PathBuf},
};

/// The generated directory itself.
pub const STORE: &str = ".curios";

/// Where a native executable is written: nested under the package that declares it.
///
/// A package's name is the one identity in a compilation that cannot collide (law 2), so nesting by it removes the collision by construction — two members of one umbrella may both declare `serve` and nothing has to refuse it. Nesting also keeps the path stable: flat, a package's binary would move when it joined an umbrella, since only the governing root changed. A name is already a path, so its segments nest exactly as the layout rule maps a qualified name onto a file.
pub fn bin(root: &Path, package: &Qualifier, executable: &str) -> PathBuf {
    package
        .iter()
        .fold(root.join(STORE).join("bin"), |path, segment| {
            path.join(segment)
        })
        .join(executable)
}

/// Where a materialized source tree is placed, keyed by the hash it was accepted against.
///
/// The scheme is a directory of its own rather than part of the leaf name, which is what lets a successor scheme sit beside `c1` during a transition instead of replacing it.
pub fn src(root: &Path, hash: &TreeHash) -> PathBuf {
    let (scheme, digest) = hash.split();

    root.join(STORE).join("src").join(scheme).join(digest)
}

/// Where a compiled unit is filed, under the key its terms and the certifier decide.
pub fn unit(root: &Path, key: &str) -> PathBuf {
    root.join(STORE).join("unit").join(key)
}

/// What a unit compiled from `terms`, by `compiler`, after `predecessors`, is filed under.
///
/// **Three parts, and each is load-bearing.** The terms are what was judged. The compiler is who judged it — see [`compiler`](crate::compiler), and note that a key naming no compiler would be believed on behalf of any. The predecessors are the part easiest to leave out and the reason this takes a list at all: a unit's lowering copies the *cumulative universe-seed table* from the unit before it, so the same source compiled after a different prefix is a different unit, byte for byte. A key covering only terms and compiler would hand one of them back for the other.
///
/// Ordered, not a set: the predecessors are a fold order, and two orders of one set are two different lowerings.
///
/// Every part is length-framed, for the reason [`TreeHash::of`](crate::TreeHash::of) frames its own — without it, moving a boundary between two parts feeds the digest identical bytes, and two different keys collapse into one.
pub fn unit_key(compiler: &str, predecessors: &[String], terms: &TreeHash) -> String {
    let mut digest = Sha256::new();

    feed(&mut digest, compiler);
    feed(&mut digest, &predecessors.len().to_string());
    for predecessor in predecessors {
        feed(&mut digest, predecessor);
    }
    feed(&mut digest, &terms.to_string());

    digest
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

/// The one hash a unit's terms come to, over every directory it reads from.
///
/// A unit is usually one directory — a package — and folding rather than special-casing that keeps the key's third part one well-formed hash whatever the unit's shape. `None` when any directory cannot be hashed, which means the unit has no terms to be keyed by and so may not be stored.
pub fn terms(directories: &[&Path]) -> Option<TreeHash> {
    let mut digest = Sha256::new();

    for directory in directories {
        feed(&mut digest, &TreeHash::of(directory).ok()?.to_string());
    }

    TreeHash::parse(&format!(
        "c1:{}",
        digest
            .finalize()
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>()
    ))
    .ok()
}

/// One length-framed part of a key.
fn feed(digest: &mut Sha256, part: &str) {
    digest.update((part.len() as u64).to_le_bytes());
    digest.update(part.as_bytes());
}
