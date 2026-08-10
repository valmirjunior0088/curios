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
