//! Where everything a build generates or fetches is kept.
//!
//! `.curios/` sits beside the governing root — the umbrella's manifest when one governs the invocation, the package's otherwise — and it is **the only generated directory in the tree**: member directories hold user files and nothing else.
//!
//! It holds three families, each in its own subtree rather than sharing one namespace:
//!
//! ```text
//! .curios/
//!   bin/    json/serve            what `curios compile` emits
//!   src/    c1/<digest>/          materialized source trees, keyed by their manifest hash
//!   unit/   <slot>/               compiled units, one slot per mount, compiler and predecessor chain
//! ```
//!
//! Separated because the alternative re-invites a collision that nesting otherwise removes: a hash has to be transformed to sit in a directory name at all — `c1:<digest>` most naturally becoming `c1/<digest>` — and a package legitimately named `c1` would then land on top of it.
//!
//! Above this sits a shared content-addressed cache keyed by the same hash, so two projects pinning one revision materialize and compile it once. Content-derived keys are what make that upper layer shareable at all; a path-keyed store could only ever have been local.

#[cfg(test)]
mod tests;

use {
    crate::TreeHash,
    curios_base::{Mount, RootKind},
    sha2::{Digest, Sha256},
    std::path::PathBuf,
};

/// The generated directory itself.
pub const STORE: &str = ".curios";

/// Where a project's generated things go: one family beside it, two in a cache it shares with every other project.
///
/// **The split is not arbitrary, and it follows from the keys.** A materialized tree is named by its `c1:` hash and a judged unit by an address over its mounts, its compiler and its predecessors — neither says anything about *which* project asked, so both name the same thing wherever they are computed and both are shareable. A unit's address says nothing about *which source*, either, which is why opening its slot verifies the files it was compiled from and refuses any that the asking compilation could not itself have read. A built executable is named by the package that declared it and belongs to the project that built it, so it stays local. Content-derived keys are what make an upper layer possible at all; a path-keyed store could only ever have been local.
pub struct Store {
    /// The governing root — the umbrella's directory when one governs, the package's otherwise.
    project: PathBuf,
    /// The cache shared across projects.
    shared: PathBuf,
}

impl Store {
    /// The store a project at `project` uses, sharing whatever this machine offers.
    pub fn at(project: PathBuf) -> Self {
        Self {
            shared: shared().unwrap_or_else(|| project.join(STORE)),
            project,
        }
    }

    /// Where a native executable is written: nested under the package that declares it, beside the project.
    ///
    /// A package's name is the one identity in a compilation that cannot collide (law 2), so nesting by it removes the collision by construction — two members of one umbrella may both declare `serve` and nothing has to refuse it. Nesting also fixes the path *inside* the store: `bin/<package>/<executable>` does not depend on what encloses the package, so joining an umbrella re-roots a binary without renaming it.
    pub fn bin(&self, package: &str, executable: &str) -> PathBuf {
        self.project
            .join(STORE)
            .join("bin")
            .join(package)
            .join(executable)
    }

    /// Where a materialized source tree is placed, keyed by the hash it was accepted against.
    ///
    /// The scheme is a directory of its own rather than part of the leaf name, which is what lets a successor scheme sit beside `c1` during a transition instead of replacing it.
    pub fn src(&self, hash: &TreeHash) -> PathBuf {
        let (scheme, digest) = hash.split();

        self.shared.join("src").join(scheme).join(digest)
    }

    /// Where a compiled unit is filed, under the address its mounts, its predecessors and the certifier decide. What it was compiled *from* is verified when the slot is opened rather than spelled here.
    pub fn unit(&self, slot: &str) -> PathBuf {
        self.shared.join("unit").join(slot)
    }

    /// Where this machine's memo of its compiler's digest lives — a fact about the machine, so it belongs beside the things every project shares.
    pub fn compiler(&self) -> PathBuf {
        self.shared.join("compiler")
    }
}

/// The cache shared across projects, when something said where to put one.
///
/// **`CURIOS_CACHE` and nothing else — there is deliberately no divined default.** A toolchain that writes into a home directory nobody pointed it at is doing something the person who ran it did not ask for, and `.curios/` beside the project is the one generated directory this design admits to. So sharing is opt in: unset, every project keeps its own store, which costs a re-fetch and a re-certification per project and surprises nobody. Set, two projects pinning one revision materialize and compile it once.
///
/// It also makes every test hermetic without asking for anything: nothing in a suite sets the variable, so nothing in a suite can reach past the directory it made.
fn shared() -> Option<PathBuf> {
    std::env::var_os("CURIOS_CACHE").map(PathBuf::from)
}

/// What version of this store's layout a key names.
///
/// In the key rather than in a file beside it, so an entry written by an older layout is not found rather than found and misread. Bump it whenever what a slot holds, or what a hit is verified against, changes.
const SCHEMA: &str = "u2";

/// The slot a unit compiled by `compiler`, after `predecessors`, claiming `mounts`, is filed under.
///
/// **No file contents here, deliberately.** This names a *place*, not a version of what lives in it: "the unit for these mounts, by this compiler, after this chain". What was compiled is checked when the slot is opened, against the record of what was read, so a source edit changes the verification and not the address. That is what bounds the store — a project has as many slots as it has units, forever, rather than one per compile — and it is what the previous scheme got wrong by hashing the unit's whole source directory, which contains this store.
///
/// **Three parts, and each is load-bearing.** The compiler is who judged it — see [`compiler`](crate::compiler), and note that a key naming no compiler would be believed on behalf of any. The mounts are how the unit's names are spelled, which the lowering depends on and no read of its files reveals: they used to ride along by accident, in a manifest that happened to sit in the hashed directory. The predecessors are the part easiest to leave out and the reason this takes a list at all: a unit's lowering copies the *cumulative universe-seed table* from the unit before it, so the same source compiled after a different prefix is a different unit, byte for byte. What a predecessor *contains* is verified rather than keyed, for the same reason the unit's own source is.
///
/// Ordered, not a set: the predecessors are a fold order, and two orders of one set are two different lowerings.
pub fn unit_slot(compiler: &str, predecessors: &[String], mounts: &[Mount]) -> String {
    let mut digest = Sha256::new();

    feed(&mut digest, SCHEMA);
    feed(&mut digest, compiler);
    feed(&mut digest, &predecessors.len().to_string());
    for predecessor in predecessors {
        feed(&mut digest, predecessor);
    }
    feed(&mut digest, &mounts.len().to_string());
    for mount in mounts {
        feed(&mut digest, &mount.prefix.join());
        // Spelled here rather than taken from `Debug`, which would make a key depend on the name of a variant instead of on the tier it means.
        feed(
            &mut digest,
            match mount.kind {
                RootKind::Internal => "internal",
                RootKind::Privileged => "privileged",
                RootKind::Ordinary => "ordinary",
            },
        );
    }

    hex(digest)
}

/// The digest of `bytes`, for a caller checking one thing against a record of it.
///
/// Deliberately not a [`TreeHash`]: that answers what a delivered tree *is*, and carries a scheme prefix because it is written into manifests and compared across machines. This answers whether some bytes are the bytes something was made from, is never published, and would be a lie in the other's spelling.
pub fn digest(bytes: &[u8]) -> String {
    let mut digest = Sha256::new();
    digest.update(bytes);

    hex(digest)
}

/// A finished digest, in hex.
fn hex(digest: Sha256) -> String {
    digest
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

/// One length-framed part of a key.
fn feed(digest: &mut Sha256, part: &str) {
    digest.update((part.len() as u64).to_le_bytes());
    digest.update(part.as_bytes());
}
