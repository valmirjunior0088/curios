//! Where everything a build generates or fetches is kept.
//!
//! `.curios/` sits beside the governing root — the umbrella's manifest when one governs the invocation, the package's otherwise — and it is **the only generated directory in the tree**: member directories hold user files and nothing else.
//!
//! It holds six families, each in its own subtree rather than sharing one namespace, and each named for what it holds:
//!
//! ```text
//! .curios/
//!   executables/    json/serve      what `curios compile` emits
//!   documentation/  json/           what `curios document` emits
//!   sources/        c1/<digest>/    materialized source trees, keyed by their manifest hash
//!   foreign/        f1/<digest>     fetched foreign modules, keyed by the hash they were accepted against
//!   verdicts/       <slot>          judged units, one file per mount, compiler and predecessor chain
//!   payloads/       <slot>          precompiled payloads, one file per executable, chain and engine
//! ```
//!
//! One transient entry passes through beside them: `pinning.<pid>`, where `curios pin` stages a delivery whose digest it has yet to compute, before renaming it into the family it belongs to. See [`Store::staging`].
//!
//! Separated because the alternative re-invites a collision that nesting otherwise removes: a hash has to be transformed to sit in a directory name at all — `c1:<digest>` most naturally becoming `c1/<digest>` — and a package legitimately named `c1` would then land on top of it.
//!
//! That tree is what a project gets when nothing points elsewhere. Setting `CURIOS_CACHE` moves the `sources/`, `foreign/`, `verdicts/` and `payloads/` families — never `executables/` or `documentation/`, which belong to the package that declared them — into a cache keyed by the same hash and shared across projects, so two projects pinning one revision materialize and compile it once. See `shared` below for why there is no divined default.
//!
//! The family names are the plural of their contents, and the words are the ones the rest of the toolchain already uses: the manifest's `[[executables]]`, the cache's `Verdicts`, the soundness entry "Cached verdicts". A rename orphans entries under an old name rather than misreading them, since a slot's schema tag lives inside its slot name and nothing looks in the old directory, so an existing store rebuilds once.

#[cfg(test)]
mod tests;

use {
    crate::{FileHash, TreeHash},
    curios_utilities::{Fingerprint, Mount, Qualifier},
    std::path::PathBuf,
};

/// The generated directory itself.
pub const STORE: &str = ".curios";

/// Where a project's generated things go: two families beside it, three in a cache it shares with every other project.
///
/// **The split is not arbitrary, and it follows from the keys.** A materialized tree is named by its `c1:` hash, a judged unit by an address over its mounts, its compiler and its predecessors, and a precompiled payload by an address over that chain, the executable's identity and the engine that will run it — none of them says anything about *which* project asked, so each names the same thing wherever it is computed and all three are shareable. A unit's address says nothing about *which source*, either, which is why opening its slot verifies the files it was compiled from and refuses any that the asking compilation could not itself have read; a payload's slot is opened the same way. A built executable and a package's documentation are named by the package that declared them and belong to the project that built them, so they stay local. Content-derived keys are what make an upper layer possible at all; a path-keyed store could only ever have been local.
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
    /// A package's name is the one identity in a compilation that cannot collide (law 2), so nesting by it removes the collision by construction — two members of one umbrella may both declare `serve` and nothing has to refuse it. Nesting also fixes the path *inside* the store: `executables/<package>/<executable>` does not depend on what encloses the package, so joining an umbrella re-roots a binary without renaming it.
    pub fn executable(&self, package: &str, executable: &str) -> PathBuf {
        self.project
            .join(STORE)
            .join("executables")
            .join(package)
            .join(executable)
    }

    /// Where a package's generated documentation is written: nested under the package, beside the project, for the reason an executable is.
    pub fn documentation(&self, package: &str) -> PathBuf {
        self.project.join(STORE).join("documentation").join(package)
    }

    /// Where a delivery is staged while the digest that will name it is still being computed, before it is renamed onto that digest.
    ///
    /// **The one fetch in this toolchain with no pin to fetch against**, because deriving the pin is what `curios pin` is doing. Every other stages under the key it already has — `curate` uses `source(hash).with_extension("fetching")` — which is exactly what this cannot do.
    ///
    /// It still lands in the store rather than being hashed and thrown away: the bytes that were just fetched are the bytes the next command wants, and a delivery discarded here would be downloaded a second time by the `curate` that follows. What makes filing it sound is that the digest is computed *from* the delivery, so the entry it is renamed onto holds bytes that hash to its own name by construction — and every reader re-hashes what it takes out.
    ///
    /// In the shared half beside the families, so the rename onto a digest is within one filesystem however `CURIOS_CACHE` points; `what` carries the process id, so two `pin`s at once stage two paths.
    pub fn staging(&self, what: &str) -> PathBuf {
        self.shared.join(format!("pinning.{what}"))
    }

    /// Where a materialized source tree is placed, keyed by the hash it was accepted against.
    ///
    /// The scheme is a directory of its own rather than part of the leaf name, which is what lets a successor scheme sit beside `c1` during a transition instead of replacing it.
    pub fn source(&self, hash: &TreeHash) -> PathBuf {
        let (scheme, digest) = hash.split();

        self.shared.join("sources").join(scheme).join(digest)
    }

    /// Where a fetched foreign module is placed, keyed by the hash it was accepted against.
    ///
    /// Beside `sources` rather than within it, and for the reason the two are different kinds: a source is a tree a package *is*, a foreign module is one file a package *names*. Both are content-keyed and both live in the shared half, so one machine fetches a module once however many projects reach for it — which is what makes a module too large to commit affordable to depend on.
    ///
    /// A module the package carries has no entry here at all: it is already in the delivered tree, and the tree hash its consumer pinned covers it.
    pub fn foreign(&self, hash: &FileHash) -> PathBuf {
        let (scheme, digest) = hash.split();

        self.shared.join("foreign").join(scheme).join(digest)
    }

    /// Where a judged unit is filed, under the address its mounts, its predecessors and the certifier decide: one file, holding the record of what it was compiled *from* ahead of the unit, verified when the slot is opened rather than spelled here.
    pub fn verdict(&self, slot: &str) -> PathBuf {
        self.shared.join("verdicts").join(slot)
    }

    /// Where an executable's precompiled payload is filed, under the address its predecessor chain, its own identity and the engine decide: one file, laid out as a unit's is, with what it was compiled *from* verified when the slot is opened.
    ///
    /// Shared rather than project-local, for the reason the type states: the address says nothing about which project asked. `executables/` stays local because a built executable's identity *is* project-relative; the payload inside it is not.
    pub fn payload(&self, slot: &str) -> PathBuf {
        self.shared.join("payloads").join(slot)
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

/// What version of the verdict family's layout a key names.
///
/// In the key rather than in a file beside it, so an entry written by an older layout is not found rather than found and misread. Bump it whenever what a slot holds, or what a hit is verified against, changes.
const SCHEMA: &str = "u12";

/// The same, for the payload family — its own tag, because the two families version independently and neither should invalidate the other by moving.
const PAYLOAD_SCHEMA: &str = "p4";

/// The slot a unit compiled by `compiler`, after `predecessors`, claiming `mounts` and declaring `declared`, is filed under.
///
/// **No file contents here, deliberately.** This names a *place*, not a version of what lives in it: "the unit for these mounts, by this compiler, after this chain". What was compiled is checked when the slot is opened, against the record of what was read, so a source edit changes the verification and not the address. That is what bounds the store — a project has as many slots as it has units, forever, rather than one per compile — and it is what the previous scheme got wrong by hashing the unit's whole source directory, which contains this store.
///
/// **Four parts, and each is load-bearing.** The compiler is who judged it — see [`compiler`](crate::compiler()), and note that a key naming no compiler would be believed on behalf of any. The mounts are how the unit's names are spelled, which the lowering depends on and no read of its files reveals: they used to ride along by accident, in a manifest that happened to sit in the hashed directory. The predecessors are the part easiest to leave out and the reason this takes a list at all: a unit's lowering copies the *cumulative universe-seed table* from the unit before it, so the same source compiled after a different prefix is a different unit, byte for byte. What a predecessor *contains* is verified rather than keyed, for the same reason the unit's own source is.
///
/// The fourth is what the unit declared it could see, which replaces the privilege tier the mounts used to carry. The tier answered "may these names reach that root" for a whole root at a time; a declared dependency answers it per unit, and it is the answer the lowering actually consults — the same source over the same scope, declaring different prefixes, resolves different names and is a different unit. `None` is its own value, not an empty list: it means the caller declared nothing, which is every open prefix in scope, where an empty list means a unit that asked for none of them.
///
/// Ordered, not a set: the predecessors are a fold order, and two orders of one set are two different lowerings.
pub fn unit_slot(
    compiler: &str,
    predecessors: &[String],
    mounts: &[Mount],
    declared: Option<&[Qualifier]>,
) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(SCHEMA);
    fingerprint.feed(compiler);
    fingerprint.feed(predecessors.len().to_string());
    for predecessor in predecessors {
        fingerprint.feed(predecessor);
    }
    fingerprint.feed(mounts.len().to_string());
    for mount in mounts {
        fingerprint.feed(mount.prefix.join());
    }
    match declared {
        // Two distinguishable shapes rather than one, so "declared nothing" and "declared none" cannot collide: a length-prefixed list can never spell the sentinel.
        None => fingerprint.feed("*"),
        Some(declared) => {
            fingerprint.feed(declared.len().to_string());
            for prefix in declared {
                fingerprint.feed(prefix.join());
            }
        }
    }

    fingerprint.hex()
}

/// The slot the precompiled payload of `package`'s `executable`, built by `compiler` after `predecessors` and run by the engine `engine` fingerprints, is filed under.
///
/// **The same address-and-record split [`unit_slot`] states, one level up.** This names a place — "the payload for this executable, by this compiler, after this chain, for this engine" — and carries no file contents, so an edit changes what the slot is verified against rather than where it is looked for. Predecessor slots are themselves stable across source edits, so this is too: one slot per executable per chain per compiler per engine, overwritten in place, bounded exactly as the unit family is.
///
/// **The executable's identity is its package's name and its own**, never a path. That pair is the one identity in a compilation that cannot collide (law 2), which is why two executables of one package occupy two slots and why nothing has to refuse an umbrella whose members both declare `serve`. A path here would re-import the store growth the unit key's own replacement records.
///
/// **The engine fingerprint is the part [`unit_slot`] has no counterpart for.** A unit is a judgment and travels anywhere the compiler binary does; a payload is machine code, and Cranelift compiled it for the host's ISA — the one input neither the compiler digest nor any recorded source file covers. It rides here as an opaque string because deciding what makes two engines compatible is `curios-runtime`'s to answer, this crate's only job being to keep the answer in the address.
pub fn payload_slot(
    compiler: &str,
    predecessors: &[String],
    package: &str,
    executable: &str,
    engine: &str,
) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(PAYLOAD_SCHEMA);
    fingerprint.feed(compiler);
    fingerprint.feed(predecessors.len().to_string());
    for predecessor in predecessors {
        fingerprint.feed(predecessor);
    }
    fingerprint.feed(package);
    fingerprint.feed(executable);
    fingerprint.feed(engine);

    fingerprint.hex()
}
