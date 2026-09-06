//! Where everything a build generates or fetches is kept.
//!
//! `.curios/` sits beside the governing root — the umbrella's manifest when one governs the invocation, the package's otherwise — and it is **the only generated directory in the tree**: member directories hold user files and nothing else.
//!
//! It holds five families, each in its own subtree rather than sharing one namespace, and each named for what it holds:
//!
//! ```text
//! .curios/
//!   executables/    json/serve      what `curios compile` emits
//!   documentation/  json/           what `curios document` emits
//!   sources/        c1/<digest>/    materialized source trees, keyed by their manifest hash
//!   verdicts/       <slot>          judged units, one file per mount, compiler and predecessor chain
//!   payloads/       <slot>          precompiled payloads, one file per executable, chain and engine
//! ```
//!
//! Separated because the alternative re-invites a collision that nesting otherwise removes: a hash has to be transformed to sit in a directory name at all — `c1:<digest>` most naturally becoming `c1/<digest>` — and a package legitimately named `c1` would then land on top of it.
//!
//! That tree is what a project gets when nothing points elsewhere. Setting `CURIOS_CACHE` moves the `sources/`, `verdicts/` and `payloads/` families — never `executables/` or `documentation/`, which belong to the package that declared them — into a cache keyed by the same hash and shared across projects, so two projects pinning one revision materialize and compile it once. See `shared` below for why there is no divined default.
//!
//! The family names are the plural of their contents, and the words are the ones the rest of the toolchain already uses: the manifest's `[[executables]]`, the cache's `Verdicts`, the soundness entry "Cached verdicts". A rename orphans entries under an old name rather than misreading them, since a slot's schema tag lives inside its slot name and nothing looks in the old directory, so an existing store rebuilds once.

#[cfg(test)]
mod tests;

use {
    crate::TreeHash,
    curios_utilities::{Mount, RootKind},
    sha2::{Digest, Sha256},
    std::{hash::Hasher, path::PathBuf},
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

    /// Where a materialized source tree is placed, keyed by the hash it was accepted against.
    ///
    /// The scheme is a directory of its own rather than part of the leaf name, which is what lets a successor scheme sit beside `c1` during a transition instead of replacing it.
    pub fn source(&self, hash: &TreeHash) -> PathBuf {
        let (scheme, digest) = hash.split();

        self.shared.join("sources").join(scheme).join(digest)
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
const SCHEMA: &str = "u7";

/// The same, for the payload family — its own tag, because the two families version independently and neither should invalidate the other by moving.
const PAYLOAD_SCHEMA: &str = "p2";

/// The slot a unit compiled by `compiler`, after `predecessors`, claiming `mounts`, is filed under.
///
/// **No file contents here, deliberately.** This names a *place*, not a version of what lives in it: "the unit for these mounts, by this compiler, after this chain". What was compiled is checked when the slot is opened, against the record of what was read, so a source edit changes the verification and not the address. That is what bounds the store — a project has as many slots as it has units, forever, rather than one per compile — and it is what the previous scheme got wrong by hashing the unit's whole source directory, which contains this store.
///
/// **Three parts, and each is load-bearing.** The compiler is who judged it — see [`compiler`](crate::compiler()), and note that a key naming no compiler would be believed on behalf of any. The mounts are how the unit's names are spelled, which the lowering depends on and no read of its files reveals: they used to ride along by accident, in a manifest that happened to sit in the hashed directory. The predecessors are the part easiest to leave out and the reason this takes a list at all: a unit's lowering copies the *cumulative universe-seed table* from the unit before it, so the same source compiled after a different prefix is a different unit, byte for byte. What a predecessor *contains* is verified rather than keyed, for the same reason the unit's own source is.
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
    let mut digest = Sha256::new();

    feed(&mut digest, PAYLOAD_SCHEMA);
    feed(&mut digest, compiler);
    feed(&mut digest, &predecessors.len().to_string());
    for predecessor in predecessors {
        feed(&mut digest, predecessor);
    }
    feed(&mut digest, package);
    feed(&mut digest, executable);
    feed(&mut digest, engine);

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

/// A [`Hasher`] that finishes into a digest, for a key part whose producer speaks `std::hash` and whose consumer speaks SHA-256.
///
/// One part of the payload address is not a string anybody here can build: the engine compatibility stamp, which only `curios-runtime` can describe and which it hands over by *writing into* a hasher rather than by returning a digest — keeping `sha2` out of that crate and `wasmtime` out of the one that assembles the address. This is the adapter between the two vocabularies, and it lives beside [`digest`] because what it produces is one.
///
/// [`Hasher::finish`] is not how a value is taken out of this. It has to exist, and a `u64` is not what a store key is spelled in, so it answers with the leading eight bytes of the digest so far and [`Fingerprint::hex`] is what a caller uses. Nothing in `std::hash` calls `finish` on the caller's behalf — `Hash::hash` only ever writes — so the narrow answer is never the one that reaches a key.
pub struct Fingerprint(Sha256);

impl Fingerprint {
    /// A fingerprint with nothing folded into it yet.
    pub fn new() -> Self {
        Self(Sha256::new())
    }

    /// Everything written so far, as the hex digest a key part is spelled in.
    pub fn hex(self) -> String {
        hex(self.0)
    }
}

impl Default for Fingerprint {
    fn default() -> Self {
        Self::new()
    }
}

impl Hasher for Fingerprint {
    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
    }

    fn finish(&self) -> u64 {
        let digest = self.0.clone().finalize();

        u64::from_le_bytes(digest[..8].try_into().expect("a digest is 32 bytes wide"))
    }
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
