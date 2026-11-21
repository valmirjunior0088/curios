//! The cabinet: what `.curios/` keeps for reuse — materialized sources, fetched foreign modules, judged units, each an object holding the blobs it names, precompiled payloads and the compiler's memo — and where each family stands in it. Its families and their place in `.curios/` are [`crate::PRODUCTS`]'s map; the addresses a unit and a payload are filed under are `curios-verdict`'s, beside the record each is verified against.

#[cfg(test)]
mod tests;

#[cfg(any(test, feature = "test-support"))]
mod test_support;

use {
    crate::{FileHash, PRODUCTS, Target, TreeHash},
    std::path::{Path, PathBuf},
};

/// The directory of `.curios/` a project's own cabinet is, where nothing names another.
const OWN: &str = "cabinet";

/// The family of materialized source trees, a directory for each hash scheme.
const SOURCES: &str = "sources";

/// The family of fetched foreign modules, a directory for each hash scheme.
const FOREIGN: &str = "foreign";

/// The family of judged units as the cabinet keeps them, an object a directory.
const OBJECTS: &str = "objects";

/// The file of an object holding its record and its index.
const UNIT: &str = "unit";

/// The file of an object holding the record its `unit` replaced, and that record's index.
const REPLACED: &str = "replaced";

/// The family of precompiled payloads, a directory for each target.
const PAYLOADS: &str = "payloads";

/// The file of this machine's memo of its compiler's digest.
const COMPILER: &str = "compiler";

/// What a project keeps for reuse: five families in a directory it shares with every project that names the same one.
///
/// **The cabinet is not arbitrary, and it follows from the keys.** A materialized tree is named by its `c1:` hash, a fetched foreign module by its `f1:` hash, a judged unit's object by an address over its mounts, what it declared and its kind, the blobs it names by that address and their own digests, and a precompiled payload by an address over the executable's identity — none of them says anything about *which* project asked, so each names the same thing wherever it is computed and all five are shareable. A unit's address says nothing about *which source*, either, which is why opening its object verifies the files it was compiled from and refuses any that the asking compilation could not itself have read; a payload is opened the same way. A built executable and a package's documentation are named by the package that declared them and belong to the project that built them, so they are [`Products`](crate::Products) and stay local. Content-derived keys are what make a shared cabinet possible at all; a path-keyed one could only ever have been local.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cabinet {
    /// The directory the families stand in.
    directory: PathBuf,
}

impl Cabinet {
    /// The cabinet the project at `project` files into: the directory `named` names, where whoever ran the command named one for every project it sees, and the project's own otherwise.
    ///
    /// Named by the caller and never looked for: what names it is the command line's to read, so no cabinet a suite opens is any but the one it names.
    pub fn of(project: &Path, named: Option<&Path>) -> Self {
        Self {
            directory: named.map_or_else(|| project.join(PRODUCTS).join(OWN), Path::to_path_buf),
        }
    }

    /// The directory the families stand in: what names this cabinet again where a governing root is opened anew, as a program's graph is walked again to link it.
    pub fn directory(&self) -> &Path {
        &self.directory
    }

    /// Where a delivery is staged while the digest that will name it is still being computed, before it is renamed onto that digest.
    ///
    /// **The one fetch in this toolchain with no pin to fetch against**, because deriving the pin is what `curios pin` is doing. Every other stages under the key it already has — `curate` uses `source(hash).with_extension("fetching")` — which is exactly what this cannot do.
    ///
    /// It still lands in the cabinet rather than being hashed and thrown away: the bytes that were just fetched are the bytes the next command wants, and a delivery discarded here would be downloaded a second time by the `curate` that follows. What makes filing it sound is that the digest is computed *from* the delivery, so the entry it is renamed onto holds bytes that hash to its own name by construction — and every reader re-hashes what it takes out.
    ///
    /// In the cabinet beside the families, so the rename onto a digest is within one filesystem wherever the cabinet stands; `what` carries the process id, so two `pin`s at once stage two paths.
    pub fn staging(&self, what: &str) -> PathBuf {
        self.directory.join(format!("pinning.{what}"))
    }

    /// Where a materialized source tree is placed, keyed by the hash it was accepted against.
    ///
    /// The scheme is a directory of its own rather than part of the leaf name, which is what lets a successor scheme sit beside `c1` during a transition instead of replacing it.
    pub fn source(&self, hash: &TreeHash) -> PathBuf {
        let (scheme, digest) = hash.split();

        self.directory.join(SOURCES).join(scheme).join(digest)
    }

    /// Where a fetched foreign module is placed, keyed by the hash it was accepted against.
    ///
    /// Beside `sources` rather than within it, and for the reason the two are different kinds: a source is a tree a package *is*, a foreign module is one file a package *names*. Both are content-keyed and both live in the cabinet, so one machine sharing one fetches a module once however many projects reach for it — which is what makes a module too large to commit affordable to depend on.
    ///
    /// A module the package carries has no entry here at all: it is already in the delivered tree, and the tree hash its consumer pinned covers it.
    pub fn foreign(&self, hash: &FileHash) -> PathBuf {
        let (scheme, digest) = hash.split();

        self.directory.join(FOREIGN).join(scheme).join(digest)
    }

    /// The object a judged unit is kept as at `address`, which its mounts, what it declared and its kind decide: one directory, holding its [`unit`](Self::unit) file and the one that file [`replaced`](Self::replaced), beside the packs of the blobs they name, which `curios-verdict` names.
    ///
    /// A directory for each address rather than a family for each kind of file, because an address is a unit's through every edit and a unit's baseline is its own object: what an object names is in its own directory, so finding it lists one directory, collecting it lists the same one, and two units never write one.
    pub fn object(&self, address: &str) -> PathBuf {
        self.directory.join(OBJECTS).join(address)
    }

    /// Where the [`object`](Self::object) at `address` keeps its unit: one file, holding the record of what it was compiled *from* ahead of the unit's index, verified when the object is opened rather than spelled here.
    pub fn unit(&self, address: &str) -> PathBuf {
        self.object(address).join(UNIT)
    }

    /// Where the [`object`](Self::object) at `address` keeps the unit file its [`unit`](Self::unit) replaced: the filing before the last, laid out as the last is, which `curios-verdict` reads for the compiler that filed it.
    pub fn replaced(&self, address: &str) -> PathBuf {
        self.object(address).join(REPLACED)
    }

    /// Where an executable's precompiled payload is filed, at the `address` its own identity decides: one file, laid out as a unit's is, with what it was compiled *from* verified when the payload is opened.
    ///
    /// In the cabinet rather than the products, for the reason the type states: the address says nothing about which project asked. A build stays local because its identity *is* project-relative; the payload inside it is not.
    pub fn payload(&self, runs: Runs<'_>, address: &str) -> PathBuf {
        self.payloads(runs.target()).join(address)
    }

    /// The directory `target`'s payloads are filed in.
    pub fn payloads(&self, target: Target) -> PathBuf {
        self.directory.join(PAYLOADS).join(target.spelled())
    }

    /// Where this machine's memo of its compiler's digest lives — a fact about the machine, so it belongs beside the things every project shares.
    pub fn compiler(&self) -> PathBuf {
        self.directory.join(COMPILER)
    }

    /// Every entry the cabinet holds of its own, each where it stands. Anything else in it is a staging on its way into a family ([`Cabinet::staging`]), or what a layout moved away from left behind.
    pub fn families(&self) -> Families {
        Families {
            sources: self.directory.join(SOURCES),
            foreign: self.directory.join(FOREIGN),
            objects: self.directory.join(OBJECTS),
            payloads: self.directory.join(PAYLOADS),
            compiler: self.compiler(),
        }
    }
}

/// Where each family of a cabinet stands, and the compiler's memo beside them ([`Cabinet::families`]).
pub struct Families {
    /// Materialized source trees, a directory for each hash scheme ([`Cabinet::source`]).
    pub sources: PathBuf,
    /// Fetched foreign modules, a directory for each hash scheme ([`Cabinet::foreign`]).
    pub foreign: PathBuf,
    /// Judged units, an object a directory ([`Cabinet::object`]).
    pub objects: PathBuf,
    /// Precompiled payloads, a directory for each target ([`Cabinet::payloads`]).
    pub payloads: PathBuf,
    /// This machine's memo of its compiler's digest ([`Cabinet::compiler`]).
    pub compiler: PathBuf,
}

impl Families {
    /// Each of them.
    pub fn all(self) -> [PathBuf; 5] {
        [
            self.sources,
            self.foreign,
            self.objects,
            self.payloads,
            self.compiler,
        ]
    }
}

/// What runs a precompiled payload, which is half its address: machine code one engine runs, named by that engine's fingerprint, or a module any browser runs, which needs no fingerprint. Each target's payloads are a directory of their own, so neither half of the family is read for the other.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Runs<'a> {
    Native { engine: &'a str },
    Browser,
}

impl Runs<'_> {
    /// The target the payload is built for.
    pub fn target(self) -> Target {
        match self {
            Runs::Native { .. } => Target::Native,
            Runs::Browser => Target::Browser,
        }
    }
}
