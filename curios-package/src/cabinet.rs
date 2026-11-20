//! The cabinet: what `.curios/` keeps for reuse — materialized sources, fetched foreign modules, judged units and the objects their slots name, precompiled payloads and the compiler's memo — and the addresses a unit and a payload are filed under. Its families and their place in `.curios/` are [`crate::PRODUCTS`]'s map.

#[cfg(test)]
mod tests;

#[cfg(any(test, feature = "test-support"))]
mod test_support;

use {
    crate::{FileHash, PRODUCTS, Target, TreeHash},
    curios_common::{Fingerprint, Mount, Qualifier},
    curios_module::Compiled,
    std::path::{Path, PathBuf},
};

/// The directory of `.curios/` a project's own cabinet is, where nothing names another.
const OWN: &str = "cabinet";

/// The family of judged units' slots, a directory each.
const VERDICTS: &str = "verdicts";

/// The file of a unit's slot holding its record and its index.
const VERDICT: &str = "verdict";

/// The family of precompiled payloads, a directory for each target.
const PAYLOADS: &str = "payloads";

/// What a project keeps for reuse: five families in a directory it shares with every project that names the same one.
///
/// **The cabinet is not arbitrary, and it follows from the keys.** A materialized tree is named by its `c1:` hash, a fetched foreign module by its `f1:` hash, a judged unit by an address over its mounts, what it declared and its compiler, the objects its slot names by that address and their own digests, and a precompiled payload by an address over the executable's identity, the compiler and the engine that will run it — none of them says anything about *which* project asked, so each names the same thing wherever it is computed and all five are shareable. A unit's address says nothing about *which source*, either, which is why opening its slot verifies the files it was compiled from and refuses any that the asking compilation could not itself have read; a payload's slot is opened the same way. A built executable and a package's documentation are named by the package that declared them and belong to the project that built them, so they are [`Products`](crate::Products) and stay local. Content-derived keys are what make a shared cabinet possible at all; a path-keyed one could only ever have been local.
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

        self.directory.join("sources").join(scheme).join(digest)
    }

    /// Where a fetched foreign module is placed, keyed by the hash it was accepted against.
    ///
    /// Beside `sources` rather than within it, and for the reason the two are different kinds: a source is a tree a package *is*, a foreign module is one file a package *names*. Both are content-keyed and both live in the cabinet, so one machine sharing one fetches a module once however many projects reach for it — which is what makes a module too large to commit affordable to depend on.
    ///
    /// A module the package carries has no entry here at all: it is already in the delivered tree, and the tree hash its consumer pinned covers it.
    pub fn foreign(&self, hash: &FileHash) -> PathBuf {
        let (scheme, digest) = hash.split();

        self.directory.join("foreign").join(scheme).join(digest)
    }

    /// The slot a judged unit is filed in, under the address its mounts, what it declared and the certifier decide: one directory, holding its [`verdict`](Self::verdict) beside the packs of the objects it names and what its address's last collection found, which `curios-verdict` names.
    ///
    /// A directory for each address rather than a family for each kind of file, because an address is a unit's through every edit and a unit's baseline is its own address's slot: what a slot names is in its own directory, so finding it lists one directory, collecting it lists the same one, and two units never write one.
    pub fn slot(&self, slot: &str) -> PathBuf {
        self.directory.join(VERDICTS).join(slot)
    }

    /// Where a judged unit's verdict is filed in its [`slot`](Self::slot): one file, holding the record of what it was compiled *from* ahead of the unit's index, verified when the slot is opened rather than spelled here.
    pub fn verdict(&self, slot: &str) -> PathBuf {
        self.slot(slot).join(VERDICT)
    }

    /// Where an executable's precompiled payload is filed, under the address its own identity, the compiler and the engine decide: one file, laid out as a unit's is, with what it was compiled *from* verified when the slot is opened.
    ///
    /// In the cabinet rather than the products, for the reason the type states: the address says nothing about which project asked. A build stays local because its identity *is* project-relative; the payload inside it is not.
    pub fn payload(&self, runs: Runs<'_>, slot: &str) -> PathBuf {
        self.directory
            .join(PAYLOADS)
            .join(runs.target().spelled())
            .join(slot)
    }

    /// Where this machine's memo of its compiler's digest lives — a fact about the machine, so it belongs beside the things every project shares.
    pub fn compiler(&self) -> PathBuf {
        self.directory.join("compiler")
    }
}

/// What version of the verdict family's layout a key names.
///
/// In the key rather than in a file beside it, so an entry written by an older layout is not found rather than found and misread. Bump it whenever what a slot holds, or what a hit is verified against, changes.
const VERDICT_SCHEMA: &str = "u114";

/// The same, for the payload family — its own tag, because the two families version independently and neither should invalidate the other by moving.
const PAYLOAD_SCHEMA: &str = "p13";

/// The slot a unit compiled by `compiler`, claiming `mounts`, declaring `declared`, `compiled` built or tested, and in a fold whose first unit stands in for the archived root claiming `stand_in` where one does, is filed under.
///
/// **No file contents here, deliberately.** This names a *place*, not a version of what lives in it: "the unit for these mounts, declaring these dependencies, by this compiler". What was compiled is checked when the slot is opened, against the record of what was read, so a source edit changes the verification and not the address. That is what bounds the cabinet — a project has as many slots as it has units, forever, rather than one per compile, as an address hashed over the unit's source directory, which contains its `.curios/`, would give.
///
/// **Five parts, and each is load-bearing.** The compiler is who judged it — see [`compiler`](crate::compiler()), and note that a key naming no compiler would be believed on behalf of any. The mounts are how the unit's names are spelled, which the lowering depends on and no read of its files reveals. The third is what the unit declared it could see: a declared dependency answers "may these names reach that root" per unit, and it is the answer the lowering consults — the same source declaring different prefixes resolves different names and is a different unit. `None` is its own value, not an empty list: it means the caller declared nothing, which is every open prefix in scope, where an empty list means a unit that asked for none of them. The fourth is what the unit is compiled for: a tested unit holds its tests and a built one none, so the same source is two units. The fifth is the archived root a unit of its fold stands in for, as a package named `std` does: what a unit reads of the compiler's own is named by the compiler alone and recorded nowhere, so a unit that read a stand-in in its place is another unit, and none is named where no unit stands in, so every other address is the one it was.
///
/// **No unit before it.** What a unit read of the units before it is verified, as its own source is: its record names what each unit it could have read contained, and a slot whose record no longer agrees is the baseline its unit is compiled over. So a unit two folds reach after different others is filed once, and an edit to a unit it cannot read moves neither its address nor its record.
pub fn unit_slot(
    compiler: &str,
    mounts: &[Mount],
    declared: Option<&[Qualifier]>,
    compiled: Compiled,
    stand_in: Option<&[Qualifier]>,
) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(VERDICT_SCHEMA);
    fingerprint.feed(compiler);
    fingerprint.feed(mounts.len().to_string());
    for mount in mounts {
        fingerprint.feed(mount.prefix.join());
    }
    feed_declared(&mut fingerprint, declared);
    feed_compiled(&mut fingerprint, compiled);
    feed_stand_in(&mut fingerprint, stand_in);

    fingerprint.hex()
}

/// The slot the unit of an entry's own declarations is filed under: `package`'s `executable`, compiled by `compiler`, declaring `declared`, `compiled` built or tested, and over the stand-in `stand_in` names where its fold has one.
///
/// **An entry claims the empty prefix, as every entry does**, so [`unit_slot`] would file every program of a project in one place, each overwriting the last. What tells two entries apart is what tells their payloads apart ([`payload_slot`]): the package's name and the executable's, the one identity in a compilation that cannot collide. A program no package declares has none, and its unit is filed nowhere.
///
/// What was compiled is verified when the slot is opened, as a unit's is, the entry's own file among what it read.
pub fn entry_slot(
    compiler: &str,
    package: &str,
    executable: &str,
    declared: Option<&[Qualifier]>,
    compiled: Compiled,
    stand_in: Option<&[Qualifier]>,
) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(VERDICT_SCHEMA);
    fingerprint.feed(compiler);
    // Where a unit's slot states how many prefixes it claims, which no word spells.
    fingerprint.feed("executable");
    fingerprint.feed(package);
    fingerprint.feed(executable);
    feed_declared(&mut fingerprint, declared);
    feed_compiled(&mut fingerprint, compiled);
    feed_stand_in(&mut fingerprint, stand_in);

    fingerprint.hex()
}

/// The archived root a unit of the fold stands in for, as a slot states it: nothing where none does, so an address over the archived root is the one it was before a stand-in had a word.
fn feed_stand_in(fingerprint: &mut Fingerprint, stand_in: Option<&[Qualifier]>) {
    if let Some(prefixes) = stand_in {
        fingerprint.feed("stand-in");
        fingerprint.feed(prefixes.len().to_string());
        for prefix in prefixes {
            fingerprint.feed(prefix.join());
        }
    }
}

/// What a unit is compiled for, as a slot states it: one word either way, so neither address is a prefix of the other's.
fn feed_compiled(fingerprint: &mut Fingerprint, compiled: Compiled) {
    fingerprint.feed(match compiled {
        Compiled::Built => "built",
        Compiled::Tested => "tested",
    });
}

/// What a unit declared it could see, as a slot states it.
fn feed_declared(fingerprint: &mut Fingerprint, declared: Option<&[Qualifier]>) {
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
}

/// The slot the precompiled payload of `package`'s `executable`, built by `compiler` and run by the engine `engine` fingerprints, is filed under.
///
/// **The same address-and-record split [`unit_slot`] states, one level up.** This names a place — "the payload for this executable, by this compiler, for this engine" — and carries no file contents and no unit, so an edit changes what the slot is verified against rather than where it is looked for: one slot per executable per compiler per engine, overwritten in place, bounded exactly as the unit family is. What every unit of the compilation was filed as is in the payload's record.
///
/// **The executable's identity is its package's name and its own**, never a path. That pair is the one identity in a compilation that cannot collide (law 2), which is why two executables of one package occupy two slots and why nothing has to refuse an umbrella whose members both declare `serve`. A path here would key the payload on where a project sits rather than on what it builds.
///
/// **The engine fingerprint is the part [`unit_slot`] has no counterpart for.** A unit is a judgment and travels anywhere the compiler binary does; a payload is machine code, and Cranelift compiled it for the host's ISA — the one input neither the compiler digest nor any recorded source file covers. It rides here as an opaque string because deciding what makes two engines compatible is `curios-native`'s to answer, this crate's only job being to keep the answer in the address.
pub fn payload_slot(compiler: &str, package: &str, executable: &str, runs: Runs<'_>) -> String {
    let mut fingerprint = Fingerprint::new();

    fingerprint.feed(PAYLOAD_SCHEMA);
    fingerprint.feed(compiler);
    fingerprint.feed(package);
    fingerprint.feed(executable);
    if let Runs::Native { engine } = runs {
        fingerprint.feed(engine);
    }

    fingerprint.hex()
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
