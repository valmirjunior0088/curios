//! What the fold consults before compiling a unit again.
//!
//! The store's layout and its keys belong to `curios-package`; reading and writing a `Unit` through them belongs here, because [`Cache`] is `curios-pipeline`'s trait and `curios-package` sits *beside* that boundary rather than under it. Implementing it there would make the crate that answers "what is in this compilation" depend on the driver that folds stages over the answer, which is the one direction the layering forbids.
//!
//! It is not, as this said until it was checked, about keeping `curios-package` free of the compiler. That crate depends on `curios-text` and so already links the elaborator — `curios new` included. The dependency the boundary actually buys is the driver's.
//!
//! **Taking a unit from here is believing a verdict this compiler reached earlier.** That is a change to what the compiler believes rather than a faster way to do what it already did, and the argument for it is in [Cached verdicts](../../documentation/soundness/admission-without-judgment/cached-verdicts.md). Everything below is the mechanism the argument is about.
//!
//! **A slot is addressed, and a hit is verified.** The address ([`unit_slot`]) names a place — these mounts, this compiler, this predecessor chain — and holds no file contents at all, so a project has as many slots as it has units rather than one per compile. What the unit was compiled *from* rides in a [`Record`] beside it and is checked when the slot is opened: every file the compilation read, by the text it read, plus what each predecessor contained.
//!
//! That split is deliberate, and the previous scheme is why. It hashed the unit's whole source directory into the address — a directory that, for a package's own library, *contains this store*. Filing a unit therefore changed the address it would next be looked for under, so a package's own code never hit and `unit/` grew a directory per compile. The lesson is not "exclude the store from the walk": a key derived from a belief about what the inputs are goes wrong silently the day the belief does, and it goes wrong in the direction that hands back a stale unit. Here the inputs are not believed but recorded, at the one seam every module read passes through (`RootSource::reads`), so a compilation that reads something new records it without anything here being taught to expect it.

#[cfg(test)]
mod tests;

use {
    curios_archive::rkyv,
    curios_package::{Store, compiler, digest, unit_slot},
    curios_pipeline::Cache,
    curios_text::UnitSource,
    curios_unit::Unit,
    std::{
        cell::RefCell,
        fs, io,
        path::{Path, PathBuf},
    },
};

/// The file a stored unit is written as, inside its slot.
const STORED: &str = "unit.rkyv";

/// The file recording what that unit was compiled from.
const RECORD: &str = "record.rkyv";

/// What a stored unit must still be true of to be believed.
///
/// Every field is a fact the compilation depended on and the address deliberately does not carry. Verification is all of them or nothing: a record that cannot be read, or that disagrees anywhere, is a miss.
// Derived rather than taken from `curios_archive::archived`, which expands under a `cfg(feature = "archive")` this crate neither has nor wants: a product that always reads and writes archives has nothing to gate.
#[derive(curios_archive::Archive, curios_archive::Serialize, curios_archive::Deserialize)]
#[rkyv(crate = ::curios_archive::rkyv)]
struct Record {
    /// Each file the compilation read, by canonical path, with the digest of the text that was parsed from it. Sorted, because `RootSource::reads` yields a map.
    reads: Vec<(String, String)>,
    /// What each predecessor contained, in fold order — the digest of the bytes its own slot holds. Ordered for the same reason the address orders their slots: two orders of one set are two lowerings.
    predecessors: Vec<String>,
}

/// The store, as the fold sees it.
pub struct Verdicts {
    store: Store,
    /// `None` when the compiler cannot identify itself — in which case nothing is read and nothing is written, because a verdict recorded under an identity nobody can reproduce would later be believed on behalf of a different compiler.
    compiler: Option<String>,
    /// The units placed so far, in fold order: each one's slot, which the next unit's address is built from, and the digest of what it contained, which the next unit's record is verified against.
    placed: RefCell<Vec<(String, String)>>,
    /// Why the store could not be written, if it could not.
    ///
    /// The first refusal and not a count: a store nobody can write refuses every unit for one reason, so the reason is the whole of what a reader needs and repeating it per unit would say nothing new. Recorded rather than reported here because this crate has no terminal — see [`Verdicts::refused`].
    refused: RefCell<Option<String>>,
}

impl Verdicts {
    /// The store beside `root`.
    pub fn at(root: PathBuf) -> Self {
        let store = Store::at(root);

        Self {
            compiler: compiler(&store),
            store,
            placed: RefCell::new(Vec::new()),
            refused: RefCell::new(None),
        }
    }

    /// Why nothing was filed, for a caller with somewhere to say it.
    ///
    /// **A store that cannot be written is invisible without this.** Every compilation silently recompiles everything, forever, and the only symptom is that builds are slower than they should be — which reads as the compiler being slow rather than as a directory being unwritable. It is still not a failure: the verdict is unaffected, so this is reported and the build is not stopped.
    pub fn refused(&self) -> Option<String> {
        self.refused.borrow().clone()
    }

    /// The slot `source` is filed in, or `None` when it may not be filed at all.
    ///
    /// A source with nothing on disk has no reads to verify against, so a record of it would confirm nothing. That is the fixed prelude, which has an archive of its own.
    fn slot(&self, source: &UnitSource<'_>) -> Option<String> {
        let compiler = self.compiler.as_ref()?;

        if source.directories().is_empty() {
            return None;
        }

        let placed = self.placed.borrow();
        let predecessors = placed
            .iter()
            .map(|(slot, _)| slot.clone())
            .collect::<Vec<_>>();

        Some(unit_slot(compiler, &predecessors, &source.claims()))
    }

    /// Whether `record` still describes the world: every file it names still holds the text it was read as, and every predecessor still contains what it did.
    ///
    /// A file that has since vanished, changed, or become unreadable is a disagreement like any other. So is a shorter or longer read list, which is what catches a module added or removed — though that alone never has to catch it, since a module can only join a unit through a `mod` in a header that is itself on this list.
    ///
    /// **A recorded file must also be one `source` could itself have read**, and that clause is what keeps a *shared* store from admitting across projects. The address carries no file contents, so two projects that each hold a package of one name, compiled by one compiler after one chain, address the same slot; without this, the second opens the first's record, finds the first's files unchanged on disk because nothing touched them, and is handed a unit compiled from source it has never seen. Checking containment rather than re-deriving the read set keeps the check exact: a git dependency is materialized once under the shared store and read from that same path by every project, so genuine sharing survives.
    fn agrees(&self, source: &UnitSource<'_>, record: &Record) -> bool {
        let placed = self.placed.borrow();

        if record.predecessors.len() != placed.len() {
            return false;
        }

        if record
            .predecessors
            .iter()
            .zip(placed.iter())
            .any(|(recorded, (_, contained))| recorded != contained)
        {
            return false;
        }

        // Canonical on both sides, because a record's paths are canonical and a source's directories are however the manifest walk spelled them.
        let within = source
            .directories()
            .into_iter()
            .map(|directory| {
                directory
                    .canonicalize()
                    .unwrap_or_else(|_| directory.to_path_buf())
            })
            .collect::<Vec<_>>();

        record.reads.iter().all(|(path, recorded)| {
            let path = Path::new(path);

            within.iter().any(|directory| path.starts_with(directory))
                && fs::read(path).is_ok_and(|bytes| &digest(&bytes) == recorded)
        })
    }

    /// Write `bytes`, and the record of what produced them, into `slot` — replacing whatever it held.
    ///
    /// **The order is the whole of it.** The old record goes first, the unit second, the new record last, so every state an interrupted run can leave behind is one that reads as a miss. Both other orders admit: a record written before its unit vouches for whatever the slot held previously, and a record left in place while the unit beneath it is replaced vouches for source that unit was never compiled from — which a later run walks into by reverting the very edit that caused this write.
    ///
    /// That is also why a failed removal abandons the write instead of being ignored. Finding no record to remove is the ordinary case on a fresh slot; failing to remove one that *is* there leaves precisely the state this ordering exists to prevent.
    fn file(&self, slot: &str, source: &UnitSource<'_>, bytes: &[u8]) -> io::Result<()> {
        let directory = self.store.unit(slot);
        let record = rkyv::to_bytes::<rkyv::rancor::Error>(&self.recorded(source))
            .map_err(io::Error::other)?;

        // Every failure names the directory it happened in: an error reading `Permission denied` alone leaves a reader guessing which of the three families under `.curios/` refused.
        let at = |error: io::Error| io::Error::other(format!("{}: {error}", directory.display()));

        fs::create_dir_all(&directory).map_err(at)?;

        match fs::remove_file(directory.join(RECORD)) {
            Err(error) if error.kind() != io::ErrorKind::NotFound => return Err(at(error)),
            _ => {}
        }

        fs::write(directory.join(STORED), bytes).map_err(at)?;
        fs::write(directory.join(RECORD), &record).map_err(at)
    }

    /// What `source` read, as the record of it.
    fn recorded(&self, source: &UnitSource<'_>) -> Record {
        Record {
            reads: source
                .reads()
                .into_iter()
                .map(|(path, text)| {
                    (
                        path.to_string_lossy().into_owned(),
                        digest(text.text.as_bytes()),
                    )
                })
                .collect(),
            predecessors: self
                .placed
                .borrow()
                .iter()
                .map(|(_, contained)| contained.clone())
                .collect(),
        }
    }
}

impl Cache for Verdicts {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        let slot = self.slot(source)?;
        let directory = self.store.unit(&slot);

        // Read before deserializing either: a slot whose record disagrees is not worth the unit's decode, and a slot missing one of the two files is a half-written store to ignore.
        let recorded = fs::read(directory.join(RECORD)).ok()?;
        let bytes = fs::read(directory.join(STORED)).ok()?;

        // A stored unit that will not read back is a store to ignore, never a compile to fail: the source it was made from is still there, and recompiling costs time rather than correctness.
        let record = rkyv::from_bytes::<Record, rkyv::rancor::Error>(&recorded).ok()?;

        if !self.agrees(source, &record) {
            return None;
        }

        let restored = rkyv::from_bytes::<Unit, rkyv::rancor::Error>(&bytes).ok()?;

        self.placed.borrow_mut().push((slot, digest(&bytes)));

        Some(restored)
    }

    fn put(&self, source: &UnitSource<'_>, unit_: &Unit) {
        let Some(slot) = self.slot(source) else {
            return;
        };

        // The rule a stored unit is checked against, at the second seam a unit is written — the first being the prelude's build script. An identity meaningful only in the compilation that made it has no safe direction to degrade in: restored beside a unit whose own counters hand out the same index, it aliases silently rather than failing, which admits. Storing nothing is always safe, so a unit that would carry one is dropped rather than refused: the compilation it came from is correct, and only the record is withheld.
        if curios_core::validate_stored_identities(unit_.core()).is_err() {
            return;
        }

        let Ok(bytes) = rkyv::to_bytes::<rkyv::rancor::Error>(unit_) else {
            return;
        };

        // Best effort: a store that cannot be written costs the next compilation the work it would have saved, and nothing else. What it must never do is cost the verdict — so this unit enters the chain below whether or not any of it landed, and the refusal is kept for a caller to report rather than raised here.
        if let Err(error) = self.file(&slot, source, &bytes) {
            self.refused.borrow_mut().get_or_insert(error.to_string());
        }

        self.placed.borrow_mut().push((slot, digest(&bytes)));
    }
}
