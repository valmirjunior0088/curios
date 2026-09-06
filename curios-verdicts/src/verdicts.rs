//! The unit family: what the fold consults before compiling a unit again, and what it files when it did.
//!
//! **A slot is addressed, and a hit is verified.** The address (`unit_slot`) names a place — these mounts, this compiler, this predecessor chain — and holds no file contents at all, so a project has as many slots as it has units rather than one per compile. What the unit was compiled *from* rides in a [`Record`] ahead of it in the slot's one file and is checked when the slot is opened: every file the compilation read, by the text it read, plus what each predecessor contained, plus what the slot itself holds — so a record vouches for the bytes it was written with and for no others.
//!
//! That split is deliberate, and the previous scheme is why. It hashed the unit's whole source directory into the address — a directory that, for a package's own library, *contains this store*. Filing a unit therefore changed the address it would next be looked for under, so a package's own code never hit and `verdicts/` grew a directory per compile. The lesson is not "exclude the store from the walk": a key derived from a belief about what the inputs are goes wrong silently the day the belief does, and it goes wrong in the direction that hands back a stale unit. Here the inputs are not believed but recorded, at the one seam every module read passes through (`RootSource::reads`), so a compilation that reads something new records it without anything here being taught to expect it.

#[cfg(test)]
mod tests;

use {
    crate::{replace, segments},
    curios_package::{Store, compiler, digest, unit_slot},
    curios_pipeline::Cache,
    curios_text::{Overlay, UnitSource},
    curios_unit::Unit,
    curios_utilities::Source,
    std::{
        cell::RefCell,
        fs, io,
        path::{Path, PathBuf},
        rc::Rc,
    },
};

/// What a stored unit must still be true of to be believed.
///
/// Every field is a fact the compilation depended on and the address deliberately does not carry. Verification is all of them or nothing: a record that cannot be read, or that disagrees anywhere, is a miss.
// `always`: a product that reads and writes archives unconditionally has no `archive` feature for a `cfg_attr` to gate on.
#[curios_archive::archived(always)]
struct Record {
    /// Each file the compilation read, by canonical path, with the digest of the text that was parsed from it. Sorted, because `RootSource::reads` collects its vector out of a `BTreeMap`.
    reads: Vec<(String, String)>,
    /// What each predecessor contained, in fold order — the digest of the bytes its own slot holds. Ordered for the same reason the address orders their slots: two orders of one set are two lowerings.
    predecessors: Vec<String>,
    /// The stored unit's own digest, so a damaged unit that still deserializes is a miss rather than a belief, as the payload family's own digest makes a damaged artifact. Bytecheck confirms a unit's structure and nothing about its contents: a flipped byte inside a string reads back as a different string. It no longer stands between two compilers filing one slot at once — a slot is one file renamed into place, so no write can leave a record beside a unit it was not made from.
    unit: String,
}

/// One unit's place in the chain a compilation builds.
///
/// Both halves are needed by whatever comes after it and neither substitutes for the other, which is why this is a pair rather than one string: the *slot* is where the next unit's address is anchored, and what the slot *contains* is what the next unit's record is verified against. A chain of these is the whole of what one unit hands the next, as far as the store is concerned.
pub(crate) struct Placed {
    /// The slot the unit is filed in.
    pub(crate) slot: String,
    /// The digest of the bytes that slot holds.
    pub(crate) contained: String,
}

/// The store, as a compilation sees it — the fold's units through [`Cache`], and the invocation's own payload through [`Verdicts::payload_get`] and [`Verdicts::payload_put`].
///
/// One handle for both because they are one store: the same compiler identity decides both addresses, the chain the fold places *is* the payload's predecessor half, and a directory nobody can write refuses both for one reason, which [`Verdicts::refused`] reports once.
pub struct Verdicts {
    pub(crate) store: Store,
    /// `None` when the compiler cannot identify itself — in which case nothing is read and nothing is written, because a verdict recorded under an identity nobody can reproduce would later be believed on behalf of a different compiler.
    pub(crate) compiler: Option<String>,
    /// The units placed so far, in fold order. Read afterwards by [`Verdicts::payload_put`], which files what the whole chain compiled to.
    pub(crate) placed: RefCell<Vec<Placed>>,
    /// Why the store could not be written, if it could not.
    ///
    /// The first refusal and not a count: a store nobody can write refuses every unit for one reason, so the reason is the whole of what a reader needs and repeating it per unit would say nothing new. Recorded rather than reported here because this crate has no terminal — see [`Verdicts::refused`].
    pub(crate) refused: RefCell<Option<String>>,
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

    /// The slot `source` is filed in after `placed`, or `None` when it may not be filed at all.
    ///
    /// A source with nothing on disk has no reads to verify against, so a record of it would confirm nothing. That is the fixed prelude, which has an archive of its own.
    ///
    /// The chain is a parameter rather than a read of [`Verdicts::placed`] because two callers walk one: the fold accumulates as it goes, and the payload probe re-derives the same chain before the fold has run at all.
    fn slot(&self, source: &UnitSource<'_>, placed: &[Placed]) -> Option<String> {
        let compiler = self.compiler.as_ref()?;

        if source.directories().is_empty() {
            return None;
        }

        let predecessors = placed
            .iter()
            .map(|placed| placed.slot.clone())
            .collect::<Vec<_>>();

        Some(unit_slot(compiler, &predecessors, &source.claims()))
    }

    /// The chain `sources` form, verified against the store without deserializing any of it — the probe [`Verdicts::payload_get`] decides a hit with.
    ///
    /// **This is the verification half of [`Cache::get`], and it is shared rather than restated.** Each source's slot and record are decided by exactly the calls the fold makes, so the two cannot come to different answers about whether a unit is still good; all that is left out is the `Unit` decode, which a payload hit has no use for. A stale unit is `None` and so a payload miss by construction — it is about to recompile into bytes no record of the payload could match.
    ///
    /// A source the store may not file at all is also `None`, which is stricter than the fold, where such a unit is simply compiled every time. The strictness is the point: a payload vouches for the *whole* compilation, and a unit nothing can verify is a part of it nothing can vouch for.
    pub(crate) fn chain(&self, sources: &[UnitSource<'_>]) -> Option<Vec<Placed>> {
        let mut placed: Vec<Placed> = Vec::new();

        for source in sources {
            let slot = self.slot(source, &placed)?;

            let filed = fs::read(self.store.verdict(&slot)).ok()?;
            let (recorded, bytes) = segments(&filed)?;
            let record = curios_archive::from_bytes::<Record>(recorded).ok()?;

            if !agrees(source, &record, &placed, bytes, None) {
                return None;
            }

            placed.push(Placed {
                slot,
                contained: digest(bytes),
            });
        }

        Some(placed)
    }

    /// [`Cache::get`] for a compilation reading through `overlay`: the record is verified against the text the compilation would read, which for a file the overlay holds is the overlay's and for every other the disk's.
    ///
    /// **Exact where a containment guess was not.** The record lists every file the unit was compiled from, so an open file the unit never read — an executable beside a package's library — leaves the hit standing, an open file it did read hits while its text is unchanged and misses the moment it is edited, and nothing about what a unit reads has to be believed twice. The `wonder` engine, which answers a question from an editor's unsaved buffers, is the consumer.
    pub fn get_overlaid(&self, source: &UnitSource<'_>, overlay: &Overlay) -> Option<Unit> {
        self.hit(source, Some(overlay))
    }

    /// The stored unit for `source` when its record still agrees, read through `overlay` where there is one, placed in the chain on a hit.
    fn hit(&self, source: &UnitSource<'_>, overlay: Option<&Overlay>) -> Option<Unit> {
        let slot = self.slot(source, &self.placed.borrow())?;

        // The record is judged before the unit is decoded: a slot whose record disagrees is not worth the decode, and a file that is not a slot is a store to ignore.
        let filed = fs::read(self.store.verdict(&slot)).ok()?;
        let (recorded, bytes) = segments(&filed)?;

        // A stored unit that will not read back is a store to ignore, never a compile to fail: the source it was made from is still there, and recompiling costs time rather than correctness.
        let record = curios_archive::from_bytes::<Record>(recorded).ok()?;

        if !agrees(source, &record, &self.placed.borrow(), bytes, overlay) {
            return None;
        }

        let restored = curios_archive::from_bytes::<Unit>(bytes).ok()?;

        self.placed.borrow_mut().push(Placed {
            slot,
            contained: digest(bytes),
        });

        Some(restored)
    }

    /// Place `unit` in the chain without filing it: what a caller that may read the store but not write it — the `wonder` engine, answering a question — does with a unit it had to compile.
    ///
    /// **Placing and filing are one call but not one decision, and only filing is optional.** A slot is addressed after the units placed before it, so a unit left out of the chain shifts every later unit's address by one — turning one declined hit into a miss for the whole tail, which is the cost declining it was supposed to avoid. Serializing without writing is what placing costs instead: the digest of those bytes is the fact the next unit's record is verified against, and nothing cheaper produces it.
    pub fn place(&self, source: &UnitSource<'_>, unit: &Unit) {
        if let Some((placed, _)) = self.placement(source, unit) {
            self.placed.borrow_mut().push(placed);
        }
    }

    /// The place `unit` takes in the chain after everything placed so far, and the bytes it would be filed as — `None` when it may not be placed at all.
    ///
    /// Shared by [`Verdicts::place`] and [`Cache::put`], so the two cannot come to different answers about which units enter the chain: a unit this refuses is one neither may place, or a successor's record would vouch for a predecessor that nothing here can produce again.
    fn placement(
        &self,
        source: &UnitSource<'_>,
        unit: &Unit,
    ) -> Option<(Placed, curios_archive::Serialized)> {
        let slot = self.slot(source, &self.placed.borrow())?;

        // The rule a stored unit is checked against, at the second seam a unit is written — the first being the prelude's build script. An identity meaningful only in the compilation that made it has no safe direction to degrade in: restored beside a unit whose own counters hand out the same index, it aliases silently rather than failing, which admits. Storing nothing is always safe, so a unit that would carry one is dropped rather than refused: the compilation it came from is correct, and only the record is withheld.
        if curios_core::validate_stored_identities(unit.core()).is_err() {
            return None;
        }

        let bytes = curios_archive::to_bytes(unit).ok()?;

        Some((
            Placed {
                slot,
                contained: digest(&bytes),
            },
            bytes,
        ))
    }
}

impl Cache for Verdicts {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.hit(source, None)
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        let Some((placed, bytes)) = self.placement(source, unit) else {
            return;
        };

        let filed =
            curios_archive::to_bytes(&recorded(source, &self.placed.borrow(), &placed.contained))
                .map_err(io::Error::other)
                .and_then(|record| replace(&self.store.verdict(&placed.slot), &record, &bytes));

        // Best effort: a store that cannot be written costs the next compilation the work it would have saved, and nothing else. What it must never do is cost the verdict — so this unit enters the chain below whether or not any of it landed, and the refusal is kept for a caller to report rather than raised here.
        if let Err(error) = filed {
            self.refused.borrow_mut().get_or_insert(error.to_string());
        }

        self.placed.borrow_mut().push(placed);
    }
}

/// Whether `record` still describes the world: the slot holds the bytes it was written with, every file it names still holds the text it was read as — through `overlay`, where the compilation reads through one — and every predecessor still contains what it did.
///
/// The slot's own digest comes first, as the payload family orders its check: it decides a torn or damaged slot before any file is opened.
///
/// A file that has since vanished, changed, or become unreadable is a disagreement like any other. So is a shorter or longer read list, which is what catches a module added or removed — though that alone never has to catch it, since a module can only join a unit through a `mod` in a header that is itself on this list.
///
/// **A recorded file must also be one `source` could itself have read**, and that clause is what keeps a *shared* store from admitting across projects. The address carries no file contents, so two projects that each hold a package of one name, compiled by one compiler after one chain, address the same slot; without this, the second opens the first's record, finds the first's files unchanged on disk because nothing touched them, and is handed a unit compiled from source it has never seen. Checking containment rather than re-deriving the read set keeps the check exact: a git dependency is materialized once under the shared store and read from that same path by every project, so genuine sharing survives.
fn agrees(
    source: &UnitSource<'_>,
    record: &Record,
    placed: &[Placed],
    bytes: &[u8],
    overlay: Option<&Overlay>,
) -> bool {
    record.unit == digest(bytes)
        && chained(&record.predecessors, placed)
        && read_within(&source.directories(), &record.reads, overlay)
}

/// What `source` read after `placed`, as the record of it, ahead of a unit whose bytes digest to `contained`.
fn recorded(source: &UnitSource<'_>, placed: &[Placed], contained: &str) -> Record {
    Record {
        reads: digested(source.reads()),
        predecessors: placed
            .iter()
            .map(|placed| placed.contained.clone())
            .collect(),
        unit: contained.to_string(),
    }
}

/// Whether `recorded` is what `placed` contains, position by position.
pub(crate) fn chained(recorded: &[String], placed: &[Placed]) -> bool {
    recorded.len() == placed.len()
        && recorded
            .iter()
            .zip(placed)
            .all(|(recorded, placed)| recorded == &placed.contained)
}

/// Whether every file in `reads` lies under one of `directories` and still holds the text it was recorded as — the overlay's text for a file `overlay` holds, since that is what the compilation would read, and the disk's for every other.
///
/// The containment half is what keeps a shared store from admitting across projects; see [`agrees`] for why it is a containment check rather than a re-derivation of the read set.
pub(crate) fn read_within(
    directories: &[&Path],
    reads: &[(String, String)],
    overlay: Option<&Overlay>,
) -> bool {
    // Canonical on both sides, because a record's paths are canonical and a source's directories are however the manifest walk spelled them.
    let within = directories
        .iter()
        .map(|directory| {
            directory
                .canonicalize()
                .unwrap_or_else(|_| directory.to_path_buf())
        })
        .collect::<Vec<_>>();

    reads.iter().all(|read| {
        within
            .iter()
            .any(|directory| holds(directory, read, overlay))
    })
}

/// Whether `read`'s file lies under `directory` and still holds the text it was recorded as, read through `overlay` where there is one.
fn holds(directory: &Path, (path, recorded): &(String, String), overlay: Option<&Overlay>) -> bool {
    let path = Path::new(path);

    path.starts_with(directory)
        && match overlay.and_then(|overlay| overlay.get(path)) {
            Some(text) => &digest(text.as_bytes()) == recorded,
            None => fs::read(path).is_ok_and(|bytes| &digest(&bytes) == recorded),
        }
}

/// A read log as it is recorded: each file by canonical path, with the digest of the text that was parsed from it.
pub(crate) fn digested(reads: Vec<(PathBuf, Rc<Source>)>) -> Vec<(String, String)> {
    reads
        .into_iter()
        .map(|(path, text)| {
            (
                path.to_string_lossy().into_owned(),
                digest(text.text.as_bytes()),
            )
        })
        .collect()
}
