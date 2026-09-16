//! The unit family: what the fold consults before compiling a unit again, and what it files when it did.
//!
//! **A slot is addressed, and a hit is verified.** The address (`unit_slot`) names a place — these mounts, this compiler, this predecessor chain — and holds no file contents at all, so a project has as many slots as it has units rather than one per compile. What the unit was compiled *from* rides in a [`Record`] ahead of it in the slot's one file and is checked when the slot is opened: every file the compilation read, by the text it read, plus what each predecessor contained, plus what the slot itself holds — so a record vouches for the bytes it was written with and for no others.
//!
//! That split is deliberate, and the previous scheme is why. It hashed the unit's whole source directory into the address — a directory that, for a package's own library, *contains this store*. Filing a unit therefore changed the address it would next be looked for under, so a package's own code never hit and `verdicts/` grew a directory per compile. The lesson is not "exclude the store from the walk": a key derived from a belief about what the inputs are goes wrong silently the day the belief does, and it goes wrong in the direction that hands back a stale unit. Here the inputs are not believed but recorded, at the one seam every module read passes through (`RootSource::reads`), so a compilation that reads something new records it without anything here being taught to expect it.
//!
//! What a record holds and how it is framed ahead of the unit are `curios-unit`'s, since the prelude image is written the same way by a build script below every store; what a record is *verified against* is here.

#[cfg(test)]
mod tests;

use {
    crate::replace,
    curios_package::{Store, compiler, unit_slot},
    curios_pipeline::Cache,
    curios_text::{Overlay, UnitSource},
    curios_unit::{Record, Unit, digested, read_within, segments},
    curios_utilities::digest,
    std::{
        cell::{OnceCell, RefCell},
        collections::BTreeMap,
        fs, io,
        path::{Path, PathBuf},
        rc::Rc,
    },
};

/// The last unit one session compiled for each unit it reached — the baseline a question takes when the store holds nothing nearer.
///
/// **Shared rather than owned by a store handle, because it outlives every one of them.** A handle lives for one question; this lives for as long as whoever made it. That difference is the whole point: a language server compiles a unit, the next keystroke compiles over what it just produced, and the keystroke after that over *that*, so the closure an edit re-elaborates is the closure of *that* edit rather than of everything done since the last build. Without it the baseline is the last unit the store was written with, which nothing in an editing session ever advances — so one edit to a widely-used declaration leaves every later keystroke re-elaborating its whole reverse closure, for the rest of the session.
///
/// **What this asks of the trusted base is argued in [Cached verdicts](../../documentation/soundness/admission-without-judgment/cached-verdicts.md)**, which owns it: an item reused from a unit that was itself a recompile, by induction on the per-item argument, and the guard below for what that argument does not cover. Nothing here is *filed* — a kept unit dies with the process.
///
/// **A kept unit is offered only after units that read what they read when it was kept.** The recompile diffs a unit's *own* lowered items and nothing else, so it cannot see that a unit before it changed: a reference into an edited predecessor lowers to the same name either way, the diff comes out empty, and every item reaching the edit would be reused on the strength of a definition that is gone. A slot cannot catch this, since it addresses a place — predecessors by *where* they are, not by what they hold. So each kept unit carries the read log of every unit the fold took before it, and is offered only while this fold's units read the same, position by position.
///
/// **Reads, not bytes, because reads are what the definitions depend on.** A unit's meaning is fixed by what it read, the units before it, and the compiler that read it; the slot covers the compiler and the places, and the read logs cover the rest, so by induction from the first unit equal logs mean every earlier unit holds the same definitions — whether it was compiled whole, recompiled, or restored from a slot. That last clause is why the digest of a predecessor's bytes, which is what the store checks a *filed* unit's chain by, is the wrong evidence here. The store needs bytes because a hit reuses its arena, which is built on the bytes of every unit before it; a kept unit is only ever a baseline, which lends its elaborated items and its lowered text while the recompile erases whole onto the arena in hand. And bytes refuse where nothing changed: a recompiled unit serializes differently from the same unit compiled whole, though each of its parts serializes the same, so a byte guard refused every unit after one that had flipped between a hit and a recompile.
///
/// **The log is the fold's, not the chain's.** A unit the store may not place — one carrying an identity meaningful only in its own compilation — is missing from the placed chain, so a guard read off that chain would pass over a change to it. Every unit the fold takes, restored or compiled, adds its log to [`Verdicts`]'s own, placed or not. What the guard costs is only ever a refusal, and a refused kept unit falls back to the store.
///
/// **One entry per unit, whatever scope it was compiled in.** Keyed by which unit it is — the prefixes it claims and the directories it reads — and not by its slot, which also names the scope: a manifest edit that adds, reorders or narrows a dependency gives every later unit a new slot, and a map keyed by slot would strand the old entry beside the new one for as long as the session lives, each holding a whole unit. Keyed this way, an edit and a rescoping both *replace*, so the map is bounded by the units the editor has reached, as the parse memo is by the files. The slot rides inside the entry instead, and a kept unit is offered only where it still matches. The directories are what keep two projects apart: a package of one name in each claims the same prefix and, compiled after the same chain, addresses the same slot, which is sound to share — a baseline is only diffed against — but a poor baseline for either, and each would keep overwriting the other.
///
/// A one-shot invocation makes none of these and behaves exactly as it did.
#[derive(Clone, Default)]
pub struct Session {
    units: Rc<RefCell<BTreeMap<Identity, Kept>>>,
}

/// Which unit a source is, whatever scope it is compiled in: the prefixes it claims, and the directories it reads from.
type Identity = (Vec<String>, Vec<PathBuf>);

/// The [`Identity`] of `source`.
fn identity(source: &UnitSource<'_>) -> Identity {
    (
        source
            .claims()
            .iter()
            .map(|mount| mount.prefix.join())
            .collect(),
        source
            .directories()
            .into_iter()
            .map(Path::to_path_buf)
            .collect(),
    )
}

/// Each file one unit read, by canonical path, with the digest of the text parsed from it — spelled as a record spells it, since a restored unit's log *is* its record's.
type ReadLog = Vec<(String, String)>;

/// A unit a session compiled, beside the slot it was compiled at and the read log of every unit the fold took before it — the evidence a kept unit is checked against before it is offered.
struct Kept {
    slot: String,
    earlier: Vec<ReadLog>,
    unit: Unit,
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
    /// The identity of the compiler running now, asked for on the first slot that needs it and not before. `None` when the compiler cannot identify itself — in which case nothing is read and nothing is written, because a verdict recorded under an identity nobody can reproduce would later be believed on behalf of a different compiler.
    ///
    /// Lazy because identifying the compiler writes its memo beside the store, and opening a store is not yet a decision to file anything in it: a compilation that is refused before its first unit is addressed — a package claiming a prefix the prelude mounts — would otherwise leave a `.curios/` holding that memo and nothing else.
    compiler: OnceCell<Option<String>>,
    /// The units placed so far, in fold order. Read afterwards by [`Verdicts::payload_put`], which files what the whole chain compiled to.
    pub(crate) placed: RefCell<Vec<Placed>>,
    /// Why the store could not be written, if it could not.
    ///
    /// The first refusal and not a count: a store nobody can write refuses every unit for one reason, so the reason is the whole of what a reader needs and repeating it per unit would say nothing new. Recorded rather than reported here because this crate has no terminal — see [`Verdicts::refused`].
    pub(crate) refused: RefCell<Option<String>>,
    /// What the session this handle belongs to has compiled — empty, and never consulted, for a handle nobody attached one to.
    session: Session,
    /// The read log of every unit this fold has taken so far, in fold order: restored by [`Verdicts::hit`] or kept by [`Verdicts::keep`], placed or not. What a kept unit's `earlier` is compared against — see [`Session`].
    taken: RefCell<Vec<ReadLog>>,
}

impl Verdicts {
    /// The store beside `root`.
    pub fn at(root: PathBuf) -> Self {
        let store = Store::at(root);

        Self {
            store,
            compiler: OnceCell::new(),
            placed: RefCell::new(Vec::new()),
            refused: RefCell::new(None),
            session: Session::default(),
            taken: RefCell::new(Vec::new()),
        }
    }

    /// Consult `session` for a baseline ahead of the store, and keep what is compiled in it — see [`Session`].
    pub fn reuse(&mut self, session: Session) {
        self.session = session;
    }

    /// The unit this session last compiled for `source` at its place in the chain, if it compiled one.
    ///
    /// Offered only at the slot it was compiled at — the one the store files under, taken after the same predecessors — so a kept unit answers exactly where a filed one would: the same mounts, by the same compiler, after the same chain; and only while the units before it read what they read when it was kept, which the slot alone cannot say. See [`Session`]. Not placed — the unit compiled over it is, through [`Verdicts::place`], as [`Verdicts::earlier`] leaves it.
    pub fn kept(&self, source: &UnitSource<'_>) -> Option<Unit> {
        let slot = self.slot(source, &self.placed.borrow())?;
        let units = self.session.units.borrow();
        let kept = units.get(&identity(source))?;

        (kept.slot == slot && kept.earlier == *self.taken.borrow()).then(|| kept.unit.clone())
    }

    /// Keep `unit` as what this session compiled for `source`, replacing whatever it compiled for that unit before — in this scope or any other.
    ///
    /// Called before `unit` is placed, so the slot is taken after the same predecessors [`Verdicts::kept`] will see the next time the fold reaches this source, and recorded beside the read log of every unit before it — to which this one's is then added. A clone rather than a move, because the fold goes on to hand the unit to its successors — and cheaper than what it stands beside, which serializes the same unit whole.
    pub fn keep(&self, source: &UnitSource<'_>, unit: &Unit) {
        if let Some(slot) = self.slot(source, &self.placed.borrow()) {
            self.session.units.borrow_mut().insert(
                identity(source),
                Kept {
                    slot,
                    earlier: self.taken.borrow().clone(),
                    unit: unit.clone(),
                },
            );
        }

        // Whether or not it could be kept: a unit after this one is checked against what this one read either way.
        self.taken.borrow_mut().push(digested(source.reads()));
    }

    /// The compiler's identity, memoized on first use — see the field.
    pub(crate) fn compiler(&self) -> Option<&String> {
        self.compiler.get_or_init(|| compiler(&self.store)).as_ref()
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
        let compiler = self.compiler()?;

        if source.directories().is_empty() {
            return None;
        }

        let predecessors = placed
            .iter()
            .map(|placed| placed.slot.clone())
            .collect::<Vec<_>>();

        Some(unit_slot(
            compiler,
            &predecessors,
            &source.claims(),
            source.declared(),
        ))
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
        // A hit's log is its record's, verified against the text just now, and spelled as the compile that filed it spelled it.
        self.taken.borrow_mut().push(record.reads);

        Some(restored)
    }

    /// The unit an earlier compilation of `source` filed under this address, whatever its files now hold: intact, filed after this chain, and read from files this source could itself have read. `None` is nothing filed, a slot that will not read back, a chain that moved, or another project's files.
    ///
    /// **A baseline is only as good as its chain.** An item that mentions a predecessor's name is outside the diff a recompile makes over the unit's own items, so a slot filed after a different chain would replay an item judged against a predecessor that has since changed; the chain clause is what keeps a baseline to the scope it was compiled in. The text clause is the one [`Cache::get`] makes and this does not: what the files hold now is exactly what the recompile diffs against.
    ///
    /// Not placed: the unit compiled over it is, through [`Verdicts::place`], as any unit the fold produces.
    pub fn earlier(&self, source: &UnitSource<'_>) -> Option<Unit> {
        let slot = self.slot(source, &self.placed.borrow())?;

        let filed = fs::read(self.store.verdict(&slot)).ok()?;
        let (recorded, bytes) = segments(&filed)?;
        let record = curios_archive::from_bytes::<Record>(recorded).ok()?;

        if record.unit != digest(bytes)
            || !chained(&record.predecessors, &self.placed.borrow())
            || !read_within(&source.directories(), &record.reads)
        {
            return None;
        }

        curios_archive::from_bytes::<Unit>(bytes).ok()
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
/// **A recorded file must also be one `source` could itself have read**, and that clause is what keeps a *shared* store from admitting across projects: `curios_unit::read_within` states why it is a containment check rather than a re-derivation of the read set.
fn agrees(
    source: &UnitSource<'_>,
    record: &Record,
    placed: &[Placed],
    bytes: &[u8],
    overlay: Option<&Overlay>,
) -> bool {
    record.unit == digest(bytes)
        && chained(&record.predecessors, placed)
        && read_within(&source.directories(), &record.reads)
        && unchanged(&record.reads, overlay)
}

/// What `source` read after `placed`, as the record of it, ahead of a unit whose bytes digest to `contained`.
fn recorded(source: &UnitSource<'_>, placed: &[Placed], contained: &str) -> Record {
    Record::of(
        source.reads(),
        placed
            .iter()
            .map(|placed| placed.contained.clone())
            .collect(),
        contained.to_string(),
    )
}

/// Whether `recorded` is what `placed` contains, position by position.
pub(crate) fn chained(recorded: &[String], placed: &[Placed]) -> bool {
    recorded.len() == placed.len()
        && recorded
            .iter()
            .zip(placed)
            .all(|(recorded, placed)| recorded == &placed.contained)
}

/// Whether every file in `reads` still holds the text it was recorded as — the overlay's text for a file `overlay` holds, since that is what the compilation would read, and the disk's for every other.
///
/// A file that has since vanished, changed, or become unreadable is a disagreement like any other.
pub(crate) fn unchanged(reads: &[(String, String)], overlay: Option<&Overlay>) -> bool {
    reads.iter().all(|(path, recorded)| {
        let path = Path::new(path);

        match overlay.and_then(|overlay| overlay.get(path)) {
            Some(text) => &digest(text.as_bytes()) == recorded,
            None => fs::read(path).is_ok_and(|bytes| &digest(&bytes) == recorded),
        }
    })
}
