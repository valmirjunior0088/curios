//! The unit family: what the fold consults before compiling a unit again, and what it files when it did.
//!
//! **A slot is addressed, and a hit is verified.** The address (`unit_slot`) names a place — these mounts, declaring these dependencies, by this compiler — and holds no file contents and no unit before it, so a project has as many slots as it has units rather than one per compile or one per scope. What the unit was compiled *from* rides in a [`Record`] ahead of it in the slot's one file and is checked when the slot is opened: every file the compilation read, by the text it read, what each unit it could have read contained, and what the slot itself holds — so a record vouches for the bytes it was written with and for no others.
//!
//! That split is deliberate. Hashing the unit's source directory into the address would hash a directory that, for a package's own library, *contains this store*: filing a unit would change the address it is next looked for under, so a package's own code would never hit and `verdicts/` would grow a directory per compile. Excluding the store from the walk is no answer: a key derived from a belief about what the inputs are goes wrong silently the day the belief does, and it goes wrong in the direction that hands back a stale unit. Here the inputs are not believed but recorded, at the one seam every module read passes through (`RootSource::reads`), so a compilation that reads something new records it without anything here being taught to expect it.
//!
//! **A hit is believed whole, so it rests on every unit its unit could have read.** That is the units it may name and, in turn, the ones those may name: a term names what its unit may name, or what a term handed to it names, which that term's unit may name; a witness is declared with its concept or with the type it is at, both of which whoever states the goal can name. A record names what each of those contained and no unit beside them, so an edit to a unit outside them leaves the hit standing, and two folds that reach one unit after different others file it once.
//!
//! **A slot whose record no longer agrees is still a baseline.** The unit it holds is compiled over, each stage asking again what its own record says it read: of the unit's own text, of the units its record names where they contain what they did, nothing, and of any other unit in scope each part by its digest (`curios_elab::Foreign`, `curios_core::Abroad`). So a unit after an edited dependency redoes the declarations that read what the edit changed.
//!
//! What a record holds and how it is framed ahead of the unit are `curios-unit`'s, since the prelude image is written the same way by a build script below every store; what a record is *verified against* is here.

#[cfg(test)]
mod tests;

use {
    crate::replace,
    curios_package::{Store, compiler, entry_slot, unit_slot},
    curios_pipeline::{After, Cache, Earlier},
    curios_text::{Overlay, UnitSource},
    curios_unit::{Record, Unit, digested, read_within, segments},
    curios_utilities::{Qualifier, digest},
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet},
        fs, io,
        path::{Path, PathBuf},
        rc::Rc,
    },
};

/// The last unit one session had in hand for each unit it reached, compiled or restored — the baseline a question takes when the store holds nothing nearer, and the copy a hit hands back in place of decoding its slot again.
///
/// **Shared rather than owned by a store handle, because it outlives every one of them.** A handle lives for one question; this lives for as long as whoever made it. That difference is the whole point: a language server compiles a unit, the next keystroke compiles over what it just produced, and the keystroke after that over *that*, so what an edit elaborates again is what read what *that* edit changed, rather than everything changed since the last build. Without it the baseline is the last unit the store was written with, which nothing in an editing session ever advances — so one edit to a widely-read declaration leaves every later keystroke elaborating its readers again, for the rest of the session.
///
/// **What this asks of the trusted base is argued in [Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)**, which owns it: an item reused from a unit that was itself a recompile, by induction on the per-item argument, and what a kept unit says of the units it was compiled after. A session files nothing: a kept unit dies with the process, and whether the question that kept it also files it is that question's rule, asked of the store ([`Verdicts::taken_on_disk`]).
///
/// **A kept unit is offered with the units it was compiled after.** A recompile cannot see by itself that a unit before it changed: a reference into an edited predecessor lowers to the same name either way. So each kept unit carries every unit the fold had taken before it, and is offered beside the ones the fold in hand has taken again — the same unit in hand, or one whose slot holds the bytes it held. What the kept unit read of any other is asked again, part by part, by the stage that read it.
///
/// **The unit in hand, where there is one, because a session's units are mostly never stored.** A unit's stored bytes are consed and serialized whole, which a guard over them would pay on every keystroke for a unit nothing files; the handle a fold was given is in hand, and one unit is one handle for as long as nothing compiled it again. Where the handles differ the bytes are compared, which is what tells a slot decoded twice from a unit that moved.
///
/// **One entry per unit, whatever scope it was compiled in.** Keyed by which unit it is — the prefixes it claims and the directories it reads — and not by its slot, which also names what it declared: a manifest edit that adds or narrows a dependency gives the unit a new slot, and a map keyed by slot would strand the old entry beside the new one for as long as the session lives, each holding a whole unit. Keyed this way, an edit and a rescoping both *replace*, so the map is bounded by the units the editor has reached, as the parse memo is by the files. The slot rides inside the entry instead, and a kept unit is offered only where it still matches. The directories are what keep two projects apart: a package of one name in each claims the same prefix and addresses the same slot, which is sound to share — a baseline is only compiled over — but a poor baseline for either, and each would keep overwriting the other.
///
/// **A unit the store restored is kept as one a fold compiled is, beside the digest of the bytes it was decoded from.** Kept, it is the baseline of the first edit to its unit, which would otherwise miss and be handed the same slot read back a second time. And a later hit on that slot, its record verified against the texts and the units it names as any hit's is, is answered with the copy where the slot's bytes still digest to what the copy was decoded from: the file is read and judged as before, and what is spared is the decode. A slot another process has rewritten digests to something else and is decoded afresh. The copy is believed on the evidence the slot is, so nothing rests on the session that a hit does not rest on already.
///
/// A one-shot invocation makes none of these, and the store is its only baseline.
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

/// A unit a session has in hand for a source — compiled by a fold of its own, or restored from a slot — beside the slot it stands at and every unit the fold had taken before it: what a kept unit is offered with.
struct Kept {
    slot: String,
    /// Each unit the fold had taken when this one was kept, by the prefixes it claims: the unit in hand then, and what its slot's bytes digested to where it had a place.
    after: Vec<(String, Option<Unit>, Option<String>)>,
    unit: Unit,
    /// The digest of the slot's bytes this unit was decoded from, for one a hit restored: what a later hit compares with the slot's own before it hands this copy back in place of decoding another. `None` for a unit a fold compiled, which is a baseline and never a hit.
    restored: Option<String>,
}

/// One unit a fold has taken, restored or compiled: which unit it is to the units after it, and what it contains where it has a place in the store.
pub(crate) struct Taken {
    /// The prefixes the unit claims, as one key: which unit it is to a record that names it.
    key: String,
    /// The prefixes the unit claims: which unit it is to a unit that declares a dependency, and to a baseline offered after it ([`After`]).
    prefixes: Vec<Qualifier>,
    /// The keys of the units it could have read: the ones it may name, and the ones those may name.
    reach: BTreeSet<String>,
    /// Each file it read. What a question asks the disk about before it files ([`Verdicts::taken_on_disk`]).
    reads: ReadLog,
    /// The unit as the fold holds it. `None` where a chain is verified and nothing decoded ([`Verdicts::chain`]).
    unit: Option<Unit>,
    /// The digest of the bytes the unit's slot holds, where it has a place: what a record of a unit that could have read it vouches for. `None` for a unit the store may not place, and for one a question compiled and did not file.
    pub(crate) contained: Option<String>,
    /// Whether it is the unit of an entry's own declarations: the last a fold takes, which no unit reads and no payload counts among the units it was made after.
    entry: bool,
}

/// The key a record names the unit `source` is by: the prefixes it claims, in the order it claims them.
fn key(source: &UnitSource<'_>) -> String {
    source
        .claims()
        .iter()
        .map(|mount| mount.prefix.join())
        .collect::<Vec<_>>()
        .join(" ")
}

/// The keys of the units of `taken` that `source` could have read: each one it may name — every one, where it declared no dependency, and otherwise the ones claiming a prefix it declared — and the ones that one could have read in turn.
fn reach(source: &UnitSource<'_>, taken: &[Taken]) -> BTreeSet<String> {
    let declared = source.declared();
    let mut reach = BTreeSet::new();
    for unit in taken {
        let named = declared
            .is_none_or(|declared| unit.prefixes.iter().any(|prefix| declared.contains(prefix)));
        if named {
            reach.insert(unit.key.clone());
            reach.extend(unit.reach.iter().cloned());
        }
    }

    reach
}

/// What each unit of `reach` contains, in the order of their keys: what a record vouches for. `None` where one of them has no place in the store, since nothing can vouch for what it held.
fn contents(reach: &BTreeSet<String>, taken: &[Taken]) -> Option<Vec<(String, String)>> {
    reach
        .iter()
        .map(|key| {
            let unit = taken.iter().rev().find(|unit| unit.key == *key)?;

            Some((key.clone(), unit.contained.clone()?))
        })
        .collect()
}

/// The store, as a compilation sees it — the fold's units through [`Cache`], and the invocation's own payload through [`Verdicts::payload_get`] and [`Verdicts::payload_put`].
///
/// One handle for both because they are one store: the same compiler identity decides both addresses, the units the fold takes *are* what a payload was made from, and a directory nobody can write refuses both for one reason, which [`Verdicts::refused`] reports once.
pub struct Verdicts {
    /// `None` for a handle with no store behind it ([`Verdicts::kept_by`]): nothing is read and nothing is filed, and the handle is what its session keeps a unit through.
    pub(crate) store: Option<Store>,
    /// The identity of the compiler running now, asked for on the first slot that needs it and not before. `None` when the compiler cannot identify itself — in which case nothing is read and nothing is written, because a verdict recorded under an identity nobody can reproduce would later be believed on behalf of a different compiler.
    ///
    /// Lazy because identifying the compiler writes its memo beside the store, and opening a store is not yet a decision to file anything in it: a compilation that is refused before its first unit is addressed — a package claiming a prefix the prelude mounts — would otherwise leave a `.curios/` holding that memo and nothing else.
    compiler: OnceCell<Option<String>>,
    /// Every unit this fold has taken so far, in fold order: restored by [`Verdicts::hit`], kept by [`Verdicts::keep`] or filed by [`Cache::put`]. Read afterwards by [`Verdicts::payload_put`], which files what the whole fold compiled to.
    pub(crate) taken: RefCell<Vec<Taken>>,
    /// Why the store could not be written, if it could not.
    ///
    /// The first refusal and not a count: a store nobody can write refuses every unit for one reason, so the reason is the whole of what a reader needs and repeating it per unit would say nothing new. Recorded rather than reported here because this crate has no terminal — see [`Verdicts::refused`].
    pub(crate) refused: RefCell<Option<String>>,
    /// What the session this handle belongs to has in hand — `None` for a handle nobody attached one to, a one-shot question's or a build's, which keeps nothing and so copies nothing.
    session: Option<Session>,
    /// The executable the entry this handle's fold ends in is, by its package's name and its own — `None` for a handle told of none, which files no entry's unit. See [`Verdicts::executing`].
    executable: Option<(String, String)>,
}

impl Verdicts {
    /// The store beside `root`.
    pub fn at(root: PathBuf) -> Self {
        let store = Store::at(root);

        Self {
            store: Some(store),
            compiler: OnceCell::new(),
            taken: RefCell::new(Vec::new()),
            refused: RefCell::new(None),
            session: None,
            executable: None,
        }
    }

    /// A handle with no store behind it, keeping in `session` what its fold compiles: what a question about a file no package declares is compiled through. Such a file has no project and so nowhere to file a unit, and the session that asks about it again at the next keystroke still has the last unit it compiled of it to compile over.
    ///
    /// Nothing is addressed, since nothing is filed: a kept unit is told from another by which unit it is, the prefixes it claims and where it reads from ([`Session`]).
    pub fn kept_by(session: Session) -> Self {
        Self {
            store: None,
            compiler: OnceCell::new(),
            taken: RefCell::new(Vec::new()),
            refused: RefCell::new(None),
            session: Some(session),
            executable: None,
        }
    }

    /// This handle, told that the entry its fold ends in is `package`'s `executable`: the identity the unit of the entry's own declarations is filed under, as its payload is (`curios_package::entry_slot`). An entry claims the empty prefix whichever program it is, so a handle told of none files no entry's unit.
    pub fn executing(mut self, package: &str, executable: &str) -> Self {
        self.executable = Some((package.to_string(), executable.to_string()));
        self
    }

    /// Consult `session` for a baseline ahead of the store, and keep in it what is compiled and what is restored — see [`Session`].
    pub fn reuse(&mut self, session: Session) {
        self.session = Some(session);
    }

    /// The unit this session last had in hand for `source` — compiled, or restored from its slot — if it had one, with the units of this fold it was compiled after.
    ///
    /// Offered only at the slot it stood at, the one the store files under, so a kept unit answers where a filed one would: the same mounts, declaring the same dependencies, by the same compiler. See [`Session`].
    pub fn kept(&self, source: &UnitSource<'_>) -> Option<Earlier> {
        let session = self.session.as_ref()?;
        let slot = self.slot(source)?;
        let units = session.units.borrow();
        let kept = units.get(&identity(source))?;
        if kept.slot != slot {
            return None;
        }

        let after = self
            .taken
            .borrow()
            .iter()
            .filter(|taken| {
                kept.after.iter().any(|(key, unit, contained)| {
                    *key == taken.key
                        && (same(unit.as_ref(), taken.unit.as_ref())
                            || (contained.is_some() && *contained == taken.contained))
                })
            })
            .map(|taken| taken.prefixes.clone())
            .collect();

        Some(Earlier {
            unit: kept.unit.clone(),
            after: After::Units(after),
        })
    }

    /// Keep `unit` as what this session compiled for `source`, replacing whatever it had for that unit before — in this scope or any other — and take it into the fold.
    ///
    /// Called before `unit` is filed, so it is recorded beside every unit the fold took before it. A clone rather than a move, because the fold goes on to hand the unit to its successors — and cheaper than what it stands beside, which serializes the same unit whole.
    pub fn keep(&self, source: &UnitSource<'_>, unit: &Unit) {
        if let Some(slot) = self.slot(source) {
            self.hold(source, slot, unit, None);
        }

        // Whether or not it could be kept: a unit after this one is checked against this one either way.
        self.take(source, digested(source.reads()), Some(unit.clone()), None);
    }

    /// Hold `unit` as what the session has for `source` at `slot`, after the units this fold has taken so far, replacing whatever it held for that unit before. Nothing where no session is attached.
    fn hold(&self, source: &UnitSource<'_>, slot: String, unit: &Unit, restored: Option<String>) {
        if let Some(session) = &self.session {
            let after = self
                .taken
                .borrow()
                .iter()
                .map(|taken| {
                    (
                        taken.key.clone(),
                        taken.unit.clone(),
                        taken.contained.clone(),
                    )
                })
                .collect();

            session.units.borrow_mut().insert(
                identity(source),
                Kept {
                    slot,
                    after,
                    unit: unit.clone(),
                    restored,
                },
            );
        }
    }

    /// Take the unit `source` is into the fold, after everything taken so far: what it read, the unit in hand where there is one, and what its slot holds where it has a place.
    fn take(
        &self,
        source: &UnitSource<'_>,
        reads: ReadLog,
        unit: Option<Unit>,
        contained: Option<String>,
    ) {
        let mut taken = self.taken.borrow_mut();
        let reach = reach(source, &taken);

        taken.push(Taken {
            key: key(source),
            prefixes: source.claims().iter().map(|mount| mount.prefix).collect(),
            reach,
            reads,
            unit,
            contained,
            entry: source.is_entry(),
        });
    }

    /// Say what the slot of the unit `source` is holds, `contained`: of the entry the fold last took for it where it took one without a place ([`Verdicts::keep`]), and of a new one otherwise.
    fn placed(&self, source: &UnitSource<'_>, unit: &Unit, contained: String) {
        let key = key(source);
        {
            let mut taken = self.taken.borrow_mut();
            if let Some(last) = taken
                .last_mut()
                .filter(|last| last.key == key && last.contained.is_none())
            {
                last.contained = Some(contained);
                return;
            }
        }

        self.take(
            source,
            digested(source.reads()),
            Some(unit.clone()),
            Some(contained),
        );
    }

    /// The unit the session decoded from the bytes `slot` holds, when it has: the copy a hit hands back in place of decoding the slot again.
    fn decoded(&self, source: &UnitSource<'_>, slot: &str, contained: &str) -> Option<Unit> {
        let units = self.session.as_ref()?.units.borrow();
        let kept = units.get(&identity(source))?;

        (kept.slot == slot && kept.restored.as_deref() == Some(contained))
            .then(|| kept.unit.clone())
    }

    /// Whether the disk holds every text the units this fold has taken read — restored, kept or compiled, each by its read log.
    ///
    /// What a question asks before it files. A unit's record names what it read and what each unit it could have read contained, and a later reader verifies both against the disk, so a unit compiled after one the disk does not answer for is a miss wherever it is filed, in the place of a slot that may have held the disk's own. Asked of the disk rather than of an overlay because it is the test that later hit makes: it catches a document held edited, a hit taken on a held text the disk has moved from, and a file rewritten while its unit compiled.
    pub fn taken_on_disk(&self) -> bool {
        self.taken
            .borrow()
            .iter()
            .all(|taken| unchanged(&taken.reads, None))
    }

    /// The compiler's identity, memoized on first use — see the field.
    pub(crate) fn compiler(&self) -> Option<&String> {
        self.compiler
            .get_or_init(|| compiler(self.store.as_ref()?))
            .as_ref()
    }

    /// Why nothing was filed, for a caller with somewhere to say it.
    ///
    /// **A store that cannot be written is invisible without this.** Every compilation silently recompiles everything, forever, and the only symptom is that builds are slower than they should be — which reads as the compiler being slow rather than as a directory being unwritable. It is not a failure: the verdict is unaffected, so this is reported and the build is not stopped.
    pub fn refused(&self) -> Option<String> {
        self.refused.borrow().clone()
    }

    /// The slot `source` is filed in, or `None` when it may not be filed at all.
    ///
    /// A source with nothing on disk has no reads to verify against, so a record of it would confirm nothing. That is the fixed prelude, which has an archive of its own. And an entry is filed as the executable this handle was told it is, so one it was told nothing of has no slot.
    fn slot(&self, source: &UnitSource<'_>) -> Option<String> {
        if source.directories().is_empty() {
            return None;
        }
        // A handle with no store files nothing, so it addresses nothing: every unit its session keeps stands at the one slot, and is told from another by which unit it is.
        if self.store.is_none() {
            return Some(String::new());
        }
        let compiler = self.compiler()?;
        if source.is_entry() {
            let (package, executable) = self.executable.as_ref()?;

            return Some(entry_slot(compiler, package, executable, source.declared()));
        }

        Some(unit_slot(compiler, &source.claims(), source.declared()))
    }

    /// What each unit of `sources` is filed as, in order, verified against the store without deserializing any of it — the probe [`Verdicts::payload_get`] decides a hit with.
    ///
    /// **This is the verification half of [`Cache::get`], and it is shared rather than restated.** Each source's slot and record are decided by exactly the calls the fold makes, so the two cannot come to different answers about whether a unit is still good; all that is left out is the `Unit` decode, which a payload hit has no use for. A stale unit is `None` and so a payload miss by construction — it is about to recompile into bytes no record of the payload could match.
    ///
    /// A source the store may not file at all is also `None`, which is stricter than the fold, where such a unit is simply compiled every time. The strictness is the point: a payload vouches for the *whole* compilation, and a unit nothing can verify is a part of it nothing can vouch for.
    pub(crate) fn chain(&self, sources: &[UnitSource<'_>]) -> Option<Vec<String>> {
        let mut taken: Vec<Taken> = Vec::new();

        for source in sources {
            let slot = self.slot(source)?;

            let filed = fs::read(self.store.as_ref()?.verdict(&slot)).ok()?;
            let (recorded, bytes) = segments(&filed)?;
            let record = curios_archive::from_bytes::<Record>(recorded).ok()?;

            let reach = reach(source, &taken);
            if !agrees(source, &record, &reach, &taken, bytes, None) {
                return None;
            }

            taken.push(Taken {
                key: key(source),
                prefixes: source.claims().iter().map(|mount| mount.prefix).collect(),
                reach,
                reads: record.reads,
                unit: None,
                contained: Some(record.unit),
                entry: false,
            });
        }

        taken.into_iter().map(|taken| taken.contained).collect()
    }

    /// What every unit this fold has taken before its entry is filed as, in fold order: `None` where one of them has no place. The unit of the entry's own declarations is not among them: a payload vouches for its entry by the text it was compiled from.
    pub(crate) fn filed(&self) -> Option<Vec<String>> {
        self.taken
            .borrow()
            .iter()
            .filter(|taken| !taken.entry)
            .map(|taken| taken.contained.clone())
            .collect()
    }

    /// [`Cache::get`] for a compilation reading through `overlay`: the record is verified against the text the compilation would read, which for a file the overlay holds is the overlay's and for every other the disk's.
    ///
    /// **Exact rather than a containment guess.** The record lists every file the unit was compiled from, so an open file the unit never read — an executable beside a package's library — leaves the hit standing, an open file it did read hits while its text is unchanged and misses the moment it is edited, and nothing about what a unit reads has to be believed twice. The `wonder` engine, which answers a question from an editor's unsaved buffers, is the consumer.
    pub fn get_overlaid(&self, source: &UnitSource<'_>, overlay: &Overlay) -> Option<Unit> {
        self.hit(source, Some(overlay))
    }

    /// The stored unit for `source` when its record still agrees, read through `overlay` where there is one, taken into the fold on a hit.
    fn hit(&self, source: &UnitSource<'_>, overlay: Option<&Overlay>) -> Option<Unit> {
        curios_profile::profile!("verdicts::hit");
        let slot = self.slot(source)?;

        // The record is judged before the unit is decoded: a slot whose record disagrees is not worth the decode, and a file that is not a slot is a store to ignore.
        let filed = fs::read(self.store.as_ref()?.verdict(&slot)).ok()?;
        let (recorded, bytes) = segments(&filed)?;

        // A stored unit that will not read back is a store to ignore, never a compile to fail: the source it was made from is still there, and recompiling costs time rather than correctness.
        let record = curios_archive::from_bytes::<Record>(recorded).ok()?;

        {
            let taken = self.taken.borrow();
            if !agrees(
                source,
                &record,
                &reach(source, &taken),
                &taken,
                bytes,
                overlay,
            ) {
                return None;
            }
        }

        // The record agreed, so it names the bytes read: a copy this session decoded from those very bytes is the slot's unit, and is handed back without decoding it a second time.
        let contained = record.unit;
        let restored = match self.decoded(source, &slot, &contained) {
            Some(decoded) => decoded,
            None => Unit::restored(bytes).ok()?,
        };
        // Held as a compiled unit is, so the first edit to this unit is recompiled over the copy in hand rather than over the slot read back again.
        self.hold(source, slot, &restored, Some(contained.clone()));
        // A hit's log is its record's, verified against the text just now, and spelled as the compile that filed it spelled it.
        self.take(
            source,
            record.reads,
            Some(restored.clone()),
            Some(contained),
        );

        Some(restored)
    }

    /// The unit an earlier compilation of `source` filed under this address, whatever its files and the units before it now hold, with the units of this fold it was compiled after: intact, and read from files this source could itself have read. `None` is nothing filed, a slot that will not read back, or another project's files.
    ///
    /// **A baseline is offered whatever moved.** What the files hold now is what the recompile lowers and compares, and what the units before it hold now is what each stage asks its own reads of again: the record says which of them contain what they did, and every other is one the baseline was not compiled after.
    ///
    /// Not taken: the unit compiled over it is, as any unit the fold produces.
    pub fn earlier(&self, source: &UnitSource<'_>) -> Option<Earlier> {
        let slot = self.slot(source)?;

        let filed = fs::read(self.store.as_ref()?.verdict(&slot)).ok()?;
        let (recorded, bytes) = segments(&filed)?;
        let record = curios_archive::from_bytes::<Record>(recorded).ok()?;

        if record.unit != digest(bytes) || !read_within(&source.directories(), &record.reads) {
            return None;
        }

        let unit = Unit::restored(bytes).ok()?;
        let after = self
            .taken
            .borrow()
            .iter()
            .filter(|taken| {
                record.after.iter().any(|(key, contained)| {
                    *key == taken.key && Some(contained) == taken.contained.as_ref()
                })
            })
            .map(|taken| taken.prefixes.clone())
            .collect();

        Some(Earlier {
            unit,
            after: After::Units(after),
        })
    }

    /// The place `unit` takes after everything taken so far — its slot, what each unit it could have read contains, and the bytes it would be filed as with their digest — or `None` when it may not be placed at all.
    ///
    /// A unit this refuses has no place, and neither has a unit that could have read it: a record would vouch for a unit nothing here can produce again.
    fn placement(&self, source: &UnitSource<'_>, unit: &Unit) -> Option<Placement> {
        curios_profile::profile!("verdicts::placement");
        let store = self.store.as_ref()?;
        let slot = self.slot(source)?;
        let after = {
            let taken = self.taken.borrow();
            // The fold may have taken this unit already, kept and not yet placed: it is no unit before itself.
            let before = match taken.last() {
                Some(last) if last.key == key(source) && last.contained.is_none() => {
                    &taken[..taken.len() - 1]
                }
                _ => &taken[..],
            };

            contents(&reach(source, before), before)?
        };

        // The rule a stored unit is checked against, at the second seam a unit is written — the first being the prelude's build script. An identity meaningful only in the compilation that made it has no safe direction to degrade in: restored beside a unit whose own counters hand out the same index, it aliases silently rather than failing, which admits. Storing nothing is always safe, so a unit that would carry one is dropped rather than refused: the compilation it came from is correct, and only the record is withheld.
        if curios_core::validate_stored_identities(unit.core()).is_err() {
            return None;
        }

        let bytes = unit.stored().ok()?;

        Some(Placement {
            path: store.verdict(&slot),
            after,
            contained: digest(&bytes),
            bytes,
        })
    }
}

/// Where a unit would be filed and as what: see [`Verdicts::placement`].
struct Placement {
    path: PathBuf,
    after: Vec<(String, String)>,
    contained: String,
    bytes: Vec<u8>,
}

/// Whether two units in hand are one: the same handle, which is what a fold hands on and a session keeps.
fn same(this: Option<&Unit>, that: Option<&Unit>) -> bool {
    matches!((this, that), (Some(this), Some(that)) if this.same(that))
}

impl Cache for Verdicts {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.hit(source, None)
    }

    /// The nearest earlier compilation of the unit there is: what a session attached to this cache last had in hand for it, then the slot this tree filed for it however its files and the units before it have moved since, then whatever the scope offered.
    ///
    /// Nearest first, because the order decides how much a compile over one redoes and nothing else — every one of the three is a unit judged when it was made, so the answer is the same over any of them. The session's is the last keystroke's, which is nearer the text being asked about than anything a build filed; a filed unit is nearer than the archived image; and the image is what is there when nothing else is, compiled after the units it stands among.
    ///
    /// **A build takes one as a question does.** A unit compiled over a baseline is stored as the bytes a whole compile of the same text stores, so what a build files is what it would have filed compiling whole, and it elaborates, judges and erases what its edit moved.
    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        self.kept(source)
            .or_else(|| self.earlier(source))
            .or_else(|| {
                offered.cloned().map(|unit| Earlier {
                    unit,
                    after: After::Scope,
                })
            })
    }

    /// Filed, and taken into the fold with what its slot holds: [`Verdicts::payload_put`] files a payload under what every unit of the fold was filed as.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        curios_profile::profile!("verdicts::put");
        let Some(placement) = self.placement(source, unit) else {
            // A unit with no place is still one the fold took: every unit after it is checked against it.
            if self
                .taken
                .borrow()
                .last()
                .is_none_or(|last| last.key != key(source))
            {
                self.take(source, digested(source.reads()), Some(unit.clone()), None);
            }
            return;
        };

        let record = Record::of(source.reads(), placement.after, placement.contained.clone());
        let filed = curios_archive::to_bytes(&record)
            .map_err(io::Error::other)
            .and_then(|record| replace(&placement.path, &record, &placement.bytes));

        // Best effort: a store that cannot be written costs the next compilation the work it would have saved, and nothing else. What it must never do is cost the verdict — so this unit takes its place below whether or not any of it landed, and the refusal is kept for a caller to report rather than raised here.
        if let Err(error) = filed {
            self.refused.borrow_mut().get_or_insert(error.to_string());
        }

        self.placed(source, unit, placement.contained);
    }
}

/// Whether `record` still describes the world: the slot holds the bytes it was written with, every unit `source` could have read — `reach`, of `taken` — contains what the record says it did, every file it names still holds the text it was read as — through `overlay`, where the compilation reads through one — and each is a file `source` could itself have read.
///
/// The slot's own digest comes first, as the payload family orders its check: it decides a torn or damaged slot before any file is opened.
///
/// A file that has since vanished, changed, or become unreadable is a disagreement like any other. So is a shorter or longer read list, which is what catches a module added or removed — though that alone never has to catch it, since a module can only join a unit through a `mod` in a header that is itself on this list. And so is a unit the source could have read that the record does not name, or names with other contents: one that joined its scope, left it, or changed.
///
/// **A recorded file must also be one `source` could itself have read**, and that clause is what keeps a *shared* store from admitting across projects: `curios_unit::read_within` states why it is a containment check rather than a re-derivation of the read set.
fn agrees(
    source: &UnitSource<'_>,
    record: &Record,
    reach: &BTreeSet<String>,
    taken: &[Taken],
    bytes: &[u8],
    overlay: Option<&Overlay>,
) -> bool {
    record.unit == digest(bytes)
        && contents(reach, taken).is_some_and(|contents| contents == record.after)
        && read_within(&source.directories(), &record.reads)
        && unchanged(&record.reads, overlay)
}

/// Whether `recorded` is what `filed` holds, position by position.
pub(crate) fn chained(recorded: &[String], filed: &[String]) -> bool {
    recorded == filed
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
