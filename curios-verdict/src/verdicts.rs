//! The unit family: what the fold consults before compiling a unit again, and what it files when it did.
//!
//! **An object is addressed, and a hit is verified.** The address (`unit_address`) names a place — these mounts, declaring these dependencies — and holds no compiler, no file contents and no unit before it, so a project has as many objects as it has units rather than one per compile, per compiler or per scope. Who compiled the unit is named in the header of the file the object keeps it in ([`Filer`]), and what it was compiled *from* rides in a [`Record`] behind that header, checked when the object is opened: every file the compilation read, by the text it read, what each unit it could have read contained, and what the object itself holds — so a record vouches for the bytes it was written with and for no others.
//!
//! That split is deliberate. Hashing the unit's source directory into the address would hash a directory that, for a package's own library, *contains this cabinet*: filing a unit would change the address it is next looked for under, so a package's own code would never hit and `objects/` would grow a directory per compile. Excluding the cabinet from the walk is no answer: a key derived from a belief about what the inputs are goes wrong silently the day the belief does, and it goes wrong in the direction that hands back a stale unit. Here the inputs are not believed but recorded, at the one seam every module read passes through (`RootSource::reads`), so a compilation that reads something new records it without anything here being taught to expect it.
//!
//! **A hit is believed whole, so it rests on every unit its unit could have read.** That is the units it may name and, in turn, the ones those may name: a term names what its unit may name, or what a term handed to it names, which that term's unit may name; a witness is declared with its concept or with the type it is at, both of which whoever states the goal can name. A record names what each of those contained and no unit beside them, so an edit to a unit outside them leaves the hit standing, and two folds that reach one unit after different others file it once.
//!
//! **An object whose record no longer agrees is still a baseline.** The unit it holds is compiled over, each stage asking again what its own record says it read: of the unit's own text, of the units its record names where they contain what they did, nothing, and of any other unit in scope each part by its digest (`curios_elab::Foreign`, `curios_erase::AskedItems`, `curios_core::Abroad`). So a unit after an edited dependency redoes the declarations that read what the edit changed.
//!
//! **An object keeps two records, each read for the compiler that filed it.** A filing keeps the record it replaces beside its own, so the object holds its last two filings, whichever compilers made them, and the blobs both name. A hit, a baseline and the candidates a compile takes come from whichever of them the running compiler filed, the newer first; another compiler's is read for nothing, since its verdicts would be believed on behalf of a compiler that never reached them. Two compilers alternating on one project each hit their own record, and a third's filing pushes the older out.
//!
//! What a record holds and how it is framed ahead of the unit are `curios-unit`'s, since the prelude image is written the same way by a build script below every cabinet; what a record is *verified against* is here.

#[cfg(test)]
mod tests;

mod payload;
pub use payload::*;

use {
    crate::{
        Filer, Opened, Packs, entry_address, opened, opened_by, replace, set_aside, unit_address,
    },
    curios_common::{
        Blob, Candidates, Digest, Facet, Qualifier, Source, SourceName, candidate_prefix, digest,
    },
    curios_module::{Compiled, Overlay, PackageRoot, UnitSource},
    curios_package::{Cabinet, compiler},
    curios_pipeline::{After, Cache, Earlier, complete_records},
    curios_unit::{Indexed, Record, Unit, recorded},
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet},
        fs, io,
        path::{Path, PathBuf},
        rc::Rc,
        sync::Arc,
        time::SystemTime,
    },
};

/// The last unit one session had in hand for each unit it reached, compiled or restored — the baseline a question takes when the cabinet holds nothing nearer, and the unit a hit hands back in place of reading and decoding its object.
///
/// **Shared rather than owned by a cabinet handle, because it outlives every one of them.** A handle lives for one question; this lives for as long as whoever made it. That difference is the whole point: a language server compiles a unit, the next keystroke compiles over what it just produced, and the keystroke after that over *that*, so what an edit elaborates again is what read what *that* edit changed, rather than everything changed since the last build. Without it the baseline is the last unit the cabinet was written with, which nothing in an editing session ever advances — so one edit to a widely-read declaration leaves every later keystroke elaborating its readers again, for the rest of the session.
///
/// **What this asks of the trusted base is argued in [Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)**, which owns it: an item reused from a unit that was itself a recompile, by induction on the per-item argument, and what a kept unit says of the units it was compiled after. A session files nothing: a kept unit dies with the process, and whether the question that kept it also files it is that question's rule, asked of the cabinet ([`Verdicts::taken_on_disk`]).
///
/// **A kept unit is offered with the units it was compiled after.** A recompile cannot see by itself that a unit before it changed: a reference into an edited predecessor lowers to the same name either way. So each kept unit carries every unit the fold had taken before it, and is offered beside the ones the fold in hand has taken again — the same unit in hand, or one whose object holds the bytes it held. What the kept unit read of any other is asked again, part by part, by the stage that read it.
///
/// **The unit in hand, where there is one, because a session's units are mostly never stored.** A unit's stored bytes are consed and serialized whole, which a guard over them would pay on every keystroke for a unit nothing files; the handle a fold was given is in hand, and one unit is one handle for as long as nothing compiled it again. Where the handles differ the bytes are compared, which is what tells an object decoded twice from a unit that moved.
///
/// **One entry per unit, whatever scope it was compiled in.** Keyed by which unit it is — the prefixes it claims and the directories it reads — and not by its object, which also names what it declared: a manifest edit that adds or narrows a dependency gives the unit a new object, and a map keyed by object would strand the old entry beside the new one for as long as the session lives, each holding a whole unit. Keyed this way, an edit and a rescoping both *replace*, so the map is bounded by the units the editor has reached, as the parse memo is by the files. The object rides inside the entry instead, and a kept unit is offered only where it still matches. The directories are what keep two projects apart: a package of one name in each claims the same prefix and addresses the same object, which is sound to share — a baseline is only compiled over — but a poor baseline for either, and each would keep overwriting the other.
///
/// **A unit the cabinet restored, or one this session filed, is kept beside the digest of the bytes the object holds for it.** Kept, a restored unit is the baseline of the first edit to its unit, which would otherwise miss and be handed the same object read back a second time. And a later hit on that object, its record verified against the texts and the units it names as any hit's is, is answered with the unit in hand where the record names the bytes it was decoded from or filed as: the artifact is neither read nor decoded. An object another process has rewritten carries a record naming other bytes, and is read and decoded afresh. The unit in hand is believed on the evidence the object is, since its bytes are the ones the record vouches for, so nothing rests on the session that a hit does not rest on already.
///
/// A one-shot invocation makes none of these, and the cabinet is its only baseline.
#[derive(Clone, Default)]
pub struct Session {
    units: Rc<RefCell<BTreeMap<Identity, Kept>>>,
}

/// Which unit a source is, whatever scope it is compiled in: the prefixes it claims, the directories it reads from, and what it is compiled for, a source built and tested being two units.
type Identity = (Vec<String>, Vec<PathBuf>, Compiled);

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
        source.compiled(),
    )
}

/// Each file one unit read, by the file this process reads it from, with the digest of the text parsed from it: a record's reads resolved for the source asking, or a compile's own.
type ReadLog = Vec<(PathBuf, String)>;

/// The log of a source's own `reads`: each file it read, with the digest of the text parsed from it.
fn logged(reads: &[(PathBuf, Arc<Source>)]) -> ReadLog {
    reads
        .iter()
        .map(|(file, source)| (file.clone(), digest(source.text().as_bytes())))
        .collect()
}

/// What a record's `reads` log for a source reading for `packages`: each name resolved to the file one of the packages holds under it. `None` where a name is of no package the source reads for, which no file of this project answers.
fn resolved(packages: &[PackageRoot], reads: &[(SourceName, String)]) -> Option<ReadLog> {
    reads
        .iter()
        .map(|(name, digest)| {
            let file = packages.iter().find_map(|package| package.file_of(name))?;

            Some((file, digest.clone()))
        })
        .collect()
}

/// A unit a session has in hand for a source — compiled by a fold of its own, or restored from an object — beside the object it stands at and every unit the fold had taken before it: what a kept unit is offered with.
struct Kept {
    address: String,
    /// Each unit the fold had taken when this one was kept, by the prefixes it claims: the unit in hand then, and what its object's bytes digested to where it had a place.
    after: Vec<(String, Option<Unit>, Option<String>)>,
    unit: Unit,
    /// The digest of the bytes the object holds that this unit is: the ones it was decoded from, for one a hit restored, or the ones it was filed as, for one this session filed. What a later hit compares with the digest its record names before it hands this unit back in place of reading and decoding the object. `None` for a unit compiled and not filed, which is a baseline and never a hit.
    digest: Option<String>,
}

/// One unit a fold has taken, restored or compiled: which unit it is to the units after it, and what it contains where it has a place in the cabinet.
pub struct Taken {
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
    /// The digest of the bytes the unit's object holds, where it has a place: what a record of a unit that could have read it vouches for. `None` for a unit the cabinet may not place, and for one a question compiled and did not file.
    contained: Option<String>,
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

/// What each unit of `reach` contains, in the order of their keys: what a record vouches for. `None` where one of them has no place in the cabinet, since nothing can vouch for what it held.
fn contents(reach: &BTreeSet<String>, taken: &[Taken]) -> Option<Vec<(String, String)>> {
    reach
        .iter()
        .map(|key| {
            let unit = taken.iter().rev().find(|unit| unit.key == *key)?;

            Some((key.clone(), unit.contained.clone()?))
        })
        .collect()
}

/// The cabinet, as a compilation sees it — the fold's units through [`Cache`], and the invocation's own payload through [`Verdicts::payload_get`] and [`Verdicts::payload_put`].
///
/// One handle for both because they are one cabinet: the same compiler identity heads every file of both families, the units the fold takes *are* what a payload was made from, and a directory nobody can write refuses both for one reason, which [`Verdicts::refused`] reports once.
pub struct Verdicts {
    /// `None` for a handle with no cabinet behind it ([`Verdicts::kept_by`]): nothing is read and nothing is filed, and the handle is what its session keeps a unit through.
    cabinet: Option<Cabinet>,
    /// The identity of the compiler running now, asked for on the first address that needs it and not before. `None` when the compiler cannot identify itself — in which case nothing is read and nothing is written, because a verdict recorded under an identity nobody can reproduce would later be believed on behalf of a different compiler.
    ///
    /// Lazy because identifying the compiler writes its memo beside the cabinet, and opening a cabinet is not yet a decision to file anything in it: a compilation that is refused before its first unit is addressed — a package claiming a prefix the prelude mounts — would otherwise leave a `.curios/` holding that memo and nothing else.
    compiler: OnceCell<Option<String>>,
    /// Every unit this fold has taken so far, in fold order: restored by [`Verdicts::hit`], kept by [`Verdicts::keep`] or filed by [`Cache::put`]. Read afterwards by [`Verdicts::payload_put`], which files what the whole fold compiled to.
    taken: RefCell<Vec<Taken>>,
    /// Why the cabinet could not be written, if it could not.
    ///
    /// The first refusal and not a count: a cabinet nobody can write refuses every unit for one reason, so the reason is the whole of what a reader needs and repeating it per unit would say nothing new. Recorded rather than reported here because this crate has no terminal — see [`Verdicts::refused`].
    refused: RefCell<Option<String>>,
    /// What the session this handle belongs to has in hand — `None` for a handle nobody attached one to, a one-shot question's or a build's, which keeps nothing and so copies nothing.
    session: Option<Session>,
    /// The executable the entry this handle's fold ends in is, by its package's name and its own — `None` for a handle told of none, which files no entry's unit. See [`Verdicts::executing`].
    executable: Option<(String, String)>,
    /// The packs at each address this handle has read or filed blobs at, by the address: a pack is read once for as long as the handle lives, so a filing after a restore reads no pack the restore read.
    packs: RefCell<BTreeMap<String, Rc<Packs>>>,
    /// What this compiler's records at each address name, by the address, as [`Verdicts::named`] read them.
    named: RefCell<BTreeMap<String, Rc<BTreeSet<Digest>>>>,
}

impl Verdicts {
    /// A handle filing into `cabinet`.
    pub fn at(cabinet: Cabinet) -> Self {
        Self {
            cabinet: Some(cabinet),
            compiler: OnceCell::new(),
            taken: RefCell::new(Vec::new()),
            refused: RefCell::new(None),
            session: None,
            executable: None,
            packs: RefCell::new(BTreeMap::new()),
            named: RefCell::new(BTreeMap::new()),
        }
    }

    /// A handle with no cabinet behind it, keeping in `session` what its fold compiles: what a question about a file no package declares is compiled through. Such a file has no project and so nowhere to file a unit, and the session that asks about it again at the next keystroke still has the last unit it compiled of it to compile over.
    ///
    /// Nothing is addressed, since nothing is filed: a kept unit is told from another by which unit it is, the prefixes it claims and where it reads from ([`Session`]).
    pub fn kept_by(session: Session) -> Self {
        Self {
            cabinet: None,
            compiler: OnceCell::new(),
            taken: RefCell::new(Vec::new()),
            refused: RefCell::new(None),
            session: Some(session),
            executable: None,
            packs: RefCell::new(BTreeMap::new()),
            named: RefCell::new(BTreeMap::new()),
        }
    }

    /// This handle, told that the entry its fold ends in is `package`'s `executable`: the identity the unit of the entry's own declarations is filed under, as its payload is (`entry_address`). An entry claims the empty prefix whichever program it is, so a handle told of none files no entry's unit.
    pub fn executing(mut self, package: &str, executable: &str) -> Self {
        self.executable = Some((package.to_string(), executable.to_string()));
        self
    }

    /// Consult `session` for a baseline ahead of the cabinet, and keep in it what is compiled and what is restored — see [`Session`].
    pub fn reuse(&mut self, session: Session) {
        self.session = Some(session);
    }

    /// The unit this session last had in hand for `source` — compiled, or restored from its object — if it had one, with the units of this fold it was compiled after.
    ///
    /// Offered only at the object it stood at, the one the cabinet files under, so a kept unit answers where a filed one would: the same mounts, declaring the same dependencies, by the same compiler. See [`Session`].
    pub fn kept(&self, source: &UnitSource<'_>) -> Option<Earlier> {
        let session = self.session.as_ref()?;
        let address = self.address(source)?;
        let units = session.units.borrow();
        let kept = units.get(&identity(source))?;
        if kept.address != address {
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
            candidates: self.candidates(&address),
        })
    }

    /// Keep `unit` as what this session compiled for `source`, replacing whatever it had for that unit before — in this scope or any other — and take it into the fold.
    ///
    /// Called before `unit` is filed, so it is recorded beside every unit the fold took before it. A clone rather than a move, because the fold goes on to hand the unit to its successors — and cheaper than what it stands beside, which serializes the same unit whole.
    pub fn keep(&self, source: &UnitSource<'_>, unit: &Unit) {
        if let Some(address) = self.address(source) {
            self.hold(source, address, unit, None);
        }

        // Whether or not it could be kept: a unit after this one is checked against this one either way.
        self.take(source, logged(&source.reads()), Some(unit.clone()), None);
    }

    /// Hold `unit` as what the session has for `source` at `address`, after the units this fold has taken so far, replacing whatever it held for that unit before: `digest` names the bytes the object holds that it is, where it is one the object holds. Nothing where no session is attached.
    fn hold(&self, source: &UnitSource<'_>, address: String, unit: &Unit, digest: Option<String>) {
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
                    address,
                    after,
                    unit: unit.clone(),
                    digest,
                },
            );
        }
    }

    /// Say of the unit this session holds for `source` at `address`, where it is `unit`, that the object now holds it as the bytes `contained` names: a later hit on them is answered with it.
    fn filed_as(&self, source: &UnitSource<'_>, address: &str, unit: &Unit, contained: &str) {
        let Some(session) = &self.session else {
            return;
        };
        if let Some(kept) = session.units.borrow_mut().get_mut(&identity(source))
            && kept.address == address
            && kept.unit.same(unit)
        {
            kept.digest = Some(contained.to_string());
        }
    }

    /// Take the unit `source` is into the fold, after everything taken so far: what it read, the unit in hand where there is one, and what its object holds where it has a place.
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

    /// Say what the object of the unit `source` is holds, `contained`: of the entry the fold last took for it where it took one without a place ([`Verdicts::keep`]), and of a new one otherwise.
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
            logged(&source.reads()),
            Some(unit.clone()),
            Some(contained),
        );
    }

    /// The unit the session holds as the bytes the object at `address` holds, `contained` naming them, decoded from them or filed as them: what a hit hands back in place of reading and decoding the object.
    fn in_hand(&self, source: &UnitSource<'_>, address: &str, contained: &str) -> Option<Unit> {
        let units = self.session.as_ref()?.units.borrow();
        let kept = units.get(&identity(source))?;

        (kept.address == address && kept.digest.as_deref() == Some(contained))
            .then(|| kept.unit.clone())
    }

    /// Whether the disk holds every text the units this fold has taken read — restored, kept or compiled, each by its read log.
    ///
    /// What a question asks before it files. A unit's record names what it read and what each unit it could have read contained, and a later reader verifies both against the disk, so a unit compiled after one the disk does not answer for is a miss wherever it is filed, in the place of an object that may have held the disk's own. Asked of the disk rather than of an overlay because it is the test that later hit makes: it catches a document held edited, a hit taken on a held text the disk has moved from, and a file rewritten while its unit compiled.
    pub fn taken_on_disk(&self) -> bool {
        self.taken
            .borrow()
            .iter()
            .all(|taken| unchanged(&taken.reads, None))
    }

    /// The compiler's identity, memoized on first use — see the field.
    pub(crate) fn compiler(&self) -> Option<&String> {
        self.compiler
            .get_or_init(|| compiler(self.cabinet.as_ref()?))
            .as_ref()
    }

    /// Why nothing was filed, for a caller with somewhere to say it.
    ///
    /// **A cabinet that cannot be written is invisible without this.** Every compilation silently recompiles everything, forever, and the only symptom is that builds are slower than they should be — which reads as the compiler being slow rather than as a directory being unwritable. It is not a failure: the verdict is unaffected, so this is reported and the build is not stopped.
    pub fn refused(&self) -> Option<String> {
        self.refused.borrow().clone()
    }

    /// The address `source` is filed at, or `None` when it may not be filed at all.
    ///
    /// A source with nothing on disk has no reads to verify against, so a record of it would confirm nothing. That is the fixed prelude, which has an archive of its own. And an entry is filed as the executable this handle was told it is, so one it was told nothing of has no object. The address names no compiler, but a compiler that cannot identify itself reads and files nothing, so nothing is addressed for it.
    fn address(&self, source: &UnitSource<'_>) -> Option<String> {
        if source.directories().is_empty() {
            return None;
        }
        // A handle with no cabinet files nothing, so it addresses nothing: every unit its session keeps stands at the one address, and is told from another by which unit it is.
        if self.cabinet.is_none() {
            return Some(String::new());
        }
        self.compiler()?;
        if source.is_entry() {
            let (package, executable) = self.executable.as_ref()?;

            return Some(entry_address(
                package,
                executable,
                source.declared(),
                source.compiled(),
                source.stand_in(),
            ));
        }

        Some(unit_address(
            &source.claims(),
            source.declared(),
            source.compiled(),
            source.stand_in(),
        ))
    }

    /// Who this handle files as: the compiler running now. `None` where it cannot identify itself.
    pub(crate) fn filer(&self) -> Option<Filer> {
        Some(Filer::compiler(self.compiler()?))
    }

    /// The records of the object at `address` this compiler filed, the newer first, each opened as far as its record and read back: the object keeps the record of its last filing and of the one before, whichever compiler filed each, and another compiler's is read for nothing ([`Filer`]). None for a handle with no cabinet.
    fn records(&self, address: &str) -> Vec<(Opened, Record)> {
        let (Some(cabinet), Some(filer)) = (self.cabinet.as_ref(), self.filer()) else {
            return Vec::new();
        };

        [cabinet.unit(address), cabinet.replaced(address)]
            .iter()
            .filter_map(|file| {
                let opened = opened(file, &filer)?;
                // A record that will not read back is ignored, never a compile to fail: the source it was made from is still there, and recompiling costs time rather than correctness.
                let record = curios_archive::from_bytes::<Record>(opened.record()).ok()?;

                Some((opened, record))
            })
            .collect()
    }

    /// The packs at `address`, opened the first time this handle asks for them. `None` for a handle with no cabinet.
    fn packs(&self, address: &str) -> Option<Rc<Packs>> {
        let cabinet = self.cabinet.as_ref()?;
        let packs = self
            .packs
            .borrow_mut()
            .entry(address.to_string())
            .or_insert_with(|| Rc::new(Packs::at(cabinet.object(address), SystemTime::now())))
            .clone();

        Some(packs)
    }

    /// What the packs at `address` hold of declarations elaborated before that a compile of the object there may take one from (`curios_elab::Candidating`): those a record of this compiler's names ([`Offered`]). `None` for a handle with no cabinet.
    fn candidates(&self, address: &str) -> Option<Rc<dyn Candidates>> {
        let packs = self.packs(address)?;
        let named = self.named(address);

        Some(Rc::new(Offered { packs, named }))
    }

    /// Every blob a record of this compiler's at `address` names, read off each record's index once for as long as the handle lives, or until it files there. A record whose index is not the bytes it was written with names nothing.
    fn named(&self, address: &str) -> Rc<BTreeSet<Digest>> {
        if let Some(named) = self.named.borrow().get(address) {
            return Rc::clone(named);
        }
        let named = Rc::new(
            self.records(address)
                .into_iter()
                .filter_map(|(opened, record)| {
                    let index = opened.artifact()?;

                    intact(&record.unit, &index)
                        .then(|| Unit::named(&index).ok())
                        .flatten()
                })
                .flatten()
                .collect::<BTreeSet<_>>(),
        );
        self.named
            .borrow_mut()
            .insert(address.to_string(), Rc::clone(&named));

        named
    }

    /// The unit whose index the object at `address` holds as `index`, each blob it names found among its packs, restored for `source`: each source of its stored texts holds the file its name resolves to where `source`'s packages stand. `None` where one of the blobs is not found there, or the index holds no unit.
    fn restored(&self, address: &str, index: &[u8], source: &UnitSource<'_>) -> Option<Unit> {
        curios_profile::profile!("verdicts::restored");
        let packs = self.packs(address)?;

        Unit::restored(index, |digest| packs.find(digest))
            .ok()
            .map(|unit| unit.read_for(source.packages()))
    }

    /// File the blobs of `stored` the packs at `address` do not hold, in one pack: what an object naming them is renamed into place after.
    fn file_blobs(&self, address: &str, stored: &Indexed) -> io::Result<()> {
        curios_profile::profile!("verdicts::file_blobs");
        let packs = self
            .packs(address)
            .ok_or_else(|| io::Error::other("a cabinet files blobs"))?;
        let missing = packs
            .missing(stored.digests())
            .into_iter()
            .collect::<BTreeSet<_>>();
        let blobs = stored
            .blobs(|digest| missing.contains(digest))
            .map_err(io::Error::other)?;

        packs.write(&blobs)
    }

    /// Collect the packs at `address`, keeping `kept`, what its object's two records name ([`Packs::collect`]).
    fn collect(&self, address: &str, kept: &BTreeSet<Digest>) -> io::Result<()> {
        let packs = self
            .packs(address)
            .ok_or_else(|| io::Error::other("a cabinet collects blobs"))?;

        packs.collect(kept)
    }

    /// What each unit of `sources` is filed as, in order, verified against the cabinet without deserializing any of it — the probe [`Verdicts::payload_get`] decides a hit with.
    ///
    /// **This is the verification half of [`Cache::get`], and it is shared rather than restated.** Each source's object and record are decided by exactly the calls the fold makes, so the two cannot come to different answers about whether a unit is still good; all that is left out is the `Unit` decode, which a payload hit has no use for. A stale unit is `None` and so a payload miss by construction — it is about to recompile into bytes no record of the payload could match.
    ///
    /// A source the cabinet may not file at all is also `None`, which is stricter than the fold, where such a unit is simply compiled every time. The strictness is the point: a payload vouches for the *whole* compilation, and a unit nothing can verify is a part of it nothing can vouch for.
    pub(crate) fn chain(&self, sources: &[UnitSource<'_>]) -> Option<Vec<String>> {
        let mut taken: Vec<Taken> = Vec::new();

        for source in sources {
            let address = self.address(source)?;
            let reach = reach(source, &taken);
            // Either record of this compiler's that agrees, as a hit takes one.
            let (reads, contained) =
                self.records(&address)
                    .into_iter()
                    .find_map(|(opened, record)| {
                        let reads = agreeing(source, &record, &reach, &taken, None)?;

                        intact(&record.unit, &opened.artifact()?).then_some((reads, record.unit))
                    })?;

            taken.push(Taken {
                key: key(source),
                prefixes: source.claims().iter().map(|mount| mount.prefix).collect(),
                reach,
                reads,
                unit: None,
                contained: Some(contained),
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

    /// The stored unit for `source` when a record of this compiler's still agrees, the newer first, read through `overlay` where there is one, taken into the fold on a hit.
    ///
    /// **Either record of the object answers.** The newer is the one the last filing wrote, and the older the one it replaced, which an edit undone brings back and which another compiler's filing pushed aside: each is verified as a hit is, so whichever agrees is believed on the evidence the other would be.
    fn hit(&self, source: &UnitSource<'_>, overlay: Option<&Overlay>) -> Option<Unit> {
        curios_profile::profile!("verdicts::hit");
        let address = self.address(source)?;

        // A record is judged before its artifact is read: one that disagrees is not worth reading past.
        let (opened, record, reads) =
            self.records(&address)
                .into_iter()
                .find_map(|(opened, record)| {
                    let taken = self.taken.borrow();
                    let reads = agreeing(source, &record, &reach(source, &taken), &taken, overlay)?;

                    Some((opened, record, reads))
                })?;

        // The record agreed, so it names the bytes the object holds: a unit this session holds as those very bytes is the object's unit, and is handed back with the artifact unread.
        let contained = record.unit;
        let restored = match self.in_hand(source, &address, &contained) {
            Some(held) => held,
            None => {
                let index = opened.artifact()?;
                if !intact(&contained, &index) {
                    return None;
                }

                self.restored(&address, &index, source)?
            }
        };
        // Held as a compiled unit is, so the first edit to this unit is recompiled over the copy in hand rather than over the object read back again.
        self.hold(source, address, &restored, Some(contained.clone()));
        // A hit's log is its record's, resolved for this source and verified against the text just now.
        self.take(source, reads, Some(restored.clone()), Some(contained));

        Some(restored)
    }

    /// The unit an earlier compilation of `source` by this compiler filed under this address, whatever its files and the units before it now hold, with the units of this fold it was compiled after: intact, the newer of its records first. `None` is nothing this compiler filed, or an object that will not read back. An object another project filed for a package of the same name is a baseline as any is: what a recompile keeps of it, it keeps where this project's text is the text it was lowered from. Another compiler's record is no baseline, since a baseline's verdicts are believed too.
    ///
    /// **A baseline is offered whatever moved.** What the files hold now is what the recompile lowers and compares, and what the units before it hold now is what each stage asks its own reads of again: the record says which of them contain what they did, and every other is one the baseline was not compiled after.
    ///
    /// Not taken: the unit compiled over it is, as any unit the fold produces.
    pub fn earlier(&self, source: &UnitSource<'_>) -> Option<Earlier> {
        let address = self.address(source)?;

        let (record, unit) = self
            .records(&address)
            .into_iter()
            .find_map(|(opened, record)| {
                let index = opened.artifact()?;
                if !intact(&record.unit, &index) {
                    return None;
                }
                let unit = self.restored(&address, &index, source)?;

                Some((record, unit))
            })?;
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
            candidates: self.candidates(&address),
        })
    }

    /// The place `unit` takes after everything taken so far — its address, what each unit it could have read contains, and what it would be filed as with its index's digest — or `None` when it may not be placed at all.
    ///
    /// A unit this refuses has no place, and neither has a unit that could have read it: a record would vouch for a unit nothing here can produce again.
    fn placement(&self, source: &UnitSource<'_>, unit: &Unit) -> Option<Placement> {
        curios_profile::profile!("verdicts::placement");
        let cabinet = self.cabinet.as_ref()?;
        let address = self.address(source)?;
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
        if curios_core::validate_stored_identities(unit.core(), unit.mounts()).is_err() {
            return None;
        }

        // A unit that read a file for no package has nothing a record can name it by.
        let reads = recorded(&source.reads())?;
        let stored = unit.stored().ok()?;

        Some(Placement {
            path: cabinet.unit(&address),
            replaced: cabinet.replaced(&address),
            filer: self.filer()?,
            address,
            reads,
            after,
            contained: digest(stored.index()),
            stored,
        })
    }
}

/// Where a unit would be filed and as what: see [`Verdicts::placement`].
struct Placement {
    path: PathBuf,
    /// Where the record the filing replaces is kept beside it.
    replaced: PathBuf,
    filer: Filer,
    address: String,
    reads: Vec<(SourceName, String)>,
    after: Vec<(String, String)>,
    contained: String,
    stored: Indexed,
}

/// The candidates a compile at an address may take ([`Verdicts::candidates`]): the blobs its packs hold under a candidate's name that a record of the running compiler kept there names.
///
/// **What holds a candidate to the compiler that made it.** An address names no compiler, so its packs hold what every compiler filing there wrote, and a candidate's name says what it was made from and nothing of who made it. A record names its compiler, and a candidate's name ends in the digest of what it holds (`curios_common::candidate_name`), so a blob such a record names is the bytes that compiler filed, whoever wrote the pack it stands in. Offered otherwise, a candidate's verdict would be believed on behalf of a compiler that never reached it ([Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)).
struct Offered {
    packs: Rc<Packs>,
    named: Rc<BTreeSet<Digest>>,
}

impl Candidates for Offered {
    fn candidates(&self, facet: Facet, input: &Digest) -> Vec<Blob> {
        self.packs
            .prefixed(&candidate_prefix(facet, input))
            .into_iter()
            .filter(|blob| self.named.contains(&blob.digest))
            .collect()
    }

    /// A blob by its name, whoever's record names it: a name a candidate reads is the digest of what it holds, so what it finds is what was filed under it.
    fn blob(&self, digest: &Digest) -> Option<Blob> {
        self.packs.find(digest)
    }
}

/// Whether two units in hand are one: the same handle, which is what a fold hands on and a session keeps.
fn same(this: Option<&Unit>, that: Option<&Unit>) -> bool {
    matches!((this, that), (Some(this), Some(that)) if this.same(that))
}

impl Cache for Verdicts {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.hit(source, None)
    }

    /// The nearest earlier compilation of the unit there is: what a session attached to this cache last had in hand for it, then the object this tree filed for it however its files and the units before it have moved since, then the same two of the unit compiled for the other kind ([`UnitSource::other`]), then whatever the scope offered.
    ///
    /// Nearest first, because the order decides how much a compile over one redoes and nothing else — every one of these is a unit judged when it was made, so the answer is the same over any of them. The session's is the last keystroke's, which is nearer the text being asked about than anything a build filed; a filed unit is nearer than its other kind, which differs by every test the source declares; and both are nearer than the archived image, which is what is there when nothing else is, compiled after the units it stands among.
    ///
    /// **A build takes one as a question does.** A unit compiled over a baseline is stored as the bytes a whole compile of the same text stores, so what a build files is what it would have filed compiling whole, and it elaborates, judges and erases what its edit moved — a tested unit over its built one, its tests.
    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        let other = source.other();

        self.kept(source)
            .or_else(|| self.earlier(source))
            .or_else(|| self.kept(&other))
            .or_else(|| self.earlier(&other))
            .or_else(|| {
                offered.cloned().map(|unit| Earlier {
                    unit,
                    after: After::Scope,
                    candidates: self
                        .address(source)
                        .and_then(|address| self.candidates(&address)),
                })
            })
    }

    /// Filed, and taken into the fold with what its object holds: [`Verdicts::payload_put`] files a payload under what every unit of the fold was filed as.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        curios_profile::profile!("verdicts::put");
        // What the certifier records of each judgment is completed where the unit is filed, by the certifier, so a later walk at this address finds each verdict by the item it judged.
        if self.address(source).is_some() {
            complete_records(unit);
        }
        let Some(placement) = self.placement(source, unit) else {
            // A unit with no place is still one the fold took: every unit after it is checked against it.
            if self
                .taken
                .borrow()
                .last()
                .is_none_or(|last| last.key != key(source))
            {
                self.take(source, logged(&source.reads()), Some(unit.clone()), None);
            }
            return;
        };

        let record = Record::of(
            placement.reads,
            placement.after,
            source.answered().to_vec(),
            placement.contained.clone(),
        );
        // The blobs first, so an object never names one not yet written; then the record this filing replaces, kept beside it, so the compiler that filed it reads it still; then what neither record names is collected.
        let kept = kept(&placement.path, &placement.stored);
        let filed = self
            .file_blobs(&placement.address, &placement.stored)
            .and_then(|()| curios_archive::to_bytes(&record).map_err(io::Error::other))
            .and_then(|record| {
                set_aside(&placement.path, &placement.replaced)?;
                replace(
                    &placement.path,
                    &placement.filer,
                    &record,
                    placement.stored.index(),
                )
            })
            .and_then(|()| self.collect(&placement.address, &kept));
        // What this compiler's records name has moved, for whatever asks again in this handle's life.
        self.named.borrow_mut().remove(&placement.address);

        // Best effort: a cabinet that cannot be written costs the next compilation the work it would have saved, and nothing else. What it must never do is cost the verdict — so this unit takes its place below whether or not any of it landed, and the refusal is kept for a caller to report rather than raised here.
        match filed {
            // The session holds this very unit, now the bytes the object holds: a later hit on them is answered with it.
            Ok(()) => self.filed_as(source, &placement.address, unit, &placement.contained),
            Err(error) => {
                self.refused.borrow_mut().get_or_insert(error.to_string());
            }
        }

        self.placed(source, unit, placement.contained);
    }
}

/// What `record` logs for `source`, where it still describes the world: every unit `source` could have read — `reach`, of `taken` — contains what the record says it did, and every name it records is a file one of `source`'s packages holds, which still holds the text it was read as — through `overlay`, where the compilation reads through one. `None` where it does not.
///
/// **Judged before the artifact is read.** A record is small and an artifact is the unit, so a miss — text an editor holds edited, a unit before it that moved — is decided by the record alone. What the object's artifact digests to is asked after, of an object the record agrees about and only where its artifact is read ([`intact`]): a torn or damaged object is still refused before anything in it is believed.
///
/// A file that has since vanished, changed, or become unreadable is a disagreement like any other. So is a shorter or longer read list, which is what catches a module added or removed — though that alone never has to catch it, since a module can only join a unit through a `mod` in a header that is itself on this list. And so is a unit the source could have read that the record does not name, or names with other contents: one that joined its scope, left it, or changed.
///
/// **A recorded name resolves only through `source`'s own packages**, and that is what keeps a *shared* cabinet from admitting across projects: an object two projects address alike is believed only where this project's files hold the texts it was compiled from, after units holding what it was compiled after — the unit this project would compile (`curios_unit::recorded`).
///
/// **And every file `source` has read already must be one the record names, holding the text it holds now.** An entry's unit is filed by its executable and not by its root file, and the source logs that root as read the moment it is opened ([`RootSource::opened`](curios_module::RootSource)): a record that names other files, every one of which still stands, is the record of another file, as after a manifest points its executable elsewhere.
fn agreeing(
    source: &UnitSource<'_>,
    record: &Record,
    reach: &BTreeSet<String>,
    taken: &[Taken],
    overlay: Option<&Overlay>,
) -> Option<ReadLog> {
    let reads = resolved(&source.packages(), &record.reads)?;
    let agrees = record.answers == source.answered()
        && contents(reach, taken).is_some_and(|contents| contents == record.after)
        && recorded(&source.reads())
            .is_some_and(|logged| logged.iter().all(|read| record.reads.contains(read)))
        && unchanged(&reads, overlay);

    agrees.then_some(reads)
}

/// What a filing of the unit stored as `stored` keeps at the object whose `unit` file is at `path`: the blobs its own record names, and those the record it replaces names, whoever filed that one, so the compiler that did reads it still. A record whose index is not the bytes it was written with names nothing.
fn kept(path: &Path, stored: &Indexed) -> BTreeSet<Digest> {
    let replaced = opened_by(path)
        .and_then(|(_, opened)| {
            let record = curios_archive::from_bytes::<Record>(opened.record()).ok()?;
            let index = opened.artifact()?;

            intact(&record.unit, &index)
                .then(|| Unit::named(&index).ok())
                .flatten()
        })
        .unwrap_or_default();

    stored.digests().copied().chain(replaced).collect()
}

/// Whether `index` is the bytes a record naming `contained` was written with: what makes a damaged object that still deserializes a miss rather than a belief.
fn intact(contained: &str, index: &[u8]) -> bool {
    contained == digest(index)
}

/// Whether `recorded` is what `filed` holds, position by position.
pub fn chained(recorded: &[String], filed: &[String]) -> bool {
    recorded == filed
}

/// Whether every file in `reads` still holds the text it was recorded as — the overlay's text for a file `overlay` holds, since that is what the compilation would read, and the disk's for every other.
///
/// A file that has since vanished, changed, or become unreadable is a disagreement like any other.
pub fn unchanged(reads: &[(PathBuf, String)], overlay: Option<&Overlay>) -> bool {
    reads.iter().all(
        |(path, recorded)| match overlay.and_then(|overlay| overlay.get(path)) {
            Some(text) => &digest(text.as_bytes()) == recorded,
            None => fs::read(path).is_ok_and(|bytes| &digest(&bytes) == recorded),
        },
    )
}
