//! What one source declaration lowered to beside its items, and what it asked of its scope to get there: the record a later lowering of the same sources keeps the declaration's lowering by.
//!
//! **A lowering is a function of its declaration's text and the names it resolved.** Every counter a lowering mints from starts at zero for each declaration, and a declaration reads its scope through two doors alone: `Context::resolve_term_name` for a name written as a path, `Context::resolve_bare` for a bare one no binder of its own binds. So a declaration whose text is the one a baseline lowered, and whose every resolution answers as it did, lowers to what it lowered to, and is not lowered: its items, its registry entries, what it minted, its import scopes and its lints are taken from the baseline ([`Baseline::kept`]).
//!
//! **Asked only where an edit could have reached** ([`Moved`]): a declaration of a file the edit did not touch, none of whose lookups read a binding that moved, is kept with nothing compared or asked, and what asking its lookups would have marked is marked from the bindings its record says they read ([`Consulted`]).
//!
//! **Decided by asking again, through the same doors.** A resolution marks the `use` it came through as used and the mount it was spelled under as reached, so a trace asked again leaves the unit's tables as lowering the declaration would have. An answer that differs, or a resolution the scope now refuses, lowers the declaration from its text, where the refusal is raised as it always was.
//!
//! **Found by its text, confirmed by its text.** A lowered declaration is held under its anchor and the stamp of the text it was lowered from, which is how a position finds its declaration too; a stamp is thirty-two bits, so the text itself is compared before a lowering is believed.

use {
    super::{
        Context, FlatItem, FlatLet, Journal, LoweredItem, Lowering, Resolution, Scan, Step, Table,
    },
    crate::{Lint, LintKind, LintedBinder, PreparedText},
    curios_utilities::{Digest, Filed, Object, Ordered, Persistent, Qualifier, Report, Sorted},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashSet},
        rc::Rc,
    },
};

/// A lowered declaration as a later lowering finds it: by its anchor and the stamp of its text.
pub(super) type Key = (curios_core::Anchor, u32);

/// A declaration's record as a unit holds it: shared with every version that keeps it, beside the digest it is stored under.
pub(crate) type HeldRecord = Rc<Filed<LoweredDeclaration>>;

/// A file's scan as a unit holds it: shared with every version that keeps it, beside the digest it is stored under.
pub(crate) type HeldScan = Rc<Filed<Scan>>;

/// Each file-backed module's scan, by the module.
pub(crate) type Scans = BTreeMap<Qualifier, HeldScan>;

/// A unit's records, by the place of each declaration's first item among the unit's items ([`curios_core::Items::places`]): the declarations lie in the order their items do.
pub(crate) type Records = Ordered<HeldRecord>;

/// Who read each binding, of a unit's lowered declarations and of the `use` declarations of its module bodies, read backwards: what a lowering over the unit finds the declarations and the bodies an edit could reach by, from the bindings it moved ([`Consulted`]).
///
/// **Read off the records, never stored.** Made the first time a lowering over the unit asks, and kept with the unit; the unit lowered over it holds it with the edges of every declaration lowered again or taken away, and of every body whose `use` declarations were resolved again or taken away, replaced, so a session reads no record backwards twice.
///
/// **Looked up, never walked**: its maps are persistent and keyed by hash.
#[derive(Clone, Default)]
pub(crate) struct LookupReaders {
    by: Persistent<Consulted, Vec<Key>>,
    bodies: Persistent<Consulted, Vec<Qualifier>>,
    /// The witnesses whose key read each name.
    keys: Persistent<curios_core::Global, Vec<curios_core::Global>>,
}

impl LookupReaders {
    /// Who read what, of `declarations`, of the bodies whose `use` declarations `uses` records, and of the witnesses whose keys `keyed` records.
    pub(super) fn of(
        declarations: &Records,
        uses: &Table<Vec<Consulted>>,
        keyed: &Sorted<curios_core::Global, Rc<Vec<curios_core::Global>>>,
    ) -> Self {
        curios_profile::profile!("lookup_readers::of", declarations = declarations.len());
        // Gathered whole first, so each binding's list is built once.
        let mut gathered = BTreeMap::<Consulted, Vec<Key>>::new();
        for (_, declaration) in declarations.iter() {
            if let Some(key) = declaration.key {
                for consulted in &declaration.consulted {
                    gathered.entry(*consulted).or_default().push(key);
                }
            }
        }
        let mut bodies = BTreeMap::<Consulted, Vec<Qualifier>>::new();
        for (body, read) in uses.iter() {
            for consulted in read.iter() {
                bodies.entry(*consulted).or_default().push(*body);
            }
        }
        let mut readers = LookupReaders::default();
        for (consulted, keys) in gathered {
            readers.by.insert(consulted, keys);
        }
        for (consulted, read) in bodies {
            readers.bodies.insert(consulted, read);
        }
        let mut keys = BTreeMap::<curios_core::Global, Vec<curios_core::Global>>::new();
        for (witness, read) in keyed.iter() {
            for name in read.iter() {
                keys.entry(*name).or_default().push(*witness);
            }
        }
        for (name, witnesses) in keys {
            readers.keys.insert(name, witnesses);
        }

        readers
    }

    /// The witnesses whose key read `name`.
    pub(super) fn of_key(&self, name: &curios_core::Global) -> &[curios_core::Global] {
        self.keys.get(name).map_or(&[], Vec::as_slice)
    }

    /// Replace the edges of the witness `witness`: those `earlier`, what its key read, added are taken out, and those `now` adds are put in.
    fn replace_key(
        &mut self,
        witness: curios_core::Global,
        earlier: &[curios_core::Global],
        now: &[curios_core::Global],
    ) {
        for name in earlier {
            let Some(held) = self.keys.get(name) else {
                continue;
            };
            let kept = held
                .iter()
                .filter(|held| **held != witness)
                .copied()
                .collect::<Vec<_>>();
            match kept.is_empty() {
                true => {
                    self.keys.remove(name);
                }
                false => self.keys.insert(*name, kept),
            }
        }
        for name in now {
            let mut held = self.keys.get(name).cloned().unwrap_or_default();
            held.push(witness);
            self.keys.insert(*name, held);
        }
    }

    /// The declarations whose lookups read `consulted`.
    fn of_binding(&self, consulted: &Consulted) -> &[Key] {
        self.by.get(consulted).map_or(&[], Vec::as_slice)
    }

    /// Whether a declaration or a body's `use` declarations wrote a name under the mount `prefix`, which the name reached.
    pub(super) fn reaches_mount(&self, prefix: Qualifier) -> bool {
        let consulted = Consulted::Mount(prefix);

        !self.of_binding(&consulted).is_empty() || !self.of_body(&consulted).is_empty()
    }

    /// The module bodies whose `use` declarations read `consulted`.
    pub(super) fn of_body(&self, consulted: &Consulted) -> &[Qualifier] {
        self.bodies.get(consulted).map_or(&[], Vec::as_slice)
    }

    /// Replace the edges of the module body `body`: those `earlier`, what its `use` declarations read, added are taken out, and those `now` adds are put in.
    fn replace_body(&mut self, body: Qualifier, earlier: &[Consulted], now: &[Consulted]) {
        for consulted in earlier {
            let Some(held) = self.bodies.get(consulted) else {
                continue;
            };
            let kept = held
                .iter()
                .filter(|held| **held != body)
                .copied()
                .collect::<Vec<_>>();
            match kept.is_empty() {
                true => {
                    self.bodies.remove(consulted);
                }
                false => self.bodies.insert(*consulted, kept),
            }
        }
        for consulted in now {
            let mut held = self.bodies.get(consulted).cloned().unwrap_or_default();
            held.push(body);
            self.bodies.insert(*consulted, held);
        }
    }

    /// Replace the edges of the declaration found by `key`: those `earlier`, what its lookups read, added are taken out, and those `now` adds are put in.
    pub(super) fn replace(&mut self, key: Key, earlier: &[Consulted], now: &[Consulted]) {
        for consulted in earlier {
            let Some(held) = self.by.get(consulted) else {
                continue;
            };
            let kept = held
                .iter()
                .filter(|held| **held != key)
                .copied()
                .collect::<Vec<_>>();
            match kept.is_empty() {
                true => {
                    self.by.remove(consulted);
                }
                false => self.by.insert(*consulted, kept),
            }
        }
        for consulted in now {
            let mut held = self.by.get(consulted).cloned().unwrap_or_default();
            held.push(key);
            self.by.insert(*consulted, held);
        }
    }
}

/// What an edit to a unit's sources moved of what its lowering reads, found from the edit: the modules whose every declaration is asked, and the declarations of the others whose lookups read a binding that moved.
pub(super) struct Moved {
    touched: HashSet<Qualifier>,
    reached: HashSet<Key>,
}

impl Moved {
    /// What moved, as resolving the unit's export view over the baseline's found it ([`Resolution`]): every module it seeded again is asked whole, its text touched or its scope read through a binding that moved, and of the others each declaration whose lookups read a binding that moved.
    pub(super) fn found(baseline: &PreparedText, revised: Resolution) -> Self {
        let readers = baseline.lookup_readers();
        let reached = revised
            .moved
            .iter()
            .flat_map(|consulted| readers.of_binding(consulted))
            .copied()
            .collect();

        Self {
            touched: revised.affected,
            reached,
        }
    }
}

/// A name a declaration's lowering resolved through its scope, as it was written: a path of several segments or an absolute one through the modules it may see, a bare one through the bindings in scope where it stands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub(crate) struct Lookup {
    pub absolute: bool,
    pub path: Qualifier,
}

/// A binding the lowering read to resolve a name, found or not: what an answer to a lookup can move with. Each is a label of one table, named by the path the label extends.
///
/// **Read where the resolution reads it.** A bare name and the head of a relative path are read off the lexical scope of the module body they are written in, a binding or a module; every further step, and a path's last segment, off the table and the interface of the module the step stands in; a glob off the whole of a module's interface. So a lookup answers as it did wherever every binding it read is the entry it was, and a declaration whose lookups all do lowers to what it lowered to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[curios_archive::archived]
pub(crate) enum Consulted {
    /// The label `at` names of the lexical scope of the module body its parent names: a binding, where `binding`, and a module otherwise.
    Scope { at: Qualifier, binding: bool },
    /// The label `at` names of the table and interface of the module its parent names: a binding, where `binding`, and a child module otherwise.
    Member { at: Qualifier, binding: bool },
    /// Every label the interface of the module offers, as a glob reads them.
    Members(Qualifier),
    /// The mount a name was written under, which the name reached.
    Mount(Qualifier),
}

/// An `unused-binder` lint as a lowered declaration holds it: placed in the declaration's text, and naming the definition it is about by its place among the declaration's, since a witness's name is given anew by every lowering.
#[derive(Debug, Clone)]
#[curios_archive::archived]
struct Unused {
    message: String,
    at: curios_core::Extent,
    definition: u32,
    ordinal: u32,
}

/// What one source declaration lowered to, beside the items themselves, which are its unit's: see the module documentation.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(crate) struct LoweredDeclaration {
    /// The text it was lowered from, by its anchor and the text's stamp. `None` for a declaration no later lowering may keep: one that was built rather than written, one a name of which resolved to nothing, and one whose findings could not be placed in its text.
    key: Option<Key>,
    /// The text it was lowered from, whether or not a later lowering may keep it: what the unit's anchors hold of it. `None` for one built rather than written.
    text: Option<Key>,
    /// The module body it is written in.
    body: Qualifier,
    /// Whether its lowering registered a `foreign` row, which the unit's store holds in the order the declarations stand.
    foreign: bool,
    /// What the unit-wide passes read of each item it lowered to, in the unit's order.
    items: Vec<LoweredItem>,
    /// The definitions it recorded an import scope for, each by its place among the declaration's.
    scoped: Vec<u32>,
    lints: Vec<Unused>,
    /// Every name it resolved through its scope, with what each resolved to.
    trace: curios_core::Trace<Lookup, Qualifier>,
    /// Every binding resolving those names read, each once, in order: what an answer can move with ([`Consulted`]).
    consulted: Vec<Consulted>,
}

impl LoweredDeclaration {
    /// What the declaration `context` has just lowered to `items` is recorded as, `facts` having been taken of them.
    pub(super) fn made(
        context: &Context,
        items: &[FlatItem],
        facts: Vec<LoweredItem>,
        foreign: bool,
    ) -> Self {
        let Journal {
            trace,
            consulted,
            scoped,
            lints,
            unbound,
        } = context.journal();
        let names = items.iter().flat_map(FlatItem::names).collect::<Vec<_>>();
        let place = |name: &curios_core::Global| {
            let place = names.iter().position(|declared| declared == name)?;
            u32::try_from(place).ok()
        };

        let scoped = scoped.iter().map(place).collect::<Option<Vec<_>>>();
        let lints = lints
            .iter()
            .map(|lint| {
                let binder = lint.binder?;

                Some(Unused {
                    message: lint.report.message.clone(),
                    at: context.extent(lint.report.span.as_ref()?)?,
                    definition: place(&binder.declaration?)?,
                    ordinal: binder.ordinal,
                })
            })
            .collect::<Option<Vec<_>>>();
        // A name that was written and could not be placed is one the `unused-declaration` lint would lose with a kept lowering.
        let placed = items
            .iter()
            .flat_map(FlatItem::lets)
            .zip(facts.iter().flat_map(|lowered| &lowered.definitions))
            .all(|(let_, declared)| let_.span.is_none() || declared.label.is_some());

        let text = context.sited(|key, _| key);
        let key = match (unbound, placed, &scoped, &lints) {
            (false, true, Some(_), Some(_)) => text,
            _ => None,
        };

        Self {
            key,
            text,
            body: context.prefix(),
            foreign,
            items: facts,
            scoped: scoped.unwrap_or_default(),
            lints: lints.unwrap_or_default(),
            trace,
            consulted: consulted
                .into_iter()
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect(),
        }
    }

    /// What the unit-wide passes read of each item this lowered to.
    pub(super) fn facts(&self) -> &[LoweredItem] {
        &self.items
    }

    /// The text this was lowered from. See the field.
    pub(super) fn text(&self) -> Option<Key> {
        self.text
    }

    /// The module body this is written in.
    pub(super) fn body(&self) -> Qualifier {
        self.body
    }

    /// Whether a later lowering may keep this. See [`LoweredDeclaration::key`].
    pub(super) fn keyable(&self) -> bool {
        self.key.is_some()
    }

    /// The text a later lowering keeps this by. See the field.
    pub(super) fn key(&self) -> Option<Key> {
        self.key
    }

    /// Whether this registered a `foreign` row. See the field.
    pub(super) fn foreign(&self) -> bool {
        self.foreign
    }
}

/// `records`, in the order their items stand, each at the index its first item has among the unit's.
pub(super) fn in_walk_order(records: impl IntoIterator<Item = HeldRecord>) -> Records {
    let mut ordered = Records::new();
    let mut first = 0;
    for record in records {
        let count = record.items.len();
        ordered.insert(
            i64::try_from(first).expect("a unit's items are counted in an i64"),
            record,
        );
        first += count;
    }

    ordered
}

/// A unit's baseline half: what a later lowering of the same sources reads of it, and nothing compiled after the unit does.
///
/// **Each record and each scan an object of its own** ([`HalfEntry`]), stored apart and handed back where the half is restored ([`curios_utilities::Records`]): what is archived with the unit is the rest, the tables of each module body and each witness.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub(crate) struct BaselineHalf {
    /// What each source declaration lowered to beside its items, by the place of its first item ([`Records`]).
    #[archived_with(curios_archive::Skip)]
    pub(crate) declarations: Records,
    /// Each file-backed module's scan, by the module.
    #[archived_with(curios_archive::Skip)]
    pub(crate) scans: Scans,
    /// Every binding the `use` declarations of each module body read, by the module: what the scope of its body can move with. A body that reads none holds none.
    #[archived_with(curios_utilities::InSharedKeyOrder)]
    pub(crate) uses: Table<Vec<Consulted>>,
    /// Every lint each module body's declarations and `use` declarations were found to leave, by the body, before elaboration credits a binder: what a body a later lowering passes unwalked contributes. A body that leaves none holds none.
    #[archived_with(curios_utilities::InSharedKeyOrder)]
    pub(crate) lints: Table<Vec<Lint>>,
    /// Every name each witness's key read, by the witness: what its name can move with.
    #[archived_with(curios_utilities::InSharedKeyOrder)]
    pub(crate) keyed: Sorted<curios_core::Global, Rc<Vec<curios_core::Global>>>,
}

/// An entry of a unit's baseline half as a store keeps it, an object of its own bytes: a declaration's record, or a file's scan.
#[curios_archive::archived]
pub(crate) enum HalfEntry {
    Record(LoweredDeclaration),
    Scan(Qualifier, Scan),
}

impl HalfEntry {
    /// The object this entry is stored as: its archive, and the digest of it.
    fn object(&self) -> Result<Object, String> {
        curios_archive::to_bytes(self).map(|bytes| Object::of(bytes.to_vec()))
    }
}

impl BaselineHalf {
    /// Each entry of this half, in the order the unit names them: each declaration's record in the order its items stand, then each file's scan by its module.
    fn entries(&self) -> impl Iterator<Item = Entry<'_>> {
        self.declarations
            .iter()
            .map(|(_, record)| Entry::Record(record))
            .chain(
                self.scans
                    .iter()
                    .map(|(module, scan)| Entry::Scan(module, scan)),
            )
    }

    /// The digest of each entry, in the order the unit names them: the one it was restored with or first filed under, and its object's otherwise.
    pub(crate) fn digests(&self) -> Result<Vec<Digest>, String> {
        self.entries().map(|entry| entry.digest()).collect()
    }

    /// The entries `wanted` asks for, as the objects a store keeps them as, each archived now.
    pub(crate) fn objects(&self, wanted: &dyn Fn(&Digest) -> bool) -> Result<Vec<Object>, String> {
        let mut objects = Vec::new();
        for entry in self.entries() {
            if wanted(&entry.digest()?) {
                objects.push(entry.stored().object()?);
            }
        }

        Ok(objects)
    }
}

/// An entry of a baseline half as it holds it.
enum Entry<'h> {
    Record(&'h HeldRecord),
    Scan(&'h Qualifier, &'h HeldScan),
}

impl Entry<'_> {
    /// The entry as a store keeps it.
    fn stored(&self) -> HalfEntry {
        match self {
            Entry::Record(record) => HalfEntry::Record(LoweredDeclaration::clone(record)),
            Entry::Scan(module, scan) => HalfEntry::Scan(**module, Scan::clone(scan)),
        }
    }

    /// The digest this entry is stored under.
    fn digest(&self) -> Result<Digest, String> {
        let stored = || self.stored().object().map(|object| object.digest);
        match self {
            Entry::Record(record) => record.digest(|_| stored()),
            Entry::Scan(_, scan) => scan.digest(|_| stored()),
        }
    }
}

/// A baseline half restored out of what a unit stores of it: the rest, archived with the unit, and each record and scan an object of its own, restored beside the digest it was found by.
impl curios_utilities::Records for BaselineHalf {
    fn restored(table: &[u8], entries: &[Object]) -> Result<Self, String> {
        let mut half = curios_archive::from_bytes::<BaselineHalf>(table)?;
        let mut records = Vec::new();
        for object in entries {
            match curios_archive::from_bytes::<HalfEntry>(object.bytes.as_slice())? {
                HalfEntry::Record(record) => {
                    records.push(Rc::new(Filed::stored(record, object.digest)));
                }
                HalfEntry::Scan(module, scan) => {
                    half.scans
                        .insert(module, Rc::new(Filed::stored(scan, object.digest)));
                }
            }
        }
        half.declarations = in_walk_order(records);

        Ok(half)
    }
}

/// A unit an earlier text of the same sources lowered to, as a later one is lowered over it: its declarations by the text each was lowered from, each by the place of its first item.
pub(super) struct Baseline<'a> {
    unit: &'a PreparedText,
    by_text: &'a Persistent<Key, i64>,
    /// What the edit moved of what the lowering reads ([`Moved`]): `None` where a unit in scope moved, and every declaration is asked.
    moved: Option<Moved>,
    /// The declarations kept without asking, by their keys: what marks the `use` sites and the mounts their lookups read, where asking would have.
    unasked: RefCell<HashSet<Key>>,
    /// The declarations kept, by the place of each one's first item: a declaration is kept once, a second written as the same text, which the same anchor and stamp find, being lowered.
    claimed: RefCell<HashSet<i64>>,
}

impl<'a> Baseline<'a> {
    pub(super) fn of(unit: &'a PreparedText) -> Self {
        Self {
            unit,
            by_text: unit.by_text(),
            moved: None,
            unasked: RefCell::new(HashSet::new()),
            claimed: RefCell::new(HashSet::new()),
        }
    }

    /// This baseline with `moved` as what the edit moved of what the lowering reads.
    pub(super) fn moving(self, moved: Option<Moved>) -> Self {
        Self { moved, ..self }
    }

    /// The modules a walk over this baseline enters, every other being passed unentered with everything under it: each whose text the edit touched or whose scope a binding that moved is read in, each holding a declaration whose lookups read a binding that moved, each holding a kept witness whose key read a declaration of those, and each holding a declaration no later lowering may keep, with every module above each. `None` where a unit in scope moved, and every module is entered.
    ///
    /// **A module passed unentered is the baseline's whole**: its text stands, its scope stands, and nothing any of its declarations read moved, so lowering it again would write what the baseline holds.
    pub(super) fn entered(&self) -> Option<HashSet<Qualifier>> {
        let moved = self.moved.as_ref()?;
        let bodies = self.unit.bodies();
        let items = &self.unit.core().items;
        let mut entered = moved.touched.iter().copied().collect::<Vec<_>>();
        // The names a declaration lowered now or taken away can hold: those of every declaration of a touched module and of every one an edit reached.
        let mut names = Vec::new();
        let declared = |first: i64, names: &mut Vec<curios_core::Global>| {
            let record = self.unit.lowerings().get(first)?;
            let mut place = usize::try_from(first).ok();
            for _ in 0..record.facts().len() {
                let at = place?;
                names.extend(items.names(at).into_iter().copied());
                place = items.place_after(Some(at));
            }
            Some(record.body())
        };
        #[expect(clippy::iter_over_hash_type, reason = "the modules entered are a set")]
        for key in &moved.reached {
            if let Some(&first) = self.by_text.get(key)
                && let Some(body) = declared(first, &mut names)
            {
                entered.push(body);
            }
        }
        #[expect(
            clippy::iter_over_hash_type,
            reason = "the names gathered are read as a set"
        )]
        for module in &moved.touched {
            if let Some(body) = bodies.get(module) {
                for &first in &body.firsts {
                    declared(first, &mut names);
                }
            }
        }
        entered.extend(
            bodies
                .iter()
                .filter(|(_, body)| body.unkeyable > 0)
                .map(|(module, _)| *module),
        );
        let readers = self.unit.lookup_readers();
        for name in &names {
            for witness in readers.of_key(name) {
                if let curios_core::Global::Witness(id) = witness {
                    entered.push(*id.module());
                }
            }
        }

        let mut closed = HashSet::new();
        for module in entered {
            let mut at = module;
            while closed.insert(at) && !at.is_root() {
                at = at.without_last();
            }
        }
        Some(closed)
    }

    /// The declaration of this unit the one `context` stands at was lowered as, by the place of its first item, kept without asking: its module's text is the one the baseline lowered, so its own is, and no binding its lookups read moved. `None` where either may have, and the declaration is asked ([`Baseline::standing`]).
    pub(super) fn unasked(&self, context: &Context) -> Option<i64> {
        let moved = self.moved.as_ref()?;
        if !self.stands(&context.prefix()) {
            return None;
        }
        let (key, index) = context.sited(|key, _| Some((key, *self.by_text.get(&key)?)))??;
        if moved.reached.contains(&key) || !self.claimed.borrow_mut().insert(index) {
            return None;
        }
        self.unasked.borrow_mut().insert(key);

        Some(index)
    }

    /// Whether what the module body `module` is read off stands: its text the one the baseline lowered and nothing its `use` declarations read moved, so its scope is the baseline's.
    pub(super) fn stands(&self, module: &Qualifier) -> bool {
        self.moved
            .as_ref()
            .is_some_and(|moved| !moved.touched.contains(module))
    }

    /// Mark used each `use` site of the module body `context` is whose label a declaration kept unasked read: what asking its lookups again would have marked.
    pub(super) fn mark_uses(&self, context: &Context) {
        let unasked = self.unasked.borrow();
        if unasked.is_empty() {
            return;
        }
        let readers = self.unit.lookup_readers();
        let used = context
            .imported()
            .filter(|(label, binding)| {
                readers
                    .of_binding(&Consulted::Scope {
                        at: context.prefix().with(label),
                        binding: *binding,
                    })
                    .iter()
                    .any(|key| unasked.contains(key))
            })
            .filter_map(|(label, binding)| context.site_of(label, binding))
            .collect::<Vec<_>>();
        for site in used {
            context.use_site(site);
        }
    }

    /// Who read what of the unit lowered over this baseline, `uses` being what the `use` declarations of its bodies read: the baseline's, with the edges of every declaration `gone` takes away and every one `made` lowers replaced, of every body of `bodies`, each one whose `use` declarations were resolved again or that the unit no longer holds, and of every witness of `witnesses`, whose key `keyed` records what it read of.
    pub(super) fn readers_of<'r>(
        &self,
        gone: impl IntoIterator<Item = &'r HeldRecord>,
        made: impl IntoIterator<Item = &'r HeldRecord>,
        uses: &Table<Vec<Consulted>>,
        bodies: impl IntoIterator<Item = Qualifier>,
        keyed: &Sorted<curios_core::Global, Rc<Vec<curios_core::Global>>>,
        witnesses: impl IntoIterator<Item = curios_core::Global>,
    ) -> LookupReaders {
        let mut readers = self.unit.lookup_readers().clone();
        for declaration in gone {
            if let Some(key) = declaration.key {
                readers.replace(key, &declaration.consulted, &[]);
            }
        }
        for declaration in made {
            if let Some(key) = declaration.key {
                readers.replace(key, &[], &declaration.consulted);
            }
        }
        let earlier = &self.unit.half().uses;
        for body in bodies {
            let read = |uses: &Table<Vec<Consulted>>| {
                uses.get(&body)
                    .map_or(&[][..], |read| read.as_slice())
                    .to_vec()
            };
            readers.replace_body(body, &read(earlier), &read(uses));
        }
        let earlier = &self.unit.half().keyed;
        for witness in witnesses {
            let read = |keyed: &Sorted<curios_core::Global, Rc<Vec<curios_core::Global>>>| {
                keyed
                    .get(&witness)
                    .map_or(&[][..], |read| read.as_slice())
                    .to_vec()
            };
            readers.replace_key(witness, &read(earlier), &read(keyed));
        }

        readers
    }

    /// The unit lowered over.
    pub(super) fn unit(&self) -> &'a PreparedText {
        self.unit
    }

    /// The declaration of this unit that the one `context` stands at was lowered as, by the place of its first item: the one lowered from the text that declaration is written in, where every name it resolved resolves here as it did.
    pub(super) fn standing(&self, context: &Context) -> Option<i64> {
        let first = context.sited(|key, text| {
            let first = *self.by_text.get(&key)?;

            (self.unit.anchors().text(key.0, key.1) == Some(text)).then_some(first)
        })??;
        if self.claimed.borrow().contains(&first) {
            return None;
        }

        let stands = self
            .record(first)
            .trace
            .stands(|lookup, found| context.answers(lookup, found));
        context.forget_asked();
        if stands {
            self.claimed.borrow_mut().insert(first);
        }

        stands.then_some(first)
    }

    /// The record of the declaration whose first item stands at `first`.
    fn record(&self, first: i64) -> &'a HeldRecord {
        self.unit
            .lowerings()
            .get(first)
            .expect("a record the baseline holds")
    }

    /// Take the declaration whose first item stands at `first` into `lowered` as lowering it again would have written it, its items under the names this lowering gives them, their registry entries and rows, its import scopes and its lints: what the walk reads of the unit's declarations. What it minted, and its items and record as the unit holds them, are the baseline's, which the unit is a version of, and are left where they stand ([`Step`]).
    pub(super) fn kept(&self, first: i64, context: &Context, lowered: &mut Lowering) {
        let declaration = self.record(first);
        let from = lowered.items.len();

        let mut names = Vec::new();
        let items = &self.unit.core().items;
        let mut place = usize::try_from(first).ok();
        for _ in 0..declaration.items.len() {
            let at = place.expect("a declaration's items stand together");
            place = items.place_after(Some(at));
            let item = self.unit.lowered_item(at);
            let kept = match item {
                curios_core::Item::Let(definition) => {
                    FlatItem::Let(kept_let(definition, context, lowered))
                }
                // A group of witnesses is closed again under the names this lowering gives its members, which the scope it was closed under spells.
                curios_core::Item::Rec(rec) if rec.definitions.iter().any(is_witness) => {
                    FlatItem::Rec(
                        rec.definitions()
                            .iter()
                            .map(|definition| kept_let(definition, context, lowered))
                            .collect(),
                    )
                }
                curios_core::Item::Rec(rec) => {
                    for definition in &rec.definitions {
                        keep_entries(definition.name, &definition.kind, lowered);
                    }

                    FlatItem::Kept(rec.clone())
                }
            };
            names.extend(kept.names());
            lowered.items.push(kept);
        }

        for &place in &declaration.scoped {
            context.record_import_scope(Some(&names[place as usize]));
        }
        context.report(declaration.lints.iter().map(|unused| Lint {
            kind: LintKind::UnusedBinder,
            report: Report {
                span: context.resolved(&unused.at),
                message: unused.message.clone(),
                notes: Vec::new(),
            },
            binder: Some(LintedBinder {
                declaration: Some(names[unused.definition as usize]),
                ordinal: unused.ordinal,
            }),
        }));
        lowered.steps.push(Step {
            kept: Some(first),
            passed: None,
            record: lowered.declarations.len(),
            from,
        });
        lowered.declarations.push(Rc::clone(declaration));
    }
}

/// One definition of a kept item as the walk holds one it lowered: a witness under the next name of its module, as a lowering gives it before its signature is read.
fn kept_let(
    definition: &curios_core::Definition,
    context: &Context,
    lowered: &mut Lowering,
) -> FlatLet {
    // A kept witness keeps the name the baseline gives it, which naming changes where its place or its key may have moved; it still takes its place among its module's, which a witness lowered after it counts.
    if matches!(definition.kind, curios_core::DefinitionKind::Witness) {
        context.fresh_witness();
    }
    let name = definition.name;
    keep_entries(name, &definition.kind, lowered);

    FlatLet {
        name,
        kind: definition.kind.clone(),
        // What is read of where it was written was taken where it was lowered.
        span: None,
        island: definition.island,
        type_: definition.type_.clone(),
        body: definition.body.clone(),
    }
}

/// Take the entries the baseline lowered with its definition `name`, of kind `kind`, into `lowered`.
fn keep_entries(
    name: curios_core::Global,
    kind: &curios_core::DefinitionKind,
    lowered: &mut Lowering,
) {
    // A kept former carries the entry the baseline lowered with it.
    match kind.nominal() {
        Some(curios_core::Nominal::Inductive(entry)) => {
            lowered.induct_decls.insert(name, entry.clone());
        }
        Some(curios_core::Nominal::Struct(record)) => {
            lowered.struct_decls.insert(name, record.clone());
        }
        Some(curios_core::Nominal::Concept { concept, record }) => {
            lowered.struct_decls.insert(name, record.clone());
            lowered.concepts.insert(name, concept.clone());
        }
        None => {}
    }
    match kind {
        curios_core::DefinitionKind::Witness => {
            lowered.witnesses.insert(name);
        }
        curios_core::DefinitionKind::Test => lowered.tests.push(name),
        _ => {}
    }
}

fn is_witness(definition: &curios_core::RecDefinition) -> bool {
    matches!(definition.kind, curios_core::DefinitionKind::Witness)
}
