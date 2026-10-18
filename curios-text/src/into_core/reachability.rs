//! Which of a unit's items the `unused-declaration` lint's roots reach, kept with the unit so a lowering over it decides again only what its edit could have moved ([`Reachability`]).
//!
//! **Decided by a search, not a count.** An item is live where a root reaches it, so an item an edit could have moved — one lowered now, one whose readers or edges changed, one whose name an export now shows or no longer does — is decided by searching back through its readers for a root or an item already decided live. A search that runs out finds every item it passed dead too, since a root reaching one of them would reach the item. What changed of an item's liveness moves the items its edges reach, which are decided again in turn, so the lint's dead items are the ones a whole walk finds.

use {
    super::{Gone, LoweredItem, PublicInterface, Records, Table, kept, named},
    crate::Lint,
    curios_utilities::{Mount, Persistent, Qualifier, Sorted, Span},
    std::{
        collections::{BTreeSet, HashMap, HashSet},
        rc::Rc,
    },
};

/// What reaches each of a unit's items: see the module documentation.
#[derive(Clone, Default)]
pub(crate) struct Reachability {
    /// The first name of every item no root reaches.
    dead: Persistent<curios_core::Global, ()>,
    /// The first names of the items whose edges reach each name.
    readers: Persistent<curios_core::Global, Vec<curios_core::Global>>,
    /// The modules a consumer outside the unit reaches through public children from a prefix the unit claims, whose bindings are roots.
    open: Rc<BTreeSet<Qualifier>>,
    /// The names the entry's terms name, each a root.
    entry: Rc<BTreeSet<curios_core::Global>>,
    /// How many reportable definitions each module declares, by the module.
    reportable: Sorted<Qualifier, usize>,
}

/// A unit's items as the lint reads them, by place: what each declares and what was taken of it where it was lowered.
#[derive(Clone, Copy)]
pub(super) struct Items<'u> {
    pub items: &'u curios_core::Items,
    pub places: &'u curios_core::Places,
    pub records: &'u Records,
}

impl<'u> Items<'u> {
    /// What was taken of the item at `place` where it was lowered.
    fn facts(&self, place: usize) -> &'u LoweredItem {
        let (first, record) = self
            .records
            .before(position(place) + 1)
            .expect("every item is a declaration's");
        let mut at = place_of(first);
        let mut index = 0;
        while at != place {
            at = self
                .items
                .place_after(Some(at))
                .expect("a declaration's items stand together");
            index += 1;
        }

        &record.facts()[index]
    }

    /// The place of the item declaring `name`, where the unit declares it.
    fn place(&self, name: &curios_core::Global) -> Option<usize> {
        self.places.place(name).map(|place| place.item())
    }

    fn first(&self, place: usize) -> curios_core::Global {
        *self
            .items
            .first_name(place)
            .expect("an item declares a name")
    }

    /// Whether the item at `place` is a root wherever it stands: a test or a witness.
    fn rooted(&self, place: usize) -> bool {
        self.items.declared(place).iter().any(|declared| {
            matches!(
                declared,
                curios_core::Declared::Witness | curios_core::Declared::Test
            )
        })
    }

    /// The names of the item at `place` the lint may report, each a module's: those of the definitions a reader wrote under their own name.
    fn reportable(&self, place: usize) -> Vec<(usize, curios_core::Global)> {
        self.items
            .names(place)
            .into_iter()
            .zip(self.items.declared(place))
            .enumerate()
            .filter(|(_, (_, declared))| is_reportable(*declared))
            .map(|(index, (name, _))| (index, *name))
            .collect()
    }
}

/// Whether a definition is one the reader declared under its own name: a `let`, a `foreign`, or the type former of an `induct`, `struct` or `concept`.
fn is_reportable(declared: curios_core::Declared) -> bool {
    matches!(
        declared,
        curios_core::Declared::Authored
            | curios_core::Declared::Inductive
            | curios_core::Declared::Struct
            | curios_core::Declared::Concept
    )
}

/// The modules a consumer outside the unit reaches through public children from `own`'s prefixes.
fn open_modules(public: &Table<PublicInterface>, own: &[Mount]) -> BTreeSet<Qualifier> {
    let mut open = BTreeSet::new();
    let mut pending = own.iter().map(|mount| mount.prefix).collect::<Vec<_>>();
    while let Some(module) = pending.pop() {
        if !open.insert(module) {
            continue;
        }
        if let Some(interface) = public.get(&module) {
            pending.extend(interface.children.values().map(|entry| entry.target));
        }
    }

    open
}

/// The names `entry`'s terms name.
pub(super) fn entry_names(
    entry: Option<&curios_core::Entrypoint>,
) -> BTreeSet<curios_core::Global> {
    entry
        .into_iter()
        .flat_map(|entry| std::iter::once(&entry.body).chain(entry.type_.as_ref()))
        .flat_map(named)
        .collect()
}

/// The module a definition's name is declared in.
fn module_of(name: &curios_core::Global) -> Option<Qualifier> {
    name.qualifier().map(Qualifier::without_last)
}

impl Reachability {
    /// What reaches each item of `items`, the items of a unit whose export view is `public`, which claims `own`, and whose entry's terms name `entry`: every item's edges read backwards, and the items its roots reach.
    pub(super) fn of(
        items: Items<'_>,
        public: &Table<PublicInterface>,
        own: &[Mount],
        entry: BTreeSet<curios_core::Global>,
    ) -> Self {
        curios_profile::profile!("reachability::of");
        let open = open_modules(public, own);
        let mut gathered = HashMap::<curios_core::Global, Vec<curios_core::Global>>::new();
        let mut reportable = HashMap::<Qualifier, usize>::new();
        let mut pending = Vec::new();
        for place in items.items.places() {
            let first = items.first(place);
            for name in &items.facts(place).reaches {
                gathered.entry(*name).or_default().push(first);
            }
            for (_, name) in items.reportable(place) {
                if let Some(module) = module_of(&name) {
                    *reportable.entry(module).or_default() += 1;
                }
            }
            if items.rooted(place) {
                pending.push(place);
            }
        }
        for module in &open {
            if let Some(interface) = public.get(module) {
                #[expect(clippy::iter_over_hash_type, reason = "the items reached are a set")]
                for entry in interface.bindings.values() {
                    pending.extend(items.place(&curios_core::Global::Authored(entry.target)));
                }
            }
        }
        pending.extend(entry.iter().filter_map(|name| items.place(name)));
        let mut reached = HashSet::new();
        while let Some(place) = pending.pop() {
            if !reached.insert(place) {
                continue;
            }
            pending.extend(
                items
                    .facts(place)
                    .reaches
                    .iter()
                    .filter_map(|name| items.place(name)),
            );
        }

        let mut state = Self {
            open: Rc::new(open),
            entry: Rc::new(entry),
            ..Self::default()
        };
        for place in items.items.places() {
            if !reached.contains(&place) {
                state.dead.insert(items.first(place), ());
            }
        }
        #[expect(
            clippy::iter_over_hash_type,
            reason = "a persistent map holds the same entries whatever order they arrive in"
        )]
        for (name, read) in gathered {
            state.readers.insert(name, read);
        }
        #[expect(
            clippy::iter_over_hash_type,
            reason = "a sorted map holds the same entries whatever order they arrive in"
        )]
        for (module, count) in reportable {
            state.reportable.insert(module, count);
        }

        state
    }

    /// These, of a baseline whose items were `then`, made what reaches each of `now`'s, a version of them: `gone` holds the baseline's declarations the walk did not keep, with the names they declared, `made` the places of the items lowered now, `renamed` each kept witness named otherwise, `exported` every name whose exports may have changed, and `entry` what the entry's terms name now. `None` where the modules open to a consumer may have moved, and every item is decided again ([`Reachability::of`]).
    #[expect(
        clippy::too_many_arguments,
        reason = "what the edit changed of each thing an item's liveness reads"
    )]
    pub(super) fn revised(
        &self,
        then: Items<'_>,
        now: Items<'_>,
        gone: &[Gone<'_>],
        made: &[usize],
        renamed: &[(curios_core::Global, curios_core::Global)],
        exported: &BTreeSet<Qualifier>,
        audiences: &dyn Fn(&Qualifier) -> Vec<(Qualifier, String)>,
        entry: BTreeSet<curios_core::Global>,
    ) -> Self {
        curios_profile::profile!("reachability::revised");
        let mut state = self.clone();
        let mut candidates = Vec::<curios_core::Global>::new();

        // The edges and the counts of what was taken away, then of what was lowered now.
        for gone in gone {
            for &place in &gone.places {
                let first = then.first(place);
                for name in &then.facts(place).reaches {
                    state.unread(name, &first);
                    candidates.push(*name);
                }
                for (_, name) in then.reportable(place) {
                    if let Some(module) = module_of(&name) {
                        state.count(module, false);
                    }
                }
                state.dead.remove(&first);
            }
        }
        for &place in made {
            let first = now.first(place);
            for name in &now.facts(place).reaches {
                state.read(name, first);
                candidates.push(*name);
            }
            for (_, name) in now.reportable(place) {
                if let Some(module) = module_of(&name) {
                    state.count(module, true);
                }
            }
            candidates.push(first);
        }
        // A kept witness named otherwise reads what it read under the name it takes now.
        for (then_name, now_name) in renamed {
            let Some(place) = now.place(now_name) else {
                continue;
            };
            if now.first(place) != *now_name {
                continue;
            }
            for name in &now.facts(place).reaches {
                state.unread(name, then_name);
                state.read(name, *now_name);
            }
        }
        candidates.extend(
            exported
                .iter()
                .map(|name| curios_core::Global::Authored(*name)),
        );
        candidates.extend(
            state
                .entry
                .symmetric_difference(&entry)
                .copied()
                .collect::<Vec<_>>(),
        );
        state.entry = Rc::new(entry);

        // Each candidate decided, and what its liveness moved decided after it.
        let root = |place: usize, state: &Self| {
            now.rooted(place)
                || now.items.names(place).iter().any(|name| {
                    state.entry.contains(*name)
                        || name.qualifier().is_some_and(|path| {
                            audiences(path)
                                .iter()
                                .any(|(module, _)| state.open.contains(module))
                        })
                })
        };
        let mut decided = HashMap::<curios_core::Global, bool>::new();
        let mut pending = candidates
            .iter()
            .filter_map(|name| now.place(name))
            .collect::<Vec<_>>();
        while let Some(place) = pending.pop() {
            let first = now.first(place);
            if decided.contains_key(&first) {
                continue;
            }
            let (live, passed) = state.search(place, now, &decided, &root);
            let changed = passed.iter().filter(|(first, _)| {
                decided.insert(*first, live);
                state.dead.contains_key(first) == live
            });
            let moved = changed.map(|(_, place)| *place).collect::<Vec<_>>();
            for place in moved {
                let first = now.first(place);
                match live {
                    true => {
                        state.dead.remove(&first);
                    }
                    false => state.dead.insert(first, ()),
                }
                pending.extend(
                    now.facts(place)
                        .reaches
                        .iter()
                        .filter_map(|name| now.place(name)),
                );
            }
        }

        state
    }

    /// Whether a root reaches the item at `place` of `items`, searching back through its readers, beside the first name and place of each item passed: the item alone where it is live, and every item the search passed where it is not.
    fn search(
        &self,
        place: usize,
        items: Items<'_>,
        decided: &HashMap<curios_core::Global, bool>,
        root: &dyn Fn(usize, &Self) -> bool,
    ) -> (bool, Vec<(curios_core::Global, usize)>) {
        let start = (items.first(place), place);
        let mut passed = vec![start];
        let mut seen = HashSet::from([start.0]);
        let mut index = 0;
        while let Some(&(first, at)) = passed.get(index) {
            index += 1;
            if decided.get(&first) == Some(&true) || root(at, self) {
                return (true, vec![start]);
            }
            for name in items.items.names(at) {
                for reader in self.readers.get(name).into_iter().flatten() {
                    if decided.get(reader) == Some(&false) || !seen.insert(*reader) {
                        continue;
                    }
                    if let Some(place) = items.place(reader) {
                        passed.push((*reader, place));
                    }
                }
            }
        }

        (false, passed)
    }

    fn read(&mut self, name: &curios_core::Global, reader: curios_core::Global) {
        let mut held = self.readers.get(name).cloned().unwrap_or_default();
        held.push(reader);
        self.readers.insert(*name, held);
    }

    fn unread(&mut self, name: &curios_core::Global, reader: &curios_core::Global) {
        let Some(held) = self.readers.get(name) else {
            return;
        };
        let mut held = held.clone();
        if let Some(at) = held.iter().position(|held| held == reader) {
            held.remove(at);
        }
        match held.is_empty() {
            true => {
                self.readers.remove(name);
            }
            false => self.readers.insert(*name, held),
        }
    }

    fn count(&mut self, module: Qualifier, more: bool) {
        let held = self.reportable.get(&module).copied().unwrap_or_default();
        let count = match more {
            true => held + 1,
            false => held.saturating_sub(1),
        };
        match count {
            0 => {
                self.reportable.remove(&module);
            }
            count => self.reportable.insert(module, count),
        }
    }

    /// The `unused-declaration` lints of `items`, read off the items these say no root reaches: each dead declaration a reader wrote, and each private module every reportable declaration of which is dead, once, its subtree folded into it. `placed` gives the span an extent of one of the unit's declarations lies on, `child` whether a module declares a child of a label public, and `mod_spans` where each module was written.
    pub(super) fn lints(
        &self,
        items: Items<'_>,
        placed: &dyn Fn(&curios_core::Extent) -> Option<Span>,
        child: &dyn Fn(&Qualifier, &str) -> Option<bool>,
        mod_spans: &HashMap<Qualifier, Span>,
    ) -> Vec<Lint> {
        // Every dead reportable name, and the ones a reader wrote and did not keep with a `_`, with where each is written.
        let mut dead_names = Vec::new();
        let mut written = Vec::new();
        for (first, ()) in self.dead.iter() {
            let Some(place) = items.place(first) else {
                continue;
            };
            let facts = items.facts(place);
            for (index, name) in items.reportable(place) {
                dead_names.push(name);
                let Some(label) = facts
                    .definitions
                    .get(index)
                    .and_then(|declared| declared.label)
                else {
                    continue;
                };
                if !name.qualifier().is_some_and(kept) {
                    written.push((name, label));
                }
            }
        }

        // Every module the lint may fold: one holding a declaration it reports.
        let mut modules = written
            .iter()
            .filter_map(|(name, _)| name.qualifier())
            .flat_map(|path| {
                let mut ancestors = Vec::new();
                let mut module = path.without_last();
                while !module.is_root() {
                    ancestors.push(module);
                    module = module.without_last();
                }
                ancestors
            })
            .filter(|module| mod_spans.contains_key(module))
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        modules.sort_by_key(|module| module.segments().len());
        let mut dead_modules = BTreeSet::new();
        for module in modules {
            if kept(&module) {
                continue;
            }
            let private = child(&module.without_last(), module.last()) == Some(false);
            let within = |name: &curios_core::Global| {
                name.qualifier().is_some_and(|path| path.is_within(&module))
            };
            let total = self.reportable_within(&module);
            let dead = dead_names.iter().filter(|name| within(name)).count();
            let has_dead = written.iter().any(|(name, _)| within(name));
            let folded = dead_modules
                .iter()
                .any(|dead: &Qualifier| module.is_within(dead));
            if private && has_dead && total == dead && !folded {
                dead_modules.insert(module);
            }
        }

        let mut lints = dead_modules
            .iter()
            .map(|module| Lint::unused_declaration(module.last(), mod_spans[module].clone()))
            .collect::<Vec<_>>();
        lints.extend(
            written
                .iter()
                .filter(|(name, _)| {
                    !name
                        .qualifier()
                        .is_some_and(|name| dead_modules.iter().any(|dead| name.is_within(dead)))
                })
                .map(|(name, label)| {
                    Lint::unused_declaration(
                        name.qualifier().map_or("", Qualifier::last),
                        placed(label)
                            .expect("a name lies in the text its declaration was sited at"),
                    )
                }),
        );
        lints
    }

    /// How many reportable definitions `module` and every module under it declare.
    fn reportable_within(&self, module: &Qualifier) -> usize {
        let mut total = self.reportable.get(module).copied().unwrap_or_default();
        let mut at = *module;
        while let Some((next, count)) = self.reportable.after(&at) {
            if !next.is_within(module) {
                break;
            }
            total += count;
            at = *next;
        }

        total
    }
}

/// A place as a record's key.
fn position(place: usize) -> i64 {
    i64::try_from(place).expect("a place is counted in an i64")
}

/// The place a record's key is.
fn place_of(position: i64) -> usize {
    usize::try_from(position).expect("a place is not negative")
}
