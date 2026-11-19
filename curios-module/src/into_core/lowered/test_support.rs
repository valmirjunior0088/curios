//! The census of a unit's lowering: each declaration it keeps a record of, held to that record and to the answers its trace holds ([`PreparedText::census`]). What a recompile keeps of a declaration is what a whole lowering makes where both hold: one kept unasked where nothing its record names moved, and one kept by asking where every name it resolved is answered as it was.

use {
    super::{Consulted, Found, Key, LoweredDeclaration},
    crate::{
        Compiled, PreparedText,
        into_core::{
            ChildInfo, Context, Declares, Entry, FlatItem, Head, Headed, Held, Lowering,
            ModuleInfo, Named, PublicInterface, Reach, Scoped, Shared, Table, anchored,
            lower_declaration, rename_witnesses,
        },
        tree_of,
    },
    curios_common::{Mount, Qualifier, SyntaxRegistry},
    curios_surface::{Name, TopItem, TopMod},
    std::{
        collections::{BTreeMap, HashMap},
        rc::Rc,
    },
};

impl PreparedText {
    /// Each declaration this unit keeps a record of that its record or its answers do not hold, spelled as a line: `predecessors` the units it was lowered over, `syntax` the registry it was lowered through. Empty where every lowering is held.
    ///
    /// - **The record names everything the doors read**, which holds a declaration kept unasked: each lookup of its trace, asked again where only what its record names stands — those labels of the unit's tables and its predecessors', and its body's scope at its place — answers as recorded.
    /// - **The lowering reads nothing of its scope but the doors' answers**, which holds a declaration kept by asking: lowered where only what its trace answered stands, each lookup reaching its answer through export entries alone and a method through its namespace's entry, it lowers to the items and mints the unit holds, a witness's name read as the item's own.
    pub fn census(&self, predecessors: &[&PreparedText], syntax: &SyntaxRegistry) -> Vec<String> {
        self.with_census(predecessors, syntax, |census| {
            self.lowerings()
                .iter()
                .flat_map(|(first, record)| census.departures(first, record))
                .collect()
        })
    }

    /// `then`, handed what the census of this unit reads every declaration against.
    pub(super) fn with_census<T>(
        &self,
        predecessors: &[&PreparedText],
        syntax: &SyntaxRegistry,
        then: impl FnOnce(&Census<'_>) -> T,
    ) -> T {
        let tables = predecessors
            .iter()
            .map(|unit| &unit.table)
            .collect::<Vec<_>>();
        let publics = predecessors
            .iter()
            .map(|unit| &unit.public)
            .collect::<Vec<_>>();
        let table = Scoped::from(&tables, self.table.clone());
        let public = Scoped::from(&publics, self.public.clone());
        let mounts = predecessors
            .iter()
            .flat_map(|unit| unit.mounts.iter().cloned())
            .chain(self.mounts.iter().cloned())
            .collect::<Vec<_>>();
        let reach = Reach::over(&mounts, &self.visible);
        let trees = self
            .scans()
            .iter()
            .map(|(module, scan)| {
                let tree = tree_of(&scan.source).expect("a text a unit scanned parses");
                (*module, Rc::clone(&scan.heads), tree)
            })
            .collect::<Vec<_>>();
        let shared = Shared::new();
        let mut sites = HashMap::new();
        for (module, heads, tree) in &trees {
            let context = within(
                Context::new(&table, &public, reach, &shared, syntax),
                module,
            );
            sited(&context, heads, &tree.items, &mut sites);
        }

        then(&Census {
            unit: self,
            table: &table,
            public: &public,
            mounts: &mounts,
            reach,
            syntax,
            sites,
        })
    }
}

/// What the census reads every declaration of a unit against: the unit, its tables over its predecessors', the mounts its compilation holds and the reach it was lowered with, the registry, and where each of its declarations is written.
pub(super) struct Census<'c> {
    unit: &'c PreparedText,
    table: &'c Scoped<'c, ModuleInfo>,
    public: &'c Scoped<'c, PublicInterface>,
    mounts: &'c [Mount],
    reach: Reach<'c>,
    syntax: &'c SyntaxRegistry,
    sites: HashMap<Key, Site<'c>>,
}

/// Where a declaration is written: the module body it stands in, its head and its item.
pub(super) struct Site<'t> {
    body: Qualifier,
    headed: &'t Headed,
    item: &'t TopItem,
}

impl Census<'_> {
    /// What the census finds of the declaration `record` describes, whose first item stands at `first`, each a line: a lookup its record does not hold, and a lowering its answers do not. Nothing for a declaration no later lowering may keep.
    pub(super) fn departures(&self, first: i64, record: &LoweredDeclaration) -> Vec<String> {
        let Some(site) = record.key.and_then(|key| self.sites.get(&key)) else {
            return Vec::new();
        };
        let at = usize::try_from(first).expect("a declaration's first item has a place");
        let names = self.unit.core().items.names(at);
        let declaration = names
            .first()
            .map(|name| name.faithful().to_string())
            .unwrap_or_default();

        let mut departures = self
            .unrecorded(record, site, names.first())
            .into_iter()
            .map(|lookup| {
                format!("{declaration}: `{lookup}` answers otherwise where only its record stands")
            })
            .collect::<Vec<_>>();
        if let Some(lowered) = self.unanswered(at, record, site) {
            departures.push(format!(
                "{declaration}: lowers otherwise where only its answers stand: {lowered}"
            ));
        }

        departures
    }

    /// Each lookup of `record`'s trace that answers otherwise where only what the record names stands, spelled as written.
    fn unrecorded(
        &self,
        record: &LoweredDeclaration,
        site: &Site<'_>,
        first: Option<&curios_core::Global>,
    ) -> Vec<String> {
        let mut table = BTreeMap::<Qualifier, ModuleInfo>::new();
        let mut public = BTreeMap::<Qualifier, PublicInterface>::new();
        let mut scope = Vec::new();
        for consulted in &record.consulted {
            match *consulted {
                Consulted::Member { at, binding } => {
                    let (parent, label) = (at.without_last(), at.last());
                    let held = Held::of(
                        self.table.get(&parent),
                        self.public.get(&parent),
                        label,
                        binding,
                    );
                    if let Some(declares) = held.declared {
                        table
                            .entry(parent)
                            .or_insert_with(ModuleInfo::new)
                            .holding(label, declares);
                    }
                    if let Some(entry) = held.exported {
                        let interface = public.entry(parent).or_insert_with(nothing_exported);
                        match binding {
                            true => interface.bindings.insert(label.to_string(), entry.clone()),
                            false => interface.children.insert(label.to_string(), entry.clone()),
                        };
                    }
                }
                Consulted::Members(module) => {
                    if let Some(interface) = self.public.get(&module) {
                        let held = public.entry(module).or_insert_with(nothing_exported);
                        held.children.extend(interface.children.clone());
                        held.bindings.extend(interface.bindings.clone());
                    }
                }
                Consulted::Scope { at, binding } => scope.push((at.last().to_string(), binding)),
                Consulted::Mount(_) => {}
            }
        }
        let body = self.body_scope(&site.body, first);
        let (table, public) = (
            Scoped::from(&[], held(table)),
            Scoped::from(&[], held(public)),
        );
        let shared = Shared::new();
        let mut context = within(
            Context::new(&table, &public, self.reach, &shared, self.syntax),
            &site.body,
        );
        for (label, binding) in scope {
            if let Some(&(target, imported)) = body.get(&(label.clone(), binding)) {
                context
                    .bound_as(&label, binding, target, imported)
                    .expect("a body binds a label once");
            }
        }

        record
            .trace
            .asked()
            .iter()
            .filter(|(lookup, found)| !context.answers(lookup, found))
            .map(|(lookup, _)| Name::new(lookup.absolute, lookup.path).join())
            .collect()
    }

    /// The scope of the module body `body` where the declaration whose first name is `first` is written: what the body declares, the imports its `use` declarations brought in before the declaration, and at the root, every mount's name; each label, a binding or a module, with its target and whether a `use` brought it in.
    fn body_scope(
        &self,
        body: &Qualifier,
        first: Option<&curios_core::Global>,
    ) -> HashMap<(String, bool), (Qualifier, bool)> {
        let mut scope = HashMap::new();
        if let Some(info) = self.unit.table.get(body) {
            for (label, _) in info.children() {
                scope.insert((label.to_string(), false), (body.with(label), false));
            }
            for (label, _) in info.bindings() {
                scope.insert((label.to_string(), true), (body.with(label), false));
            }
        }
        if body.is_root() {
            for mount in self.mounts {
                if let Some(label) = mount.prefix.segments().first() {
                    scope.insert((label.clone(), false), (mount.prefix, false));
                }
            }
        }
        let imports = self.unit.imports();
        let count = first
            .and_then(|name| imports.scoped(name))
            .map_or(0, |in_scope| {
                usize::try_from(in_scope.count).expect("a body's imports are counted in a usize")
            });
        for import in imports.body(body).iter().take(count) {
            match import {
                curios_core::Import::Binding { global, spelling } => {
                    if let Some(target) = global.qualifier() {
                        scope.insert((spelling.clone(), true), (*target, true));
                    }
                }
                curios_core::Import::Module { module, label } => {
                    scope.insert((label.clone(), false), (*module, true));
                }
            }
        }

        scope
    }

    /// What the declaration `record` describes, whose first item stands at `at`, lowers to where only what its trace answered stands, spelled, where that is not what the unit holds.
    fn unanswered(
        &self,
        at: usize,
        record: &LoweredDeclaration,
        site: &Site<'_>,
    ) -> Option<String> {
        let mut table = BTreeMap::<Qualifier, ModuleInfo>::new();
        let mut public = BTreeMap::<Qualifier, PublicInterface>::new();
        let mut bindings = BTreeMap::new();
        let mut qualifiers = BTreeMap::new();
        for (lookup, found) in record.trace.asked() {
            let target = found.target();
            let Some((last, steps)) = lookup.path.segments().split_last() else {
                continue;
            };
            if !lookup.absolute && steps.is_empty() {
                bindings.insert(last.clone(), target);
            } else {
                // A relative path's head stands for a module of the body's own, which no guard refuses.
                let (mut current, steps) = match (lookup.absolute, steps.split_first()) {
                    (false, Some((head, rest))) => {
                        let start = site.body.with(head);
                        qualifiers.insert(head.clone(), start);
                        (start, rest)
                    }
                    _ => (Qualifier::empty(), steps),
                };
                for step in steps {
                    let next = current.with(step);
                    public
                        .entry(current)
                        .or_insert_with(nothing_exported)
                        .children
                        .insert(step.clone(), exported(next));
                    current = next;
                }
                public
                    .entry(current)
                    .or_insert_with(nothing_exported)
                    .bindings
                    .insert(last.clone(), exported(target));
            }
            if let Found::Method(method) = found {
                let namespace = method.without_last();
                table
                    .entry(namespace.without_last())
                    .or_insert_with(ModuleInfo::new)
                    .holding(
                        namespace.last(),
                        Declares::Child(ChildInfo::ConceptMethods { vis_pub: true }),
                    );
            }
        }
        let (table, public) = (
            Scoped::from(&[], held(table)),
            Scoped::from(&[], held(public)),
        );
        let shared = Shared::new();
        let mut context = within(
            Context::new(&table, &public, self.reach, &shared, self.syntax),
            &site.body,
        );
        for (label, target) in qualifiers {
            context
                .bound_as(&label, false, target, false)
                .expect("a label bound once");
        }
        for (label, target) in bindings {
            context
                .bound_as(&label, true, target, false)
                .expect("a label bound once");
        }

        context.begin_declaration();
        if let (Some(anchored), Some(span)) =
            (anchored(&site.headed.head, &context), &site.headed.own)
        {
            context.anchor(anchored, span);
        }
        let mut lowering = Lowering {
            tested: self.unit.compiled == Compiled::Tested,
            ..Lowering::default()
        };
        if let Err(error) = lower_declaration(site.item, &context, &mut lowering) {
            return Some(format!("refused: {error:?}"));
        }
        context.end_declaration(lowering.items.iter().flat_map(FlatItem::names));

        let items = &self.unit.core().items;
        let mut places = Vec::new();
        let mut place = Some(at);
        for _ in 0..record.items.len() {
            let at = place?;
            places.push(at);
            place = items.place_after(Some(at));
        }
        let held_names = places
            .iter()
            .flat_map(|place| items.names(*place))
            .collect::<Vec<_>>();
        // A witness is named by its signature once every item is lowered; one lowered alone keeps the name its place gives it.
        let witness = |name: &curios_core::Global| matches!(name, curios_core::Global::Witness(_));
        let renames = lowering
            .items
            .iter()
            .flat_map(FlatItem::names)
            .filter(witness)
            .zip(held_names.iter().copied().filter(witness))
            .filter(|(ours, theirs)| ours != theirs)
            .collect::<BTreeMap<_, _>>();
        rename_witnesses(
            Named {
                items: &mut lowering.items,
                minted: &mut shared.minted.borrow_mut(),
                imports: &mut shared.imports.borrow_mut(),
            },
            &renames,
        );

        let minted = shared.minted.borrow();
        let lowered = lowering
            .items
            .into_iter()
            .map(|item| item.into_core(&minted))
            .collect::<Vec<_>>();
        let same_items = lowered.len() == places.len()
            && lowered
                .iter()
                .zip(&places)
                .all(|(lowered, place)| lowered == self.unit.lowered_item(*place));
        let same_mints = held_names
            .iter()
            .all(|name| minted.of(name) == self.unit.minted().of(name));

        match (same_items, same_mints) {
            (true, true) => None,
            (false, _) => Some(format!("{lowered:?}")),
            (true, false) => Some("what it minted".to_string()),
        }
    }
}

/// The context of the module body `body`, nested from the compilation root's `root`.
fn within<'a>(root: Context<'a>, body: &Qualifier) -> Context<'a> {
    body.segments()
        .iter()
        .fold(root, |context, segment| context.nested(segment))
}

/// Each declaration `heads` declare in the body `context` stands in, whose items are `items`, by the text its lowering is found by, inline modules' among them.
fn sited<'t>(
    context: &Context<'_>,
    heads: &'t [Headed],
    items: &'t [TopItem],
    sites: &mut HashMap<Key, Site<'t>>,
) {
    let mut anchors = curios_core::Anchors::default();
    for (headed, item) in heads.iter().zip(items) {
        match (&headed.head, item) {
            (
                Head::Mod {
                    label,
                    inline: Some(inner),
                    ..
                },
                TopItem::Mod(TopMod {
                    module: Some(module),
                    ..
                }),
            ) => sited(&context.nested(label), inner, &module.items, sites),
            (Head::Mod { .. } | Head::Use(_) | Head::Broken { .. }, _) => {}
            (head, item) => {
                if let (Some(anchored), Some(span)) = (anchored(head, context), &headed.own) {
                    let key = anchors.site(curios_core::Anchor::of(anchored), span).key();
                    sites.insert(
                        key,
                        Site {
                            body: context.prefix(),
                            headed,
                            item,
                        },
                    );
                }
            }
        }
    }
}

/// A module's export view holding nothing.
fn nothing_exported() -> PublicInterface {
    PublicInterface {
        children: HashMap::new(),
        bindings: HashMap::new(),
    }
}

/// An export of `target`, exposing no representation.
fn exported(target: Qualifier) -> Entry {
    Entry {
        target,
        representation: None,
    }
}

/// `entries` as a table a scope layers.
fn held<V>(entries: BTreeMap<Qualifier, V>) -> Table<V> {
    let mut table = Table::new();
    for (module, entry) in entries {
        table.insert(module, Rc::new(entry));
    }

    table
}
