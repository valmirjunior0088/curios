use {
    super::{Consulted, Head, Headed, Loaded, ModuleInfo, PreparedText, Reach, Scoped, Table},
    crate::{Error, GroupItem, Name, OrderedMap, UseGroup},
    curios_utilities::{Mount, Persistent, Qualifier, RootKind, nearest},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        rc::Rc,
    },
};

// The export view of a module: public names only, each pointing at the canonical declaration site. Built to a fixed point before any body is elaborated.
#[derive(Clone, PartialEq)]
#[curios_archive::archived]
pub(crate) struct PublicInterface {
    #[archived_with(OrderedMap)]
    pub children: HashMap<String, Entry>,
    #[archived_with(OrderedMap)]
    pub bindings: HashMap<String, Entry>,
}

impl PublicInterface {
    fn new() -> Self {
        Self {
            children: HashMap::new(),
            bindings: HashMap::new(),
        }
    }
}

#[derive(Clone, PartialEq)]
#[curios_archive::archived]
pub(crate) struct Entry {
    pub target: Qualifier,
    /// The nominal declaration whose representation this export exposes. Kept distinct from `target` so re-exports cannot manufacture representation visibility and aliases can inherit it during the post-lowering audit.
    pub representation: Option<Qualifier>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Ns {
    Module,
    Binding,
}

// A `pub use` lifted out of the syntax tree, tagged with the module it lives in. Collected once; the fixed point reads only these plus the interface map.
struct PubUse {
    module: Qualifier,
    name: Name,
    group: UseGroup,
}

impl PubUse {
    // A refusal of this `pub use`, placed at the half of it the refusal is about: the selector it names, where this `use` wrote one of that label, and the path otherwise, which is all a glob writes.
    fn placed(&self, error: Error) -> Error {
        let label = match &error {
            Error::ExportConflict { label } | Error::CyclicReExport { label } => {
                Some(label.as_str())
            }
            // The step that failed is the selector, where it is the name's last.
            Error::Unresolved {
                failure,
                leaf: true,
            } => Some(failure.segment.as_str()),
            _ => None,
        };
        let selector = match (label, &self.group) {
            (Some(label), UseGroup::Named(items)) => items
                .iter()
                .map(GroupItem::label)
                .find(|item| item.as_str() == label)
                .and_then(|item| item.span()),
            _ => None,
        };

        match selector.or(self.name.span()) {
            Some(span) => error.at(span.clone()),
            None => error,
        }
    }
}

// === Visibility ==============================================================
//
// One rule governs both namespaces: a declaration written without `pub` in module `M` is visible exactly within `M`'s subtree, and a `pub` one is additionally visible wherever `M` itself is. Reachability is therefore the conjunction along a path, which the callers obtain by walking hop by hop.
//
// The public interface is consulted first, so a `pub` declaration and a re-export target resolve identically for every consumer. The subtree fallback covers only what the interface deliberately omits: the module's own non-`pub` declarations, visible to itself and its descendants.
//
// Globs are deliberately *not* relaxed — `resolvable` reads the public interface alone, so `use M/*` imports M's exported surface and `pub use M/*` can never widen a subtree-private item's audience. Reaching a non-`pub` declaration always requires naming it.

/// Who can see each declaration of one unit, as a set of subtree roots: a declaration is visible to consumer `C` when `C` lies within any of them.
///
/// The audience of a non-`pub` declaration in `M` is `M` itself. The audience of anything the interface graph exposes at `M` — a `pub` declaration or a re-export target — is `M`'s own audience, so `pub` inside a private module reaches exactly that module's audience and no further. Re-exports can expose one declaration at several unrelated points, which is why an audience is a set rather than a single qualifier.
///
/// Module audiences are a fixed point (a `pub use` chain may cycle, and cyclic module dependencies are supported); binding audiences read off the converged module map in one pass.
///
/// **A unit decides the audience of each name it declares, and of no other.** Its audiences are a function of its own direct interface and export view ([`Audiences::of`]), so they are made once for the unit and kept with it, and a unit lowered after it reads them as they stand ([`AudienceScope`]): no later unit widens one. None needs to: a unit reaches another's name through public hops from the compilation root alone, and its own unit already shows a name so reached to everyone, which every lowering asserts of its exports ([`AudienceScope::widens_nothing`]).
///
/// **Persistent, and revised by what changed** ([`Audiences::revised`]): a unit lowered over a baseline holds the baseline's audiences with those of the names its changed modules declare or export read again.
#[derive(Clone)]
pub(super) struct Audiences {
    modules: Persistent<Qualifier, Vec<Qualifier>>,
    bindings: Persistent<Qualifier, Vec<Qualifier>>,
    /// Each name one of this unit's exports shows at a path other than its own, with every such export: what a name under an internal root is seen through ([`AudienceScope::seen`]). Recorded beside the audiences, never folded into one: a name's own audience stays its declaring unit's.
    relayed: Persistent<Qualifier, Vec<Relay>>,
    /// Each module one of this unit's exports shows at a path other than its own, with every such export: a module exported shows its public members with it, as resolution walks into them, so a name under an internal root is seen through an export of a module it is a member of too ([`AudienceScope::seen`]). Read off the exported children alone, which [`Audiences::revised`] reads again whole where any moved.
    relayed_modules: Persistent<Qualifier, Vec<Relay>>,
    /// Each name the unit's export view exports a binding as, with every module and label it is exported under: what its audience and its relays are read again by.
    exports: Persistent<Qualifier, Vec<(Qualifier, String)>>,
}

/// One export of a name at a path other than its own: the path the export stands at, and who sees that path.
#[derive(Clone, PartialEq)]
pub(super) struct Relay {
    site: Qualifier,
    exposure: Vec<Qualifier>,
}

impl Audiences {
    /// The audiences of the unit whose direct interface is `table` and whose export view is `public`, each the unit's own layer.
    pub(super) fn of(table: &Table<ModuleInfo>, public: &Table<PublicInterface>) -> Self {
        curios_profile::profile!("audiences::of");
        // What the unit declares. The compilation root every unit seeds lists each mounted prefix as a child, an earlier unit's among them, and an export may name an earlier unit's declaration: neither is this unit's to decide.
        let declares_module = |module: &Qualifier| table.contains_key(module);
        let declares_binding = |binding: &Qualifier| {
            table
                .get(&binding.without_last())
                .is_some_and(|info| info.get_binding(binding.last()).is_some())
        };

        let mut modules: HashMap<Qualifier, Vec<Qualifier>> = HashMap::new();
        // The compilation root is visible to the whole program.
        modules.insert(Qualifier::empty(), vec![Qualifier::empty()]);

        for (module, info) in table.iter() {
            for (label, vis_pub) in info.children() {
                if !vis_pub {
                    widen(&mut modules, module.with(label), *module);
                }
            }
        }

        loop {
            let mut changed = false;
            for (module, interface) in public.iter() {
                let exposure = modules.get(module).cloned().unwrap_or_default();
                let exported = interface
                    .children
                    .values()
                    .filter(|entry| declares_module(&entry.target));
                for entry in exported {
                    for root in &exposure {
                        changed |= widen(&mut modules, entry.target, *root);
                    }
                }
            }

            if !changed {
                break;
            }
        }

        let mut bindings: HashMap<Qualifier, Vec<Qualifier>> = HashMap::new();

        for (module, info) in table.iter() {
            for (label, vis_pub) in info.bindings() {
                if !vis_pub {
                    widen(&mut bindings, module.with(label), *module);
                }
            }
        }

        let mut relayed: HashMap<Qualifier, Vec<Relay>> = HashMap::new();

        for (module, interface) in public.iter() {
            let exposure = modules.get(module).cloned().unwrap_or_default();
            #[expect(
                clippy::iter_over_hash_type,
                reason = "each name's relays are sorted below"
            )]
            for (label, entry) in &interface.bindings {
                let site = module.with(label);
                if site != entry.target {
                    relayed.entry(entry.target).or_default().push(Relay {
                        site,
                        exposure: exposure.clone(),
                    });
                }
                if !declares_binding(&entry.target) {
                    continue;
                }
                for root in &exposure {
                    widen(&mut bindings, entry.target, *root);
                }
            }
        }

        let mut relayed_modules: HashMap<Qualifier, Vec<Relay>> = HashMap::new();
        for (module, interface) in public.iter() {
            let exposure = modules.get(module).cloned().unwrap_or_default();
            #[expect(
                clippy::iter_over_hash_type,
                reason = "each module's relays are sorted below"
            )]
            for (label, entry) in &interface.children {
                let site = module.with(label);
                if site != entry.target {
                    relayed_modules
                        .entry(entry.target)
                        .or_default()
                        .push(Relay {
                            site,
                            exposure: exposure.clone(),
                        });
                }
            }
        }

        // An audience is a set, and the walks above reach its roots in a hash's order: listed by root, so the list is the same from every compilation.
        for audience in modules.values_mut().chain(bindings.values_mut()) {
            audience.sort();
        }
        relayed
            .values_mut()
            .chain(relayed_modules.values_mut())
            .for_each(|relays| relays.sort_by_key(|relay| relay.site));
        let mut exports = HashMap::<Qualifier, Vec<(Qualifier, String)>>::new();
        for (module, interface) in public.iter() {
            #[expect(
                clippy::iter_over_hash_type,
                reason = "each name's exports are looked up, never read in order"
            )]
            for (label, entry) in &interface.bindings {
                exports
                    .entry(entry.target)
                    .or_default()
                    .push((*module, label.clone()));
            }
        }

        Self {
            modules: persistent(modules),
            bindings: persistent(bindings),
            relayed: persistent(relayed),
            relayed_modules: persistent(relayed_modules),
            exports: persistent(exports),
        }
    }

    /// Each module this unit exports at a path other than its own, with who may write that path, in path order: what a report reaches the module's members through ([`spell()`](super::spell())), as the audit sees them through it ([`AudienceScope::seen`]).
    pub(super) fn module_exports(&self) -> Vec<curios_core::ModuleExport> {
        let mut exports = self
            .relayed_modules
            .iter()
            .flat_map(|(module, relays)| {
                relays.iter().map(|relay| curios_core::ModuleExport {
                    module: *module,
                    at: curios_core::WritablePath {
                        path: relay.site,
                        audience: relay.exposure.clone(),
                    },
                })
            })
            .collect::<Vec<_>>();
        exports.sort_by_key(|export| export.at.path);
        exports
    }

    /// These audiences, of a unit whose tables were `earlier_table` and `earlier_public`, made those of `table` and `public`, which differ from them at `changed`'s modules alone. Every module's audience stands where no changed module declares or exports a child otherwise, and the audience and the relays of each name a changed module declares or exports, then or now, are read again; where a child moved, every audience is made again ([`Audiences::of`]). Beside them, the names whose audience or relays were read again, `None` where every one was.
    pub(super) fn revised(
        &self,
        earlier_table: &Table<ModuleInfo>,
        earlier_public: &Table<PublicInterface>,
        table: &Table<ModuleInfo>,
        public: &Table<PublicInterface>,
        changed: &BTreeSet<Qualifier>,
    ) -> (Self, Option<BTreeSet<Qualifier>>) {
        let children = |table: &Table<ModuleInfo>, module: &Qualifier| {
            table.get(module).map(|info| {
                info.children()
                    .map(|(label, child)| (label.to_string(), child))
                    .collect::<BTreeMap<_, _>>()
            })
        };
        let exported = |public: &Table<PublicInterface>, module: &Qualifier| {
            public.get(module).map(|interface| {
                interface
                    .children
                    .iter()
                    .map(|(label, entry)| (label.clone(), entry.clone()))
                    .collect::<BTreeMap<_, _>>()
            })
        };
        if changed.iter().any(|module| {
            children(earlier_table, module) != children(table, module)
                || exported(earlier_public, module) != exported(public, module)
        }) {
            return (Self::of(table, public), None);
        }

        let mut revised = self.clone();
        let mut names = BTreeSet::new();
        // Every export a changed module made is taken out of the index before any it makes now is put in.
        for (public, now) in [(earlier_public, false), (public, true)] {
            for module in changed {
                let Some(interface) = public.get(module) else {
                    continue;
                };
                #[expect(
                    clippy::iter_over_hash_type,
                    reason = "a name's exports are looked up, and its relays sorted where they are read again"
                )]
                for (label, entry) in &interface.bindings {
                    names.insert(entry.target);
                    let mut held = revised
                        .exports
                        .get(&entry.target)
                        .cloned()
                        .unwrap_or_default();
                    held.retain(|(held, held_label)| !(held == module && held_label == label));
                    if now {
                        held.push((*module, label.clone()));
                    }
                    match held.is_empty() {
                        true => {
                            revised.exports.remove(&entry.target);
                        }
                        false => revised.exports.insert(entry.target, held),
                    }
                }
            }
        }
        for table in [earlier_table, table] {
            for module in changed {
                if let Some(info) = table.get(module) {
                    names.extend(info.bindings().map(|(label, _)| module.with(label)));
                }
            }
        }
        for name in &names {
            revised.read_again(name, table);
        }

        (revised, Some(names))
    }

    /// The audience and the relays of `name` read again of `table` and the index of exports, every module's audience standing: as [`Audiences::of`] reads them.
    fn read_again(&mut self, name: &Qualifier, table: &Table<ModuleInfo>) {
        let module = name.without_last();
        let declared = table
            .get(&module)
            .and_then(|info| info.get_binding(name.last()));
        let mut audience = Vec::new();
        if declared == Some(false) {
            widen_one(&mut audience, module);
        }
        let mut relays = Vec::new();
        for (exporter, label) in self.exports.get(name).into_iter().flatten() {
            let exposure = self.modules.get(exporter).cloned().unwrap_or_default();
            let site = exporter.with(label);
            if site != *name {
                relays.push(Relay {
                    site,
                    exposure: exposure.clone(),
                });
            }
            if declared.is_some() {
                for root in exposure {
                    widen_one(&mut audience, root);
                }
            }
        }
        audience.sort();
        relays.sort_by_key(|relay| relay.site);
        match audience.is_empty() {
            true => {
                self.bindings.remove(name);
            }
            false => self.bindings.insert(*name, audience),
        }
        match relays.is_empty() {
            true => {
                self.relayed.remove(name);
            }
            false => self.relayed.insert(*name, relays),
        }
    }

    /// Whether `name` is seen in `other` as in these: by the same audience, through the same exports.
    pub(super) fn same_as(&self, other: &Audiences, name: &Qualifier) -> bool {
        self.bindings.get(name) == other.bindings.get(name)
            && self.relayed.get(name) == other.relayed.get(name)
    }

    /// Every module and label the unit's export view exports `name` as a binding under.
    pub(super) fn exported_at(&self, name: &Qualifier) -> Vec<(Qualifier, String)> {
        self.exports.get(name).cloned().unwrap_or_default()
    }

    /// The audience of the module `qualifier` names, one of the unit's. A namespace the interface never mentions (a synthetic one built during lowering) inherits its parent's audience.
    pub(super) fn module(&self, qualifier: &Qualifier) -> Vec<Qualifier> {
        match self.modules.get(qualifier) {
            Some(audience) => audience.clone(),
            None if qualifier.is_root() => vec![Qualifier::empty()],
            None => self.module(&qualifier.without_last()),
        }
    }

    /// The audience of the binding `qualifier` names, one of the unit's, falling back to its namespace for compiler-built bindings the interface never registered.
    pub(super) fn binding(&self, qualifier: &Qualifier) -> Vec<Qualifier> {
        match self.bindings.get(qualifier) {
            Some(audience) => audience.clone(),
            None => self.module(&qualifier.without_last()),
        }
    }

    /// Whether everything that can see `audience` can also see `referent`: every root of the exposed audience must lie within some root of the referent's. An empty referent audience is nobody, so nothing covers it.
    pub(super) fn covers(audience: &[Qualifier], referent: &[Qualifier]) -> bool {
        audience
            .iter()
            .all(|root| referent.iter().any(|reach| root.is_within(reach)))
    }
}

/// Each module above `name` from which a walk down `name`'s own labels through `public` reaches it — every hop a public child, the last a public binding standing for `name` — beside those labels: what an export of that module shows of `name` ([`AudienceScope::seen`]).
fn shown_through(
    public: &Table<PublicInterface>,
    name: &Qualifier,
) -> Vec<(Qualifier, Vec<String>)> {
    let segments = name.segments();
    (1..segments.len())
        .filter_map(|depth| {
            let module = segments[..depth]
                .iter()
                .fold(Qualifier::empty(), |module, label| module.with(label));
            let below = &segments[depth..];
            let (last, hops) = below.split_last()?;
            let mut at = module;
            for hop in hops {
                at = public.get(&at)?.children.get(hop)?.target;
            }
            (public.get(&at)?.bindings.get(last)?.target == *name).then(|| (module, below.to_vec()))
        })
        .collect()
}

/// Who can see each declaration in a unit's scope: the audiences the unit decided, over the ones each unit lowered before it decided for itself.
///
/// A *scope*, not a merged map. A name is asked of the unit that declares it, found by the prefix it lies under, so an earlier unit's audiences are made only where a lowering asks about one of that unit's names.
#[derive(Clone, Copy)]
pub(super) struct AudienceScope<'a> {
    own: &'a Audiences,
    /// The unit's own prefixes, and the mounts it may name — its own among them.
    own_mounts: &'a [Mount],
    own_visible: &'a [Mount],
    /// The units already lowered, in dependency order.
    earlier: &'a [&'a PreparedText],
}

impl<'a> AudienceScope<'a> {
    pub(super) fn over(
        own: &'a Audiences,
        own_mounts: &'a [Mount],
        own_visible: &'a [Mount],
        earlier: &'a [&'a PreparedText],
    ) -> Self {
        Self {
            own,
            own_mounts,
            own_visible,
            earlier,
        }
    }

    /// Who can see the binding `qualifier` names where `item`'s signature names it, beside the paths of the exports that show it, which a refusal reads the keeping module off. A name is seen by its own audience, except one under an internal root named outside that root: no reader but a unit that may name the root reaches it by its own path, so it is seen within those units and wherever an export shows it, whichever unit's export that is. An item under the root shares its readers, so what it names is seen as its own unit decided.
    pub(super) fn seen(
        &self,
        qualifier: &Qualifier,
        item: &Qualifier,
    ) -> (Vec<Qualifier>, Vec<Qualifier>) {
        let mut mounts = self
            .own_mounts
            .iter()
            .chain(self.earlier.iter().flat_map(|unit| &unit.mounts));
        let Some(root) = mounts
            .find(|mount| mount.kind == RootKind::Internal && qualifier.is_within(&mount.prefix))
            .map(|mount| mount.prefix)
            .filter(|root| !item.is_within(root))
        else {
            return (self.binding(qualifier), Vec::new());
        };

        // A module an export shows carries its public members with it, as resolution walks into them: the modules above the name its declaring unit's export view reaches it from.
        let carried = self
            .declaring(qualifier)
            .map_or_else(Vec::new, |unit| shown_through(&unit.public, qualifier));
        let units = self
            .earlier
            .iter()
            .map(|unit| {
                (
                    unit.mounts.as_slice(),
                    unit.visible.as_slice(),
                    unit.audiences(),
                )
            })
            .chain([(self.own_mounts, self.own_visible, self.own)]);
        let (mut audience, mut sites) = (Vec::new(), Vec::new());
        for (mounts, visible, audiences) in units {
            if visible.iter().any(|mount| mount.prefix == root) {
                audience.extend(
                    mounts
                        .iter()
                        .filter(|mount| !mount.prefix.is_root())
                        .map(|mount| mount.prefix),
                );
            }
            // An export inside the root shows the name to nobody the root does not.
            let relays = audiences.relayed.get(qualifier).into_iter().flatten();
            for relay in relays.filter(|relay| !relay.site.is_within(&root)) {
                audience.extend(relay.exposure.iter().copied());
                sites.push(relay.site);
            }
            for (module, below) in &carried {
                let relays = audiences.relayed_modules.get(module).into_iter().flatten();
                for relay in relays.filter(|relay| !relay.site.is_within(&root)) {
                    audience.extend(relay.exposure.iter().copied());
                    sites.push(
                        below
                            .iter()
                            .fold(relay.site, |site, label| site.with(label)),
                    );
                }
            }
        }

        (audience, sites)
    }

    /// The earlier unit that declares `qualifier`, where one does: the one it lies under a prefix of. The compilation root is every unit's, so a name under no earlier unit's prefix is the unit's own.
    fn declaring(&self, qualifier: &Qualifier) -> Option<&'a PreparedText> {
        self.earlier.iter().copied().find(|unit| {
            unit.mounts
                .iter()
                .any(|mount| !mount.prefix.is_root() && qualifier.is_within(&mount.prefix))
        })
    }

    /// The audience of the module `qualifier` names — see [`Audiences::module`].
    pub(super) fn module(&self, qualifier: &Qualifier) -> Vec<Qualifier> {
        match self.declaring(qualifier) {
            Some(unit) => unit.audiences().module(qualifier),
            None => self.own.module(qualifier),
        }
    }

    /// The audience of the binding `qualifier` names — see [`Audiences::binding`].
    pub(super) fn binding(&self, qualifier: &Qualifier) -> Vec<Qualifier> {
        match self.declaring(qualifier) {
            Some(unit) => unit.audiences().binding(qualifier),
            None => self.own.binding(qualifier),
        }
    }

    /// Whether no export of `public`, the unit's own export view, shows an earlier unit's name to anyone its own unit does not: what lets every unit decide its audiences alone.
    pub(super) fn widens_nothing(&self, public: &Table<PublicInterface>) -> bool {
        public.iter().all(|(module, interface)| {
            let exposure = self.own.module(module);
            let children = interface.children.values().all(|entry| {
                self.declaring(&entry.target).is_none_or(|unit| {
                    Audiences::covers(&exposure, &unit.audiences().module(&entry.target))
                })
            });
            let bindings = interface.bindings.values().all(|entry| {
                self.declaring(&entry.target).is_none_or(|unit| {
                    Audiences::covers(&exposure, &unit.audiences().binding(&entry.target))
                })
            });

            children && bindings
        })
    }
}

/// `map`, persistent.
fn persistent<V: Clone>(map: HashMap<Qualifier, V>) -> Persistent<Qualifier, V> {
    let mut persistent = Persistent::new();
    #[expect(
        clippy::iter_over_hash_type,
        reason = "a persistent map holds the same entries whatever order they arrive in"
    )]
    for (key, value) in map {
        persistent.insert(key, value);
    }

    persistent
}

/// [`widen`] for one audience in hand.
fn widen_one(audience: &mut Vec<Qualifier>, root: Qualifier) {
    if audience.iter().any(|existing| root.is_within(existing)) {
        return;
    }
    audience.retain(|existing| !existing.is_within(&root));
    audience.push(root);
}

// Add `root` to `key`'s audience unless an existing root already contains it, dropping any it subsumes so the set stays an antichain and the fixed point terminates. Returns whether the audience grew.
fn widen(
    audiences: &mut HashMap<Qualifier, Vec<Qualifier>>,
    key: Qualifier,
    root: Qualifier,
) -> bool {
    let audience = audiences.entry(key).or_default();

    if audience.iter().any(|existing| root.is_within(existing)) {
        return false;
    }

    audience.retain(|existing| !existing.is_within(&root));
    audience.push(root);
    true
}

/// The target of `parent`'s child module `label` as seen from `consumer`, or `None` when it is absent or out of view.
pub(crate) fn visible_child(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    consumer: &Qualifier,
    parent: &Qualifier,
    label: &str,
) -> Option<Qualifier> {
    if let Some(entry) = public.get(parent).and_then(|i| i.children.get(label)) {
        return Some(entry.target);
    }

    let within = consumer.is_within(parent);
    let declared = table
        .get(parent)
        .and_then(|info| info.get_child(label))
        .is_some();

    (within && declared).then(|| parent.with(label))
}

/// The target of `parent`'s binding `label` as seen from `consumer`, or `None` when it is absent or out of view.
pub(crate) fn visible_binding(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    consumer: &Qualifier,
    parent: &Qualifier,
    label: &str,
) -> Option<Qualifier> {
    if let Some(entry) = public.get(parent).and_then(|i| i.bindings.get(label)) {
        return Some(entry.target);
    }

    let within = consumer.is_within(parent);
    let declared = table
        .get(parent)
        .and_then(|info| info.get_binding(label))
        .is_some();

    (within && declared).then(|| parent.with(label))
}

/// Why `consumer` cannot step from `parent` to `label`, sought as `sought`: the one value every resolution that stops in a module raises, the lowering's and the export view's alike. Private where `parent` declares the label out of the consumer's view, since a consumer inside `parent` would have been answered; absent otherwise. The nearest name is chosen among what the consumer may write there: what `parent` exports, and what it declares where the consumer lies within it.
pub(super) fn unresolved(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    consumer: &Qualifier,
    parent: &Qualifier,
    label: &str,
    sought: curios_core::Sought,
) -> curios_core::Unresolved {
    let (modules, bindings) = match sought {
        curios_core::Sought::Module => (true, false),
        curios_core::Sought::Binding | curios_core::Sought::Structure => (false, true),
        curios_core::Sought::Either => (true, true),
    };
    let declared = table.get(parent);
    let exported = public.get(parent);

    let hidden = |held: Option<bool>| held == Some(false);
    let private = declared.is_some_and(|info| {
        (modules && hidden(info.get_child(label))) || (bindings && hidden(info.get_binding(label)))
    });

    let own = declared.filter(|_| consumer.is_within(parent));
    let children = exported
        .into_iter()
        .flat_map(|interface| interface.children.keys().map(String::as_str))
        .chain(
            own.into_iter()
                .flat_map(|info| info.children().map(|(name, _)| name)),
        )
        .filter(|_| modules);
    let members = exported
        .into_iter()
        .flat_map(|interface| interface.bindings.keys().map(String::as_str))
        .chain(
            own.into_iter()
                .flat_map(|info| info.bindings().map(|(name, _)| name)),
        )
        .filter(|_| bindings);

    curios_core::Unresolved {
        segment: label.to_string(),
        sought,
        within: Some(*parent),
        missing: match private {
            true => curios_core::Missing::Private,
            false => curios_core::Missing::Absent,
        },
        nearest: nearest(label, children.chain(members)).map(str::to_string),
        elsewhere: Vec::new(),
    }
}

/// Every public interface in scope that carries `label` in the namespace `sought` names, as the absolute path the label has there: the paths a refusal offers for a name that resolved to nothing. Found by the label alone — a module and a declaration of one name are two things, and nothing here pairs a path with another that exports the same declaration. Read off the public interfaces, where a `pub use` is a binding of the re-exporting module, rather than the declaration table, because the path a reader may write is the exporting module's: `/std/Bool` is `pub use Bool/{let Bool}`, and Core knows only the `/sys/Bool/Bool` it stands for. Only the shortest paths are kept — a longer one is the same name through a deeper module — in the path's own order, which is deterministic and nothing more.
///
/// A candidate `consumer` may not write is dropped first — one under a root the unit may not name, or one through a module it cannot see — because a report that offered one would spell a way out the next compile refuses. Dropping before the shortest paths are kept rather than after is what makes the rule reachability instead of depth: `/sys/Nat` stands beside `/std/Nat` at the same length, and a reachable route below an internal one is still worth offering.
pub(super) fn carriers(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    consumer: &Qualifier,
    label: &str,
    sought: curios_core::Sought,
) -> Vec<Qualifier> {
    let (modules, bindings) = match sought {
        curios_core::Sought::Module => (true, false),
        curios_core::Sought::Binding | curios_core::Sought::Structure => (false, true),
        curios_core::Sought::Either => (true, true),
    };

    let mut found = public
        .iter()
        .filter(|(_, interface)| {
            (modules && interface.children.contains_key(label))
                || (bindings && interface.bindings.contains_key(label))
        })
        .filter(|(module, _)| reachable(public, table, consumer, module))
        .map(|(module, _)| module.with(label))
        .filter(|path| reach.refused(path.segments()).is_none())
        .collect::<Vec<_>>();
    found.sort_by_key(|path| (path.segments().len(), path.join()));
    let shortest = found.first().map(|path| path.segments().len());
    found.retain(|path| Some(path.segments().len()) == shortest);
    found
}

/// Whether `consumer` may walk to `module` from the compilation root, one step at a time: each a child its parent shows the consumer, by its export or by the consumer standing within the parent.
fn reachable(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    consumer: &Qualifier,
    module: &Qualifier,
) -> bool {
    let mut parent = Qualifier::empty();

    module.segments().iter().all(|segment| {
        match visible_child(public, table, consumer, &parent, segment) {
            Some(child) => {
                parent = child;
                true
            }
            None => false,
        }
    })
}

/// The refusal of a reach into a root the unit may not name: the root of `resolved` as the step that failed, for the reason `missing` gives, with every path `consumer` may write that carries `wanted`, the name it reached for under that root and the namespace it sought it in.
pub(super) fn out_of_reach(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    consumer: &Qualifier,
    resolved: &[String],
    missing: curios_core::Missing,
    wanted: Option<(&str, curios_core::Sought)>,
) -> curios_core::Unresolved {
    curios_core::Unresolved {
        segment: resolved.first().cloned().unwrap_or_default(),
        sought: curios_core::Sought::Module,
        within: Some(Qualifier::empty()),
        missing,
        nearest: None,
        elsewhere: match wanted {
            Some((label, sought)) => carriers(public, table, reach, consumer, label, sought),
            None => Vec::new(),
        },
    }
}

/// Seed this unit's interfaces over its scope's — constructor modules included, which are also added to `table`, the direct-interface view, so lowering (`process_items`) can classify private-vs-missing accesses through them — then resolve every `pub use` to a fixed point. Runs after discovery has built `table` and before any item is lowered.
///
/// **One resolution for every unit.** What differs is which items are seeded at which prefix: the entry's at the empty qualifier, a mounted unit's under each prefix it claims — with the synthetic compilation root seeded empty in that case, since absolute references resolve through it even though it has no source items of its own. Each prefix takes an explicit `seed` call, since `seed`'s own recursion follows only the `TopItem::Mod` items it is handed.
pub(super) fn resolve_unit<'a>(
    root: &[Headed],
    own: &[Mount],
    modules: &HashMap<Qualifier, Rc<Loaded>>,
    table: &mut Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    scope: Scoped<'a, PublicInterface>,
) -> Result<Scoped<'a, PublicInterface>, Error> {
    curios_profile::profile!("resolve_unit");
    let mut public = scope;
    let mut pub_uses = Vec::new();

    // The compilation root: the entry's own items, or — for a unit that carries no entrypoint — a root belonging to no unit, seeded empty because absolute references resolve through it regardless, its public children being the mounted prefixes.
    seed(
        root,
        &Qualifier::empty(),
        modules,
        table,
        &mut public,
        &mut pub_uses,
    )?;

    for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
        let content = modules
            .get(&mount.prefix)
            .expect("a mounted prefix was loaded during discovery");
        seed(
            &content.heads,
            &mount.prefix,
            modules,
            table,
            &mut public,
            &mut pub_uses,
        )?;
    }

    fixed_point(&mut public, table, reach, &pub_uses)?;
    classify_dead(&public, table, reach, &pub_uses)?;
    Ok(public)
}

/// [`resolve_unit`] over the export view of `baseline`, the unit an earlier text of the same sources lowered to, `public` holding it as the unit's own half: every module whose text `touched` holds is seeded again, with every module whose `use` declarations read a binding that moved and every one after them, until nothing more moved; every other module's view is the baseline's. `removed` holds every module discovery took away. Beside the view, the modules seeded again, the bindings that moved and the modules whose tables are not the baseline's.
///
/// **A view seeded again is the one a whole resolution makes**, since a module's view is its own seed and what its `pub use` declarations select, and a module none of whose selections read a moved binding selects what it did.
#[expect(
    clippy::too_many_arguments,
    reason = "the tables resolution reads, and the baseline with what discovery found moved"
)]
pub(super) fn resolve_unit_over<'a>(
    root: &[Headed],
    modules: &HashMap<Qualifier, Rc<Loaded>>,
    order: &HashMap<Qualifier, usize>,
    table: &mut Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    mut public: Scoped<'a, PublicInterface>,
    baseline: &PreparedText,
    touched: &HashSet<Qualifier>,
    removed: &[Qualifier],
) -> Result<(Scoped<'a, PublicInterface>, Resolution), Error> {
    curios_profile::profile!("resolve_unit_over");
    for module in removed {
        public.remove(module);
    }
    let readers = baseline.lookup_readers();
    let in_order = |modules: &mut Vec<Qualifier>| {
        modules.sort_by_key(|module| (order.get(module).copied(), *module));
    };

    let mut affected = HashSet::new();
    let mut seeded = BTreeSet::new();
    let mut pub_uses = Vec::new();
    let mut moved = HashSet::new();
    let mut changed;
    let mut pending = touched.iter().copied().collect::<Vec<_>>();
    loop {
        in_order(&mut pending);
        for module in pending.drain(..) {
            if !affected.insert(module) {
                continue;
            }
            if let Some(heads) = heads_of(&module, root, modules) {
                seed_module(
                    heads,
                    &module,
                    table,
                    &mut public,
                    &mut pub_uses,
                    &mut seeded,
                )?;
            }
        }
        // A whole resolution meets the `pub use` declarations in the order of their modules.
        pub_uses.sort_by_key(|use_| order.get(&use_.module).copied());
        fixed_point(&mut public, table, reach, &pub_uses)?;

        let found;
        (found, changed) = moved_between(
            baseline,
            table.own(),
            public.own(),
            seeded.iter().chain(removed),
        );
        pending = found
            .iter()
            .filter(|consulted| !moved.contains(*consulted))
            .flat_map(|consulted| readers.of_body(consulted))
            .filter(|module| !affected.contains(*module))
            .copied()
            .collect();
        moved.extend(found);
        if pending.is_empty() {
            break;
        }
    }
    classify_dead(&public, table, reach, &pub_uses)?;

    Ok((
        public,
        Resolution {
            affected,
            moved,
            changed,
        },
    ))
}

/// What resolving a unit's export view over a baseline's found ([`resolve_unit_over`]): the modules it seeded again, and the bindings whose entries are not the baseline's ([`Consulted`]).
pub(super) struct Resolution {
    pub affected: HashSet<Qualifier>,
    pub moved: HashSet<Consulted>,
    pub changed: BTreeSet<Qualifier>,
}

/// What `module`'s items declare: the compilation root's are `root`, a file module's its file's, and an inline module's its parent's for it. `None` for a namespace a declaration makes, which its module seeds.
fn heads_of<'h>(
    module: &Qualifier,
    root: &'h [Headed],
    modules: &'h HashMap<Qualifier, Rc<Loaded>>,
) -> Option<&'h [Headed]> {
    if module.is_root() {
        return Some(root);
    }
    if let Some(loaded) = modules.get(module) {
        return Some(&loaded.heads);
    }
    let parent = heads_of(&module.without_last(), root, modules)?;
    parent.iter().find_map(|headed| match &headed.head {
        Head::Mod {
            label,
            inline: Some(heads),
            ..
        } if label.as_str() == module.last() => Some(heads.as_slice()),
        _ => None,
    })
}

/// The bindings whose entries in `table` and `public`, the unit's own halves, are not `baseline`'s, among `modules`, beside the modules whose entries are not: what a resolution reads of a module, read off the labels each declares or exports, and the labels a glob reads.
///
/// **A binding moved where its entry did.** A step of a path reads one module's entry for one label, its export's target and representation and how the module declares it; a glob reads which labels a module exports. So the bindings that moved are read off the modules whose tables are not the baseline's, a label at a time.
pub(super) fn moved_between<'m>(
    baseline: &PreparedText,
    table: &Table<ModuleInfo>,
    public: &Table<PublicInterface>,
    modules: impl IntoIterator<Item = &'m Qualifier>,
) -> (HashSet<Consulted>, BTreeSet<Qualifier>) {
    let mut moved = HashSet::new();
    let mut changed = BTreeSet::new();
    for module in modules.into_iter().copied().collect::<BTreeSet<_>>() {
        let (table_now, table_then) = (table.get(&module), baseline.table.get(&module));
        let (public_now, public_then) = (public.get(&module), baseline.public.get(&module));
        if same(table_now, table_then) && same(public_now, public_then) {
            continue;
        }
        changed.insert(module);
        let (table_now, table_then) = (table_now.map(|now| &**now), table_then.map(|then| &**then));
        let (public_now, public_then) = (
            public_now.map(|now| &**now),
            public_then.map(|then| &**then),
        );
        let labels = |table: Option<&ModuleInfo>, public: Option<&PublicInterface>| {
            let children = table
                .into_iter()
                .flat_map(|info| info.children().map(|(label, _)| (label.to_string(), false)))
                .chain(public.into_iter().flat_map(|interface| {
                    interface
                        .children
                        .keys()
                        .map(|label| (label.clone(), false))
                }));
            let bindings = table
                .into_iter()
                .flat_map(|info| info.bindings().map(|(label, _)| (label.to_string(), true)))
                .chain(public.into_iter().flat_map(|interface| {
                    interface.bindings.keys().map(|label| (label.clone(), true))
                }));
            children.chain(bindings).collect::<BTreeSet<_>>()
        };
        let mut all = labels(table_now, public_now);
        all.extend(labels(table_then, public_then));
        for (label, binding) in all {
            let entry = |table: Option<&ModuleInfo>, public: Option<&PublicInterface>| {
                let exported = public.and_then(|interface| match binding {
                    true => interface.bindings.get(&label),
                    false => interface.children.get(&label),
                });
                let declared = table.and_then(|info| info.declares(&label, binding));
                (exported.cloned(), declared)
            };
            if entry(table_now, public_now) != entry(table_then, public_then) {
                moved.insert(Consulted::Member {
                    at: module.with(&label),
                    binding,
                });
            }
        }
        if exported(public_now) != exported(public_then) {
            moved.insert(Consulted::Members(module));
        }
    }

    (moved, changed)
}

/// The labels `public` exports, children and bindings: what a glob reads of a module.
fn exported(public: Option<&PublicInterface>) -> Option<(BTreeSet<&String>, BTreeSet<&String>)> {
    public.map(|interface| {
        (
            interface.children.keys().collect(),
            interface.bindings.keys().collect(),
        )
    })
}

/// Whether a module's entry now is the one the baseline holds: shared with it, or equal.
fn same<T: PartialEq>(now: Option<&Rc<T>>, then: Option<&Rc<T>>) -> bool {
    match (now, then) {
        (Some(now), Some(then)) => Rc::ptr_eq(now, then) || now == then,
        (None, None) => true,
        _ => false,
    }
}

// Walk the module tree (mirroring `discover`/`process_items`): for each module, seed its `PublicInterface` from the direct interface already in `table`; materialize each inductive's constructor module; and collect every `pub use`.
fn seed(
    heads: &[Headed],
    prefix: &Qualifier,
    modules: &HashMap<Qualifier, Rc<Loaded>>,
    table: &mut Scoped<'_, ModuleInfo>,
    public: &mut Scoped<'_, PublicInterface>,
    pub_uses: &mut Vec<PubUse>,
) -> Result<(), Error> {
    seed_module(heads, prefix, table, public, pub_uses, &mut BTreeSet::new())?;
    for headed in heads {
        if let Head::Mod { label, inline, .. } = &headed.head {
            let path = prefix.with(label);
            let child = match inline {
                Some(heads) => heads,
                None => {
                    &modules
                        .get(&path)
                        .expect("module loaded during discovery")
                        .heads
                }
            };

            seed(child, &path, modules, table, public, pub_uses)?;
        }
    }

    Ok(())
}

/// Seed the `PublicInterface` of the one module `prefix` from the direct interface already in `table`, materialize the namespace each of its inductives and concepts makes, and collect its `pub use` declarations, noting in `seeded` every module whose entries this wrote.
fn seed_module(
    heads: &[Headed],
    prefix: &Qualifier,
    table: &mut Scoped<'_, ModuleInfo>,
    public: &mut Scoped<'_, PublicInterface>,
    pub_uses: &mut Vec<PubUse>,
    seeded: &mut BTreeSet<Qualifier>,
) -> Result<(), Error> {
    seeded.insert(*prefix);
    let mut interface = PublicInterface::new();
    let info = table
        .get(prefix)
        .expect("module info present from discovery");

    for label in info.public_children() {
        let target = prefix.with(&label);
        interface.children.insert(
            label,
            Entry {
                target,
                representation: None,
            },
        );
    }

    for label in info.public_bindings() {
        let target = prefix.with(&label);
        interface.bindings.insert(
            label,
            Entry {
                target,
                representation: None,
            },
        );
    }

    // Attach declaration provenance to directly exposed nominal bindings. The fixed point copies this bit alongside the canonical target.
    {
        for headed in heads {
            match &headed.head {
                Head::Struct(group) => {
                    for item in group.iter().filter(|item| item.vis_pub && item.rep_pub) {
                        interface
                            .bindings
                            .get_mut(item.label.as_str())
                            .expect("direct struct binding")
                            .representation = Some(prefix.with(&item.label));
                    }
                }
                Head::Induct(group) => {
                    for item in group.iter().filter(|item| item.vis_pub && item.rep_pub) {
                        interface
                            .bindings
                            .get_mut(item.label.as_str())
                            .expect("direct inductive binding")
                            .representation = Some(prefix.with(&item.label));
                    }
                }
                Head::Concept(group) => {
                    for item in group.iter().filter(|item| item.vis_pub) {
                        interface
                            .bindings
                            .get_mut(item.label.as_str())
                            .expect("direct concept binding")
                            .representation = Some(prefix.with(&item.label));
                    }
                }
                _ => {}
            }
        }
    }
    public.insert(*prefix, interface);

    for headed in heads {
        match &headed.head {
            Head::Use(use_item) if use_item.vis_pub => {
                pub_uses.push(PubUse {
                    module: *prefix,
                    name: use_item.name.clone(),
                    group: use_item.group.clone(),
                });
            }
            Head::Induct(group) => {
                for induct in group {
                    let ctor = prefix.with(&induct.label);

                    // Constructor bindings are public within their synthetic namespace. The parent's child bit, seeded separately as `vis_pub && rep_pub`, gates all external walks while the declaring module retains direct access.
                    let mut direct = ModuleInfo::new();
                    for case in &induct.cases {
                        direct.insert_binding(case, true)?;
                    }
                    table.insert(ctor, direct);
                    seeded.insert(ctor);

                    let mut interface = PublicInterface::new();
                    for case in &induct.cases {
                        let target = ctor.with(case);
                        interface.bindings.insert(
                            case.to_string(),
                            Entry {
                                target,
                                representation: None,
                            },
                        );
                    }
                    public.insert(ctor, interface);
                }
            }
            Head::Concept(group) => {
                for concept in group {
                    // A concept's method wrappers live in a nested namespace, exactly like an inductive's constructors: seed both the direct info and the public interface of that module unconditionally (the fields are always public within it), so `Show/show` resolves. The concept's own visibility gates the walk from outside via the parent's child-module flag.
                    let namespace = prefix.with(&concept.label);

                    let mut direct = ModuleInfo::new();
                    // A superclass edge has no label — a positional slot with no name to reach it by, and no wrapper (`into_core` leaves it out of wrapper generation the same way) — so only the methods declare a binding.
                    for label in &concept.fields {
                        direct.insert_binding(label, true)?;
                    }
                    table.insert(namespace, direct);
                    seeded.insert(namespace);

                    let mut interface = PublicInterface::new();
                    for label in &concept.fields {
                        let target = namespace.with(label);
                        interface.bindings.insert(
                            label.to_string(),
                            Entry {
                                target,
                                representation: None,
                            },
                        );
                    }
                    public.insert(namespace, interface);
                }
            }
            _ => {}
        }
    }

    Ok(())
}

// Repeatedly resolve every `pub use` against the current interface graph, inserting whatever is resolvable, until a full round adds nothing.
fn fixed_point(
    public: &mut Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    pub_uses: &[PubUse],
) -> Result<(), Error> {
    loop {
        let mut changed = false;

        for use_ in pub_uses {
            for (ns, label, target, representation) in resolvable(public, table, reach, use_) {
                let entry = Entry {
                    target,
                    representation,
                };
                changed |= insert(public, &use_.module, ns, label, entry)
                    .map_err(|error| use_.placed(error))?;
            }
        }

        if !changed {
            break;
        }
    }

    Ok(())
}

// The selectors of one `pub use` that resolve against the interfaces *as they currently stand*. Anything not yet present is skipped (deferred to a later round). Never errors — dead entries are classified after the fixed point.
fn resolvable(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    use_: &PubUse,
) -> Vec<(Ns, String, Qualifier, Option<Qualifier>)> {
    let Some(provider) = provider(public, table, reach, &use_.module, &use_.name) else {
        return Vec::new();
    };

    let Some(interface) = public.get(&provider) else {
        return Vec::new();
    };

    let mut out = Vec::new();

    match &use_.group {
        UseGroup::Glob => {
            // By label, so the selector a round refuses first is the first the names give.
            let mut children = interface.children.iter().collect::<Vec<_>>();
            children.sort_by(|left, right| left.0.cmp(right.0));
            for (label, entry) in children {
                out.push((Ns::Module, label.clone(), entry.target, None));
            }
            let mut bindings = interface.bindings.iter().collect::<Vec<_>>();
            bindings.sort_by(|left, right| left.0.cmp(right.0));
            for (label, entry) in bindings {
                out.push((
                    Ns::Binding,
                    label.clone(),
                    entry.target,
                    entry.representation,
                ));
            }
        }
        UseGroup::Named(items) => {
            for item in items {
                match item {
                    GroupItem::Mod(label) => {
                        if let Some(entry) = interface.children.get(label.as_str()) {
                            out.push((Ns::Module, label.to_string(), entry.target, None));
                        }
                    }
                    GroupItem::Let(label) => {
                        if let Some(entry) = interface.bindings.get(label.as_str()) {
                            out.push((
                                Ns::Binding,
                                label.to_string(),
                                entry.target,
                                entry.representation,
                            ));
                        }
                    }
                    GroupItem::Both(label) => {
                        if let Some(entry) = interface.children.get(label.as_str()) {
                            out.push((Ns::Module, label.to_string(), entry.target, None));
                        }
                        if let Some(entry) = interface.bindings.get(label.as_str()) {
                            out.push((
                                Ns::Binding,
                                label.to_string(),
                                entry.target,
                                entry.representation,
                            ));
                        }
                    }
                }
            }
        }
    }

    out
}

// The `Option` view of `resolve_provider`, for callers where non-resolution is benign: a selector that does not resolve *yet* during the fixed point, or a chain hop that simply does not exist. The terminal `classify_dead` pass calls `resolve_provider` directly to surface the precise error instead.
fn provider(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    module: &Qualifier,
    name: &Name,
) -> Option<Qualifier> {
    resolve_provider(public, table, reach, module, name).ok()
}

// Insert one resolved entry into a slot. Returns whether the map changed.
fn insert(
    public: &mut Scoped<'_, PublicInterface>,
    module: &Qualifier,
    ns: Ns,
    label: String,
    entry: Entry,
) -> Result<bool, Error> {
    let slots = public.get(module).expect("seeded module");
    let slot = match ns {
        Ns::Module => &slots.children,
        Ns::Binding => &slots.bindings,
    };

    // Conflict is about *what* a slot exports, not which `pub use` claimed it: two selectors (a glob and a named item, or two paths through different re-export chains) that land on the same declaration agree, so re-deriving one is idempotent. Only genuinely divergent targets are ambiguous. Keeping the first entry preserves its representation provenance, which is derived from the target and therefore identical across the agreeing paths.
    match slot.get(&label) {
        Some(existing) if existing.target == entry.target => Ok(false),
        Some(_) => Err(Error::ExportConflict { label }),
        None => {
            public.update(module, |interface| {
                match ns {
                    Ns::Module => &mut interface.children,
                    Ns::Binding => &mut interface.bindings,
                }
                .insert(label, entry);
            });
            Ok(true)
        }
    }
}

// After the fixed point converges, any selector still resolving to nothing is an error, classified by following its re-export chain: a chain that returns to a slot already seen is a cyclic re-export, otherwise the target is missing.
fn classify_dead(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    pub_uses: &[PubUse],
) -> Result<(), Error> {
    for use_ in pub_uses {
        let provider = resolve_provider(public, table, reach, &use_.module, &use_.name)
            .map_err(|error| use_.placed(error))?;

        let interface = public.get(&provider).expect("seeded module");

        match &use_.group {
            // A glob with a reachable provider always resolves (possibly to no labels); only an unreachable source is an error, handled above.
            UseGroup::Glob => {}
            UseGroup::Named(items) => {
                for item in items {
                    let (label, in_module, in_binding) = match item {
                        GroupItem::Mod(label) => (label, true, false),
                        GroupItem::Let(label) => (label, false, true),
                        GroupItem::Both(label) => (label, true, true),
                    };

                    let module_ok = !in_module || interface.children.contains_key(label.as_str());
                    let binding_ok = !in_binding || interface.bindings.contains_key(label.as_str());

                    // `{x}` resolves if either namespace filled; `{mod x}` / `{let x}` require their own namespace.
                    let resolved = match item {
                        GroupItem::Both(_) => {
                            interface.children.contains_key(label.as_str())
                                || interface.bindings.contains_key(label.as_str())
                        }
                        GroupItem::Mod(_) => module_ok,
                        GroupItem::Let(_) => binding_ok,
                    };

                    if !resolved {
                        let ns = if in_binding { Ns::Binding } else { Ns::Module };
                        let cyclic = cycles(public, table, reach, pub_uses, &provider, ns, label);
                        let sought = match item {
                            GroupItem::Mod(_) => curios_core::Sought::Module,
                            GroupItem::Let(_) => curios_core::Sought::Binding,
                            GroupItem::Both(_) => curios_core::Sought::Either,
                        };
                        let error = match cyclic {
                            true => Error::CyclicReExport {
                                label: label.to_string(),
                            },
                            false => Error::unresolved(
                                unresolved(public, table, &use_.module, &provider, label, sought),
                                true,
                            ),
                        };

                        return Err(use_.placed(error));
                    }
                }
            }
        }
    }

    Ok(())
}

// Walk the re-export chain for an unresolved `(module, ns, label)` to decide whether it is a cycle or a genuine miss: whether the chain comes back to a module it has passed.
fn cycles(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    pub_uses: &[PubUse],
    module: &Qualifier,
    ns: Ns,
    label: &str,
) -> bool {
    let mut visited = HashSet::new();
    let mut current = *module;

    loop {
        if !visited.insert(current) {
            return true;
        }

        match producer(public, table, reach, pub_uses, &current, ns, label) {
            Some(next) => current = next,
            None => return false,
        }
    }
}

// The provider module of a `pub use` in `module` that would supply `label` in namespace `ns`, if any (named selector match, or a glob whose source is reachable). Used only by chain classification.
fn producer(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    pub_uses: &[PubUse],
    module: &Qualifier,
    ns: Ns,
    label: &str,
) -> Option<Qualifier> {
    for use_ in pub_uses {
        if &use_.module != module {
            continue;
        }

        let names = match &use_.group {
            UseGroup::Glob => true,
            UseGroup::Named(items) => items.iter().any(|item| match item {
                GroupItem::Mod(l) => matches!(ns, Ns::Module) && l == label,
                GroupItem::Let(l) => matches!(ns, Ns::Binding) && l == label,
                GroupItem::Both(l) => l == label,
            }),
        };

        if names && let Some(provider) = provider(public, table, reach, module, &use_.name) {
            return Some(provider);
        }
    }

    None
}

// Walk a `use` source path to its provider module, following re-export targets. A relative path's first segment may be the current module's own child of any visibility (you are inside it, so its privacy does not apply to itself); every later segment, and every segment of an absolute path, must be a public child. Each resolved hop is guarded so a consumer that did not declare it cannot follow a re-export into an internal root (`sys`) by any spelling. On failure, returns the precise error at the offending segment, using the direct-interface table to tell private from absent; `provider` is the `Option` view for callers where that is benign.
fn resolve_provider(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    reach: Reach<'_>,
    module: &Qualifier,
    name: &Name,
) -> Result<Qualifier, Error> {
    let segments = name.qualifier().segments();
    // The refusal of a step this module may not take from `parent`.
    let step = |parent: &Qualifier, segment: &str| {
        let sought = curios_core::Sought::Module;

        Error::unresolved(
            unresolved(public, table, module, parent, segment, sought),
            false,
        )
    };
    // Refuse a hop into a root this unit may not name, `next` being the segment the path steps to from there.
    let reachable = |reached: &Qualifier, next: Option<&String>| {
        let Some(missing) = reach.refused(reached.segments()) else {
            return Ok(());
        };
        let wanted = reached
            .segments()
            .get(1)
            .or(next)
            .map(|wanted| (wanted.as_str(), curios_core::Sought::Module));

        Err(Error::unresolved(
            out_of_reach(
                public,
                table,
                reach,
                module,
                reached.segments(),
                missing,
                wanted,
            ),
            false,
        ))
    };

    let (mut current, walk) = if name.is_abs() {
        (Qualifier::empty(), segments)
    } else {
        let first = &segments[0];

        // An opaque constructor namespace is never in the public interface, and re-exporting one would widen its audience past the subtree that owns the representation — so it is refused before the subtree fallback can offer it.
        if public
            .get(module)
            .and_then(|i| i.children.get(first))
            .is_none()
            && table
                .get(module)
                .is_some_and(|info| info.is_opaque_constructor_child(first))
        {
            return Err(Error::OpaqueConstructorsCannotBeReExported {
                induct_decl: module.with(first).join(),
            });
        }

        let start = visible_child(public, table, module, module, first)
            .ok_or_else(|| step(module, first))?;

        (start, &segments[1..])
    };

    reachable(&current, walk.first())?;
    for (index, segment) in walk.iter().enumerate() {
        match visible_child(public, table, module, &current, segment) {
            Some(target) => current = target,
            None => return Err(step(&current, segment)),
        }
        reachable(&current, walk.get(index + 1))?;
    }

    Ok(current)
}
