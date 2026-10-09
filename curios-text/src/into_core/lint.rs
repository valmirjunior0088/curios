//! The `unused-declaration` lint: a declaration nothing reaches from the unit's roots.
//!
//! Reachability rather than a reference count, so a private definition used only by itself, or only by another dead one, is dead. The roots are what the unit hands out or runs: every binding of a module reachable through `pub` children from a mount the unit claims — read off the public interfaces, so a re-export out of a private child counts as the facade it is — every test, every witness, and the entrypoint tail. The edges are the ones the dependency order already walks: a definition's free names, plus what its registry entry mentions. A constructor or a method wrapper is reached through its owner's namespace and reaches its owner through its own type, so neither is reported and neither is a root of its own.
//!
//! A private module none of whose declarations is reachable is reported once, at the `mod`, and its members are not: the fix is one deletion, and one line says so.

use {
    super::{FlatItem, LoweredItem, ModuleInfo, PublicInterface, Scoped, owner_of},
    crate::Lint,
    curios_utilities::{Mount, Qualifier, Span},
    std::collections::{BTreeSet, HashMap, HashSet},
};

/// What the walk is asked over: the unit's items and the tables the roots and the report are read from.
pub(super) struct Declarations<'a> {
    pub items: &'a [FlatItem],
    /// What was taken of each item where it was lowered — see [`LoweredItem`].
    pub facts: &'a [&'a LoweredItem],
    /// The span an extent of one of the unit's declarations lies on.
    pub placed: &'a dyn Fn(&curios_core::Extent) -> Option<Span>,
    pub entry: Option<&'a curios_core::Entrypoint>,
    pub table: &'a Scoped<'a, ModuleInfo>,
    pub public: &'a Scoped<'a, PublicInterface>,
    /// The prefixes this unit claims — where the walk over public modules starts.
    pub own: &'a [Mount],
    /// Where each `mod` the unit declares was written, by the module it declares.
    pub mod_spans: &'a HashMap<Qualifier, Span>,
}

/// Every `unused-declaration` lint of the unit, in item order; the caller sorts.
pub(super) fn unused_declarations(declarations: &Declarations<'_>) -> Vec<Lint> {
    curios_profile::profile!("unused_declarations");
    let Declarations {
        items,
        facts,
        placed,
        entry,
        table,
        public,
        own,
        mod_spans,
    } = declarations;

    let nodes = (0..items.len()).collect::<Vec<_>>();
    let owner = owner_of(items, &nodes);

    let mut reached = HashSet::new();
    let mut pending = Vec::new();
    let root = |global: &curios_core::Global, pending: &mut Vec<usize>| {
        if let Some(&node) = owner.get(global) {
            pending.push(node);
        }
    };

    for target in exported_bindings(public, own) {
        root(&curios_core::Global::Authored(target), &mut pending);
    }
    for (node, item) in items.iter().enumerate() {
        if item.declared().any(|(_, kind, _)| {
            matches!(
                kind,
                curios_core::DefinitionKind::Witness | curios_core::DefinitionKind::Test
            )
        }) {
            pending.push(node);
        }
    }
    if let Some(entry) = entry {
        let terms = std::iter::once(&entry.body).chain(entry.type_.as_ref());
        for global in terms.flat_map(|term| term.free_vars_shared().iter()) {
            if let Some(global) = global.as_global() {
                root(global, &mut pending);
            }
        }
    }

    while let Some(node) = pending.pop() {
        if !reached.insert(node) {
            continue;
        }
        for name in &facts[node].reaches {
            root(name, &mut pending);
        }
    }

    // A declaration the reader wrote, by the name it was written under and where that name stands: what the lint may report. Kept by a `_` prefix on its own name or on a module above it, exactly as a binder is.
    let dead = items
        .iter()
        .zip(facts.iter())
        .enumerate()
        .filter(|(node, _)| !reached.contains(node))
        .flat_map(|(_, (item, lowered))| item.declared().zip(&lowered.definitions))
        .filter(|((_, kind, _), _)| is_reportable(kind))
        .filter_map(|((name, _, _), declared)| Some((name, declared.label.as_ref()?)))
        .filter(|(name, _)| !name.qualifier().is_some_and(kept))
        .collect::<Vec<_>>();
    let live = items
        .iter()
        .enumerate()
        .filter(|(node, _)| reached.contains(node))
        .flat_map(|(_, item)| item.declared())
        .filter(|(_, kind, _)| is_reportable(kind))
        .filter_map(|(name, _, _)| name.qualifier().cloned())
        .collect::<Vec<_>>();

    // A private module every reportable declaration of which is dead is reported once, and its subtree is folded into it. Ancestors first, so a dead module inside a dead module is folded too.
    let mut dead_modules = BTreeSet::new();
    let mut modules = mod_spans.keys().collect::<Vec<_>>();
    modules.sort_by_key(|module| module.segments().len());
    for module in modules {
        if kept(module) {
            continue;
        }
        let private = table
            .get(&module.without_last())
            .and_then(|parent| parent.get_child(module.last()))
            == Some(false);
        let within = |qualifier: &Qualifier| qualifier.is_within(module);
        let has_dead = dead
            .iter()
            .filter_map(|(name, _)| name.qualifier())
            .any(within);
        let has_live = live.iter().any(within);
        let folded = dead_modules
            .iter()
            .any(|dead: &Qualifier| module.is_within(dead));
        if private && has_dead && !has_live && !folded {
            dead_modules.insert(*module);
        }
    }

    let mut lints = dead_modules
        .iter()
        .map(|module| Lint::unused_declaration(module.last(), mod_spans[module].clone()))
        .collect::<Vec<_>>();
    lints.extend(
        dead.iter()
            .filter(|(name, _)| {
                !name
                    .qualifier()
                    .is_some_and(|name| dead_modules.iter().any(|dead| name.is_within(dead)))
            })
            .map(|(name, label)| {
                Lint::unused_declaration(
                    name.qualifier().map_or("", Qualifier::last),
                    placed(label).expect("a name lies in the text its declaration was sited at"),
                )
            }),
    );
    lints
}

/// Whether a `_` prefix keeps the declaration at `name`: on its own segment, or on any module above it, since keeping a module keeps what it holds.
fn kept(name: &Qualifier) -> bool {
    name.iter().any(|segment| segment.starts_with('_'))
}

/// Whether a definition is one the reader declared under its own name: a `let`, a `foreign`, or the type former of an `induct`, `struct` or `concept`. A constructor, a method wrapper, a witness and a test are reached through their owner or are roots, and are never reported.
fn is_reportable(kind: &curios_core::DefinitionKind) -> bool {
    matches!(
        kind,
        curios_core::DefinitionKind::Authored | curios_core::DefinitionKind::Nominal(_)
    )
}

/// The target of every binding a consumer outside the unit can reach: those of every module reachable from a claimed prefix through public children, re-exports included, since that is what the public interfaces record.
fn exported_bindings(public: &Scoped<'_, PublicInterface>, own: &[Mount]) -> Vec<Qualifier> {
    let mut seen = HashSet::new();
    let mut pending = own.iter().map(|mount| mount.prefix).collect::<Vec<_>>();
    let mut targets = Vec::new();

    while let Some(module) = pending.pop() {
        if !seen.insert(module) {
            continue;
        }
        let Some(interface) = public.own().get(&module) else {
            continue;
        };
        targets.extend(interface.bindings.values().map(|entry| entry.target));
        pending.extend(interface.children.values().map(|entry| entry.target));
    }

    targets
}
