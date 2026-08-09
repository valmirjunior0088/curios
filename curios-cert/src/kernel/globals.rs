//! What is in scope beyond the walk: top-level definitions, and the nominal registry.
//!
//! This is the one component that exists to answer with something other than the term in hand, which is exactly what a definition store is for. Everything about it is therefore stated rather than assumed: a definition unfolds through a bare occurrence only when it is monomorphic, the registry is data no judgment in this crate types, and overwriting a name is the single event that invalidates a remembered reduct.
//!
//! # Two ways to reach a declaration, and they are not the same question
//!
//! [`Globals::induct_decl`] and [`Globals::struct_decl`] hand back the raw entry. That is correct for the *shared analyses* — positivity walks a declaration's telescopes, inversion reads its `result_sort` — because they analyze the declaration itself and there is no occurrence involved.
//!
//! It is **not** the way a typing rule should reach one. A rule consulting a declaration *for an occurrence* is asking what that occurrence is, and the answer is only meaningful once the occurrence's universe instance and its parameter and index counts have been checked against what the declaration declares. Reading the raw entry and opening its arity at whatever the occurrence carried is how `Telescope::open` came to abort the walk instead of refusing the item. Those rules go through the checked handle instead.
//!
//! # It is also what a walk starts from
//!
//! [`Globals::of`] builds this from a whole module, which is how an already-certified module reaches a later walk: its definitions at their declared types with their real bodies, its nominal registry, and the two bounds a walk cannot re-derive cheaply. A caller that has one hands it to [`recheck_module_verdicts`](crate::recheck_module_verdicts) and the walk judges what it does not already answer for, by name — the environment is a set of names, so nothing about it identifies an item by where it sits.

use {
    curios_core::{Free, Global, InductDecl, Item, Module, StructDecl, Term, UniverseContext},
    std::collections::{HashMap, HashSet},
};

/// A top-level name's entry: what it is, and what it unfolds to if anything.
///
/// The universe context is not decoration. A definition with universe parameters is *not* unfoldable through a bare occurrence, because such an occurrence denotes no particular instance; it reduces only through a [`UniverseInst`](curios_core::UniverseInst) that says which one.
#[derive(Clone)]
struct Definition {
    type_: Term,
    /// `None` for something with a type and no body — a `foreign` declaration, or a name deliberately kept opaque.
    value: Option<Term>,
    universes: UniverseContext,
}

#[derive(Default, Clone)]
pub struct Globals {
    definitions: HashMap<Free, Definition>,
    inducts: HashMap<Global, InductDecl>,
    structs: HashMap<Global, StructDecl>,
    /// Concept names only. No judgment in this crate reads a concept's resolution metadata, so what is held is exactly what [`Globals::in_scope`] needs to answer for the namespace — the one query that had no home when it lived on a prefix descriptor.
    concepts: HashSet<Global>,
    /// One above the highest binder index every term in scope here mentions, as derived by the walk that established this environment.
    ///
    /// Carried rather than re-derived because it is a constant of that walk, and re-deriving it means traversing every term in scope on every later walk. A floor is a bound rather than a verdict, so a caller combines it with its own by maximum and can only ever widen.
    binder_floor: usize,
}

impl Globals {
    /// Everything `module` puts in scope: its definitions at their declared types with their real bodies, and its nominal registry.
    ///
    /// `carried` is the binder floor derived by the build that established `module` — `curios_core::derived_binder_floor` over exactly it — which is why this does not walk the terms again.
    ///
    /// A definition enters here exactly as a refused item enters a walk's environment: at its declared type, with its real body, unjudged. That is deliberate and it is the whole meaning of this type — an environment records what is in scope, and whether the recording is warranted is the caller's question, answered before it ever built one.
    pub fn of(module: &Module, carried: usize) -> Self {
        let mut definitions = HashMap::new();
        let mut record = |name: Free, type_: &Term, value: &Term, universes: &UniverseContext| {
            // Written straight in rather than through `insert`: a fresh environment has no memos behind it, so the overwrite that method reports has nothing to invalidate.
            definitions.insert(
                name,
                Definition {
                    type_: type_.clone(),
                    value: Some(value.clone()),
                    universes: universes.clone(),
                },
            );
        };

        for item in &module.items {
            match item {
                Item::Let(definition) => record(
                    Free::from(&definition.name),
                    &definition.type_,
                    &definition.body,
                    &definition.universe_context,
                ),
                Item::Rec(rec) => {
                    let universes = rec.group.universe_context().clone();
                    for (member, name) in item.declared_names().into_iter().enumerate() {
                        record(
                            Free::from(name),
                            &rec.group.member_type(member),
                            &Term::rec_proj(rec.group.clone(), member),
                            &universes,
                        );
                    }
                }
            }
        }

        Self {
            definitions,
            inducts: module.induct_decls.clone().into_iter().collect(),
            structs: module.struct_decls.clone().into_iter().collect(),
            concepts: module.concepts.keys().cloned().collect(),
            binder_floor: carried,
        }
    }

    /// Whether `name` is already in scope here, in any of the namespaces this holds.
    ///
    /// One predicate across four namespaces, because a `Global` names one top-level thing within a module and every namespace here is populated from the same module. That is what lets a walk decide what to judge by identity: an item whose declared names this answers for was judged by whichever walk built this environment.
    pub fn in_scope(&self, name: &Global) -> bool {
        self.definitions.contains_key(&Free::from(name))
            || self.inducts.contains_key(name)
            || self.structs.contains_key(name)
            || self.concepts.contains(name)
    }

    /// The floor below which every binder identity in scope here was minted. See the field.
    pub fn binder_floor(&self) -> usize {
        self.binder_floor
    }

    /// Record `name` at `type_`, generalized over `universes`, with `value` as its body where it has one.
    ///
    /// Reports whether it *overwrote* an existing entry, which is the one event that invalidates a remembered reduct. The caller owns that coupling because the memos are a sibling component; returning the fact rather than acting on it is what keeps this one unable to forget it silently.
    #[must_use = "an overwrite invalidates every remembered reduct"]
    pub(super) fn insert(
        &mut self,
        name: &Free,
        type_: &Term,
        value: Option<&Term>,
        universes: &UniverseContext,
    ) -> bool {
        self.definitions
            .insert(
                name.clone(),
                Definition {
                    type_: type_.clone(),
                    value: value.cloned(),
                    universes: universes.clone(),
                },
            )
            .is_some()
    }

    pub(super) fn declare_induct(&mut self, name: &Global, declaration: &InductDecl) {
        self.inducts.insert(name.clone(), declaration.clone());
    }

    pub(super) fn declare_struct(&mut self, name: &Global, declaration: &StructDecl) {
        self.structs.insert(name.clone(), declaration.clone());
    }

    /// An `induct` registry entry, as data. See the module documentation on why a typing rule wants the checked handle instead.
    pub(super) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.inducts.get(name)
    }

    /// A `struct` registry entry, as data. See the module documentation on why a typing rule wants the checked handle instead.
    pub(super) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.structs.get(name)
    }

    /// The type `name` was declared at.
    pub(super) fn type_of(&self, name: &Free) -> Option<&Term> {
        self.definitions.get(name).map(|entry| &entry.type_)
    }

    /// The universe scheme `name` was generalized under, for a use that states its own instance.
    pub(super) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        self.definitions
            .get(name)
            .map(|entry| (&entry.type_, &entry.universes))
    }

    /// What `name` unfolds to through a bare occurrence.
    ///
    /// A definition with universe parameters is withheld: see [`Definition`].
    pub(super) fn value(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .filter(|definition| definition.universes.parameter_count == 0)
            .and_then(|definition| definition.value.as_ref())
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one position a polymorphic definition may be unfolded from.
    pub(super) fn value_at(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .and_then(|definition| definition.value.as_ref())
    }
}
