//! What is in scope beyond the walk: top-level definitions, and the nominal registry.
//!
//! This is the one component that exists to answer with something other than the term in hand, which is exactly what a definition store is for. Everything about it is therefore stated rather than assumed: a definition unfolds through a bare occurrence only when it is monomorphic, the registry is data no judgment in this crate types, and overwriting a name is the single event that invalidates a remembered reduct.
//!
//! # Two ways to reach a declaration, and they are not the same question
//!
//! [`Globals::induct_decl`] and [`Globals::struct_decl`] hand back the raw entry. That is correct for the *shared analyses* — positivity walks a declaration's telescopes, inversion reads its `result_sort` — because they analyze the declaration itself and there is no occurrence involved.
//!
//! It is **not** the way a typing rule should reach one. A rule consulting a declaration *for an occurrence* is asking what that occurrence is, and the answer is only meaningful once the occurrence's universe instance and its parameter and index counts have been checked against what the declaration declares. Reading the raw entry and opening its arity at whatever the occurrence carried is how `Telescope::open` came to abort the walk instead of refusing the item. Those rules go through the checked handle instead.

use {
    curios_core::{Free, Global, InductDecl, StructDecl, Term, UniverseContext},
    std::collections::HashMap,
};

/// A top-level name's entry: what it is, and what it unfolds to if anything.
///
/// The universe context is not decoration. A definition with universe parameters is *not* unfoldable through a bare occurrence, because such an occurrence denotes no particular instance; it reduces only through a [`UniverseInst`](curios_core::UniverseInst) that says which one.
struct Definition {
    type_: Term,
    /// `None` for something with a type and no body — a `foreign` declaration, or a name deliberately kept opaque.
    value: Option<Term>,
    universes: UniverseContext,
}

#[derive(Default)]
pub(super) struct Globals {
    definitions: HashMap<Free, Definition>,
    inducts: HashMap<Global, InductDecl>,
    structs: HashMap<Global, StructDecl>,
    /// Whether a definition's body fails to fix a value — `fixes_no_value`'s memo, kept here because it is a derived fact about *these* definitions and so is invalidated by exactly the event that invalidates a remembered reduct.
    effects: HashMap<Free, bool>,
}

impl Globals {
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

    /// Where `fixes_no_value` remembers what a definition's body reaches and calls. Cleared by the same overwrite that invalidates a reduct, since a replaced body is a different closure.
    pub(super) fn effects_mut(&mut self) -> &mut HashMap<Free, bool> {
        &mut self.effects
    }

    /// Forget every remembered purity answer, for an overwrite that may have changed any of them.
    pub(super) fn forget_effects(&mut self) {
        self.effects.clear();
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one position a polymorphic definition may be unfolded from.
    pub(super) fn value_at(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .and_then(|definition| definition.value.as_ref())
    }
}
