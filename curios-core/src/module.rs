use {
    super::{Concept, Inductive, Qualifier, Structure, Term},
    curios_abi::RootId,
    std::collections::{BTreeMap, BTreeSet},
};

/// A single top-level definition: `name` bound to `body` of declared `type_`.
///
/// Unlike a local `Subterm::Let`, the binder is *not* a de Bruijn scope: every
/// top-level cross-reference stays a free `Var` keyed by `name`. The passes
/// `define` each one into the `Context` as they go, so `reduce`/`convert`
/// delta-reduce through them — exactly the named global signature the kernel
/// already maintained behind the (now removed) nested spine (§9, BUG.md).
#[derive(Debug, Clone, PartialEq)]
pub struct Definition {
    pub name: String,
    /// This definition's declaring module — `name`'s qualifier prefix,
    /// precomputed once by `to_core` (before `name` was flattened) rather
    /// than re-derived from it later. Stamped into `Context::island` per item
    /// by `elaborate_module`/`erase` for the struct representation-privacy
    /// check (§7); the same value `Structure::module` carries for type
    /// declarations.
    pub island: Qualifier,
    /// This definition's declaring root — `island`'s leading segment,
    /// precomputed once by `to_core` the same way `Concept`/`Structure`/
    /// `Inductive` are, so `Context::set_island` (and, downstream, the
    /// orphan-rule check) never has to re-derive it from `island` itself.
    pub root: RootId,
    pub type_: Term,
    pub body: Term,
}

/// A top-level item: a single `let` definition, or a `rec` group of
/// mutually-recursive definitions (which may reference each other by `name`).
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Let(Definition),
    Rec(Vec<Definition>),
}

/// The whole program as a *flat* list of top-level `items`, the entrypoint
/// `body`, and its optional `type_` annotation.
///
/// This replaces the single, N-deep nested `Subterm::Let`/`Rec` term that
/// `text::to_core` used to fold the entire prelude into — the construction
/// (`Scope::close` over the whole accumulator at each step) and every pass that
/// recursed along its `.tail` spine were both O(N) in stack and overflowed at
/// prelude depth (BUG.md). `Subterm::Let`/`Rec` remain for genuine *local*,
/// in-expression bindings, which are shallow.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<Item>,
    /// Inductive declarations' registry entries, keyed by the type's qualified
    /// name. Carried on the module — not on a `Context` — because elaboration
    /// and erasure each run with their *own* `Context` (see `run::compile`);
    /// both seed their context's flat inductive store from here on entry.
    pub inductives: BTreeMap<String, Inductive>,
    /// Struct declarations' registry entries, keyed by the type's qualified
    /// name. Carried on the module like `inductives` (and for the same reason):
    /// elaboration and erasure each seed their own `Context` from here on entry.
    pub structures: BTreeMap<String, Structure>,
    /// Concept declarations' resolution metadata, keyed by the concept's
    /// qualified name (each concept's record shape also lives in
    /// `structures`). Seeded into the elaboration `Context` on entry; erasure
    /// never consults it.
    pub concepts: BTreeMap<String, Concept>,
    /// The definition names that are witness declarations. Elaboration
    /// registers each into the witness table when its signature elaborates —
    /// carried as names (not keys) because the table key needs the
    /// *elaborated* head, which only exists once elaboration runs.
    pub witnesses: BTreeSet<String>,
    pub type_: Option<Term>,
    pub body: Term,
}

impl Module {
    /// Re-fold the flat module into the legacy nested `Let`/`Rec` `Term` it
    /// replaced (items are already in binding order). Test-only: lets the
    /// `to_core`/`erase` suites keep asserting against the historical shape — and
    /// keep feeding a single `Term` to `erase` — without rewriting every
    /// expectation. Drops `type_` (the old `run` helper only returned the term).
    /// Not `#[cfg(test)]`: its callers live in `curios`'s test suite, a
    /// different crate, where that cfg would never activate.
    pub fn into_nested_term(self) -> Term {
        self.items
            .into_iter()
            .rev()
            .fold(self.body, |acc, item| match item {
                Item::Let(def) => Term::let_(def.name, def.type_, def.body, acc),
                Item::Rec(defs) => Term::rec(
                    defs.into_iter().map(|def| (def.name, def.type_, def.body)),
                    acc,
                ),
            })
    }
}
