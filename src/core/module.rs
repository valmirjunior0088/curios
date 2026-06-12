use {
    super::{Inductive, Term},
    std::collections::BTreeMap,
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
    pub type_: Option<Term>,
    pub body: Term,
}

#[cfg(test)]
impl Module {
    /// Re-fold the flat module into the legacy nested `Let`/`Rec` `Term` it
    /// replaced (items are already in binding order). Test-only: lets the
    /// `to_core`/`erase` suites keep asserting against the historical shape — and
    /// keep feeding a single `Term` to `erase` — without rewriting every
    /// expectation. Drops `type_` (the old `run` helper only returned the term).
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
