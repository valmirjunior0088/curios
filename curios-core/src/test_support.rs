//! Re-folding a finished [`Module`] into the nested term it replaced, for suites written against the older shape.
//!
//! A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: `curios_core::test_support::into_nested_term(module)` says at its use site that the caller reached for scaffolding rather than product API, which a `Module::into_nested_term` method sitting beside `nominal_plicities` would not. The path is the warning label.
//!
//! **Behind `test-support`, not `#[cfg(test)]`.** The caller is `curios-text`'s lowering suite, a different crate, and that cfg is set only while *this* crate is its own test harness — so a `cfg(test)` item would be invisible to it. The gate is also what keeps a shape no compiler stage produces any more out of every build that ships.

use crate::{Free, Item, Module, Term};

/// Re-fold the flat module into the legacy nested `Let`/`Rec` [`Term`] it replaced (items are already in binding order).
///
/// Lets the `into_core` suite keep asserting against the historical shape — and keep feeding a single [`Term`] to `erase` — without rewriting every expectation. Drops `type_`, because the old `run` helper only returned the term.
///
/// # Panics
///
/// If the module carries no entrypoint body, since there would be nothing to fold the items around.
pub fn into_nested_term(module: Module) -> Term {
    let body = module
        .body
        .expect("into_nested_term is for a module with an entrypoint");

    module
        .items
        .into_iter()
        .rev()
        .fold(body, |acc, item| match item {
            Item::Let(def) => Term::let_(&Free::from(&def.name), def.type_, def.body, acc),
            Item::Rec(rec) => Term::rec(
                rec.definitions()
                    .into_iter()
                    .map(|def| (Free::from(&def.name), def.type_, def.body)),
                acc,
            ),
        })
}
