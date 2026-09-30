//! Re-folding a finished [`Program`] into one nested term, for the suites that assert against that shape.
//!
//! A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: `curios_core::test_support::into_nested_term(module)` says at its use site that the caller reached for scaffolding rather than product API, which a `Module::into_nested_term` method sitting beside `nominal_plicities` would not. The path is the warning label.
//!
//! **Behind `test-support`, not `#[cfg(test)]`.** The caller is `curios-text`'s lowering suite, a different crate, and that cfg is set only while *this* crate is its own test harness — so a `cfg(test)` item would be invisible to it. The gate is also what keeps a shape no compiler stage produces out of every build that ships.

use crate::{Free, Item, Program, Term};

/// Re-fold the program's flat module into a nested `Let`/`Rec` [`Term`] around the entry's body (items are already in binding order).
///
/// Lets the `into_core` suite assert against a single term. Drops the entry's type, which that suite's `run` helper does not return.
pub fn into_nested_term(program: Program) -> Term {
    program
        .module
        .items
        .into_iter()
        .rev()
        .fold(program.entry.body, |acc, item| match item {
            Item::Let(def) => Term::let_(&Free::from(&def.name), def.type_, def.body, acc),
            Item::Rec(rec) => Term::rec(
                rec.definitions()
                    .into_iter()
                    .map(|def| (Free::from(&def.name), def.type_, def.body)),
                acc,
            ),
        })
}
