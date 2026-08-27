//! Fixtures the term suites share: a name, distinct nodes, and the deep spines that must not recurse natively.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `term`, and nothing outside it.

use {
    crate::*,
    curios_utilities::Qualifier,
    std::{collections::HashSet, rc::Rc},
};

/// A declaration's name, from the path a test writes. Fixture-only.
pub(super) fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// The number of distinct `Node`s reachable from `term`, counting a shared node once. Inlined here because only these tests ask the question.
pub(super) fn distinct_nodes(term: &Term) -> usize {
    let mut seen = HashSet::new();
    let mut stack = Vec::from([term.clone()]);
    while let Some(node) = stack.pop() {
        if !seen.insert(Rc::as_ptr(&node.inner)) {
            continue;
        }
        node.as_ref().any_child_term(&mut |child| {
            stack.push(child.clone());
            false
        });
    }
    seen.len()
}

/// Deeper than the ~50,000 steps at which a growing conversion used to abort the process, so a regression is a stack overflow rather than a slow test.
pub(super) const DEEP: u32 = 100_000;

/// A left-nested application spine `((x a) a) …`, `DEEP` links tall.
pub(super) fn deep_spine(seed: u32) -> Term {
    let argument = Term::free_var(&Free::local(seed, None));
    let mut term = Term::free_var(&Free::local(seed, None));
    for _ in 0..DEEP {
        term = Term::apply(term, [argument.clone()]);
    }
    term
}

/// Past one 32 MiB stack segment several times over, which is what proves a walk can chain another rather than merely start on one. `DEEP` would prove the same thing at twenty segments; this asks for four.
pub(super) const TALL: u32 = 20_000;
