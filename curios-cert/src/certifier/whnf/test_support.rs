//! Fixtures the weak-head reduction suites share: the certifier they start from, the chains they spend budget on, and the arm they take equations under.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{Reducer, Term},
};

/// The certifier every test starts from.
pub fn certifier() -> Certifier<'static> {
    Certifier::new(1_000_000, SYNTAX)
}

/// What reducing `term` costs `certifier`, read off what its declaration has consumed on either side.
pub fn spent(certifier: &mut Certifier, term: Term) -> u64 {
    let before = certifier.consumed().units();
    certifier.reduce_forced(term).expect("reduces");
    let after = certifier.consumed().units();

    after - before
}

/// An arithmetic tree `links` deep over `tip`, as text: `#NatAdd(… #NatAdd(tip, 1) …, 1)`.
pub fn added(tip: &str, links: usize) -> String {
    [
        "#NatAdd(".repeat(links),
        tip.to_string(),
        ", 1)".repeat(links),
    ]
    .concat()
}

/// A closed arithmetic tree `links` deep. Local-free, so it is a term the `whnf`/`forced` tables may key on — and one the closed machine takes, at machine depth.
pub fn chain(links: usize) -> Term {
    term(&added("0", links))
}

/// The same tree over an open tip, a `Nat` assumed in `certifier`, which the closed machine's gate declines — the term that still exercises the recursive strategy and the depth it prices.
pub fn open_chain(certifier: &mut Certifier, links: usize) -> Term {
    let (_, chain) = opened(
        certifier,
        &fill!("(tip: #NatType) => ${chain}", chain = added("tip", links)),
    );

    chain
}

/// Reduce an open term and a closed one inside an arm refining a `Nat` `n` to `0`, then both again after the arm retracts: the two inside reducts followed by the two outside ones.
pub fn across_an_arm(certifier: &mut Certifier) -> [Term; 4] {
    let [n, open] = under(certifier, "n: #NatType", "n, #NatAdd(n, 1)");
    let closed = chain(8);

    let (inside_open, inside_closed) = certifier.scoped(|certifier| {
        certifier
            .refine(n, term("0"))
            .expect("the equation records");

        (
            certifier.reduce_forced(open.clone()).expect("reduces"),
            certifier.reduce_forced(closed.clone()).expect("reduces"),
        )
    });

    [
        inside_open,
        inside_closed,
        certifier.reduce_forced(open).expect("reduces"),
        certifier.reduce_forced(closed).expect("reduces"),
    ]
}

/// The strategy arm of the differential fixture below: the ordinary test certifier with its closed machine off, so every closed term is walked by the recursive strategy. Beside its one consumer on purpose — nothing else may evaluate with the machine disabled.
pub fn strategy_certifier() -> Certifier<'static> {
    let mut certifier = certifier();
    certifier.machine = false;
    certifier
}
