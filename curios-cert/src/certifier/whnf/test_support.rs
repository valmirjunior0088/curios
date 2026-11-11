//! Fixtures the weak-head reduction suites share: the certifier they start from, the chains they spend budget on, and the arm they take equations under.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::*,
    curios_analysis::test_support::*,
    curios_core::{Free, Intrinsic, Nat, Reducer, Term, UniverseContext},
};

/// The certifier every test starts from.
pub fn certifier() -> Certifier<'static> {
    Certifier::new(1_000_000, SYNTAX)
}

/// A test binder. Once assumed it is one the certifier mints nothing over ([`Certifier::assume`]), so a binder it opens for eta-contraction cannot alias it.
pub fn binder(index: u32, hint: &str) -> Free {
    Free::local(u64::from(index), Some(hint))
}

pub fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

pub fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

pub fn monomorphic() -> UniverseContext {
    UniverseContext::default()
}

pub fn polymorphic() -> UniverseContext {
    UniverseContext {
        parameter_count: 1,
        ..Default::default()
    }
}

/// What reducing `term` costs `certifier`, read off what its declaration has consumed on either side.
pub fn spent(certifier: &mut Certifier, term: Term) -> u64 {
    let before = certifier.consumed().units();
    certifier.reduce_forced(term).expect("reduces");
    let after = certifier.consumed().units();

    after - before
}

/// A closed arithmetic tree `links` deep. Local-free, so it is a term the `whnf`/`forced` tables may key on — and one the closed machine takes, at machine depth.
pub fn chain(links: usize) -> Term {
    (0..links).fold(nat(0), |accumulator, _| {
        Term::intrinsic(Intrinsic::nat_add(accumulator, nat(1)))
    })
}

/// The same tree over an open tip, which the closed machine's gate declines — the term that still exercises the recursive strategy and the depth it prices.
pub fn open_chain(links: usize, tip: &Free) -> Term {
    (0..links).fold(Term::free_var(tip), |accumulator, _| {
        Term::intrinsic(Intrinsic::nat_add(accumulator, nat(1)))
    })
}

/// Reduce an open term and a closed one inside an arm refining `n` to `0`, then both again after the arm retracts: the two inside reducts followed by the two outside ones.
pub fn across_an_arm(certifier: &mut Certifier) -> [Term; 4] {
    let n = binder(1, "n");
    certifier.assume(&n, &nat_type());

    let open = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));
    let closed = chain(8);

    let (inside_open, inside_closed) = certifier.scoped(|certifier| {
        certifier
            .refine(Term::free_var(&n), nat(0))
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
