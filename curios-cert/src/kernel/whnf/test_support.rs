//! Fixtures the weak-head reduction suites share: a kernel above every binder they use, the chains they spend budget on, and the arm they take equations under.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::Kernel,
    curios_core::{Free, Intrinsic, Nat, Reducer, Term, UniverseContext},
};

/// The kernel every test starts from. The floor keeps the identities minted below out of the range the kernel mints from for eta-contraction, exactly as a real caller must seed it above the lowerer's and the elaborator's binders.
pub(super) fn kernel() -> Kernel {
    let mut kernel = Kernel::new(1_000_000, crate::SYNTAX);
    kernel.set_local_floor(1_000);
    kernel
}

/// A test binder. Indices below the kernel's floor, so they cannot alias one it mints itself.
pub(super) fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

pub(super) fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

pub(super) fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

pub(super) fn monomorphic() -> UniverseContext {
    UniverseContext::default()
}

pub(super) fn polymorphic() -> UniverseContext {
    UniverseContext {
        parameter_count: 1,
        ..Default::default()
    }
}

/// What reducing `term` costs `kernel`, read off the remaining budget on either side.
pub(super) fn spent(kernel: &mut Kernel, term: Term) -> u64 {
    let (before, _) = kernel.consumption();
    kernel.reduce_forced(term).expect("reduces");
    let (after, _) = kernel.consumption();

    before - after
}

/// A closed arithmetic tree `links` deep. Local-free, so it is a term the `whnf`/`forced` tables may key on — and one the closed machine takes, at machine depth.
pub(super) fn chain(links: usize) -> Term {
    (0..links).fold(nat(0), |accumulator, _| {
        Term::intrinsic(Intrinsic::nat_add(accumulator, nat(1)))
    })
}

/// The same tree over an open tip, which the closed machine's gate declines — the term that still exercises the recursive strategy and the depth it prices.
pub(super) fn open_chain(links: usize, tip: &Free) -> Term {
    (0..links).fold(Term::free_var(tip), |accumulator, _| {
        Term::intrinsic(Intrinsic::nat_add(accumulator, nat(1)))
    })
}

/// Reduce an open term and a closed one inside an arm refining `n` to `0`, then both again after the arm retracts: the two inside reducts followed by the two outside ones.
pub(super) fn across_an_arm(kernel: &mut Kernel) -> [Term; 4] {
    let n = binder(1, "n");
    kernel.assume(&n, &nat_type());

    let open = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));
    let closed = chain(8);

    let (inside_open, inside_closed) = kernel.scoped(|kernel| {
        kernel.refine(Term::free_var(&n), nat(0));

        (
            kernel.reduce_forced(open.clone()).expect("reduces"),
            kernel.reduce_forced(closed.clone()).expect("reduces"),
        )
    });

    [
        inside_open,
        inside_closed,
        kernel.reduce_forced(open).expect("reduces"),
        kernel.reduce_forced(closed).expect("reduces"),
    ]
}

/// The strategy arm of the differential fixture below: the ordinary test kernel with its closed machine off, so every closed term is walked by the recursive strategy. Beside its one consumer on purpose — nothing else may evaluate with the machine disabled.
pub(super) fn strategy_kernel() -> Kernel {
    let mut kernel = kernel();
    kernel.machine = false;
    kernel
}
