//! Every walk the kernel runs over whole terms, over one doubling term: sixty levels that each sum the one below with itself around a local, a tree no walk per path finishes and a graph of sixty-one nodes. A walk the kernel adds joins the table here, so one a change makes per-path again stalls its row rather than waiting for a profile; the walks it shares with the elaborator are `curios-core`'s, held by that crate's table. Inference and checking are not rows: the `infer` memo keeps local-free terms alone, so a term mentioning a local is typed once per path and such a row would stall.

use {
    crate::*,
    curios_analysis::fixture::SYNTAX,
    curios_core::{Free, Intrinsic, Reducer, Term},
};

fn doubled(base: Term) -> Term {
    let mut term = base;
    for _ in 0..60 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    term
}

/// A kernel with `n: Nat` assumed, and a fresh budget: each row spends its own, so one row's cost cannot exhaust the next.
fn kernel(n: &Free) -> Kernel {
    let mut kernel = Kernel::new(1_000_000, SYNTAX);
    kernel.assume(n, &Term::intrinsic(Intrinsic::NatType));
    kernel
}

#[test]
fn every_walk_answers_a_doubling_term_in_its_own_size() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let n = Free::local(0, Some("n"));
    let term = doubled(Term::free_var(&n));

    let rows: Vec<(&str, bool)> = vec![
        (
            "weak-head reduction",
            kernel(&n).reduce_forced(term.clone()).is_ok(),
        ),
        (
            "conversion",
            convert(&mut kernel(&n), &nat, &term, &doubled(Term::free_var(&n)))
                .is_ok_and(|equal| equal),
        ),
    ];

    for (walk, answered) in rows {
        assert!(answered, "{walk} over a doubling term did not answer");
    }
}
