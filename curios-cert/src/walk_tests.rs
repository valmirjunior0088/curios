//! Every walk the kernel runs over whole terms, over one doubling term: sixty levels that each hold the one below twice, a tree no walk per path finishes and a graph of sixty-one nodes — a sum of the level below with itself around a local, and a record of two fields at the level below. A walk the kernel adds joins the table here, so one a change makes per-path again stalls its row rather than waiting for a profile; the walks it shares with the elaborator are `curios-core`'s, held by that crate's table. Inference and checking are rows over a pair of the level below with itself, sixty deep over the same local: the first infers each field and the second checks it against a record of two fields at the level below, so neither is answered by the other's table.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, Level, Reducer, Term},
};

fn doubled(base: Term) -> Term {
    let mut term = base;
    for _ in 0..60 {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    term
}

/// A record of two fields at `base`, sixty levels deep.
fn doubled_record(base: Term) -> Term {
    (0..60u32).fold(base, |type_, level| {
        let first = Free::local(2 * level + 100, None);
        let second = Free::local(2 * level + 101, None);

        Term::tuple_type([(first, type_.clone()), (second, type_)])
    })
}

/// A pair of `base` with itself, sixty levels deep.
fn doubled_pair(base: Term) -> Term {
    (0..60).fold(base, |value, _| Term::tuple([value.clone(), value]))
}

/// A kernel with `n: Nat` and `a: Type` assumed, and a fresh budget: each row spends its own, so one row's cost cannot exhaust the next.
fn kernel(n: &Free, a: &Free) -> Kernel {
    let mut kernel = Kernel::new(1_000_000, SYNTAX);
    kernel.assume(n, &Term::intrinsic(Intrinsic::NatType));
    kernel.assume(a, &Term::type_ground());
    kernel
}

#[test]
fn every_walk_answers_a_doubling_term_in_its_own_size() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let n = Free::local(0, Some("n"));
    let a = Free::local(1, Some("a"));
    let term = doubled(Term::free_var(&n));
    let ground = Ok(Sort::Type(Level::zero()));

    let rows: Vec<(&str, bool)> = vec![
        (
            "weak-head reduction",
            kernel(&n, &a).reduce_forced(term.clone()).is_ok(),
        ),
        (
            "conversion",
            convert(
                &mut kernel(&n, &a),
                &nat,
                &term,
                &doubled(Term::free_var(&n)),
            )
            .is_ok_and(|equal| equal),
        ),
        (
            "the sort of a closed type",
            Sort::of(&mut kernel(&n, &a), &doubled_record(nat.clone())) == ground,
        ),
        (
            "the sort of a type over a local",
            Sort::of(&mut kernel(&n, &a), &doubled_record(Term::free_var(&a))) == ground,
        ),
        (
            "inference",
            infer(&mut kernel(&n, &a), &doubled_pair(Term::free_var(&n))).is_ok(),
        ),
        (
            "checking",
            check(
                &mut kernel(&n, &a),
                &doubled_pair(Term::free_var(&n)),
                &doubled_record(nat.clone()),
            )
            .is_ok(),
        ),
    ];

    for (walk, answered) in rows {
        assert!(answered, "{walk} over a doubling term did not answer");
    }
}
