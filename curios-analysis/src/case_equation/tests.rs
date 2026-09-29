//! The recording rule on each kind of spelling it is asked about.

use {
    super::records_case_equation,
    curios_core::{Free, Intrinsic, Nat, Term},
    curios_utilities::Qualifier,
};

fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

#[test]
fn a_spelling_that_mentions_a_local_carries_an_equation() {
    let n = Free::local(0, Some("n"));

    assert!(records_case_equation(&Term::intrinsic(Intrinsic::nat_lt(
        Term::free_var(&n),
        nat(2)
    ))));
}

#[test]
fn a_closed_spelling_carries_none() {
    assert!(!records_case_equation(&Term::intrinsic(Intrinsic::nat_lt(
        nat(3),
        nat(2)
    ))));
}

#[test]
fn a_top_level_name_carries_none() {
    let flag = Free::global(Qualifier::from(["flag"]));

    assert!(!records_case_equation(&Term::free_var(&flag)));
    assert!(!records_case_equation(&Term::proj(
        Term::free_var(&flag),
        0
    )));
}
