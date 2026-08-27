//! The kernel in `curios-core` re-decides reduction from the term alone, with none of this crate's machinery — no cache, no refinements, no metavariables. These tests are the check that the two agree where they must.
//!
//! Agreement is worth asserting precisely because the implementations are separate. If the kernel simply called this reducer the tests would be tautologies; because it does not, a divergence here is a real disagreement about what a term computes to, and one of the two is wrong.
//!
//! The known *deliberate* divergences are internal to reduction and invisible in the result: a `let` is an environment step here and a substitution there, and a match arm binds a projection of the scrutinee here and the payload itself there. Both routes land on the same weak-head normal form, which is exactly what these assertions pin.

use curios_core::*;
use {
    super::test_support::{context, nat, nominal},
    curios_cert::Kernel,
};

/// A kernel minting above every binder these fixtures use.
fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000, crate::SYNTAX);
    kernel.set_local_floor(10_000);
    kernel
}

/// Reduce `term` both ways and require the same answer.
fn agree(term: Term) {
    let mut context = context();
    let mut kernel = kernel();

    let elaborated = super::reduce_forced(&mut context, term.clone());
    let checked = kernel.reduce_forced(term.clone());

    assert_eq!(
        elaborated, checked,
        "the elaborator and the kernel disagree on {term}",
    );
}

#[test]
fn beta_agrees() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    agree(Term::apply(
        Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)),
        [nat(9)],
    ));
}

#[test]
fn intrinsic_folds_agree() {
    agree(Term::intrinsic(Intrinsic::nat_add(nat(20), nat(22))));
    agree(Term::intrinsic(Intrinsic::nat_mul(nat(6), nat(7))));
    agree(Term::intrinsic(Intrinsic::nat_lt(nat(2), nat(3))));
}

#[test]
fn a_stuck_intrinsic_agrees() {
    let mut context = context();
    let n = context.fresh(Some("n"));

    agree(Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&n),
        nat(0),
    )));
    agree(Term::intrinsic(Intrinsic::nat_add(
        nat(1),
        Term::free_var(&n),
    )));
}

#[test]
fn zeta_agrees_despite_different_mechanisms() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    agree(Term::let_(
        &x,
        Term::intrinsic(Intrinsic::NatType),
        nat(4),
        Term::let_(
            &y,
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(5))),
            Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&x), Term::free_var(&y))),
        ),
    ));
}

#[test]
fn iota_agrees_despite_different_arm_binding() {
    let mut context = context();
    let m = context.fresh(Some("m"));
    let payload = context.fresh(Some("a"));

    agree(Term::induct_match(
        Term::variant(nominal("E"), Vec::<Term>::new(), "some", [nat(42)]),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        [
            ("none", Vec::<Free>::new(), nat(0)),
            (
                "some",
                vec![payload.clone()],
                Term::intrinsic(Intrinsic::nat_add(Term::free_var(&payload), nat(1))),
            ),
        ],
    ));
}

#[test]
fn structural_nat_induction_agrees() {
    let mut context = context();
    let m = context.fresh(Some("m"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));

    // The cons arm sums the hypothesis, so the whole spine is walked rather than one layer peeled.
    agree(Term::nat_match(
        nat(5),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        nat(0),
        &pred,
        &ih,
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat(2))),
    ));
}

#[test]
fn a_stuck_match_agrees() {
    let mut context = context();
    let m = context.fresh(Some("m"));
    let n = context.fresh(Some("n"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));

    agree(Term::nat_match(
        Term::free_var(&n),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        nat(0),
        &pred,
        &ih,
        Term::free_var(&pred),
    ));
}

#[test]
fn projection_agrees() {
    agree(Term::proj(Term::tuple([nat(10), nat(20), nat(30)]), 2));
    agree(Term::proj(
        Term::variant(nominal("E"), Vec::<Term>::new(), "some", [nat(42)]),
        1,
    ));
}

#[test]
fn recursion_agrees_to_a_literal_and_stays_folded_otherwise() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let m = context.fresh(Some("m"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    let countdown = context.fresh(Some("countdown"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    let body = Term::func(
        [(n.clone(), nat_type.clone())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&m),
            nat_type.clone(),
            nat(0),
            &pred,
            &ih,
            Term::apply(Term::free_var(&countdown), [Term::free_var(&pred)]),
        ),
    );

    let group = [(
        countdown.clone(),
        Term::func_type([(n.clone(), nat_type.clone())], nat_type),
        body,
    )];

    agree(Term::rec(
        group.clone(),
        Term::apply(Term::free_var(&countdown), [nat(4)]),
    ));
    agree(Term::rec(
        group,
        Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
    ));
}
