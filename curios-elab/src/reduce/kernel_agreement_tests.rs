//! The kernel in `curios-core` re-decides reduction from the term alone, with none of this crate's machinery — no cache, no refinements, no metavariables. These tests are the check that the two agree where they must.
//!
//! Agreement is worth asserting precisely because the implementations are separate. If the kernel simply called this reducer the tests would be tautologies; because it does not, a divergence here is a real disagreement about what a term computes to, and one of the two is wrong.
//!
//! The known *deliberate* divergences are internal to reduction and invisible in the result: a `let` is an environment step here and a substitution there, and a match arm binds a projection of the scrutinee here and the payload itself there. Both routes land on the same weak-head normal form, which is exactly what these assertions pin.
//!
//! One case puts an arm's equation in, where the two sides meet it through different doors: the kernel through its arm rule, which is all its public surface offers, and the elaborator through the reducer under the equation registered as an arm registers it.

use curios_core::*;
use {
    super::test_support::{context, nat, nominal},
    crate::refine_head,
    curios_analysis::fixture::SYNTAX,
    curios_cert::Kernel,
};

/// The kernel these fixtures are put to.
fn kernel() -> Kernel {
    Kernel::new(100_000, SYNTAX)
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
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
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
                vec![payload],
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
        [(n, nat_type.clone())],
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
        countdown,
        Term::func_type([(n, nat_type.clone())], nat_type),
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

/// A guard answers its own definition one unfolding down, in both checkers.
///
/// With `small(x) = x < 10`, the type `T = match n < 10 | true => Nat | false => Bool` is `Nat` in the true arm of `match small(n)` and `Bool` in the false one — the guard itself, met unfolded. The kernel settles the guard's reduced spelling and answers `n < 10` from it; the elaborator refused it, comparing guards only as written, until it settled reduced spellings as the kernel does. So the kernel is asked to accept the match whose arms inhabit `T` at `0` and `false`, and the elaborator to reduce `T` to each carrier under the arm's equation. Mutation-checked: without `reduce::refined_reduct`, the elaborator's `T` stays a stuck match in both arms.
#[test]
fn a_guard_answers_its_definition_one_unfolding_down() {
    let mut context = context();
    let mut kernel = kernel();
    let n = context.fresh(Some("n"));
    let small = context.fresh(Some("small"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let bool_type = Term::intrinsic(Intrinsic::BoolType);

    let small_type = Term::func_type([(x, nat_type.clone())], bool_type.clone());
    let small_body = Term::func(
        [(x, nat_type.clone())],
        Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&x), nat(10))),
    );
    let guard = Term::apply(Term::free_var(&small), [Term::free_var(&n)]);
    // The carrier the unfolded guard picks: `Nat` below ten, `Bool` otherwise.
    let carrier = Term::bool_match(
        Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), nat(10))),
        None,
        Term::type_ground(),
        bool_type.clone(),
        nat_type.clone(),
    );

    kernel.define(
        &small,
        &small_type,
        &small_body,
        &UniverseContext::default(),
    );
    kernel.assume(&n, &nat_type);
    let split = Term::bool_match(
        guard.clone(),
        None,
        carrier.clone(),
        Term::intrinsic(Intrinsic::Bool(false)),
        nat(0),
    );
    assert_eq!(
        curios_cert::check(&mut kernel, &split, &carrier),
        Ok(()),
        "the kernel answers the unfolded guard in each arm"
    );

    context.assume(&small, &small_type);
    context.define(&small, &small_body, None);
    context.assume(&n, &nat_type);
    for (value, expected) in [(true, nat_type), (false, bool_type)] {
        let reduced = context.with_frame(|context| {
            refine_head(context, &guard, &Term::intrinsic(Intrinsic::Bool(value)))
                .expect("the arm's equation registers");
            super::reduce(context, carrier.clone())
        });
        assert_eq!(
            reduced,
            Ok(expected),
            "the elaborator answers the unfolded guard in the {value} arm"
        );
    }
}
