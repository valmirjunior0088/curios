//! Raw intrinsic obligations, independently of the generated system wrappers.

use {
    crate::*,
    curios_analysis::test_support::*,
    curios_core::{Global, Intrinsic, Nat, Term, Universes},
};

#[test]
fn a_channel_allocation_checks_its_positive_capacity_evidence() {
    let mut context = Context::with_default_budget(SYNTAX);
    let holds = Global::Authored(SYNTAX.proof.holds.qualifier());
    let b = context.fresh(Some("b"));
    context.declare(
        &holds,
        &Term::func_type([(b, Term::intrinsic(Intrinsic::BoolType))], Term::prop()),
    );
    let proof = context.fresh(Some("positive"));
    context.assume(
        &proof,
        &Term::apply(
            Term::instance(holds, Universes::default()),
            vec![Term::intrinsic(Intrinsic::Bool(true))],
        ),
    );
    let nat = |n| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let allocation = |capacity, evidence| {
        Term::intrinsic(Intrinsic::Channel {
            element: Term::intrinsic(Intrinsic::NatType),
            capacity: nat(capacity),
            positive: evidence,
        })
    };
    let (_, type_) = elaborate(
        &mut context,
        &allocation(1usize, Term::free_var(&proof)),
        Mode::Infer,
    )
    .expect("positive capacity");
    assert_eq!(
        type_,
        Term::intrinsic(Intrinsic::IoType(Term::intrinsic(Intrinsic::ChannelType(
            Term::intrinsic(Intrinsic::NatType)
        ))))
    );
    for invalid in [
        allocation(0usize, Term::free_var(&proof)),
        allocation(1usize, nat(1usize)),
    ] {
        let error = elaborate(&mut context, &invalid, Mode::Infer)
            .expect_err("raw node must carry the right evidence")
            .shown(&mut context);
        assert!(format!("{error:?}").contains("Mismatch"), "{error:?}");
    }
}
