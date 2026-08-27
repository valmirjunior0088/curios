//! Intrinsic operands and results, the narrowing that needs its bound, and the free-monoid arms.

use {
    crate::{KernelError, infer},
    curios_core::{Free, Intrinsic, Term, UniverseContext},
};

use super::test_support::*;

/// An intrinsic's operands are checked against the types its rule demands.
#[test]
fn an_intrinsic_operand_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();

    let mixed = Term::intrinsic(Intrinsic::nat_add(
        Term::intrinsic(Intrinsic::Bool(true)),
        nat(1),
    ));

    assert!(matches!(
        infer(&mut kernel, &mixed),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A bound stated only on `/sys`'s wrapper is re-checked wherever that application survives and nowhere else: one unfolding leaves the bare operation, which a signature reading its operand alone would admit. Carrying the proof as an operand is what makes the check a property of the node, so it is this crate — not the elaborator that built the node — that decides the narrowing was justified.
///
/// `/syn/Int/NonNeg` is declared rather than defined, since the fixture registry only names it. Opaque is enough: what is under test is that the operand is checked against the proposition at all.
#[test]
fn a_narrowing_to_nat_is_refused_without_its_bound() {
    let mut kernel = kernel();
    let int_type = Term::intrinsic(Intrinsic::IntType);

    let non_neg = Free::global(crate::SYNTAX.proof.int_non_neg.qualifier());
    kernel.declare(
        &non_neg,
        &Term::func_type([(binder(0, "a"), int_type.clone())], Term::prop()),
        &UniverseContext::default(),
    );

    let x = binder(1, "x");
    kernel.assume(&x, &int_type);

    let ok = binder(2, "ok");
    kernel.assume(
        &ok,
        &Term::apply(Term::free_var(&non_neg), vec![Term::free_var(&x)]),
    );

    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::IntToNat {
                int: Term::free_var(&x),
                non_neg: Term::free_var(&ok),
            }),
        ),
        Ok(nat_type()),
    );

    assert!(matches!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::IntToNat {
                int: Term::free_var(&x),
                non_neg: nat(1),
            }),
        ),
        Err(KernelError::Mismatch { .. }),
    ));
}

#[test]
fn an_intrinsic_operation_has_the_result_type_its_rule_states() {
    let mut kernel = kernel();

    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::nat_add(nat(1), nat(2)))
        ),
        Ok(nat_type()),
    );
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::nat_lt(nat(1), nat(2)))
        ),
        Ok(bool_type()),
    );
}

/// A list literal carries its element type, every element checks against it — and `[]` types at exactly that carried element, the case that used to be refused for having no element to read a type from.
#[test]
fn a_list_literal_checks_its_elements_against_its_carried_type() {
    let mut kernel = kernel();

    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::List {
                element: nat_type(),
                items: vec![nat(1), nat(2)]
            }),
        ),
        Ok(Term::intrinsic(Intrinsic::ListType(nat_type()))),
    );

    assert!(matches!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::List {
                element: nat_type(),
                items: vec![nat(1), Term::intrinsic(Intrinsic::Bool(true))]
            }),
        ),
        Err(KernelError::Mismatch { .. }),
    ));

    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::List {
                element: nat_type(),
                items: Vec::new()
            })
        ),
        Ok(Term::intrinsic(Intrinsic::ListType(nat_type()))),
    );
}

/// A free-monoid cons arm is checked under its binders — the peeled generator, the tail, and the induction hypothesis at that tail — against the motive at one generator over the tail. The hypothesis really is usable at the tail's instance, and a body of the wrong type at the case's instance is refused.
#[test]
fn a_free_monoid_arm_must_inhabit_the_motive_at_its_case() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let ih = binder(3, "ih");
    kernel.assume(&n, &nat_type());

    // Motive `(m) => Nat`: the zero arm at `Nat`, the succ arm's ih at `Nat`, and using the ih is exactly inhabiting the motive at the tail.
    let counts = Term::nat_match(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &ih,
        Term::free_var(&ih),
    );
    assert_eq!(infer(&mut kernel, &counts), Ok(nat_type()));

    // A succ arm that produces a Bool where the motive demands a Nat.
    let wrong = Term::nat_match(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &ih,
        Term::intrinsic(Intrinsic::Bool(true)),
    );
    assert!(matches!(
        infer(&mut kernel, &wrong),
        Err(KernelError::Mismatch { .. }),
    ));

    // A zero arm of the wrong type is refused too — the identity case is a case like any other.
    let wrong_zero = Term::nat_match(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        Term::intrinsic(Intrinsic::Bool(true)),
        &pred,
        &ih,
        Term::free_var(&ih),
    );
    assert!(matches!(
        infer(&mut kernel, &wrong_zero),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// The carrier's element type must agree with the scrutinee's: the arms are typed against the carrier's copy, and the values flowing through the match carry the scrutinee's.
#[test]
fn a_free_monoid_carrier_must_match_its_scrutinee() {
    let mut kernel = kernel();
    let xs = binder(0, "xs");
    let motive = binder(1, "m");
    let head = binder(2, "head");
    let tail = binder(3, "tail");
    let ih = binder(4, "ih");
    kernel.assume(&xs, &Term::intrinsic(Intrinsic::ListType(nat_type())));

    // Carrier claims Bool elements over a Nat-list scrutinee.
    let mismatched = Term::list_match(
        Term::free_var(&xs),
        bool_type(),
        Some(&motive),
        nat_type(),
        nat(0),
        &head,
        &tail,
        &ih,
        Term::free_var(&ih),
    );
    assert!(matches!(
        infer(&mut kernel, &mismatched),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A case form names the carrier it eliminates, and that claim needs establishing like any other.
///
/// `check_free_monoid` establishes it — `Carrier::Nat` matches the scrutinee's type against `NatType`, `Carrier::Bin` against its own grain, and `Carrier::List` converts its carried element type against the scrutinee's — and `Cases::Induct` gets it from needing an `InductType` to read a declaration off at all. The other two forms read the claim and checked nothing, which is the same shape as every count the boundary now checks: no typing rule looks at a case form, so no ordering discipline would ever have caught it.
///
/// What it costs is the discipline the free-monoid rule states for itself: the arms are typed at the *case values* — `false` and `true`, or the enumerated literals — while the result is typed at `motive(scrutinee)` and a value flowing through the match carries the scrutinee's type, so a disagreement types the arms at one carrier and runs them at another. `curios-elab` refuses both spellings at `check_intrinsic_head`, which is why no surface program reaches them and why the certifier's copy of the rule went unwritten.
#[test]
fn a_boolean_elimination_requires_a_boolean_scrutinee() {
    let mut kernel = kernel();

    // `match (0 : Nat) : (_) => Nat | false => 1 | true => 2 end`
    let eliminated = Term::bool_match(nat(0), None, nat_type(), nat(1), nat(2));

    assert!(
        infer(&mut kernel, &eliminated).is_err(),
        "the kernel typed a boolean elimination of a `Nat`",
    );
}

/// The same for the dispatch form, whose default is checked at the scrutinee's own instance while its enumerated arms are checked at `Nat` literals.
#[test]
fn a_dispatch_requires_a_natural_scrutinee() {
    let mut kernel = kernel();

    // `match (true : Bool) : (_) => Nat | 0 => 1 | _ => 2 end`
    let eliminated = Term::switch(
        Term::intrinsic(Intrinsic::Bool(true)),
        None,
        nat_type(),
        [(0u32, nat(1))],
        nat(2),
    );

    assert!(
        infer(&mut kernel, &eliminated).is_err(),
        "the kernel typed a `Nat` dispatch on a `Bool`",
    );
}

/// The control for both, and it is what a fix by brick would fail: each form at its own carrier is ordinary code and must keep typing.
#[test]
fn each_intrinsic_elimination_at_its_own_carrier_is_still_accepted() {
    let mut kernel = kernel();

    let boolean = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(true)),
        None,
        nat_type(),
        nat(1),
        nat(2),
    );
    assert_eq!(infer(&mut kernel, &boolean), Ok(nat_type()));

    let dispatch = Term::switch(nat(0), None, nat_type(), [(0u32, nat(1))], nat(2));
    assert_eq!(infer(&mut kernel, &dispatch), Ok(nat_type()));
}
