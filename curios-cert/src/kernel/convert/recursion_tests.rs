//! Recursive heads: alpha-variant groups, a folded call compared against its unfolding, and the recurrence that is assumed rather than unfolded forever.

use super::test_support::*;
use {
    crate::{Kernel, convert},
    curios_core::{Free, Intrinsic, Term},
};

/// Binder *identity* must not leak into conversion. Two `rec` groups written with different minted names are the same group: binder names are display hints, and the bodies are de Bruijn-indexed under their scopes.
///
/// This is the property that lets a folded recursive call be compared structurally at all — see the projection arm, which requires the groups to be equal.
#[test]
fn two_alpha_variant_recursive_groups_convert() {
    let mut kernel = kernel();
    let x = binder(90, "x");

    let countdown = |group_binder: Free, param: Free, motive: Free, pred: Free, ih: Free| {
        let body = Term::func(
            [(param.clone(), nat_type())],
            Term::nat_match(
                Term::free_var(&param),
                Some(&motive),
                nat_type(),
                nat(0),
                &pred,
                &ih,
                Term::apply(Term::free_var(&group_binder), [Term::free_var(&pred)]),
            ),
        );

        Term::rec(
            [(
                group_binder.clone(),
                Term::func_type([(param, nat_type())], nat_type()),
                body,
            )],
            Term::apply(Term::free_var(&group_binder), [Term::free_var(&x)]),
        )
    };

    let left = countdown(
        binder(0, "countdown"),
        binder(1, "n"),
        binder(2, "m"),
        binder(3, "pred"),
        binder(4, "ih"),
    );
    let right = countdown(
        binder(10, "loop"),
        binder(11, "k"),
        binder(12, "motive"),
        binder(13, "p"),
        binder(14, "rest"),
    );

    assert_ne!(
        format!("{left}"),
        String::new(),
        "the fixture should render, so a failure names real terms",
    );
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(true));
}

/// A recursive call applied to a symbolic argument stays folded, and comparing it with itself terminates rather than unfolding in lockstep forever.
#[test]
fn a_folded_recursive_call_converts_without_unfolding_forever() {
    let mut kernel = Kernel::new(10_000, crate::SYNTAX);
    kernel.set_local_floor(1_000);

    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let hypothesis = binder(3, "ih");
    let countdown = binder(4, "countdown");
    let x = binder(5, "x");

    let body = Term::func(
        [(n.clone(), nat_type())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive),
            nat_type(),
            nat(0),
            &pred,
            &hypothesis,
            Term::apply(Term::free_var(&countdown), [Term::free_var(&pred)]),
        ),
    );

    let group = [(
        countdown.clone(),
        Term::func_type([(n.clone(), nat_type())], nat_type()),
        body,
    )];

    let folded = Term::rec(
        group.clone(),
        Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
    );
    let same_shape_other_argument = Term::rec(
        group,
        Term::apply(
            Term::free_var(&countdown),
            [Term::free_var(&binder(6, "y"))],
        ),
    );

    assert_eq!(
        convert(&mut kernel, &nat_type(), &folded, &folded.clone()),
        Ok(true),
    );
    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &folded,
            &same_shape_other_argument,
        ),
        Ok(false),
    );
}

/// Conversion keeps the constant function apart from the identity even when the binder floor claims every name is available.
///
/// A positive control for capture-avoidance. Eta at a function type opens a binder that would alias a free local if the floor were wrong — `(x) => y` and `(x) => x` become the same term the moment the opened binder *is* `y` — so seeding at zero and colliding deliberately with what the kernel mints next is the sharpest form of the question. It does not produce a capture, and the route that would have made it reachable is now closed at the source: `recheck` derives the floor from the module's own terms rather than reading `Module::binder_floor`, which nothing checks.
#[test]
fn conversion_separates_a_constant_from_the_identity_at_a_zero_floor() {
    let colliding = {
        let mut scout = Kernel::new(100_000, crate::SYNTAX);
        scout.set_local_floor(0);
        scout.fresh(Some("y"))
    };

    let mut kernel = Kernel::new(100_000, crate::SYNTAX);
    kernel.set_local_floor(0);

    let nat = Term::intrinsic(Intrinsic::NatType);
    kernel.assume(&colliding, &nat);

    let parameter = Free::local(9_000, Some("x"));
    let constant = Term::func(
        [(parameter.clone(), nat.clone())],
        Term::free_var(&colliding),
    );
    let identity = Term::func(
        [(parameter.clone(), nat.clone())],
        Term::free_var(&parameter),
    );
    let function = Term::func_type([(parameter, nat.clone())], nat);

    assert!(
        !convert(&mut kernel, &function, &constant, &identity).expect("the comparison completes"),
        "the constant function and the identity are not convertible",
    );
}

/// The coinductive recurrence rule, which nothing in the corpus reaches.
///
/// Two folded recursive spellings can unfold forever without ever disagreeing — that is what an equirecursive type is — so `compare` keeps the goals it is already inside and *assumes* one that recurs. It is the only rule in this crate that discharges a goal without looking at either term, which is why `convert`'s module documentation calls it the place a conversion checker is most likely to be unsound.
///
/// Instrumenting `History::enter` and running the whole corpus — the fixed prelude through the kernel's own walk, plus every test program — counts 83,945 goals entered and **zero** recurrences. The two recursive-group fixtures above do not reach it either: `two_alpha_variant_recursive_groups_convert` converges because alpha-variant groups are structurally equal, and `a_folded_recursive_call_converts_without_unfolding_forever` terminates on `force` discarding an unfolding that restuck. So the rule was live code that no program could exercise, which is the condition under which a rule's mistakes stay invisible — the same shape as (V)'s argument rule sitting inert at 6010 of 6041 sites while the corpus passed throughout, and one of the four `documentation/soundness/across-the-perimeter.md` collects.
///
/// This reaches it. Both sides are the equirecursive type `X = (X) -> Nat`, spelled as *different* groups — the right carries a second, unused member — so the projection arm's syntactic comparison fails and both sides take a delta step. Unfolding poses `(left) -> Nat ≡ (right) -> Nat`, whose domain goal is the goal already in progress, and the recurrence discharges it. The two really are the same type, so accepting is correct; what is being pinned is that the rule fires here at all.
///
/// The control is [`a_recurrence_does_not_excuse_a_finite_disagreement`], the same construction with the codomains differing: the cycle closes on the domain exactly as here, and the comparison must still fail on the sibling goal. That is the whole argument the rule rests on — a genuine cycle leaves nothing but itself to check, and any finite disagreement surfaces elsewhere first — so a blanket accept would pass the witness and fail the control.
#[test]
fn a_recurring_goal_is_assumed_rather_than_unfolded_forever() {
    let mut kernel = kernel();

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &equirecursive(binder(20, "a"), binder(21, "x"), nat_type(), false),
            &equirecursive(binder(30, "b"), binder(31, "y"), nat_type(), true),
        ),
        Ok(true),
        "two spellings of the equirecursive type `X = (X) -> Nat` did not converge",
    );
}

/// The control for the fixture above: the cycle closes on the domain, and the codomains still have to agree.
#[test]
fn a_recurrence_does_not_excuse_a_finite_disagreement() {
    let mut kernel = kernel();
    let other = declare(&mut kernel, "Other", Term::type_ground());

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &equirecursive(binder(40, "a"), binder(41, "x"), nat_type(), false),
            &equirecursive(binder(50, "b"), binder(51, "y"), other, true),
        ),
        Ok(false),
        "the recurrence on the domain was read as settling the whole comparison",
    );
}
