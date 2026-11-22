//! How a recorded call is graded: what an arm, a guard and an inversion establish of the variable it names and where that ends, an applied lambda's binder standing for its argument, and a curried spine's lists.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::{Subterm, Term},
};

/// What inferring `term` costs `certifier`, read off what its declaration has consumed on either side.
fn spent_inferring(certifier: &mut Certifier, term: &Term) -> u64 {
    let before = certifier.consumed().units();
    infer(certifier, term).expect("is typed");
    let after = certifier.consumed().units();

    after - before
}

/// `rec f(h, a, b, n) -> Type`, its successor arm calling `f` at `match ${left} == ${right} | false => n | true => pred end`.
const ASKED_ARGUMENT: &str = "#{node = Rec} (let f: (h: (x: #NatType) -> #NatType, a: #NatType, b: #NatType, n: #NatType) -> Type = (h2: (x: #NatType) -> #NatType, a2: #NatType, b2: #NatType, n2: #NatType) => #{goal = Type} match n2 | 0 => #NatType | pred + 1 => f(h2, a2, b2, #{goal = #NatType} match #NatEql(${left}, ${right}) | false => n2 | true => pred end) end; f)";

/// The control every accepting fixture below is read against: a call at the caller's own argument is a cycle with nothing decreasing, and the gate refuses it. Mutation-checked: recording no applied call empties the group's calls, and this is then accepted.
#[test]
fn a_type_calling_itself_at_its_own_argument_is_refused() {
    assert!(matches!(
        infer(&mut certifier(), &group("f(n)")),
        Err(Error::NotDescending { .. }),
    ));
}

/// A `Nat` arm refines its scrutinee to one successor over the predecessor it binds, so a call at the predecessor is below the caller's argument. Mutation-checked: refining the scrutinee from the arm's typing spelling, `pred + 1`, reads nothing below it, and this is refused.
#[test]
fn a_type_descending_through_a_nat_arm_is_accepted() {
    assert!(infer(&mut certifier(), &group(&split("n", "f(pred)"))).is_ok());
}

/// A lambda applied on the spot is read through its binder: `((m) => f(m))(pred)` calls `f` at what `m` stands for, which is below the caller's argument. Mutation-checked: typing the lambda without its binders standing for the arguments leaves `m` a fresh binder nothing is below, and this is refused.
#[test]
fn a_descent_through_an_applied_lambda_is_read_through_its_binder() {
    let body = split("n", "((m: #NatType) => f(m))(pred)");

    assert!(infer(&mut certifier(), &group(&body)).is_ok());
}

/// A comparison behind a definition is read as a guard before the arm's case equation is assumed: the arm where `is_zero(n)` is false rules zero out for `n`, so `n - 1` is below it. Mutation-checked: read after the equation, the scrutinee reduces to the arm's own `false` and compares nothing, and this is refused.
#[test]
fn a_guard_behind_a_definition_establishes_what_its_arm_descends_on() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "let /is_zero: (x: #NatType) -> #BoolType = (x: #NatType) => #NatEql(x, 0);",
    );

    let body =
        "#{goal = Type} match /is_zero(n) | false => f(#NatSub(n, 1)) | true => #NatType end";

    assert!(infer(&mut certifier, &group(body)).is_ok());
}

/// A typing remembered on one side of what an arm establishes for the recorder does not answer on the other. A group typed inside a term closes under what the arms around it established, and the default arm of a `switch` rules zero out for its scrutinee while assuming no equation and opening no binder, so nothing else stands between a term typed outside it and the same term inside. Mutation-checked: without the clear where the arm's knowledge is entered the term is answered inside the bracket for nothing, and without the one where it is retracted, after it.
#[test]
fn a_remembered_type_does_not_outlive_what_an_arm_established() {
    let stated = group("#NatType");
    let Subterm::Rec(rec) = &*stated else {
        unreachable!("a group")
    };

    let mut certifier = certifier();
    let (locals, term) = opened(
        &mut certifier,
        "(rec: (n: #NatType) -> Type, k: #NatType) => #NatAdd(k, 1)",
    );
    certifier.open_group(rec.group(), &[locals[0]]);
    certifier.begin_member(0);

    let first = spent_inferring(&mut certifier, &term);
    let again = spent_inferring(&mut certifier, &term);
    let inside = certifier.scoped(|certifier| {
        certifier
            .assume_nonzero(locals[1])
            .expect("the arm's knowledge enters");
        spent_inferring(certifier, &term)
    });
    let after = spent_inferring(&mut certifier, &term);

    assert!(first > 0, "typing it spends");
    assert_eq!(again, 0, "and a second typing is remembered");
    assert!(inside > 0, "the arm's knowledge is entered");
    assert!(after > 0, "and retracted");
}

/// An arm that splits a lambda's binder splits what the binder stands for: in `((x) => match x | 0 => … | k + 1 => f(k) end)(n)` the lambda's body runs with `x` being `n`, so the arm's `k + 1` is `n`'s case too and `k` is below `n`. Mutation-checked: with the arm refining `x` alone, `k` is below nothing the caller's parameter reads, and this is refused.
#[test]
fn an_arm_splitting_an_applied_lambdas_binder_splits_what_the_binder_stands_for() {
    let body = fill!(
        "((x: #NatType) => ${split})(n)",
        split = split("x", "f(pred)")
    );

    assert!(infer(&mut certifier(), &group(&body)).is_ok());
}

/// Zero ruled out of a lambda's binder is ruled out of what it stands for: in `((x) => match x | 0 => … | _ => f(x - 1) end)(n)` the default runs with `x` being `n` and nonzero, so `x - 1` is below `n`, as a default over `n` itself reads it, and a boolean guard's `false` arm over `x == 0` alike. Mutation-checked: ruling zero out of the binder alone, the call reads at most `n`, and both are refused.
#[test]
fn zero_ruled_out_of_an_applied_lambdas_binder_is_ruled_out_of_what_it_stands_for() {
    for body in [
        "((x: #NatType) => #{goal = Type} match x | 0 => #NatType | _ => f(#NatSub(x, 1)) end)(n)",
        "((x: #NatType) => #{goal = Type} match #NatEql(x, 0) | false => f(#NatSub(x, 1)) | true => #NatType end)(n)",
    ] {
        assert!(infer(&mut certifier(), &group(body)).is_ok(), "{body}");
    }
}

/// A lambda applied on the spot to a `let`-bound local stands for what the `let` stands for, as every graded call reads the local: in `let u = n; ((x) => match x | 0 => … | pred + 1 => f(pred) end)(u)` the arm takes `n` apart, so the call descends. Mutation-checked: with `infer_telescope` handing `refine_size` the argument as written, `x` stands for `u`, which no graded call names, and the group is refused.
#[test]
fn a_lambda_applied_to_a_let_bound_local_stands_for_what_the_let_stands_for() {
    let body = fill!(
        "let u: #NatType = n; ((x: #NatType) => ${split})(u)",
        split = split("x", "f(pred)")
    );

    assert!(infer(&mut certifier(), &group(&body)).is_ok(), "{body}");
}

/// A self-call the closure cannot read as a square matrix does not descend: `t(n, n)` in `m`'s type is typed before `m` is bound, so it is graded against `n` alone, one row short of `t`'s two parameters, and a cycle through it is one `decide` reads as `Partial` rather than passes. Mutation-checked: with `decide` passing a self-call whose matrix is not square, and `close` dropping a pair it cannot compose, the group closes with no cycle and is accepted.
#[test]
fn a_self_call_graded_short_in_a_leading_binders_type_does_not_descend() {
    let group = term(
        "#{node = Rec} (let t: (n: #NatType, m: #NatType) -> Type = (n: #NatType, m: ((y: Type) => #NatType)(t(n, n))) => #NatType; t)",
    );

    assert!(matches!(
        infer(&mut certifier(), &group),
        Err(Error::NotDescending { .. }),
    ));
}

/// The size reader reads a call's argument by plain reduction (`Driver::force`), which asks the certifier's conversion nothing: an argument below the caller's only to a question — `h(a + b)` and `h(b + a)` one call to conversion and two atoms to the fold — descends on nothing, and the gate refuses the group. The control spells the two calls alike, which the fold pairs with no question, and descends. Totality reads terms this way because conversion's proof irrelevance is sound only while acceptance waits on a totality that rests on no verdict of conversion's.
#[test]
fn a_call_below_its_caller_only_to_a_question_descends_on_nothing() {
    let group = |left: &str, right: &str| term(&fill!(ASKED_ARGUMENT, left, right));

    assert!(
        infer(
            &mut certifier(),
            &group("h2(#NatAdd(a2, b2))", "h2(#NatAdd(a2, b2))")
        )
        .is_ok(),
        "the calls spelled alike descend, or the refusal below proves nothing"
    );
    assert!(matches!(
        infer(
            &mut certifier(),
            &group("h2(#NatAdd(a2, b2))", "h2(#NatAdd(b2, a2))")
        ),
        Err(Error::NotDescending { .. }),
    ));
}

/// Zero ruled out of a lambda's binder is ruled out of what it stands for only where it stands for it bare: in `((x) => match x | 0 => … | _ => f(n - 1) end)(n + 1)` the default rules zero out of a successor, which says nothing of `n`, so `n - 1` is at most `n`, and at `0` the call hands back what it was handed.
#[test]
fn zero_ruled_out_of_a_binder_standing_for_a_successor_is_ruled_out_of_no_parameter() {
    let body = "((x: #NatType) => #{goal = Type} match x | 0 => #NatType | _ => f(#NatSub(n, 1)) end)(#NatAdd(n, 1))";

    assert!(matches!(
        infer(&mut certifier(), &group(body)),
        Err(Error::NotDescending { .. }),
    ));
}

/// What an arm establishes of a lambda's binder, and so of what the binder stands for, leaves with the arm: past a `let` whose value split `x`, standing for `n`, at a default or at `pred + 1`, `n` is neither nonzero nor a successor, so `n - 1` is at most `n`.
#[test]
fn what_an_arm_establishes_of_an_applied_lambdas_binder_leaves_with_the_arm() {
    for arm in ["_", "pred + 1"] {
        let body = fill!(
            "((x: #NatType) => let b: #BoolType = #{goal = #BoolType} match x | 0 => false | ${arm} => true end; #{goal = Type} match b | false => f(#NatSub(n, 1)) | true => #NatType end)(n)",
            arm
        );

        assert!(
            matches!(
                infer(&mut certifier(), &group(&body)),
                Err(Error::NotDescending { .. }),
            ),
            "{body}"
        );
    }
}

/// A curried spine whose head is a lambda applied on the spot binds its later list to the lambda the head's body is, as what that binder stands for: in `((x) => (y) => f(y))(0)(n)` the call is at the caller's own argument and the gate refuses it, and through a split of `y` the call is below `n` and the group is accepted.
#[test]
fn a_curried_spine_binds_its_later_list_to_the_lambda_its_head_answers() {
    assert!(matches!(
        infer(
            &mut certifier(),
            &group("((x: #NatType) => (y: #NatType) => f(y))(0)(n)")
        ),
        Err(Error::NotDescending { .. }),
    ));

    let body = fill!(
        "((x: #NatType) => (y: #NatType) => ${split})(0)(n)",
        split = split("y", "f(pred)")
    );
    assert!(infer(&mut certifier(), &group(&body)).is_ok());
}

/// A member handed in a curried spine's later list is named bare, a call graded unknown, whatever the lambda the head answers does with it: `((x) => (h) => h(x))(n)(f)` applies `f` at the caller's own argument, and the gate refuses it.
#[test]
fn a_member_handed_in_a_later_list_is_a_call_of_unknown_grade() {
    let body = "((x: #NatType) => (h: (k: #NatType) -> Type) => h(x))(n)(f)";

    assert!(matches!(
        infer(&mut certifier(), &group(body)),
        Err(Error::NotDescending { .. }),
    ));
}

/// A member called across two argument lists makes one call of its two leading lambdas: `t(n)(m)` hands `t` both its parameters back, and the gate refuses it.
#[test]
fn a_member_called_across_two_lists_is_one_call_of_both_its_parameters() {
    let group = term(
        "#{node = Rec} (let t: (n: #NatType) -> (m: #NatType) -> Type = (n: #NatType) => (m: #NatType) => t(n)(m); t)",
    );

    assert!(matches!(
        infer(&mut certifier(), &group),
        Err(Error::NotDescending { .. }),
    ));
}

/// An arm's case stands for the scrutinee as a graded call reads it, a variable an index inversion solved standing as written: under `q`'s arm, whose inversion solves `t` to `k`, `match t` takes `t` apart, so the call on `pred` descends. Mutation-checked: with the arm assuming its case of the scrutinee read by value, `k` stands where `t` was matched, no call names it, and the group is refused.
#[test]
fn an_arm_after_an_inversion_takes_apart_the_variable_a_call_names() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "induct /Q: (i: #NatType) -> pub Type\n| c(k: #NatType): (k)\nend",
    );
    let group = term(
        "#{node = Rec} (let d: (t: #NatType, q: #{node = InductType} /Q()(t)) -> Type = (t: #NatType, q: #{node = InductType} /Q()(t)) => #{goal = Type} match q | c(k) => #{goal = Type} match t | 0 => #NatType | pred + 1 => d(pred, #{node = Variant} /Q/c(pred)) end end; d)",
    );

    assert!(infer(&mut certifier, &group).is_ok());
}

/// A guard rules zero out of its comparison's operand as a graded call reads it, a variable an index inversion solved standing as written: under `q`'s arm, whose inversion solves `t` to `k`, the `false` arm of `t == 0` rules zero out of `t`, so the call on `t - 1` descends. Mutation-checked: with the guard read off the scrutinee by value, zero is ruled out of `k`, which no call names, and the group is refused.
#[test]
fn a_guard_after_an_inversion_rules_zero_out_of_the_variable_a_call_names() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "induct /Q: (i: #NatType) -> pub Type\n| c(k: #NatType): (k)\nend",
    );
    let group = term(
        "#{node = Rec} (let d: (t: #NatType, q: #{node = InductType} /Q()(t)) -> Type = (t: #NatType, q: #{node = InductType} /Q()(t)) => #{goal = Type} match q | c(k) => #{goal = Type} match #NatEql(t, 0) | false => d(#NatSub(t, 1), #{node = Variant} /Q/c(#NatSub(t, 1))) | true => #NatType end end; d)",
    );

    assert!(infer(&mut certifier, &group).is_ok());
}

/// A case of an expression takes its value apart as a case of a binder does, so the binders it opens are below the value the expression reads at: a payload of a case on `x.next` is below `x`, and the call on it descends. Mutation-checked: with an expression's case refining nothing, the payload stands for no value below `x`, and the group is refused.
#[test]
fn a_case_on_a_field_takes_apart_what_is_below_the_record() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "induct /Opt(A: Type): pub Type\n| none()\n| some(a: A)\nend\nstruct /S: pub Type { next: #{node = InductType} /Opt(#{node = StructType} /S()) }",
    );
    let group = |call: &str| {
        term(&fill!(
            "#{node = Rec} (let f: (x: #{node = StructType} /S()) -> Type = (x: #{node = StructType} /S()) => #{goal = Type} match #{field = Struct(/S, 0)} x.0 | none() => #NatType | some(y) => ${call} end; f)",
            call
        ))
    };

    assert!(infer(&mut certifier, &group("f(y)")).is_ok());
    assert!(matches!(
        infer(&mut certifier, &group("f(x)")),
        Err(Error::NotDescending { .. }),
    ));
}
