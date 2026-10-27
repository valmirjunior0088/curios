//! A bound whose subject or proposition is pinned later: discharged once a later argument, the expected type or the goal pins it, held under the guard its slot was born under, and what a failing one reports.

use crate::tests::{run, typecheck};

// A bound is tried when it is inserted, where an arm's refinements are in scope — but its subject may not be known yet. Here a later argument pins `n`, after `ok` was inserted over an unsolved `n`; the bound is parked and discharged once the subject is known, instead of reported with the subject already substituted into it.
#[test]
fn a_bound_whose_subject_a_later_argument_pins_discharges() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq};
        use /std/Bool/{Holds};
        let need(@n: Nat, @_ok: Holds(n < 10), _witness: Eq()(n, 3)) -> Nat = n;
        let g: Nat = need(Eq/refl());
        /std/print(Nat/to_str(g))
        "#),
        b"3"
    );
}

// The expectation pins the subject only at the call's turnaround, after every argument was inserted: the shape of any constructor indexed by a value whose bound is an implicit.
#[test]
fn a_bound_whose_subject_the_expected_type_pins_discharges() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Vec};
        use /std/Bool/{Holds};
        let mk(@n: Nat, @_ok: Holds(n < 10)) -> Vec(Nat, n) = Vec/replicate(n, 0);
        let v: Vec(Nat, 3) = mk();
        /std/print(Nat/to_str(Vec/len(v)))
        "#),
        b"3"
    );
}

// A witness's telescope is instantiated before the goal pins its parameters, so its bounds are tried once the goal has unified with its head.
#[test]
fn a_bound_in_a_witness_telescope_discharges_once_the_goal_pins_it() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Show};
        use /std/Bool/{Holds};
        struct Small(n: Nat): pub Type { Nat }
        satisfy (@n: Nat, @_ok: Holds(n < 10)) => Show(Small(n)) { show(_s) = "small" }
        /std/print(Show/show(Small(3) { 0 }))
        "#),
        b"small"
    );
}

// A late subject that makes the bound false is refused, and the refusal says what the bound came to, which the insertion could not: the bound was still waiting then.
#[test]
fn a_late_pinned_bound_that_fails_reports_what_it_reduces_to() {
    let error = typecheck(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{Holds};
        let need(@n: Nat, @_ok: Holds(n < 10), _witness: Eq()(n, 30)) -> Nat = n;
        let g: Nat = need(Eq/refl());
        /std/print("unreachable")
        "#,
    )
    .expect_err("30 is not below 10");

    assert!(
        error.contains("nothing discharged Holds(30 < 10), which reduces to /std/Bool/False"),
        "unexpected report: {error}"
    );
}

// A bound that is an inductive proposition as written reduces to itself, and the report says it once: the clause that tells a decided bound from the `False` it reduces to has nothing to add where reduction moved nothing.
#[test]
fn a_refused_bound_reduction_does_not_move_is_said_once() {
    let error = typecheck(
        r#"
        use /std/{Nat, proved};
        induct Even: (Nat) -> Prop | zero(): (0) end
        let other(n: Nat, m: Nat, p: Even(m)) -> Even(n) = proved();
        /std/print("unreachable")
        "#,
    )
    .expect_err("nothing proves an arbitrary number even");

    assert!(error.contains("nothing discharged Even(n)"), "{error}");
    assert!(!error.contains("which reduces to"), "{error}");
}

// A late fill is a metavariable solution, decided under the refinements its slot was born under: a bound true only under `m < 10` is filled when its subject arrives late inside that arm, as it is when written first — `need(@m, …)`. The control is the same bound with its slot born outside the arm, at a call whose witness is the match: the guard is not one the slot was born under, so the bound is refused.
#[test]
fn a_late_pinned_bound_holds_under_the_guard_its_slot_was_born_under() {
    typecheck(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{Holds};
        let need(m: Nat, @n: Nat, @_ok: Holds(n < 10), _witness: Eq()(n, m)) -> Nat = n;
        let f(m: Nat) -> Nat =
            match m < 10 | true => need(m, Eq/refl()) | false => 0 end;
        /std/print("ok")
        "#,
    )
    .expect("the slot was born in the arm, so the arm's guard fills it");

    let error = typecheck(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{Holds};
        let need(m: Nat, @n: Nat, @_ok: Holds(n < 10), _witness: Eq()(n, m)) -> Nat = n;
        let f(m: Nat) -> Nat =
            need(m, match m < 10 | true => Eq/refl() | false => Eq/refl() end);
        /std/print("unreachable")
        "#,
    )
    .expect_err("a slot born outside the arm does not rest on its guard");

    assert!(
        error.contains("nothing discharged Holds(m < 10)"),
        "unexpected report: {error}"
    );
}

// **A bound whose proposition is pinned after its insertion is filled on retry.** `proved`'s `p` has type `P`, an unsolved metavariable when the call inserts it, so it is a bound only because `P`'s own type is `Prop`: the sort is read off the metavariable's type rather than defaulted to `Type`, the bound is parked, and the expectation's `Holds(n <= n + 1)` brings it to truth once `P` is solved. The control is a proposition that reduces to `False`, which the retry refuses as it would a bound known at insertion.
#[test]
fn a_bound_whose_proposition_is_pinned_later_is_filled_on_retry() {
    assert_eq!(
        run(r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        let proved(@P: Prop, @p: P) -> P = p;
        let filled(n : Nat) -> Holds(n <= n + 1) = proved();
        /std/print("ok")
        "#),
        b"ok"
    );

    let error = typecheck(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{Holds};
        let proved(@P: Prop, @p: P) -> P = p;
        let refused(n : Nat) -> Holds(n + 1 <= n) = proved();
        /std/print("unreachable")
        "#,
    )
    .expect_err("a proposition that reduces to False is not filled");

    assert!(
        error.contains("nothing discharged Holds(n + 1 <= n), which reduces to Bool/False"),
        "expected the undischarged bound, got: {error}"
    );
}

// **The retry sees the guard its slot was born under.** A fill made on retry is a metavariable solution, so it is decided as re-validation decides one: under `a < b`, a `proved()` whose proposition the annotation pins afterwards is filled on retry, as the same call with its proposition written is at insertion. The control is a `proved()` made before the match and pinned inside its arm: its slot was born outside the arm, so the guard does not fill it.
#[test]
fn a_bound_pinned_later_holds_under_the_guard_its_slot_was_born_under() {
    assert_eq!(
        run(r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        let proved(@P: Prop, @p: P) -> P = p;
        let guarded(a : Nat, b : Nat) -> Nat =
            match a < b | true => let q: Holds(a < b) = proved(); a | false => b end;
        /std/print("ok")
        "#),
        b"ok"
    );

    let error = typecheck(
        r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        let proved(@P: Prop, @p: P) -> P = p;
        let guarded(a : Nat, b : Nat) -> Nat =
            let early = proved();
            match a < b | true => let q: Holds(a < b) = early; a | false => b end;
        /std/print("unreachable")
        "#,
    )
    .expect_err("a slot born outside the arm does not rest on its guard");

    assert!(
        error.contains("nothing discharged"),
        "expected the undischarged bound, got: {error}"
    );
}
