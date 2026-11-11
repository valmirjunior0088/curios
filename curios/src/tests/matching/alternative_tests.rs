//! An arm's alternatives: the arms they abbreviate, each typed under its own cases, matching no value in common and binding the same names.

use crate::tests::test_support::*;

#[test]
fn an_arm_is_taken_by_a_value_any_of_its_alternatives_matches() {
    let source = r#"
        use /std/{Nat, print};
        induct Shape: Type
        | circle(Nat)
        | square(Nat)
        | point()
        end
        let size(s: Shape) -> Nat =
            match s
            | circle(n) | square(n) => n
            | point() => 0
            end;
        let blank(c: Nat) -> Nat =
            match c
            | ' ' | '\t' | '\n' => 1
            | _ => 0
            end;
        print(Nat/to_str(size(Shape/circle(3)) + size(Shape/square(4)) + size(Shape/point()) + blank(9) + blank(97)))
        "#;

    assert_eq!(run(source), b"8");
}

/// Each alternative binds its names where it writes them and at the types they have there: `x` is a `Nat` under `left` and a `Str` under `right`, and the body finds a witness for each.
#[test]
fn a_name_stands_where_each_alternative_writes_it_at_the_type_it_has_there() {
    let source = r#"
        use /std/{Nat, Str, Show, print};
        induct Either: Type
        | left(Nat)
        | right(Str, Nat)
        end
        let shown(e: Either) -> Str =
            match e
            | left(x) | right(x, _) => Show/show(x)
            end;
        print(Str/concat(shown(Either/left(3)), shown(Either/right("hi", 4))))
        "#;

    assert_eq!(run(source), b"3hi");
}

/// The body is typed under each alternative's own case, so a proof that holds case by case is written once: `Eq/refl()` holds at `true` and at `false`, and not at a `b` neither case fixes.
#[test]
fn a_proof_that_holds_case_by_case_is_written_once_for_its_alternatives() {
    let proof = |body: &str| {
        format!(
            r#"
            use /std/{{Bool, Eq, print}};
            let flip(b: Bool) -> Bool = match b | true => false | false => true end;
            let twice(b: Bool) -> Eq()(flip(flip(b)), b) = {body};
            print("ok")
            "#
        )
    };

    assert_eq!(
        run(&proof("match b | true | false => Eq/refl() end")),
        b"ok"
    );
    let error = error(&proof("Eq/refl()"));
    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}

/// An alternative that is a case of another adds nothing to the arm, in whichever order the two are written.
#[test]
fn an_alternative_that_is_a_case_of_another_is_refused() {
    for arm in ["some(0) | some(_)", "some(_) | some(0)"] {
        let source = format!(
            r#"
            use /std/{{Nat, Option, print}};
            let f(o: Option(Nat)) -> Nat = match o | {arm} => 1 | none() => 0 end;
            print(Nat/to_str(f(Option/none())))
            "#
        );

        let error = error(&source);
        assert!(
            error.contains("the alternative `some(0)` is a case of `some(_)` in the same arm"),
            "unexpected error for {arm}: {error}"
        );
    }
}

/// Two alternatives that meet would leave which typing a shared value takes to the order they are written in.
#[test]
fn alternatives_that_meet_are_refused_with_the_value_they_share() {
    let source = r#"
        use /std/{Nat, Bool, print};
        let f(p: {Bool, Bool}) -> Nat =
            match p
            | (true, _) | (_, true) => 1
            | (false, false) => 0
            end;
        print(Nat/to_str(f((true, false))))
        "#;

    let error = error(source);
    assert!(
        error.contains(
            "the alternatives `(true, _)` and `(_, true)` of one arm both match `(true, true)`"
        ),
        "unexpected error: {error}"
    );
}

/// Against the arms beside it an alternative is held as an arm is: one a general arm before it covers is reached by no value.
#[test]
fn an_alternative_an_arm_before_it_covers_is_refused() {
    let source = r#"
        use /std/{Nat, Option, print};
        let f(o: Option(Nat)) -> Nat =
            match o
            | some(_) => 1
            | some(0) | none() => 2
            end;
        print(Nat/to_str(f(Option/none())))
        "#;

    let error = error(source);
    assert!(
        error.contains("no value reaches this one past the arms before it, `some(_)` among them"),
        "unexpected error: {error}"
    );
}

/// What one alternative's typing refuses is reported with the alternative, since the body alone does not say under which it failed.
#[test]
fn a_refusal_under_one_alternative_names_it() {
    let source = r#"
        use /std/{Nat, Str, print};
        induct Either: Type
        | left(Nat)
        | right(Str)
        end
        let next(e: Either) -> Nat =
            match e
            | left(x) | right(x) => x + 1
            end;
        print(Nat/to_str(next(Either/left(3))))
        "#;

    let error = error(source);
    assert!(
        error.contains("under the alternative `right(x)`"),
        "unexpected error: {error}"
    );
}

#[test]
fn a_bind_arm_fires_where_any_of_its_alternatives_matches() {
    let source = r#"
        use /std/{Nat, Option, print};
        let small(o: Option(Nat)) -> Nat =
            choose
            | some(0) | some(1) = o => 1
            | _ => 0
            end;
        print(Nat/to_str(small(Option/some(1)) + small(Option/some(2)) + small(Option/none())))
        "#;

    assert_eq!(run(source), b"1");
}
