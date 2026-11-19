//! A successor pattern's step: `p + k` takes `k` successors apart, its predecessor what is left of the value.

use {crate::tests::test_support::*, curios_common::fill};

// `half` halves by a step of two, its `_` taking the one value the step leaves, so each arm answers as stated and the recursion descends on the predecessor; the law at `k + 2` holds by reduction alone, through both checkers.
#[test]
fn a_successor_pattern_takes_its_step_apart() {
    let source = r#"
        use /std/{Eq, List, Nat, Str};

        let half(n: Nat) -> Nat =
            match n
            | 0 => 0
            | p + 2 => 1 + half(p)
            | _ => 0
            end;

        let law(k: Nat) -> Eq()(half(k + 2), 1 + half(k)) = Eq/refl();

        /std/print(Str/join(" ", List/map([half(0), half(1), half(2), half(7), half(10)], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"0 0 1 3 5");
}

// A binder the arms before it leave one value is that value's arm, typed as the value it is: past `0` and every `p + 2`, `q` is `1`.
#[test]
fn a_binder_past_a_step_stands_for_the_value_it_leaves() {
    let source = r#"
        use /std/{Eq, Nat};

        let f(n: Nat) -> Nat =
            match n
            | 0 => 0
            | p + 2 => p
            | q => let one: Eq()(q, 1) = Eq/refl(); 7
            end;

        /std/print(Nat/to_str(f(1) + f(5)))
        "#;

    assert_eq!(run(source), b"10");
}

// A step nested under a constructor takes its payload apart as one at the column does.
#[test]
fn a_nested_successor_pattern_takes_its_step_apart() {
    let source = r#"
        use /std/{List, Nat, Option, Str};

        let f(o: Option(Nat)) -> Nat =
            match o
            | some(0) => 0
            | some(p + 3) => p
            | some(_) => 1
            | none() => 2
            end;

        /std/print(Str/join(" ", List/map([f(Option/some(0)), f(Option/some(2)), f(Option/some(10)), f(Option/none())], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"0 1 7 2");
}

// A row reached at several places is typed once along its own patterns, and its step is taken apart there as the tree takes it: `(_, p + 2)` is reached under both truth values, its predecessor read as a column of its own.
#[test]
fn a_successor_row_reached_at_several_places_takes_its_step_apart() {
    let source = r#"
        use /std/{Bool, List, Nat, Str};

        let f(b: Bool, n: Nat) -> Nat =
            match (b, n)
            | (true, 0) => 1
            | (_, p + 2) => p + 10
            | (_, _) => 2
            end;

        /std/print(Str/join(" ", List/map([f(true, 0), f(true, 1), f(false, 0), f(true, 5), f(false, 7)], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"1 2 2 13 15");
}

// A step past the values no other row takes leaves the least of them to no arm, named as the number it is.
#[test]
fn a_step_past_a_value_no_arm_takes_names_it() {
    for (arms, missing) in [
        ("| 0 => 0 | p + 2 => p", "`1`"),
        ("| 0 => 0 | p + 3 => p", "`1`"),
        ("| p + 2 => p", "`0`"),
    ] {
        let error = error(&fill!(
            r#"
            use /std/{Nat};
            let f(n: Nat) -> Nat = match n ${arms} end;
            /std/print(Nat/to_str(f(4)))
            "#,
            arms
        ));
        assert!(
            error.contains("no arm matches") && error.contains(missing),
            "{arms}: unexpected error: {error}"
        );
    }
}
