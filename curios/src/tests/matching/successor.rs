//! A successor pattern's step, `p + k` taking `k` successors apart, its predecessor what is left of the value, and the literals beside it, each the successors it builds over zero.

use {crate::test_support::*, curios_common::fill};

// `half` halves by a step of two, its `_` taking the one value the step leaves, so each arm answers as stated and the recursion descends on the predecessor; the law at `k + 2` holds by reduction alone, through both checkers.
#[test]
fn a_successor_pattern_takes_its_step_apart() {
    let source = r#"
        use /std/{Eq, List, Nat, Str};

        let half(n: Nat) -> Nat =>
            match n
            | 0 => 0
            | p + 2 => 1 + half(p)
            | _ => 0
            end;

        let law(k: Nat) -> Eq()(half(k + 2), 1 + half(k)) => Eq/refl();

        /std/print(Str/join(" ", List/map([half(0), half(1), half(2), half(7), half(10)], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"0 0 1 3 5");
}

// A binder the arms before it leave one value is that value's arm, typed as the value it is: past `0` and every `p + 2`, `q` is `1`.
#[test]
fn a_binder_past_a_step_stands_for_the_value_it_leaves() {
    let source = r#"
        use /std/{Eq, Nat};

        let f(n: Nat) -> Nat =>
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

        let f(o: Option(Nat)) -> Nat =>
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

        let f(b: Bool, n: Nat) -> Nat =>
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
            let f(n: Nat) -> Nat => match n ${arms} end;
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

// A Fibonacci written by its literals and a step of two refines each arm to its case, so its law at `k + 2` holds by reduction alone, through both checkers.
#[test]
fn literals_beside_a_step_refine_each_arm() {
    let source = r#"
        use /std/{Eq, Nat};

        let fib(n: Nat) -> Nat =>
            match n
            | 0 => 0
            | 1 => 1
            | p + 2 => fib(p) + fib(p + 1)
            end;

        let law(k: Nat) -> Eq()(fib(k + 2), fib(k) + fib(k + 1)) => Eq/refl();

        /std/print(Nat/to_str(fib(10)))
        "#;

    assert_eq!(run(source), b"55");
}

// Literals and successor arms meet as every pattern does: a literal a successor arm takes is an exception written before it, below the step or past it, and a literal far past the step stays one test.
#[test]
fn literals_beside_a_step_answer_as_stated() {
    for (arms, calls, expected) in [
        (
            "| 0 => 0 | 7 => 70 | p + 1 => p",
            "f(0), f(1), f(7), f(9)",
            "0 0 70 8",
        ),
        (
            "| 0 => 0 | 1 => 10 | 7 => 70 | p + 2 => p",
            "f(0), f(1), f(2), f(7), f(9)",
            "0 10 0 70 7",
        ),
        (
            "| 5 => 50 | p + 3 => p | _ => 99",
            "f(0), f(2), f(3), f(5), f(8)",
            "99 99 0 50 5",
        ),
        (
            "| 1000 => 1 | p + 2 => p | _ => 99",
            "f(0), f(1), f(2), f(1000), f(1001)",
            "99 99 0 1 999",
        ),
    ] {
        let source = fill!(
            r#"
            use /std/{List, Nat, Str};
            let f(n: Nat) -> Nat => match n ${arms} end;
            /std/print(Str/join(" ", List/map([${calls}], Nat/to_str)))
            "#,
            arms,
            calls
        );

        assert_eq!(run(&source), expected.as_bytes(), "{arms}");
    }
}

// The arm past an exception knows its value is not the exception: `p + 2` past `7` knows `n != 7`, which discharges the bound it is called with.
#[test]
fn a_step_past_an_exception_knows_the_exception_is_not_its_value() {
    let source = r#"
        use /std/{Bool, Nat};

        let need(n: Nat, @ok: Bool/Holds(n != 7)) -> Nat => n;

        let f(n: Nat) -> Nat =>
            match n
            | 0 => 0
            | 1 => 1
            | 7 => 70
            | p + 2 => need(p + 2)
            end;

        /std/print(Nat/to_str(f(7) + f(9)))
        "#;

    assert_eq!(run(source), b"79");
}

// A literal nested under a constructor meets a step there as it does at the column.
#[test]
fn literals_beside_a_nested_step_answer_as_stated() {
    let source = r#"
        use /std/{List, Nat, Option, Str};

        let f(o: Option(Nat)) -> Nat =>
            match o
            | some(0) => 10
            | some(1) => 11
            | some(p + 2) => p
            | none() => 99
            end;

        /std/print(Str/join(" ", List/map([f(Option/some(0)), f(Option/some(1)), f(Option/some(6)), f(Option/none())], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"10 11 4 99");
}

// A row reached at several places is typed once along its own patterns, a literal at a column the tree peels for another row's step among them.
#[test]
fn a_literal_row_reached_at_several_places_meets_a_step() {
    let source = r#"
        use /std/{Bool, List, Nat, Str};

        let f(b: Bool, n: Nat) -> Nat =>
            match (b, n)
            | (true, 0) => 1
            | (_, 7) => 70
            | (_, p + 2) => p
            | (_, _) => 2
            end;

        /std/print(Str/join(" ", List/map([f(true, 0), f(false, 0), f(true, 7), f(false, 7), f(true, 9), f(false, 1)], Nat/to_str)))
        "#;

    assert_eq!(run(source), b"1 2 70 70 7 2");
}

// A literal a successor arm before it takes is reached by no value, and a successor arm with no zero arm leaves `0` to none.
#[test]
fn a_literal_past_a_step_is_refused_as_unreachable() {
    for (arms, refusal) in [
        ("| 0 => 0 | p + 1 => p | 3 => 3", "unreachable arm"),
        ("| p + 1 => p | 3 => 3", "no arm matches `0`"),
    ] {
        let error = error(&fill!(
            r#"
            use /std/{Nat};
            let f(n: Nat) -> Nat => match n ${arms} end;
            /std/print(Nat/to_str(f(4)))
            "#,
            arms
        ));
        assert!(error.contains(refusal), "{arms}: unexpected error: {error}");
    }
}
