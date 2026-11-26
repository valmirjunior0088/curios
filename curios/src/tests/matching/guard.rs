//! Guards: the tests an arm writes after its patterns, what a guard that fails teaches the arms after it, and guards at each level of a recursion.

use {crate::test_support::*, curios_common::fill};

// A guarded arm takes the values its guards pass, from the left, each only where the ones before it held: `100 / n` reads `n != 0` from the guard before it. A value whose guards fail goes on to the arms after it, in the order they are written. Runtime-tainted, so it runs as wasm.
#[test]
fn a_value_whose_guards_fail_goes_on_to_the_arms_after_it() {
    let source = r#"
        use /std/{Nat, Option, Bytes, rand};
        let size(o: Option(Nat)) -> Nat =
            match o
            | some(n), n > 9 => 2
            | some(n), n != 0, 100 / n > 20 => 1
            | _ => 0
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            let a = size(Option/some(10 + z));
            let b = size(Option/some(3 + z));
            let c = size(Option/some(z));
            let d = size(Option/none());
            /std/print(Nat/to_str(a * 1000 + b * 100 + c * 10 + d));
        entry()
        "#;

    // some(10) → 2; some(3) → 1, 100 / 3 is above 20; some(0) fails `n != 0` → 0; none() → 0.
    assert_eq!(run(source), b"2100");
}

// A bind guard matches a value computed from what the arm bound and binds its names for the guards after it and the body.
#[test]
fn a_bind_guard_binds_for_the_guards_after_it_and_the_body() {
    let source = r#"
        use /std/{Nat, Option, Bytes, rand};
        let inner(o: Option(Option(Nat))) -> Nat =
            match o
            | some(held), some(n) = held, n > 1 => n
            | _ => 0
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            let a = inner(Option/some(Option/some(7 + z)));
            let b = inner(Option/some(Option/some(z)));
            let c = inner(Option/some(Option/none()));
            /std/print(Nat/to_str(a * 100 + b * 10 + c));
        entry()
        "#;

    assert_eq!(run(source), b"700");
}

// Where an arm's one guard is a condition and fails, the arms after it at that case are read under its failure: `100 / n` needs `n != 0`, which the failure of `n == 0` states. A chain that fails, or a bind guard that fails, failed in more ways than one and teaches nothing.
#[test]
fn a_failed_lone_condition_is_a_fact_in_the_arms_after_it_and_a_failed_chain_is_none() {
    let lone = r#"
        use /std/{Nat, Option};
        let share(o: Option(Nat)) -> Nat =
            match o
            | some(n), n == 0 => 0
            | some(n) => 100 / n
            | none() => 0
            end;
        /std/print(Nat/to_str(share(Option/some(4))))
        "#;
    assert_eq!(run(lone), b"25");

    let chain = r#"
        use /std/{Nat, Option};
        let share(o: Option(Nat)) -> Nat =
            match o
            | some(n), n < 5, n == 0 => 0
            | some(n) => 100 / n
            | none() => 0
            end;
        /std/print(Nat/to_str(share(Option/some(4))))
        "#;
    let report = error(chain);
    assert!(
        report.contains("the bound of the `/` operator was not discharged"),
        "{report}"
    );
}

// A place only guarded arms reach leaves a value their guards refuse with no arm, and the refusal names the last of them.
#[test]
fn a_value_only_a_guarded_arm_reaches_is_refused() {
    let report = error(
        r#"
        use /std/{Nat, Option};
        let f(o: Option(Nat)) -> Nat =
            match o
            | some(n), n > 9 => 2
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/none())))
        "#,
    );
    assert!(
        report.contains("no arm matches `some(_)` where the guards of `some(n)` fail"),
        "{report}"
    );
}

// Guards order no pattern: a guarded arm stands after the arms that are cases of it, as one with no guard does, so a value is never handed by a guard to an arm its pattern would have made unreachable.
#[test]
fn a_guarded_arm_before_a_case_of_it_is_refused() {
    let report = error(
        r#"
        use /std/{Nat, Option};
        let f(o: Option(Nat), flag: /std/Bool) -> Nat =
            match o
            | _, flag => 0
            | some(n) => n
            | none() => 1
            end;
        /std/print(Nat/to_str(f(Option/none(), true)))
        "#,
    );
    assert!(
        report.contains("`some(n)` is a case of `_`, which is written before it"),
        "{report}"
    );
}

// An arm's guards are read under each of its alternatives, each typed apart, as the arm's body is.
#[test]
fn guards_are_read_under_each_alternative() {
    let source = r#"
        use /std/{Nat, Option};
        let either(a: Option(Nat), b: Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), none()) | (none(), some(x)), x > 3 => x
            | _ => 0
            end;
        /std/print(Nat/to_str(either(Option/some(5), Option/none()) + either(Option/none(), Option/some(2))))
        "#;

    assert_eq!(run(source), b"5");
}

// A guarded arm whose binder stands beside another arm's case is typed once, by its own walk, for every place it reaches, each place supplying what its failure takes there.
#[test]
fn a_guarded_arm_typed_once_for_several_places_falls_through_at_each() {
    let source = r#"
        use /std/{Nat, Option};
        let sum(a: Option(Nat), b: Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (_, some(y)), y > 3 => y
            | _ => 0
            end;
        /std/print(Nat/to_str(
            sum(Option/none(), Option/some(9)) * 100
                + sum(Option/none(), Option/some(1)) * 10
                + sum(Option/some(1), Option/some(1))))
        "#;

    assert_eq!(run(source), b"902");
}

// A bind whose pattern takes nothing apart matches every value: it is a `let`, and is refused as a guard.
#[test]
fn a_bind_guard_that_cannot_fail_is_refused() {
    let report = error(
        r#"
        use /std/{Nat, Option};
        let f(o: Option(Nat)) -> Nat =
            match
            | x = o => 0
            | _ => 1
            end;
        /std/print(Nat/to_str(f(Option/none())))
        "#,
    );
    assert!(
        report.contains("a bind, `pattern = value`, takes a pattern that can fail"),
        "{report}"
    );
}

// A recursion's arm is taken at every level, and its guards run at each: a guard on the predecessor, one that reads the call at the predecessor, and one on the base case. A lone condition that fails at a level is a fact in the arms after it there: `10 / k` reads `k != 0`. Runtime-tainted, so each runs as wasm.
#[test]
fn guards_run_at_every_level_of_a_recursion() {
    let source = r#"
        use /std/{Bool, List, Nat, Str, Bytes, rand};
        let count(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1, k > 2 => count(k) + 1
            | k + 1 => count(k)
            end;
        let capped(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1, capped(k) > 3 => capped(k)
            | k + 1 => capped(k) + 1
            end;
        let based(flag: Bool, n: Nat) -> Nat =
            match n
            | 0, flag => 100
            | 0 => 0
            | k + 1 => based(flag, k) + 1
            end;
        let big(l: List(Nat)) -> Nat =
            match l
            | [] => 0
            | [h, ..t], h > 2 => big(t) + h
            | [_, ..t] => big(t)
            end;
        let shares(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1, k == 0 => shares(k)
            | k + 1 => shares(k) + 10 / k
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            /std/print(Str/join(" ", [
                Nat/to_str(count(6 + z)),
                Nat/to_str(capped(10 + z)),
                Nat/to_str(based(true, 3 + z) + based(false, 2 + z)),
                Nat/to_str(big([1 + z, 5, 3])),
                Nat/to_str(shares(4 + z))]));
        entry()
        "#;

    assert_eq!(run(source), b"3 4 105 8 18");
}

// A guard may call the recursion its arm stands in, and where it fails the row that cannot fail takes the value: `even(k)` is the predecessor's parity, read by nothing but the guard.
#[test]
fn a_guard_may_call_the_recursion_its_arm_stands_in() {
    let source = r#"
        use /std/{Bool, Nat, Str, Bytes, rand};
        let even(n: Nat) -> Bool =
            match n
            | 0 => true
            | k + 1, even(k) => false
            | _ => true
            end;
        let shown(b: Bool) -> Str = match b | true => "even" | false => "odd" end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            /std/print(Str/join(" ", [shown(even(7 + z)), shown(even(4 + z)), shown(even(z))]));
        entry()
        "#;

    assert_eq!(run(source), b"odd even even");
}

// A bind guard takes a recursive call apart: the last element above 2, found from the end of the list.
#[test]
fn a_bind_guard_takes_a_recursive_call_apart() {
    let source = r#"
        use /std/{List, Nat, Option, Str, Bytes, rand};
        let last_big(l: List(Nat)) -> Option(Nat) =
            match l
            | [] => Option/none()
            | [_, ..t], some(v) = last_big(t) => Option/some(v)
            | [h, .._], h > 2 => Option/some(h)
            | [_, .._] => Option/none()
            end;
        let shown(o: Option(Nat)) -> Str = match o | some(v) => Nat/to_str(v) | none() => "-" end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            /std/print(Str/join(" ", [
                shown(last_big([5 + z, 1, 3, 0])),
                shown(last_big([1 + z, 2])),
                shown(last_big([4 + z]))]));
        entry()
        "#;

    assert_eq!(run(source), b"3 - 4");
}

// A chain that fails at a level falls to the arms after it there, the next with a guard of its own, each reading the call at the predecessor; and a row that cannot fail stands both for the base case and for a level whose guard fails.
#[test]
fn a_chain_failing_at_a_level_falls_through_the_arms_after_it() {
    let source = r#"
        use /std/{Nat, Str, Bytes, rand};
        let ramp(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1, k > 5, ramp(k) > 10 => ramp(k)
            | k + 1, k > 2 => ramp(k) + 2
            | k + 1 => ramp(k) + 1
            end;
        let late(n: Nat) -> Nat =
            match n
            | k + 1, k > 2 => late(k) + 1
            | _ => 7
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            /std/print(Str/join(" ", [
                Nat/to_str(ramp(6 + z)),
                Nat/to_str(ramp(7 + z)),
                Nat/to_str(ramp(9 + z)),
                Nat/to_str(late(z)),
                Nat/to_str(late(3 + z)),
                Nat/to_str(late(5 + z))]));
        entry()
        "#;

    // ramp: 1, 2, 3 by the last arm, 5, 7, 9 by the second, then at k = 6 the chain fails on `ih > 10` and the second takes 11, which the chain then keeps. late: 7 up to 3, then one more a step.
    assert_eq!(run(source), b"9 11 11 7 7 9");
}

// Guards read every shape of recursive call: a tuple one projected, a function one applied, and one over a packed sequence.
#[test]
fn guards_read_a_recursive_call_projected_applied_and_over_bytes() {
    let source = r#"
        use /std/{Byte, List, Nat, Str, Bytes, rand};
        let tally(l: List(Nat)) -> {Nat, Nat} =
            match l
            | [] => (0, 0)
            | [h, ..t], h > 2, tally(t).0 < 2 => (tally(t).0 + 1, tally(t).1 + h)
            | [_, ..t] => tally(t)
            end;
        let skip(l: List(Nat)) -> (Nat) -> Nat =
            match l
            | [] => (a) => a
            | [h, ..t], skip(t)(h) > 10 => (a) => skip(t)(a)
            | [h, ..t] => (a) => skip(t)(a + h)
            end;
        let high(b: Bytes) -> Nat =
            match b
            | 0x[] => 0
            | 0x[h, ..t], Byte/to_nat(h) > 127 => high(t) + 1
            | 0x[_, ..t] => high(t)
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            let (count, total) = tally([1 + z, 5, 3, 7, 2]);
            /std/print(Str/join(" ", [
                Nat/to_str(count),
                Nat/to_str(total),
                Nat/to_str(skip([3 + z, 20, 1])(0)),
                Nat/to_str(high(0x[0x80, 0x01, 0xFF, ..rand/bytes(0)!]))]));
        entry()
        "#;

    // tally keeps the last two above 2, 7 and 3; skip drops 20, whose rest already passes 10; high counts 0x80 and 0xFF.
    assert_eq!(run(source), b"2 10 4 2");
}

// A guard may hold a match of its own, a list's column may stand beside another the guard reads, and a guarded arm typed once by its own walk beside the list's column falls through at the level.
#[test]
fn a_guard_holds_a_match_and_reads_the_columns_beside_one() {
    let source = r#"
        use /std/{Bool, List, Nat, Str, Bytes, rand};
        let even(n: Nat) -> Bool = match n | 0 => true | k + 1 => even(k) == false end;
        let evens(l: List(Nat)) -> Nat =
            match l
            | [] => 0
            | [h, ..t], (match h | 0 => true | k + 1 => even(k) == false end) => evens(t) + 1
            | [_, ..t] => evens(t)
            end;
        let over(l: List(Nat), m: Nat) -> Nat =
            match (l, m)
            | ([], _) => 0
            | ([h, ..t], k), h > k => over(t, m) + 1
            | ([_, ..t], _) => over(t, m)
            end;
        let stray(l: List(Nat), m: Nat) -> Nat =
            match (l, m)
            | ([], _) => 0
            | ([_, ..t], 0) => stray(t, 0) + 1
            | (_, k), k > 5 => k
            | _ => 1
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/length(rand/bytes(0)!);
            /std/print(Str/join(" ", [
                Nat/to_str(evens([1 + z, 2, 3, 4, 6])),
                Nat/to_str(over([1 + z, 5, 3, 9], 2)),
                Nat/to_str(stray([1 + z, 2, 3], 0)),
                Nat/to_str(stray([1 + z, 2, 3], 7)),
                Nat/to_str(stray([1 + z, 2, 3], 2)),
                Nat/to_str(stray([], 9 + z))]));
        entry()
        "#;

    assert_eq!(run(source), b"3 3 3 7 1 0");
}

// A proof by induction whose step a guard splits: where `n < j` holds, the call at the predecessor takes the premise the guard states, and where it fails, the failure is the `j <= n` the arm after it reads. A chain that fails teaches nothing, so the same proof under a second guard is refused.
#[test]
fn a_failed_guard_at_a_step_is_a_fact_a_proof_by_induction_reads() {
    let proof = |guards: &str| {
        fill!(
            r#"
            use /std/{Nat, Bool, Eq, proved};
            use /std/Bool/{True, Holds};
            let below(k: Nat, p: (Nat) -> Bool) -> Bool =
                match k | 0 => true | j + 1 => p(j) && below(j, p) end;
            let at(k: Nat, p: (Nat) -> Bool, n: Nat, all: Holds(below(k, p)), under: Holds(n < k)) -> Holds(p(n)) =
                match k
                | 0 => match under end
                | j + 1, ${guards} => at(j, p, n, Bool/and/second(p(j), below(j, p), all), proved())
                | j + 1 =>
                    Eq/subst(
                        (m: Nat) => Holds(p(m)),
                        Nat/le/antisym(j, n, proved(), proved()),
                        Bool/and/first(p(j), below(j, p), all))
                end;
            let small: Holds(3 < 9) = at(5, (x) => x < 9, 3, True/qed(), True/qed());
            /std/print("ok")
            "#,
            guards
        )
    };

    assert_eq!(run(&proof("n < j")), b"ok");
    let report = error(&proof("n < j, true"));
    assert!(
        report.contains("nothing discharged `Holds(j <= n)`"),
        "{report}"
    );
}

// Conversion runs a recursion whose arms take guards, as the program does: `count(6)` is 3 by reduction alone, and 4 is refused.
#[test]
fn conversion_runs_a_recursion_whose_arms_take_guards() {
    let claim = |value: &str| {
        fill!(
            r#"
            use /std/{Nat};
            use /std/Bool/{True, Holds};
            let count(n: Nat) -> Nat =
                match n
                | 0 => 0
                | k + 1, k > 2 => count(k) + 1
                | k + 1 => count(k)
                end;
            let counted: Holds(count(6) == ${value}) = True/qed();
            /std/print("ok")
            "#,
            value
        )
    };

    assert_eq!(run(&claim("3")), b"ok");
    let report = error(&claim("4"));
    assert!(report.contains("type mismatch"), "{report}");
}

// A match its guards flatten converts with the nested match it was written as: the catch-all the guard falls through to is the arm of `none()`, which the nested match writes, and the function of no argument it is bound to, called where `pred(a)` fails, is the nested `false` arm.
#[test]
fn a_match_its_guards_flatten_converts_with_the_nested_one() {
    let source = r#"
        use /std/{Bool, Nat, Option, Eq};
        let nested(m: Option(Nat), pred: (Nat) -> Bool) -> Option(Nat) =
            match m
            | some(a) => match pred(a) | true => Option/some(a) | false => Option/none() end
            | none() => Option/none()
            end;
        let guarded(m: Option(Nat), pred: (Nat) -> Bool) -> Option(Nat) =
            match m | some(a), pred(a) => Option/some(a) | _ => Option/none() end;
        let same(m: Option(Nat), pred: (Nat) -> Bool) -> Eq()(nested(m, pred), guarded(m, pred)) =
            Eq/refl();
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A `Nat` or a list case split is held to the rules of guards as any match is: a base case only a guarded arm reaches is refused, and so is a guarded arm written before a case of it.
#[test]
fn a_sequence_split_with_guards_is_held_to_coverage_and_order() {
    let uncovered = error(
        r#"
        use /std/{Bool, Nat};
        let f(flag: Bool, n: Nat) -> Nat =
            match n
            | 0, flag => 1
            | k + 1 => f(flag, k)
            end;
        /std/print(Nat/to_str(f(true, 3)))
        "#,
    );
    assert!(
        uncovered.contains("no arm matches `0` where the guards of `0` fail"),
        "{uncovered}"
    );

    let misplaced = error(
        r#"
        use /std/{List, Nat};
        let f(l: List(Nat), m: Nat) -> Nat =
            match (l, m)
            | ([], _) => 0
            | ([_, ..t], _), m > 3 => f(t, m)
            | ([_, ..t], 0) => f(t, 0) + 1
            | ([_, ..t], _) => f(t, m)
            end;
        /std/print(Nat/to_str(f([1, 2], 0)))
        "#,
    );
    assert!(
        misplaced
            .contains("`([_, ..t], 0)` is a case of `([_, ..t], _)`, which is written before it"),
        "{misplaced}"
    );
}

// A guarded arm read as the function of what its failures take — two guards, or a guard on an arm its own walk types — is checked in a region whose type is a reduct: after a `!`, `Parse(Str, Nat)` stands unfolded, holding the method its carrier's sealed `Input` witness gives. The failure's type comes from the function type the arm is checked at and is not elaborated again, where that method is private to `/std/Parse`.
#[test]
fn a_guarded_arm_is_read_in_a_region_whose_type_holds_a_sealed_method() {
    let source = r#"
        use /std/{Char, Nat, Option, Parse, Result, Str};
        let two_guards() -> Parse(Str, Nat) =
            let c = Parse/str/char()!;
            match Option/some(Char/to_nat(c))
            | some(n), n > 50, n < 55 => Parse/pure(1)
            | _ => Parse/pure(0)
            end;
        let walked() -> Parse(Str, Nat) =
            let c = Parse/str/char()!;
            match Char/to_nat(c)
            | '/' => Parse/pure(2)
            | _, Char/is_ascii_digit(c) => Parse/pure(3)
            | _ => Parse/pure(4)
            end;
        let read(p: Parse(Str, Nat), s: Str) -> Str =
            match Parse/run(p, s) | success(n) => Nat/to_str(n) | failure(_) => "refused" end;
        /std/print(Str/join(" ", [read(two_guards(), "4"), read(walked(), "/"), read(walked(), "7"), read(walked(), "x")]))
        "#;

    assert_eq!(run(source), b"1 2 3 4");
}
