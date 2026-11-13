//! Guards: the tests an arm writes after its patterns, a match with no head, and what a guard that fails teaches the arms after it.

use {crate::tests::test_support::*, curios_common::fill};

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
            let z = Bytes/len(rand/bytes(0)!);
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
            let z = Bytes/len(rand/bytes(0)!);
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

// A match with no head is its guards alone: the arms are tried from the top, a condition that fails as an arm's one guard is a fact in every arm after it, the default included, and a bind that fails teaches it nothing.
#[test]
fn a_match_with_no_head_reads_its_arms_from_the_top() {
    let source = r#"
        use /std/{Nat, Option, Bytes, rand};
        let pick(x: Nat, o: Option(Nat)) -> Nat =
            match
            | x == 0 => 0
            | some(y) = o => y + 100 / x
            | _ => 10 / x
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            let a = pick(z, Option/some(1));
            let b = pick(2 + z, Option/some(1));
            let c = pick(5 + z, Option/none());
            /std/print(Nat/to_str(a * 1000 + b + c));
        entry()
        "#;

    // x = 0 → 0; x = 2 with some(1) → 1 + 50 = 51; x = 5 with none() → 2.
    assert_eq!(run(source), b"53");
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

// A match with no head that closes a program closes it in each arm, at each arm's type, as a headed match does.
#[test]
fn a_match_with_no_head_closes_a_program_in_each_arm() {
    let source = r#"
        use /std/{Nat, print};
        let n: Nat = 3;
        match
        | n > 2 => print("big")
        | _ => ()
        end
        "#;

    assert_eq!(run(source), b"big");
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

// A fold's arm is taken at every tail, and its guards run at each: a guard on the arm that writes the hypothesis, one that reads the hypothesis, and one on the base case. A lone condition that fails at a step is a fact in the arms after it at that step: `10 / k` reads `k != 0`. Runtime-tainted, so each runs as wasm.
#[test]
fn guards_run_at_every_step_of_a_fold() {
    let source = r#"
        use /std/{Bool, List, Nat, Str, Bytes, rand};
        let count(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1; ih, k > 2 => ih + 1
            | _ + 1; ih => ih
            end;
        let capped(n: Nat) -> Nat =
            match n
            | 0 => 0
            | _ + 1; ih, ih > 3 => ih
            | _ + 1; ih => ih + 1
            end;
        let based(flag: Bool, n: Nat) -> Nat =
            match n
            | 0, flag => 100
            | 0 => 0
            | _ + 1; ih => ih + 1
            end;
        let big(l: List(Nat)) -> Nat =
            match l
            | [] => 0
            | [h, .._]; acc, h > 2 => acc + h
            | [_, .._]; acc => acc
            end;
        let shares(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1; ih, k == 0 => ih
            | k + 1; ih => ih + 10 / k
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
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

// Where one arm alone writes the step and only its guard names the hypothesis, the guard is what makes the match a fold, as naming it in the body does: `before` is the predecessor's parity, read by nothing but the guard.
#[test]
fn a_guard_alone_naming_the_hypothesis_makes_the_match_a_fold() {
    let source = r#"
        use /std/{Bool, Nat, Str, Bytes, rand};
        let even(n: Nat) -> Bool =
            match n
            | 0 => true
            | _ + 1; before, before => false
            | _ => true
            end;
        let shown(b: Bool) -> Str = match b | true => "even" | false => "odd" end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            /std/print(Str/join(" ", [shown(even(7 + z)), shown(even(4 + z)), shown(even(z))]));
        entry()
        "#;

    assert_eq!(run(source), b"odd even even");
}

// A goal in a guard could read anything in scope, so it makes the match a fold as a goal in the body does: the hypothesis is in its scope though nothing names it.
#[test]
fn a_goal_in_a_guard_sees_the_hypothesis_of_a_fold() {
    let source = r#"
        use /std/{Nat};
        let f(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1; ih, k > ? => k
            | _ => 0
            end;
        /std/print(Nat/to_str(f(3)))
        "#;
    let report = typecheck(source).expect_err("a program with a written goal never compiles");

    assert!(report.contains("└─ [goal]"), "{report}");
    assert!(report.contains("ih : Nat"), "{report}");
}

// A bind guard takes the hypothesis apart, beside an arm that writes none: the last element above 2, found from the end of the list.
#[test]
fn a_bind_guard_takes_apart_the_hypothesis_of_a_fold() {
    let source = r#"
        use /std/{List, Nat, Option, Str, Bytes, rand};
        let last_big(l: List(Nat)) -> Option(Nat) =
            match l
            | [] => Option/none()
            | [_, .._]; found, some(v) = found => Option/some(v)
            | [h, .._], h > 2 => Option/some(h)
            | [_, .._] => Option/none()
            end;
        let shown(o: Option(Nat)) -> Str = match o | some(v) => Nat/to_str(v) | none() => "-" end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            /std/print(Str/join(" ", [
                shown(last_big([5 + z, 1, 3, 0])),
                shown(last_big([1 + z, 2])),
                shown(last_big([4 + z]))]));
        entry()
        "#;

    assert_eq!(run(source), b"3 - 4");
}

// A chain that fails at a step falls to the arms after it there, the next with a guard of its own, each reading that step's hypothesis; and a row that cannot fail stands both for the base case and for a step whose guard fails.
#[test]
fn a_chain_failing_at_a_step_falls_through_the_arms_after_it() {
    let source = r#"
        use /std/{Nat, Str, Bytes, rand};
        let ramp(n: Nat) -> Nat =
            match n
            | 0 => 0
            | k + 1; ih, k > 5, ih > 10 => ih
            | k + 1; ih, k > 2 => ih + 2
            | _ + 1; ih => ih + 1
            end;
        let late(n: Nat) -> Nat =
            match n
            | k + 1; ih, k > 2 => ih + 1
            | _ => 7
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
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

// Guards read every shape of hypothesis: one taken apart into a tuple, one that is a function, called in the guard under a written motive, and a packed fold's.
#[test]
fn guards_read_a_hypothesis_taken_apart_a_function_one_and_a_packed_folds() {
    let source = r#"
        use /std/{Byte, List, Nat, Str, Bytes, rand};
        let tally(l: List(Nat)) -> {Nat, Nat} =
            match l
            | [] => (0, 0)
            | [h, .._]; (count, total), h > 2, count < 2 => (count + 1, total + h)
            | [_, .._]; kept => kept
            end;
        let skip(l: List(Nat)) -> (Nat) -> Nat =
            match l: (_) => (Nat) -> Nat
            | [] => (a) => a
            | [h, .._]; rest, rest(h) > 10 => (a) => rest(a)
            | [h, .._]; rest => (a) => rest(a + h)
            end;
        let high(b: Bytes) -> Nat =
            match b
            | x[] => 0
            | x[h, .._]; n, Byte/to_nat(h) > 127 => n + 1
            | x[_, .._]; n => n
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            let (count, total) = tally([1 + z, 5, 3, 7, 2]);
            /std/print(Str/join(" ", [
                Nat/to_str(count),
                Nat/to_str(total),
                Nat/to_str(skip([3 + z, 20, 1])(0)),
                Nat/to_str(high(x[0x80, 0x01, 0xFF, ..rand/bytes(0)!]))]));
        entry()
        "#;

    // tally keeps the last two above 2, 7 and 3; skip drops 20, whose rest already passes 10; high counts 0x80 and 0xFF.
    assert_eq!(run(source), b"2 10 4 2");
}

// A guard may hold a fold of its own, a fold's column may stand beside another the guard reads, and a guarded arm typed once by its own walk beside the fold's column falls through at the step.
#[test]
fn a_guard_holds_a_fold_and_reads_the_columns_beside_one() {
    let source = r#"
        use /std/{List, Nat, Str, Bytes, rand};
        let evens(l: List(Nat)) -> Nat =
            match l
            | [] => 0
            | [h, .._]; acc, (match h | 0 => true | _ + 1; even => even == false end) => acc + 1
            | [_, .._]; acc => acc
            end;
        let over(l: List(Nat), m: Nat) -> Nat =
            match (l, m)
            | ([], _) => 0
            | ([h, .._]; acc, k), h > k => acc + 1
            | ([_, .._]; acc, _) => acc
            end;
        let stray(l: List(Nat), m: Nat) -> Nat =
            match (l, m)
            | ([], _) => 0
            | ([_, .._]; acc, 0) => acc + 1
            | (_, k), k > 5 => k
            | _ => 1
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
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

// A proof by induction whose step a guard splits: where `n < j` holds, the hypothesis takes the premise the guard states, and where it fails, the failure is the `j <= n` the arm after it reads. A chain that fails teaches nothing, so the same proof under a second guard is refused.
#[test]
fn a_failed_guard_at_a_step_is_a_fact_a_proof_by_induction_reads() {
    let proof = |guards: &str| {
        fill!(
            r#"
            use /std/{Nat, Bool, Eq, proved};
            use /std/Bool/{True, Holds};
            let below(k: Nat, p: (Nat) -> Bool) -> Bool =
                match k | 0 => true | j + 1; rest => p(j) && rest end;
            let at(k: Nat, p: (Nat) -> Bool, n: Nat, all: Holds(below(k, p)), under: Holds(n < k)) -> Holds(p(n)) =
                match k: (m) => (all: Holds(below(m, p)), under: Holds(n < m)) -> Holds(p(n))
                | 0 => (_, under) => match under end
                | j + 1; ih, ${guards} => (all, _) => ih(Bool/and/second(p(j), below(j, p), all), proved())
                | j + 1 => (all, under) =>
                    Eq/subst(
                        (m: Nat) => Holds(p(m)),
                        Nat/le/antisym(j, n, proved(), proved()),
                        Bool/and/first(p(j), below(j, p), all))
                end(all, under);
            let small: Holds(3 < 9) = at(5, (x) => x < 9, 3, True/qed(), True/qed());
            /std/print("ok")
            "#,
            guards
        )
    };

    assert_eq!(run(&proof("n < j")), b"ok");
    let report = error(&proof("n < j, true"));
    assert!(
        report.contains("nothing discharged Holds(j <= n)"),
        "{report}"
    );
}

// Conversion runs a fold whose arms take guards, as the program does: `count(6)` is 3 by reduction alone, and 4 is refused.
#[test]
fn conversion_runs_a_fold_whose_arms_take_guards() {
    let claim = |value: &str| {
        fill!(
            r#"
            use /std/{Nat};
            use /std/Bool/{True, Holds};
            let count(n: Nat) -> Nat =
                match n
                | 0 => 0
                | k + 1; ih, k > 2 => ih + 1
                | _ + 1; ih => ih
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

// A fold is held to the rules of guards as any match is: a base case only a guarded arm reaches is refused, and so is a guarded arm written before a case of it.
#[test]
fn a_fold_with_guards_is_held_to_coverage_and_order() {
    let uncovered = error(
        r#"
        use /std/{Bool, Nat};
        let f(flag: Bool, n: Nat) -> Nat =
            match n
            | 0, flag => 1
            | _ + 1; ih => ih
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
            | ([_, .._]; acc, _), acc > 3 => acc
            | ([_, .._]; acc, 0) => acc + 1
            | ([_, .._]; acc, _) => acc
            end;
        /std/print(Nat/to_str(f([1, 2], 0)))
        "#,
    );
    assert!(
        misplaced.contains(
            "`([_, .._]; acc, 0)` is a case of `([_, .._]; acc, _)`, which is written before it"
        ),
        "{misplaced}"
    );
}
