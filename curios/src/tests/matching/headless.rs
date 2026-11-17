//! A match with no head: arm order, bind guards, the fallthrough a nested bind shares, what the arms after a failed guard learn, and a program closed in each arm.

use crate::tests::test_support::*;

// An arm whose guard is a condition before one whose guard is a bind: each is tried in turn, and the bind's arm is taken where the condition's failed and its pattern matches.
#[test]
fn a_condition_guard_may_stand_before_a_bind_guard() {
    let source = r#"
        use /std/{Bool, Nat, Option};
        let pick(prefer_fresh : Bool, cached : Option(Nat), fresh : Nat) -> Nat =
            match
            | prefer_fresh && fresh > 0 => fresh
            | some(n) = cached => n
            | _ => 0
            end;
        /std/print(Nat/to_str(pick(true, Option/some(21), 5) + pick(false, Option/some(21), 5)))
        "#;

    assert_eq!(run(source), b"26");
}

// Exercised as emitted wasm rather than folded: `rand/bytes(0)` is length 0 so `z` is a runtime-opaque 0, and `n` is a runtime 2. The first arm whose guard holds is taken — `n <= 0` and `n <= 1` fail, `n <= 2` selects `300`, and the default, which would also take it, is never reached.
#[test]
fn the_first_arm_whose_guards_pass_is_taken() {
    let source = r#"
        use /std/{Nat, Bytes, rand};
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            let n = Nat/add(z, 2);
            let result =
                match
                | n <= 0 => Nat/add(z, 100)
                | n <= 1 => Nat/add(z, 200)
                | n <= 2 => Nat/add(z, 300)
                | _ => Nat/add(z, 999)
                end;
            /std/print(Nat/to_str(result));
        entry()
        "#;

    assert_eq!(run(source), b"300");
}

// A match with no head and no guarded arm is just its default. Runtime-tainted so it runs as wasm.
#[test]
fn a_match_with_no_head_of_only_a_default_takes_it() {
    let source = r#"
        use /std/{Nat, Bytes, rand};
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            let result =
                match
                | _ => Nat/add(z, 42)
                end;
            /std/print(Nat/to_str(result));
        entry()
        "#;

    assert_eq!(run(source), b"42");
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

// A bind guard `pattern = value` (Rust's `if let`) binds where `value` matches, and its arm falls through to the arms after it where it does not.
#[test]
fn a_bind_guard_binds_where_its_pattern_matches_and_falls_through_where_it_misses() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand};
        let f(o : Option(Nat)) -> Nat =
            match
            | some(x) = o => x + 10
            | _ => 99
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            /std/print(Nat/to_str((f(Option/some(5)) + f(Option/none())) + z));
        entry()
        "#;

    assert_eq!(run(source), b"114");
}

// A bind guard whose pattern is refutable at *two* points (`some` and the `cons` nested in its payload) reaches the arms after it from either, through the one `_` row the bind is read as.
#[test]
fn a_bind_guard_refutable_at_two_places_shares_one_fallthrough() {
    let source = r#"
        use /std/{Option, List, Nat, Bytes, rand};
        let f(o : Option(List(Nat))) -> Nat =
            match
            | some([h, ..t]) = o => h + 1
            | _ => 99
            end;
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            let a = f(Option/some([5, 6, 7]));
            let b = f(Option/some([]));
            let c = f(Option/none());
            /std/print(Nat/to_str(((a + b) + c) + z));
        entry()
        "#;

    // some([5,..]) → 6; some([]) → 99 (inner cons fails); none() → 99 (outer some fails). 6 + 99 + 99 = 204.
    assert_eq!(run(source), b"204");
}

// The arms after a bind guard are read once, at the match's own type, however many dispatches of the bind's pattern reach them.
#[test]
fn the_arms_after_a_nested_bind_guard_are_typed_at_the_matchs_type() {
    let source = r#"
        use /std/{Int, Option};
        let g(m : Option(Option(Int))) -> Int =
            match | some(some(x)) = m => x | _ => 0 end;
        /std/print(Int/to_str(g(Option/some(Option/some(-7))) + g(Option/some(Option/none())) + g(Option/none())))
        "#;

    assert_eq!(run(source), b"-7");
}

// The arms after an arm whose one guard is a condition are typed under its failure, as a `Bool` match's `false` arm is; the arms after a bind guard learn nothing of the value its pattern missed.
#[test]
fn the_arms_after_a_failed_condition_learn_it_and_after_a_missed_bind_nothing() {
    let condition = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let learned(b: Bool) -> Nat =
            match
            | b => 1
            | _ => (let _: Holds(Bool/not(b)) = True/qed(); 0)
            end;
        /std/print(Nat/to_str(learned(false)))
        "#;
    assert_eq!(run(condition), b"0");

    let bind = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let unlearned(b: Bool) -> Nat =
            match
            | true = b => 1
            | _ => (let _: Holds(Bool/not(b)) = True/qed(); 0)
            end;
        /std/print(Nat/to_str(unlearned(false)))
        "#;
    let report = error(bind);
    assert!(
        report.contains(
            "type mismatch\n│  inferred: `True`\n│  expected: `Holds(Bool/xor(b, true))`"
        ),
        "{report}"
    );
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
