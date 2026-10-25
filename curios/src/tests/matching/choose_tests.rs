//! `choose`: arm order, bind arms, the fallthrough a nested bind shares, and what a ladder's rest learns of the arms before it.

use crate::tests::{error, run};

// A condition arm followed by a refutable bind arm parses as `Choose`, and lowering the bind arm does not route through the headed-match dependent-motive/default gate.
#[test]
fn allows_condition_before_bind_arm() {
    let source = r#"
        use /std/{Bool, Nat, Option};
        let pick(prefer_fresh : Bool, cached : Option(Nat), fresh : Nat) -> Nat =
            choose
            | prefer_fresh && fresh > 0 => fresh
            | some(n) = cached => n
            | _ => 0
            end;
        /std/print(Nat/to_str(pick(true, Option/some(21), 5)))
        "#;

    assert_eq!(run(source), b"5");
}

// `choose`, exercised as emitted wasm rather than folded: `rand/bytes(0)` is length 0 so `z` is a runtime-opaque 0, and `n` is a runtime 2. The first *true* condition wins — `n <= 0` and `n <= 1` are false, `n <= 2` selects `300`, and the later-true `_` default is never reached.
#[test]
fn selects_first_true_arm() {
    let source = r#"
        use /std/{Nat, Bytes, rand};
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            let n = Nat/add(z, 2);
            let result =
                choose
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

// A `choose` with no condition arms is just its default. Runtime-tainted so it runs as wasm.
#[test]
fn a_choose_of_only_a_default_takes_it() {
    let source = r#"
        use /std/{Nat, Bytes, rand};
        let entry() -> /std/Io({}) =
            let z = Bytes/len(rand/bytes(0)!);
            let result =
                choose
                | _ => Nat/add(z, 42)
                end;
            /std/print(Nat/to_str(result));
        entry()
        "#;

    assert_eq!(run(source), b"42");
}

// A `choose` bind arm `| pattern = value =>` (Rust `if let`): fires and binds when `value` matches, else falls through to the rest of the ladder.
#[test]
fn bind_arm_destructures_or_falls_through() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand};
        let f(o : Option(Nat)) -> Nat =
            choose
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

// A bind arm whose pattern is refutable at *two* points (`some` and the `cons` nested in its payload): the rest-of-ladder is shared through a nullary thunk, reached whether the outer `some` or the inner cons fails to match.
#[test]
fn nested_bind_shares_the_fallthrough() {
    let source = r#"
        use /std/{Option, List, Nat, Bytes, rand};
        let f(o : Option(List(Nat))) -> Nat =
            choose
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

// A bind arm's rest is the row that cannot fail of its one-row match: it is read where the ladder stands and at the ladder's own type, however many dispatches of the pattern reach it.
#[test]
fn a_nested_bind_arms_rest_is_typed_at_the_ladders_type() {
    let source = r#"
        use /std/{Int, Option};
        let g(m : Option(Option(Int))) -> Int =
            choose | some(some(x)) = m => x | _ => 0 end;
        /std/print(Int/to_str(g(Option/some(Option/some(-7))) + g(Option/some(Option/none())) + g(Option/none())))
        "#;

    assert_eq!(run(source), b"-7");
}

// What a ladder's rest learns is what the nested match it lowers to states: a condition arm's rest is the `false` arm of a match on the condition, so the condition's failure is a fact there, and a bind arm's rest is a `_` row, which learns nothing of the value the pattern missed.
#[test]
fn a_ladders_rest_learns_a_conditions_failure_and_nothing_of_a_missed_pattern() {
    let condition = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let learned(b: Bool) -> Nat =
            choose
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
            choose
            | true = b => 1
            | _ => (let _: Holds(Bool/not(b)) = True/qed(); 0)
            end;
        /std/print(Nat/to_str(unlearned(false)))
        "#;
    let report = error(bind);
    assert!(
        report.contains("type mismatch\n  inferred: True\n  expected: Holds(Bool/xor(b, true))"),
        "{report}"
    );
}
