//! `choose`: arm order, bind arms, and the fallthrough a nested bind shares.

use super::super::run;

// Regression for a `choose` lowering bug: a condition arm followed by a refutable bind arm was parsed as `Choose`, but lowering the bind arm accidentally routed through the headed-match dependent-motive/default gate.
#[test]
fn allows_condition_before_bind_arm() {
    let source = r#"
        use /std/{Bool, Nat, Option, Handle};
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
        use /std/{Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bytes(0)!);
        let n = Nat/add(z, 2);
        let result =
            choose
            | n <= 0 => Nat/add(z, 100)
            | n <= 1 => Nat/add(z, 200)
            | n <= 2 => Nat/add(z, 300)
            | _ => Nat/add(z, 999)
            end;
        /std/print(Nat/to_str(result))
        "#;

    assert_eq!(run(source), b"300");
}

// A `choose` with no condition arms is just its default. Runtime-tainted so it runs as wasm.
#[test]
fn a_choose_of_only_a_default_takes_it() {
    let source = r#"
        use /std/{Nat, Bytes, rand, Handle};
        let z = Bytes/len(rand/bytes(0)!);
        let result =
            choose
            | _ => Nat/add(z, 42)
            end;
        /std/print(Nat/to_str(result))
        "#;

    assert_eq!(run(source), b"42");
}

// A `choose` bind arm `| pattern = value =>` (Rust `if let`): fires and binds when `value` matches, else falls through to the rest of the ladder.
#[test]
fn bind_arm_destructures_or_falls_through() {
    let source = r#"
        use /std/{Option, Nat, Bytes, rand, Handle};
        let f(o : Option(Nat)) -> Nat =
            choose
            | some(x) = o => x + 10
            | _ => 99
            end;
        let z = Bytes/len(rand/bytes(0)!);
        /std/print(Nat/to_str((f(Option/some(5)) + f(Option/none())) + z))
        "#;

    assert_eq!(run(source), b"114");
}

// A bind arm whose pattern is refutable at *two* points (`some` and the `cons` nested in its payload): the rest-of-ladder is shared through a nullary thunk, reached whether the outer `some` or the inner cons fails to match.
#[test]
fn nested_bind_shares_the_fallthrough() {
    let source = r#"
        use /std/{Option, List, Nat, Bytes, rand, Handle};
        let f(o : Option(List(Nat))) -> Nat =
            choose
            | some([h, ..t]) = o => h + 1
            | _ => 99
            end;
        let z = Bytes/len(rand/bytes(0)!);
        let a = f(Option/some([5, 6, 7]));
        let b = f(Option/some([]));
        let c = f(Option/none());
        /std/print(Nat/to_str(((a + b) + c) + z))
        "#;

    // some([5,..]) → 6; some([]) → 99 (inner cons fails); none() → 99 (outer some fails). 6 + 99 + 99 = 204.
    assert_eq!(run(source), b"204");
}
