//! A dispatch on a natural's literals: what it takes on an open successor.

use {crate::tests::test_support::*, curios_common::fill};

/// A Fibonacci written by its literals and a catch-all over `n - 1` and `n - 2`, and a law of it at every `k + 2`.
const FIBONACCI: &str = r#"
use /std/{Eq, Nat};

let fib(n: Nat) -> Nat =
    match n
    | 0 => 0
    | 1 => 1
    | _ => fib(n - 1) + fib(n - 2)
    end;

let law(k: Nat) -> Eq()(fib(k + 2), fib(k) + fib(k + 1)) = Eq/refl();

/std/print(Nat/to_str(fib(10)))
"#;

// `k + 2` is no `0` and no `1` whatever `k` is, so `fib(k + 2)` takes the catch-all and its law holds by reduction alone, through both checkers.
#[test]
fn a_dispatch_on_a_successor_past_its_keys_takes_its_catch_all() {
    assert_eq!(run(FIBONACCI), b"55");
}

// `k + 2` may be `2`, a key the dispatch writes, so it stays stuck there and the equation it would answer is refused.
#[test]
fn a_dispatch_on_a_successor_a_key_may_be_stays_stuck() {
    let source = |keys: &str| {
        fill!(
            r#"
            use /std/{Eq, Nat};
            let f(n: Nat) -> Nat = match n ${keys} | _ => 7 end;
            let past(k: Nat) -> Eq()(f(k + 2), 7) = Eq/refl();
            /std/print(Nat/to_str(f(5)))
            "#,
            keys
        )
    };

    assert_eq!(run(&source("| 0 => 0 | 1 => 5")), b"7");
    let error = error(&source("| 0 => 0 | 2 => 5"));
    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}
