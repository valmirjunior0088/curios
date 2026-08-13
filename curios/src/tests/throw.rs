//! `/std/Throw`: short-circuiting failure — `raise` and `!` as checked early return over `Result`.

use super::run;

/// The success path computes through; the failure path short-circuits at the `raise`, skipping the rest of the region.
#[test]
fn a_raise_short_circuits_the_region() {
    let source = r#"
        use /std/{Nat, Str, Throw, print};
        use /std/Throw/{raise, rescue};
        pub let checked_div(a: Nat, b: Nat) -> Throw(Str, Nat) =
            match 0 < b
            | false => raise("division by zero")
            | true => Throw/pure(a / b)
            end;
        pub let compute(a: Nat, b: Nat) -> Throw(Str, Nat) =
            let q = checked_div(a, b)!;
            Throw/pure(q + 1);
        let ok =
            match Throw/run(compute(10, 2))
            | success(n) => Nat/to_str(n)
            | failure(e) => e
            end;
        let caught =
            match Throw/run(rescue(compute(1, 0), (_) => Throw/pure(0)))
            | success(n) => Nat/to_str(n)
            | failure(e) => e
            end;
        let _ = print(ok)!;
        print(caught)
        "#;

    // 10/2 + 1 = 6; the rescued division by zero yields the handler's 0.
    assert_eq!(run(source), b"60");
}

/// `of` and `run` bridge to the plain `Result` vocabulary in both directions.
#[test]
fn throw_bridges_result_in_both_directions() {
    let source = r#"
        use /std/{Nat, Str, Result, Throw, print};
        use /std/Throw/{of};
        pub let parse_pair(a: Result(Nat, Str), b: Result(Nat, Str)) -> Throw(Str, Nat) =
            let x = of(a)!;
            let y = of(b)!;
            Throw/pure(x + y);
        let shown =
            match Throw/run(parse_pair(Result/success(40), Result/success(2)))
            | success(n) => Nat/to_str(n)
            | failure(e) => e
            end;
        print(shown)
        "#;

    assert_eq!(run(source), b"42");
}
