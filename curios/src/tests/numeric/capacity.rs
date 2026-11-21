//! Requested construction counts stay exact until the runtime's logical length bound, and larger values refuse before allocation.

use {super::test_support::*, crate::tests::test_support::*, curios_common::fill};

#[test]
fn a_requested_length_past_the_runtime_bound_refuses_before_allocating() {
    let rows = [
        "Nat/to_str(Bytes/len(Bytes/replicate(4294967296 + n, 0)))",
        "Nat/to_str(Bytes/len(Bytes/replicate(18446744073709551616 + n, 0)))",
    ];
    let compiled = compile(&table(&rows, true)).expect("the construction table compiles");
    for (index, row) in rows.iter().enumerate() {
        let error = run_row(&compiled, index).expect_err("the logical length exceeds the bound");
        assert!(
            error.contains("panic: a value is past what the runtime can hold"),
            "wrong refusal for {row}: {error}"
        );
        assert!(
            error.contains("a length or capacity exceeds 2^32 - 1 elements"),
            "the refusal states the bound: {error}"
        );
    }
}

/// A closed length is the compile-time evaluator's to fold, which builds no sequence past what a replacement may hold: a replicate's count, and a byte string and a list each doubled until it is past any memory, compile, and stop where they run.
#[test]
fn a_closed_length_past_what_a_fold_builds_is_left_to_the_runtime() {
    let doubled = |seed: &str| (0..34).fold(seed.to_string(), |inner, _| format!("d({inner})"));
    let sources = [
        r#"
        use /std/{Nat, Bytes, print};
        print(Nat/to_str(Bytes/len(Bytes/replicate(68719476736, 0))))
        "#
        .to_string(),
        fill!(
            r#"
            use /std/{Nat, Bytes, print};
            let d(b: Bytes) -> Bytes = x[..b, ..b];
            print(Nat/to_str(Bytes/len(${doubled})))
            "#,
            doubled = doubled("x[00, 01, 02, 03]")
        ),
        fill!(
            r#"
            use /std/{Nat, List, print};
            let d(l: List(Nat)) -> List(Nat) = [..l, ..l];
            print(Nat/to_str(List/len(${doubled})))
            "#,
            doubled = doubled("[1, 2, 3, 4]")
        ),
    ];
    for source in sources {
        let error = error(&source);
        assert!(
            error.contains("panic: a value is past what the runtime can hold"),
            "wrong refusal for {source}: {error}"
        );
    }
}

#[test]
fn a_channel_capacity_past_the_runtime_bound_refuses_before_allocating() {
    for count in ["4294967296", "18446744073709551616"] {
        let source = fill!(
            r#"
            use /std/{Nat, Io, print, Channel};
            let entry() -> /std/Io({}) =
                let c = Channel/new(@Nat, ${count})!;
                print(match Channel/try_send(c, 1)!
                    | taken() => "taken"
                    | full() => "full"
                    | closed() => "closed"
                    end);
            entry()
        "#,
            count
        );
        let error = error(&source);
        assert!(
            error.contains("panic: a value is past what the runtime can hold"),
            "wrong refusal at capacity {count}: {error}"
        );
    }
}
