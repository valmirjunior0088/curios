//! Requested construction counts stay exact until the runtime's logical length bound, and larger values refuse before allocation.

use {
    super::test_support::{run_row, table},
    crate::tests::{compile, error},
    curios_utilities::fill,
};

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
            error.contains("panicked: a value is past what the runtime can hold"),
            "wrong refusal for {row}: {error}"
        );
        assert!(
            error.contains("a length or capacity exceeds 2^32 - 1 elements"),
            "the refusal states the bound: {error}"
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
                print(match Channel/try_send(c.0, 1)!
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
            error.contains("panicked: a value is past what the runtime can hold"),
            "wrong refusal at capacity {count}: {error}"
        );
    }
}
