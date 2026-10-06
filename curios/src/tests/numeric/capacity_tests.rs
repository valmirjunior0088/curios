//! Requested construction counts stay exact until the runtime's logical length bound, and larger values refuse before allocation.

use {
    super::test_support::{run_row, table},
    crate::tests::{compile, error},
};

#[test]
fn a_requested_length_past_the_runtime_bound_refuses_before_allocating() {
    let rows = [
        "Nat/to_str(Bytes/len(Bytes/replicate(4294967296 + n, 0)))",
        "Nat/to_str(Bytes/len(Bytes/replicate(18446744073709551616 + n, 0)))",
        "Nat/to_str(/std/Bits/len(/std/Bits/replicate(4294967296 + n, true)))",
        "Nat/to_str(/std/Bits/len(/std/Bits/replicate(18446744073709551616 + n, false)))",
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
        let source = format!(
            r#"
            use /std/{{Nat, Io, print}};
            use /std/Async/{{Channel}};
            let c = Channel/new(@Nat, {count})!;
            print(match Channel/try_send(c.0, 1)!
                | taken() => "taken"
                | full() => "full"
                | closed() => "closed"
                end)
        "#
        );
        let error = error(&source);
        assert!(
            error.contains("panicked: a value is past what the runtime can hold"),
            "wrong refusal at capacity {count}: {error}"
        );
    }
}

/// The host taints the counts, and each run holds at most two half-gibibyte payloads. Oversized logical lengths refuse, while every last valid packed length allocates its rounded-up final byte.
#[test]
#[ignore = "requires about 1 GiB of guest heap per run"]
fn large_lengths_refuse_only_when_the_logical_bound_is_exceeded() {
    let rows = [
        "Nat/to_str(/std/Bits/len(Bytes/to_bits(Bytes/replicate(536870911 + n, 0))))",
        "Nat/to_str(/std/Bits/len(Bytes/to_bits(Bytes/replicate(536870912 + n, 0))))",
        "Nat/to_str(/std/Bits/len(Bytes/to_bits(Bytes/replicate(536870913 + n, 0))))",
        "Nat/to_str(/std/Bits/len(/std/Bits/replicate(4294967289 + n, true)))",
        "Nat/to_str(/std/Bits/len(/std/Bits/replicate(4294967295 + n, true)))",
        "Nat/to_str(/std/Bits/len(b[.. /std/Bits/replicate(2147483648 + n, true), .. /std/Bits/replicate(2147483648 + n, false)]))",
        "Nat/to_str(/std/Bits/len(b[.. /std/Bits/replicate(4294967295 + n, true), false]))",
    ];
    let compiled = compile(&table(&rows, true)).expect("the boundary table compiles");
    let expected: &[Option<&[u8]>] = &[
        Some(b"4294967288"),
        None,
        None,
        Some(b"4294967289"),
        Some(b"4294967295"),
        None,
        None,
    ];
    for (index, expected) in expected.iter().enumerate() {
        match expected {
            Some(expected) => assert_eq!(
                run_row(&compiled, index).expect("the logical length is representable"),
                *expected,
                "wrong length for {}",
                rows[index]
            ),
            None => {
                let error =
                    run_row(&compiled, index).expect_err("the logical length exceeds the bound");
                assert!(
                    error.contains("panicked: a value is past what the runtime can hold"),
                    "wrong refusal for {}: {error}",
                    rows[index]
                );
            }
        }
    }
}
