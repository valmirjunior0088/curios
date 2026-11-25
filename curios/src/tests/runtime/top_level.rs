//! A top-level value: computed where a program first reads it, and not where nothing reads it.

use {crate::test_support::*, curios_common::fill, curios_native::test_support::*};

/// `boom` is a step of `spin`, which then shifts past what the runtime can hold and refuses; nothing a checker reduces says so, since the shift's count is a parameter. `${tail}` is the program's final term.
fn reading_boom(tail: &str) -> String {
    fill!(
        r#"
        use /std/{Nat, Io, print};

        let spin(n: Nat, k: Nat) -> Nat =
            match n < 1
            | true => spin(n + 1, k)
            | false => spin(Nat/shl(1, k), k)
            end;

        let boom: Nat = spin(0, 200000000000);

        ${tail}
        "#,
        tail
    )
}

/// A value nothing reads is never computed, however its computation would end: the program prints and exits.
#[test]
fn a_value_nothing_reads_is_never_computed() {
    assert_eq!(run(&reading_boom(r#"print("entry")"#)), b"entry");
}

/// A value is computed where the program first reads it: what the program does before the read, it does.
#[test]
fn a_value_is_computed_where_it_is_first_read() {
    let (system, io) = MockHost::builder().build();
    let error = run_text(
        &reading_boom(
            r#"
            let main() -> Io({}) =
                let () = print("before")!;
                print(Nat/to_str(boom));

            main()
            "#,
        ),
        system,
    )
    .expect_err("reading `boom` refuses");

    assert_eq!(io.output(), b"before");
    assert!(
        error.contains("past what the runtime can hold"),
        "the read runs `boom`'s computation, which refuses: {error}"
    );
}
