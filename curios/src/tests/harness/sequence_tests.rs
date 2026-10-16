//! Successful payloads, declared embeddings and first-failure sequencing.

use super::super::{error, run, run_tests_program};

/// A written function reads its Test monad from the result, and Io embeds through its declared edge.
#[test]
fn a_function_returning_test_sequences_assertions_and_io() {
    assert_eq!(
        run(r#"
        use /std/{Io, Test, print};
        let check() -> Test({}) =
            let _ = Test/assert(true)!;
            let _ = print("before|")!;
            let _ = Io/yield()!;
            let _ = Test/assert(true)!;
            Test/assert(true);
        check()
    "#),
        b"before|program: passed\n"
    );
}

#[test]
fn a_failed_assertion_skips_the_remaining_test_continuations() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Test, print};
        test sequence =
            let _ = Test/assert(true)!;
            let _ = Test/assert(false)!;
            let _ = print("unreached")!;
            Test/assert(true);
        test afterward = Test/assert(true);
        ()
    "#
        ),
        b"/sequence: failed\n  the condition was false\n/afterward: passed\n"
    );
}

#[test]
fn one_try_embedding_serves_io_and_test_bases() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Try, Test, print};
        let io_error() -> Try(Io, Str, Nat) = Try/raise("io error");
        let test_error() -> Try(Test, Str, Nat) = Try/raise("test error");
        test first =
            let _ = io_error()!;
            print("unreached");
        test second =
            let _ = test_error()!;
            print("unreached");
        ()
    "#
        ),
        b"/first: failed\n  io error\n/second: failed\n  test error\n"
    );
}

#[test]
fn a_test_declaration_requires_a_terminal_unit_payload() {
    let refusal = error(
        r#"
        use /std/{Nat, Test};
        test not_terminal = Test/pure(42);
        ()
    "#,
    );
    assert!(refusal.contains("type mismatch"), "{refusal}");
}

#[test]
fn successful_payloads_flow_through_binding_and_identity_lifting() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Test};
        let answer() -> Test(Nat) =
            let first = Test/pure(20)!;
            Test/pure(first + 1);
        let check() -> Test({}) =
            let value = answer()!;
            let _ = Test/equal(value, 21)!;
            Test/assert(value * 2 == 42);
        check()
        "#),
        b"program: passed\n"
    );
}

#[test]
fn a_test_can_inspect_expected_errors_as_data() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Try, Test};
        let refused() -> Try(Io, Str, Nat) = Try/raise("expected refusal");
        let check() -> Test({}) =
            let outcome = Try/run(refused())!;
            match outcome
            | success(_) => Test/fail("unexpected success")
            | failure(why) => Test/equal(why, "expected refusal")
            end;
        check()
        "#),
        b"program: passed\n"
    );
}

#[test]
fn a_program_refuses_to_discard_a_nonunit_test_payload() {
    let refusal = error(
        r#"
        use /std/{Nat, Test};
        Test/pure(42)
        "#,
    );
    assert!(
        refusal.contains("inferred: Nat") && refusal.contains("expected: {}"),
        "{refusal}"
    );
}

#[test]
fn a_try_embedding_requires_its_bases_declared_embedding() {
    let refusal = error(
        r#"
        use /std/{Nat, Str, Option, Try, Test};
        let action() -> Try(Option, Str, Nat) = Try/pure(1);
        let check() -> Test({}) =
            let value = action()!;
            Test/equal(value, 1);
        check()
    "#,
    );
    assert!(
        refusal.contains("Lift") && refusal.contains("Option"),
        "{refusal}"
    );
}
