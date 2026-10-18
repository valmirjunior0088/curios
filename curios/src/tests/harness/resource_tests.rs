//! Resource protection covers complete interpretation, including failures reached after suspension.

use {
    super::super::{run, run_tests_program, run_text},
    curios_runtime::MockHost,
};

#[test]
fn a_scope_releases_before_propagating_a_failure_reached_after_yielding() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Io, Test, print};
        let release(ended: Io/Ended) -> Io({}) =
            print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let body() -> Test({}) =
            let _ = print("body|")!;
            let _ = Io/yield()!;
            Test/assert(false);
        test protected = Test/scope((s) => let _ = Io/Scope/defer(s, release)!; body());
        ()
        "#
        ),
        b"body|finished|/protected: failed\n  the condition was false\n"
    );
}

#[test]
fn a_scope_releases_before_its_successful_payload_continues() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Io, Test, print};
        let release(_: Io/Ended) -> Io({}) = print("released|");
        let body() -> Test(Nat) =
            let _ = Io/yield()!;
            Test/pure(42);
        let check() -> Test({}) =
            let value = Test/scope((s) => let _ = Io/Scope/defer(s, release)!; body())!;
            let _ = print("continued|")!;
            Test/equal(value, 42);
        check()
        "#),
        b"released|continued|program: passed\n"
    );
}

#[test]
fn an_external_race_cancels_a_suspended_test_and_runs_its_release() {
    assert_eq!(
        run(r#"
        use /std/{Io, Test, print};
        let release(ended: Io/Ended) -> Io({}) =
            print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let body() -> Test({}) =
            let _ = print("waiting|")!;
            let _ = Io/wait_filled([])!;
            Test/assert(true);
        let protected() -> Test({}) = Test/scope((s) => let _ = Io/Scope/defer(s, release)!; body());
        let competing() -> Io({}) =
            let _ = Io/yield()!;
            Io/pure(());
        let entry() -> Io({}) =
            let discarded = Io/bind(Test/main([("loser", protected)]), (_) => Io/pure(()));
            let _ = Io/race([discarded, competing()])!;
            print("done|");
        entry()
        "#),
        b"waiting|cancelled|done|"
    );
}

#[test]
fn a_scope_cancels_a_started_child_before_propagating_the_tests_failure() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Io, Test, print};
        use /std/Io/{Signal};
        let child(started: Signal) -> Io({}) =
            let waiting() -> Io({}) =
                let _ = print("child-started|")!;
                let _ = Signal/notify(started)!;
                Io/wait_filled([]);
            Io/Scope/run((scope) => Io/bind(Io/Scope/defer(scope, (_) => print("child-cancelled|")), (_) => waiting()));
        let body(s: Io/Scope) -> Test({}) =
            let started = Signal/new()!;
            let _ = Io/Scope/spawn(s, child(started))!;
            let _ = Signal/wait(started)!;
            Test/assert(false);
        test scoped = Test/scope(body);
        ()
        "#
        ),
        b"child-started|child-cancelled|/scoped: failed\n  the condition was false\n"
    );
}

#[test]
fn a_timeout_cancels_and_releases_before_reporting_its_failure() {
    let (system, io) = MockHost::builder().mono((0..40i64).map(|s| (s, 0))).build();
    run_text(
        r#"
        use /std/{Io, Test, print};
        use /std/time/{Duration};
        let release(ended: Io/Ended) -> Io({}) =
            print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let waiting() -> Test({}) =
            let _ = Io/sleep(Duration/of_secs(50))!;
            let _ = print("unreached|")!;
            Test/assert(true);
        let protected() -> Test({}) = Test/scope((s) => let _ = Io/Scope/defer(s, release)!; waiting());
        let expired() -> Test({}) = Test/timeout(Duration/of_secs(2), protected);
        let entry() -> Io({}) =
            let _ = Test/main([("expired", expired)])!;
            Io/pure(());
        entry()
        "#,
        system,
    )
    .expect("the timeout reports its verdict");
    assert_eq!(
        io.output(),
        b"cancelled|expired: failed\n  the test timed out\n"
    );
}

#[test]
fn a_timeout_keeps_the_successful_payload_when_the_body_wins() {
    let (system, io) = MockHost::builder().mono((0..40i64).map(|s| (s, 0))).build();
    run_text(
        r#"
        use /std/{Nat, Io, Test};
        use /std/time/{Duration};
        let check() -> Test({}) =
            let value = Test/timeout(Duration/of_secs(5), () => Test/pure(42))!;
            Test/equal(value, 42);
        check()
        "#,
        system,
    )
    .expect("the successful payload is checked");
    assert_eq!(io.output(), b"program: passed\n");
}
