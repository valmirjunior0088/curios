//! Resource protection covers complete interpretation, including failures reached after suspension.

use {crate::test_support::*, curios_native::test_support::*};

#[test]
fn a_scope_releases_before_propagating_a_failure_reached_after_yielding() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Io, Test, print};
        let release(ended: Io/Scope/Ended) -> Io({}) =>
            print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let body() -> Test({}) =>
            let _ = print("body|")!;
            let _ = Io/yield()!;
            Test/assert("the body fails after yielding", false);
        test protected => Test/scope((s) => let _ = Io/Scope/defer(s, release)!; body());
        ()
        "#
        ),
        b"body|finished|/protected: failed\n  the body fails after yielding\n"
    );
}

#[test]
fn a_scope_releases_before_its_successful_payload_continues() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Io, Test, print};
        let release(_: Io/Scope/Ended) -> Io({}) => print("released|");
        let body() -> Test(Nat) =>
            let _ = Io/yield()!;
            Test/pure(42);
        let check() -> Test({}) =>
            let value = Test/scope((s) => let _ = Io/Scope/defer(s, release)!; body())!;
            let _ = print("continued|")!;
            Test/equal(value, 42);
        test checked => check();
        ()
        "#
        ),
        b"released|continued|/checked: passed\n"
    );
}

/// `Test/run` raced inside `Io`, as a program may race what a test does.
#[test]
fn an_external_race_cancels_a_suspended_test_and_runs_its_release() {
    let (system, io) = MockHost::builder().build();
    run_text(
        r#"
        use /std/{Io, Test, print};
        let release(ended: Io/Scope/Ended) -> Io({}) =>
            print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let body() -> Test({}) =>
            let _ = print("waiting|")!;
            let _ = Io/select(@{}, [])!;
            Test/assert("it holds", true);
        let protected() -> Test({}) => Test/scope((s) => let _ = Io/Scope/defer(s, release)!; body());
        let competing() -> Io({}) =>
            let _ = Io/yield()!;
            Io/pure(());
        let entry() -> Io({}) =>
            let discarded = Io/bind(Test/run(protected()), (_) => Io/pure(()));
            let _ = Io/select([discarded, competing()])!;
            print("done|");
        entry()
        "#,
        system,
    )
    .expect("the race ends");
    assert_eq!(io.output(), b"waiting|cancelled|done|");
}

#[test]
fn a_scope_cancels_a_started_child_before_propagating_the_tests_failure() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Io, Test, print, Channel};
        let child(started: Channel({})) -> Io({}) =>
            let waiting() -> Io({}) =>
                let _ = print("child-started|")!;
                let _ = Channel/try_push(started, ())!;
                Io/select(@{}, []);
            Io/scope((scope) => Io/bind(Io/Scope/defer(scope, (_) => print("child-cancelled|")), (_) => waiting()));
        let body(s: Io/Scope) -> Test({}) =>
            let started = Channel/new(@{}, 1)!;
            let _ = Io/Scope/spawn(s, child(started))!;
            let _ = Channel/take(started)!;
            Test/assert("the scoped test fails", false);
        test scoped => Test/scope(body);
        ()
        "#
        ),
        b"child-started|child-cancelled|/scoped: failed\n  the scoped test fails\n"
    );
}

#[test]
fn a_timeout_cancels_and_releases_before_reporting_its_failure() {
    assert_eq!(
        run_tests_under(
            r#"
        use /std/{Io, Test, print};
        use /std/time/{Duration};
        let release(ended: Io/Scope/Ended) -> Io({}) =>
            print(match ended | finished() => "finished|" | cancelled() => "cancelled|" end);
        let waiting() -> Test({}) =>
            let _ = Io/sleep(Duration/of_secs(50))!;
            let _ = print("unreached|")!;
            Test/assert("it holds", true);
        let protected() -> Test({}) => Test/scope((s) => let _ = Io/Scope/defer(s, release)!; waiting());
        test expired => Test/timeout(Duration/of_secs(2), protected);
        ()
        "#,
            ticking
        ),
        b"cancelled|/expired: failed\n  timed out\n    after: `Duration/of_nanos(+2000000000)`\n"
    );
}

#[test]
fn a_timeout_keeps_the_successful_payload_when_the_body_wins() {
    assert_eq!(
        run_tests_under(
            r#"
        use /std/{Nat, Io, Test};
        use /std/time/{Duration};
        let check() -> Test({}) =>
            let value = Test/timeout(Duration/of_secs(5), () => Test/pure(42))!;
            Test/equal(value, 42);
        test checked => check();
        ()
        "#,
            ticking
        ),
        b"/checked: passed\n"
    );
}

/// A host whose monotonic clock reads a second further at each look, so a deadline is reached without a wait.
fn ticking() -> MockHostBuilder {
    MockHost::builder().mono((0..40i64).map(|s| (s, 0)))
}
