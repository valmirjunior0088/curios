//! The few programs that run against the real host rather than a scripted one: what they assert is that the scheduler's park-poll-resume path works over a descriptor the kernel decides on. The real host's standard output cannot be captured, so a program reports through a file the test reads back.

use {crate::tests::run_text, curios_runtime::OsHost};

// A child's piped outputs are drained by the scheduler: the pipe answers would-block until the child writes, the fiber parks, `poll` wakes it, and both drains join before the child is reaped. `/bin/echo` and `/bin/cat` are present on both release targets; `cat` on the null device exits at once with nothing written.
#[test]
fn run_drains_a_real_child_through_the_scheduler() {
    let report = std::env::temp_dir().join(format!("curios-os-tests-{}", std::process::id()));
    let path = report.to_str().expect("a UTF-8 temporary path");
    let source = format!(
        r#"
        use /std/{{Str, Bytes, Option, Result, Show, Try, Async, Io, Path, File, Command}};
        let text(b: Bytes) -> Str = Option/unwrap_or(Str/of_bytes(b), "?");
        let program: Try(Async, Io/Error, {{}}) =
            let e = Command/run(Command/new("/bin/echo", ["hi"]))!;
            let c = Command/run(Command {{ ..Command/new("/bin/cat", []), stdin = Command/Stdio/null() }})!;
            let first = Str/flatten([text(e.stdout), ":", Show/show(e.exit)]);
            let second = Str/flatten([text(c.stdout), ":", Show/show(c.exit)]);
            File/write_all(Path/of_str("{path}"), Str/to_bytes(Str/flatten([first, "|", second])));
        let fiber: Async({{}}) =
            let r = Try/run(program)!;
            match r
            | failure(e) => /std/print(Show/show(e))
            | success(_) => Io/pure(())
            end;
        Async/run(fiber)
        "#
    );

    run_text(&source, OsHost::with_args(vec![])).expect("expected result");

    let written = std::fs::read(&report).expect("the program wrote its report");
    std::fs::remove_file(&report).expect("the report is removable");
    assert_eq!(written, b"hi\n:exited(0)|:exited(0)");
}
