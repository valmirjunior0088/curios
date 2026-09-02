//! The few programs that run against the real host rather than a scripted one: what they assert is that the scheduler's park-poll-resume path works over a descriptor the kernel decides on. The real host's standard output cannot be captured, so a program reports through a file the test reads back.

use {crate::tests::run_text, curios_runtime::OsHost};

// A child's piped outputs are drained by the scheduler: the pipe answers would-block until the child writes, the fiber parks, `poll` wakes it, and both drains join before the child is reaped. `/bin/echo` and `/bin/cat` are present on both release targets; `cat` on the null device exits at once with nothing written.
#[test]
fn run_drains_a_real_child_through_the_scheduler() {
    let report = std::env::temp_dir().join(format!("curios-os-tests-{}", std::process::id()));
    let path = report.to_str().expect("a UTF-8 temporary path");
    let source = format!(
        r#"
        use /std/{{Str, Bytes, Option, Result, Show, Async, Io, File, proc}};
        let text(b: Bytes) -> Str = Option/unwrap_or(Str/of_bytes(b), "?");
        let program: Async({{}}) =
            let e = proc/run(proc/Command/new("/bin/echo", ["hi"]))!;
            let c = proc/run(proc/Command {{ ..proc/Command/new("/bin/cat", []), stdin = proc/Stdio/null() }})!;
            let first = match e | success(out) => Str/flatten([text(out.stdout), ":", Show/show(out.exit)]) | failure(err) => Show/show(err) end;
            let second = match c | success(out) => Str/flatten([text(out.stdout), ":", Show/show(out.exit)]) | failure(err) => Show/show(err) end;
            let _ = File/write_all("{path}", Str/to_bytes(Str/flatten([first, "|", second])))!;
            Async/pure(());
        match Async/block_on(program)!
        | failure(_) => /std/print("deadlock")
        | success(_) => Io/pure(())
        end
        "#
    );

    run_text(&source, OsHost::with_args(vec![])).expect("expected result");

    let written = std::fs::read(&report).expect("the program wrote its report");
    std::fs::remove_file(&report).expect("the report is removable");
    assert_eq!(written, b"hi\n:exited(0)|:exited(0)");
}
