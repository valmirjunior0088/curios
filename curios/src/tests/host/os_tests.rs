//! The few programs that run against the real host rather than a scripted one: what they assert is that the scheduler's park-poll-resume path works over a descriptor the kernel decides on. The real host's standard output cannot be captured, so a program reports through a file the test reads back.

use {
    crate::tests::run_text,
    curios_native::OsHost,
    curios_utilities::{fill, test_support::Temporary},
};

// A child's piped outputs are drained side by side, and its input written beside them: the pipe answers would-block until the child writes, the reader suspends on a watch, the host's wait wakes it, and everything joins before the child is reaped. `/bin/echo` and `/bin/cat` are present on both release targets; `cat` writes back what it was fed and exits at the end of its input, which only finishing the input gives it.
#[test]
fn run_drains_a_real_child_through_the_scheduler() {
    let root = Temporary::new("host", "os-report");
    std::fs::create_dir_all(&root).expect("a directory of its own");
    let report = root.join("report");
    let path = report.to_str().expect("a UTF-8 temporary path");
    let source = fill!(
        r#"
        use /std/{Str, Bytes, Option, Result, Show, Try, Io, Path, File, Process};
        let text(b: Bytes) -> Str = Result/unwrap_or(Str/of_bytes(b), "?");
        let program() -> Try(Io, Io/Error, {}) =
            let e = Process/run(Process/new("/bin/echo", ["hi"]), x[])!;
            let c = Process/run(Process/new("/bin/cat", []), Str/to_bytes("fed"))!;
            let first = Str/flatten([text(e.stdout), ":", Show/show(e.exit)]);
            let second = Str/flatten([text(c.stdout), ":", Show/show(c.exit)]);
            File/write_all(Path/of_str("${path}"), Str/to_bytes(Str/flatten([first, "|", second])));
        let main() -> Io({}) =
            let r = Try/run(program())!;
            match r
            | failure(e) => /std/print(Show/show(e))
            | success(_) => Io/pure(())
            end;
        main()
        "#,
        path
    );

    run_text(&source, OsHost::with_args(vec![])).expect("expected result");

    let written = std::fs::read(&report).expect("the program wrote its report");
    assert_eq!(written, b"hi\n:exited(0)|fed:exited(0)");
}
