//! The few programs that run against the real host rather than a scripted one: what they assert is that the scheduler's park-poll-resume path works over a descriptor the certifier decides on. The real host's standard output cannot be captured, so a program reports through a file the test reads back.

use {
    crate::tests::test_support::*,
    curios_common::{fill, test_support::*},
    curios_native::OsHost,
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
        use /std/{Str, Bytes, Option, Result, Show, Try, Io, Path, File, Proc};
        let text(b: Bytes) -> Str = Result/unwrap_or(Str/of_bytes(b), "?");
        let program() -> Try(Io, Io/Error, {}) =
            let e = Proc/run(Proc/new("/bin/echo", ["hi"]), 0x[])!;
            let c = Proc/run(Proc/new("/bin/cat", []), Str/to_bytes("fed"))!;
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

/// The children the programs below wait on and write to, each through the stream its pipe gives, and a mebibyte of `a` to write: past what a pipe holds, so a write to a child that has not read yet waits.
const PIPED: &str = r#"
    use /std/{Nat, Bytes, Str, Option, Result, Show, Try, Io, Path, File, Proc};
    use /std/time/{Duration};
    let grown(b: Bytes, n: Nat) -> Bytes = match n | 0 => b | m + 1 => grown(0x[..b, ..b], m) end;
    let mebibyte: Bytes = grown(0x[0x61], 20);
    let started(s: Io/Scope, script: Str, stdin: Proc/Stdio, stdout: Proc/Stdio) -> Try(Io, Io/Error, Proc/Child) =
        Proc/Child/spawn(s, Proc { ..Proc/new("/bin/sh", ["-c", script]), stdin = stdin, stdout = stdout });
    let input(c: Proc/Child) -> Try(Io, Io/Error, Proc/Output) =
        match Proc/Child/stdin(c) | some(i) => Try/pure(i) | none() => Try/raise(Io/Error/not_found()) end;
    let output(c: Proc/Child) -> Try(Io, Io/Error, Proc/Input) =
        match Proc/Child/stdout(c) | some(o) => Try/pure(o) | none() => Try/raise(Io/Error/not_found()) end;
    let cut(@A: Type, timed: Option(A)) -> Str = match timed | none() => "cut" | some(_) => "whole" end;
"#;

/// What a program over [`PIPED`] whose `program()` reports through `${path}` wrote to its report, run against the real host.
fn reported(name: &str, program: &str) -> Vec<u8> {
    let root = Temporary::new("host", name);
    std::fs::create_dir_all(&root).expect("a directory of its own");
    let report = root.join("report");
    let path = report.to_str().expect("a UTF-8 temporary path");
    let source = [
        PIPED,
        fill!(program, path).as_str(),
        r#"
        let main() -> Io({}) =
            let r = Try/run(program())!;
            match r
            | failure(e) => /std/print(Show/show(e))
            | success(_) => Io/pure(())
            end;
        main()
        "#,
    ]
    .concat();

    run_text(&source, OsHost::with_args(vec![])).expect("expected result");

    std::fs::read(&report).expect("the program wrote its report")
}

// A write a timeout cuts while the child has not yet read takes its whole message before it waits, so the child reads the message whole and then the next write's, which hands the rest on first. Against a write that held the rest of its message at the wait, the child read the 65,536 bytes its pipe took and then `END`.
#[test]
fn a_write_a_timeout_cuts_reaches_its_reader_whole() {
    let written = reported(
        "write-cut",
        r#"
        let program() -> Try(Io, Io/Error, {}) =
            Try/scope((s) =>
                let c = started(s, "sleep 0.5; cat", Proc/Stdio/piped(), Proc/Stdio/piped())!;
                let into = input(c)!;
                let back = output(c)!;
                let echo = Io/Reader/within(back, 2097152)!;
                let writes() -> Try(Io, Io/Error, Str) =
                    let timed = Io/timeout(Duration/of_millis(10), Try/run(Io/write(into, mebibyte)))!;
                    let () = Io/write(into, Str/to_bytes("END"))!;
                    let () = Io/finish(into)!;
                    Try/pure(cut(timed));
                let (how, echoed) = Try/both(writes(), Io/Reader/rest(echo))!;
                File/write_all(Path/of_str("${path}"), 0x[..Str/to_bytes(how), ..Str/to_bytes("|"), ..echoed]));
        "#,
    );

    let expected = [b"cut|".as_slice(), &[b'a'; 1 << 20], b"END"].concat();
    assert!(
        written == expected,
        "the child read {} bytes after {:?}",
        written.len(),
        String::from_utf8_lossy(&written[..written.len().min(8)])
    );
}

// A copy a timeout cuts while its destination has not yet read has moved whole chunks: run again it moves the rest, and the destination counts every byte of the source once. Against a copy whose write held a chunk at the wait, the chunk the timeout cut was lost.
#[test]
fn a_copy_a_timeout_cuts_loses_nothing() {
    let written = reported(
        "copy-cut",
        r#"
        let program() -> Try(Io, Io/Error, {}) =
            Try/scope((s) =>
                let source = started(s, "head -c 1048576 /dev/zero", Proc/Stdio/null(), Proc/Stdio/piped())!;
                let sink = started(s, "sleep 0.5; wc -c", Proc/Stdio/piped(), Proc/Stdio/piped())!;
                let from = output(source)!;
                let into = input(sink)!;
                let timed = Io/timeout(Duration/of_millis(10), Try/run(Io/copy(from, into)))!;
                let () = Io/copy(from, into)!;
                let () = Io/finish(into)!;
                let back = output(sink)!;
                let counted = Io/Reader/new(back)!;
                let count = Io/Reader/rest(counted)!;
                File/write_all(Path/of_str("${path}"), 0x[..Str/to_bytes(cut(timed)), ..Str/to_bytes("|"), ..count]));
        "#,
    );

    let written = String::from_utf8(written).expect("a count is text");
    let (how, count) = written
        .split_once('|')
        .expect("how the copy went, then the count");
    assert_eq!((how, count.trim()), ("cut", "1048576"));
}

// A write a timeout cuts leaves the rest of its message held, and a flush after the child has gone without reading meets the broken pipe and answers it. Against a write that held nothing once cut, the flush found nothing to hand on and answered success.
#[test]
fn a_drain_that_fails_after_a_cut_write_is_answered_by_the_next_flush() {
    let written = reported(
        "drain-fails",
        r#"
        let program() -> Try(Io, Io/Error, {}) =
            Try/scope((s) =>
                let c = started(s, "sleep 0.3", Proc/Stdio/piped(), Proc/Stdio/null())!;
                let into = input(c)!;
                let timed = Io/timeout(Duration/of_millis(10), Try/run(Io/write(into, mebibyte)))!;
                let () = Io/sleep(Duration/of_millis(800))!;
                let flushed = Try/run(Io/flush(into))!;
                let answered = match flushed | success(()) => "flushed" | failure(e) => Show/show(e) end;
                File/write_all(Path/of_str("${path}"), Str/to_bytes(Str/flatten([cut(timed), "|", answered]))));
        "#,
    );

    assert_eq!(String::from_utf8_lossy(&written), "cut|broken_pipe");
}

// A reader's `rest` a timeout cuts between two arrivals keeps what it read in the reader, so run again it answers both. A read to the end that held what it read at the wait, as `read_all` did, lost the first arrival and answered the second alone.
#[test]
fn a_rest_a_timeout_cuts_loses_nothing() {
    let written = reported(
        "rest-cut",
        r#"
        let program() -> Try(Io, Io/Error, {}) =
            Try/scope((s) =>
                let c = started(s, "printf abc; sleep 0.3; printf def", Proc/Stdio/null(), Proc/Stdio/piped())!;
                let out = output(c)!;
                let r = Io/Reader/new(out)!;
                let timed = Io/timeout(Duration/of_millis(100), Try/run(Io/Reader/rest(r)))!;
                let read = Io/Reader/rest(r)!;
                File/write_all(Path/of_str("${path}"), 0x[..Str/to_bytes(cut(timed)), ..Str/to_bytes("|"), ..read]));
        "#,
    );

    assert_eq!(String::from_utf8_lossy(&written), "cut|abcdef");
}
