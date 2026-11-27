//! Handles, reads and writes, and the drain that surfaces an error rather than a partial prefix.

use {crate::test_support::*, curios_common::fill, curios_native::test_support::*};

#[test]
fn io_write() {
    assert_eq!(
        run(r#"
let entry() -> /std/Io({}) =>
    let _ = /std/print("hello")!;
    /std/Io/pure(());
entry()
"#),
        b"hello"
    );
}

#[test]
fn io_write_stderr() {
    assert_eq!(
        run(r#"
let entry() -> /std/Io({}) =>
    let _ = /std/print_err("oops")!;
    /std/Io/pure(());
entry()
"#),
        b"oops"
    );
}

#[test]
fn io_read() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        let entry() -> /std/Io({}) =>
            match /std/Try/run(std/Io/read(std/Proc/stdin, 1024))!
            | success(bytes(b, @_)) => let w = /std/Try/run(std/Io/write(std/Proc/stdout, b))!; /std/Io/pure(())
            | success(ended()) => /std/Io/pure(())
            | failure(_) => /std/Io/pure(())
            end;
        entry()
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

// `Io/read(h, n)` is the typed read: each call yields a chunk of 1..n available bytes (here one injected line per refill, served in `n`-byte slices), and the third read past the data yields the end of the stream.
#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Io, Try, Result, Proc};
        let show(r : Result(Io/Error, Io/Chunk)) -> Io({}) =>
            match r
            | success(bytes(b, @_)) => let _ = Try/run(Io/write(Proc/stdout, b))!; /std/Io/pure(())
            | success(ended()) => /std/print("1")
            | failure(_) => /std/print("e")
            end;
        let entry() -> /std/Io({}) =>
            let _ = show(Try/run(Io/read(Proc/stdin, 2))!)!;
            let _ = show(Try/run(Io/read(Proc/stdin, 2))!)!;
            show(Try/run(Io/read(Proc/stdin, 2))!);
        entry()
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

// A reader's `rest` takes the end of the stream as its only orderly terminator. The load-bearing script is bytes-then-failure: the accumulated prefix must not be passed off as complete content, so the verdict is a failure and the prefix's length leaks nowhere. Bytes-then-end is the control that accumulation itself still works.
#[test]
fn a_rest_surfaces_a_read_failure_instead_of_a_partial_prefix() {
    let source = r#"
        use /std/{Nat, Bytes, Bool, Result, Str, Try, Io, print};
        use /std/Io/{Latch};
        let show(r : Result(Io/Error, Bytes)) -> Str =>
            match r
            | success(bytes) => Str/concat("ok:", Nat/to_str(Bytes/length(bytes)))
            | failure(_) => "error"
            end;
        struct Failing: Type { code: Nat }
        satisfy Io/Read(Failing) {
            read(s, _) => Try/raise(Io/Error/other(s.code)),
        }
        struct Twice: Type { calls: Latch, fails: Bool }
        satisfy Io/Read(Twice) {
            read(s, _) =>
                let again = Latch/tripped(s.calls)!;
                let () = Latch/trip(s.calls)!;
                match again
                | false => Try/pure(Io/Chunk/bytes(0x[0x41, 0x42, 0x43]))
                | true =>
                    match s.fails
                    | true => Try/raise(Io/Error/other(247))
                    | false => Try/pure(Io/Chunk/ended())
                    end
                end,
        }
        let twice(fails: Bool) -> Io(Twice) =>
            let calls = Latch/new()!;
            Io/pure(Twice { calls = calls, fails = fails });
        let rest(@S: Type, use Io/Read(S), s: S) -> Io(Result(Io/Error, Bytes)) =>
            let r = Io/Reader/new(s)!;
            Try/run(Io/Reader/rest(r));
        let entry() -> /std/Io({}) =>
            let _ = print(show(rest(Failing { code = 247 })!))!;
            let _ = print(" / ")!;
            let failing = twice(true)!;
            let _ = print(show(rest(failing)!))!;
            let _ = print(" / ")!;
            let ending = twice(false)!;
            print(show(rest(ending)!));
        entry()
        "#;

    assert_eq!(run(source), b"error / error / ok:3");
}

/// A program's own stream proves every chunk it hands a reader holds a byte. One that builds a chunk from bytes it cannot see is refused where it builds it; one that decides the length with `Bool/holds` passes over an empty piece, so a reader over it reads on to the delimiter rather than taking an empty chunk for it and ending the read early.
#[test]
fn a_program_s_own_stream_proves_every_chunk_holds_a_byte() {
    let stream = |answer: &str| {
        fill!(
            r#"
            use /std/{Bytes, Bool, Str, List, Result, Try, Io, print};
            use /std/Io/{Latch};
            struct Pieces: Type { List({Latch, Bytes}) }
            let next(ps: List({Latch, Bytes})) -> Try(Io, Io/Error, Io/Chunk) =>
                match ps
                | [] => Try/pure(Io/Chunk/ended())
                | [p, ..more] =>
                    let spent = Latch/tripped(p.0)!;
                    let () = Latch/trip(p.0)!;
                    match spent
                    | true => next(more)
                    | false => ${answer}
                    end
                end;
            satisfy Io/Read(Pieces) {
                read(s, n) => next(s.0),
            }
            let piece(b: Bytes) -> Io({Latch, Bytes}) =>
                let c = Latch/new()!;
                Io/pure((c, b));
            let entry() -> /std/Io({}) =>
                let a = piece(0x[])!;
                let b = piece(Str/to_bytes("a"))!;
                let c = piece(0x[])!;
                let d = piece(Str/to_bytes("\n"))!;
                let input = Io/Reader/new(Pieces { [a, b, c, d] })!;
                let r = Try/run(Io/Reader/until(input, '\n'))!;
                match r
                | success(found) => print(Result/unwrap_or(Str/of_bytes(found.bytes), "?"))
                | failure(_) => print("error")
                end;
            entry()
            "#,
            answer
        )
    };

    let refused = typecheck(&stream("Try/pure(Io/Chunk/bytes(p.1))"))
        .expect_err("a chunk of unseen bytes is refused");
    assert!(refused.contains("nothing discharged"), "{refused}");

    let decided = stream(
        "match Bool/holds(0 < Bytes/length(p.1)) | some(q) => Try/pure(Io/Chunk/bytes(p.1, @q)) | none() => next(more) end",
    );
    assert_eq!(run(&decided), b"a");
}

/// Standard input arrives in chunks of several bytes, and a reader's lines and fields are cut across them: a line ends at its newline whichever chunk holds it, a carriage return before the newline goes with it, and the last line needs none.
#[test]
fn lines_read_across_the_chunks_a_stream_arrives_in() {
    let source = r#"
        use /std/{Str, Bytes, Bool, Option, Result, Try, Io, print, Proc};
        let line(r: Result(Io/Error, Option(Bytes))) -> Str =>
            match r
            | success(some(b)) => Result/unwrap_or(Str/of_bytes(b), "?")
            | success(none()) => "<end>"
            | failure(_) => "<error>"
            end;
        let field(r: Result(Io/Error, {bytes: Bytes, met: Bool})) -> Str =>
            match r
            | success(found) => Result/unwrap_or(Str/of_bytes(found.bytes), "?")
            | failure(_) => "<error>"
            end;
        let program() -> Io({}) =>
            let input = Io/Reader/new(Proc/stdin)!;
            let a = Try/run(Io/Reader/line(input))!;
            let b = Try/run(Io/Reader/line(input))!;
            let c = Try/run(Io/Reader/until(input, ','))!;
            let d = Try/run(Io/Reader/line(input))!;
            let e = Try/run(Io/Reader/line(input))!;
            print(Str/join("|", [line(a), line(b), field(c), line(d), line(e)]));
        program()
    "#;

    let (system, io) = MockHost::builder()
        .stdin_chunks(vec!["ab\ncd", "e\r\nf,g", "h"])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ab|cde|f|gh|<end>");
}
