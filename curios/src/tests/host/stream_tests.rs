//! Handles, reads and writes, and the drain that surfaces an error rather than a partial prefix.

use {
    crate::tests::{run, run_text, typecheck},
    curios_runtime::MockHost,
};

#[test]
fn io_write() {
    assert_eq!(
        run(r#"
let _ = std/Io/write(std/Io/stdout, /std/Str/to_bytes("hello"))!;
/std/Io/pure(())
"#),
        b"hello"
    );
}

#[test]
fn io_write_stderr() {
    assert_eq!(
        run(r#"
let _ = std/Io/write(std/Io/stderr, /std/Str/to_bytes("oops"))!;
/std/Io/pure(())
"#),
        b"oops"
    );
}

#[test]
fn io_read() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        match std/Io/read(std/Io/stdin, 1024)! : (_) => /std/Io({})
        | chunk(b, @_) => let w = std/Io/write(std/Io/stdout, b)!; /std/Io/pure(())
        | eof() => /std/Io/pure(())
        | error(_) => /std/Io/pure(())
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

// `Handle/read(h, n)` is the typed blocking read: each call yields a `chunk` of 1..n available bytes (here one injected line per refill, served in `n`-byte slices), and the third read past the data yields `eof`.
#[test]
fn io_read_short_reads_and_eof() {
    let source = r#"
        use /std/{Io};
        let show(r : Io/Chunk) -> Io({}) =
            match r : (_) => Io({})
            | chunk(b, @_) => let _ = Io/write(Io/stdout, b)!; /std/Io/pure(())
            | eof() => /std/print("1")
            | error(_) => /std/print("e")
            end;
        let _ = show(Io/read(Io/stdin, 2)!)!;
        let _ = show(Io/read(Io/stdin, 2)!)!;
        show(Io/read(Io/stdin, 2)!)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

// `drain` treats `eof` as the stream's only orderly terminator. The load-bearing script is chunk-then-error: the accumulated prefix must not be passed off as complete content, so the verdict is a failure and the prefix's length leaks nowhere. Chunk-then-eof is the control that accumulation itself still works.
#[test]
fn async_drain_surfaces_a_read_error_instead_of_a_partial_prefix() {
    let source = r#"
        use /std/{Nat, Bytes, Result, Async, Cell, Str, Io, print};
        let show(r : Result(Async/Deadlock, Result(Io/Error, Bytes))) -> Str =
            match r
            | failure(_) => "deadlock"
            | success(inner) =>
                match inner
                | success(bytes) => Str/concat("ok:", Nat/to_str(Bytes/len(bytes)))
                | failure(_) => "error"
                end
            end;
        let error_first(n : Nat, @positive : Nat/Lt(0, n)) -> Async(Io/Chunk) =
            Async/pure(Io/Chunk/error(Io/Error/other(247)));
        let chunk_then_error : Io((n : Nat, @positive : Nat/Lt(0, n)) -> Async(Io/Chunk)) =
            let calls = Cell/new(@{})!;
            Io/pure((n) =>
                let first = Async/lift(Cell/fill(calls, ()))!;
                match first
                | true => Async/pure(Io/Chunk/chunk(x[0x41, 0x42]))
                | _ => Async/pure(Io/Chunk/error(Io/Error/other(247)))
                end);
        let chunk_then_eof : Io((n : Nat, @positive : Nat/Lt(0, n)) -> Async(Io/Chunk)) =
            let calls = Cell/new(@{})!;
            Io/pure((n) =>
                let first = Async/lift(Cell/fill(calls, ()))!;
                match first
                | true => Async/pure(Io/Chunk/chunk(x[0x41, 0x42, 0x43]))
                | _ => Async/pure(Io/Chunk/eof())
                end);
        let _ = print(show(Async/block_on(Async/drain(error_first))!))!;
        let _ = print(" / ")!;
        let _ = print(show(Async/block_on(Async/drain(chunk_then_error!))!))!;
        let _ = print(" / ")!;
        print(show(Async/block_on(Async/drain(chunk_then_eof!))!))
        "#;

    assert_eq!(run(source), b"error / error / ok:3");
}

/// A program's own stream proves every chunk it hands a reader holds a byte. One that builds a chunk from bytes it cannot see is refused where it builds it; one that decides the length with `Nat/Lt/try` passes over an empty piece, so `read_until` reads on to the delimiter rather than taking an empty chunk for it and ending the read early.
#[test]
fn a_program_s_own_stream_proves_every_chunk_holds_a_byte() {
    let stream = |answer: &str| {
        format!(
            r#"
            use /std/{{Nat, Bytes, Str, List, Option, Cell, Async, Io, print}};
            struct Pieces: Type {{ List({{Cell({{}}), Bytes}}) }}
            let next(ps: List({{Cell({{}}), Bytes}})) -> Io(Io/Chunk) =
                match ps
                | [] => Io/pure(Io/Chunk/eof())
                | [p, ..more] =>
                    let fresh = Cell/fill(p.0, ())!;
                    match fresh
                    | false => next(more)
                    | true => {answer}
                    end
                end;
            satisfy Async/Read(Io, Pieces) {{
                read(s, n) = next(s.0),
            }}
            let piece(b: Bytes) -> Io({{Cell({{}}), Bytes}}) =
                let c = Cell/new(@{{}})!;
                Io/pure((c, b));
            let a = piece(x[])!;
            let b = piece(Str/to_bytes("a"))!;
            let c = piece(x[])!;
            let d = piece(Str/to_bytes("\n"))!;
            let r = Async/read_until(@Io, Pieces {{ [a, b, c, d] }}, '\n')!;
            match r
            | success(bytes) => print(Option/unwrap_or(Str/of_bytes(bytes), "?"))
            | failure(_) => print("error")
            end
            "#
        )
    };

    let refused = typecheck(&stream("Io/pure(Io/Chunk/chunk(p.1))"))
        .expect_err("a chunk of unseen bytes is refused");
    assert!(refused.contains("nothing discharged"), "{refused}");

    let decided = stream(
        "match Nat/Lt/try(0, Bytes/len(p.1)) | some(q) => Io/pure(Io/Chunk/chunk(p.1, @q)) | none() => next(more) end",
    );
    assert_eq!(run(&decided), b"a");
}

/// Standard input arrives in chunks of several bytes, and `read_line` and `read_until` read a byte at a time across them: a line ends at its newline whichever chunk holds it, a carriage return before the newline goes with it, and the last line needs none.
#[test]
fn lines_read_across_the_chunks_a_stream_arrives_in() {
    let source = r#"
        use /std/{Str, Bytes, Option, Result, Async, Io, print};
        let line(r: Result(Io/Error, Option(Bytes))) -> Str =
            match r
            | success(some(b)) => Option/unwrap_or(Str/of_bytes(b), "?")
            | success(none()) => "<end>"
            | failure(_) => "<error>"
            end;
        let field(r: Result(Io/Error, Bytes)) -> Str =
            match r | success(b) => Option/unwrap_or(Str/of_bytes(b), "?") | failure(_) => "<error>" end;
        let fiber: Async({}) =
            let a = Async/read_line(@Async, Io/stdin)!;
            let b = Async/read_line(@Async, Io/stdin)!;
            let c = Async/read_until(@Async, Io/stdin, ',')!;
            let d = Async/read_line(@Async, Io/stdin)!;
            let e = Async/read_line(@Async, Io/stdin)!;
            print(Str/join("|", [line(a), line(b), field(c), line(d), line(e)]));
        Async/run(fiber)
    "#;

    let (system, io) = MockHost::builder()
        .stdin_chunks(vec!["ab\ncd", "e\r\nf,g", "h"])
        .build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"ab|cde|f|gh|<end>");
}
