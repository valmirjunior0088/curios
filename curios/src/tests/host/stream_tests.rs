//! Handles, reads and writes, and the drain that surfaces an error rather than a partial prefix.

use {
    crate::tests::{run, run_text},
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
        | chunk(b) => let w = std/Io/write(std/Io/stdout, b)!; /std/Io/pure(())
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
        use /std/{Io, stream};
        let show(r : stream/Chunk) -> Io({}) =
            match r : (_) => Io({})
            | chunk(b) => let _ = Io/write(Io/stdout, b)!; /std/Io/pure(())
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
        use /std/{Nat, Bytes, Result, Async, Cell, Str, Io, print, stream};
        let show(r : Result(Async/Deadlock, Result(Io/Error, Bytes))) -> Str =
            match r
            | failure(_) => "deadlock"
            | success(inner) =>
                match inner
                | success(bytes) => Str/concat("ok:", Nat/to_str(Bytes/len(bytes)))
                | failure(_) => "error"
                end
            end;
        let error_first(n : Nat) -> Async(stream/Chunk) =
            Async/pure(stream/Chunk/error(Io/Error/other(247)));
        let chunk_then_error : Io((Nat) -> Async(stream/Chunk)) =
            let calls = Cell/new(0)!;
            Io/pure((n) =>
                let k = Async/lift(Cell/get(calls))!;
                let _ = Async/lift(Cell/set(calls, k + 1))!;
                match k
                | 0 => Async/pure(stream/Chunk/chunk(x[0x41, 0x42]))
                | _ => Async/pure(stream/Chunk/error(Io/Error/other(247)))
                end);
        let chunk_then_eof : Io((Nat) -> Async(stream/Chunk)) =
            let calls = Cell/new(0)!;
            Io/pure((n) =>
                let k = Async/lift(Cell/get(calls))!;
                let _ = Async/lift(Cell/set(calls, k + 1))!;
                match k
                | 0 => Async/pure(stream/Chunk/chunk(x[0x41, 0x42, 0x43]))
                | _ => Async/pure(stream/Chunk/eof())
                end);
        let _ = print(show(Async/block_on(Async/drain(error_first))!))!;
        let _ = print(" / ")!;
        let _ = print(show(Async/block_on(Async/drain(chunk_then_error!))!))!;
        let _ = print(" / ")!;
        print(show(Async/block_on(Async/drain(chunk_then_eof!))!))
        "#;

    assert_eq!(run(source), b"error / error / ok:3");
}
