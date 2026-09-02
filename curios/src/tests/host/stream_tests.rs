//! Handles, reads and writes, and the drain that surfaces an error rather than a partial prefix.

use {
    crate::tests::{run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn io_write() {
    assert_eq!(
        run(r#"
let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("hello"))!;
/std/Io/pure(())
"#),
        b"hello"
    );
}

#[test]
fn handles_compare_with_the_operators() {
    // `Handle` had `eql` from `/sys` but no `Equal` witness, so `h == Handle/stdout` reported "no witness of Equal(Handle) found" while every other intrinsic carrier compared with the operator. The witness is over the same `eql`; the output is the truth table at runtime, where a handle is its token.
    assert_eq!(
        run(r#"
use /std/{Handle, Str, Bool};
let pick(h: Handle) -> Str =
    choose
    | h == Handle/stdout => "out"
    | h != Handle/stderr => "other"
    | _ => "err"
    end;
let _ = Handle/write(Handle/stdout, Str/to_bytes(pick(Handle/stdout)))!;
let _ = Handle/write(Handle/stdout, Str/to_bytes(pick(Handle/stderr)))!;
let _ = Handle/write(Handle/stdout, Str/to_bytes(pick(Handle/stdin)))!;
/std/Io/pure(())
"#),
        b"outerrother"
    );
}

#[test]
fn io_write_stderr() {
    assert_eq!(
        run(r#"
let _ = std/Handle/write(std/Handle/stderr, /std/Str/to_bytes("oops"))!;
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
        match std/Handle/read(std/Handle/stdin, 1024)! : (_) => /std/Io({})
        | chunk(b) => let w = std/Handle/write(std/Handle/stdout, b)!; /std/Io/pure(())
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
        use /std/{Handle, Io};
        let show(r : Handle/Read) -> Io({}) =
            match r : (_) => Io({})
            | chunk(b) => let _ = Handle/write(Handle/stdout, b)!; /std/Io/pure(())
            | eof() => /std/print("1")
            | error(_) => /std/print("e")
            end;
        let _ = show(Handle/read(Handle/stdin, 2)!)!;
        let _ = show(Handle/read(Handle/stdin, 2)!)!;
        show(Handle/read(Handle/stdin, 2)!)
        "#;

    let (system, io) = MockHost::builder().stdin_lines(["abc"]).build();
    run_text(source, system).expect("expected result");
    assert_eq!(io.output(), b"abc\n1");
}

// `drain` treats `eof` as the stream's only orderly terminator. The load-bearing script is chunk-then-error: the accumulated prefix must not be passed off as complete content, so the verdict is a failure and the prefix's length leaks nowhere. Chunk-then-eof is the control that accumulation itself still works.
#[test]
fn async_drain_surfaces_a_read_error_instead_of_a_partial_prefix() {
    let source = r#"
        use /std/{Nat, Bytes, Handle, Result, Async, Cell, Str, Io, print};
        let show(r : Result(Async/Deadlock, Result(Handle/Error, Bytes))) -> Str =
            match r
            | failure(_) => "deadlock"
            | success(inner) =>
                match inner
                | success(bytes) => Str/concat("ok:", Nat/to_str(Bytes/len(bytes)))
                | failure(_) => "error"
                end
            end;
        let error_first(n : Nat) -> Async(Handle/Read) =
            Async/pure(Handle/Read/error(Handle/Error/other(247)));
        let chunk_then_error : Io((Nat) -> Async(Handle/Read)) =
            let calls = Cell/new(0)!;
            Io/pure((n) =>
                let k = Async/lift(Cell/get(calls))!;
                let _ = Async/lift(Cell/set(calls, k + 1))!;
                match k
                | 0 => Async/pure(Handle/Read/chunk(x[0x41, 0x42]))
                | _ => Async/pure(Handle/Read/error(Handle/Error/other(247)))
                end);
        let chunk_then_eof : Io((Nat) -> Async(Handle/Read)) =
            let calls = Cell/new(0)!;
            Io/pure((n) =>
                let k = Async/lift(Cell/get(calls))!;
                let _ = Async/lift(Cell/set(calls, k + 1))!;
                match k
                | 0 => Async/pure(Handle/Read/chunk(x[0x41, 0x42, 0x43]))
                | _ => Async/pure(Handle/Read/eof())
                end);
        let _ = print(show(Async/block_on(Async/drain(error_first))!))!;
        let _ = print(" / ")!;
        let _ = print(show(Async/block_on(Async/drain(chunk_then_error!))!))!;
        let _ = print(" / ")!;
        print(show(Async/block_on(Async/drain(chunk_then_eof!))!))
        "#;

    assert_eq!(run(source), b"error / error / ok:3");
}
