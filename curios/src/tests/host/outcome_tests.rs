//! How a row's outcome reaches a program: `/sys` reads the status before any payload, and `/std` hands the failure on as the error it names. Every fallible row is scripted to fail, and the program ends with the code its failure maps to; the two failures `/std` absorbs by design are absorbed; and the payload shapes no other suite reaches arrive whole. Each reply is written raw, past the native adapter, as any host could answer it.

use {
    super::super::compile,
    curios_abi::{HostOp, status},
    curios_runtime::{
        ForeignBindings, MockHost,
        test_support::{RawValue, run_raw},
    },
};

/// The code a program ends with for `permission_denied`, the failure every scripted row answers; `0` is a success, `2` a `not_found` and `1` any other failure.
const DENIED: u8 = 3;

/// What every fixture begins with: the imports, and `verdict`, which turns an outcome into the code the program exits with.
const PRELUDE: &str = "
use /std/{Byte, Io, Result, Try, Async, Handle, File, Path, fs, Tty, Serial, Command, tcp};
use /std/Command/{Stdio, Child};
let code(e: Io/Error) -> Byte = match e | permission_denied() => 3 | not_found() => 2 | _ => 1 end;
let verdict(@A: Type, r: Result(Io/Error, A)) -> Byte = match r | success(_) => 0 | failure(e) => code(e) end;
";

/// The rows a fixture scripts, each with the raw reply it answers every call with.
type Replies = Vec<(HostOp, Vec<RawValue>)>;

/// The monad a fixture's subject runs in.
#[derive(Clone, Copy)]
enum In {
    Io,
    Async,
}

/// Run `subject`, a `Try` over `monad` with no value, against a mock host whose rows in `replies` answer raw, and the code it exits with.
fn exit_code(monad: In, subject: &str, replies: Replies) -> u8 {
    let tail = match monad {
        In::Io => {
            "
let subject: Try(Io, Io/Error, {}) =
    SUBJECT;
let r = Try/run(subject)!;
/std/proc/exit(@{}, verdict(r))
"
        }
        In::Async => {
            "
let subject: Try(Async, Io/Error, {}) =
    SUBJECT;
let fiber: Async({}) =
    let r = Try/run(subject)!;
    /std/proc/exit(@{}, verdict(r));
Async/run(fiber)
"
        }
    };
    let source = format!("{PRELUDE}{}", tail.replace("SUBJECT", subject));
    let compiled = compile(&source).expect("the fixture compiles");

    // SAFETY: precompiled in this process by `compile`.
    unsafe {
        run_raw(
            &compiled.cwasm,
            MockHost::builder().build().0,
            replies,
            ForeignBindings::empty(),
        )
    }
    .unwrap_or_else(|error| panic!("the fixture runs to its exit: {error}"))
}

fn nat(n: u64) -> RawValue {
    RawValue::Nat(n)
}

fn bytes(bytes: &[u8]) -> RawValue {
    RawValue::Bytes(bytes.to_vec())
}

/// A failure of `permission_denied`, padded with `padding`.
fn denied(padding: Vec<RawValue>) -> Vec<RawValue> {
    [vec![nat(status::PERMISSION_DENIED)], padding].concat()
}

/// A success carrying `payload`.
fn ok(payload: Vec<RawValue>) -> Vec<RawValue> {
    [vec![nat(status::OK)], payload].concat()
}

/// Each case's row fails, and the program exits with the code its failure maps to.
fn each_fails(monad: In, cases: Vec<(&str, Replies)>) {
    for (subject, replies) in cases {
        assert_eq!(exit_code(monad, subject, replies), DENIED, "{subject}");
    }
}

#[test]
fn a_stream_row_s_failure_is_the_error_it_names() {
    each_fails(
        In::Io,
        vec![
            (
                "let c = Io/read(Io/stdin, 8)!;
                 match c | error(e) => Try/raise(e) | _ => Try/pure(()) end",
                vec![(HostOp::HandleRead, denied(vec![bytes(b"")]))],
            ),
            (
                "Try/attempt(Async/Write/write(Handle/stdout, x[1]))",
                vec![(HostOp::HandleWrite, denied(vec![nat(0)]))],
            ),
            (
                "Try/attempt(Async/Write/flush(Handle/stdout))",
                vec![(HostOp::HandleFlush, denied(vec![]))],
            ),
        ],
    );
}

#[test]
fn a_filesystem_row_s_failure_is_the_error_it_names() {
    each_fails(
        In::Io,
        vec![
            (
                r#"let _ = File/open(Path/of_str("a"), File/Mode/read())!; Try/pure(())"#,
                vec![(HostOp::FileOpen, denied(vec![bytes(b"")]))],
            ),
            (
                r#"let _ = fs/stat(Path/of_str("a"))!; Try/pure(())"#,
                vec![(
                    HostOp::FileStat,
                    denied(vec![nat(0), nat(0), nat(0), nat(0)]),
                )],
            ),
            (
                r#"fs/remove_file(Path/of_str("a"))"#,
                vec![(HostOp::FileRemove, denied(vec![]))],
            ),
            (
                r#"fs/rename(Path/of_str("a"), Path/of_str("b"))"#,
                vec![(HostOp::FileRename, denied(vec![]))],
            ),
            (
                r#"let _ = fs/list(Path/of_str("a"))!; Try/pure(())"#,
                vec![(HostOp::DirList, denied(vec![RawValue::BytesList(vec![])]))],
            ),
            (
                r#"fs/create_dir(Path/of_str("a"))"#,
                vec![(HostOp::DirCreate, denied(vec![]))],
            ),
            (
                r#"fs/remove_dir(Path/of_str("a"))"#,
                vec![(HostOp::DirRemove, denied(vec![]))],
            ),
            (
                "let _ = fs/cwd!; Try/pure(())",
                vec![(HostOp::ProcCwd, denied(vec![bytes(b"")]))],
            ),
        ],
    );
}

#[test]
fn a_device_row_s_failure_is_the_error_it_names() {
    let open = r#"Serial/open(Path/of_str("/dev/x"), Serial/config(9600))"#;

    each_fails(
        In::Io,
        vec![
            (
                "Tty/raw(Tty/stdin, true)",
                vec![(HostOp::TtyRaw, denied(vec![]))],
            ),
            (
                "let _ = Tty/size(Tty/stdin)!; Try/pure(())",
                vec![(HostOp::TtySize, denied(vec![nat(0), nat(0)]))],
            ),
            (
                &format!("let _ = {open}!; Try/pure(())"),
                vec![(HostOp::SerialOpen, denied(vec![bytes(b"")]))],
            ),
            (
                &format!("let s = {open}!; Serial/set_dtr(s, true)"),
                vec![
                    (HostOp::SerialOpen, ok(vec![bytes(&[7])])),
                    (HostOp::SerialControl, denied(vec![])),
                ],
            ),
        ],
    );
}

#[test]
fn a_network_row_s_failure_is_the_error_it_names() {
    let looked_up = || (HostOp::DnsLookup, ok(vec![bytes(&[7])]));
    let resolved = || {
        (
            HostOp::DnsResolve,
            ok(vec![RawValue::BytesList(vec![b"a".to_vec()])]),
        )
    };
    let opened = || (HostOp::SocketOpen, ok(vec![bytes(&[8])]));
    let connect = r#"let _ = tcp/Socket/connect("h", 80)!; Try/pure(())"#;
    let listen = r#"let _ = tcp/Listener/listen("h", 80)!; Try/pure(())"#;

    each_fails(
        In::Async,
        vec![
            (
                r#"let _ = tcp/resolve("h", 80)!; Try/pure(())"#,
                vec![(HostOp::DnsLookup, denied(vec![bytes(b"")]))],
            ),
            (
                r#"let _ = tcp/resolve("h", 80)!; Try/pure(())"#,
                vec![
                    looked_up(),
                    (
                        HostOp::DnsResolve,
                        denied(vec![RawValue::BytesList(vec![])]),
                    ),
                ],
            ),
            (
                connect,
                vec![
                    looked_up(),
                    resolved(),
                    (HostOp::SocketOpen, denied(vec![bytes(b"")])),
                ],
            ),
            (
                connect,
                vec![
                    looked_up(),
                    resolved(),
                    opened(),
                    (HostOp::SocketConnect, denied(vec![])),
                ],
            ),
            (
                connect,
                vec![
                    looked_up(),
                    resolved(),
                    opened(),
                    (HostOp::SocketConnect, vec![nat(status::WOULD_BLOCK)]),
                    (HostOp::SocketFinishConnect, denied(vec![])),
                ],
            ),
            (
                r#"let _ = tcp/Socket/connect_tls("h", 80)!; Try/pure(())"#,
                vec![
                    looked_up(),
                    resolved(),
                    opened(),
                    (HostOp::SocketConnect, ok(vec![])),
                    (HostOp::TlsStart, denied(vec![])),
                ],
            ),
            (
                listen,
                vec![
                    looked_up(),
                    resolved(),
                    opened(),
                    (HostOp::SocketBind, denied(vec![])),
                ],
            ),
            (
                listen,
                vec![
                    looked_up(),
                    resolved(),
                    opened(),
                    (HostOp::SocketBind, ok(vec![])),
                    (HostOp::SocketListen, denied(vec![])),
                ],
            ),
            (
                r#"let l = tcp/Listener/listen("h", 80)!;
                   let _ = tcp/Listener/accept(l)!;
                   Try/pure(())"#,
                vec![
                    looked_up(),
                    resolved(),
                    opened(),
                    (HostOp::SocketBind, ok(vec![])),
                    (HostOp::SocketListen, ok(vec![])),
                    (HostOp::SocketAccept, denied(vec![bytes(b"")])),
                ],
            ),
            (
                r#"tcp/Listener/serve_tls("h", 80, x[], x[], (_) => Async/pure(()))"#,
                vec![(HostOp::TlsServerConfig, denied(vec![bytes(b"")]))],
            ),
        ],
    );
}

#[test]
fn a_process_row_s_failure_is_the_error_it_names() {
    let spawned = || (HostOp::ProcSpawn, ok(vec![bytes(&[7])]));

    each_fails(
        In::Io,
        vec![
            (
                r#"let _ = Command/spawn(Command/new("x", []))!; Try/pure(())"#,
                vec![(HostOp::ProcSpawn, denied(vec![bytes(b"")]))],
            ),
            (
                r#"let _ = Command/spawn(Command { ..Command/new("x", []), stdout = Stdio/piped() })!;
                   Try/pure(())"#,
                vec![spawned(), (HostOp::ProcStream, denied(vec![bytes(b"")]))],
            ),
        ],
    );
    each_fails(
        In::Async,
        vec![(
            r#"let c = Command/spawn(Command/new("x", []))!;
               let _ = Child/wait(c)!;
               Try/pure(())"#,
            vec![spawned(), (HostOp::ProcWait, denied(vec![nat(0), nat(0)]))],
        )],
    );
}

/// `/std` absorbs two failures by design: a listener's `SO_REUSEADDR` is a courtesy the bind does not depend on, and a kill that fails leaves a child the bracket reaps regardless.
#[test]
fn the_failures_std_absorbs_by_design_are_absorbed() {
    let spawned = (HostOp::ProcSpawn, ok(vec![bytes(&[7])]));

    assert_eq!(
        exit_code(
            In::Async,
            r#"let _ = tcp/Listener/listen("h", 80)!; Try/pure(())"#,
            vec![
                (HostOp::DnsLookup, ok(vec![bytes(&[7])])),
                (
                    HostOp::DnsResolve,
                    ok(vec![RawValue::BytesList(vec![b"a".to_vec()])]),
                ),
                (HostOp::SocketOpen, ok(vec![bytes(&[8])])),
                (HostOp::SocketSetReuseaddr, denied(vec![])),
                (HostOp::SocketBind, ok(vec![])),
                (HostOp::SocketListen, ok(vec![])),
            ],
        ),
        0
    );
    assert_eq!(
        exit_code(
            In::Io,
            r#"let c = Command/spawn(Command/new("x", []))!;
               let _ = Child/kill(c)!;
               Try/pure(())"#,
            vec![spawned, (HostOp::ProcKill, denied(vec![]))],
        ),
        0
    );
}

/// A stream that was not piped is one the host has no end of, which reaches the program as `not_found`.
#[test]
fn an_unpiped_stream_is_not_found() {
    assert_eq!(
        exit_code(
            In::Io,
            r#"let _ = Command/spawn(Command { ..Command/new("x", []), stdout = Stdio/piped() })!;
               Try/pure(())"#,
            vec![
                (HostOp::ProcSpawn, ok(vec![bytes(&[7])])),
                (HostOp::ProcStream, vec![nat(status::NOT_FOUND), bytes(b"")]),
            ],
        ),
        2
    );
}

/// A lookup answers a value or its absence, and an empty value is a value: a variable set to nothing is not a variable that is unset.
#[test]
fn an_empty_variable_is_set_and_an_absent_one_is_not() {
    let (host, io) = MockHost::builder().env([("EMPTY", "")]).build();
    let compiled = compile(
        r#"
        use /std/{Option, Bytes, Str, print};
        let said(v: Option(Bytes)) -> Str =
            match v
            | some(b) => match Bytes/len(b) == 0 | true => "empty" | false => "set" end
            | none() => "unset"
            end;
        let a = /std/proc/env("EMPTY")!;
        let b = /std/proc/env("NOPE")!;
        print(Str/flatten([said(a), " ", said(b)]))
        "#,
    )
    .expect("the program compiles");

    // SAFETY: precompiled in this process by `compile`.
    unsafe { run_raw(&compiled.cwasm, host, vec![], ForeignBindings::empty()) }
        .expect("the program runs");

    assert_eq!(io.output(), b"empty unset");
}

/// The working directory is the one `Bytes` payload no other suite reads back.
#[test]
fn a_bytes_payload_arrives_whole() {
    let (host, io) = MockHost::builder().build();
    let compiled = compile(
        r#"
        use /std/{Io, Path, Try, fs};
        let r = Try/run(fs/cwd)!;
        match r | success(p) => Io/write(Io/stdout, Path/to_bytes(p)) | failure(_) => Io/pure(()) end
        "#,
    )
    .expect("the program compiles");

    // SAFETY: precompiled in this process by `compile`.
    unsafe {
        run_raw(
            &compiled.cwasm,
            host,
            vec![(HostOp::ProcCwd, ok(vec![bytes(b"/work")]))],
            ForeignBindings::empty(),
        )
    }
    .expect("the program runs");

    assert_eq!(io.output(), b"/work");
}
