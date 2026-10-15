//! How a row's outcome reaches a program: `/std` reads the status before any payload, and hands the failure on as the error it names. Every fallible row is scripted to fail, and the program ends with the code its failure maps to; the two failures `/std` absorbs by design are absorbed; and the payload shapes no other suite reaches arrive whole. Each reply is written raw, past the native adapter, as any host could answer it.

use {
    crate::tests::compile,
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
use /std/{Byte, Io, Result, Try, Handle, File, Path, fs, Tty, Serial, Command, tcp};
use /std/Command/{Stdio, Child};
let code(e: Io/Error) -> Byte = match e | permission_denied() => 3 | not_found() => 2 | _ => 1 end;
let verdict(@A: Type, r: Result(Io/Error, A)) -> Byte = match r | success(_) => 0 | failure(e) => code(e) end;
";

/// The rows a fixture scripts, each with the raw reply it answers every call with.
type Replies = Vec<(HostOp, Vec<RawValue>)>;

/// Run `subject`, a `Try` over `Io` with no value, against a mock host whose rows in `replies` answer raw, and the code it exits with.
fn exit_code(subject: &str, replies: Replies) -> u8 {
    let tail = "
let subject: Try(Io, Io/Error, {}) =
    SUBJECT;
let r = Try/run(subject)!;
/std/proc/exit(@{}, verdict(r))
";
    let source = format!("{PRELUDE}{}", tail.replace("SUBJECT", subject));
    let compiled = compile(&source).expect("the fixture compiles");

    run_raw(
        &compiled,
        MockHost::builder().build().0,
        replies,
        ForeignBindings::empty(),
    )
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
fn each_fails(cases: Vec<(&str, Replies)>) {
    for (subject, replies) in cases {
        assert_eq!(exit_code(subject, replies), DENIED, "{subject}");
    }
}

#[test]
fn a_stream_row_s_failure_is_the_error_it_names() {
    each_fails(vec![
        (
            "let c = Io/read(Io/stdin, 8)!;
                 match c | error(e) => Try/raise(e) | _ => Try/pure(()) end",
            vec![(HostOp::HandleRead, denied(vec![bytes(b"")]))],
        ),
        (
            "Try/attempt(Io/Write/write(Handle/stdout, x[1]))",
            vec![(HostOp::HandleWrite, denied(vec![nat(0)]))],
        ),
        (
            "Try/attempt(Io/Write/flush(Handle/stdout))",
            vec![(HostOp::HandleFlush, denied(vec![]))],
        ),
    ]);
}

#[test]
fn a_filesystem_row_s_failure_is_the_error_it_names() {
    each_fails(vec![
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
            "let _ = fs/cwd()!; Try/pure(())",
            vec![(HostOp::ProcCwd, denied(vec![bytes(b"")]))],
        ),
    ]);
}

#[test]
fn a_device_row_s_failure_is_the_error_it_names() {
    let open = r#"Serial/open(Path/of_str("/dev/x"), Serial/config(9600))"#;

    each_fails(vec![
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
            "let _ = Serial/list()!; Try/pure(())",
            vec![(
                HostOp::SerialList,
                denied(vec![RawValue::BytesList(vec![])]),
            )],
        ),
        (
            &format!("let s = {open}!; Serial/set_dtr(s, true)"),
            vec![
                (HostOp::SerialOpen, ok(vec![bytes(&[7])])),
                (HostOp::SerialControl, denied(vec![])),
            ],
        ),
    ]);
}

#[test]
fn a_serial_drain_s_failure_is_the_error_it_names() {
    each_fails(vec![(
        r#"let s = Serial/open(Path/of_str("/dev/x"), Serial/config(9600))!; Serial/drain(s)"#,
        vec![
            (HostOp::SerialOpen, ok(vec![bytes(&[7])])),
            (HostOp::SerialDrain, denied(vec![bytes(b"")])),
        ],
    )]);
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

    each_fails(vec![
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
            r#"let _ = tcp/Listener/listen("h", 80)!; Try/pure(())"#,
            vec![
                looked_up(),
                resolved(),
                (HostOp::SocketListen, denied(vec![bytes(b"")])),
            ],
        ),
        (
            r#"let l = tcp/Listener/listen("h", 80)!;
                   let _ = tcp/Listener/accept(l)!;
                   Try/pure(())"#,
            vec![
                looked_up(),
                resolved(),
                (HostOp::SocketListen, ok(vec![bytes(&[8])])),
                (HostOp::SocketAccept, denied(vec![bytes(b"")])),
            ],
        ),
        (
            r#"tcp/Listener/serve_tls("h", 80, x[], x[], (_) => Io/pure(()))"#,
            vec![(HostOp::TlsServerConfig, denied(vec![bytes(b"")]))],
        ),
        (
            "Try/attempt(Handle/finish(Handle/stdout))",
            vec![(HostOp::HandleFinish, denied(vec![]))],
        ),
    ]);
}

/// A name that resolves to no address names nothing a socket can reach: the resolution is an answer, and `/std` reads it as `not_found`, as it reads a name that resolves to nothing at all.
#[test]
fn a_name_resolving_to_no_address_is_not_found() {
    assert_eq!(
        exit_code(
            r#"let _ = tcp/Socket/connect("h", 80)!; Try/pure(())"#,
            vec![
                (HostOp::DnsLookup, ok(vec![bytes(&[7])])),
                (HostOp::DnsResolve, ok(vec![RawValue::BytesList(vec![])])),
            ],
        ),
        2
    );
}

/// A spawn with no stream piped: the child's handle, and the empty token for each stream.
fn spawned() -> (HostOp, Vec<RawValue>) {
    (
        HostOp::ProcSpawn,
        ok(vec![bytes(&[7]), bytes(b""), bytes(b""), bytes(b"")]),
    )
}

#[test]
fn a_process_row_s_failure_is_the_error_it_names() {
    each_fails(vec![(
        r#"let _ = Command/spawn(Command/new("x", []))!; Try/pure(())"#,
        vec![(HostOp::ProcSpawn, denied(vec![bytes(b""); 4]))],
    )]);
    each_fails(vec![(
        r#"let c = Command/spawn(Command/new("x", []))!;
               let _ = Child/wait(c)!;
               Try/pure(())"#,
        vec![spawned(), (HostOp::ProcWait, denied(vec![nat(0), nat(0)]))],
    )]);
}

/// `/std` absorbs two failures by design: a kill that fails leaves a child the bracket reaps regardless, and a socket's finish is a courtesy to a peer that may already be gone.
#[test]
fn the_failures_std_absorbs_by_design_are_absorbed() {
    assert_eq!(
        exit_code(
            r#"tcp/Socket/with(tcp/Socket/connect("h", 80), (_) => Try/pure(()))"#,
            vec![
                (HostOp::DnsLookup, ok(vec![bytes(&[7])])),
                (
                    HostOp::DnsResolve,
                    ok(vec![RawValue::BytesList(vec![b"a".to_vec()])]),
                ),
                (HostOp::SocketOpen, ok(vec![bytes(&[8])])),
                (HostOp::SocketConnect, ok(vec![])),
                (HostOp::HandleFlush, ok(vec![])),
                (HostOp::HandleFinish, denied(vec![])),
            ],
        ),
        0
    );
    assert_eq!(
        exit_code(
            r#"let c = Command/spawn(Command/new("x", []))!;
               let _ = Child/kill(c)!;
               Try/pure(())"#,
            vec![spawned(), (HostOp::ProcKill, denied(vec![]))],
        ),
        0
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

    run_raw(&compiled, host, vec![], ForeignBindings::empty()).expect("the program runs");

    assert_eq!(io.output(), b"empty unset");
}

/// The working directory is the one `Bytes` payload no other suite reads back.
#[test]
fn a_bytes_payload_arrives_whole() {
    let (host, io) = MockHost::builder().build();
    let compiled = compile(
        r#"
        use /std/{Io, Path, Try, fs};
        let r = Try/run(fs/cwd())!;
        match r | success(p) => Io/write(Io/stdout, Path/to_bytes(p)) | failure(_) => Io/pure(()) end
        "#,
    )
    .expect("the program compiles");

    run_raw(
        &compiled,
        host,
        vec![(HostOp::ProcCwd, ok(vec![bytes(b"/work")]))],
        ForeignBindings::empty(),
    )
    .expect("the program runs");

    assert_eq!(io.output(), b"/work");
}
