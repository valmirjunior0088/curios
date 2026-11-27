//! How a row's outcome reaches a program: `/std` reads the status before any payload, and hands the failure on as the error it names. Every fallible row is made to fail in turn, and the program ends with the code its failure maps to; the two failures `/std` absorbs by design are absorbed; and the payload shapes no other suite reaches arrive whole. Each failure is answered at its row's type by a front over the mock host, which answers every row before it as a host would, and reaches the program through the adapter every host answers by.

use {
    super::test_support::*,
    crate::test_support::*,
    curios_abi::{ChildExit, Failure, HostFront, HostOp, HostOps, Later},
    curios_native::{ForeignBindings, test_support::*},
    std::future,
};

/// The code a program ends with for `permission_denied`, the failure each case's row answers; `0` is a success, `2` a `not_found` and `1` any other failure.
const DENIED: u8 = 3;

/// What every fixture begins with: the imports, and `verdict`, which turns an outcome into the code the program exits with.
const PRELUDE: &str = "
use /std/{Byte, Io, Result, Try, File, Path, fs, Tty, Serial, Proc, tcp};
let code(e: Io/Error) -> Byte => match e | permission_denied() => 3 | not_found() => 2 | _ => 1 end;
let verdict(@A: Type, r: Result(Io/Error, A)) -> Byte => match r | success(_) => 0 | failure(e) => code(e) end;
";

/// Run `subject`, a `Try` over `Io` with no value inside a scope `s`, against `host`, and the code it exits with.
fn exit_code(subject: &str, host: impl HostOps + Send + Sync + 'static) -> u8 {
    let tail = "
let subject() -> Try(Io, Io/Error, {}) =>
    Try/scope((s) => SUBJECT);
let main() -> Io({}) =>
    let r = Try/run(subject())!;
    Proc/exit(@{}, verdict(r));
main()
";
    let source = format!("{PRELUDE}{}", tail.replace("SUBJECT", subject));
    let compiled = compile(&source).expect("the fixture compiles");
    compiled
        .run(host, ForeignBindings::empty())
        .unwrap_or_else(|error| panic!("the fixture runs to its exit: {error}"))
}

/// A mock host scripted with nothing.
fn bare() -> MockHost {
    MockHost::builder().build().0
}

/// `host`, every call of `row` answered `permission_denied`.
fn denying(host: MockHost, row: HostOp) -> Failing {
    Failing {
        behind: host,
        row,
        failure: Failure::PermissionDenied,
    }
}

/// A host whose terminal is open: what a terminal row's failure is reached through.
fn with_terminal() -> MockHost {
    MockHost::builder().tty_size(80, 24).build().0
}

/// A host with a serial port at `/dev/x`.
fn with_port() -> MockHost {
    MockHost::builder()
        .serial([("/dev/x", Vec::<&[u8]>::new())])
        .build()
        .0
}

/// A host serving `h:80`, with `pending` making a connect answer `WouldBlock` before its finish.
fn with_peer(pending: bool) -> MockHost {
    let builder = MockHost::builder().net([("h:80", "")]);
    match pending {
        true => builder.connect_pending().build().0,
        false => builder.build().0,
    }
}

/// A host that can start `x`, a child exiting `0` at once, or one running until it is killed.
fn with_child(running: bool) -> MockHost {
    let builder = MockHost::builder();
    match running {
        true => builder.running_children(["x"]).build().0,
        false => {
            builder
                .children([("x", "", "", ChildExit::Code(0))])
                .build()
                .0
        }
    }
}

/// Each case's row fails, and the program exits with the code its failure maps to.
fn each_fails(cases: Vec<(&str, Failing)>) {
    for (subject, host) in cases {
        assert_eq!(exit_code(subject, host), DENIED, "{subject}");
    }
}

#[test]
fn a_stream_row_s_failure_is_the_error_it_names() {
    each_fails(vec![
        (
            "let _ = Io/read(/std/Proc/stdin, 8)!;
                 Try/pure(())",
            denying(bare(), HostOp::HandleRead),
        ),
        (
            "Io/write(/std/Proc/stdout, 0x[1])",
            denying(bare(), HostOp::HandleWrite),
        ),
        (
            "Io/flush(/std/Proc/stdout)",
            denying(bare(), HostOp::HandleFlush),
        ),
    ]);
}

#[test]
fn a_filesystem_row_s_failure_is_the_error_it_names() {
    each_fails(vec![
        (
            r#"let _ = File/open(s, Path/of_str("a"), File/Mode/read())!; Try/pure(())"#,
            denying(bare(), HostOp::FileOpen),
        ),
        (
            r#"let _ = fs/stat(Path/of_str("a"))!; Try/pure(())"#,
            denying(bare(), HostOp::FileStat),
        ),
        (
            r#"fs/remove_file(Path/of_str("a"))"#,
            denying(bare(), HostOp::FileRemove),
        ),
        (
            r#"fs/rename(Path/of_str("a"), Path/of_str("b"))"#,
            denying(bare(), HostOp::FileRename),
        ),
        (
            r#"let _ = fs/list(Path/of_str("a"))!; Try/pure(())"#,
            denying(bare(), HostOp::DirList),
        ),
        (
            r#"fs/create_dir(Path/of_str("a"))"#,
            denying(bare(), HostOp::DirCreate),
        ),
        (
            r#"fs/remove_dir(Path/of_str("a"))"#,
            denying(bare(), HostOp::DirRemove),
        ),
        (
            "let _ = Proc/cwd()!; Try/pure(())",
            denying(bare(), HostOp::ProcCwd),
        ),
    ]);
}

#[test]
fn a_device_row_s_failure_is_the_error_it_names() {
    let open = r#"Serial/open(s, Path/of_str("/dev/x"), Serial/config(9600))"#;

    each_fails(vec![
        (
            "let t = Tty/open(s)!; Tty/raw(s, t, 0x[])",
            denying(with_terminal(), HostOp::TtyTake),
        ),
        (
            "let t = Tty/open(s)!; let _ = Tty/size(t)!; Try/pure(())",
            denying(with_terminal(), HostOp::TtySize),
        ),
        (
            &format!("let _ = {open}!; Try/pure(())"),
            denying(bare(), HostOp::SerialOpen),
        ),
        (
            "let _ = Serial/list()!; Try/pure(())",
            denying(bare(), HostOp::SerialList),
        ),
        (
            &format!("let port = {open}!; Serial/set_dtr(port, true)"),
            denying(with_port(), HostOp::SerialControl),
        ),
    ]);
}

#[test]
fn a_serial_drain_s_failure_is_the_error_it_names() {
    each_fails(vec![(
        r#"let port = Serial/open(s, Path/of_str("/dev/x"), Serial/config(9600))!; Serial/drain(port)"#,
        // A drain hands on what the host holds of the port's writes before the device's wait, which the port answers.
        denying(with_port(), HostOp::SerialDrain),
    )]);
}

#[test]
fn a_network_row_s_failure_is_the_error_it_names() {
    let connect = r#"let _ = tcp/Socket/connect(s, "h", 80)!; Try/pure(())"#;

    each_fails(vec![
        (
            r#"let _ = tcp/resolve("h", 80)!; Try/pure(())"#,
            denying(bare(), HostOp::DnsLookup),
        ),
        (connect, denying(bare(), HostOp::SocketOpen)),
        (connect, denying(bare(), HostOp::SocketConnect)),
        (
            connect,
            denying(with_peer(true), HostOp::SocketFinishConnect),
        ),
        (
            r#"let _ = tcp/Socket/connect_tls(s, "h", 80)!; Try/pure(())"#,
            denying(with_peer(false), HostOp::TlsStart),
        ),
        (
            r#"let _ = tcp/Listener/listen(s, "h", 80)!; Try/pure(())"#,
            denying(bare(), HostOp::SocketListen),
        ),
        (
            r#"let l = tcp/Listener/listen(s, "h", 80)!;
                   let _ = tcp/Listener/accept(s, l)!;
                   Try/pure(())"#,
            denying(bare(), HostOp::SocketAccept),
        ),
        (
            r#"tcp/Listener/serve_tls("h", 80, 0x[], 0x[], (_) => Io/pure(()))"#,
            denying(bare(), HostOp::TlsServerConfig),
        ),
        (
            r#"let c = tcp/Socket/connect(s, "h", 80)!; Io/finish(c)"#,
            denying(with_peer(false), HostOp::HandleFinish),
        ),
    ]);
}

/// A host whose every lookup resolves to no address, every other row answered as `behind` does.
struct Unresolved {
    behind: MockHost,
}

impl HostFront for Unresolved {
    type Behind = MockHost;

    fn behind(&self) -> &MockHost {
        &self.behind
    }

    fn dns_lookup(&self, _host: Vec<u8>, _port: u64) -> Later<Result<Vec<Vec<u8>>, Failure>> {
        Later::new(future::ready(Ok(Vec::new())))
    }
}

/// A name that resolves to no address names nothing a socket can reach: the resolution is an answer, and `/std` reads it as `not_found`, as it reads a name that resolves to nothing at all.
#[test]
fn a_name_resolving_to_no_address_is_not_found() {
    assert_eq!(
        exit_code(
            r#"let _ = tcp/Socket/connect(s, "h", 80)!; Try/pure(())"#,
            Unresolved { behind: bare() },
        ),
        2
    );
}

#[test]
fn a_process_row_s_failure_is_the_error_it_names() {
    each_fails(vec![(
        r#"let _ = Proc/Child/spawn(s, Proc/new("x", []))!; Try/pure(())"#,
        denying(bare(), HostOp::ProcSpawn),
    )]);
    each_fails(vec![(
        r#"let c = Proc/Child/spawn(s, Proc/new("x", []))!;
               let _ = Proc/Child/wait(c)!;
               Try/pure(())"#,
        denying(with_child(false), HostOp::ProcWait),
    )]);
}

/// `/std` absorbs two failures by design: a kill that fails leaves a child the scope reaps regardless, and a socket's finish is a courtesy to a peer that may already be gone.
#[test]
fn the_failures_std_absorbs_by_design_are_absorbed() {
    assert_eq!(
        exit_code(
            r#"let _ = tcp/Socket/connect(s, "h", 80)!; Try/pure(())"#,
            denying(with_peer(false), HostOp::HandleFinish),
        ),
        0
    );
    assert_eq!(
        exit_code(
            r#"let c = Proc/Child/spawn(s, Proc/new("x", []))!;
               let _ = Proc/Child/kill(c)!;
               Try/pure(())"#,
            denying(with_child(true), HostOp::ProcKill),
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
        let said(v: Option(Bytes)) -> Str =>
            match v
            | some(b) => match Bytes/length(b) == 0 | true => "empty" | false => "set" end
            | none() => "unset"
            end;
        let main() -> /std/Io({}) =>
        let a = /std/Proc/env("EMPTY")!;
        let b = /std/Proc/env("NOPE")!;
        print(Str/flatten([said(a), " ", said(b)]));
        main()
        "#,
    )
    .expect("the program compiles");

    compiled
        .run(host, ForeignBindings::empty())
        .expect("the program runs");

    assert_eq!(io.output(), b"empty unset");
}

/// The working directory is the one `Bytes` payload no other suite reads back.
#[test]
fn a_bytes_payload_arrives_whole() {
    let (host, io) = MockHost::builder().cwd("/work").build();
    let compiled = compile(
        r#"
        use /std/{Io, Path, Try, Proc};
        let main() -> Io({}) =>
        let r = Try/run(Proc/cwd())!;
        match r | success(p) => Io/bind(Try/run(Io/write(Proc/stdout, Path/to_bytes(p))), (_) => Io/pure(())) | failure(_) => Io/pure(()) end;
        main()
        "#,
    )
    .expect("the program compiles");

    compiled
        .run(host, ForeignBindings::empty())
        .expect("the program runs");

    assert_eq!(io.output(), b"/work");
}
