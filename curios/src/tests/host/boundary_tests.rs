//! The guest's own checks of a host's reply: a host that answers outside its row — a status the row never answers, a success its checks refuse, a value its wire type cannot be — is refused by the program itself, whichever host it is, while a failure's inert padding is accepted. Each reply is written raw, past the native adapter, which would refuse it first.

use {
    super::super::compile,
    crate::to_cwasm,
    curios_abi::{ChildExit, HostOp, status},
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{
        ForeignBindings, MockHost,
        test_support::{RawValue, raw_reply, run_raw},
    },
    curios_text::{Entrypoint, RootSource},
};

/// The sentence the guest refuses a reply outside its row with.
const REFUSED: &str = "the host answered a call with a value outside that call's contract";

/// Run `source` against `host`, except that `op` answers `reply` raw on every call.
fn answering(source: &str, host: MockHost, op: HostOp, reply: Vec<RawValue>) -> Result<u8, String> {
    let compiled = compile(source).expect("the program compiles");

    // SAFETY: precompiled in this process by `compile`.
    unsafe {
        run_raw(
            &compiled.cwasm,
            host,
            vec![(op, reply)],
            ForeignBindings::empty(),
        )
    }
}

fn refused(result: Result<u8, String>) {
    let error = result.expect_err("the reply is refused");

    assert!(error.contains(REFUSED), "{error}");
}

fn mock() -> MockHost {
    MockHost::builder().build().0
}

const OPEN: &str = r#"
    let r = /std/Try/run(/std/File/open(/std/Path/of_str("a.txt"), /std/File/Mode/read()))!;
    match r
    | success(_) => /std/print("opened")
    | failure(e) => /std/print(/std/Show/show(e))
    end
"#;

/// `file/open` is not marked as blocking, so `would_block` is no status it answers.
#[test]
fn a_status_the_row_never_answers_is_refused() {
    refused(answering(
        OPEN,
        mock(),
        HostOp::FileOpen,
        vec![RawValue::Nat(status::WOULD_BLOCK), RawValue::Bytes(vec![])],
    ));
}

/// The errno lane ends at the largest errno an OS reports; a status in it is an ordinary failure the program handles, and one past it is no status at all.
#[test]
fn the_errno_lane_ends_where_an_errno_can() {
    let (host, io) = MockHost::builder().build();
    let inside = answering(
        OPEN,
        host,
        HostOp::FileOpen,
        vec![
            RawValue::Nat(status::OTHER_BASE + 5),
            RawValue::Bytes(vec![]),
        ],
    );

    assert!(inside.is_ok(), "{inside:?}");
    assert!(!io.output().is_empty());

    refused(answering(
        OPEN,
        mock(),
        HostOp::FileOpen,
        vec![
            RawValue::Nat(status::OTHER_BASE + status::ERRNO_MAX + 1),
            RawValue::Bytes(vec![]),
        ],
    ));
}

#[test]
fn a_success_with_the_empty_token_is_refused() {
    refused(answering(
        OPEN,
        mock(),
        HostOp::FileOpen,
        vec![RawValue::Nat(status::OK), RawValue::Bytes(vec![])],
    ));
}

/// A failure's padding is inert: the empty token that no success may carry is exactly what a failure answers beside its status, and the program reads the failure.
#[test]
fn a_failure_s_padding_is_accepted_though_no_success_could_carry_it() {
    let (host, io) = MockHost::builder().build();

    answering(
        OPEN,
        host,
        HostOp::FileOpen,
        vec![RawValue::Nat(status::NOT_FOUND), RawValue::Bytes(vec![])],
    )
    .expect("the failure is the program's to handle");
    assert_eq!(io.output(), b"not_found");
}

#[test]
fn a_read_past_its_request_is_refused() {
    let (host, _) = MockHost::builder().stdin_lines(["hello"]).build();

    refused(answering(
        "
        let _ = /std/Io/read(/std/Io/stdin, 8)!;
        /std/Io/pure(())
        ",
        host,
        HostOp::HandleRead,
        vec![RawValue::Nat(status::OK), RawValue::Bytes(vec![b'x'; 9])],
    ));
}

/// The second read waits for its chunk, so the program polls standard input — and a poll's answer is one mask per handle, each within the readiness bits.
const POLLING: &str = "
    let _ = /std/Io/read(/std/Io/stdin, 8)!;
    let _ = /std/Io/read(/std/Io/stdin, 8)!;
    /std/Io/pure(())
";

fn chunked() -> MockHost {
    MockHost::builder()
        .stdin_chunks(vec![b"a".as_slice(), b"b".as_slice()])
        .build()
        .0
}

#[test]
fn a_poll_mask_outside_the_readiness_bits_is_refused() {
    refused(answering(
        POLLING,
        chunked(),
        HostOp::HandlePoll,
        vec![RawValue::Bytes(vec![0b1_0000])],
    ));
}

#[test]
fn a_poll_that_answers_for_other_handles_is_refused() {
    refused(answering(
        POLLING,
        chunked(),
        HostOp::HandlePoll,
        vec![RawValue::Bytes(vec![])],
    ));
}

#[test]
fn a_clock_past_a_second_of_nanoseconds_is_refused() {
    refused(answering(
        "
        let _ = /std/time/Instant/now()!;
        /std/Io/pure(())
        ",
        mock(),
        HostOp::ClockMono,
        vec![RawValue::Nat(0), RawValue::Nat(1_000_000_000)],
    ));
}

#[test]
fn a_file_kind_outside_its_codes_is_refused() {
    refused(answering(
        r#"
        let _ = /std/Try/run(/std/fs/stat(/std/Path/of_str("a")))!;
        /std/Io/pure(())
        "#,
        mock(),
        HostOp::FileStat,
        vec![
            RawValue::Nat(status::OK),
            RawValue::Nat(7),
            RawValue::Nat(0),
            RawValue::Nat(0),
            RawValue::Nat(0),
        ],
    ));
}

/// A child ends by a code or by a signal, never by both.
#[test]
fn a_child_that_ended_both_ways_is_refused() {
    let (host, _) = MockHost::builder()
        .children([("done", "", "", ChildExit::Code(0))])
        .build();

    refused(answering(
        r#"
        use /std/{Try, Async, Io, Command};
        use /std/Command/{Child};
        let program: Try(Async, Io/Error, {}) =
            let child = Command/spawn(Command/new("done", []))!;
            let _ = Child/wait(child)!;
            Try/pure(());
        let fiber: Async({}) =
            let _ = Try/run(program)!;
            Async/pure(());
        Async/run(fiber)
        "#,
        host,
        HostOp::ProcWait,
        vec![
            RawValue::Nat(status::OK),
            RawValue::Nat(3),
            RawValue::Nat(9),
        ],
    ));
}

/// Nothing resumes after `exit`, so a host whose `exit` returns has broken its contract and cannot resume the program.
#[test]
fn a_host_that_returns_from_exit_is_refused() {
    refused(answering(
        "/std/proc/exit(@{}, 3)",
        mock(),
        HostOp::ProcExit,
        vec![],
    ));
}

/// A reply is checked when the call is made, whatever the program does with it afterwards: one it discards is held to its row as one it reads is.
#[test]
fn a_discarded_reply_is_still_checked() {
    refused(answering(
        r#"
        let _ = /std/rand/bytes(4)!;
        /std/print("done")
        "#,
        mock(),
        HostOp::RandBytes,
        vec![RawValue::Bytes(vec![0; 3])],
    ));
}

/// A description that is never performed makes no call, so the reply that would have been refused is never asked for.
#[test]
fn an_io_never_performed_asks_the_host_nothing() {
    let (host, io) = MockHost::builder().build();

    answering(
        r#"
        use /std/{Io, Bytes};
        let unused: Io(Bytes) = /std/rand/bytes(4);
        /std/print("done")
        "#,
        host,
        HostOp::RandBytes,
        vec![RawValue::Bytes(vec![0; 3])],
    )
    .expect("the call is never made");
    assert_eq!(io.output(), b"done");
}

/// Run `source`, whose `foreign` row `name` answers `reply` raw, as an embedder's own binding would answer it.
fn embedded(source: &str, name: &str, reply: Vec<RawValue>) -> Result<u8, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");
    let (module, foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("the program compiles");
    let cwasm = to_cwasm(&module).expect("the module precompiles");
    let mut bindings = ForeignBindings::new(foreigns);

    raw_reply(&mut bindings, name, reply);

    // SAFETY: precompiled in this process just above.
    unsafe { run_raw(&cwasm, mock(), vec![], bindings) }
}

/// An embedder's row is held to what its wire types can be: a `Bool` is `0` or `1`, and so is every element of a `List(Bool)`.
#[test]
fn an_embedder_s_bool_past_one_is_refused() {
    let flag = r#"
        foreign flag : (Nat) -> Bool;
        let b = flag(1)!;
        /std/proc/exit(@{}, match b | true => 1 | false => 0 end)
    "#;

    assert_eq!(embedded(flag, "/flag", vec![RawValue::Word(1)]), Ok(1));
    refused(embedded(flag, "/flag", vec![RawValue::Word(2)]));

    let flags = r#"
        foreign flags : (Nat) -> List(Bool);
        let _ = flags(1)!;
        /std/Io/pure(())
    "#;

    assert_eq!(
        embedded(flags, "/flags", vec![RawValue::Words(vec![0, 1])]),
        Ok(0)
    );
    refused(embedded(flags, "/flags", vec![RawValue::Words(vec![0, 2])]));
}
