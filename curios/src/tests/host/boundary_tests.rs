//! What a program makes of a host's reply. A reply is a value of its wire type because whatever answered the row lowered it as one, so the program checks nothing where it receives one: the library reads the status as the outcome it names, whatever the status, and a failure's padding reaches no reader. Each reply here is written raw, past the native adapter, to show what that reading does with it.

use {
    crate::tests::{compile, typecheck},
    curios_abi::{HostOp, status},
    curios_runtime::{
        ForeignBindings, MockHost,
        test_support::{RawValue, run_raw},
    },
};

/// Run `source` against `host`, except that `op` answers `reply` raw on every call.
fn answering(source: &str, host: MockHost, op: HostOp, reply: Vec<RawValue>) -> Result<u8, String> {
    let compiled = compile(source).expect("the program compiles");

    run_raw(&compiled, host, vec![(op, reply)], ForeignBindings::empty())
}

/// What `source` wrote when `op` answered `reply` raw on every call.
fn written(source: &str, op: HostOp, reply: Vec<RawValue>) -> Vec<u8> {
    let (host, io) = MockHost::builder().build();

    answering(source, host, op, reply).expect("the reply is the program's to read");
    io.output()
}

const OPEN: &str = r#"
    let r = /std/Try/run(/std/File/open(/std/Path/of_str("a.txt"), /std/File/Mode/read()))!;
    match r
    | success(_) => /std/print("opened")
    | failure(e) => /std/print(/std/Show/show(e))
    end
"#;

/// A failure's padding is inert: the empty token beside a failing status is never read as a handle, and the program reads the failure.
#[test]
fn a_failure_s_padding_reaches_no_reader() {
    assert_eq!(
        written(
            OPEN,
            HostOp::FileOpen,
            vec![RawValue::Nat(status::NOT_FOUND), RawValue::Bytes(vec![])],
        ),
        b"not_found"
    );
}

/// The reader has no status it cannot read: one an open is not expected to answer is the failure it names all the same.
#[test]
fn a_status_a_row_is_not_expected_to_answer_is_the_failure_it_names() {
    assert_eq!(
        written(
            OPEN,
            HostOp::FileOpen,
            vec![RawValue::Nat(status::WOULD_BLOCK), RawValue::Bytes(vec![])],
        ),
        b"would_block"
    );
}

/// A status past every named one, and past the largest errno an operating system reports, is still a failure, carried as the number it is.
#[test]
fn a_status_no_failure_is_named_by_is_carried_as_its_number() {
    let past = status::ERRNO_MAX + 1;

    assert_eq!(
        written(
            OPEN,
            HostOp::FileOpen,
            vec![
                RawValue::Nat(status::OTHER_BASE + past),
                RawValue::Bytes(vec![])
            ],
        ),
        format!("other({past})").into_bytes()
    );
}

/// A closed code is read by the library, which has no kind for a code it does not know: the stat fails, and no kind is made up.
#[test]
fn a_file_kind_no_code_names_fails_the_stat() {
    assert_eq!(
        written(
            r#"
            let r = /std/Try/run(/std/fs/stat(/std/Path/of_str("a")))!;
            match r
            | success(_) => /std/print("a kind")
            | failure(e) => /std/print(/std/Show/show(e))
            end
            "#,
            HostOp::FileStat,
            vec![
                RawValue::Nat(status::OK),
                RawValue::Nat(7),
                RawValue::Nat(0),
                RawValue::Nat(0),
                RawValue::Nat(0),
            ],
        ),
        b"other(0)"
    );
}

/// A write the host accepts nothing of would be resent for ever, so the library reads it as a failure and its loop ends.
#[test]
fn a_write_that_accepts_nothing_is_a_failure() {
    let code = answering(
        r#"
        use /std/{Io, Handle, proc};
        let r = Io/Write/write(@Handle, Handle/stdout, x[0x41])!;
        match r | success(_) => proc/exit(@{}, 0) | failure(_) => proc/exit(@{}, 7) end
        "#,
        MockHost::builder().build().0,
        HostOp::HandleWrite,
        vec![RawValue::Nat(status::OK), RawValue::Nat(0)],
    );

    assert_eq!(code, Ok(7));
}

/// Nothing resumes after `exit`, so the program holds no code for a return from it: a host whose `exit` returns traps where it lands.
#[test]
fn a_host_that_returns_from_exit_traps() {
    let error = answering(
        "/std/proc/exit(@{}, 3)",
        MockHost::builder().build().0,
        HostOp::ProcExit,
        vec![],
    )
    .expect_err("nothing follows the call");

    assert!(error.contains("unreachable"), "{error}");
}

/// A description that is never performed makes no call.
#[test]
fn an_io_never_performed_asks_the_host_nothing() {
    let (host, io) = MockHost::builder().build();
    let compiled = compile(
        r#"
        use /std/{Io};
        let unused: Io({}) = /std/print("performed");
        /std/print("done")
        "#,
    )
    .expect("the program compiles");

    run_raw(&compiled, host, vec![], ForeignBindings::empty()).expect("the program runs");
    assert_eq!(io.output(), b"done");
}

/// A row is private to the module that reads it, so no program reaches one unread: naming it is refused where the name is written.
#[test]
fn a_program_cannot_name_a_row_the_library_declares() {
    let refusal = typecheck(
        r#"
        use /std/Handle/{handle_read};
        /std/print("unread")
        "#,
    )
    .expect_err("the row is private to `/std/Handle`");

    assert!(
        refusal.contains("`handle_read` is private to `/std/Handle`"),
        "{refusal}"
    );
}
