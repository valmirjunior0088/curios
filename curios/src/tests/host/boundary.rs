//! What a program makes of a host's reply. A reply is a value of its wire type because whatever answered the row lowered it as one, so the program checks nothing where it receives one: the library reads the status as the outcome it names, whatever the status. Each failure here is answered at its row's type by a front over the mock host, and reaches the program through the adapter every host answers by.

use {
    super::test_support::*,
    crate::test_support::*,
    curios_abi::{Failure, HostOp, StatusCodes},
    curios_native::{ForeignBindings, test_support::*},
};

/// Run `source` against `host`, every call of `row` failing with `failure`.
fn failing(source: &str, host: MockHost, row: HostOp, failure: Failure) -> Result<u8, String> {
    let compiled = compile(source).expect("the program compiles");

    compiled
        .run(
            Failing {
                behind: host,
                row,
                failure,
            },
            ForeignBindings::empty(),
        )
        .map_err(|stop| stop.to_string())
}

/// What `source` wrote when every call of `row` failed with `failure`.
fn written(source: &str, row: HostOp, failure: Failure) -> Vec<u8> {
    let (host, io) = MockHost::builder().build();

    failing(source, host, row, failure).expect("the reply is the program's to read");
    io.output()
}

const OPEN: &str = r#"
    let entry() -> /std/Io({}) =
        let r = /std/Try/run(/std/Try/scope((s) => /std/File/open(s, /std/Path/of_str("a.txt"), /std/File/Mode/read())))!;
        match r
        | success(_) => /std/print("opened")
        | failure(e) => /std/print(/std/Show/show(e))
        end;
    entry()
"#;

/// The reader has no status it cannot read: one an open is not expected to answer is the failure it names all the same.
#[test]
fn a_status_a_row_is_not_expected_to_answer_is_the_failure_it_names() {
    assert_eq!(
        written(OPEN, HostOp::FileOpen, Failure::ConnectionReset,),
        b"connection_reset"
    );
}

/// A status past every named one, and past the largest errno an operating system reports, is still a failure, carried as the number it is.
#[test]
fn a_status_no_failure_is_named_by_is_carried_as_its_number() {
    let past = StatusCodes::ERRNO_MAX + 1;

    assert_eq!(
        written(
            OPEN,
            HostOp::FileOpen,
            Failure::Other(
                u32::try_from(past).expect("an errno past the largest still fits its carrier")
            ),
        ),
        format!("other({past})").into_bytes()
    );
}

/// A write the host answers would block took the whole of its message and still holds some of it, so the library waits on a flush of the handle and never sends the write again: the write succeeds once the flush drains.
#[test]
fn a_write_the_host_answers_would_block_is_handed_on_by_a_flush() {
    let code = failing(
        r#"
        use /std/{Io, Try, Proc};
        let entry() -> /std/Io({}) =
            let r = Try/run(Io/write(Proc/stdout, 0x[0x41]))!;
            match r | success(_) => Proc/exit(@{}, 0) | failure(_) => Proc/exit(@{}, 7) end;
        entry()
        "#,
        MockHost::builder().build().0,
        HostOp::HandleWrite,
        Failure::WouldBlock,
    );

    assert_eq!(code, Ok(0));
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

    compiled
        .run(host, ForeignBindings::empty())
        .expect("the program runs");
    assert_eq!(io.output(), b"done");
}

/// A row is private to the module that reads it, so no program reaches one unread: naming it is refused where the name is written.
#[test]
fn a_program_cannot_name_a_row_the_library_declares() {
    let refusal = typecheck(
        r#"
        use /std/Proc/{proc_args};
        /std/print("unread")
        "#,
    )
    .expect_err("the row is private to `/std/Proc`");

    assert!(
        refusal.contains("`proc_args` is private to `/std/Proc`"),
        "{refusal}"
    );
}
