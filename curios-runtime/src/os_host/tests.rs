use super::*;

/// Every entry in the handle table is one [`OsResource`], so the enum's size is what a plain file or an unconnected socket costs to hold. Boxing the two TLS variants took that from 1176 bytes to 16, measured 2026-08-23 by the `size_of` calls below — a `rustls` connection carries its record buffers inline, and unboxed it set the size of every other kind.
///
/// The bound is the guard rather than the figure: unboxing either variant, or adding a third large one inline, puts a kilobyte back on every handle and fails here.
#[test]
fn a_table_entry_does_not_carry_a_tls_connection_inline() {
    assert!(
        size_of::<OsResource>() <= 64,
        "OsResource is {} bytes; the TLS streams are {} and {}",
        size_of::<OsResource>(),
        size_of::<StreamOwned<ClientConnection, Socket>>(),
        size_of::<StreamOwned<ServerConnection, Socket>>(),
    );
}

/// The standard streams are never in the handle table, so a setter that asked the table would call them closed. `Async/read`/`write` begin with `set_nonblocking`, so that verdict would fail every asynchronous use of stdio on the real host while the mock — which answers `Ok` for any handle — lets the suite pass.
#[test]
fn a_standard_stream_takes_the_socket_setters_like_a_file() {
    let host = OsHost::with_args(vec![]);

    for handle in [Handle::Stdin, Handle::Stdout, Handle::Stderr] {
        assert!(matches!(
            host.set_nonblocking(handle.clone(), 1),
            Status::Ok
        ));
        assert!(matches!(
            host.set_recv_timeout(handle.clone(), 10),
            Status::Ok
        ));
        assert!(matches!(host.set_reuseaddr(handle, 1), Status::Ok));
    }
}

/// A descriptor that is not a terminal refuses both tty rows with `ENOTTY` through the errno lane, which is how a program learns it has none. `/dev/null` rather than a standard stream, because under an interactive `cargo test` stdin *is* a terminal and a passing `raw` would leave it in raw mode.
#[test]
fn the_tty_rows_on_a_descriptor_that_is_not_a_terminal_report_enotty() {
    let host = OsHost::with_args(vec![]);
    let (status, handle) = host.open(b"/dev/null", Mode::Read);

    assert!(matches!(status, Status::Ok));
    assert!(matches!(host.raw(handle.clone(), 1), Status::Other(25)));
    assert!(matches!(
        host.size(handle.clone()),
        (Status::Other(25), 0, 0)
    ));
    assert!(matches!(host.raw(handle.clone(), 0), Status::Ok));

    host.close(handle);
}

/// A pipe end is the one non-socket resource `set_nonblocking` acts on: with `O_NONBLOCK` applied an empty pipe answers `WouldBlock` instead of blocking the caller, bytes written at the other end come back through `read`, and the writer's close is `Eof`. The child streams `spawn` files as this kind are what let a fiber's drain yield instead of stalling the scheduler.
#[test]
fn a_pipe_end_becomes_non_blocking_and_reads_what_arrives() {
    let host = OsHost::with_args(vec![]);
    let (reader, mut writer) = std::io::pipe().expect("a pipe");
    let handle = host.mint(OsResource::Descriptor(OwnedFd::from(reader)));

    assert!(matches!(
        host.set_nonblocking(handle.clone(), 1),
        Status::Ok
    ));
    assert!(matches!(
        host.read(handle.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));

    writer
        .write_all(b"abc")
        .expect("the pipe takes three bytes");
    assert!(matches!(
        host.read(handle.clone(), 8),
        (Status::Ok, bytes) if bytes == b"abc"
    ));

    // Another test's child may have been forked while this write end was open and hold it until its `exec`, so the end of the stream may arrive a few reads late as `WouldBlock`.
    drop(writer);
    let mut outcome = host.read(handle.clone(), 8);
    for _ in 0..100 {
        if !matches!(outcome, (Status::WouldBlock, _)) {
            break;
        }
        std::thread::sleep(Duration::from_millis(10));
        outcome = host.read(handle.clone(), 8);
    }
    assert!(matches!(outcome, (Status::Eof, bytes) if bytes.is_empty()));

    host.close(handle);
}

/// A real child end to end: `echo` is spawned with its output piped, its handle becomes readable once the reaper has recorded the exit, `wait` reports a clean zero, and the piped output is what it wrote. The unpiped stdin comes back as the empty handle.
#[test]
fn a_child_is_reaped_through_its_handle_and_its_piped_output_read() {
    let host = OsHost::with_args(vec![]);
    let (status, child) = host.spawn(
        &[b"/bin/echo".to_vec(), b"hi".to_vec()],
        b"",
        &[],
        curios_abi::stdio_mode::INHERIT,
        curios_abi::stdio_mode::PIPE,
        curios_abi::stdio_mode::NULL,
    );

    assert!(matches!(status, Status::Ok));
    let (status, stdin) = host.stream(child.clone(), 0);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(&stdin, Handle::Other(token) if token.is_empty()));
    let (status, stdout) = host.stream(child.clone(), 1);
    assert!(matches!(status, Status::Ok));

    let ready = host.poll(
        std::slice::from_ref(&child),
        &[Poll::from_bits(curios_abi::poll::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::poll::READ, 0);
    assert!(matches!(host.wait(child), (Status::Ok, 0, 0)));
    assert!(matches!(
        host.read(stdout.clone(), 64),
        (Status::Ok, bytes) if bytes == b"hi\n"
    ));

    host.close(stdout);
}
