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

/// The standard streams are never in the handle table, so a setter that asked the table would call them closed; like a file, they record nothing and answer `Ok`, which is what the mock answers for any handle.
#[test]
fn a_standard_stream_takes_set_reuseaddr_like_a_file() {
    let host = OsHost::with_args(vec![]);

    for handle in [Handle::Stdin, Handle::Stdout, Handle::Stderr] {
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

/// A piped child stream is filed non-blocking by `spawn`: before the child writes, a read answers `WouldBlock` instead of blocking the caller; bytes it echoes come back through `read` once `poll` reports them; closing its stdin ends it, its stdout reads `Eof`, and `wait` reaps it. These streams are what let a fiber's drain yield instead of stalling the scheduler.
#[test]
fn a_piped_child_stream_is_filed_non_blocking() {
    let host = OsHost::with_args(vec![]);
    let (status, child) = host.spawn(
        &[b"/bin/cat".to_vec()],
        b"",
        &[],
        curios_abi::stdio_mode::PIPE,
        curios_abi::stdio_mode::PIPE,
        curios_abi::stdio_mode::NULL,
    );
    assert!(matches!(status, Status::Ok));
    let (status, stdin) = host.stream(child.clone(), 0);
    assert!(matches!(status, Status::Ok));
    let (status, stdout) = host.stream(child.clone(), 1);
    assert!(matches!(status, Status::Ok));

    assert!(matches!(
        host.read(stdout.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    assert!(matches!(host.write(stdin.clone(), b"abc"), (Status::Ok, 3)));
    let ready = host.poll(
        std::slice::from_ref(&stdout),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(
        host.read(stdout.clone(), 8),
        (Status::Ok, bytes) if bytes == b"abc"
    ));

    // Another test's child may have been forked while this write end was open and hold it until its `exec`, so the end of the stream may arrive a few reads late as `WouldBlock`.
    host.close(stdin);
    let mut outcome = host.read(stdout.clone(), 8);
    for _ in 0..100 {
        if !matches!(outcome, (Status::WouldBlock, _)) {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
        outcome = host.read(stdout.clone(), 8);
    }
    assert!(matches!(outcome, (Status::Eof, bytes) if bytes.is_empty()));

    let ready = host.poll(
        std::slice::from_ref(&child),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(host.wait(child), (Status::Ok, 0, 0)));

    host.close(stdout);
}

/// The canonical `ip:port` blob `resolve` mints, for a loopback port nothing listens on: bound once at port zero to learn a free one, then released.
fn free_loopback_address() -> Vec<u8> {
    let probe = std::net::TcpListener::bind("127.0.0.1:0").expect("a loopback port");
    let port = probe.local_addr().expect("a bound address").port();
    drop(probe);

    format!("127.0.0.1:{port}").into_bytes()
}

/// A listener never blocks: `accept` with nothing pending answers `WouldBlock`. A connect to it answers at once on loopback or goes pending and settles through `poll` and `finish_connect`, which is idempotent on a settled socket. Both ends are non-blocking, so a read before any write answers `WouldBlock` and one after `poll` serves the bytes.
#[test]
fn a_loopback_connect_settles_and_both_ends_would_block_before_data() {
    let host = OsHost::with_args(vec![]);
    let blob = free_loopback_address();

    let (status, listener) = host.socket(&blob);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.set_reuseaddr(listener.clone(), 1),
        Status::Ok
    ));
    assert!(matches!(host.bind(listener.clone(), &blob), Status::Ok));
    assert!(matches!(host.listen(listener.clone(), 1), Status::Ok));
    assert!(matches!(
        host.accept(listener.clone()),
        (Status::WouldBlock, _)
    ));

    let (status, client) = host.socket(&blob);
    assert!(matches!(status, Status::Ok));
    match host.connect(client.clone(), &blob) {
        Status::Ok => {}
        Status::WouldBlock => {
            let ready = host.poll(
                std::slice::from_ref(&client),
                &[Poll::from_bits(curios_abi::event::WRITE)],
                5_000,
            );
            assert_ne!(ready[0].bits() & curios_abi::event::WRITE, 0);
            assert!(matches!(host.finish_connect(client.clone()), Status::Ok));
        }
        other => panic!("connect answered status code {}", other.code()),
    }
    assert!(matches!(host.finish_connect(client.clone()), Status::Ok));

    let ready = host.poll(
        std::slice::from_ref(&listener),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    let (status, server) = host.accept(listener.clone());
    assert!(matches!(status, Status::Ok));

    assert!(matches!(
        host.read(server.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    assert!(matches!(
        host.write(client.clone(), b"ping"),
        (Status::Ok, 4)
    ));
    let ready = host.poll(
        std::slice::from_ref(&server),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(
        host.read(server.clone(), 8),
        (Status::Ok, bytes) if bytes == b"ping"
    ));

    host.close(server);
    host.close(client);
    host.close(listener);
}

/// A connected loopback pair: a listener, a client settled through `poll` and `finish_connect` where the kernel made it pend, and the accepted server end.
fn loopback_pair(host: &OsHost) -> (Handle, Handle, Handle) {
    let blob = free_loopback_address();
    let (status, listener) = host.socket(&blob);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.set_reuseaddr(listener.clone(), 1),
        Status::Ok
    ));
    assert!(matches!(host.bind(listener.clone(), &blob), Status::Ok));
    assert!(matches!(host.listen(listener.clone(), 1), Status::Ok));

    let (status, client) = host.socket(&blob);
    assert!(matches!(status, Status::Ok));
    if matches!(host.connect(client.clone(), &blob), Status::WouldBlock) {
        host.poll(
            std::slice::from_ref(&client),
            &[Poll::from_bits(curios_abi::event::WRITE)],
            5_000,
        );
        assert!(matches!(host.finish_connect(client.clone()), Status::Ok));
    }
    host.poll(
        std::slice::from_ref(&listener),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    let (status, server) = host.accept(listener.clone());
    assert!(matches!(status, Status::Ok));

    (listener, client, server)
}

/// A TLS upgrade files the stream and touches the socket not at all: the client hello leaves only when the guest first writes, which answers `WouldBlock` while the reply is awaited, and the bytes the server side then reads are a TLS handshake record — the handshake is driven by the guest's own reads and writes and parks like any other progress. A plaintext reply is not TLS, so the client's next read reports `TlsError`, with the handle still filed for its finalizer to close.
#[test]
fn a_tls_upgrade_is_driven_by_the_reads_and_writes_that_follow() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert!(matches!(
        host.start_tls(client.clone(), b"localhost"),
        Status::Ok
    ));
    assert!(matches!(
        host.read(server.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    assert!(matches!(
        host.write(client.clone(), b"x"),
        (Status::WouldBlock, 0)
    ));

    let ready = host.poll(
        std::slice::from_ref(&server),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    let (status, hello) = host.read(server.clone(), 4096);
    assert!(matches!(status, Status::Ok));
    assert_eq!(&hello[..2], &[0x16, 0x03], "a TLS handshake record");

    assert!(matches!(
        host.write(server.clone(), b"HTTP/1.0 400 Bad Request\r\n\r\n"),
        (Status::Ok, _)
    ));
    let ready = host.poll(
        std::slice::from_ref(&client),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(
        host.read(client.clone(), 8),
        (Status::TlsError, _)
    ));
    assert!(!matches!(
        host.read(client.clone(), 8),
        (Status::NotFound, _)
    ));

    host.close(server);
    host.close(client);
    host.close(listener);
}

/// A connect nobody listens for is refused — at once, or through `finish_connect` once the pending connect has settled — and the socket is gone either way.
#[test]
fn a_refused_connect_reports_and_drops_the_socket() {
    let host = OsHost::with_args(vec![]);
    let blob = free_loopback_address();

    let (status, client) = host.socket(&blob);
    assert!(matches!(status, Status::Ok));
    let outcome = match host.connect(client.clone(), &blob) {
        Status::WouldBlock => {
            let ready = host.poll(
                std::slice::from_ref(&client),
                &[Poll::from_bits(curios_abi::event::WRITE)],
                5_000,
            );
            assert_ne!(ready[0].bits() & curios_abi::event::WRITE, 0);
            host.finish_connect(client.clone())
        }
        other => other,
    };
    assert!(matches!(outcome, Status::ConnectionRefused));
    assert!(matches!(host.read(client, 8), (Status::NotFound, _)));
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
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(host.wait(child), (Status::Ok, 0, 0)));
    assert!(matches!(
        host.read(stdout.clone(), 64),
        (Status::Ok, bytes) if bytes == b"hi\n"
    ));

    host.close(stdout);
}
