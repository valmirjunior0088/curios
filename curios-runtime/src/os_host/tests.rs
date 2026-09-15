use {
    super::*,
    curios_abi::{serial_flow, serial_parity},
    curios_utilities::test_support::Temporary,
    rustix::{
        pty::{OpenptFlags, grantpt, openpt, ptsname, unlockpt},
        termios::LocalModes,
    },
};

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
        assert!(matches!(host.socket_set_reuseaddr(handle, 1), Status::Ok));
    }
}

/// A descriptor that is not a terminal refuses both tty rows through the errno lane, which is how a program learns it has none. Which errno is the platform's own — Linux answers `ENOTTY`, macOS `ENODEV` for both rows — so what is under test is the lane rather than the code. `/dev/null` rather than a standard stream, because under an interactive `cargo test` stdin *is* a terminal and a passing `tty_raw` would leave it in raw mode.
#[test]
fn the_tty_rows_on_a_descriptor_that_is_not_a_terminal_refuse_through_the_errno_lane() {
    #[cfg(target_os = "linux")]
    const NOT_A_TERMINAL: u32 = 25;
    #[cfg(not(target_os = "linux"))]
    const NOT_A_TERMINAL: u32 = 19;

    let host = OsHost::with_args(vec![]);
    let (status, handle) = host.file_open(b"/dev/null", Mode::Read);

    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.tty_raw(handle.clone(), 1),
        Status::Other(NOT_A_TERMINAL)
    ));
    assert!(matches!(
        host.tty_size(handle.clone()),
        (Status::Other(NOT_A_TERMINAL), 0, 0)
    ));
    assert!(matches!(host.tty_raw(handle.clone(), 0), Status::Ok));

    host.handle_close(handle);
}

/// A piped child stream is filed non-blocking by `proc_spawn`: before the child writes, a read answers `WouldBlock` instead of blocking the caller; bytes it echoes come back through `handle_read` once `handle_poll` reports them; closing its stdin ends it, its stdout reads `Eof`, and `proc_wait` reaps it. These streams are what let a fiber's drain yield instead of stalling the scheduler.
#[test]
fn a_piped_child_stream_is_filed_non_blocking() {
    let host = OsHost::with_args(vec![]);
    let (status, child) = host.proc_spawn(
        &[b"/bin/cat".to_vec()],
        b"",
        &[],
        curios_abi::stdio_mode::PIPE,
        curios_abi::stdio_mode::PIPE,
        curios_abi::stdio_mode::NULL,
    );
    assert!(matches!(status, Status::Ok));
    let (status, stdin) = host.proc_stream(child.clone(), 0);
    assert!(matches!(status, Status::Ok));
    let (status, stdout) = host.proc_stream(child.clone(), 1);
    assert!(matches!(status, Status::Ok));

    assert!(matches!(
        host.handle_read(stdout.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    assert!(matches!(
        host.handle_write(stdin.clone(), b"abc"),
        (Status::Ok, 3)
    ));
    let ready = host.handle_poll(
        std::slice::from_ref(&stdout),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(
        host.handle_read(stdout.clone(), 8),
        (Status::Ok, bytes) if bytes == b"abc"
    ));

    // Another test's child may have been forked while this write end was open and hold it until its `exec`, so the end of the stream may arrive a few reads late as `WouldBlock`.
    host.handle_close(stdin);
    let mut outcome = host.handle_read(stdout.clone(), 8);
    for _ in 0..100 {
        if !matches!(outcome, (Status::WouldBlock, _)) {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(10));
        outcome = host.handle_read(stdout.clone(), 8);
    }
    assert!(matches!(outcome, (Status::Eof, bytes) if bytes.is_empty()));

    let ready = host.handle_poll(
        std::slice::from_ref(&child),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(host.proc_wait(child), (Status::Ok, 0, 0)));

    host.handle_close(stdout);
}

/// The stdin gate, exercised on a pipe of its own: an empty pipe is not readable, a written byte makes it readable, and so does the writer's close, since a read then answers the end of the stream at once.
#[test]
fn readable_now_follows_a_pipe_end_as_its_writer_fills_and_closes_it() {
    let (mut reader, mut writer) = std::io::pipe().expect("a pipe");

    assert!(!readable_now(reader.as_fd()));
    writer.write_all(b"x").expect("the pipe takes a byte");
    assert!(readable_now(reader.as_fd()));

    let mut byte = [0u8; 1];
    reader.read_exact(&mut byte).expect("the byte is there");
    assert!(!readable_now(reader.as_fd()));

    drop(writer);
    assert!(readable_now(reader.as_fd()));
}

/// A listener bound and listening through the host at a loopback port the kernel picked, with the canonical `ip:port` blob `dns_resolve` would mint for it.
///
/// Bound at port zero and read back, never probed and released: a released port is free only until the next probe takes it, and the two tests that need one run in parallel, so the earlier probe-and-release helper had the second test bind the port the first was about to — measured 2026-09-03 at three failures in fifteen runs of this module.
fn loopback_listener(host: &OsHost) -> (Handle, Vec<u8>) {
    let any = b"127.0.0.1:0";
    let (status, listener) = host.socket_open(any);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.socket_set_reuseaddr(listener.clone(), 1),
        Status::Ok
    ));
    assert!(matches!(
        host.socket_bind(listener.clone(), any),
        Status::Ok
    ));
    assert!(matches!(
        host.socket_listen(listener.clone(), 1),
        Status::Ok
    ));

    let port = match host.table.lock().unwrap().get(&listener) {
        Some(OsResource::Listener(socket)) => socket
            .local_addr()
            .expect("a bound address")
            .as_socket()
            .expect("an IP address")
            .port(),
        _ => panic!("the listener is filed as one"),
    };

    (listener, format!("127.0.0.1:{port}").into_bytes())
}

/// A listener never blocks: `socket_accept` with nothing pending answers `WouldBlock`. A connect to it answers at once on loopback or goes pending and settles through `handle_poll` and `socket_finish_connect`, which is idempotent on a settled socket. Both ends are non-blocking, so a read before any write answers `WouldBlock` and one after `handle_poll` serves the bytes.
#[test]
fn a_loopback_connect_settles_and_both_ends_would_block_before_data() {
    let host = OsHost::with_args(vec![]);
    let (listener, blob) = loopback_listener(&host);
    assert!(matches!(
        host.socket_accept(listener.clone()),
        (Status::WouldBlock, _)
    ));

    let (status, client) = host.socket_open(&blob);
    assert!(matches!(status, Status::Ok));
    match host.socket_connect(client.clone(), &blob) {
        Status::Ok => {}
        Status::WouldBlock => {
            let ready = host.handle_poll(
                std::slice::from_ref(&client),
                &[Poll::from_bits(curios_abi::event::WRITE)],
                5_000,
            );
            assert_ne!(ready[0].bits() & curios_abi::event::WRITE, 0);
            assert!(matches!(
                host.socket_finish_connect(client.clone()),
                Status::Ok
            ));
        }
        other => panic!("connect answered status code {}", other.code()),
    }
    assert!(matches!(
        host.socket_finish_connect(client.clone()),
        Status::Ok
    ));

    let ready = host.handle_poll(
        std::slice::from_ref(&listener),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    let (status, server) = host.socket_accept(listener.clone());
    assert!(matches!(status, Status::Ok));

    assert!(matches!(
        host.handle_read(server.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    assert!(matches!(
        host.handle_write(client.clone(), b"ping"),
        (Status::Ok, 4)
    ));
    let ready = host.handle_poll(
        std::slice::from_ref(&server),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(
        host.handle_read(server.clone(), 8),
        (Status::Ok, bytes) if bytes == b"ping"
    ));

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// A connected loopback pair: a listener, a client settled through `handle_poll` and `socket_finish_connect` where the kernel made it pend, and the accepted server end.
fn loopback_pair(host: &OsHost) -> (Handle, Handle, Handle) {
    let (listener, blob) = loopback_listener(host);

    let (status, client) = host.socket_open(&blob);
    assert!(matches!(status, Status::Ok));
    if matches!(
        host.socket_connect(client.clone(), &blob),
        Status::WouldBlock
    ) {
        host.handle_poll(
            std::slice::from_ref(&client),
            &[Poll::from_bits(curios_abi::event::WRITE)],
            5_000,
        );
        assert!(matches!(
            host.socket_finish_connect(client.clone()),
            Status::Ok
        ));
    }
    host.handle_poll(
        std::slice::from_ref(&listener),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    let (status, server) = host.socket_accept(listener.clone());
    assert!(matches!(status, Status::Ok));

    (listener, client, server)
}

/// A TLS upgrade files the stream and touches the socket not at all: the client hello leaves only when the guest first writes, which answers `WouldBlock` while the reply is awaited, and the bytes the server side then reads are a TLS handshake record — the handshake is driven by the guest's own reads and writes and parks like any other progress. A plaintext reply is not TLS, so the client's next read reports `TlsError`, with the handle still filed for its finalizer to close.
#[test]
fn a_tls_upgrade_is_driven_by_the_reads_and_writes_that_follow() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert!(matches!(
        host.tls_start(client.clone(), b"localhost"),
        Status::Ok
    ));
    assert!(matches!(
        host.handle_read(server.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    assert!(matches!(
        host.handle_write(client.clone(), b"x"),
        (Status::WouldBlock, 0)
    ));

    let ready = host.handle_poll(
        std::slice::from_ref(&server),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    let (status, hello) = host.handle_read(server.clone(), 4096);
    assert!(matches!(status, Status::Ok));
    assert_eq!(&hello[..2], &[0x16, 0x03], "a TLS handshake record");

    assert!(matches!(
        host.handle_write(server.clone(), b"HTTP/1.0 400 Bad Request\r\n\r\n"),
        (Status::Ok, _)
    ));
    let ready = host.handle_poll(
        std::slice::from_ref(&client),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(
        host.handle_read(client.clone(), 8),
        (Status::TlsError, _)
    ));
    assert!(!matches!(
        host.handle_read(client.clone(), 8),
        (Status::NotFound, _)
    ));

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// A connect nobody listens for is refused — at once, or through `socket_finish_connect` once the pending connect has settled — and the socket is gone either way.
#[test]
fn a_refused_connect_reports_and_drops_the_socket() {
    let host = OsHost::with_args(vec![]);

    // A port the kernel just handed out and nothing holds: the connect meets an RST wherever the platform sends one. A bound socket that never listens is not the same staging — Linux refuses a connect to one, macOS leaves the SYN unanswered — so the port is taken to learn its number and released before the connect.
    let holder = Socket::new(Domain::IPV4, Type::STREAM, None).expect("a socket");
    holder
        .bind(&SockAddr::from(
            "127.0.0.1:0".parse::<SocketAddr>().expect("an address"),
        ))
        .expect("a loopback port");
    let port = holder
        .local_addr()
        .expect("a bound address")
        .as_socket()
        .expect("an IP address")
        .port();
    drop(holder);
    let blob = format!("127.0.0.1:{port}").into_bytes();

    let (status, client) = host.socket_open(&blob);
    assert!(matches!(status, Status::Ok));
    let outcome = match host.socket_connect(client.clone(), &blob) {
        Status::WouldBlock => {
            let ready = host.handle_poll(
                std::slice::from_ref(&client),
                &[Poll::from_bits(curios_abi::event::WRITE)],
                5_000,
            );
            // A settled connect is reported as the platform reports it: Linux answers a refused one `WRITE`, macOS `HUP`, and `ERR` rides either. `/std`'s scheduler resumes a park on any of the three for the same reason — a handle in one of those states will never become ready.
            let settled =
                curios_abi::event::WRITE | curios_abi::event::ERR | curios_abi::event::HUP;
            assert_ne!(
                ready[0].bits() & settled,
                0,
                "the poll reported {:#06b}",
                ready[0].bits()
            );
            host.socket_finish_connect(client.clone())
        }
        other => other,
    };
    assert!(matches!(outcome, Status::ConnectionRefused));
    assert!(matches!(host.handle_read(client, 8), (Status::NotFound, _)));
}

/// A real child end to end: `echo` is spawned with its output piped, its handle becomes readable once the reaper has recorded the exit, `proc_wait` reports a clean zero, and the piped output is what it wrote. The unpiped stdin comes back as the empty handle.
#[test]
fn a_child_is_reaped_through_its_handle_and_its_piped_output_read() {
    let host = OsHost::with_args(vec![]);
    let (status, child) = host.proc_spawn(
        &[b"/bin/echo".to_vec(), b"hi".to_vec()],
        b"",
        &[],
        curios_abi::stdio_mode::INHERIT,
        curios_abi::stdio_mode::PIPE,
        curios_abi::stdio_mode::NULL,
    );

    assert!(matches!(status, Status::Ok));
    let (status, stdin) = host.proc_stream(child.clone(), 0);
    assert!(matches!(status, Status::Ok));
    assert!(stdin.is_none());
    let (status, stdout) = host.proc_stream(child.clone(), 1);
    assert!(matches!(status, Status::Ok));

    let ready = host.handle_poll(
        std::slice::from_ref(&child),
        &[Poll::from_bits(curios_abi::event::READ)],
        5_000,
    );
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert!(matches!(host.proc_wait(child), (Status::Ok, 0, 0)));
    assert!(matches!(
        host.handle_read(stdout.clone(), 64),
        (Status::Ok, bytes) if bytes == b"hi\n"
    ));

    host.handle_close(stdout);
}

/// A path is the bytes the host holds, never decoded on the way: the name `dir_list` hands back opens as the file it names, exactly as `file_stat` finds it. The file is made with the standard library on the raw bytes, so the row under test is the only one asked to read them back. The name is the hardest one the filesystem takes — bytes that are not UTF-8 at all where they are allowed, and a non-ASCII name where they are not, since APFS refuses an undecodable one with `EILSEQ`.
#[test]
fn open_takes_a_listed_name_back_as_the_bytes_it_was_given() {
    #[cfg(target_os = "linux")]
    const NAME: &[u8] = b"\xffname.txt";
    #[cfg(not(target_os = "linux"))]
    const NAME: &[u8] = "café.txt".as_bytes();

    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-open", "listed-name");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let name = NAME;
    fs::write(dir.join(OsStr::from_bytes(name)), b"x").expect("a file under the raw name");

    let (status, names) = host.dir_list(dir.as_os_str().as_bytes());
    assert!(matches!(status, Status::Ok));
    assert_eq!(names, vec![name.to_vec()]);

    let path = dir.join(OsStr::from_bytes(&names[0]));
    let (status, handle) = host.file_open(path.as_os_str().as_bytes(), Mode::Read);
    assert!(
        matches!(status, Status::Ok),
        "open answered {}",
        status.code()
    );
    assert!(matches!(host.handle_read(handle.clone(), 8), (Status::Ok, bytes) if bytes == b"x"));

    host.handle_close(handle);
}

/// A serial port opened on the far end of a pseudo-terminal: a frame outside the row's ranges opens nothing, the open takes the raw frame and the exclusive hold, a byte the near end writes arrives through `handle_read` once `handle_poll` has reported it, the input discard is served, and a closed port misses loudly. A pty forces its own character size and keeps no modem lines, so what is under test is the open's shape rather than a wire: Linux refuses the lines through the errno lane, and the speed is never exercised. The hold is asserted only for a process without `CAP_SYS_ADMIN`, which the kernel lets past it, and a reopen after the close is not asserted at all: the near end keeps the far end's tty alive, so on Linux the hold outlives the port's close, where a real device's last close frees its tty and the hold with it.
#[test]
fn a_serial_port_opens_raw_and_exclusive_on_a_pseudo_terminal() {
    const EBUSY: u32 = 16;
    const EINVAL: u32 = 22;

    let near = openpt(OpenptFlags::RDWR | OpenptFlags::NOCTTY).expect("a pseudo-terminal");
    grantpt(&near).expect("its far end granted");
    unlockpt(&near).expect("its far end unlocked");
    let name = ptsname(&near, Vec::new()).expect("its far end's name");

    let host = OsHost::with_args(vec![]);
    let open = |data_bits| {
        host.serial_open(
            name.as_bytes(),
            115_200,
            data_bits,
            serial_parity::NONE,
            1,
            serial_flow::NONE,
        )
    };

    assert!(matches!(open(5), (Status::Other(EINVAL), _)));

    let (status, port) = open(8);
    assert!(matches!(status, Status::Ok));

    let termios = host
        .with_fd(&port, |fd| tcgetattr(fd))
        .expect("a filed descriptor")
        .expect("its settings");
    assert!(
        termios
            .control_modes
            .contains(ControlModes::CS8 | ControlModes::CLOCAL | ControlModes::CREAD)
    );
    assert!(
        !termios
            .local_modes
            .intersects(LocalModes::ICANON | LocalModes::ECHO)
    );

    if !rustix::process::geteuid().is_root() {
        assert!(matches!(open(8), (Status::Other(EBUSY), _)));
    }

    assert!(matches!(
        host.handle_read(port.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    rustix::io::write(&near, b"ok").expect("the near end writes");
    let ready = host.handle_poll(
        std::slice::from_ref(&port),
        &[Poll::from_bits(event::READ)],
        5_000,
    );
    assert!(ready[0].bits() & event::READ != 0);
    assert!(matches!(
        host.handle_read(port.clone(), 8),
        (Status::Ok, bytes) if bytes == b"ok"
    ));

    assert!(matches!(
        host.serial_control(port.clone(), serial_op::DISCARD_INPUT, 0),
        Status::Ok
    ));
    #[cfg(target_os = "linux")]
    assert!(matches!(
        host.serial_control(port.clone(), serial_op::DTR, 1),
        Status::Other(25)
    ));

    host.handle_close(port.clone());
    assert!(matches!(
        host.serial_control(port, serial_op::DISCARD_INPUT, 0),
        Status::NotFound
    ));
}
