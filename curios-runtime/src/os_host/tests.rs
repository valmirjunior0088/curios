use {
    super::*,
    curios_abi::errno,
    curios_utilities::test_support::Temporary,
    rustix::{
        pty::{OpenptFlags, grantpt, openpt, ptsname, unlockpt},
        termios::LocalModes,
    },
    std::{num::NonZeroU32, thread, time::Duration},
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
        assert_eq!(host.socket_set_reuseaddr(handle, true), Ok(()));
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
    let handle = host.file_open(b"/dev/null".to_vec(), Mode::Read).unwrap();

    assert_eq!(
        host.tty_raw(handle.clone(), true),
        Err(Failure::Other(NOT_A_TERMINAL))
    );
    assert_eq!(
        host.tty_size(handle.clone()),
        Err(Failure::Other(NOT_A_TERMINAL))
    );
    assert_eq!(host.tty_raw(handle.clone(), false), Ok(()));

    host.handle_close(handle);
}

/// A piped child stream is filed non-blocking by `proc_spawn`: before the child writes, a read answers `WouldBlock` instead of blocking the caller; bytes it echoes come back through `handle_read` once `handle_poll` reports them; closing its stdin ends it, its stdout reads to its end, and `proc_wait` reaps it. These streams are what let a fiber's drain yield instead of stalling the scheduler.
#[test]
fn a_piped_child_stream_is_filed_non_blocking() {
    let host = OsHost::with_args(vec![]);
    let child = host
        .proc_spawn(
            vec![b"/bin/cat".to_vec()],
            vec![],
            vec![],
            StdioMode::Pipe,
            StdioMode::Pipe,
            StdioMode::Null,
        )
        .unwrap();
    let stdin = host.proc_stream(child.clone(), ChildStream::Stdin).unwrap();
    let stdout = host
        .proc_stream(child.clone(), ChildStream::Stdout)
        .unwrap();

    assert_eq!(
        host.handle_read(stdout.clone(), 8),
        Err(Failure::WouldBlock)
    );
    assert_eq!(host.handle_write(stdin.clone(), b"abc".to_vec()), Ok(3));
    let ready = host
        .handle_poll(
            vec![stdout.clone()],
            vec![Poll::from_bits(curios_abi::event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert_eq!(
        host.handle_read(stdout.clone(), 8),
        Ok(Some(b"abc".to_vec()))
    );

    // Another test's child may have been forked while this write end was open and hold it until its `exec`, so the end of the stream may arrive a few reads late as `WouldBlock`.
    host.handle_close(stdin);
    let mut outcome = host.handle_read(stdout.clone(), 8);
    for _ in 0..100 {
        if outcome != Err(Failure::WouldBlock) {
            break;
        }
        thread::sleep(Duration::from_millis(10));
        outcome = host.handle_read(stdout.clone(), 8);
    }
    assert_eq!(outcome, Ok(None));

    let ready = host
        .handle_poll(
            vec![child.clone()],
            vec![Poll::from_bits(curios_abi::event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));

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

    // A child another test spawns while this pipe's close-on-exec flag is being set — macOS has no `pipe2`, so `std` sets it after the pipe exists — holds the write end until it exits or is killed, so the end of the stream may arrive late rather than at once.
    drop(writer);
    let closed = (0..100).any(|_| {
        let readable = readable_now(reader.as_fd());

        if !readable {
            thread::sleep(Duration::from_millis(10));
        }

        readable
    });
    assert!(closed);
}

/// A listener bound and listening through the host at a loopback port the kernel picked, with the canonical `ip:port` blob `dns_resolve` would mint for it.
///
/// Bound at port zero and read back, never probed and released: a released port is free only until the next probe takes it, and the two tests that need one run in parallel, so the earlier probe-and-release helper had the second test bind the port the first was about to — measured 2026-09-03 at three failures in fifteen runs of this module.
fn loopback_listener(host: &OsHost) -> (Handle, Vec<u8>) {
    let any = b"127.0.0.1:0".to_vec();
    let listener = host.socket_open(any.clone()).unwrap();
    assert_eq!(host.socket_set_reuseaddr(listener.clone(), true), Ok(()));
    assert_eq!(host.socket_bind(listener.clone(), any), Ok(()));
    assert_eq!(host.socket_listen(listener.clone(), 1), Ok(()));

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
        Err(Failure::WouldBlock)
    ));

    let client = host.socket_open(blob.clone()).unwrap();
    match host.socket_connect(client.clone(), blob) {
        Ok(()) => {}
        Err(Failure::WouldBlock) => {
            let ready = host
                .handle_poll(
                    vec![client.clone()],
                    vec![Poll::from_bits(curios_abi::event::WRITE)],
                    5_000,
                )
                .unwrap();
            assert_ne!(ready[0].bits() & curios_abi::event::WRITE, 0);
            assert_eq!(host.socket_finish_connect(client.clone()), Ok(()));
        }
        Err(failure) => panic!("connect answered {failure:?}"),
    }
    assert_eq!(host.socket_finish_connect(client.clone()), Ok(()));

    let ready = host
        .handle_poll(
            vec![listener.clone()],
            vec![Poll::from_bits(curios_abi::event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    let server = host.socket_accept(listener.clone()).unwrap();

    assert_eq!(
        host.handle_read(server.clone(), 8),
        Err(Failure::WouldBlock)
    );
    assert_eq!(host.handle_write(client.clone(), b"ping".to_vec()), Ok(4));
    let ready = host
        .handle_poll(
            vec![server.clone()],
            vec![Poll::from_bits(curios_abi::event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert_eq!(
        host.handle_read(server.clone(), 8),
        Ok(Some(b"ping".to_vec()))
    );

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// A connected loopback pair: a listener, a client settled through `handle_poll` and `socket_finish_connect` where the kernel made it pend, and the accepted server end.
fn loopback_pair(host: &OsHost) -> (Handle, Handle, Handle) {
    let (listener, blob) = loopback_listener(host);

    let client = host.socket_open(blob.clone()).unwrap();
    if host.socket_connect(client.clone(), blob) == Err(Failure::WouldBlock) {
        host.handle_poll(
            vec![client.clone()],
            vec![Poll::from_bits(curios_abi::event::WRITE)],
            5_000,
        )
        .unwrap();
        assert_eq!(host.socket_finish_connect(client.clone()), Ok(()));
    }
    host.handle_poll(
        vec![listener.clone()],
        vec![Poll::from_bits(curios_abi::event::READ)],
        5_000,
    )
    .unwrap();
    let server = host.socket_accept(listener.clone()).unwrap();

    (listener, client, server)
}

/// A TLS upgrade files the stream and touches the socket not at all: the client hello leaves only when the guest first writes, which answers `WouldBlock` while the reply is awaited, and the bytes the server side then reads are a TLS handshake record — the handshake is driven by the guest's own reads and writes and parks like any other progress. A plaintext reply is not TLS, so the client's next read reports `TlsError`, with the handle still filed for its finalizer to close.
#[test]
fn a_tls_upgrade_is_driven_by_the_reads_and_writes_that_follow() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(
        host.tls_start(client.clone(), b"localhost".to_vec()),
        Ok(())
    );
    assert_eq!(
        host.handle_read(server.clone(), 8),
        Err(Failure::WouldBlock)
    );
    assert_eq!(
        host.handle_write(client.clone(), b"x".to_vec()),
        Err(Failure::WouldBlock)
    );

    let ready = host
        .handle_poll(
            vec![server.clone()],
            vec![Poll::from_bits(curios_abi::event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    let hello = host
        .handle_read(server.clone(), 4096)
        .unwrap()
        .expect("the client hello");
    assert_eq!(&hello[..2], &[0x16, 0x03], "a TLS handshake record");

    assert!(
        host.handle_write(server.clone(), b"HTTP/1.0 400 Bad Request\r\n\r\n".to_vec())
            .is_ok()
    );
    let ready = host
        .handle_poll(
            vec![client.clone()],
            vec![Poll::from_bits(curios_abi::event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert_eq!(host.handle_read(client.clone(), 8), Err(Failure::TlsError));
    assert_ne!(host.handle_read(client.clone(), 8), Err(Failure::NotFound));

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

    let client = host.socket_open(blob.clone()).unwrap();
    let outcome = match host.socket_connect(client.clone(), blob) {
        Err(Failure::WouldBlock) => {
            let ready = host
                .handle_poll(
                    vec![client.clone()],
                    vec![Poll::from_bits(curios_abi::event::WRITE)],
                    5_000,
                )
                .unwrap();
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
    assert_eq!(outcome, Err(Failure::ConnectionRefused));
    assert_eq!(host.handle_read(client, 8), Err(Failure::NotFound));
}

/// A real child end to end: `echo` is spawned with its output piped, its handle becomes readable once the reaper has recorded the exit, `proc_wait` reports a clean zero, and the piped output is what it wrote. The unpiped stdin has no handle, which is `not_found`.
#[test]
fn a_child_is_reaped_through_its_handle_and_its_piped_output_read() {
    let host = OsHost::with_args(vec![]);
    let child = host
        .proc_spawn(
            vec![b"/bin/echo".to_vec(), b"hi".to_vec()],
            vec![],
            vec![],
            StdioMode::Inherit,
            StdioMode::Pipe,
            StdioMode::Null,
        )
        .unwrap();

    assert_eq!(
        host.proc_stream(child.clone(), ChildStream::Stdin),
        Err(Failure::NotFound)
    );
    let stdout = host
        .proc_stream(child.clone(), ChildStream::Stdout)
        .unwrap();

    let ready = host
        .handle_poll(
            vec![child.clone()],
            vec![Poll::from_bits(curios_abi::event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & curios_abi::event::READ, 0);
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));
    assert_eq!(
        host.handle_read(stdout.clone(), 64),
        Ok(Some(b"hi\n".to_vec()))
    );

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

    let names = host.dir_list(dir.as_os_str().as_bytes().to_vec()).unwrap();
    assert_eq!(names, vec![name.to_vec()]);

    let path = dir.join(OsStr::from_bytes(&names[0]));
    let handle = host
        .file_open(path.into_os_string().into_vec(), Mode::Read)
        .unwrap();
    assert_eq!(host.handle_read(handle.clone(), 8), Ok(Some(b"x".to_vec())));

    host.handle_close(handle);
}

/// A serial port opened on the far end of a pseudo-terminal: a frame outside the row's ranges opens nothing, the open takes the raw frame, a byte the near end writes arrives through `handle_read` once `handle_poll` has reported it, the input discard is served, and a closed port misses loudly. A pty forces its own character size and keeps no modem lines, so what is under test is the open's shape rather than a wire: Linux refuses the lines through the errno lane, and the speed is never exercised.
#[test]
fn a_serial_port_opens_raw_on_a_pseudo_terminal() {
    const EINVAL: u32 = 22;

    let near = openpt(OpenptFlags::RDWR | OpenptFlags::NOCTTY).expect("a pseudo-terminal");
    grantpt(&near).expect("its far end granted");
    unlockpt(&near).expect("its far end unlocked");
    let name = ptsname(&near, Vec::new()).expect("its far end's name");

    let host = OsHost::with_args(vec![]);
    let open = |data_bits| {
        host.serial_open(
            name.as_bytes().to_vec(),
            115_200,
            data_bits,
            SerialParity::None,
            1,
            SerialFlow::None,
        )
    };

    assert!(matches!(open(5), Err(Failure::Other(EINVAL))));

    let port = open(8).unwrap();

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

    assert_eq!(host.handle_read(port.clone(), 8), Err(Failure::WouldBlock));
    rustix::io::write(&near, b"ok").expect("the near end writes");
    let ready = host
        .handle_poll(
            vec![port.clone()],
            vec![Poll::from_bits(event::READ)],
            5_000,
        )
        .unwrap();
    assert!(ready[0].bits() & event::READ != 0);
    assert_eq!(host.handle_read(port.clone(), 8), Ok(Some(b"ok".to_vec())));

    assert_eq!(
        host.serial_control(port.clone(), SerialOp::DiscardInput, false),
        Ok(())
    );
    #[cfg(target_os = "linux")]
    assert_eq!(
        host.serial_control(port.clone(), SerialOp::Dtr, true),
        Err(Failure::Other(25))
    );

    host.handle_close(port.clone());
    assert_eq!(
        host.serial_control(port, SerialOp::DiscardInput, false),
        Err(Failure::NotFound)
    );
}

/// Spawn `argv` with every stream on the null device.
fn quiet(host: &OsHost, argv: &[&[u8]]) -> Result<Handle, Failure> {
    host.proc_spawn(
        argv.iter().map(|arg| arg.to_vec()).collect(),
        vec![],
        vec![],
        StdioMode::Null,
        StdioMode::Null,
        StdioMode::Null,
    )
}

/// Wait up to five seconds for `child`'s handle to report that its end is recorded.
fn ended(host: &OsHost, child: &Handle) {
    let ready = host
        .handle_poll(
            vec![child.clone()],
            vec![Poll::from_bits(event::READ)],
            5_000,
        )
        .unwrap();

    assert_ne!(ready[0].bits() & event::READ, 0, "the child ended");
}

/// Once the reaper has recorded an exit it has also reaped the pid, which the OS may hand to another process, so a kill answers `ok` without signaling anything — and the exit still reads back. A kill that signaled the freed pid answered `ESRCH` here, or ended whatever process had inherited it.
#[test]
fn killing_a_child_that_has_ended_signals_nothing() {
    let host = OsHost::with_args(vec![]);
    let child = quiet(&host, &[b"/bin/sh", b"-c", b"exit 0"]).unwrap();

    ended(&host, &child);
    assert_eq!(host.proc_kill(child.clone()), Ok(()));
    assert_eq!(host.proc_wait(child), Ok(ChildExit::Code(0)));
}

#[test]
fn killing_a_running_child_ends_it_by_its_signal() {
    let host = OsHost::with_args(vec![]);
    let child = quiet(&host, &[b"/bin/sleep", b"60"]).unwrap();

    assert_eq!(host.proc_wait(child.clone()), Err(Failure::WouldBlock));
    assert_eq!(host.proc_kill(child.clone()), Ok(()));
    ended(&host, &child);
    assert_eq!(
        host.proc_wait(child),
        Ok(ChildExit::Signal(NonZeroU32::new(9).unwrap()))
    );
}

/// An empty `argv` names no program, a NUL fits in no C string, and an environment entry needs a name before its `=`: each is `EINVAL`, as `execve` and `setenv` answer, and none starts anything.
#[test]
fn spawning_refuses_what_no_program_can_be() {
    const EINVAL: u32 = 22;

    let host = OsHost::with_args(vec![]);
    let spawn = |argv: Vec<Vec<u8>>, env: Vec<Vec<u8>>| {
        host.proc_spawn(
            argv,
            vec![],
            env,
            StdioMode::Null,
            StdioMode::Null,
            StdioMode::Null,
        )
    };
    let refused = Err(Failure::Other(EINVAL));

    assert_eq!(spawn(vec![], vec![]), refused);
    assert_eq!(spawn(vec![b"/bin/e\0cho".to_vec()], vec![]), refused);
    assert_eq!(
        spawn(vec![b"/bin/echo".to_vec()], vec![b"=x".to_vec()]),
        refused
    );
    assert_eq!(spawn(vec![b"/bin/echo".to_vec()], vec![vec![]]), refused);
}

/// The standard streams are open one way each, and using one the other way is `EBADF` — the operating system's own, which is the one the hosts without one answer in its place — as it is for any descriptor opened one way: never an invented end of stream, and never a write into a terminal the program only reads.
#[test]
fn a_standard_stream_is_open_one_way() {
    let host = OsHost::with_args(vec![]);

    assert_eq!(
        host.handle_read(Handle::Stdout, 8),
        Err(Failure::Other(errno::EBADF))
    );
    assert_eq!(
        host.handle_read(Handle::Stderr, 0),
        Err(Failure::Other(errno::EBADF))
    );
    assert_eq!(
        host.handle_write(Handle::Stdin, b"x".to_vec()),
        Err(Failure::Other(errno::EBADF))
    );
    assert_eq!(
        host.handle_write(Handle::Stdin, vec![]),
        Err(Failure::Other(errno::EBADF))
    );
}

/// A request for nothing moves nothing: a read of zero bytes answers empty bytes, never the end of a stream it did not look at, and an empty write answers `0` — each only after checking the handle is a stream at all.
#[test]
fn a_request_for_nothing_checks_the_handle_and_moves_nothing() {
    let host = OsHost::with_args(vec![]);
    let null = host.file_open(b"/dev/null".to_vec(), Mode::Read).unwrap();
    let unknown = Handle::Other(vec![0xff, 0xff]);

    assert_eq!(host.handle_read(null.clone(), 0), Ok(Some(vec![])));
    assert_eq!(host.handle_read(null.clone(), 8), Ok(None));
    assert_eq!(host.handle_read(unknown.clone(), 0), Err(Failure::NotFound));
    assert_eq!(host.handle_write(Handle::Stdout, vec![]), Ok(0));
    assert_eq!(host.handle_write(unknown, vec![]), Err(Failure::NotFound));

    host.handle_close(null);
}

/// One read allocates for at most its cap, however much it is asked for, and answers the prefix that fits — which the row allows, since a read may answer fewer bytes than requested.
#[test]
fn a_read_takes_at_most_its_cap() {
    let host = OsHost::with_args(vec![]);
    let dir = Temporary::new("runtime-read", "cap");
    fs::create_dir(&dir).expect("a fresh temporary directory");
    let path = dir.join("large");
    fs::write(&path, vec![b'x'; 100 * 1024]).expect("a file past the cap");

    let file = host
        .file_open(path.into_os_string().into_vec(), Mode::Read)
        .unwrap();
    let read = host.handle_read(file.clone(), u64::MAX).unwrap().unwrap();

    assert_eq!(read.len() as u64, READ_MAX);
    host.handle_close(file);
}

/// A flush pushes what `rustls` holds: after a TLS upgrade and before any write, the client's hello waits inside `rustls`, and flushing is what puts it on the wire. Every other kind holds nothing, and an unknown handle is `NotFound`.
#[test]
fn a_flush_pushes_what_rustls_holds() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(host.handle_flush(Handle::Stdout), Ok(()));
    assert_eq!(
        host.handle_flush(Handle::Other(vec![0xff, 0xff])),
        Err(Failure::NotFound)
    );

    assert_eq!(
        host.tls_start(client.clone(), b"localhost".to_vec()),
        Ok(())
    );
    assert_eq!(
        host.handle_read(server.clone(), 8),
        Err(Failure::WouldBlock)
    );
    assert_eq!(host.handle_flush(client.clone()), Ok(()));

    let ready = host
        .handle_poll(
            vec![server.clone()],
            vec![Poll::from_bits(event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & event::READ, 0);
    let hello = host.handle_read(server.clone(), 4096).unwrap().unwrap();
    assert_eq!(&hello[..2], &[0x16, 0x03], "a TLS handshake record");

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// Two slots naming one descriptor are polled once and answered each in its own terms: the one asking to read sees the readiness to read, the one asking to write the readiness to write, and neither the other's.
#[test]
fn slots_naming_one_descriptor_are_answered_each_for_its_interest() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(host.handle_write(client.clone(), b"x".to_vec()), Ok(1));

    // The byte is waited for alone first: a poll answers once any slot is ready, and the writing one is ready at once, possibly before the byte has crossed the loopback.
    let arrived = host
        .handle_poll(
            vec![server.clone()],
            vec![Poll::from_bits(event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(arrived[0].bits() & event::READ, 0);

    let ready = host
        .handle_poll(
            vec![server.clone(), server.clone()],
            vec![Poll::from_bits(event::READ), Poll::from_bits(event::WRITE)],
            0,
        )
        .unwrap();
    assert_eq!(ready[0].bits() & (event::READ | event::WRITE), event::READ);
    assert_eq!(ready[1].bits() & (event::READ | event::WRITE), event::WRITE);

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// Randomness answers exactly the bytes asked for, and a request no memory can hold refuses the call rather than abort the process.
#[test]
fn randomness_answers_exactly_what_was_asked_or_refuses() {
    let host = OsHost::with_args(vec![]);

    assert_eq!(host.rand_bytes(32).map(|bytes| bytes.len()), Ok(32));
    assert!(host.rand_bytes(u64::MAX).is_err());
}

/// A lookup is refused before it starts when it names nothing a resolver could find — a host that is not UTF-8 — or a port past the sixteen bits a port is.
#[test]
fn a_lookup_of_no_host_or_no_port_is_refused_before_it_starts() {
    const EINVAL: u32 = 22;

    let host = OsHost::with_args(vec![]);

    assert_eq!(
        host.dns_lookup(b"\xffhost".to_vec(), 80),
        Err(Failure::NotFound)
    );
    assert_eq!(
        host.dns_lookup(b"localhost".to_vec(), 65_536),
        Err(Failure::Other(EINVAL))
    );
}

/// A name that is empty or holds `=` or NUL names no variable, so it is absent without the environment being asked how the platform reads such a name.
#[test]
fn a_name_no_variable_can_have_is_absent() {
    let host = OsHost::with_args(vec![]);

    for name in [&b""[..], b"PATH=", b"PA\0TH"] {
        assert_eq!(host.proc_env(name.to_vec()), None);
    }
    assert!(host.proc_env(b"PATH".to_vec()).is_some());
}

/// An upgrade that cannot start leaves the socket as it was: connected, and still serving plaintext.
#[test]
fn a_tls_upgrade_that_cannot_start_leaves_the_socket_connected() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(
        host.tls_start(client.clone(), b"\xff".to_vec()),
        Err(Failure::TlsError)
    );
    assert_eq!(host.handle_write(client.clone(), b"x".to_vec()), Ok(1));

    let ready = host
        .handle_poll(
            vec![server.clone()],
            vec![Poll::from_bits(event::READ)],
            5_000,
        )
        .unwrap();
    assert_ne!(ready[0].bits() & event::READ, 0);
    assert_eq!(host.handle_read(server.clone(), 8), Ok(Some(b"x".to_vec())));

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}
