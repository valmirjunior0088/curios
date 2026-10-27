//! Sockets: a listener's address, a loopback connect, a TLS upgrade driven by the reads and writes that follow, and how each kind of end is reported.

use {
    super::test_support::{awaited, come, loopback_listener, loopback_pair},
    super::*,
    crate::{Failure, Handle},
};

/// Every entry in the handle table is one [`OsResource`], so the enum's size is what a plain file or an unconnected socket costs to hold. The two TLS variants are boxed because a `rustls` connection carries its record buffers inline, and unboxed it would set the size of every other kind.
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

/// A listener's address is reusable, as the row promises, so a restart binds the port its predecessor's connections still linger on.
#[test]
fn a_listener_s_address_is_reusable() {
    let host = OsHost::with_args(vec![]);
    let (listener, _) = loopback_listener(&host);

    match host.table.lock().unwrap().get(&listener) {
        Some(OsResource::Listener(socket)) => {
            assert!(socket.reuse_address().expect("the option read"));
        }
        _ => panic!("the listener is filed as one"),
    }

    host.handle_close(listener);
}

/// A listener that cannot be made is the row's failure, whichever step refused it: an address a listener already holds refuses the bind, and the failure is the system's own.
#[test]
fn a_listener_at_a_held_address_is_refused() {
    let host = OsHost::with_args(vec![]);
    let (listener, blob) = loopback_listener(&host);

    assert_eq!(
        host.socket_listen(blob),
        Err(Failure::Other(Errno::ADDRINUSE.raw_os_error() as u32))
    );

    host.handle_close(listener);
}

/// A listener never blocks: `socket_accept` with nothing pending answers `WouldBlock`. A connect to it answers at once on loopback or goes pending and settles through a watch and `socket_finish_connect`, which is idempotent on a settled socket. Both ends are non-blocking, so a read before any write answers `WouldBlock` and one after a watch is ready serves the bytes.
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
            assert!(awaited(&host, &client, false, true));
            assert_eq!(host.socket_finish_connect(client.clone()), Ok(()));
        }
        Err(failure) => panic!("connect answered {failure:?}"),
    }
    assert_eq!(host.socket_finish_connect(client.clone()), Ok(()));

    assert!(awaited(&host, &listener, true, false));
    let server = host.socket_accept(listener.clone()).unwrap();

    assert_eq!(
        host.handle_read(server.clone(), 8),
        Err(Failure::WouldBlock)
    );
    assert_eq!(host.handle_write(client.clone(), b"ping".to_vec()), Ok(4));
    assert!(awaited(&host, &server, true, false));
    assert_eq!(
        host.handle_read(server.clone(), 8),
        Ok(Some(b"ping".to_vec()))
    );

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
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

    assert!(awaited(&host, &server, true, false));
    let hello = host
        .handle_read(server.clone(), 4096)
        .unwrap()
        .expect("the client hello");
    assert_eq!(&hello[..2], &[0x16, 0x03], "a TLS handshake record");

    assert!(
        host.handle_write(server.clone(), b"HTTP/1.0 400 Bad Request\r\n\r\n".to_vec())
            .is_ok()
    );
    assert!(awaited(&host, &client, true, false));
    assert_eq!(host.handle_read(client.clone(), 8), Err(Failure::TlsError));
    assert_ne!(host.handle_read(client.clone(), 8), Err(Failure::NotFound));

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// A TLS stream whose peer ends its side with no closure alert has been cut, not ended: its read reports `TlsError` where a plain socket's reads the end of the stream, so what arrived is not taken for the whole of it. The peer here ends its sending half before the handshake has begun, which `rustls` reports as it does an end after it.
#[test]
fn a_tls_stream_ended_without_its_alert_reads_as_a_failure() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(
        host.tls_start(client.clone(), b"localhost".to_vec()),
        Ok(())
    );
    assert_eq!(host.handle_finish(server.clone()), Ok(()));

    // Driven as a program drives it: with the client hello still unsent, a watch to read wakes on the socket's writability as well, so a read can come before the peer's end has arrived and answer `WouldBlock`, and the program waits again.
    let read = loop {
        assert!(awaited(&host, &client, true, false));

        match host.handle_read(client.clone(), 8) {
            Err(Failure::WouldBlock) => continue,
            read => break read,
        }
    };
    assert_eq!(read, Err(Failure::TlsError));

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
            // A settled connect is reported as the platform reports it: Linux answers a refused one ready to write, macOS hung up, and the failure rides either. A watch is ready on any of the three for the same reason — a handle in one of those states will never become ready.
            assert!(awaited(&host, &client, false, true));
            host.socket_finish_connect(client.clone())
        }
        other => other,
    };
    assert_eq!(outcome, Err(Failure::ConnectionRefused));
    assert_eq!(host.handle_read(client, 8), Err(Failure::NotFound));
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

    assert!(awaited(&host, &server, true, false));
    let hello = host.handle_read(server.clone(), 4096).unwrap().unwrap();
    assert_eq!(&hello[..2], &[0x16, 0x03], "a TLS handshake record");

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// Everything `handle` sends until its stream ends, each read waited for through a watch.
fn read_to_the_end(host: &OsHost, handle: &Handle) -> Vec<u8> {
    let mut bytes = Vec::new();

    loop {
        assert!(awaited(host, handle, true, false));

        match host.handle_read(handle.clone(), 4096) {
            Ok(Some(chunk)) => bytes.extend(chunk),
            Ok(None) => return bytes,
            Err(Failure::WouldBlock) => {}
            Err(failure) => panic!("the read answered {failure:?}"),
        }
    }
}

/// A finish ends the sending half alone: the peer reads the end of the stream, and what the peer sends afterwards is still read. A kind with no sending half of its own — a standard stream, a listener — and an unknown handle are `NotFound`.
#[test]
fn a_finished_socket_ends_for_its_peer_and_still_reads() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(host.handle_finish(Handle::Stdout), Err(Failure::NotFound));
    assert_eq!(host.handle_finish(listener.clone()), Err(Failure::NotFound));
    assert_eq!(
        host.handle_finish(Handle::Other(vec![0xff, 0xff])),
        Err(Failure::NotFound)
    );

    assert_eq!(host.handle_write(server.clone(), b"done".to_vec()), Ok(4));
    assert_eq!(host.handle_finish(server.clone()), Ok(()));
    assert_eq!(read_to_the_end(&host, &client), b"done");

    assert_eq!(host.handle_write(client.clone(), b"late".to_vec()), Ok(4));
    host.handle_close(client);
    assert_eq!(read_to_the_end(&host, &server), b"late");

    host.handle_close(server);
    host.handle_close(listener);
}

/// A TLS stream's finish sends its closure alert ahead of the end: the peer's last record is the alert — content type 21, two bytes, a warning's `close_notify` — and its stream ends after it. Here the alert follows the client's hello in the clear, since no handshake has completed to encrypt it.
#[test]
fn a_finished_tls_stream_sends_its_closure_alert_before_it_ends() {
    let host = OsHost::with_args(vec![]);
    let (listener, client, server) = loopback_pair(&host);

    assert_eq!(
        host.tls_start(client.clone(), b"localhost".to_vec()),
        Ok(())
    );
    assert_eq!(host.handle_finish(client.clone()), Ok(()));

    let sent = read_to_the_end(&host, &server);
    assert_eq!(&sent[..2], &[0x16, 0x03], "a TLS handshake record");

    let alert = &sent[sent.len() - 7..];
    assert_eq!(alert[0], 0x15, "an alert record");
    assert_eq!(&alert[3..], &[0x00, 0x02, 0x01, 0x00]);

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}

/// A lookup is refused before it starts when it names nothing a resolver could find — a host that is not UTF-8 — or a port past the sixteen bits a port is.
#[test]
fn a_lookup_of_no_host_or_no_port_is_refused_before_it_starts() {
    const EINVAL: u32 = 22;

    let host = OsHost::with_args(vec![]);

    assert_eq!(
        come(&host, host.dns_lookup(b"\xffhost".to_vec(), 80)),
        Err(Failure::NotFound)
    );
    assert_eq!(
        come(&host, host.dns_lookup(b"localhost".to_vec(), 65_536)),
        Err(Failure::Other(EINVAL))
    );
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

    assert!(awaited(&host, &server, true, false));
    assert_eq!(host.handle_read(server.clone(), 8), Ok(Some(b"x".to_vec())));

    host.handle_close(server);
    host.handle_close(client);
    host.handle_close(listener);
}
