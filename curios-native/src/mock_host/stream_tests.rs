//! Scripted streams: a chunked endpoint and standard input handing the wait back between chunks, a peer's end, and a read and a write within what was asked.

use {
    super::{
        MockHost, SocketEvent,
        test_support::{spawned, watched},
    },
    crate::{Failure, Handle, Mode},
    curios_abi::{HostOps, event},
};

#[test]
fn a_chunked_endpoint_serves_one_chunk_then_would_blocks_until_watched() {
    let (host, _io) = MockHost::builder()
        .net_chunks([("example.com:80", vec!["ab", "cd"])])
        .build();

    let handle = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(handle.clone(), b"example.com:80".to_vec()),
        Ok(())
    );

    // The first chunk is due from the start; spending it disarms the stream.
    assert_eq!(
        host.handle_read(handle.clone(), 8),
        Ok(Some(b"ab".to_vec()))
    );
    assert_eq!(
        host.handle_read(handle.clone(), 8),
        Err(Failure::WouldBlock)
    );

    // A look at a watch arms the next chunk and finds the handle readable, and only then does the read serve it.
    assert!(watched(&host, &handle, true, false));
    assert_eq!(
        host.handle_read(handle.clone(), 8),
        Ok(Some(b"cd".to_vec()))
    );

    // Past the last chunk the stream is at its end, which a watch still finds readable.
    assert_eq!(host.handle_read(handle.clone(), 8), Ok(None));
    assert!(watched(&host, &handle, true, false));
}

#[test]
fn a_pending_connect_settles_through_a_watch_and_finish_connect() {
    let (host, _io) = MockHost::builder()
        .net([("example.com:80", "pong")])
        .connect_pending()
        .build();

    // A scripted endpoint: pending, then writable, then connected and serving.
    let handle = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(handle.clone(), b"example.com:80".to_vec()),
        Err(Failure::WouldBlock)
    );
    assert_eq!(
        host.socket_finish_connect(handle.clone()),
        Err(Failure::WouldBlock)
    );
    assert!(watched(&host, &handle, false, true));
    assert_eq!(host.socket_finish_connect(handle.clone()), Ok(()));
    assert_eq!(host.handle_read(handle, 8), Ok(Some(b"pong".to_vec())));

    // An unscripted endpoint: the refusal is deferred to the settle, and the handle is gone afterwards.
    let stray = host.socket_open(b"nowhere:1".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(stray.clone(), b"nowhere:1".to_vec()),
        Err(Failure::WouldBlock)
    );
    assert!(watched(&host, &stray, false, true));
    assert_eq!(
        host.socket_finish_connect(stray.clone()),
        Err(Failure::ConnectionRefused)
    );
    assert_eq!(host.handle_read(stray, 8), Err(Failure::NotFound));
}

#[test]
fn a_chunked_endpoint_ends_readable() {
    let (host, _io) = MockHost::builder().net([("example.com:80", "")]).build();
    let handle = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(handle.clone(), b"example.com:80".to_vec()),
        Ok(())
    );

    // A stream at its end reads as its end, and a watch still finds it readable, as an OS reports a closed peer.
    assert_eq!(host.handle_read(handle.clone(), 8), Ok(None));
    assert!(watched(&host, &handle, true, false));
}

#[test]
fn scripted_stdin_serves_one_chunk_then_would_blocks_until_watched() {
    let (host, _io) = MockHost::builder()
        .stdin_chunks(vec![b"\x1b[A".as_slice(), b"q".as_slice()])
        .build();

    // The first chunk is due from the start, and it is the bytes the script wrote — no terminator was added to a key.
    assert_eq!(
        host.handle_read(Handle::Stdin, 8),
        Ok(Some(b"\x1b[A".to_vec()))
    );
    assert_eq!(host.handle_read(Handle::Stdin, 8), Err(Failure::WouldBlock));

    // A look at a watch arms the next chunk and finds standard input readable, and only then does the read serve it: the wait-and-resume path a keystroke arriving later takes.
    assert!(watched(&host, &Handle::Stdin, true, false));
    assert_eq!(host.handle_read(Handle::Stdin, 8), Ok(Some(b"q".to_vec())));

    // Past the last chunk the script is spent, which is end-of-input.
    assert_eq!(host.handle_read(Handle::Stdin, 8), Ok(None));
}

#[test]
fn scripted_stdin_lines_are_one_chunk_that_never_waits() {
    let (host, _io) = MockHost::builder().stdin_lines(["one", "two"]).build();

    // Lines are one armed chunk, so a reader crosses from one to the next without a wait between them.
    assert_eq!(
        host.handle_read(Handle::Stdin, 4),
        Ok(Some(b"one\n".to_vec()))
    );
    assert_eq!(
        host.handle_read(Handle::Stdin, 4),
        Ok(Some(b"two\n".to_vec()))
    );
    assert_eq!(host.handle_read(Handle::Stdin, 4), Ok(None));
}

#[test]
fn stderr_is_readable_apart_from_the_concatenation_of_both_streams() {
    let (host, io) = MockHost::builder().build();

    for (stream, bytes) in [
        (Handle::Stdout, b"out ".as_slice()),
        (Handle::Stderr, b"err "),
        (Handle::Stdout, b"more"),
    ] {
        assert!(host.handle_write(stream, bytes.to_vec()).is_ok());
    }

    assert_eq!(io.output(), b"out err more");
    assert_eq!(io.errors(), b"err ");
}

/// A stream used in the direction it is not open for is `EBADF`, as the native host answers: a standard stream, a file opened the other way, a child's pipe read from its writing end. Anything that is not a stream is `NotFound`.
#[test]
fn a_stream_is_open_the_ways_its_host_opened_it() {
    const EBADF: Result<(), Failure> = Err(Failure::Other(9));

    let (host, _io) = MockHost::builder().files([("r", "text")]).build();
    let reading = host.file_open(b"r".to_vec(), Mode::Read).unwrap();
    let writing = host.file_open(b"w".to_vec(), Mode::Write).unwrap();

    assert_eq!(host.handle_read(Handle::Stdout, 8).map(|_| ()), EBADF);
    assert_eq!(
        host.handle_write(Handle::Stdin, b"x".to_vec()).map(|_| ()),
        EBADF
    );
    assert_eq!(
        host.handle_write(reading.clone(), b"x".to_vec())
            .map(|_| ()),
        EBADF
    );
    assert_eq!(host.handle_read(writing.clone(), 8).map(|_| ()), EBADF);
    assert_eq!(host.handle_read(reading, 2), Ok(Some(b"te".to_vec())));
    assert_eq!(host.handle_write(writing, b"x".to_vec()), Ok(1));
}

/// A request for nothing checks the handle and moves nothing: a zero read of a spent stream is empty bytes rather than its end, and an empty write is `0`.
#[test]
fn a_request_for_nothing_moves_nothing() {
    let (host, _io) = MockHost::builder().build();

    assert_eq!(host.handle_read(Handle::Stdin, 0), Ok(Some(vec![])));
    assert_eq!(host.handle_read(Handle::Stdin, 8), Ok(None));
    assert_eq!(host.handle_write(Handle::Stdout, vec![]), Ok(0));
    assert_eq!(
        host.handle_read(Handle::Other(vec![0xff]), 0),
        Err(Failure::NotFound)
    );
}

/// A scripted flush waits as a TLS stream's does while its records wait for the socket, then drains; the standard streams hold nothing, and an unknown handle is `NotFound`.
#[test]
fn a_scripted_flush_waits_then_drains() {
    let (host, _io) = MockHost::builder()
        .net([("example.com:80", "")])
        .pending_flushes(2)
        .build();
    let socket = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(socket.clone(), b"example.com:80".to_vec()),
        Ok(())
    );

    assert_eq!(host.handle_flush(Handle::Stdout), Ok(()));
    assert_eq!(host.handle_flush(socket.clone()), Err(Failure::WouldBlock));
    assert_eq!(host.handle_flush(socket.clone()), Err(Failure::WouldBlock));
    assert_eq!(host.handle_flush(socket), Ok(()));
    assert_eq!(
        host.handle_flush(Handle::Other(vec![0xff])),
        Err(Failure::NotFound)
    );
}

/// A scripted finish waits as a TLS stream's does while its closure alert waits for the socket, then ends the sending half, which is recorded; a standard stream and an unknown handle have none to end.
#[test]
fn a_scripted_finish_waits_then_ends_the_sending_half() {
    let (host, io) = MockHost::builder()
        .net([("example.com:80", "")])
        .pending_finishes(1)
        .build();
    let socket = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(socket.clone(), b"example.com:80".to_vec()),
        Ok(())
    );

    assert_eq!(host.handle_finish(Handle::Stdout), Err(Failure::NotFound));
    assert_eq!(
        host.handle_finish(Handle::Other(vec![0xff])),
        Err(Failure::NotFound)
    );
    assert_eq!(host.handle_finish(socket.clone()), Err(Failure::WouldBlock));
    assert!(io.socket_events().is_empty());
    assert_eq!(host.handle_finish(socket.clone()), Ok(()));
    host.handle_close(socket);
    assert_eq!(
        io.socket_events(),
        [SocketEvent::Finished, SocketEvent::Closed]
    );
}

/// A peer scripted to wait keeps its sending half open past its chunks: a read answers `WouldBlock`, a watch finds nothing to read, and a wait on it is refused, where a native one would sleep without end. Once this side finishes, the peer has closed, and the events say in what order it all happened.
#[test]
fn a_waiting_peer_closes_once_this_side_has_finished() {
    let (host, io) = MockHost::builder().inbound_waiting([vec!["ping"]]).build();
    let listener = host.socket_listen(b"0.0.0.0:8080".to_vec()).unwrap();
    let conn = host.socket_accept(listener).unwrap();
    let reading = || watched(&host, &conn, true, false);

    assert_eq!(
        host.handle_read(conn.clone(), 8),
        Ok(Some(b"ping".to_vec()))
    );
    assert_eq!(host.handle_read(conn.clone(), 8), Err(Failure::WouldBlock));
    assert!(!reading());
    let watch = host.handle_watch(conn.clone(), true, false);
    assert!(host.wait(vec![watch.clone()]).is_err());
    host.handle_close(watch);

    assert_eq!(host.handle_write(conn.clone(), b"pong".to_vec()), Ok(4));
    assert_eq!(host.handle_finish(conn.clone()), Ok(()));
    assert!(reading());
    assert_eq!(host.handle_read(conn.clone(), 8), Ok(None));
    host.handle_close(conn);

    assert_eq!(
        io.socket_events(),
        [
            SocketEvent::Wrote(4),
            SocketEvent::Finished,
            SocketEvent::Ended,
            SocketEvent::Closed,
        ]
    );
}

/// A peer scripted to stay open keeps its sending half open whatever this side does: once this side has finished, a read still answers `WouldBlock` and a watch still finds nothing to read.
#[test]
fn an_open_peer_stays_open_once_this_side_has_finished() {
    let (host, io) = MockHost::builder().inbound_open([vec!["ping"]]).build();
    let listener = host.socket_listen(b"0.0.0.0:8080".to_vec()).unwrap();
    let conn = host.socket_accept(listener).unwrap();

    assert_eq!(
        host.handle_read(conn.clone(), 8),
        Ok(Some(b"ping".to_vec()))
    );
    assert_eq!(host.handle_finish(conn.clone()), Ok(()));
    assert_eq!(host.handle_read(conn.clone(), 8), Err(Failure::WouldBlock));
    assert!(!watched(&host, &conn, true, false));
    host.handle_close(conn);

    assert_eq!(
        io.socket_events(),
        [SocketEvent::Finished, SocketEvent::Closed]
    );
}

/// A peer scripted to reset answers `ConnectionReset` to the read after its chunks, where one that closed answers the end of the stream.
#[test]
fn a_resetting_peer_answers_a_reset_after_its_chunks() {
    let (host, _io) = MockHost::builder()
        .net_reset([("example.com:80", vec!["par"])])
        .build();
    let socket = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(socket.clone(), b"example.com:80".to_vec()),
        Ok(())
    );

    assert_eq!(
        host.handle_read(socket.clone(), 8),
        Ok(Some(b"par".to_vec()))
    );
    assert_eq!(host.handle_read(socket, 8), Err(Failure::ConnectionReset));
}

/// A peer scripted to have stopped reading refuses every write with a broken pipe, and what it sent before is still read to its end.
#[test]
fn a_deaf_peer_refuses_a_write_and_is_still_read() {
    let (host, _io) = MockHost::builder()
        .net_deaf([("example.com:80", vec!["early"])])
        .build();
    let socket = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(socket.clone(), b"example.com:80".to_vec()),
        Ok(())
    );

    assert_eq!(
        host.handle_write(socket.clone(), b"request".to_vec()),
        Err(Failure::BrokenPipe)
    );
    assert_eq!(
        host.handle_read(socket.clone(), 8),
        Ok(Some(b"early".to_vec()))
    );
    assert_eq!(host.handle_read(socket, 8), Ok(None));
}

/// A read answers no more than it was asked for, and leaves the rest for the next.
#[test]
fn a_read_answers_no_more_than_it_was_asked() {
    let (host, _io) = MockHost::builder().stdin_lines(["hello"]).build();

    assert_eq!(host.handle_read(Handle::Stdin, 2), Ok(Some(b"he".to_vec())));
    assert!(
        host.handle_read(Handle::Stdin, 8)
            .unwrap()
            .is_some_and(|rest| rest.starts_with(b"llo"))
    );
}

/// A write accepts no more than it was offered, and at least a byte of a buffer that holds one.
#[test]
fn a_write_accepts_within_what_it_was_offered() {
    let (host, _io) = MockHost::builder().build();
    let accepted = host
        .handle_write(Handle::Stdout, b"hello".to_vec())
        .unwrap();

    assert!((1..=5).contains(&accepted));
}

/// A watch is ready for what it asks and no other: a stream with a chunk to read is ready to read and to write, and a child that still runs, which a watch waits on for its end, for neither. A look at several answers a byte for each, in their order.
#[test]
fn a_watch_is_ready_for_what_it_asks() {
    let (host, _io) = MockHost::builder()
        .net([("example.com:80", "pong")])
        .running_children(["sleepy"])
        .build();

    let stream = host.socket_open(b"example.com:80".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(stream.clone(), b"example.com:80".to_vec()),
        Ok(())
    );
    let child = spawned(&host, b"sleepy").child;

    assert!(watched(&host, &stream, true, false), "a chunk is due");
    assert!(watched(&host, &stream, false, true));
    assert!(!watched(&host, &child, true, false), "the child still runs");
    assert!(
        !watched(&host, &child, false, true),
        "a child is never written"
    );

    let watches = vec![
        host.handle_watch(child, true, true),
        host.handle_watch(stream, true, true),
    ];
    assert_eq!(host.handle_ready(watches), [0, event::READY]);
}

/// A standard stream the program closed names nothing, as a closed handle does: a read, a write and a flush answer `NotFound`, a watch of it is ready so a waiter wakes into the call that says why, and the streams left open answer as before.
#[test]
fn a_closed_standard_stream_names_nothing() {
    let (host, io) = MockHost::builder().stdin_lines(["unread"]).build();

    host.handle_close(Handle::Stdin);
    host.handle_close(Handle::Stdout);

    assert_eq!(host.handle_read(Handle::Stdin, 8), Err(Failure::NotFound));
    assert_eq!(
        host.handle_write(Handle::Stdout, b"lost".to_vec()),
        Err(Failure::NotFound)
    );
    assert_eq!(host.handle_flush(Handle::Stdout), Err(Failure::NotFound));
    let watch = host.handle_watch(Handle::Stdin, true, false);
    assert_eq!(host.handle_ready(vec![watch]), [event::READY]);

    assert_eq!(host.handle_write(Handle::Stderr, b"kept".to_vec()), Ok(4));
    assert_eq!(io.output(), b"kept");
}
