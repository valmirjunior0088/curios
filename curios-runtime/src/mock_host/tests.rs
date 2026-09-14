//! The scripted host's contract: handles miss loudly after close, and a chunked stream — standard input among them — hands the wait back to its reader between chunks.

use {
    super::{super::host::*, EBUSY, MockHost},
    curios_abi::{event, serial_flow, serial_op, serial_parity},
};

#[test]
fn a_chunked_endpoint_serves_one_chunk_then_would_blocks_until_polled() {
    let (host, _io) = MockHost::builder()
        .net_chunks([("example.com:80", vec!["ab", "cd"])])
        .build();

    let (status, handle) = host.socket_open(b"example.com:80");
    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.socket_connect(handle.clone(), b"example.com:80"),
        Status::Ok
    ));

    // The first chunk is due from the start; spending it disarms the stream.
    assert!(matches!(host.handle_read(handle.clone(), 8), (Status::Ok, bytes) if bytes == b"ab"));
    assert!(matches!(
        host.handle_read(handle.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));

    // A poll arms the next chunk and reports the handle readable, and only then does the read serve it.
    let ready = host.handle_poll(
        std::slice::from_ref(&handle),
        &[Poll::from_bits(event::READ)],
        -1,
    );
    assert_eq!(ready[0].bits() & event::READ, event::READ);
    assert!(matches!(host.handle_read(handle.clone(), 8), (Status::Ok, bytes) if bytes == b"cd"));

    // Past the last chunk the stream is at its end, which a poll still reports as readable.
    assert!(matches!(
        host.handle_read(handle.clone(), 8),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
    let ready = host.handle_poll(&[handle], &[Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
}

#[test]
fn a_pending_connect_settles_through_poll_and_finish_connect() {
    let (host, _io) = MockHost::builder()
        .net([("example.com:80", "pong")])
        .connect_pending()
        .build();

    // A scripted endpoint: pending, then writable, then connected and serving.
    let (_, handle) = host.socket_open(b"example.com:80");
    assert!(matches!(
        host.socket_connect(handle.clone(), b"example.com:80"),
        Status::WouldBlock
    ));
    assert!(matches!(
        host.socket_finish_connect(handle.clone()),
        Status::WouldBlock
    ));
    let ready = host.handle_poll(
        std::slice::from_ref(&handle),
        &[Poll::from_bits(event::WRITE)],
        -1,
    );
    assert_eq!(ready[0].bits() & event::WRITE, event::WRITE);
    assert!(matches!(
        host.socket_finish_connect(handle.clone()),
        Status::Ok
    ));
    assert!(matches!(host.handle_read(handle, 8), (Status::Ok, bytes) if bytes == b"pong"));

    // An unscripted endpoint: the refusal is deferred to the settle, and the handle is gone afterwards.
    let (_, stray) = host.socket_open(b"nowhere:1");
    assert!(matches!(
        host.socket_connect(stray.clone(), b"nowhere:1"),
        Status::WouldBlock
    ));
    host.handle_poll(
        std::slice::from_ref(&stray),
        &[Poll::from_bits(event::WRITE)],
        -1,
    );
    assert!(matches!(
        host.socket_finish_connect(stray.clone()),
        Status::ConnectionRefused
    ));
    assert!(matches!(host.handle_read(stray, 8), (Status::NotFound, _)));
}

#[test]
fn a_chunked_endpoint_ends_readable() {
    let (host, _io) = MockHost::builder().net([("example.com:80", "")]).build();
    let (_, handle) = host.socket_open(b"example.com:80");
    assert!(matches!(
        host.socket_connect(handle.clone(), b"example.com:80"),
        Status::Ok
    ));

    // A stream at its end reads `Eof`, and a poll still reports it readable, as an OS reports a closed peer.
    assert!(matches!(
        host.handle_read(handle.clone(), 8),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
    let ready = host.handle_poll(&[handle], &[Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
}

#[test]
fn use_after_close_on_a_handle_is_a_loud_miss_not_an_alias() {
    let (host, _io) = MockHost::builder().build();

    // Open a write-mode file, write to it, then close it.
    let (status, handle) = host.file_open(b"f", Mode::Write);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.handle_write(handle.clone(), b"x"),
        (Status::Ok, 1)
    ));
    host.handle_close(handle.clone());

    // Write after close is a loud `NotFound`, never a silent success...
    assert!(matches!(
        host.handle_write(handle.clone(), b"y"),
        (Status::NotFound, 0)
    ));
    // ...read after close is the same loud miss, never a quiet `Eof` drain...
    assert!(matches!(
        host.handle_read(handle.clone(), 8),
        (Status::NotFound, bytes) if bytes.is_empty()
    ));
    // ...and a double close is a no-op, not a panic.
    host.handle_close(handle.clone());

    // A second open never reuses the closed token, and the stale handle keeps missing rather than aliasing the freshly opened file.
    let (status, fresh) = host.file_open(b"g", Mode::Write);
    assert!(matches!(status, Status::Ok));
    assert_ne!(handle.bytes(), fresh.bytes());
    assert!(matches!(
        host.handle_write(handle, b"z"),
        (Status::NotFound, 0)
    ));
    assert!(matches!(host.handle_write(fresh, b"ok"), (Status::Ok, 2)));
}

/// A writing `file_open` under a directory that is not there answers `NotFound`, as the OS does, rather than filing a file whose parent `file_stat` would then deny; once the directory is made, the same open succeeds and the parent stats as a directory.
#[test]
fn a_writing_open_under_a_missing_directory_is_refused_until_the_directory_exists() {
    let (host, _io) = MockHost::builder().build();

    for mode in [Mode::Write, Mode::Append] {
        assert!(matches!(
            host.file_open(b"a/b.txt", mode),
            (Status::NotFound, handle) if handle.is_none()
        ));
    }
    assert!(matches!(host.file_stat(b"a"), (Status::NotFound, ..)));

    assert!(matches!(host.dir_create(b"a"), Status::Ok));
    let (status, handle) = host.file_open(b"a/b.txt", Mode::Write);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.handle_write(handle.clone(), b"x"),
        (Status::Ok, 1)
    ));
    host.handle_close(handle);
    assert!(matches!(
        host.file_stat(b"a"),
        (Status::Ok, kind, ..) if kind == curios_abi::file_kind::DIRECTORY
    ));
    assert!(matches!(
        host.file_open(b"a/b.txt", Mode::Append),
        (Status::Ok, _)
    ));
}

/// The root exists without being seeded: a directory is made and a file written under `/`, the root stats and lists as a directory holding them, remaking it is `AlreadyExists`, and removing it is refused — `NotEmpty` while it holds anything, `EBUSY` once it is bare, as `rmdir(2)` answers.
#[test]
fn the_root_directory_exists_holds_absolute_paths_and_cannot_be_removed() {
    let (host, _io) = MockHost::builder().build();

    assert!(matches!(host.dir_create(b"/x"), Status::Ok));
    let (status, handle) = host.file_open(b"/x/f", Mode::Write);
    assert!(matches!(status, Status::Ok));
    host.handle_close(handle);

    assert!(matches!(
        host.file_stat(b"/"),
        (Status::Ok, kind, ..) if kind == curios_abi::file_kind::DIRECTORY
    ));
    assert!(matches!(host.dir_list(b"/"), (Status::Ok, names) if names == [b"x".to_vec()]));
    assert!(matches!(host.dir_list(b"/x"), (Status::Ok, names) if names == [b"f".to_vec()]));
    assert!(matches!(host.dir_create(b"/"), Status::AlreadyExists));
    assert!(matches!(host.file_remove(b"/"), Status::IsDirectory));
    assert!(matches!(host.dir_remove(b"/"), Status::NotEmpty));

    assert!(matches!(host.file_remove(b"/x/f"), Status::Ok));
    assert!(matches!(host.dir_remove(b"/x"), Status::Ok));
    assert!(matches!(host.dir_remove(b"/"), Status::Other(EBUSY)));
}

#[test]
fn scripted_stdin_serves_one_chunk_then_would_blocks_until_polled() {
    let (host, _io) = MockHost::builder()
        .stdin_chunks(vec![b"\x1b[A".as_slice(), b"q".as_slice()])
        .build();

    // The first chunk is due from the start, and it is the bytes the script wrote — no terminator was added to a key.
    assert!(
        matches!(host.handle_read(Handle::Stdin, 8), (Status::Ok, bytes) if bytes == b"\x1b[A")
    );
    assert!(matches!(
        host.handle_read(Handle::Stdin, 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));

    // A poll arms the next chunk and reports standard input readable, and only then does the read serve it: the park-poll-resume path a keystroke arriving later takes.
    let ready = host.handle_poll(&[Handle::Stdin], &[Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
    assert!(matches!(host.handle_read(Handle::Stdin, 8), (Status::Ok, bytes) if bytes == b"q"));

    // Past the last chunk the script is spent, which is end-of-input.
    assert!(matches!(
        host.handle_read(Handle::Stdin, 8),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
}

#[test]
fn scripted_stdin_lines_are_one_chunk_that_never_waits() {
    let (host, _io) = MockHost::builder().stdin_lines(["one", "two"]).build();

    // Lines are the one armed chunk they have always been, so a reader crosses from one to the next without a poll between them.
    assert!(matches!(host.handle_read(Handle::Stdin, 4), (Status::Ok, bytes) if bytes == b"one\n"));
    assert!(matches!(host.handle_read(Handle::Stdin, 4), (Status::Ok, bytes) if bytes == b"two\n"));
    assert!(matches!(
        host.handle_read(Handle::Stdin, 4),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
}

#[test]
fn stderr_is_readable_apart_from_the_concatenation_of_both_streams() {
    let (host, io) = MockHost::builder().build();

    host.handle_write(Handle::Stdout, b"out ");
    host.handle_write(Handle::Stderr, b"err ");
    host.handle_write(Handle::Stdout, b"more");

    assert_eq!(io.output(), b"out err more");
    assert_eq!(io.errors(), b"err ");
}

// A scripted serial port holds its path until it is closed, and an input discard drops the chunk that has arrived while the one still to come survives it.
#[test]
fn a_serial_port_is_held_until_closed_and_a_discard_drops_only_what_arrived() {
    let (host, io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", vec!["banner", "ready"])])
        .build();
    let open = || {
        host.serial_open(
            b"/dev/ttyUSB0",
            9600,
            8,
            serial_parity::NONE,
            1,
            serial_flow::NONE,
        )
    };

    let (status, port) = open();
    assert!(matches!(status, Status::Ok));
    assert!(matches!(open(), (Status::Other(EBUSY), _)));

    assert!(matches!(
        host.serial_control(port.clone(), serial_op::DISCARD_INPUT, 0),
        Status::Ok
    ));
    assert!(matches!(
        host.handle_read(port.clone(), 16),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));
    host.handle_poll(
        std::slice::from_ref(&port),
        &[Poll::from_bits(event::READ)],
        0,
    );
    assert!(matches!(
        host.handle_read(port.clone(), 16),
        (Status::Ok, bytes) if bytes == b"ready"
    ));

    host.handle_close(port);
    assert!(matches!(open(), (Status::Ok, _)));
    assert_eq!(io.serial_opens().len(), 2);
}
