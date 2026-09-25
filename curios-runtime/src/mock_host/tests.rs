//! The scripted host's contract: handles miss loudly after close, and a chunked stream — standard input among them — hands the wait back to its reader between chunks.

use {
    super::{super::host::*, EBUSY, ENOTTY, MockHost},
    curios_abi::event,
};

#[test]
fn terminal_sizes_advance_on_queries_and_repeat_the_last() {
    let (host, io) = MockHost::builder().tty_sizes([(12, 5), (16, 6)]).build();

    assert_eq!(host.tty_raw(Handle::Stdin, true), Ok(()));
    assert_eq!(
        host.tty_size(Handle::Stdin),
        Ok(TtySize { cols: 12, rows: 5 })
    );
    assert_eq!(host.tty_raw(Handle::Stdin, false), Ok(()));
    for _ in 0..3 {
        assert_eq!(
            host.tty_size(Handle::Stdin),
            Ok(TtySize { cols: 16, rows: 6 })
        );
    }
    assert_eq!(io.raw_modes(), [true, false]);
}

#[test]
fn a_fixed_terminal_size_repeats_and_an_empty_script_has_no_terminal() {
    let (host, _) = MockHost::builder().tty_size(20, 5).build();
    for _ in 0..3 {
        assert_eq!(
            host.tty_size(Handle::Stdin),
            Ok(TtySize { cols: 20, rows: 5 })
        );
    }

    let (host, io) = MockHost::builder().tty_size(20, 5).tty_sizes([]).build();
    assert_eq!(host.tty_size(Handle::Stdin), Err(Failure::Other(ENOTTY)));
    assert_eq!(
        host.tty_raw(Handle::Stdin, true),
        Err(Failure::Other(ENOTTY))
    );
    assert!(io.raw_modes().is_empty());
}

#[test]
fn a_chunked_endpoint_serves_one_chunk_then_would_blocks_until_polled() {
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

    // A poll arms the next chunk and reports the handle readable, and only then does the read serve it.
    let ready = host.handle_poll(vec![handle.clone()], vec![Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
    assert_eq!(
        host.handle_read(handle.clone(), 8),
        Ok(Some(b"cd".to_vec()))
    );

    // Past the last chunk the stream is at its end, which a poll still reports as readable.
    assert_eq!(host.handle_read(handle.clone(), 8), Ok(None));
    let ready = host.handle_poll(vec![handle], vec![Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
}

#[test]
fn a_pending_connect_settles_through_poll_and_finish_connect() {
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
    let ready = host.handle_poll(
        vec![handle.clone()],
        vec![Poll::from_bits(event::WRITE)],
        -1,
    );
    assert_eq!(ready[0].bits() & event::WRITE, event::WRITE);
    assert_eq!(host.socket_finish_connect(handle.clone()), Ok(()));
    assert_eq!(host.handle_read(handle, 8), Ok(Some(b"pong".to_vec())));

    // An unscripted endpoint: the refusal is deferred to the settle, and the handle is gone afterwards.
    let stray = host.socket_open(b"nowhere:1".to_vec()).unwrap();
    assert_eq!(
        host.socket_connect(stray.clone(), b"nowhere:1".to_vec()),
        Err(Failure::WouldBlock)
    );
    host.handle_poll(vec![stray.clone()], vec![Poll::from_bits(event::WRITE)], -1);
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

    // A stream at its end reads as its end, and a poll still reports it readable, as an OS reports a closed peer.
    assert_eq!(host.handle_read(handle.clone(), 8), Ok(None));
    let ready = host.handle_poll(vec![handle], vec![Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
}

#[test]
fn use_after_close_on_a_handle_is_a_loud_miss_not_an_alias() {
    let (host, _io) = MockHost::builder().build();

    // Open a write-mode file, write to it, then close it.
    let handle = host.file_open(b"f".to_vec(), Mode::Write).unwrap();
    assert_eq!(host.handle_write(handle.clone(), b"x".to_vec()), Ok(1));
    host.handle_close(handle.clone());

    // Write after close is a loud `NotFound`, never a silent success...
    assert_eq!(
        host.handle_write(handle.clone(), b"y".to_vec()),
        Err(Failure::NotFound)
    );
    // ...read after close is the same loud miss, never a quiet end-of-stream drain...
    assert_eq!(host.handle_read(handle.clone(), 8), Err(Failure::NotFound));
    // ...and a double close is a no-op, not a panic.
    host.handle_close(handle.clone());

    // A second open never reuses the closed token, and the stale handle keeps missing rather than aliasing the freshly opened file.
    let fresh = host.file_open(b"g".to_vec(), Mode::Write).unwrap();
    assert_ne!(handle.bytes(), fresh.bytes());
    assert_eq!(
        host.handle_write(handle, b"z".to_vec()),
        Err(Failure::NotFound)
    );
    assert_eq!(host.handle_write(fresh, b"ok".to_vec()), Ok(2));
}

/// A writing `file_open` under a directory that is not there answers `NotFound`, as the OS does, rather than filing a file whose parent `file_stat` would then deny; once the directory is made, the same open succeeds and the parent stats as a directory.
#[test]
fn a_writing_open_under_a_missing_directory_is_refused_until_the_directory_exists() {
    let (host, _io) = MockHost::builder().build();

    for mode in [Mode::Write, Mode::Append] {
        assert!(matches!(
            host.file_open(b"a/b.txt".to_vec(), mode),
            Err(Failure::NotFound)
        ));
    }
    assert_eq!(host.file_stat(b"a".to_vec()), Err(Failure::NotFound));

    assert_eq!(host.dir_create(b"a".to_vec()), Ok(()));
    let handle = host.file_open(b"a/b.txt".to_vec(), Mode::Write).unwrap();
    assert_eq!(host.handle_write(handle.clone(), b"x".to_vec()), Ok(1));
    host.handle_close(handle);
    assert!(matches!(
        host.file_stat(b"a".to_vec()),
        Ok(FileStat {
            kind: FileKind::Directory,
            ..
        })
    ));
    assert!(host.file_open(b"a/b.txt".to_vec(), Mode::Append).is_ok());
}

/// The root exists without being seeded: a directory is made and a file written under `/`, the root stats and lists as a directory holding them, remaking it is `AlreadyExists`, and removing it is refused — `NotEmpty` while it holds anything, `EBUSY` once it is bare, as `rmdir(2)` answers.
#[test]
fn the_root_directory_exists_holds_absolute_paths_and_cannot_be_removed() {
    let (host, _io) = MockHost::builder().build();

    assert_eq!(host.dir_create(b"/x".to_vec()), Ok(()));
    let handle = host.file_open(b"/x/f".to_vec(), Mode::Write).unwrap();
    host.handle_close(handle);

    assert!(matches!(
        host.file_stat(b"/".to_vec()),
        Ok(FileStat {
            kind: FileKind::Directory,
            ..
        })
    ));
    assert_eq!(host.dir_list(b"/".to_vec()), Ok(vec![b"x".to_vec()]));
    assert_eq!(host.dir_list(b"/x".to_vec()), Ok(vec![b"f".to_vec()]));
    assert_eq!(host.dir_create(b"/".to_vec()), Err(Failure::AlreadyExists));
    assert_eq!(host.file_remove(b"/".to_vec()), Err(Failure::IsDirectory));
    assert_eq!(host.dir_remove(b"/".to_vec()), Err(Failure::NotEmpty));

    assert_eq!(host.file_remove(b"/x/f".to_vec()), Ok(()));
    assert_eq!(host.dir_remove(b"/x".to_vec()), Ok(()));
    assert_eq!(host.dir_remove(b"/".to_vec()), Err(Failure::Other(EBUSY)));
}

#[test]
fn scripted_stdin_serves_one_chunk_then_would_blocks_until_polled() {
    let (host, _io) = MockHost::builder()
        .stdin_chunks(vec![b"\x1b[A".as_slice(), b"q".as_slice()])
        .build();

    // The first chunk is due from the start, and it is the bytes the script wrote — no terminator was added to a key.
    assert_eq!(
        host.handle_read(Handle::Stdin, 8),
        Ok(Some(b"\x1b[A".to_vec()))
    );
    assert_eq!(host.handle_read(Handle::Stdin, 8), Err(Failure::WouldBlock));

    // A poll arms the next chunk and reports standard input readable, and only then does the read serve it: the park-poll-resume path a keystroke arriving later takes.
    let ready = host.handle_poll(vec![Handle::Stdin], vec![Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
    assert_eq!(host.handle_read(Handle::Stdin, 8), Ok(Some(b"q".to_vec())));

    // Past the last chunk the script is spent, which is end-of-input.
    assert_eq!(host.handle_read(Handle::Stdin, 8), Ok(None));
}

#[test]
fn scripted_stdin_lines_are_one_chunk_that_never_waits() {
    let (host, _io) = MockHost::builder().stdin_lines(["one", "two"]).build();

    // Lines are the one armed chunk they have always been, so a reader crosses from one to the next without a poll between them.
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

// An input discard on a scripted serial port drops the chunk that has arrived, while the one still to come survives it.
#[test]
fn a_serial_discard_drops_only_what_arrived() {
    let (host, _io) = MockHost::builder()
        .serial([("/dev/ttyUSB0", vec!["banner", "ready"])])
        .build();

    let port = host
        .serial_open(
            b"/dev/ttyUSB0".to_vec(),
            9600,
            8,
            SerialParity::None,
            1,
            SerialFlow::None,
        )
        .unwrap();

    assert_eq!(
        host.serial_control(port.clone(), SerialOp::DiscardInput, false),
        Ok(())
    );
    assert_eq!(host.handle_read(port.clone(), 16), Err(Failure::WouldBlock));
    host.handle_poll(vec![port.clone()], vec![Poll::from_bits(event::READ)], 0);
    assert_eq!(host.handle_read(port, 16), Ok(Some(b"ready".to_vec())));
}
