//! The scripted host's contract: handles miss loudly after close, and a chunked stream — standard input among them — hands the wait back to its reader between chunks.

use {
    super::{super::host::*, EBUSY, MockHost},
    curios_abi::event,
};

#[test]
fn a_chunked_endpoint_serves_one_chunk_then_would_blocks_until_polled() {
    let (host, _io) = MockHost::builder()
        .net_chunks([("example.com:80", vec!["ab", "cd"])])
        .build();

    let (status, handle) = host.socket(b"example.com:80");
    assert!(matches!(status, Status::Ok));
    assert!(matches!(
        host.connect(handle.clone(), b"example.com:80"),
        Status::Ok
    ));

    // The first chunk is due from the start; spending it disarms the stream.
    assert!(matches!(host.read(handle.clone(), 8), (Status::Ok, bytes) if bytes == b"ab"));
    assert!(matches!(
        host.read(handle.clone(), 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));

    // A poll arms the next chunk and reports the handle readable, and only then does the read serve it.
    let ready = host.poll(
        std::slice::from_ref(&handle),
        &[Poll::from_bits(event::READ)],
        -1,
    );
    assert_eq!(ready[0].bits() & event::READ, event::READ);
    assert!(matches!(host.read(handle.clone(), 8), (Status::Ok, bytes) if bytes == b"cd"));

    // Past the last chunk the stream is at its end, which a poll still reports as readable.
    assert!(matches!(
        host.read(handle.clone(), 8),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
    let ready = host.poll(&[handle], &[Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
}

#[test]
fn a_pending_connect_settles_through_poll_and_finish_connect() {
    let (host, _io) = MockHost::builder()
        .net([("example.com:80", "pong")])
        .connect_pending()
        .build();

    // A scripted endpoint: pending, then writable, then connected and serving.
    let (_, handle) = host.socket(b"example.com:80");
    assert!(matches!(
        host.connect(handle.clone(), b"example.com:80"),
        Status::WouldBlock
    ));
    assert!(matches!(
        host.finish_connect(handle.clone()),
        Status::WouldBlock
    ));
    let ready = host.poll(
        std::slice::from_ref(&handle),
        &[Poll::from_bits(event::WRITE)],
        -1,
    );
    assert_eq!(ready[0].bits() & event::WRITE, event::WRITE);
    assert!(matches!(host.finish_connect(handle.clone()), Status::Ok));
    assert!(matches!(host.read(handle, 8), (Status::Ok, bytes) if bytes == b"pong"));

    // An unscripted endpoint: the refusal is deferred to the settle, and the handle is gone afterwards.
    let (_, stray) = host.socket(b"nowhere:1");
    assert!(matches!(
        host.connect(stray.clone(), b"nowhere:1"),
        Status::WouldBlock
    ));
    host.poll(
        std::slice::from_ref(&stray),
        &[Poll::from_bits(event::WRITE)],
        -1,
    );
    assert!(matches!(
        host.finish_connect(stray.clone()),
        Status::ConnectionRefused
    ));
    assert!(matches!(host.read(stray, 8), (Status::NotFound, _)));
}

#[test]
fn a_chunked_endpoint_ends_readable() {
    let (host, _io) = MockHost::builder().net([("example.com:80", "")]).build();
    let (_, handle) = host.socket(b"example.com:80");
    assert!(matches!(
        host.connect(handle.clone(), b"example.com:80"),
        Status::Ok
    ));

    // A stream at its end reads `Eof`, and a poll still reports it readable, as an OS reports a closed peer.
    assert!(matches!(
        host.read(handle.clone(), 8),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
    let ready = host.poll(&[handle], &[Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
}

#[test]
fn use_after_close_on_a_handle_is_a_loud_miss_not_an_alias() {
    let (host, _io) = MockHost::builder().build();

    // Open a write-mode file, write to it, then close it.
    let (status, handle) = host.open(b"f", Mode::Write);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(host.write(handle.clone(), b"x"), (Status::Ok, 1)));
    host.close(handle.clone());

    // Write after close is a loud `NotFound`, never a silent success...
    assert!(matches!(
        host.write(handle.clone(), b"y"),
        (Status::NotFound, 0)
    ));
    // ...read after close is the same loud miss, never a quiet `Eof` drain...
    assert!(matches!(
        host.read(handle.clone(), 8),
        (Status::NotFound, bytes) if bytes.is_empty()
    ));
    // ...and a double close is a no-op, not a panic.
    host.close(handle.clone());

    // A second open never reuses the closed token, and the stale handle keeps missing rather than aliasing the freshly opened file.
    let (status, fresh) = host.open(b"g", Mode::Write);
    assert!(matches!(status, Status::Ok));
    assert_ne!(handle.bytes(), fresh.bytes());
    assert!(matches!(host.write(handle, b"z"), (Status::NotFound, 0)));
    assert!(matches!(host.write(fresh, b"ok"), (Status::Ok, 2)));
}

/// A writing `open` under a directory that is not there answers `NotFound`, as the OS does, rather than filing a file whose parent `stat` would then deny; once the directory is made, the same open succeeds and the parent stats as a directory.
#[test]
fn a_writing_open_under_a_missing_directory_is_refused_until_the_directory_exists() {
    let (host, _io) = MockHost::builder().build();

    for mode in [Mode::Write, Mode::Append] {
        assert!(matches!(
            host.open(b"a/b.txt", mode),
            (Status::NotFound, handle) if handle.is_none()
        ));
    }
    assert!(matches!(host.stat(b"a"), (Status::NotFound, ..)));

    assert!(matches!(host.create_dir(b"a"), Status::Ok));
    let (status, handle) = host.open(b"a/b.txt", Mode::Write);
    assert!(matches!(status, Status::Ok));
    assert!(matches!(host.write(handle.clone(), b"x"), (Status::Ok, 1)));
    host.close(handle);
    assert!(matches!(
        host.stat(b"a"),
        (Status::Ok, kind, ..) if kind == curios_abi::file_kind::DIRECTORY
    ));
    assert!(matches!(
        host.open(b"a/b.txt", Mode::Append),
        (Status::Ok, _)
    ));
}

/// The root exists without being seeded: a directory is made and a file written under `/`, the root stats and lists as a directory holding them, remaking it is `AlreadyExists`, and removing it is refused — `NotEmpty` while it holds anything, `EBUSY` once it is bare, as `rmdir(2)` answers.
#[test]
fn the_root_directory_exists_holds_absolute_paths_and_cannot_be_removed() {
    let (host, _io) = MockHost::builder().build();

    assert!(matches!(host.create_dir(b"/x"), Status::Ok));
    let (status, handle) = host.open(b"/x/f", Mode::Write);
    assert!(matches!(status, Status::Ok));
    host.close(handle);

    assert!(matches!(
        host.stat(b"/"),
        (Status::Ok, kind, ..) if kind == curios_abi::file_kind::DIRECTORY
    ));
    assert!(matches!(host.list(b"/"), (Status::Ok, names) if names == [b"x".to_vec()]));
    assert!(matches!(host.list(b"/x"), (Status::Ok, names) if names == [b"f".to_vec()]));
    assert!(matches!(host.create_dir(b"/"), Status::AlreadyExists));
    assert!(matches!(host.remove_file(b"/"), Status::IsDirectory));
    assert!(matches!(host.remove_dir(b"/"), Status::NotEmpty));

    assert!(matches!(host.remove_file(b"/x/f"), Status::Ok));
    assert!(matches!(host.remove_dir(b"/x"), Status::Ok));
    assert!(matches!(host.remove_dir(b"/"), Status::Other(EBUSY)));
}

#[test]
fn scripted_stdin_serves_one_chunk_then_would_blocks_until_polled() {
    let (host, _io) = MockHost::builder()
        .stdin_chunks(vec![b"\x1b[A".as_slice(), b"q".as_slice()])
        .build();

    // The first chunk is due from the start, and it is the bytes the script wrote — no terminator was added to a key.
    assert!(matches!(host.read(Handle::Stdin, 8), (Status::Ok, bytes) if bytes == b"\x1b[A"));
    assert!(matches!(
        host.read(Handle::Stdin, 8),
        (Status::WouldBlock, bytes) if bytes.is_empty()
    ));

    // A poll arms the next chunk and reports standard input readable, and only then does the read serve it: the park-poll-resume path a keystroke arriving later takes.
    let ready = host.poll(&[Handle::Stdin], &[Poll::from_bits(event::READ)], -1);
    assert_eq!(ready[0].bits() & event::READ, event::READ);
    assert!(matches!(host.read(Handle::Stdin, 8), (Status::Ok, bytes) if bytes == b"q"));

    // Past the last chunk the script is spent, which is end-of-input.
    assert!(matches!(
        host.read(Handle::Stdin, 8),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
}

#[test]
fn scripted_stdin_lines_are_one_chunk_that_never_waits() {
    let (host, _io) = MockHost::builder().stdin_lines(["one", "two"]).build();

    // Lines are the one armed chunk they have always been, so a reader crosses from one to the next without a poll between them.
    assert!(matches!(host.read(Handle::Stdin, 4), (Status::Ok, bytes) if bytes == b"one\n"));
    assert!(matches!(host.read(Handle::Stdin, 4), (Status::Ok, bytes) if bytes == b"two\n"));
    assert!(matches!(
        host.read(Handle::Stdin, 4),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
}

#[test]
fn stderr_is_readable_apart_from_the_concatenation_of_both_streams() {
    let (host, io) = MockHost::builder().build();

    host.write(Handle::Stdout, b"out ");
    host.write(Handle::Stderr, b"err ");
    host.write(Handle::Stdout, b"more");

    assert_eq!(io.output(), b"out err more");
    assert_eq!(io.errors(), b"err ");
}
