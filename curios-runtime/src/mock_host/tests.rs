//! The scripted host's contract: handles miss loudly after close, and a chunked stream hands the wait back to its reader between chunks.

use {
    super::{super::host::*, MockHost},
    curios_abi::poll,
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
        &[Poll::from_bits(poll::READ)],
        -1,
    );
    assert_eq!(ready[0].bits() & poll::READ, poll::READ);
    assert!(matches!(host.read(handle.clone(), 8), (Status::Ok, bytes) if bytes == b"cd"));

    // Past the last chunk the stream is at its end, which a poll still reports as readable.
    assert!(matches!(
        host.read(handle.clone(), 8),
        (Status::Eof, bytes) if bytes.is_empty()
    ));
    let ready = host.poll(&[handle], &[Poll::from_bits(poll::READ)], -1);
    assert_eq!(ready[0].bits() & poll::READ, poll::READ);
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
