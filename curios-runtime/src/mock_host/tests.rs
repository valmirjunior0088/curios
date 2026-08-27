use super::{super::host::*, MockHost};

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
