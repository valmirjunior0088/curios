//! The terminal session the coordination and host boundary measurements both drive.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use curios_runtime::{MockHost, MockIo};

pub(super) fn listing_host() -> (MockHost, MockIo) {
    let mut keys = vec![b"\x1b[".to_vec(), b"B".to_vec(), b"\x1b[A".to_vec()];
    for _ in 1..100 {
        keys.extend([b"\x1b[B".to_vec(), b"\x1b[A".to_vec()]);
    }
    keys.push(b"q".to_vec());

    MockHost::builder()
        .tty_sizes([(12, 5), (16, 6), (12, 5)])
        .stdin_chunks(keys)
        .mono((0..100_000).map(|millis| (millis / 1_000, millis % 1_000 * 1_000_000)))
        .build()
}

pub(super) fn assert_listing(io: &MockIo) {
    let output = String::from_utf8(io.output()).expect("terminal output is UTF-8");
    assert!(output.starts_with("\x1b[?1049h\x1b[?25l\x1b[?2004h\x1b[>1u"));
    assert!(output.ends_with("\x1b[<u\x1b[?2004l\x1b[?25h\x1b[?1049lone"));
    assert_eq!(
        output.matches("\x1b[2J").count(),
        3,
        "initial frame and two resizes"
    );
    assert!(output.contains("┌pick"));
    assert_eq!(io.raw_modes(), [true, false]);
    assert!(io.errors().is_empty());
}
