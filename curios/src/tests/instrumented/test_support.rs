//! The terminal session the coordination and host boundary measurements both drive.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use curios_native::{MockHost, MockIo};

// The terminal answers the session's questions first, supporting none of what it asks after, so the loop asks the terminal's size where it is quiet. The keys pause twice between two whole keys, so the loop's deadline passes once at each and it reads the two scripted resizes, 16×6 and then 12×5.
pub(super) fn listing_host() -> (MockHost, MockIo) {
    let pair = || vec![b"\x1b[B".to_vec(), b"\x1b[A".to_vec()];
    let mut rest = Vec::new();
    for _ in 3..100 {
        rest.extend(pair());
    }
    rest.push(b"q".to_vec());

    MockHost::builder()
        .tty_sizes([(12, 5), (16, 6), (12, 5)])
        .stdin_chunks(vec![
            b"\x1b[?62c".to_vec(),
            b"\x1b[".to_vec(),
            b"B".to_vec(),
            b"\x1b[A".to_vec(),
        ])
        .stdin_pause()
        .stdin_chunks(pair())
        .stdin_pause()
        .stdin_chunks(pair())
        .stdin_chunks(rest)
        .mono(
            (0..100_000i64)
                .map(|millis| (millis / 1_000, (millis % 1_000 * 1_000_000).unsigned_abs())),
        )
        .build()
}

pub(super) fn assert_listing(io: &MockIo) {
    let output = String::from_utf8(io.output()).expect("terminal output is UTF-8");
    assert!(output.starts_with(
        "\x1b[?u\x1b[?2026$p\x1b[?2027$p\x1b[?2048$p\x1b[c\x1b[?1049h\x1b[?25l\x1b[?2004h\x1b[?1004h"
    ));
    assert!(
        output.ends_with(
            "\x1b[<u\x1b[?2048l\x1b[?2027l\x1b[?1004l\x1b[?2004l\x1b[?25h\x1b[?1049lone"
        )
    );
    assert_eq!(
        output.matches("\x1b[2J").count(),
        3,
        "initial frame and two resizes"
    );
    assert!(output.contains("┌\x1b[1;2Hpick"));
    assert_eq!(io.raw_modes(), [true, false]);
    assert!(io.errors().is_empty());
}
