use curios_runtime::{Host, Io, OsHost, Poll};

// A manual check of the real readiness oracle: a genuine `poll(2)` through
// rustix. stdout (fd 1) is essentially always ready to write and `timeout 0`
// returns at once, so this drives the rustix call, the stdio `BorrowedFd` path,
// and the `Poll` <-> `PollFlags` mapping end to end against the real OS.
//
// Deliberately not a `#[test]`: it touches real OS file descriptors rather than
// the mock readiness oracle (`io_poll_mock_echoes_interest` covers the wiring).
// Run it by hand:
//
//     cargo run --example crs_poll --features cli
fn main() {
    let revents = OsHost::new().poll(&[Io::Stdout], &[Poll::WRITE], 0);
    assert!(
        revents[0].contains(Poll::WRITE),
        "stdout should poll writable"
    );
    println!("poll: stdout writable");
}
