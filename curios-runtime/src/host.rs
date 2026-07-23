//! The native adapter's own concerns, and the contract it speaks in.
//!
//! The wire contract — the [`Handle`]/[`Status`]/[`Poll`]/[`Mode`] semantic
//! types and the [`HostOps`] trait — is authored once in `curios-abi` and
//! re-exported here so the rest of the runtime names it unqualified. What lives
//! here is only what is genuinely native: mapping an `io::Error` to a wire
//! [`Status`], and a [`Poll`] mask to and from the platform `poll` flags (whose
//! raw values differ per platform). These are the adapter's job, not the
//! contract's, so they stay free functions in the runtime rather than methods
//! on the shared types.

pub use curios_abi::{Handle, HostOps, Mode, Poll, Status};

use {
    curios_abi::poll,
    rustix::event::PollFlags,
    std::io::{Error, ErrorKind},
};

/// Map an `io::Error` to its wire [`Status`]. The named kinds map to named
/// statuses; anything else with an errno surfaces raw through `Other(errno)`.
/// An errno-less failure (e.g. `write_all`'s synthesized `WriteZero`) is
/// unclassifiable, so it reports the catch-all `Other(0)`; callers that can
/// name it (e.g. `resolve` → `NotFound`) map it at the call site.
pub(crate) fn status_from_error(error: Error) -> Status {
    match error.kind() {
        ErrorKind::NotFound => Status::NotFound,
        ErrorKind::PermissionDenied => Status::PermissionDenied,
        ErrorKind::AlreadyExists => Status::AlreadyExists,
        ErrorKind::ConnectionRefused => Status::ConnectionRefused,
        ErrorKind::WouldBlock => Status::WouldBlock,
        _ => match error.raw_os_error() {
            Some(errno) => Status::Other(errno as u32),
            None => Status::Other(0),
        },
    }
}

/// Map a [`Poll`] interest mask to the platform `poll` flags. Only `READ`/
/// `WRITE` are settable interests; the result-only `ERR`/`HUP` are never
/// requested.
pub(crate) fn poll_to_flags(events: Poll) -> PollFlags {
    let mut flags = PollFlags::empty();

    if events.bits() & poll::READ != 0 {
        flags |= PollFlags::IN;
    }

    if events.bits() & poll::WRITE != 0 {
        flags |= PollFlags::OUT;
    }

    flags
}

/// Map the platform `revents` back to a [`Poll`] readiness mask, including the
/// result-only `ERR`/`HUP` the kernel reports whether or not they were asked
/// for.
pub(crate) fn poll_from_flags(flags: PollFlags) -> Poll {
    let mut bits = 0;

    if flags.contains(PollFlags::IN) {
        bits |= poll::READ;
    }

    if flags.contains(PollFlags::OUT) {
        bits |= poll::WRITE;
    }

    if flags.contains(PollFlags::ERR) {
        bits |= poll::ERR;
    }

    if flags.contains(PollFlags::HUP) {
        bits |= poll::HUP;
    }

    Poll::from_bits(bits)
}
