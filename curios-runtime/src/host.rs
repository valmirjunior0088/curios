//! The native adapter's own concerns, and the contract it speaks in.
//!
//! The wire contract — the [`Handle`]/[`Status`]/[`Poll`]/[`Mode`] semantic types and the [`HostOps`] trait — is authored once in `curios-abi` and re-exported here so the rest of the runtime names it unqualified. What lives here is only what is genuinely native: mapping an `io::Error` to a wire [`Status`], a [`Poll`] mask to and from the platform `poll` flags (whose raw values differ per platform), and a serial frame's tags to the termios bits that set it. These are the adapter's job, not the contract's, so they stay free functions in the runtime rather than methods on the shared types.

pub use curios_abi::{Handle, HostOps, Mode, Poll, Status, Termination};

use {
    curios_abi::{event, serial_flow, serial_parity},
    rustix::{event::PollFlags, termios::ControlModes},
    std::io::{Error, ErrorKind},
};

/// The control-mode bits a serial frame sets, or `None` for a setting `serial/open` refuses: 7 or 8 data bits, a [`serial_parity`] tag, 1 or 2 stop bits, and a [`serial_flow`] tag. Both hosts ask it, so a frame the native host refuses is one the scripted host refuses too.
pub(crate) fn serial_frame(
    data_bits: u64,
    parity: u64,
    stop_bits: u64,
    flow: u64,
) -> Option<ControlModes> {
    let size = match data_bits {
        7 => ControlModes::CS7,
        8 => ControlModes::CS8,
        _ => return None,
    };

    let parity = match parity {
        serial_parity::NONE => ControlModes::empty(),
        serial_parity::EVEN => ControlModes::PARENB,
        serial_parity::ODD => ControlModes::PARENB | ControlModes::PARODD,
        _ => return None,
    };

    let stop = match stop_bits {
        1 => ControlModes::empty(),
        2 => ControlModes::CSTOPB,
        _ => return None,
    };

    let flow = match flow {
        serial_flow::NONE => ControlModes::empty(),
        serial_flow::HARDWARE => ControlModes::CRTSCTS,
        _ => return None,
    };

    Some(size | parity | stop | flow)
}

/// Map an `io::Error` to its wire [`Status`]. The named kinds map to named statuses; anything else with an errno surfaces raw through `Other(errno)`. An errno-less failure (e.g. `write_all`'s synthesized `WriteZero`) is unclassifiable, so it reports the catch-all `Other(0)`; callers that can name it (e.g. `dns_resolve` → `NotFound`) map it at the call site.
pub(crate) fn status_from_error(error: Error) -> Status {
    match error.kind() {
        ErrorKind::NotFound => Status::NotFound,
        ErrorKind::PermissionDenied => Status::PermissionDenied,
        ErrorKind::AlreadyExists => Status::AlreadyExists,
        ErrorKind::ConnectionRefused => Status::ConnectionRefused,
        ErrorKind::WouldBlock => Status::WouldBlock,
        ErrorKind::DirectoryNotEmpty => Status::NotEmpty,
        ErrorKind::IsADirectory => Status::IsDirectory,
        ErrorKind::NotADirectory => Status::NotDirectory,
        _ => match error.raw_os_error() {
            Some(errno) => Status::Other(errno as u32),
            None => Status::Other(0),
        },
    }
}

/// Map a [`Poll`] interest mask to the platform `poll` flags. Only `READ`/ `WRITE` are settable interests; the result-only `ERR`/`HUP` are never requested.
pub(crate) fn poll_to_flags(events: Poll) -> PollFlags {
    let mut flags = PollFlags::empty();

    if events.bits() & event::READ != 0 {
        flags |= PollFlags::IN;
    }

    if events.bits() & event::WRITE != 0 {
        flags |= PollFlags::OUT;
    }

    flags
}

/// Map the platform `revents` back to a [`Poll`] readiness mask, including the result-only `ERR`/`HUP` the kernel reports whether or not they were asked for.
pub(crate) fn poll_from_flags(flags: PollFlags) -> Poll {
    let mut bits = 0;

    if flags.contains(PollFlags::IN) {
        bits |= event::READ;
    }

    if flags.contains(PollFlags::OUT) {
        bits |= event::WRITE;
    }

    if flags.contains(PollFlags::ERR) {
        bits |= event::ERR;
    }

    if flags.contains(PollFlags::HUP) {
        bits |= event::HUP;
    }

    Poll::from_bits(bits)
}
