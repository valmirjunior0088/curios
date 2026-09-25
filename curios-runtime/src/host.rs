//! The native adapter's own concerns, and the contract it speaks in.
//!
//! The wire contract — the [`Handle`]/[`Failure`]/[`Poll`]/[`Mode`] semantic types, the payloads and the [`HostOps`] trait — is authored once in `curios-abi` and re-exported here so the rest of the runtime names it unqualified. What lives here is only what is genuinely native: mapping an `io::Error` to a [`Failure`], a [`Poll`] mask to and from the platform `poll` flags (whose raw values differ per platform), and a serial frame's tags to the termios bits that set it. These are the adapter's job, not the contract's, so they stay free functions in the runtime rather than methods on the shared types.

pub use curios_abi::{
    ChildExit, ChildStream, Failure, FileKind, FileStat, Handle, HostOps, Mode, Poll, Refusal,
    SerialFlow, SerialOp, SerialParity, StdioMode, Termination, Timestamp, TtySize,
};

use {
    curios_abi::event,
    rustix::{event::PollFlags, io::Errno, termios::ControlModes},
    std::io::{Error, ErrorKind},
};

/// The control-mode bits a serial frame sets, or `None` for a setting `serial/open` refuses: 7 or 8 data bits and 1 or 2 stop bits, beside the parity and flow control a closed code has already decided. Both hosts ask it, so a frame the native host refuses is one the scripted host refuses too.
pub(crate) fn serial_frame(
    data_bits: u64,
    parity: SerialParity,
    stop_bits: u64,
    flow: SerialFlow,
) -> Option<ControlModes> {
    let size = match data_bits {
        7 => ControlModes::CS7,
        8 => ControlModes::CS8,
        _ => return None,
    };

    let parity = match parity {
        SerialParity::None => ControlModes::empty(),
        SerialParity::Even => ControlModes::PARENB,
        SerialParity::Odd => ControlModes::PARENB | ControlModes::PARODD,
    };

    let stop = match stop_bits {
        1 => ControlModes::empty(),
        2 => ControlModes::CSTOPB,
        _ => return None,
    };

    let flow = match flow {
        SerialFlow::None => ControlModes::empty(),
        SerialFlow::Hardware => ControlModes::CRTSCTS,
    };

    Some(size | parity | stop | flow)
}

/// Which way a stream row moves bytes: what a request of nothing is checked against without moving any, and what a stream open only the other way refuses with `EBADF`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Direction {
    Read,
    Write,
}

/// The `host:port` a lookup asks for, or the failure that refuses it before any lookup starts: a host that is not UTF-8 names nothing a resolver can find, which is `NotFound`, and a port past 65535 is no port, which is `EINVAL`. Both hosts ask it, so a lookup the native host refuses is one the scripted host refuses too.
pub(crate) fn lookup_address(host: &[u8], port: u64) -> Result<String, Failure> {
    let host = str::from_utf8(host).map_err(|_| Failure::NotFound)?;
    let port = u16::try_from(port).map_err(|_| failure_from_error(Error::from(Errno::INVAL)))?;

    Ok(format!("{host}:{port}"))
}

/// Whether `name` can name an environment variable at all: an empty name cannot, nor one holding the `=` that ends a name in `NAME=VALUE` or the NUL that ends a C string. Both hosts ask it, so such a name is absent from either.
pub(crate) fn names_a_variable(name: &[u8]) -> bool {
    !name.is_empty() && !name.contains(&b'=') && !name.contains(&0)
}

/// Where a child's `which` stream sits in the `[stdin, stdout, stderr]` both hosts file a child's streams in.
pub(crate) fn stream_index(which: ChildStream) -> usize {
    match which {
        ChildStream::Stdin => 0,
        ChildStream::Stdout => 1,
        ChildStream::Stderr => 2,
    }
}

/// Map an `io::Error` to its [`Failure`]. The named kinds map to named failures; anything else with an errno surfaces raw through `Other(errno)`. An errno-less failure (e.g. `write_all`'s synthesized `WriteZero`) is unclassifiable, so it reports the catch-all `Other(0)`; callers that can name it (e.g. `dns_resolve` → `NotFound`) map it at the call site.
pub(crate) fn failure_from_error(error: Error) -> Failure {
    match error.kind() {
        ErrorKind::NotFound => Failure::NotFound,
        ErrorKind::PermissionDenied => Failure::PermissionDenied,
        ErrorKind::AlreadyExists => Failure::AlreadyExists,
        ErrorKind::ConnectionRefused => Failure::ConnectionRefused,
        ErrorKind::WouldBlock => Failure::WouldBlock,
        ErrorKind::DirectoryNotEmpty => Failure::NotEmpty,
        ErrorKind::IsADirectory => Failure::IsDirectory,
        ErrorKind::NotADirectory => Failure::NotDirectory,
        _ => match error.raw_os_error() {
            Some(errno) => Failure::Other(errno as u32),
            None => Failure::Other(0),
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

/// Map the platform `revents` back to a [`Poll`] readiness mask, including the result-only `ERR`/`HUP` the kernel reports whether or not they were asked for. A descriptor the kernel calls invalid (`POLLNVAL`) reports `ERR`: its waiter wakes into the call that says why rather than waiting on a descriptor that cannot become ready.
pub(crate) fn poll_from_flags(flags: PollFlags) -> Poll {
    let mut bits = 0;

    if flags.contains(PollFlags::IN) {
        bits |= event::READ;
    }

    if flags.contains(PollFlags::OUT) {
        bits |= event::WRITE;
    }

    if flags.intersects(PollFlags::ERR | PollFlags::NVAL) {
        bits |= event::ERR;
    }

    if flags.contains(PollFlags::HUP) {
        bits |= event::HUP;
    }

    Poll::from_bits(bits)
}
