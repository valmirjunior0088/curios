//! The semantic Rust types a builtin host operation speaks in — the pure
//! halves, free of any native-platform dependency, that the
//! [`HostOps`](super::HostOps) trait's signatures reference and every host
//! adapter shares.
//!
//! Each mirrors a guest-side notion and lifts from / lowers to its wire shape:
//! a [`Handle`] is its token bytes (a `Bin`), a [`Status`]/[`Poll`] its raw
//! `Nat` code, a [`Mode`] its `0`/`1`/`2` tag. The native adapter's own
//! concerns — mapping an `io::Error` to a `Status`, a `Poll` mask to platform
//! `poll` flags — live with the adapter (`curios-runtime`), not here.

use {crate::stdio, num_bigint::BigUint};

/// A handle the guest shuttles across the host boundary: one of the three
/// standard streams, or a host-minted token for an open file, socket, TLS
/// config, or lookup. Mirrors the guest's `/sys/Handle` values; lifts from /
/// lowers to its `Bin` wire token (the opaque bytes a host mints — see
/// [`bytes`](Self::bytes)).
#[derive(Clone)]
pub enum Handle {
    Stdin,
    Stdout,
    Stderr,
    Other(Vec<u8>),
}

impl Handle {
    /// The well-known stdin handle token minted by the `/sys` prelude.
    const STDIN: u32 = stdio::STDIN;
    /// The well-known stdout handle token.
    const STDOUT: u32 = stdio::STDOUT;
    /// The well-known stderr handle token, the last before [`HANDLE_SEED`](Self::HANDLE_SEED).
    const STDERR: u32 = stdio::STDERR;
    /// The first handle token a host mints, one past the stdio tokens so a minted
    /// file or socket handle never collides with stdin/stdout/stderr; each host
    /// counts up from here with an unbounded `BigUint`.
    pub const HANDLE_SEED: u32 = Self::STDERR + 1;

    /// The canonical byte encoding of a token integer: its little-endian
    /// `BigUint` bytes. The single shared convention — the runtime mints and
    /// keys handles on it, and the `ersd → cont` lowering encodes the stdio
    /// constants `Handle(0/1/2)` the same way — so the two ends cannot drift.
    fn encode(token: u32) -> Vec<u8> {
        BigUint::from(token).to_bytes_le()
    }

    /// The raw wire token bytes: the stdio encodings, or the minted handle.
    pub fn bytes(&self) -> Vec<u8> {
        match self {
            Handle::Stdin => Self::encode(Self::STDIN),
            Handle::Stdout => Self::encode(Self::STDOUT),
            Handle::Stderr => Self::encode(Self::STDERR),
            Handle::Other(bytes) => bytes.clone(),
        }
    }

    /// Lift wire token bytes back to a descriptor: the three stdio encodings map
    /// to the named streams, anything else is a host-minted handle. The inverse
    /// of [`bytes`](Self::bytes).
    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        if bytes == Self::encode(Self::STDIN) {
            Handle::Stdin
        } else if bytes == Self::encode(Self::STDOUT) {
            Handle::Stdout
        } else if bytes == Self::encode(Self::STDERR) {
            Handle::Stderr
        } else {
            Handle::Other(bytes)
        }
    }
}

/// A `poll` event mask — the interest a guest registers for a handle, and the
/// readiness the host reports back. The one bitfield in the host design: a set
/// of flags riding a `u32`, mirroring the guest's per-handle `Nat` mask. Lifts
/// from / lowers to the raw `Nat` bits, exactly as [`Status`] does for its
/// code. The mapping to platform `POLLIN`/`POLLOUT`/… (whose raw values differ
/// per platform) is the native adapter's concern.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Poll(u32);

impl Poll {
    /// The empty mask — no interest, or no readiness.
    pub const fn empty() -> Self {
        Self(0)
    }

    /// Lift the raw `Nat` bits the guest marshals into a mask.
    pub fn from_bits(bits: u32) -> Self {
        Self(bits)
    }

    /// The raw bits, to lower back to the guest's `Nat`.
    pub fn bits(self) -> u32 {
        self.0
    }
}

/// The status contract of failable host ops, mirrored by `/std/File`'s
/// `decode`. Each named status has a fixed wire code; `Other` is the catch-all
/// carrying the OS errno of an otherwise-unrecognized failure, exactly like the
/// guest's `Error/other(Nat)`. [`Status`] lowers to that code. The native
/// adapter maps an `io::Error` to one of these (`curios-runtime`).
#[derive(Clone, Copy)]
pub enum Status {
    Ok,
    Eof,
    NotFound,
    PermissionDenied,
    AlreadyExists,
    /// A `connect` was actively refused — no listener at the target host:port.
    ConnectionRefused,
    /// A non-blocking op could not make progress (`ErrorKind::WouldBlock`).
    /// Produced once a handle has been switched to non-blocking mode by
    /// `Async/nonblocking` (`/std/Async.crs`); `/std`'s scheduler matches on it to
    /// reschedule the read/write instead of treating it as a real failure.
    WouldBlock,
    /// A TLS upgrade (`start_tls`/`start_tls_server`) or server-config build
    /// failed: an unparseable certificate/key, an invalid SNI, or a failed
    /// handshake (bad cert chain, protocol error). These are `rustls`'s own
    /// errors, not OS errnos, so they collapse to this one named code rather
    /// than passing through the errno mapping.
    TlsError,
    /// An otherwise-unrecognized failure, carrying the OS errno that produced it.
    Other(u32),
}

impl Status {
    /// The wire code the guest decodes. The named statuses have fixed tags;
    /// `Other(code)` lowers its carried errno raw.
    pub fn code(self) -> u32 {
        match self {
            Status::Ok => crate::status::OK,
            Status::Eof => crate::status::EOF,
            Status::NotFound => crate::status::NOT_FOUND,
            Status::PermissionDenied => crate::status::PERMISSION_DENIED,
            Status::AlreadyExists => crate::status::ALREADY_EXISTS,
            Status::ConnectionRefused => crate::status::CONNECTION_REFUSED,
            Status::WouldBlock => crate::status::WOULD_BLOCK,
            Status::TlsError => crate::status::TLS_ERROR,
            Status::Other(code) => code,
        }
    }
}

/// The open mode of `/sys/open`, mirrored by `/std/File`'s `Mode` inductive.
/// Lifts from its `0`/`1`/`2` tag; an out-of-range tag panics — `/std/File`
/// only ever marshals those three, so anything else is a codegen bug.
#[derive(Clone, Copy, PartialEq)]
pub enum Mode {
    Read,
    Write,
    Append,
}
