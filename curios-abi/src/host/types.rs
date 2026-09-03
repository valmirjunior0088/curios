//! The semantic Rust types a builtin host operation speaks in — the pure halves, free of any native-platform dependency, that the [`HostOps`](super::HostOps) trait's signatures reference and every host adapter shares.
//!
//! Each mirrors a guest-side notion at its wire shape: a [`Handle`] is its token bytes (a `Bytes`), a [`Status`]/[`Poll`] its raw `Nat` code, a [`Mode`] its `0`/`1`/`2` tag. [`Handle`] and [`Poll`] lift from and lower to that shape here; [`Status`] only lowers, since a host produces one and never reads one back; and [`Mode`] is lifted by the adapter that reads the tag off the wire. The native adapter's own concerns — mapping an `io::Error` to a `Status`, a `Poll` mask to platform `poll` flags — live with the adapter (`curios-runtime`), not here.

use {
    crate::{status, stdio},
    curios_num::Natural,
};

/// A handle the guest shuttles across the host boundary: one of the three standard streams, or a host-minted token for an open file, socket, TLS config, or lookup. Mirrors the guest's `/sys/Handle` values; lifts from / lowers to its `Bytes` wire token (the opaque bytes a host mints — see [`bytes`](Self::bytes)).
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
    /// The first handle token a host mints, one past the stdio tokens so a minted file or socket handle never collides with stdin/stdout/stderr; each host counts up from here with a [`TokenMint`].
    pub const HANDLE_SEED: u32 = Self::STDERR + 1;

    /// The canonical byte encoding of a token: its little-endian [`Natural`] bytes — one zero byte for zero, never the empty string, and never padded to a fixed width. This is the one place the convention is spelled: [`TokenMint`] mints through it, hosts key their tables on the bytes it returns, and the `ersd → cont` lowering calls it for the stdio constants `Handle(0/1/2)` — so the three ends cannot drift.
    pub fn encode(token: &Natural) -> Vec<u8> {
        token.to_bytes_le()
    }

    /// No handle: the empty token, which no host mints. It is what a `(status, handle)` row hands back beside a status other than `Ok`, and what `proc/stream` answers for a stream that was not piped — the guest never inspects it, and `close` on it is the no-op closing any unknown handle is.
    pub fn none() -> Self {
        Handle::Other(Vec::new())
    }

    /// Whether this is [`none`](Self::none) — the empty token rather than a stream or a minted handle.
    pub fn is_none(&self) -> bool {
        matches!(self, Handle::Other(bytes) if bytes.is_empty())
    }

    /// The raw wire token bytes: the stdio encodings, or the minted handle.
    pub fn bytes(&self) -> Vec<u8> {
        match self {
            Handle::Stdin => Self::encode(&Natural::from(Self::STDIN)),
            Handle::Stdout => Self::encode(&Natural::from(Self::STDOUT)),
            Handle::Stderr => Self::encode(&Natural::from(Self::STDERR)),
            Handle::Other(bytes) => bytes.clone(),
        }
    }

    /// Lift wire token bytes back to a descriptor: the three stdio encodings map to the named streams, anything else is a host-minted handle. The inverse of [`bytes`](Self::bytes).
    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        if bytes == Self::encode(&Natural::from(Self::STDIN)) {
            Handle::Stdin
        } else if bytes == Self::encode(&Natural::from(Self::STDOUT)) {
            Handle::Stdout
        } else if bytes == Self::encode(&Natural::from(Self::STDERR)) {
            Handle::Stderr
        } else {
            Handle::Other(bytes)
        }
    }
}

/// The monotonic source a host mints handle tokens from, seeded one past the stdio band so a minted token never collides with stdin/stdout/stderr.
///
/// It lives beside [`Handle`] rather than in the host that drives it because the token encoding is wire contract, not host policy: [`Handle::bytes`] reads back the same little-endian convention this writes. A host owns *when* to mint and what to file under the result; it does not get its own opinion about what a token looks like.
///
/// The counter is unbounded and never wraps, so a token is never reused. A closed handle's bytes are removed from the host's table and never minted again, which is what makes use-after-close a loud miss rather than a silent alias onto a later resource.
pub struct TokenMint {
    next: Natural,
}

impl TokenMint {
    /// A mint seeded at [`Handle::HANDLE_SEED`], having issued nothing.
    pub fn new() -> Self {
        Self {
            next: Natural::from(Handle::HANDLE_SEED),
        }
    }

    /// The next token's canonical bytes, advancing the counter past it so no later call can reproduce them.
    pub fn mint(&mut self) -> Vec<u8> {
        let bytes = Handle::encode(&self.next);
        self.next = &self.next + Natural::one();

        bytes
    }
}

impl Default for TokenMint {
    fn default() -> Self {
        Self::new()
    }
}

/// A `poll` event mask — the interest a guest registers for a handle, and the readiness the host reports back. The one bitfield in the host design: a set of flags riding a `u32`, mirroring the guest's per-handle `Nat` mask. Lifts from / lowers to the raw `Nat` bits; [`Status`] lowers to its code the same way. The mapping to platform `POLLIN`/`POLLOUT`/… (whose raw values differ per platform) is the native adapter's concern.
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

/// The status contract of failable host ops, mirrored by `/std/Io/Error`'s `error_of`. Each named status has a fixed wire code; `Other` is the catch-all carrying the OS errno of an otherwise-unrecognized failure, exactly like the guest's `Error/other(Nat)`, and lowers offset by [`OTHER_BASE`](status::OTHER_BASE) so an errno can never collide with a named code. The native adapter maps an `io::Error` to one of these (`curios-runtime`).
#[derive(Clone, Copy)]
pub enum Status {
    Ok,
    Eof,
    NotFound,
    PermissionDenied,
    AlreadyExists,
    /// A `connect` was actively refused — no listener at the target host:port.
    ConnectionRefused,
    /// A non-blocking op could not make progress (`ErrorKind::WouldBlock`). Every handle a peer decides on is non-blocking from the moment the host mints it, so this is the status a fiber parks on: `/std`'s scheduler matches on it to reschedule the read/write instead of treating it as a real failure.
    WouldBlock,
    /// A TLS upgrade (`start_tls`/`start_tls_server`) or server-config build failed: an unparseable certificate/key, an invalid SNI, or a failed handshake (bad cert chain, protocol error). These are `rustls`'s own errors, not OS errnos, so they collapse to this one named code rather than passing through the errno mapping.
    TlsError,
    /// A directory removal refused because the directory still has entries (`ErrorKind::DirectoryNotEmpty`).
    NotEmpty,
    /// A file operation applied to a directory (`ErrorKind::IsADirectory`).
    IsDirectory,
    /// A directory operation applied to something that is not one (`ErrorKind::NotADirectory`).
    NotDirectory,
    /// An otherwise-unrecognized failure, carrying the OS errno that produced it.
    Other(u32),
}

impl Status {
    /// The wire code the guest decodes. The named statuses have fixed tags; `Other(errno)` lowers as [`OTHER_BASE`](status::OTHER_BASE) plus its carried errno, keeping the errno lane disjoint from the named tags.
    pub fn code(self) -> u32 {
        match self {
            Status::Ok => status::OK,
            Status::Eof => status::EOF,
            Status::NotFound => status::NOT_FOUND,
            Status::PermissionDenied => status::PERMISSION_DENIED,
            Status::AlreadyExists => status::ALREADY_EXISTS,
            Status::ConnectionRefused => status::CONNECTION_REFUSED,
            Status::WouldBlock => status::WOULD_BLOCK,
            Status::TlsError => status::TLS_ERROR,
            Status::NotEmpty => status::NOT_EMPTY,
            Status::IsDirectory => status::IS_DIRECTORY,
            Status::NotDirectory => status::NOT_DIRECTORY,
            Status::Other(errno) => status::OTHER_BASE + errno,
        }
    }
}

/// The open mode of `/sys/file/open`, mirrored by `/std/File`'s `Mode` inductive. Its `0`/`1`/`2` tag is [`open_mode`](crate::open_mode)'s; lifting a tag is the native adapter's (`curios-runtime`'s `Lift`), which is where an out-of-range one is refused.
#[derive(Clone, Copy, PartialEq)]
pub enum Mode {
    Read,
    Write,
    Append,
}
