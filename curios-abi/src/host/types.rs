//! The semantic Rust types a builtin host operation speaks in — the pure halves, free of any native-platform dependency, that the [`HostOps`](super::HostOps) trait's signatures reference and every host adapter shares.
//!
//! [`Termination`] is how a diverging row answers, and [`Failure`] how a fallible one says why it failed. The rest mirror guest-side notions: a [`Handle`] is its token bytes (a `Bytes`), a [`Poll`] a byte of flags, a [`Mode`] its `0`/`1`/`2` tag, and [`Timestamp`], [`TtySize`], [`FileStat`] and [`ChildExit`] the payloads whose several fields the guest projects by label. How each crosses the wire is its [`WireOperand`](super::WireOperand), [`WirePayload`](super::WirePayload) or [`WireReply`](super::WireReply) impl's to say; the native adapter's own concerns — mapping an `io::Error` to a `Failure`, a `Poll` mask to platform `poll` flags — live with the adapter (`curios-runtime`), not here.

use {
    crate::{file_kind, status, stdio},
    curios_num::Natural,
    std::num::NonZeroU32,
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

    /// No handle: the empty token, which no host mints. It is what a `(status, handle)` row hands back beside a status other than `Ok`, and what `proc/stream` answers for a stream that was not piped — the guest never inspects it, and `handle_close` on it is the no-op closing any unknown handle is.
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

/// A `handle_poll` event mask — the interest a guest registers for a handle, and the readiness the host reports back. The one bitfield in the host design: a set of flags riding a byte, mirroring the guest's per-handle `Byte`, so the masks for a call cross as one `Bytes` with a byte per handle. The mapping to platform `POLLIN`/`POLLOUT`/… (whose raw values differ per platform) is the native adapter's concern.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Poll(u8);

impl Poll {
    /// The empty mask — no interest, or no readiness.
    pub const fn empty() -> Self {
        Self(0)
    }

    /// The mask a byte of the guest's `Bytes` holds.
    pub fn from_bits(bits: u8) -> Self {
        Self(bits)
    }

    /// The raw bits, one byte of the `Bytes` lowered back to the guest.
    pub fn bits(self) -> u8 {
        self.0
    }
}

/// Why a fallible host operation failed, mirrored by `/std/Io/Error`'s `of`. It names failures only: success and a stream's end are the shape of the reply that carries it ([`WireReply`](super::WireReply)), so no host can answer a success beside a failure's padding. Each named failure has a fixed wire code; `Other` is the catch-all carrying the OS errno of an otherwise-unrecognized failure, exactly like the guest's `Error/other(Nat)`, and lowers offset by [`OTHER_BASE`](status::OTHER_BASE) so an errno can never collide with a named code. The native adapter maps an `io::Error` to one of these (`curios-runtime`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Failure {
    NotFound,
    PermissionDenied,
    AlreadyExists,
    /// A `socket_connect` was actively refused — no listener at the target host:port.
    ConnectionRefused,
    /// A non-blocking op could not make progress (`ErrorKind::WouldBlock`). Every handle a peer decides on is non-blocking from the moment the host mints it, so this is the failure a fiber parks on: `/std`'s scheduler matches on it to reschedule the read/write instead of treating it as a real failure.
    WouldBlock,
    /// A TLS upgrade (`tls_start`/`tls_start_server`) or server-config build failed: an unparseable certificate/key, an invalid SNI, or a failed handshake (bad cert chain, protocol error). These are `rustls`'s own errors, not OS errnos, so they collapse to this one named code rather than passing through the errno mapping.
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

impl Failure {
    /// The wire code the guest decodes. The named failures have fixed tags; `Other(errno)` lowers as [`OTHER_BASE`](status::OTHER_BASE) plus its carried errno, keeping the errno lane disjoint from the named tags.
    pub fn code(self) -> u64 {
        match self {
            Failure::NotFound => status::NOT_FOUND,
            Failure::PermissionDenied => status::PERMISSION_DENIED,
            Failure::AlreadyExists => status::ALREADY_EXISTS,
            Failure::ConnectionRefused => status::CONNECTION_REFUSED,
            Failure::WouldBlock => status::WOULD_BLOCK,
            Failure::TlsError => status::TLS_ERROR,
            Failure::NotEmpty => status::NOT_EMPTY,
            Failure::IsDirectory => status::IS_DIRECTORY,
            Failure::NotDirectory => status::NOT_DIRECTORY,
            Failure::Other(errno) => status::OTHER_BASE + u64::from(errno),
        }
    }
}

/// A clock reading, as `clock_wall` and `clock_mono` answer it: whole seconds, and the nanoseconds within the second.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Timestamp {
    pub secs: u64,
    pub nanos: u64,
}

/// A terminal's dimensions, as `tty_size` answers them.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TtySize {
    pub cols: u64,
    pub rows: u64,
}

/// What `file_stat` found at a path, its kind one of the closed [`file_kind`] codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileKind {
    File,
    Directory,
    /// A symbolic link whose target is missing; a link that resolves reports what it resolves to.
    Symlink,
    Other,
}

impl FileKind {
    /// The [`file_kind`] code the guest decodes.
    pub fn code(self) -> u64 {
        match self {
            FileKind::File => file_kind::FILE,
            FileKind::Directory => file_kind::DIRECTORY,
            FileKind::Symlink => file_kind::SYMLINK,
            FileKind::Other => file_kind::OTHER,
        }
    }
}

/// What `file_stat` answers: the kind, the size in bytes, and the modification time as `clock_wall` reads the clock.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileStat {
    pub kind: FileKind,
    pub size: u64,
    pub mtime_secs: u64,
    pub mtime_nanos: u64,
}

/// How a child ended: the exit code it returned, or the signal that ended it — never both, which is what `proc_wait`'s pair of fields cannot say by itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChildExit {
    Code(u8),
    Signal(NonZeroU32),
}

/// The open mode of `/sys/file/open`, mirrored by `/std/File`'s `Mode` inductive. Its `0`/`1`/`2` tag is [`open_mode`](crate::open_mode)'s; lifting a tag is the native adapter's (`curios-runtime`'s `Lift`), which is where an out-of-range one is refused.
#[derive(Clone, Copy, PartialEq)]
pub enum Mode {
    Read,
    Write,
    Append,
}

/// How a diverging row ends the instance: the code `proc/exit` hands the embedder, which the adapter carries out as its guest-exit trap. A host method that answers one cannot return into the guest, because nothing but the trap is made of it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Termination(pub u8);
