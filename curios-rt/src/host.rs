use {
    curios_abi::{mode, poll, status},
    num_bigint::BigUint,
    rustix::event::PollFlags,
    std::{
        io::{Error, ErrorKind},
        ops::{BitOr, BitOrAssign},
    },
};

/// A handle the guest shuttles across the IO boundary: one of the three standard
/// streams, or a host-minted token for an open file or socket. Mirrors the
/// guest's `/std/Io` handles; lifts from / lowers to its `Bin` wire token (the
/// opaque bytes a host mints — see [`bytes`](Self::bytes)).
#[derive(Clone)]
pub enum Io {
    Stdin,
    Stdout,
    Stderr,
    Other(Vec<u8>),
}

impl Io {
    /// The well-known stdio handle tokens minted by the `/sys/Io` prelude.
    pub const STDIN: u32 = 0;
    pub const STDOUT: u32 = 1;
    pub const STDERR: u32 = 2;
    /// The first handle token a host mints, one past the stdio tokens so a minted
    /// file or socket handle never collides with stdin/stdout/stderr; each host
    /// counts up from here with an unbounded `BigUint`.
    pub const HANDLE_SEED: u32 = Self::STDERR + 1;

    /// The canonical byte encoding of a token integer: its little-endian
    /// `BigUint` bytes. The single shared convention — the runtime mints and
    /// keys handles on it, and the `ersd → cont` lowering encodes the stdio
    /// constants `Io(0/1/2)` the same way — so the two ends cannot drift.
    pub fn encode(token: u32) -> Vec<u8> {
        BigUint::from(token).to_bytes_le()
    }

    /// The raw wire token bytes: the stdio encodings, or the minted handle.
    pub fn bytes(&self) -> Vec<u8> {
        match self {
            Io::Stdin => Self::encode(Self::STDIN),
            Io::Stdout => Self::encode(Self::STDOUT),
            Io::Stderr => Self::encode(Self::STDERR),
            Io::Other(bytes) => bytes.clone(),
        }
    }

    /// Lift wire token bytes back to a descriptor: the three stdio encodings map
    /// to the named streams, anything else is a host-minted handle. The inverse
    /// of [`bytes`](Self::bytes).
    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        if bytes == Self::encode(Self::STDIN) {
            Io::Stdin
        } else if bytes == Self::encode(Self::STDOUT) {
            Io::Stdout
        } else if bytes == Self::encode(Self::STDERR) {
            Io::Stderr
        } else {
            Io::Other(bytes)
        }
    }
}

/// A `poll` event mask — the interest a guest registers for a handle, and the
/// readiness the host reports back. The one bitfield in the IO design: a set of
/// flags riding a `u32`, mirroring the guest's per-handle `Nat`
/// mask, that the host maps to platform `POLLIN`/`POLLOUT`/… (whose raw values
/// differ per platform). `READ`/`WRITE` are settable interests; the host also
/// reports the result-only `ERR`/`HUP`, requested or not. Lifts from / lowers to
/// the raw `Nat` bits, exactly as [`Status`] does for its code.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Poll(u32);

impl Poll {
    pub const READ: Self = Self(poll::READ);
    pub const WRITE: Self = Self(poll::WRITE);
    pub const ERR: Self = Self(poll::ERR);
    pub const HUP: Self = Self(poll::HUP);

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

    /// Whether every flag in `other` is set — the per-interest readiness test.
    pub fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    /// Map this interest mask to the platform `poll` flags. Only `READ`/`WRITE`
    /// are settable interests; the result-only `ERR`/`HUP` are never requested.
    pub fn to_poll_flags(self) -> PollFlags {
        let mut flags = PollFlags::empty();

        if self.contains(Self::READ) {
            flags |= PollFlags::IN;
        }

        if self.contains(Self::WRITE) {
            flags |= PollFlags::OUT;
        }

        flags
    }

    /// Map the platform `revents` back to a readiness mask, including the
    /// result-only `ERR`/`HUP` the kernel reports whether or not they were asked
    /// for.
    pub fn from_poll_flags(flags: PollFlags) -> Self {
        let mut events = Self::empty();

        if flags.contains(PollFlags::IN) {
            events |= Self::READ;
        }

        if flags.contains(PollFlags::OUT) {
            events |= Self::WRITE;
        }

        if flags.contains(PollFlags::ERR) {
            events |= Self::ERR;
        }

        if flags.contains(PollFlags::HUP) {
            events |= Self::HUP;
        }

        events
    }
}

impl BitOr for Poll {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }
}

impl BitOrAssign for Poll {
    fn bitor_assign(&mut self, other: Self) {
        self.0 |= other.0;
    }
}

/// The status contract of failable IO ops, mirrored by `/std/File`'s `decode`.
/// Each named status has a fixed wire code; `Other` is the catch-all carrying
/// the OS errno of an otherwise-unrecognized failure, exactly like the guest's
/// `Error/other(Nat)`. `Status` lowers to that code.
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
    /// `Task/nonblocking` (`/std/Task.crs`); `/std`'s scheduler matches on it to
    /// reschedule the read/write instead of treating it as a real failure.
    WouldBlock,
    /// A TLS upgrade (`start_tls`/`start_tls_server`) or server-config build
    /// failed: an unparseable certificate/key, an invalid SNI, or a failed
    /// handshake (bad cert chain, protocol error). These are `rustls`'s own
    /// errors, not OS errnos, so they collapse to this one named code rather
    /// than passing through the `From<io::Error>` errno mapping.
    TlsError,
    /// An otherwise-unrecognized failure, carrying the OS errno that produced it.
    Other(u32),
}

impl Status {
    /// The wire code the guest decodes. The named statuses have fixed tags;
    /// `Other(code)` lowers its carried errno raw.
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
            Status::Other(code) => code,
        }
    }
}

impl From<Error> for Status {
    fn from(error: Error) -> Self {
        match error.kind() {
            ErrorKind::NotFound => Status::NotFound,
            ErrorKind::PermissionDenied => Status::PermissionDenied,
            ErrorKind::AlreadyExists => Status::AlreadyExists,
            ErrorKind::ConnectionRefused => Status::ConnectionRefused,
            ErrorKind::WouldBlock => Status::WouldBlock,
            // Unmapped but with an errno (a file/socket syscall): surface it raw
            // through the guest's `other(n)`. An errno-less failure (e.g.
            // `write_all`'s synthesized `WriteZero`) is unclassifiable here, so
            // report the catch-all `other(0)`; callers that can name it (e.g.
            // `resolve` → `NotFound`) map it at the call site.
            _ => match error.raw_os_error() {
                Some(errno) => Status::Other(errno as u32),
                None => Status::Other(0),
            },
        }
    }
}

/// The open mode of `/sys/Io/open`, mirrored by `/std/File`'s `Mode` inductive.
/// Lifts from its `0`/`1`/`2` tag; an out-of-range tag panics — `/std/File`
/// only ever marshals those three, so anything else is a codegen bug.
#[derive(Clone, Copy, PartialEq)]
pub enum Mode {
    Read,
    Write,
    Append,
}

impl Mode {
    /// The wire tag the guest marshals, mirrored by `/sys/Io/Mode`. The inverse
    /// of `Lift for Mode`.
    pub fn code(self) -> u32 {
        match self {
            Mode::Read => mode::READ,
            Mode::Write => mode::WRITE,
            Mode::Append => mode::APPEND,
        }
    }
}

pub trait Host {
    /// Open the file at `path` in `mode`. Returns `(status, io)`; the handle is
    /// meaningful only when the status is `Status::Ok`.
    fn open(&self, path: &[u8], mode: Mode) -> (Status, Io);

    /// Start an asynchronous lookup of `host`:`port`. Returns `(status, handle)`;
    /// on `Status::Ok` the handle is poll-readable and becomes ready for `READ`
    /// once the lookup completes, at which point `resolve` forces the address
    /// list off it. The blocking name resolution runs off the calling thread, so
    /// a single-threaded scheduler can poll the handle instead of blocking.
    fn lookup(&self, host: &[u8], port: u32) -> (Status, Io);

    /// Force a finished lookup `handle` to its list of opaque address blobs the
    /// socket lifecycle consumes, consuming the handle. Returns
    /// `(status, addresses)`; each blob is the host's private encoding (canonical
    /// address string here) the guest only shuttles back into
    /// `socket`/`bind`/`connect`. On `Status::Ok` the list is non-empty. Called
    /// before readiness, it reports `WouldBlock`.
    fn resolve(&self, handle: Io) -> (Status, Vec<Vec<u8>>);

    /// Create an unconnected socket for the address family encoded in `addr`.
    /// Returns `(status, io)` like `open`; the handle is configured via the
    /// setters, then `bind`/`connect`/`listen` transition it.
    fn socket(&self, addr: &[u8]) -> (Status, Io);

    /// Bind socket `io` to the local address `addr`.
    fn bind(&self, io: Io, addr: &[u8]) -> Status;

    /// Connect socket `io` to the resolved address `addr`. On `Status::Ok` the
    /// handle is an ordinary byte stream `read`/`write`/`close` serve.
    fn connect(&self, io: Io, addr: &[u8]) -> Status;

    /// Upgrade the connected socket `io` to a TLS client stream in place,
    /// running the handshake inline. `sni` is the server name to present and
    /// verify against (derived from the connect hostname); the client trusts a
    /// bundled root set, verification always on. On `Status::Ok` the same handle
    /// is now an encrypted byte stream the existing `read`/`write`/`close` serve;
    /// on failure the connection is dropped and `Status::TlsError` is reported.
    fn start_tls(&self, io: Io, sni: &[u8]) -> Status;

    /// Build an opaque server-side TLS configuration from a PEM certificate
    /// chain and private key. Returns `(status, io)` like `socket`: the handle
    /// is a host-owned config token (no socket), consumed by `start_tls_server`
    /// and released by `close`. Certs/keys cross as opaque `Bin`, parsed here.
    fn tls_server_config(&self, cert: &[u8], key: &[u8]) -> (Status, Io);

    /// Upgrade the accepted socket `io` to a TLS server stream in place using
    /// the configuration handle `cfg` (from `tls_server_config`), running the
    /// handshake inline. Same in-place conduit swap as `start_tls`.
    fn start_tls_server(&self, io: Io, cfg: Io) -> Status;

    /// Mark bound socket `io` as listening with accept-queue depth `backlog`
    /// (OS-clamped to `somaxconn`); `accept` then pulls connections and `close`
    /// releases it.
    fn listen(&self, io: Io, backlog: u32) -> Status;

    /// Pull the next connection from the listener `io`, blocking until one
    /// arrives. Returns `(status, io)`; the connection handle is an ordinary
    /// byte stream the same `read`/`write`/`close` serve, like a `connect`ed one.
    fn accept(&self, io: Io) -> (Status, Io);

    /// Set socket `io`'s non-blocking flag. A no-op on a file handle
    /// (recorded, not enforced).
    fn set_nonblocking(&self, io: Io, on: u32) -> Status;

    /// Set socket `io`'s receive timeout to `ms` milliseconds (`0` clears).
    fn set_recv_timeout(&self, io: Io, ms: u32) -> Status;

    /// Set socket `io`'s send timeout to `ms` milliseconds (`0` clears).
    fn set_send_timeout(&self, io: Io, ms: u32) -> Status;

    /// Set socket `io`'s `SO_REUSEADDR` flag; set before `bind`.
    fn set_reuseaddr(&self, io: Io, on: u32) -> Status;

    /// The readiness oracle. Wait until at least one of `handles` is ready for
    /// the interest in the parallel `events` mask (`Poll::READ`/`WRITE`),
    /// or `timeout_ms` elapses. `timeout_ms` follows `poll(2)`'s sign convention:
    /// negative waits forever, `0` returns immediately, positive waits that many
    /// milliseconds. Returns the parallel `revents` masks, one per handle; a
    /// handle the host does not recognize reports `Poll::empty()`.
    fn poll(&self, handles: &[Io], events: &[Poll], timeout_ms: i32) -> Vec<Poll>;

    /// Close `io`. Closing an unknown handle is a no-op.
    fn close(&self, io: Io);

    /// Read up to `count` bytes from `io`, blocking until at least one byte is
    /// available. Returns `(status, bytes)`: `Status::Ok` with 1..count bytes,
    /// `Status::Eof` with empty bytes, or an error status.
    fn read(&self, io: Io, count: u32) -> (Status, Vec<u8>);

    /// Write `bytes` to `io`, returning `(status, written)`: the number of bytes
    /// actually accepted this call. On a non-blocking handle a single `write`
    /// may take only a prefix before the kernel buffer fills — `written` is that
    /// prefix length so the caller can advance and resend only the unwritten
    /// tail, never duplicating bytes. `Status::WouldBlock` reports `written` 0
    /// (nothing moved); the blocking std streams always report the full length.
    fn write(&self, io: Io, bytes: &[u8]) -> (Status, u32);

    /// Read the wall clock. Returns `(secs_hi, secs_lo, nanos)`: seconds since
    /// the Unix epoch split base-10⁹ so each limb fits an i31, plus sub-second
    /// nanoseconds.
    fn clock_wall(&self) -> (u32, u32, u32);

    /// Read the monotonic clock. Returns `(secs, nanos)` elapsed since a fixed
    /// origin (host construction); only differences are meaningful.
    fn clock_mono(&self) -> (u32, u32);

    /// Return `count` random bytes.
    fn random(&self, count: u32) -> Vec<u8>;

    /// The process arguments, each an opaque byte string.
    fn args(&self) -> Vec<Vec<u8>>;

    /// Look up the environment variable `name`. Returns `(status, value)`:
    /// `Status::Ok` with the value, or `Status::NotFound` with empty bytes.
    fn env(&self, name: &[u8]) -> (Status, Vec<u8>);
}
