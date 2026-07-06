//! The numeric wire codes for `/sys/Io`'s status, poll-event, open-mode, and
//! stdio-handle tags. Each set is mirrored by a guest-side declaration; the
//! runtime cites these constants when it lowers a `Status`/`Poll`/`Mode` to
//! the wire, and both ends cite [`stdio`] for the well-known handle tokens.

/// Status codes of failable IO ops, mirrored by the guest's `/sys/Io/Status` and
/// decoded into `/std/Io/Error`. `Other` carries an OS errno raw, so it has no
/// fixed code here.
pub mod status {
    /// The op succeeded — the reply's payload fields are meaningful only under this code.
    pub const OK: u32 = 0;
    /// A `read` reached the end of the stream and returned no bytes. Terminal but not a fault — the guest's stream consumers stop on it rather than erroring.
    pub const EOF: u32 = 1;
    /// The named thing does not exist: an `open` path, an `env` variable, or a `resolve` that yielded no addresses (mapped at that call site, since the OS reports it errno-less).
    pub const NOT_FOUND: u32 = 2;
    /// The OS denied access to the path or socket op (`ErrorKind::PermissionDenied`).
    pub const PERMISSION_DENIED: u32 = 3;
    /// The target the op would create already exists (`ErrorKind::AlreadyExists`).
    pub const ALREADY_EXISTS: u32 = 4;
    /// A `connect` was actively refused — no listener at the target address.
    pub const CONNECTION_REFUSED: u32 = 5;
    /// A non-blocking op could not make progress right now. Retriable by design: `/std`'s task scheduler matches on it to reschedule the read/write instead of surfacing a failure.
    pub const WOULD_BLOCK: u32 = 6;
    /// A TLS upgrade or server-config build failed. `rustls` errors carry no OS errno, so they collapse to this one named code instead of riding the errno passthrough.
    pub const TLS_ERROR: u32 = 7;
}

/// `poll` interest/readiness flags — a bitmask, mirrored by `/sys/Io/Poll`.
/// `READ`/`WRITE` are settable interests; `ERR`/`HUP` are result-only.
pub mod poll {
    /// The handle is (or should be watched to become) readable.
    pub const READ: u32 = 0b0001;
    /// The handle is (or should be watched to become) writable.
    pub const WRITE: u32 = 0b0010;
    /// The handle is in an error state. Result-only: reported even when never requested.
    pub const ERR: u32 = 0b0100;
    /// The peer hung up. Result-only, like `ERR`.
    pub const HUP: u32 = 0b1000;
}

/// `open` modes, mirrored by `/sys/Io/Mode` and the guest's `/std/Io/Mode`.
pub mod mode {
    /// Open an existing file read-only.
    pub const READ: u32 = 0;
    /// Open for writing: created if absent, truncated if present.
    pub const WRITE: u32 = 1;
    /// Open for appending: created if absent, every write lands at the end.
    pub const APPEND: u32 = 2;
}

/// The well-known stdio handle tokens minted by the `/sys/Io` prelude. A
/// handle's wire encoding is the little-endian bytes of its token (see
/// curios-rt's `Io::encode`), so STDIN encodes to the empty byte string.
pub mod stdio {
    /// Standard input — the token whose wire encoding is the empty byte string.
    pub const STDIN: u32 = 0;
    /// Standard output.
    pub const STDOUT: u32 = 1;
    /// Standard error.
    pub const STDERR: u32 = 2;
}
