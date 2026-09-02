//! The numeric wire codes for `/sys/Handle`'s status, poll-event, open-mode, file-kind, stdio-wiring, and stdio-handle tags, each module named by the tag it holds. Each set is mirrored by a guest-side `/sys` module of the same name; the runtime cites these constants when it lowers a `Status`/`Poll`/`Mode` to the wire, and both ends cite [`stdio`] for the well-known handle tokens.

/// Status codes of failable IO ops, mirrored by the guest's `/sys/status` and decoded into `/std/Handle/Error`. `Other` has no fixed code here: it lowers its carried errno offset by `OTHER_BASE`, keeping the errno lane disjoint from the named codes.
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
    /// A `dir/remove` on a directory that still has entries (`ErrorKind::DirectoryNotEmpty`). Named, with the two below, because a program removing a directory has to tell them apart portably and the browser has no errno to pass through.
    pub const NOT_EMPTY: u32 = 8;
    /// A file operation applied to a directory (`ErrorKind::IsADirectory`).
    pub const IS_DIRECTORY: u32 = 9;
    /// A directory operation applied to something that is not one (`ErrorKind::NotADirectory`).
    pub const NOT_DIRECTORY: u32 = 10;
    /// The errno passthrough lane: `Status::Other(errno)` lowers as `OTHER_BASE + errno`, one past the last named code, so a raw OS errno — EIO is 5, ENXIO is 6 — can never masquerade as `OK` or a named failure. The guest's `error_of` subtracts it back out.
    pub const OTHER_BASE: u32 = NOT_DIRECTORY + 1;
}

/// `poll` interest/readiness flags — a bitmask, mirrored by `/sys/event`. `READ`/`WRITE` are settable interests; `ERR`/`HUP` are result-only.
pub mod event {
    /// The handle is (or should be watched to become) readable.
    pub const READ: u32 = 0b0001;
    /// The handle is (or should be watched to become) writable.
    pub const WRITE: u32 = 0b0010;
    /// The handle is in an error state. Result-only: reported even when never requested.
    pub const ERR: u32 = 0b0100;
    /// The peer hung up. Result-only, like `ERR`.
    pub const HUP: u32 = 0b1000;
}

/// `open` modes, mirrored by `/sys/open_mode` and the guest's `/std/File/Mode`.
pub mod open_mode {
    /// Open an existing file read-only.
    pub const READ: u32 = 0;
    /// Open for writing: created if absent, truncated if present.
    pub const WRITE: u32 = 1;
    /// Open for appending: created if absent, every write lands at the end.
    pub const APPEND: u32 = 2;
}

/// What `file/stat` found at a path, mirrored by `/sys/file_kind` and the guest's `/std/fs/Kind`. `stat` follows symbolic links, so `SYMLINK` is reported only where the link's target is missing.
pub mod file_kind {
    /// A regular file.
    pub const FILE: u32 = 0;
    /// A directory.
    pub const DIRECTORY: u32 = 1;
    /// A symbolic link whose target is missing — the one case following the link finds nothing to report.
    pub const SYMLINK: u32 = 2;
    /// Anything else: a device, a socket, a pipe.
    pub const OTHER: u32 = 3;
}

/// How `proc/spawn` wires each of a child's standard streams, mirrored by `/sys/stdio_mode` and the guest's `/std/proc/Stdio`: the shape Lean's `Stdio`, Haskell's `StdStream`, Rust's `Stdio` and Zig's `StdIo` share.
pub mod stdio_mode {
    /// The child shares the parent's stream.
    pub const INHERIT: u32 = 0;
    /// The stream is a pipe the parent holds the other end of, as a handle.
    pub const PIPE: u32 = 1;
    /// The stream is attached to the null device.
    pub const NULL: u32 = 2;
}

/// The well-known stdio handle tokens minted by the `/sys/Handle` prelude. A handle's wire encoding is the little-endian `Natural` bytes of its token (see `Handle::encode`), which mints one zero byte for zero — so STDIN encodes as `[0]`, never the empty byte string.
pub mod stdio {
    /// Standard input — the token whose wire encoding is the single zero byte `[0]`.
    pub const STDIN: u32 = 0;
    /// Standard output.
    pub const STDOUT: u32 = 1;
    /// Standard error.
    pub const STDERR: u32 = 2;
}
