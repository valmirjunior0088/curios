//! The numeric wire codes for `/sys/Io`'s status, poll-event, and open-mode
//! tags. Each set is mirrored by a guest-side declaration; the runtime cites
//! these constants when it lowers a `Status`/`Poll`/`Mode` to the wire.

/// Status codes of failable IO ops, mirrored by the guest's `/sys/Io/Status` and
/// decoded into `/std/Io/Error`. `Other` carries an OS errno raw, so it has no
/// fixed code here.
pub mod status {
    pub const OK: u32 = 0;
    pub const EOF: u32 = 1;
    pub const NOT_FOUND: u32 = 2;
    pub const PERMISSION_DENIED: u32 = 3;
    pub const ALREADY_EXISTS: u32 = 4;
    pub const CONNECTION_REFUSED: u32 = 5;
    pub const WOULD_BLOCK: u32 = 6;
    pub const TLS_ERROR: u32 = 7;
}

/// `poll` interest/readiness flags — a bitmask, mirrored by `/sys/Io/Poll`.
/// `READ`/`WRITE` are settable interests; `ERR`/`HUP` are result-only.
pub mod poll {
    pub const READ: u32 = 0b0001;
    pub const WRITE: u32 = 0b0010;
    pub const ERR: u32 = 0b0100;
    pub const HUP: u32 = 0b1000;
}

/// `open` modes, mirrored by `/sys/Io/Mode` and the guest's `/std/Io/Mode`.
pub mod mode {
    pub const READ: u32 = 0;
    pub const WRITE: u32 = 1;
    pub const APPEND: u32 = 2;
}
