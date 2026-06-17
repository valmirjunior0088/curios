use {crate::Entropy, std::sync::Mutex};

/// Number → string conversions used by both the wasm runtime (via the
/// `nat_to_str`/`int_to_str`/`flt_to_str` imports) and the `scalar_eval`
/// compile-time folder. Free functions, not trait methods, so the
/// compile-time and runtime conversions cannot diverge.
pub fn nat_to_str(value: u32) -> Vec<u8> {
    format!("{value}").into_bytes()
}

pub fn int_to_str(value: i32) -> Vec<u8> {
    format!("{value:+}").into_bytes()
}

pub fn flt_to_str(value: f32) -> Vec<u8> {
    format!("{value:+}").into_bytes()
}

pub fn flt_to_le_bin(value: f32) -> Vec<u8> {
    value.to_le_bytes().to_vec()
}

/// The well-known handle tokens minted by the `/sys/Io` prelude constants.
pub const STDIN: u32 = 0;
pub const STDOUT: u32 = 1;
pub const STDERR: u32 = 2;

/// The status contract of failable IO ops, mirrored by `/std/File`'s `decode`.
pub const STATUS_OK: u32 = 0;
pub const STATUS_EOF: u32 = 1;
pub const STATUS_NOT_FOUND: u32 = 2;
pub const STATUS_PERMISSION_DENIED: u32 = 3;
pub const STATUS_EXISTS: u32 = 4;
pub const STATUS_OTHER: u32 = 5;
/// A `connect` was actively refused — no listener at the target host:port.
pub const STATUS_REFUSED: u32 = 6;
/// A non-blocking op could not make progress (`ErrorKind::WouldBlock`). No
/// Phase-1 caller sets `NONBLOCK`, so it is never observed yet; the `/std`
/// scheduler consumes it once the readiness model lands.
pub const STATUS_WOULD_BLOCK: u32 = 7;

/// The mode tokens of `/sys/Io/open`, mirrored by `/std/File`'s `Mode` union.
pub const MODE_READ: u32 = 0;
pub const MODE_WRITE: u32 = 1;
pub const MODE_APPEND: u32 = 2;

/// The handle-token gensym, seeded past the well-known stdio tokens. `Entropy`
/// is `Cell`-backed, so the host's `Send + Sync` bound puts it behind a mutex.
/// Both host implementations mint handles from one of these.
pub(crate) fn handle_entropy() -> Mutex<Entropy> {
    let entropy = Entropy::new();
    entropy.seed(STDERR as usize + 1);

    Mutex::new(entropy)
}

pub(crate) fn fresh_handle(handles: &Mutex<Entropy>) -> u32 {
    handles.lock().unwrap().fresh() as u32
}

pub trait Host {
    /// Open the file at `path` with `MODE_*` semantics. Returns
    /// `(status, handle)`; the handle is meaningful only when the status is
    /// `STATUS_OK`.
    fn open(&self, path: &[u8], mode: u32) -> (u32, u32);

    /// Resolve `host`:`port` to a list of opaque address blobs the socket
    /// lifecycle consumes. Returns `(status, addresses)`; each blob is the host's
    /// private encoding (canonical address string here) the guest only shuttles
    /// back into `socket`/`bind`/`connect`. On `STATUS_OK` the list is non-empty.
    fn resolve(&self, host: &[u8], port: u32) -> (u32, Vec<Vec<u8>>);

    /// Create an unconnected socket for the address family encoded in `addr`.
    /// Returns `(status, handle)` like `open`; the handle is configured via the
    /// setters, then `bind`/`connect`/`listen` transition it.
    fn socket(&self, addr: &[u8]) -> (u32, u32);

    /// Bind socket `handle` to the local address `addr`. Returns a status.
    fn bind(&self, handle: u32, addr: &[u8]) -> u32;

    /// Connect socket `handle` to the resolved address `addr`. Returns a status;
    /// on `STATUS_OK` the handle is an ordinary byte stream `read`/`write`/`close`
    /// serve.
    fn connect(&self, handle: u32, addr: &[u8]) -> u32;

    /// Mark bound socket `handle` as listening with accept-queue depth `backlog`
    /// (OS-clamped to `somaxconn`). Returns a status; `accept` then pulls
    /// connections and `close` releases it.
    fn listen(&self, handle: u32, backlog: u32) -> u32;

    /// Pull the next connection from the listener `handle`, blocking until one
    /// arrives. Returns `(status, handle)`; the connection handle is an ordinary
    /// byte stream the same `read`/`write`/`close` serve, like a `connect`ed one.
    fn accept(&self, handle: u32) -> (u32, u32);

    /// Set socket `handle`'s non-blocking flag. Returns a status. A no-op on a
    /// file handle (recorded, not enforced).
    fn set_nonblocking(&self, handle: u32, on: u32) -> u32;

    /// Set socket `handle`'s receive timeout to `ms` milliseconds (`0` clears).
    /// Returns a status.
    fn set_recv_timeout(&self, handle: u32, ms: u32) -> u32;

    /// Set socket `handle`'s send timeout to `ms` milliseconds (`0` clears).
    /// Returns a status.
    fn set_send_timeout(&self, handle: u32, ms: u32) -> u32;

    /// Set socket `handle`'s `SO_REUSEADDR` flag. Returns a status; set before
    /// `bind`.
    fn set_reuseaddr(&self, handle: u32, on: u32) -> u32;

    /// Close `handle`. Closing an unknown handle is a no-op.
    fn close(&self, handle: u32);

    /// Read up to `count` bytes from `handle`, blocking until at least one
    /// byte is available. Returns `(status, bytes)`: `STATUS_OK` with 1..count
    /// bytes, `STATUS_EOF` with empty bytes, or an error status.
    fn read(&self, handle: u32, count: u32) -> (u32, Vec<u8>);

    /// Write `bytes` to `handle`, returning a status.
    fn write(&self, handle: u32, bytes: &[u8]) -> u32;

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
    /// `STATUS_OK` with the value, or `STATUS_NOT_FOUND` with empty bytes.
    fn env(&self, name: &[u8]) -> (u32, Vec<u8>);
}
