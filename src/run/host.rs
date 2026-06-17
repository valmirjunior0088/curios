use {
    crate::Entropy,
    socket2::{Domain, SockAddr, Socket, Type},
    std::{
        collections::{HashMap, HashSet, VecDeque},
        env,
        fs::OpenOptions,
        io::{ErrorKind, Read, Write, stderr, stdin, stdout},
        net::{SocketAddr, ToSocketAddrs},
        sync::{
            Arc, Mutex,
            mpsc::{self, Receiver, RecvError, Sender},
        },
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
    },
};

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

fn status_of(kind: ErrorKind) -> u32 {
    match kind {
        ErrorKind::NotFound => STATUS_NOT_FOUND,
        ErrorKind::PermissionDenied => STATUS_PERMISSION_DENIED,
        ErrorKind::AlreadyExists => STATUS_EXISTS,
        ErrorKind::ConnectionRefused => STATUS_REFUSED,
        ErrorKind::WouldBlock => STATUS_WOULD_BLOCK,
        _ => STATUS_OTHER,
    }
}

/// The handle-token gensym, seeded past the well-known stdio tokens. `Entropy`
/// is `Cell`-backed, so the host's `Send + Sync` bound puts it behind a mutex.
fn handle_entropy() -> Mutex<Entropy> {
    let entropy = Entropy::new();
    entropy.seed(STDERR as usize + 1);

    Mutex::new(entropy)
}

fn fresh_handle(handles: &Mutex<Entropy>) -> u32 {
    handles.lock().unwrap().fresh() as u32
}

/// A bidirectional byte stream backing a non-stdio handle: a file or a socket.
/// Both `File` and `TcpStream` are `Read + Write + Send`, so one handle map
/// serves them uniformly — `close` (a drop) flushes or disconnects either.
trait Conduit: Read + Write + Send {}
impl<T: Read + Write + Send> Conduit for T {}

/// A non-stdio handle in [`StdioHost`]'s unified table. The BSD lifecycle moves
/// a handle between states: `socket` mints an `Unconnected` socket, `connect`
/// turns it into a `Stream` (a byte conduit `read`/`write` serve), `listen`
/// turns it into a `Listener` `accept` pulls from. A `socket2::Socket` is itself
/// `Read + Write`, so a connected one boxes straight into a `Stream` with no
/// conversion; files from `open` are `Stream`s too.
enum Resource {
    Stream(Box<dyn Conduit>),
    Unconnected(Socket),
    Listener(Socket),
}

/// Parse an address blob (a canonical "ip:port" string) back into a
/// `SocketAddr`. The encoding is `StdioHost`'s private contract with `resolve`.
fn parse_addr(addr: &[u8]) -> Option<SocketAddr> {
    String::from_utf8_lossy(addr).parse().ok()
}

/// `0` ms means "no timeout" (clear it); any other value is a duration.
fn duration_ms(ms: u32) -> Option<Duration> {
    (ms != 0).then(|| Duration::from_millis(ms.into()))
}

/// Pull an unconnected socket out of the table by handle, leaving any other
/// resource (or none) in place. Used by `connect`/`listen` to transition a
/// handle without holding the lock across the blocking syscall.
fn take_unconnected(table: &Mutex<HashMap<u32, Resource>>, handle: u32) -> Option<Socket> {
    let mut table = table.lock().unwrap();

    match table.remove(&handle) {
        Some(Resource::Unconnected(socket)) => Some(socket),
        Some(other) => {
            table.insert(handle, other);
            None
        }
        None => None,
    }
}

/// Apply a `socket2` setter to a configurable handle. Unconnected sockets and
/// listeners expose their typed setters directly; a connected stream or file has
/// its socket boxed away, but no Phase-1 caller sets flags there, so that path
/// records nothing and reports success.
fn with_socket<F>(table: &Mutex<HashMap<u32, Resource>>, handle: u32, apply: F) -> u32
where
    F: FnOnce(&Socket) -> std::io::Result<()>,
{
    let table = table.lock().unwrap();

    match table.get(&handle) {
        Some(Resource::Unconnected(socket) | Resource::Listener(socket)) => match apply(socket) {
            Ok(()) => STATUS_OK,
            Err(error) => status_of(error.kind()),
        },
        Some(Resource::Stream(_)) => STATUS_OK,
        None => STATUS_OTHER,
    }
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

pub struct StdioHost {
    /// One table for every non-stdio handle, keyed by token. Files, unconnected
    /// sockets, connected streams, and listeners share it so the BSD lifecycle
    /// can transition a handle in place and `close` releases any kind uniformly.
    table: Mutex<HashMap<u32, Resource>>,
    handles: Mutex<Entropy>,
    /// Monotonic origin: `clock_mono` reports elapsed time since this.
    start: Instant,
    /// The process arguments served by `args` (argv[0] is the program name).
    args: Vec<Vec<u8>>,
}

impl Default for StdioHost {
    fn default() -> Self {
        Self::new()
    }
}

impl StdioHost {
    pub fn new() -> Self {
        Self::with_args(env::args_os().map(|arg| arg.into_encoded_bytes()).collect())
    }

    /// Build a host whose `args` are the given byte strings — used by the CLI to
    /// forward a program's own arguments instead of the `curios` process's.
    pub fn with_args(args: Vec<Vec<u8>>) -> Self {
        Self {
            table: Mutex::new(HashMap::new()),
            handles: handle_entropy(),
            start: Instant::now(),
            args,
        }
    }
}

impl Host for StdioHost {
    fn open(&self, path: &[u8], mode: u32) -> (u32, u32) {
        let path = String::from_utf8_lossy(path).into_owned();

        let mut options = OpenOptions::new();

        match mode {
            MODE_READ => options.read(true),
            MODE_WRITE => options.write(true).create(true).truncate(true),
            MODE_APPEND => options.append(true).create(true),
            _ => return (STATUS_OTHER, 0),
        };

        match options.open(&path) {
            Ok(file) => {
                let handle = fresh_handle(&self.handles);
                self.table
                    .lock()
                    .unwrap()
                    .insert(handle, Resource::Stream(Box::new(file)));

                (STATUS_OK, handle)
            }
            Err(error) => (status_of(error.kind()), 0),
        }
    }

    fn resolve(&self, host: &[u8], port: u32) -> (u32, Vec<Vec<u8>>) {
        let host = String::from_utf8_lossy(host).into_owned();
        let address = format!("{host}:{port}");

        match address.to_socket_addrs() {
            Ok(addresses) => {
                // Each blob is the canonical "ip:port" string — debuggable, and
                // `socket` recovers the address family from it.
                let addresses: Vec<Vec<u8>> = addresses
                    .map(|addr| addr.to_string().into_bytes())
                    .collect();

                if addresses.is_empty() {
                    (STATUS_NOT_FOUND, vec![])
                } else {
                    (STATUS_OK, addresses)
                }
            }
            Err(error) => (status_of(error.kind()), vec![]),
        }
    }

    fn socket(&self, addr: &[u8]) -> (u32, u32) {
        let address = match parse_addr(addr) {
            Some(address) => address,
            None => return (STATUS_OTHER, 0),
        };

        match Socket::new(Domain::for_address(address), Type::STREAM, None) {
            Ok(socket) => {
                let handle = fresh_handle(&self.handles);
                self.table
                    .lock()
                    .unwrap()
                    .insert(handle, Resource::Unconnected(socket));

                (STATUS_OK, handle)
            }
            Err(error) => (status_of(error.kind()), 0),
        }
    }

    fn bind(&self, handle: u32, addr: &[u8]) -> u32 {
        let address = match parse_addr(addr) {
            Some(address) => address,
            None => return STATUS_OTHER,
        };

        match self.table.lock().unwrap().get(&handle) {
            Some(Resource::Unconnected(socket)) => match socket.bind(&SockAddr::from(address)) {
                Ok(()) => STATUS_OK,
                Err(error) => status_of(error.kind()),
            },
            _ => STATUS_OTHER,
        }
    }

    fn connect(&self, handle: u32, addr: &[u8]) -> u32 {
        let address = match parse_addr(addr) {
            Some(address) => address,
            None => return STATUS_OTHER,
        };

        // Take the socket out so the blocking connect runs without the table
        // lock held; re-file the connected socket as a byte stream on success.
        let socket = match take_unconnected(&self.table, handle) {
            Some(socket) => socket,
            None => return STATUS_OTHER,
        };

        match socket.connect(&SockAddr::from(address)) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(handle, Resource::Stream(Box::new(socket)));

                STATUS_OK
            }
            Err(error) => status_of(error.kind()),
        }
    }

    fn listen(&self, handle: u32, backlog: u32) -> u32 {
        let socket = match take_unconnected(&self.table, handle) {
            Some(socket) => socket,
            None => return STATUS_OTHER,
        };

        match socket.listen(backlog as i32) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(handle, Resource::Listener(socket));

                STATUS_OK
            }
            Err(error) => status_of(error.kind()),
        }
    }

    fn accept(&self, handle: u32) -> (u32, u32) {
        // `accept` blocks until a connection arrives, so clone the listener fd
        // out and drop the table lock before the wait — never hold it across one.
        let listener = match self.table.lock().unwrap().get(&handle) {
            Some(Resource::Listener(socket)) => socket.try_clone(),
            _ => return (STATUS_OTHER, 0),
        };

        match listener.and_then(|listener| listener.accept()) {
            Ok((stream, _)) => {
                let conn = fresh_handle(&self.handles);
                self.table
                    .lock()
                    .unwrap()
                    .insert(conn, Resource::Stream(Box::new(stream)));

                (STATUS_OK, conn)
            }
            Err(error) => (status_of(error.kind()), 0),
        }
    }

    fn set_nonblocking(&self, handle: u32, on: u32) -> u32 {
        with_socket(&self.table, handle, |socket| socket.set_nonblocking(on != 0))
    }

    fn set_recv_timeout(&self, handle: u32, ms: u32) -> u32 {
        with_socket(&self.table, handle, |socket| {
            socket.set_read_timeout(duration_ms(ms))
        })
    }

    fn set_send_timeout(&self, handle: u32, ms: u32) -> u32 {
        with_socket(&self.table, handle, |socket| {
            socket.set_write_timeout(duration_ms(ms))
        })
    }

    fn set_reuseaddr(&self, handle: u32, on: u32) -> u32 {
        with_socket(&self.table, handle, |socket| {
            socket.set_reuse_address(on != 0)
        })
    }

    fn close(&self, handle: u32) {
        self.table.lock().unwrap().remove(&handle);
    }

    fn read(&self, handle: u32, count: u32) -> (u32, Vec<u8>) {
        let mut buffer = vec![0; count as usize];

        let result = match handle {
            STDIN => stdin().lock().read(&mut buffer),
            _ => match self.table.lock().unwrap().get_mut(&handle) {
                Some(Resource::Stream(stream)) => stream.read(&mut buffer),
                _ => return (STATUS_EOF, vec![]),
            },
        };

        match result {
            Ok(0) => (STATUS_EOF, vec![]),
            Ok(n) => {
                buffer.truncate(n);

                (STATUS_OK, buffer)
            }
            Err(error) => (status_of(error.kind()), vec![]),
        }
    }

    fn write(&self, handle: u32, bytes: &[u8]) -> u32 {
        let result = match handle {
            STDOUT => stdout().write_all(bytes),
            STDERR => stderr().write_all(bytes),
            _ => match self.table.lock().unwrap().get_mut(&handle) {
                Some(Resource::Stream(stream)) => stream.write_all(bytes),
                _ => return STATUS_OTHER,
            },
        };

        match result {
            Ok(()) => STATUS_OK,
            Err(error) => status_of(error.kind()),
        }
    }

    fn clock_wall(&self) -> (u32, u32, u32) {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();
        let secs = now.as_secs();

        (
            (secs / 1_000_000_000) as u32,
            (secs % 1_000_000_000) as u32,
            now.subsec_nanos(),
        )
    }

    fn clock_mono(&self) -> (u32, u32) {
        let elapsed = self.start.elapsed();

        (elapsed.as_secs() as u32, elapsed.subsec_nanos())
    }

    fn random(&self, count: u32) -> Vec<u8> {
        let mut buffer = vec![0u8; count as usize];
        getrandom::fill(&mut buffer).expect("OS randomness unavailable");

        buffer
    }

    fn args(&self) -> Vec<Vec<u8>> {
        self.args.clone()
    }

    fn env(&self, name: &[u8]) -> (u32, Vec<u8>) {
        match env::var_os(String::from_utf8_lossy(name).as_ref()) {
            Some(value) => (STATUS_OK, value.into_encoded_bytes()),
            None => (STATUS_NOT_FOUND, vec![]),
        }
    }
}

/// The in-memory file map shared between a [`ChannelHost`] and the test that
/// seeded it: path → contents, inspectable after the run.
pub type ChannelFs = Arc<Mutex<HashMap<Vec<u8>, Vec<u8>>>>;

struct OpenFile {
    path: Vec<u8>,
    mode: u32,
    position: usize,
}

/// A live in-memory connection: the scripted response and a read cursor into
/// it. Writes to a connection are accepted and discarded.
struct NetConn {
    response: Vec<u8>,
    position: usize,
}

/// A live in-memory *inbound* connection minted by `accept`: `read` serves the
/// scripted request from `position`, and `write` appends to `captures[capture]`
/// so a test can inspect what the server sent back.
struct ServerConn {
    request: Vec<u8>,
    position: usize,
    capture: usize,
}

pub struct ChannelHost {
    input_receiver: Mutex<Receiver<Vec<u8>>>,
    /// Bytes received from the channel but not yet consumed by `read` —
    /// short reads must never drop the remainder of a message.
    input_leftover: Mutex<Vec<u8>>,
    /// Writes to stdout and stderr both land here; tests do not distinguish
    /// the two streams.
    output_sender: Arc<Mutex<Sender<Vec<u8>>>>,
    /// The in-memory filesystem backing `open`/`close` and file handles.
    files: ChannelFs,
    open_files: Mutex<HashMap<u32, OpenFile>>,
    /// Scripted network endpoints: `host:port` → the bytes a connection serves
    /// on read. Connecting to an unscripted endpoint is refused.
    endpoints: Mutex<HashMap<Vec<u8>, Vec<u8>>>,
    /// Live in-memory connections keyed by handle.
    connections: Mutex<HashMap<u32, NetConn>>,
    /// Unconnected sockets minted by `socket`, awaiting `connect` or `listen` —
    /// a set of valid handles; the test host does no real socket allocation.
    sockets: Mutex<HashSet<u32>>,
    /// Listening sockets minted by `listen` — a set of valid handles; the test
    /// host does no real binding.
    listeners: Mutex<HashSet<u32>>,
    /// Scripted inbound requests, one served per `accept` (FIFO).
    inbound: Mutex<VecDeque<Vec<u8>>>,
    /// Captured server responses: one entry per accepted connection, the
    /// concatenation of its writes. Inspectable after the run.
    captures: Arc<Mutex<Vec<Vec<u8>>>>,
    /// Live accepted (inbound) connections keyed by handle.
    server_conns: Mutex<HashMap<u32, ServerConn>>,
    handles: Mutex<Entropy>,
    /// Scripted wall-clock readings, served in order by `clock_wall`.
    clock_wall_seq: Mutex<VecDeque<(u32, u32, u32)>>,
    /// Scripted monotonic readings, served in order by `clock_mono`.
    clock_mono_seq: Mutex<VecDeque<(u32, u32)>>,
    /// Deterministic xorshift64 state backing `random`.
    rng: Mutex<u64>,
    /// Scripted process arguments served by `args`.
    args: Mutex<Vec<Vec<u8>>>,
    /// Scripted environment served by `env`: name → value.
    env: Mutex<HashMap<Vec<u8>, Vec<u8>>>,
}

impl ChannelHost {
    /// Like `in_out`, but pre-seeds the in-memory filesystem and returns it so
    /// the test can inspect written files after the run.
    pub fn with_fs<L, I, P, C, F>(lines: I, files: F) -> (Self, Receiver<Vec<u8>>, ChannelFs)
    where
        L: AsRef<[u8]>,
        I: IntoIterator<Item = L>,
        P: AsRef<[u8]>,
        C: AsRef<[u8]>,
        F: IntoIterator<Item = (P, C)>,
    {
        let (input_sender, input_receiver) = mpsc::channel();
        let (output_sender, output_receiver) = mpsc::channel();

        for line in lines {
            input_sender.send(line.as_ref().to_vec()).unwrap();
        }

        let files = Arc::new(Mutex::new(
            files
                .into_iter()
                .map(|(path, contents)| (path.as_ref().to_vec(), contents.as_ref().to_vec()))
                .collect(),
        ));

        (
            ChannelHost {
                input_receiver: Mutex::new(input_receiver),
                input_leftover: Mutex::new(Vec::new()),
                output_sender: Arc::new(Mutex::new(output_sender)),
                files: files.clone(),
                open_files: Mutex::new(HashMap::new()),
                endpoints: Mutex::new(HashMap::new()),
                connections: Mutex::new(HashMap::new()),
                sockets: Mutex::new(HashSet::new()),
                listeners: Mutex::new(HashSet::new()),
                inbound: Mutex::new(VecDeque::new()),
                captures: Arc::new(Mutex::new(Vec::new())),
                server_conns: Mutex::new(HashMap::new()),
                handles: handle_entropy(),
                clock_wall_seq: Mutex::new(VecDeque::new()),
                clock_mono_seq: Mutex::new(VecDeque::new()),
                rng: Mutex::new(0x2545_F491_4F6C_DD1D),
                args: Mutex::new(Vec::new()),
                env: Mutex::new(HashMap::new()),
            },
            output_receiver,
            files,
        )
    }

    pub fn in_out<L, I>(lines: I) -> (Self, Receiver<Vec<u8>>)
    where
        L: AsRef<[u8]>,
        I: IntoIterator<Item = L>,
    {
        let (host, output_receiver, _) = Self::with_fs(lines, [] as [(&[u8], &[u8]); 0]);

        (host, output_receiver)
    }

    pub fn out() -> (Self, Receiver<Vec<u8>>) {
        Self::in_out::<&[u8], [&[u8]; 0]>([])
    }

    /// Script the wall-clock readings served by `clock_wall`, in order. When
    /// the script is exhausted `clock_wall` falls back to `(0, 0, 0)`.
    pub fn script_wall<I: IntoIterator<Item = (u32, u32, u32)>>(&self, readings: I) {
        self.clock_wall_seq.lock().unwrap().extend(readings);
    }

    /// Script the monotonic readings served by `clock_mono`, in order.
    pub fn script_mono<I: IntoIterator<Item = (u32, u32)>>(&self, readings: I) {
        self.clock_mono_seq.lock().unwrap().extend(readings);
    }

    /// Reseed the deterministic RNG backing `random` (must be non-zero).
    pub fn seed_random(&self, seed: u64) {
        *self.rng.lock().unwrap() = seed;
    }

    /// Set the process arguments served by `args`.
    pub fn script_args<L: AsRef<[u8]>, I: IntoIterator<Item = L>>(&self, args: I) {
        *self.args.lock().unwrap() = args.into_iter().map(|a| a.as_ref().to_vec()).collect();
    }

    /// Set the environment served by `env`: `(name, value)` pairs.
    pub fn script_env<N: AsRef<[u8]>, V: AsRef<[u8]>, I: IntoIterator<Item = (N, V)>>(
        &self,
        vars: I,
    ) {
        *self.env.lock().unwrap() = vars
            .into_iter()
            .map(|(name, value)| (name.as_ref().to_vec(), value.as_ref().to_vec()))
            .collect();
    }

    /// Script the network endpoints served by `connect`: `(host:port, response)`
    /// pairs. Connecting to an unscripted endpoint is refused.
    pub fn script_net<E: AsRef<[u8]>, R: AsRef<[u8]>, I: IntoIterator<Item = (E, R)>>(
        &self,
        endpoints: I,
    ) {
        *self.endpoints.lock().unwrap() = endpoints
            .into_iter()
            .map(|(endpoint, response)| (endpoint.as_ref().to_vec(), response.as_ref().to_vec()))
            .collect();
    }

    /// Script the inbound requests served by `accept`, in order — one request
    /// per accepted connection. An exhausted queue makes `accept` fail, which
    /// ends a `serve` loop (a real blocking `accept` would park there).
    pub fn script_inbound<R: AsRef<[u8]>, I: IntoIterator<Item = R>>(&self, requests: I) {
        *self.inbound.lock().unwrap() =
            requests.into_iter().map(|r| r.as_ref().to_vec()).collect();
    }

    /// The captured server responses: one entry per accepted connection, the
    /// concatenation of the bytes written to it.
    pub fn captures(&self) -> Arc<Mutex<Vec<Vec<u8>>>> {
        self.captures.clone()
    }
}

impl Host for ChannelHost {
    fn open(&self, path: &[u8], mode: u32) -> (u32, u32) {
        let mut files = self.files.lock().unwrap();

        match mode {
            MODE_READ => {
                if !files.contains_key(path) {
                    return (STATUS_NOT_FOUND, 0);
                }
            }
            MODE_WRITE => {
                files.insert(path.to_vec(), vec![]);
            }
            MODE_APPEND => {
                files.entry(path.to_vec()).or_default();
            }
            _ => return (STATUS_OTHER, 0),
        }

        let handle = fresh_handle(&self.handles);

        self.open_files.lock().unwrap().insert(
            handle,
            OpenFile {
                path: path.to_vec(),
                mode,
                position: 0,
            },
        );

        (STATUS_OK, handle)
    }

    fn resolve(&self, host: &[u8], port: u32) -> (u32, Vec<Vec<u8>>) {
        // One synthetic address blob: the `host:port` key `script_net` uses, so
        // `connect` can recover the scripted endpoint from the blob.
        let endpoint = format!("{}:{port}", String::from_utf8_lossy(host)).into_bytes();

        (STATUS_OK, vec![endpoint])
    }

    fn socket(&self, _addr: &[u8]) -> (u32, u32) {
        let handle = fresh_handle(&self.handles);
        self.sockets.lock().unwrap().insert(handle);

        (STATUS_OK, handle)
    }

    fn bind(&self, handle: u32, _addr: &[u8]) -> u32 {
        if self.sockets.lock().unwrap().contains(&handle) {
            STATUS_OK
        } else {
            STATUS_OTHER
        }
    }

    fn connect(&self, handle: u32, addr: &[u8]) -> u32 {
        // The handle must be an unconnected socket minted by `socket`.
        if !self.sockets.lock().unwrap().remove(&handle) {
            return STATUS_OTHER;
        }

        let response = match self.endpoints.lock().unwrap().get(addr) {
            Some(response) => response.clone(),
            None => return STATUS_REFUSED,
        };

        self.connections.lock().unwrap().insert(
            handle,
            NetConn {
                response,
                position: 0,
            },
        );

        STATUS_OK
    }

    fn listen(&self, handle: u32, _backlog: u32) -> u32 {
        if !self.sockets.lock().unwrap().remove(&handle) {
            return STATUS_OTHER;
        }

        self.listeners.lock().unwrap().insert(handle);

        STATUS_OK
    }

    fn set_nonblocking(&self, _handle: u32, _on: u32) -> u32 {
        STATUS_OK
    }

    fn set_recv_timeout(&self, _handle: u32, _ms: u32) -> u32 {
        STATUS_OK
    }

    fn set_send_timeout(&self, _handle: u32, _ms: u32) -> u32 {
        STATUS_OK
    }

    fn set_reuseaddr(&self, _handle: u32, _on: u32) -> u32 {
        STATUS_OK
    }

    fn accept(&self, handle: u32) -> (u32, u32) {
        if !self.listeners.lock().unwrap().contains(&handle) {
            return (STATUS_OTHER, 0);
        }

        // Pull the next scripted request. An exhausted queue fails the accept,
        // ending the serve loop (a real blocking accept would park forever).
        let request = match self.inbound.lock().unwrap().pop_front() {
            Some(request) => request,
            None => return (STATUS_OTHER, 0),
        };

        let capture = {
            let mut captures = self.captures.lock().unwrap();
            let index = captures.len();
            captures.push(Vec::new());
            index
        };

        let conn = fresh_handle(&self.handles);
        self.server_conns.lock().unwrap().insert(
            conn,
            ServerConn {
                request,
                position: 0,
                capture,
            },
        );

        (STATUS_OK, conn)
    }

    fn close(&self, handle: u32) {
        self.open_files.lock().unwrap().remove(&handle);
        self.connections.lock().unwrap().remove(&handle);
        self.server_conns.lock().unwrap().remove(&handle);
        self.sockets.lock().unwrap().remove(&handle);
        self.listeners.lock().unwrap().remove(&handle);
    }

    fn read(&self, handle: u32, count: u32) -> (u32, Vec<u8>) {
        if handle != STDIN {
            // File-backed handle?
            {
                let mut open_files = self.open_files.lock().unwrap();

                if let Some(open) = open_files.get_mut(&handle) {
                    if open.mode != MODE_READ {
                        return (STATUS_OTHER, vec![]);
                    }

                    let files = self.files.lock().unwrap();
                    let contents = files.get(&open.path).map(Vec::as_slice).unwrap_or(&[]);

                    if open.position >= contents.len() {
                        return (STATUS_EOF, vec![]);
                    }

                    let stop = contents.len().min(open.position + count as usize);
                    let bytes = contents[open.position..stop].to_vec();
                    open.position = stop;

                    return (STATUS_OK, bytes);
                }
            }

            // Inbound (accepted) connection? Serve the scripted request bytes.
            {
                let mut server_conns = self.server_conns.lock().unwrap();

                if let Some(conn) = server_conns.get_mut(&handle) {
                    if conn.position >= conn.request.len() {
                        return (STATUS_EOF, vec![]);
                    }

                    let stop = conn.request.len().min(conn.position + count as usize);
                    let bytes = conn.request[conn.position..stop].to_vec();
                    conn.position = stop;

                    return (STATUS_OK, bytes);
                }
            }

            // Socket-backed handle? Serve the scripted response bytes.
            let mut connections = self.connections.lock().unwrap();

            if let Some(conn) = connections.get_mut(&handle) {
                if conn.position >= conn.response.len() {
                    return (STATUS_EOF, vec![]);
                }

                let stop = conn.response.len().min(conn.position + count as usize);
                let bytes = conn.response[conn.position..stop].to_vec();
                conn.position = stop;

                return (STATUS_OK, bytes);
            }

            return (STATUS_EOF, vec![]);
        }

        let mut leftover = self.input_leftover.lock().unwrap();

        // Each channel message is one injected line; the newline the terminal
        // would deliver is appended here. Refill only when the buffer is dry,
        // then serve up to `count` bytes and stash the rest.
        if leftover.is_empty() {
            match self.input_receiver.lock().unwrap().recv() {
                Ok(line) => {
                    leftover.extend(line);
                    leftover.push(b'\n');
                }
                Err(RecvError) => return (STATUS_EOF, vec![]),
            }
        }

        let served = leftover.len().min(count as usize);

        (STATUS_OK, leftover.drain(..served).collect())
    }

    fn write(&self, handle: u32, bytes: &[u8]) -> u32 {
        if handle == STDOUT || handle == STDERR {
            self.output_sender
                .lock()
                .unwrap()
                .send(bytes.to_owned())
                .unwrap();

            return STATUS_OK;
        }

        {
            let open_files = self.open_files.lock().unwrap();

            if let Some(open) = open_files.get(&handle) {
                if open.mode == MODE_READ {
                    return STATUS_OTHER;
                }

                self.files
                    .lock()
                    .unwrap()
                    .entry(open.path.clone())
                    .or_default()
                    .extend_from_slice(bytes);

                return STATUS_OK;
            }
        }

        // Inbound (accepted) connection: capture the response bytes so a test
        // can inspect what the server wrote back.
        {
            let server_conns = self.server_conns.lock().unwrap();

            if let Some(conn) = server_conns.get(&handle) {
                self.captures.lock().unwrap()[conn.capture].extend_from_slice(bytes);

                return STATUS_OK;
            }
        }

        // Socket-backed (outbound) handle: accept and discard (the in-memory
        // test host does not capture request bytes in Phase A).
        if self.connections.lock().unwrap().contains_key(&handle) {
            return STATUS_OK;
        }

        STATUS_OTHER
    }

    fn clock_wall(&self) -> (u32, u32, u32) {
        self.clock_wall_seq
            .lock()
            .unwrap()
            .pop_front()
            .unwrap_or((0, 0, 0))
    }

    fn clock_mono(&self) -> (u32, u32) {
        self.clock_mono_seq
            .lock()
            .unwrap()
            .pop_front()
            .unwrap_or((0, 0))
    }

    fn random(&self, count: u32) -> Vec<u8> {
        let mut state = self.rng.lock().unwrap();
        let mut output = Vec::with_capacity(count as usize);

        for _ in 0..count {
            // xorshift64: deterministic and reproducible across runs.
            let mut x = *state;
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            *state = x;
            output.push((x >> 24) as u8);
        }

        output
    }

    fn args(&self) -> Vec<Vec<u8>> {
        self.args.lock().unwrap().clone()
    }

    fn env(&self, name: &[u8]) -> (u32, Vec<u8>) {
        match self.env.lock().unwrap().get(name) {
            Some(value) => (STATUS_OK, value.clone()),
            None => (STATUS_NOT_FOUND, vec![]),
        }
    }
}
