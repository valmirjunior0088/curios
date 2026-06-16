use {
    crate::Entropy,
    std::{
        collections::{HashMap, VecDeque},
        fs::OpenOptions,
        io::{ErrorKind, Read, Write, stderr, stdin, stdout},
        net::{TcpStream, ToSocketAddrs},
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

pub trait Host {
    /// Open the file at `path` with `MODE_*` semantics. Returns
    /// `(status, handle)`; the handle is meaningful only when the status is
    /// `STATUS_OK`.
    fn open(&self, path: &[u8], mode: u32) -> (u32, u32);

    /// Connect to `host`:`port`. The three timeouts are milliseconds (`0` = no
    /// timeout). Returns `(status, handle)` like `open`; the handle is an
    /// ordinary byte stream the same `read`/`write`/`close` serve.
    fn connect(
        &self,
        host: &[u8],
        port: u32,
        connect_timeout: u32,
        read_timeout: u32,
        write_timeout: u32,
    ) -> (u32, u32);

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
    streams: Mutex<HashMap<u32, Box<dyn Conduit>>>,
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
        Self::with_args(
            std::env::args_os()
                .map(|arg| arg.into_encoded_bytes())
                .collect(),
        )
    }

    /// Build a host whose `args` are the given byte strings — used by the CLI to
    /// forward a program's own arguments instead of the `curios` process's.
    pub fn with_args(args: Vec<Vec<u8>>) -> Self {
        Self {
            streams: Mutex::new(HashMap::new()),
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
                self.streams.lock().unwrap().insert(handle, Box::new(file));

                (STATUS_OK, handle)
            }
            Err(error) => (status_of(error.kind()), 0),
        }
    }

    fn connect(
        &self,
        host: &[u8],
        port: u32,
        connect_timeout: u32,
        read_timeout: u32,
        write_timeout: u32,
    ) -> (u32, u32) {
        let host = String::from_utf8_lossy(host).into_owned();
        let address = format!("{host}:{port}");

        let stream = if connect_timeout == 0 {
            TcpStream::connect(&address)
        } else {
            match address.to_socket_addrs().ok().and_then(|mut a| a.next()) {
                Some(addr) => {
                    TcpStream::connect_timeout(&addr, Duration::from_millis(connect_timeout.into()))
                }
                None => return (STATUS_OTHER, 0),
            }
        };

        let stream = match stream {
            Ok(stream) => stream,
            Err(error) => return (status_of(error.kind()), 0),
        };

        let timeout = |ms: u32| (ms != 0).then(|| Duration::from_millis(ms.into()));
        let _ = stream.set_read_timeout(timeout(read_timeout));
        let _ = stream.set_write_timeout(timeout(write_timeout));

        let handle = fresh_handle(&self.handles);
        self.streams.lock().unwrap().insert(handle, Box::new(stream));

        (STATUS_OK, handle)
    }

    fn close(&self, handle: u32) {
        self.streams.lock().unwrap().remove(&handle);
    }

    fn read(&self, handle: u32, count: u32) -> (u32, Vec<u8>) {
        let mut buffer = vec![0; count as usize];

        let result = match handle {
            STDIN => stdin().lock().read(&mut buffer),
            _ => match self.streams.lock().unwrap().get_mut(&handle) {
                Some(stream) => stream.read(&mut buffer),
                None => return (STATUS_EOF, vec![]),
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
            _ => match self.streams.lock().unwrap().get_mut(&handle) {
                Some(stream) => stream.write_all(bytes),
                None => return STATUS_OTHER,
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
        match std::env::var_os(String::from_utf8_lossy(name).as_ref()) {
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

    fn connect(
        &self,
        host: &[u8],
        port: u32,
        _connect_timeout: u32,
        _read_timeout: u32,
        _write_timeout: u32,
    ) -> (u32, u32) {
        let endpoint = format!("{}:{port}", String::from_utf8_lossy(host)).into_bytes();

        let response = match self.endpoints.lock().unwrap().get(&endpoint) {
            Some(response) => response.clone(),
            None => return (STATUS_REFUSED, 0),
        };

        let handle = fresh_handle(&self.handles);
        self.connections
            .lock()
            .unwrap()
            .insert(handle, NetConn { response, position: 0 });

        (STATUS_OK, handle)
    }

    fn close(&self, handle: u32) {
        self.open_files.lock().unwrap().remove(&handle);
        self.connections.lock().unwrap().remove(&handle);
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

        // Socket-backed handle: accept and discard (the in-memory test host
        // does not capture request bytes in Phase A).
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
