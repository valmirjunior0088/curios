use {
    super::host::*,
    std::{
        collections::{HashMap, VecDeque},
        sync::{
            Arc, Mutex,
            atomic::{AtomicU32, Ordering},
        },
    },
};

/// The in-memory file map: path → contents, behind a shared lock. A live
/// [`MockHost`] writes it during the run; the [`MockIo`] handle a test holds
/// reads it back afterwards. `clone` shares the one underlying map.
#[derive(Clone)]
struct MockFileSystem {
    inner: Arc<Mutex<HashMap<Vec<u8>, Vec<u8>>>>,
}

impl MockFileSystem {
    /// Wrap a seeded `path → contents` map.
    fn new(files: HashMap<Vec<u8>, Vec<u8>>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(files)),
        }
    }

    /// Whether `path` exists — `open`'s existence check in read mode.
    fn contains(&self, path: &[u8]) -> bool {
        self.inner.lock().unwrap().contains_key(path)
    }

    /// Reset `path` to empty, creating it if absent — `open` in write mode.
    fn truncate(&self, path: &[u8]) {
        self.inner.lock().unwrap().insert(path.to_vec(), vec![]);
    }

    /// Create `path` empty if absent, leaving any existing contents — `open` in
    /// append mode.
    fn ensure(&self, path: &[u8]) {
        self.inner.lock().unwrap().entry(path.to_vec()).or_default();
    }

    /// Append `bytes` to `path`, creating it if absent.
    fn append(&self, path: &[u8], bytes: &[u8]) {
        self.inner
            .lock()
            .unwrap()
            .entry(path.to_vec())
            .or_default()
            .extend_from_slice(bytes);
    }

    /// Borrow `path`'s contents (empty if absent) under the lock, so a read can
    /// serve a slice without cloning the whole file.
    fn with<R>(&self, path: &[u8], serve: impl FnOnce(&[u8]) -> R) -> R {
        let files = self.inner.lock().unwrap();

        serve(files.get(path).map(Vec::as_slice).unwrap_or(&[]))
    }

    /// A clone of `path`'s contents, or `None` if absent — post-run inspection.
    fn get(&self, path: &[u8]) -> Option<Vec<u8>> {
        self.inner.lock().unwrap().get(path).cloned()
    }
}

struct MockFile {
    path: Vec<u8>,
    mode: Mode,
    position: usize,
}

/// A live in-memory *outbound* connection: the scripted response and a read
/// cursor into it. Writes to a connection are accepted and discarded.
struct MockClient {
    response: Vec<u8>,
    position: usize,
}

/// A live in-memory *inbound* connection minted by `accept`: `read` serves the
/// scripted request from `position`, and `write` appends to `captures[capture]`
/// so a test can inspect what the server sent back.
struct MockServer {
    request: Vec<u8>,
    position: usize,
    capture: usize,
}

/// A non-stdio handle in [`MockHost`]'s unified table — the scripted, in-
/// memory mirror of `OsHost`'s `OsResource`. The BSD lifecycle moves a handle
/// between states: `socket` mints a `Socket`, `connect` turns it into an
/// `Outbound` stream, `listen` turns it into a `Listener` that `accept` pulls
/// `Inbound` streams from; `open` files a `File`. `close` drops any kind.
enum MockResource {
    File(MockFile),
    Outbound(MockClient),
    Inbound(MockServer),
    Socket,
    Listener,
}

/// The scripted, in-memory [`Host`] used by the test suite — the mirror of
/// `OsHost`. Build one with [`MockHost::builder`], move it into the runner, and
/// read what the run produced through the [`MockIo`] handle `build` returns.
pub struct MockHost {
    handle_seed: AtomicU32,
    /// Scripted stdin, pre-joined and newline-terminated; `read(Io::Stdin, …)`
    /// drains its front and reports `Status::Eof` once it runs dry.
    input: Mutex<VecDeque<u8>>,
    /// Every byte written to stdout and stderr, concatenated in write order —
    /// the streams are not distinguished. Shared with [`MockIo::output`].
    output: Arc<Mutex<Vec<u8>>>,
    /// The in-memory filesystem backing `open`/`read`/`write`/`close`. Shared
    /// with [`MockIo::file`].
    files: MockFileSystem,
    /// One table for every non-stdio handle, keyed by token: open files,
    /// outbound/inbound connections, and unconnected/listening sockets. The BSD
    /// lifecycle transitions a handle in place (`socket` → `connect`/`listen` →
    /// `accept`) and `close` releases any kind uniformly — the scripted mirror
    /// of `OsHost`'s real-resource table.
    table: Mutex<HashMap<u32, MockResource>>,
    /// Scripted network endpoints: `host:port` → the bytes a connection serves
    /// on read. Read-only during the run; connecting elsewhere is refused.
    endpoints: HashMap<Vec<u8>, Vec<u8>>,
    /// Scripted inbound requests, one served per `accept` (FIFO).
    inbound: Mutex<VecDeque<Vec<u8>>>,
    /// Captured server responses: one entry per accepted connection, the
    /// concatenation of its writes. Shared with [`MockIo::captures`].
    captures: Arc<Mutex<Vec<Vec<u8>>>>,
    /// Scripted wall-clock readings, served in order by `clock_wall`.
    clock_wall_seq: Mutex<VecDeque<(u32, u32, u32)>>,
    /// Scripted monotonic readings, served in order by `clock_mono`.
    clock_mono_seq: Mutex<VecDeque<(u32, u32)>>,
    /// Deterministic xorshift64 state backing `random`.
    rng: Mutex<u64>,
    /// Scripted process arguments served by `args`.
    args: Vec<Vec<u8>>,
    /// Scripted environment served by `env`: name → value.
    env: HashMap<Vec<u8>, Vec<u8>>,
}

impl MockHost {
    /// Start seeding a host. Chain the `stdin_lines`/`files`/`net`/… setters,
    /// then `build` for the `(host, io)` pair.
    pub fn builder() -> MockHostBuilder {
        MockHostBuilder::default()
    }

    /// Serve up to `count` bytes of `contents` from `*position`, advancing the
    /// cursor; `Status::Eof` with empty bytes once it reaches the end. The shared
    /// shape of every scripted read (file, inbound request, outbound response).
    fn serve_from(contents: &[u8], position: &mut usize, count: u32) -> (Status, Vec<u8>) {
        if *position >= contents.len() {
            return (Status::Eof, vec![]);
        }

        let stop = contents.len().min(*position + count as usize);
        let bytes = contents[*position..stop].to_vec();
        *position = stop;

        (Status::Ok, bytes)
    }
}

impl Host for MockHost {
    fn open(&self, path: &[u8], mode: Mode) -> (Status, Io) {
        match mode {
            Mode::Read => {
                if !self.files.contains(path) {
                    return (Status::NotFound, Io::Other(0));
                }
            }
            Mode::Write => self.files.truncate(path),
            Mode::Append => self.files.ensure(path),
        }

        let handle = self.handle_seed.fetch_add(1, Ordering::Relaxed);

        self.table.lock().unwrap().insert(
            handle,
            MockResource::File(MockFile {
                path: path.to_vec(),
                mode,
                position: 0,
            }),
        );

        (Status::Ok, Io::Other(handle))
    }

    fn resolve(&self, host: &[u8], port: u32) -> (Status, Vec<Vec<u8>>) {
        // One synthetic address blob: the `host:port` key `net` uses, so
        // `connect` can recover the scripted endpoint from the blob.
        let endpoint = format!("{}:{port}", String::from_utf8_lossy(host)).into_bytes();

        (Status::Ok, vec![endpoint])
    }

    fn socket(&self, _addr: &[u8]) -> (Status, Io) {
        let handle = self.handle_seed.fetch_add(1, Ordering::Relaxed);
        self.table
            .lock()
            .unwrap()
            .insert(handle, MockResource::Socket);

        (Status::Ok, Io::Other(handle))
    }

    fn bind(&self, io: Io, _addr: &[u8]) -> Status {
        if matches!(
            self.table.lock().unwrap().get(&io.token()),
            Some(MockResource::Socket)
        ) {
            Status::Ok
        } else {
            Status::NotFound
        }
    }

    fn connect(&self, io: Io, addr: &[u8]) -> Status {
        // The handle must be an unconnected socket minted by `socket`; consume
        // it up front so a refusal leaves no half-open handle behind.
        {
            let mut table = self.table.lock().unwrap();

            match table.get(&io.token()) {
                Some(MockResource::Socket) => {
                    table.remove(&io.token());
                }
                _ => return Status::NotFound,
            }
        }

        let response = match self.endpoints.get(addr) {
            Some(response) => response.clone(),
            None => return Status::ConnectionRefused,
        };

        self.table.lock().unwrap().insert(
            io.token(),
            MockResource::Outbound(MockClient {
                response,
                position: 0,
            }),
        );

        Status::Ok
    }

    fn listen(&self, io: Io, _backlog: u32) -> Status {
        let mut table = self.table.lock().unwrap();

        match table.get(&io.token()) {
            Some(MockResource::Socket) => {
                table.insert(io.token(), MockResource::Listener);
                Status::Ok
            }
            _ => Status::NotFound,
        }
    }

    fn accept(&self, io: Io) -> (Status, Io) {
        if !matches!(
            self.table.lock().unwrap().get(&io.token()),
            Some(MockResource::Listener)
        ) {
            return (Status::NotFound, Io::Other(0));
        }

        // Pull the next scripted request. An exhausted queue fails the accept,
        // ending the serve loop (a real blocking accept would park forever).
        let request = match self.inbound.lock().unwrap().pop_front() {
            Some(request) => request,
            None => return (Status::NotFound, Io::Other(0)),
        };

        let capture = {
            let mut captures = self.captures.lock().unwrap();
            let index = captures.len();
            captures.push(Vec::new());
            index
        };

        let conn = self.handle_seed.fetch_add(1, Ordering::Relaxed);
        self.table.lock().unwrap().insert(
            conn,
            MockResource::Inbound(MockServer {
                request,
                position: 0,
                capture,
            }),
        );

        (Status::Ok, Io::Other(conn))
    }

    fn set_nonblocking(&self, _io: Io, _on: u32) -> Status {
        Status::Ok
    }

    fn set_recv_timeout(&self, _io: Io, _ms: u32) -> Status {
        Status::Ok
    }

    fn set_send_timeout(&self, _io: Io, _ms: u32) -> Status {
        Status::Ok
    }

    fn set_reuseaddr(&self, _io: Io, _on: u32) -> Status {
        Status::Ok
    }

    fn poll(&self, handles: &[Io], events: &[PollEvents], _: i32) -> Vec<PollEvents> {
        // In-memory data is always ready, so readiness just mirrors the requested
        // interest: a known handle reports `revents == events`, an unknown one
        // reports nothing. The deterministic in-memory oracle — any scheduler
        // resolves in a single step under test.
        let table = self.table.lock().unwrap();

        handles
            .iter()
            .enumerate()
            .map(|(slot, handle)| {
                let known = match handle {
                    Io::Stdin | Io::Stdout | Io::Stderr => true,
                    Io::Other(token) => table.contains_key(token),
                };

                if known {
                    events.get(slot).copied().unwrap_or_else(PollEvents::empty)
                } else {
                    PollEvents::empty()
                }
            })
            .collect()
    }

    fn close(&self, io: Io) {
        self.table.lock().unwrap().remove(&io.token());
    }

    fn read(&self, io: Io, count: u32) -> (Status, Vec<u8>) {
        let handle = match io {
            Io::Stdin => {
                // Scripted stdin is one pre-joined buffer; serve up to `count` of
                // its front and report EOF once it is drained (the sender is fixed
                // at build time, so a dry buffer is end-of-input, never a wait).
                let mut input = self.input.lock().unwrap();

                if input.is_empty() {
                    return (Status::Eof, vec![]);
                }

                let served = input.len().min(count as usize);

                return (Status::Ok, input.drain(..served).collect());
            }
            Io::Other(handle) => handle,
            // stdout/stderr are not readable.
            _ => return (Status::Eof, vec![]),
        };

        match self.table.lock().unwrap().get_mut(&handle) {
            // File-backed handle: serve from the in-memory filesystem.
            Some(MockResource::File(open)) => {
                if open.mode != Mode::Read {
                    return (Status::NotFound, vec![]);
                }

                self.files.with(&open.path, |contents| {
                    Self::serve_from(contents, &mut open.position, count)
                })
            }
            // Inbound (accepted) connection: serve the scripted request.
            Some(MockResource::Inbound(conn)) => {
                Self::serve_from(&conn.request, &mut conn.position, count)
            }
            // Outbound connection: serve the scripted response.
            Some(MockResource::Outbound(conn)) => {
                Self::serve_from(&conn.response, &mut conn.position, count)
            }
            _ => (Status::Eof, vec![]),
        }
    }

    fn write(&self, io: Io, bytes: &[u8]) -> Status {
        let handle = match io {
            Io::Stdout | Io::Stderr => {
                self.output.lock().unwrap().extend_from_slice(bytes);

                return Status::Ok;
            }
            Io::Other(handle) => handle,
            // stdin is not writable; the guest's `/std/Io` never issues this.
            Io::Stdin => panic!("write to stdin"),
        };

        match self.table.lock().unwrap().get(&handle) {
            // File-backed handle: append to the in-memory filesystem.
            Some(MockResource::File(open)) => {
                if open.mode == Mode::Read {
                    return Status::NotFound;
                }

                self.files.append(&open.path, bytes);

                Status::Ok
            }
            // Inbound (accepted) connection: capture the response bytes so a
            // test can inspect what the server wrote back.
            Some(MockResource::Inbound(conn)) => {
                self.captures.lock().unwrap()[conn.capture].extend_from_slice(bytes);

                Status::Ok
            }
            // Outbound connection: accept and discard (the in-memory test host
            // does not capture request bytes).
            Some(MockResource::Outbound(_)) => Status::Ok,
            _ => Status::NotFound,
        }
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
        self.args.clone()
    }

    fn env(&self, name: &[u8]) -> (Status, Vec<u8>) {
        match self.env.get(name) {
            Some(value) => (Status::Ok, value.clone()),
            None => (Status::NotFound, vec![]),
        }
    }
}

/// The inspectable side of a [`MockHost`]: the shared buffers the run writes
/// into. The host is moved into the runner, so a test holds this handle to read
/// stdout, files, and server captures back out afterwards.
pub struct MockIo {
    output: Arc<Mutex<Vec<u8>>>,
    files: MockFileSystem,
    captures: Arc<Mutex<Vec<Vec<u8>>>>,
}

impl MockIo {
    /// Every byte the guest wrote to stdout and stderr, concatenated in write
    /// order. The two streams are not distinguished.
    pub fn output(&self) -> Vec<u8> {
        self.output.lock().unwrap().clone()
    }

    /// The contents of `path` in the in-memory filesystem after the run, or
    /// `None` if it was never seeded or written.
    pub fn file(&self, path: &[u8]) -> Option<Vec<u8>> {
        self.files.get(path)
    }

    /// The captured server responses: one entry per accepted connection, the
    /// concatenation of the bytes its handler wrote back.
    pub fn captures(&self) -> Vec<Vec<u8>> {
        self.captures.lock().unwrap().clone()
    }
}

/// Fluent seed for a [`MockHost`]: gather the scripted inputs (stdin, files,
/// network endpoints, clocks, …) as plain values, then [`build`](Self::build)
/// wraps them for the run and hands back the host and its [`MockIo`].
pub struct MockHostBuilder {
    input: Vec<u8>,
    files: HashMap<Vec<u8>, Vec<u8>>,
    endpoints: HashMap<Vec<u8>, Vec<u8>>,
    inbound: VecDeque<Vec<u8>>,
    clock_wall_seq: VecDeque<(u32, u32, u32)>,
    clock_mono_seq: VecDeque<(u32, u32)>,
    rng: u64,
    args: Vec<Vec<u8>>,
    env: HashMap<Vec<u8>, Vec<u8>>,
}

impl Default for MockHostBuilder {
    fn default() -> Self {
        Self {
            input: Vec::new(),
            files: HashMap::new(),
            endpoints: HashMap::new(),
            inbound: VecDeque::new(),
            clock_wall_seq: VecDeque::new(),
            clock_mono_seq: VecDeque::new(),
            // A fixed non-zero xorshift64 seed: deterministic across runs.
            rng: 0x2545_F491_4F6C_DD1D,
            args: Vec::new(),
            env: HashMap::new(),
        }
    }
}

impl MockHostBuilder {
    /// Append one line to scripted stdin; the newline the terminal would
    /// deliver is appended for you.
    pub fn stdin_line(mut self, line: impl AsRef<[u8]>) -> Self {
        self.input.extend_from_slice(line.as_ref());
        self.input.push(b'\n');
        self
    }

    /// Append several newline-terminated lines to scripted stdin, in order.
    pub fn stdin_lines<L: AsRef<[u8]>, I: IntoIterator<Item = L>>(mut self, lines: I) -> Self {
        for line in lines {
            self = self.stdin_line(line);
        }

        self
    }

    /// Seed the in-memory filesystem with `(path, contents)` entries.
    pub fn files<P, C, I>(mut self, files: I) -> Self
    where
        P: AsRef<[u8]>,
        C: AsRef<[u8]>,
        I: IntoIterator<Item = (P, C)>,
    {
        self.files.extend(
            files
                .into_iter()
                .map(|(path, contents)| (path.as_ref().to_vec(), contents.as_ref().to_vec())),
        );

        self
    }

    /// Script the network endpoints served by `connect`: `(host:port, response)`
    /// pairs. Connecting to an unscripted endpoint is refused.
    pub fn net<E, R, I>(mut self, endpoints: I) -> Self
    where
        E: AsRef<[u8]>,
        R: AsRef<[u8]>,
        I: IntoIterator<Item = (E, R)>,
    {
        self.endpoints.extend(
            endpoints.into_iter().map(|(endpoint, response)| {
                (endpoint.as_ref().to_vec(), response.as_ref().to_vec())
            }),
        );

        self
    }

    /// Script the inbound requests served by `accept`, one per accepted
    /// connection (FIFO). An exhausted queue makes `accept` fail, which ends a
    /// `serve` loop (a real blocking `accept` would park there).
    pub fn inbound<R: AsRef<[u8]>, I: IntoIterator<Item = R>>(mut self, requests: I) -> Self {
        self.inbound
            .extend(requests.into_iter().map(|r| r.as_ref().to_vec()));

        self
    }

    /// Script the wall-clock readings served by `clock_wall`, in order. When the
    /// script is exhausted `clock_wall` falls back to `(0, 0, 0)`.
    pub fn wall<I: IntoIterator<Item = (u32, u32, u32)>>(mut self, readings: I) -> Self {
        self.clock_wall_seq.extend(readings);

        self
    }

    /// Script the monotonic readings served by `clock_mono`, in order.
    pub fn mono<I: IntoIterator<Item = (u32, u32)>>(mut self, readings: I) -> Self {
        self.clock_mono_seq.extend(readings);

        self
    }

    /// Seed the deterministic RNG backing `random` (must be non-zero).
    pub fn seed(mut self, seed: u64) -> Self {
        self.rng = seed;

        self
    }

    /// Set the process arguments served by `args` (argv[0] is the program name).
    pub fn args<A: AsRef<[u8]>, I: IntoIterator<Item = A>>(mut self, args: I) -> Self {
        self.args = args.into_iter().map(|a| a.as_ref().to_vec()).collect();

        self
    }

    /// Set the environment served by `env`: `(name, value)` pairs.
    pub fn env<N, V, I>(mut self, vars: I) -> Self
    where
        N: AsRef<[u8]>,
        V: AsRef<[u8]>,
        I: IntoIterator<Item = (N, V)>,
    {
        self.env = vars
            .into_iter()
            .map(|(name, value)| (name.as_ref().to_vec(), value.as_ref().to_vec()))
            .collect();

        self
    }

    /// Wrap the seeded values into a live host and its [`MockIo`] inspection
    /// handle: the host is moved into the runner, the handle stays behind.
    pub fn build(self) -> (MockHost, MockIo) {
        let output = Arc::new(Mutex::new(Vec::new()));
        let files = MockFileSystem::new(self.files);
        let captures = Arc::new(Mutex::new(Vec::new()));

        let io = MockIo {
            output: output.clone(),
            files: files.clone(),
            captures: captures.clone(),
        };

        let host = MockHost {
            handle_seed: AtomicU32::new(Io::HANDLE_SEED),
            input: Mutex::new(self.input.into()),
            output,
            files,
            table: Mutex::new(HashMap::new()),
            endpoints: self.endpoints,
            inbound: Mutex::new(self.inbound),
            captures,
            clock_wall_seq: Mutex::new(self.clock_wall_seq),
            clock_mono_seq: Mutex::new(self.clock_mono_seq),
            rng: Mutex::new(self.rng),
            args: self.args,
            env: self.env,
        };

        (host, io)
    }
}
