use {
    super::host::*,
    crate::Entropy,
    std::{
        collections::{HashMap, VecDeque},
        sync::{
            Arc, Mutex,
            mpsc::{self, Receiver, RecvError, Sender},
        },
    },
};

/// The in-memory file map shared between a [`MockHost`] and the test that
/// seeded it: path → contents, inspectable after the run.
pub type MockFs = Arc<Mutex<HashMap<Vec<u8>, Vec<u8>>>>;

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

/// A non-stdio handle in [`MockHost`]'s unified table — the scripted, in-
/// memory mirror of `OsHost`'s `Resource`. The BSD lifecycle moves a handle
/// between states: `socket` mints a `Socket`, `connect` turns it into an
/// `Outbound` stream, `listen` turns it into a `Listener` that `accept` pulls
/// `Inbound` streams from; `open` files a `File`. `close` drops any kind.
enum Slot {
    File(OpenFile),
    Outbound(NetConn),
    Inbound(ServerConn),
    Socket,
    Listener,
}

/// Serve up to `count` bytes of `contents` from `*position`, advancing the
/// cursor; `STATUS_EOF` with empty bytes once it reaches the end. The shared
/// shape of every scripted read (file, inbound request, outbound response).
fn serve_from(contents: &[u8], position: &mut usize, count: u32) -> (u32, Vec<u8>) {
    if *position >= contents.len() {
        return (STATUS_EOF, vec![]);
    }

    let stop = contents.len().min(*position + count as usize);
    let bytes = contents[*position..stop].to_vec();
    *position = stop;

    (STATUS_OK, bytes)
}

pub struct MockHost {
    input_receiver: Mutex<Receiver<Vec<u8>>>,
    /// Bytes received from the channel but not yet consumed by `read` —
    /// short reads must never drop the remainder of a message.
    input_leftover: Mutex<Vec<u8>>,
    /// Writes to stdout and stderr both land here; tests do not distinguish
    /// the two streams.
    output_sender: Arc<Mutex<Sender<Vec<u8>>>>,
    /// The in-memory filesystem backing `open`/`close` and file handles.
    files: MockFs,
    /// One table for every non-stdio handle, keyed by token: open files,
    /// outbound/inbound connections, and unconnected/listening sockets. The BSD
    /// lifecycle transitions a handle in place (`socket` → `connect`/`listen` →
    /// `accept`) and `close` releases any kind uniformly — the scripted mirror
    /// of `OsHost`'s real-resource table.
    table: Mutex<HashMap<u32, Slot>>,
    /// Scripted network endpoints: `host:port` → the bytes a connection serves
    /// on read. Connecting to an unscripted endpoint is refused.
    endpoints: Mutex<HashMap<Vec<u8>, Vec<u8>>>,
    /// Scripted inbound requests, one served per `accept` (FIFO).
    inbound: Mutex<VecDeque<Vec<u8>>>,
    /// Captured server responses: one entry per accepted connection, the
    /// concatenation of its writes. Inspectable after the run.
    captures: Arc<Mutex<Vec<Vec<u8>>>>,
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

impl MockHost {
    /// Like `in_out`, but pre-seeds the in-memory filesystem and returns it so
    /// the test can inspect written files after the run.
    pub fn with_fs<L, I, P, C, F>(lines: I, files: F) -> (Self, Receiver<Vec<u8>>, MockFs)
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
            MockHost {
                input_receiver: Mutex::new(input_receiver),
                input_leftover: Mutex::new(Vec::new()),
                output_sender: Arc::new(Mutex::new(output_sender)),
                files: files.clone(),
                table: Mutex::new(HashMap::new()),
                endpoints: Mutex::new(HashMap::new()),
                inbound: Mutex::new(VecDeque::new()),
                captures: Arc::new(Mutex::new(Vec::new())),
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

impl Host for MockHost {
    fn open(&self, path: &[u8], mode: u32) -> (u32, u32) {
        {
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
        }

        let handle = fresh_handle(&self.handles);

        self.table.lock().unwrap().insert(
            handle,
            Slot::File(OpenFile {
                path: path.to_vec(),
                mode,
                position: 0,
            }),
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
        self.table.lock().unwrap().insert(handle, Slot::Socket);

        (STATUS_OK, handle)
    }

    fn bind(&self, handle: u32, _addr: &[u8]) -> u32 {
        if matches!(self.table.lock().unwrap().get(&handle), Some(Slot::Socket)) {
            STATUS_OK
        } else {
            STATUS_OTHER
        }
    }

    fn connect(&self, handle: u32, addr: &[u8]) -> u32 {
        // The handle must be an unconnected socket minted by `socket`; consume
        // it up front so a refusal leaves no half-open handle behind.
        {
            let mut table = self.table.lock().unwrap();

            match table.get(&handle) {
                Some(Slot::Socket) => {
                    table.remove(&handle);
                }
                _ => return STATUS_OTHER,
            }
        }

        let response = match self.endpoints.lock().unwrap().get(addr) {
            Some(response) => response.clone(),
            None => return STATUS_REFUSED,
        };

        self.table.lock().unwrap().insert(
            handle,
            Slot::Outbound(NetConn {
                response,
                position: 0,
            }),
        );

        STATUS_OK
    }

    fn listen(&self, handle: u32, _backlog: u32) -> u32 {
        let mut table = self.table.lock().unwrap();

        match table.get(&handle) {
            Some(Slot::Socket) => {
                table.insert(handle, Slot::Listener);
                STATUS_OK
            }
            _ => STATUS_OTHER,
        }
    }

    fn accept(&self, handle: u32) -> (u32, u32) {
        if !matches!(self.table.lock().unwrap().get(&handle), Some(Slot::Listener)) {
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
        self.table.lock().unwrap().insert(
            conn,
            Slot::Inbound(ServerConn {
                request,
                position: 0,
                capture,
            }),
        );

        (STATUS_OK, conn)
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

    fn close(&self, handle: u32) {
        self.table.lock().unwrap().remove(&handle);
    }

    fn read(&self, handle: u32, count: u32) -> (u32, Vec<u8>) {
        if handle != STDIN {
            return match self.table.lock().unwrap().get_mut(&handle) {
                // File-backed handle: serve from the in-memory filesystem.
                Some(Slot::File(open)) => {
                    if open.mode != MODE_READ {
                        return (STATUS_OTHER, vec![]);
                    }

                    let files = self.files.lock().unwrap();
                    let contents = files.get(&open.path).map(Vec::as_slice).unwrap_or(&[]);

                    serve_from(contents, &mut open.position, count)
                }
                // Inbound (accepted) connection: serve the scripted request.
                Some(Slot::Inbound(conn)) => serve_from(&conn.request, &mut conn.position, count),
                // Outbound connection: serve the scripted response.
                Some(Slot::Outbound(conn)) => {
                    serve_from(&conn.response, &mut conn.position, count)
                }
                _ => (STATUS_EOF, vec![]),
            };
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

        match self.table.lock().unwrap().get(&handle) {
            // File-backed handle: append to the in-memory filesystem.
            Some(Slot::File(open)) => {
                if open.mode == MODE_READ {
                    return STATUS_OTHER;
                }

                self.files
                    .lock()
                    .unwrap()
                    .entry(open.path.clone())
                    .or_default()
                    .extend_from_slice(bytes);

                STATUS_OK
            }
            // Inbound (accepted) connection: capture the response bytes so a
            // test can inspect what the server wrote back.
            Some(Slot::Inbound(conn)) => {
                self.captures.lock().unwrap()[conn.capture].extend_from_slice(bytes);

                STATUS_OK
            }
            // Outbound connection: accept and discard (the in-memory test host
            // does not capture request bytes).
            Some(Slot::Outbound(_)) => STATUS_OK,
            _ => STATUS_OTHER,
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
        self.args.lock().unwrap().clone()
    }

    fn env(&self, name: &[u8]) -> (u32, Vec<u8>) {
        match self.env.lock().unwrap().get(name) {
            Some(value) => (STATUS_OK, value.clone()),
            None => (STATUS_NOT_FOUND, vec![]),
        }
    }
}
