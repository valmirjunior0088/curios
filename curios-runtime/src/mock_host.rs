use {
    super::{Table, host::*},
    curios_abi::{event, file_kind, stdio_mode},
    std::{
        collections::{BTreeSet, HashMap, VecDeque},
        sync::{Arc, Mutex},
    },
};

/// The in-memory disk: files as `path → contents` and the set of directories. Seeding a file implies every directory above it, so a seeded tree can be walked, listed and removed as a real one is. The root is not in the set: it always exists, spelled [`ROOT`] when named and as the empty parent `parent_of` gives an absolute path's first component, so an absolute path is filed and found as a relative one is.
#[derive(Default)]
struct MockDisk {
    files: HashMap<Vec<u8>, Vec<u8>>,
    dirs: BTreeSet<Vec<u8>>,
}

/// The root directory as a path names it. Its children have the empty parent, since the separator before them is the whole of it.
const ROOT: &[u8] = b"/";

/// `EBUSY`, the errno `rmdir(2)` reports on the root — `16` on both release targets, Linux and macOS.
const EBUSY: u32 = 16;

/// The path above `path` — the bytes before its last `/` — or `None` for a bare name.
fn parent_of(path: &[u8]) -> Option<&[u8]> {
    path.iter()
        .rposition(|&byte| byte == b'/')
        .map(|slash| &path[..slash])
}

impl MockDisk {
    /// Record `path` and every directory above it as directories.
    fn imply_dirs(&mut self, mut path: &[u8]) {
        while let Some(parent) = parent_of(path) {
            if parent.is_empty() {
                break;
            }

            self.dirs.insert(parent.to_vec());
            path = parent;
        }
    }

    /// Whether `path` is a directory: the root, or one recorded.
    fn is_dir(&self, path: &[u8]) -> bool {
        path == ROOT || self.dirs.contains(path)
    }

    /// Whether `path` names something under a directory that is not there — the refusal `create_dir` and a writing `open` share, since the OS answers both with `not_found`. A bare name has no parent to miss, and an empty parent is the root, which is always there.
    fn parent_missing(&self, path: &[u8]) -> bool {
        parent_of(path).is_some_and(|parent| !parent.is_empty() && !self.dirs.contains(parent))
    }

    /// The names directly inside directory `dir`, files and directories alike, in byte order.
    fn children(&self, dir: &[u8]) -> Vec<Vec<u8>> {
        let dir = match dir == ROOT {
            true => &[][..],
            false => dir,
        };
        let name_in = |path: &[u8]| -> Option<Vec<u8>> {
            (parent_of(path) == Some(dir)).then(|| path[dir.len() + 1..].to_vec())
        };

        self.files
            .keys()
            .chain(self.dirs.iter())
            .filter_map(|path| name_in(path))
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect()
    }
}

/// The disk behind a shared lock. A live [`MockHost`] writes it during the run; the [`MockIo`] handle a test holds reads it back afterwards. `clone` shares the one underlying disk.
#[derive(Clone)]
struct MockFileSystem {
    inner: Arc<Mutex<MockDisk>>,
}

impl MockFileSystem {
    /// Wrap a seeded `path → contents` map and a set of directories, every ancestor of either implied.
    fn new(files: HashMap<Vec<u8>, Vec<u8>>, dirs: BTreeSet<Vec<u8>>) -> Self {
        let mut disk = MockDisk { files, dirs };

        for path in disk.files.keys().cloned().collect::<Vec<_>>() {
            disk.imply_dirs(&path);
        }

        for dir in disk.dirs.clone() {
            disk.imply_dirs(&dir);
        }

        Self {
            inner: Arc::new(Mutex::new(disk)),
        }
    }

    /// Whether `path` is a file — `open`'s existence check in read mode.
    fn contains(&self, path: &[u8]) -> bool {
        self.inner.lock().unwrap().files.contains_key(path)
    }

    /// Reset `path` to empty, creating it if absent — `open` in write mode. `NotFound` under a directory that is not there, as the OS answers.
    fn truncate(&self, path: &[u8]) -> Status {
        let mut disk = self.inner.lock().unwrap();

        if disk.parent_missing(path) {
            return Status::NotFound;
        }

        disk.files.insert(path.to_vec(), vec![]);

        Status::Ok
    }

    /// Create `path` empty if absent, leaving any existing contents — `open` in append mode. `NotFound` under a directory that is not there, as `truncate` answers.
    fn ensure(&self, path: &[u8]) -> Status {
        let mut disk = self.inner.lock().unwrap();

        if disk.parent_missing(path) {
            return Status::NotFound;
        }

        disk.files.entry(path.to_vec()).or_default();

        Status::Ok
    }

    /// Append `bytes` to `path`, creating it if absent.
    fn append(&self, path: &[u8], bytes: &[u8]) {
        self.inner
            .lock()
            .unwrap()
            .files
            .entry(path.to_vec())
            .or_default()
            .extend_from_slice(bytes);
    }

    /// Borrow `path`'s contents (empty if absent) under the lock, so a read can serve a slice without cloning the whole file.
    fn with<R>(&self, path: &[u8], serve: impl FnOnce(&[u8]) -> R) -> R {
        let disk = self.inner.lock().unwrap();

        serve(disk.files.get(path).map(Vec::as_slice).unwrap_or(&[]))
    }

    /// A clone of `path`'s contents, or `None` if absent — post-run inspection.
    fn get(&self, path: &[u8]) -> Option<Vec<u8>> {
        self.inner.lock().unwrap().files.get(path).cloned()
    }

    /// `stat`: the kind tag and the size of what is at `path`.
    fn stat(&self, path: &[u8]) -> Option<(u32, usize)> {
        let disk = self.inner.lock().unwrap();

        match disk.files.get(path) {
            Some(contents) => Some((file_kind::FILE, contents.len())),
            None => disk.is_dir(path).then_some((file_kind::DIRECTORY, 0)),
        }
    }

    fn remove_file(&self, path: &[u8]) -> Status {
        let mut disk = self.inner.lock().unwrap();

        match disk.files.remove(path) {
            Some(_) => Status::Ok,
            None if disk.is_dir(path) => Status::IsDirectory,
            None => Status::NotFound,
        }
    }

    /// A file moves alone; a directory moves with everything beneath it, as `rename(2)` does.
    fn rename(&self, from: &[u8], to: &[u8]) -> Status {
        let mut disk = self.inner.lock().unwrap();

        if let Some(contents) = disk.files.remove(from) {
            disk.files.insert(to.to_vec(), contents);

            return Status::Ok;
        }

        if !disk.dirs.remove(from) {
            return Status::NotFound;
        }

        let rebased = |path: &[u8]| [to, &path[from.len()..]].concat();
        let prefixed = |path: &[u8]| path.starts_with(from) && path.get(from.len()) == Some(&b'/');

        disk.dirs.insert(to.to_vec());
        disk.dirs = disk
            .dirs
            .iter()
            .map(|dir| match prefixed(dir) {
                true => rebased(dir),
                false => dir.clone(),
            })
            .collect();
        disk.files = disk
            .files
            .drain()
            .map(|(path, contents)| match prefixed(&path) {
                true => (rebased(&path), contents),
                false => (path, contents),
            })
            .collect();

        Status::Ok
    }

    fn list(&self, path: &[u8]) -> (Status, Vec<Vec<u8>>) {
        let disk = self.inner.lock().unwrap();

        match () {
            () if disk.files.contains_key(path) => (Status::NotDirectory, vec![]),
            () if !disk.is_dir(path) => (Status::NotFound, vec![]),
            () => (Status::Ok, disk.children(path)),
        }
    }

    fn create_dir(&self, path: &[u8]) -> Status {
        let mut disk = self.inner.lock().unwrap();

        match () {
            () if disk.files.contains_key(path) || disk.is_dir(path) => Status::AlreadyExists,
            () if disk.parent_missing(path) => Status::NotFound,
            () => {
                disk.dirs.insert(path.to_vec());

                Status::Ok
            }
        }
    }

    fn remove_dir(&self, path: &[u8]) -> Status {
        let mut disk = self.inner.lock().unwrap();

        match () {
            () if disk.files.contains_key(path) => Status::NotDirectory,
            () if !disk.is_dir(path) => Status::NotFound,
            () if !disk.children(path).is_empty() => Status::NotEmpty,
            // The root cannot be removed even when empty, as `rmdir(2)` reports it.
            () if path == ROOT => Status::Other(EBUSY),
            () => {
                disk.dirs.remove(path);

                Status::Ok
            }
        }
    }
}

struct MockFile {
    path: Vec<u8>,
    mode: Mode,
    position: usize,
}

/// The scripted bytes a stream serves, chunk by chunk, as a peer would deliver them: a `read` serves from the front chunk and, once that chunk is spent, answers `WouldBlock` until a `poll` arms the next one. A flat script is one chunk, armed from the start, so it reads the way it always did; a multi-chunk script is what puts a scheduler's park-poll-resume path under test, which a host that is always ready never could.
struct Chunked {
    chunks: VecDeque<Vec<u8>>,
    position: usize,
    due: bool,
}

impl Chunked {
    fn new(chunks: Vec<Vec<u8>>) -> Self {
        Self {
            chunks: chunks.into(),
            position: 0,
            due: true,
        }
    }

    /// Serve up to `count` bytes of the front chunk: `Eof` once no chunk is left, `WouldBlock` while the next chunk is not yet due, and the chunk's tail otherwise, disarming the stream when the chunk is spent. An empty chunk is skipped rather than served as a zero-byte read.
    fn read(&mut self, count: u32) -> (Status, Vec<u8>) {
        while self.chunks.front().is_some_and(|chunk| chunk.is_empty()) {
            self.chunks.pop_front();
        }

        let Some(front) = self.chunks.front() else {
            return (Status::Eof, vec![]);
        };

        if !self.due {
            return (Status::WouldBlock, vec![]);
        }

        let stop = front.len().min(self.position + count as usize);
        let bytes = front[self.position..stop].to_vec();
        self.position = stop;

        if stop >= front.len() {
            self.chunks.pop_front();
            self.position = 0;
            self.due = false;
        }

        (Status::Ok, bytes)
    }

    /// Make the next chunk due — what a `poll` reporting the handle readable means.
    fn arm(&mut self) {
        self.due = true;
    }
}

/// A live in-memory *inbound* connection minted by `accept`: `read` serves the scripted request, and `write` appends to `captures[capture]` so a test can inspect what the server sent back.
struct MockServer {
    bytes: Chunked,
    capture: usize,
}

/// A scripted child: what it writes on each stream and how it ends, keyed by program name in the builder.
#[derive(Clone)]
struct MockChildScript {
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    code: u32,
    signal: u32,
}

/// A live scripted child: exited the moment it was spawned, its handle ready and its exit waiting for `wait`, its piped streams filed and handed out through `stream`.
struct MockChild {
    program: Vec<u8>,
    code: u32,
    signal: u32,
    streams: [Handle; 3],
}

/// A non-stdio handle in [`MockHost`]'s unified table — the scripted, in-memory mirror of `OsHost`'s `OsResource`. The BSD lifecycle moves a handle between states: `socket` mints a `Socket`, `connect` turns it into an `Outbound` stream, `listen` turns it into a `Listener` that `accept` pulls `Inbound` streams from; `open` files a `File`; `spawn` files a `Child` with a `Piped` stream per piped output and a `Sink` for a piped stdin. `close` drops any kind.
enum MockResource {
    File(MockFile),
    Child(MockChild),
    /// A child's piped output as the parent reads it.
    Piped(Chunked),
    /// A piped stdin of a scripted child: writes are accepted and discarded.
    Sink,
    /// A finished name lookup minted by `lookup`, holding the resolved address blobs `resolve` drains. The scripted host resolves synchronously, so the handle is ready the moment it is minted.
    Resolved(Vec<Vec<u8>>),
    /// A scripted connect under way: what `finish_connect` will answer — the response to serve, or the refusal — once a `poll` has marked it due.
    Connecting {
        outcome: Result<Chunked, Status>,
        due: bool,
    },
    /// A live *outbound* connection: the scripted response. Writes to it are accepted and discarded.
    Outbound(Chunked),
    Inbound(MockServer),
    Socket,
    Listener,
    /// A server TLS config token minted by `tls_server_config`. The scripted host runs cleartext, so it carries no real configuration — it only marks the handle so `start_tls_server` can recognise it.
    TlsConfig,
}

/// The scripted, in-memory `Host` used by the test suite — the mirror of `OsHost`. Build one with [`MockHost::builder`], move it into the runner, and read what the run produced through the [`MockIo`] handle `build` returns.
pub struct MockHost {
    /// Scripted stdin, served chunk by chunk as the terminal or the pipe behind it delivers: `read(Handle::Stdin, …)` drains the front chunk, answers `Status::WouldBlock` until a `poll` arms the next one, and reports `Status::Eof` once the script is spent. A script of lines is one chunk, armed from the start, so it reads the way it always did; a multi-chunk script is what puts a fiber's park-poll-resume path over standard input under test, which a host that is always ready never could.
    input: Mutex<Chunked>,
    /// Every byte written to stdout and stderr, concatenated in write order. Shared with [`MockIo::output`], which is what a fixture reads when it only cares that something was written.
    output: Arc<Mutex<Vec<u8>>>,
    /// The stderr half alone, written beside `output` rather than instead of it. Shared with [`MockIo::errors`]: a program that reports a failure on one stream and its result on the other is only pinned by a fixture that can tell them apart.
    errors: Arc<Mutex<Vec<u8>>>,
    /// The in-memory filesystem backing `open`/`read`/`write`/`close`. Shared with [`MockIo::file`].
    files: MockFileSystem,
    /// One table for every non-stdio handle, keyed by token bytes: open files, outbound/inbound connections, and unconnected/listening sockets. The BSD lifecycle transitions a handle in place (`socket` → `connect`/`listen` → `accept`) and `close` releases any kind uniformly — the scripted mirror of `OsHost`'s real-resource table.
    table: Mutex<Table<MockResource>>,
    /// Scripted network endpoints: `host:port` → the chunks a connection serves on read. Read-only during the run; connecting elsewhere is refused.
    endpoints: HashMap<Vec<u8>, Vec<Vec<u8>>>,
    /// Scripted inbound requests as chunk lists, one served per `accept` (FIFO).
    inbound: Mutex<VecDeque<Vec<Vec<u8>>>>,
    /// Whether `connect` answers `WouldBlock` and settles through `poll` and `finish_connect`, as a connect to a remote peer does, rather than at once as loopback does.
    connect_pending: bool,
    /// Captured server responses: one entry per accepted connection, the concatenation of its writes. Shared with [`MockIo::captures`].
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
    /// Every mode `raw` was asked for, in order. Shared with [`MockIo::raw_modes`], so a test can see that a bracket switched raw mode on and back off.
    raw_modes: Arc<Mutex<Vec<bool>>>,
    /// The scripted terminal size `size` answers; `None` is a host with no terminal, which answers `ENOTTY` as the native host does.
    tty_size: Option<(u32, u32)>,
    /// The scripted working directory `cwd` answers.
    cwd: Vec<u8>,
    /// Scripted children by program name: what `spawn` finds.
    children: HashMap<Vec<u8>, MockChildScript>,
    /// The program names of every child `kill` was asked to end, in order. Shared with [`MockIo::kills`], so a test can see that a cancelled task killed what it spawned.
    kills: Arc<Mutex<Vec<Vec<u8>>>>,
}

/// `ENOTTY`, the errno a terminal `ioctl` reports on a descriptor that is not a terminal — `25` on both release targets, Linux and macOS.
const ENOTTY: u32 = 25;

impl MockHost {
    /// Start seeding a host. Chain the `stdin_lines`/`files`/`net`/… setters, then `build` for the `(host, io)` pair.
    pub fn builder() -> MockHostBuilder {
        MockHostBuilder::default()
    }

    /// Mint a fresh handle for `resource` under the table lock (see [`Table::mint`]).
    fn mint(&self, resource: MockResource) -> Handle {
        self.table.lock().unwrap().mint(resource)
    }
}

impl HostOps for MockHost {
    fn open(&self, path: &[u8], mode: Mode) -> (Status, Handle) {
        let status = match mode {
            Mode::Read => match self.files.contains(path) {
                true => Status::Ok,
                false => Status::NotFound,
            },
            Mode::Write => self.files.truncate(path),
            Mode::Append => self.files.ensure(path),
        };

        if !matches!(status, Status::Ok) {
            return (status, Handle::none());
        }

        (
            Status::Ok,
            self.mint(MockResource::File(MockFile {
                path: path.to_vec(),
                mode,
                position: 0,
            })),
        )
    }

    fn lookup(&self, host: &[u8], port: u32) -> (Status, Handle) {
        // One synthetic address blob: the `host:port` key `net` uses, so `connect` can recover the scripted endpoint from the blob. Stashed behind a handle `poll` reports ready and `resolve` drains, mirroring the async OS path without a real pipe.
        let endpoint = format!("{}:{port}", String::from_utf8_lossy(host)).into_bytes();

        (
            Status::Ok,
            self.mint(MockResource::Resolved(vec![endpoint])),
        )
    }

    fn resolve(&self, handle: Handle) -> (Status, Vec<Vec<u8>>) {
        match self.table.lock().unwrap().remove(&handle) {
            Some(MockResource::Resolved(addresses)) => (Status::Ok, addresses),
            _ => (Status::NotFound, vec![]),
        }
    }

    fn socket(&self, _addr: &[u8]) -> (Status, Handle) {
        (Status::Ok, self.mint(MockResource::Socket))
    }

    fn bind(&self, io: Handle, _addr: &[u8]) -> Status {
        if matches!(
            self.table.lock().unwrap().get(&io),
            Some(MockResource::Socket)
        ) {
            Status::Ok
        } else {
            Status::NotFound
        }
    }

    fn connect(&self, io: Handle, addr: &[u8]) -> Status {
        // The handle must be an unconnected socket minted by `socket`; consume it up front so a refusal leaves no half-open handle behind.
        {
            let mut table = self.table.lock().unwrap();

            match table.get(&io) {
                Some(MockResource::Socket) => {
                    table.remove(&io);
                }
                _ => return Status::NotFound,
            }
        }

        let outcome = match self.endpoints.get(addr) {
            Some(response) => Ok(Chunked::new(response.clone())),
            None => Err(Status::ConnectionRefused),
        };

        // A pending connect defers its outcome, refusal included, to `finish_connect` after a poll, as the OS reports a refusal through `SO_ERROR`; a synchronous one answers here, as loopback does.
        if self.connect_pending {
            self.table.lock().unwrap().insert(
                &io,
                MockResource::Connecting {
                    outcome,
                    due: false,
                },
            );

            return Status::WouldBlock;
        }

        match outcome {
            Ok(response) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, MockResource::Outbound(response));

                Status::Ok
            }
            Err(status) => status,
        }
    }

    fn finish_connect(&self, io: Handle) -> Status {
        let mut table = self.table.lock().unwrap();

        match table.get(&io) {
            Some(MockResource::Connecting { due: false, .. }) => Status::WouldBlock,
            Some(MockResource::Connecting { due: true, .. }) => {
                let Some(MockResource::Connecting { outcome, .. }) = table.remove(&io) else {
                    unreachable!("the slot was just read");
                };

                match outcome {
                    Ok(response) => {
                        table.insert(&io, MockResource::Outbound(response));

                        Status::Ok
                    }
                    Err(status) => status,
                }
            }
            Some(MockResource::Outbound(_)) => Status::Ok,
            _ => Status::NotFound,
        }
    }

    fn start_tls(&self, io: Handle, _sni: &[u8]) -> Status {
        // The scripted host serves cleartext; a client TLS upgrade is a no-op identity over the existing outbound connection.
        if matches!(
            self.table.lock().unwrap().get(&io),
            Some(MockResource::Outbound(_))
        ) {
            Status::Ok
        } else {
            Status::NotFound
        }
    }

    fn tls_server_config(&self, _cert: &[u8], _key: &[u8]) -> (Status, Handle) {
        // No real config under test — just mint a token the handle table can hand back to `start_tls_server`.
        (Status::Ok, self.mint(MockResource::TlsConfig))
    }

    fn start_tls_server(&self, io: Handle, cfg: Handle) -> Status {
        // A no-op identity over the accepted connection, given a config token.
        let table = self.table.lock().unwrap();

        let has_config = matches!(table.get(&cfg), Some(MockResource::TlsConfig));
        let has_conn = matches!(table.get(&io), Some(MockResource::Inbound(_)));

        if has_config && has_conn {
            Status::Ok
        } else {
            Status::NotFound
        }
    }

    fn listen(&self, io: Handle, _backlog: u32) -> Status {
        let mut table = self.table.lock().unwrap();

        match table.get(&io) {
            Some(MockResource::Socket) => {
                table.insert(&io, MockResource::Listener);
                Status::Ok
            }
            _ => Status::NotFound,
        }
    }

    fn accept(&self, io: Handle) -> (Status, Handle) {
        if !matches!(
            self.table.lock().unwrap().get(&io),
            Some(MockResource::Listener)
        ) {
            return (Status::NotFound, Handle::none());
        }

        // Pull the next scripted request. An exhausted queue fails the accept, ending the serve loop (a real blocking accept would park forever).
        let request = match self.inbound.lock().unwrap().pop_front() {
            Some(request) => request,
            None => return (Status::NotFound, Handle::none()),
        };

        let capture = {
            let mut captures = self.captures.lock().unwrap();
            let index = captures.len();
            captures.push(Vec::new());
            index
        };

        (
            Status::Ok,
            self.mint(MockResource::Inbound(MockServer {
                bytes: Chunked::new(request),
                capture,
            })),
        )
    }

    fn set_reuseaddr(&self, _io: Handle, _on: u32) -> Status {
        Status::Ok
    }

    fn poll(&self, handles: &[Handle], events: &[Poll], _: i32) -> Vec<Poll> {
        // Readiness is what the script says is due, and never a wait: the write ends and files mirror the requested interest, standard input and a scripted stream are armed for their next chunk and reported readable (a stream's end counts as readable, as an OS reports a closed peer) plus writable where asked, and an unknown handle reports nothing. Arming here is what makes one `poll` one chunk of progress, so a scheduler's park-poll-resume path is taken exactly once per chunk boundary.
        let mut table = self.table.lock().unwrap();

        handles
            .iter()
            .enumerate()
            .map(|(slot, handle)| {
                let requested = events.get(slot).copied().unwrap_or_else(Poll::empty);
                let readable = Poll::from_bits(event::READ | (requested.bits() & event::WRITE));

                match handle {
                    // Standard input is armed like a scripted stream, so one `poll` is one chunk of progress and a fiber parked on `WouldBlock` resumes into the next chunk. The write ends have nothing to arm.
                    Handle::Stdin => {
                        self.input.lock().unwrap().arm();

                        requested
                    }
                    Handle::Stdout | Handle::Stderr => requested,
                    Handle::Other(_) => match table.get_mut(handle) {
                        Some(MockResource::Outbound(stream) | MockResource::Piped(stream)) => {
                            stream.arm();

                            readable
                        }
                        Some(MockResource::Inbound(conn)) => {
                            conn.bytes.arm();

                            readable
                        }
                        Some(MockResource::Connecting { due, .. }) => {
                            *due = true;

                            Poll::from_bits(event::WRITE)
                        }
                        Some(_) => requested,
                        None => Poll::empty(),
                    },
                }
            })
            .collect()
    }

    fn close(&self, io: Handle) {
        self.table.lock().unwrap().remove(&io);
    }

    fn read(&self, io: Handle, count: u32) -> (Status, Vec<u8>) {
        match &io {
            // Standard input is a scripted stream like any other: the front chunk, `WouldBlock` between chunks, `Eof` once the script is spent. `OsHost` gates its own stdin read by a zero-timeout poll and answers `WouldBlock` when nothing is there, so a script that hands the wait back is the faithful mirror rather than a convenience.
            Handle::Stdin => return self.input.lock().unwrap().read(count),
            Handle::Other(_) => {}
            // stdout/stderr are not readable.
            _ => return (Status::Eof, vec![]),
        }

        match self.table.lock().unwrap().get_mut(&io) {
            // File-backed handle: serve from the in-memory filesystem.
            Some(MockResource::File(open)) => {
                if open.mode != Mode::Read {
                    return (Status::NotFound, vec![]);
                }

                self.files.with(&open.path, |contents| {
                    serve_from(contents, &mut open.position, count)
                })
            }
            // Inbound (accepted) connection: serve the scripted request.
            Some(MockResource::Inbound(conn)) => conn.bytes.read(count),
            // Outbound connection and a child's piped output: serve the scripted chunks.
            Some(MockResource::Outbound(stream) | MockResource::Piped(stream)) => {
                stream.read(count)
            }
            // A missing or non-stream handle is a fault, not an exhausted stream — mirror write's `NotFound` so use-after-close stays loud.
            _ => (Status::NotFound, vec![]),
        }
    }

    fn write(&self, io: Handle, bytes: &[u8]) -> (Status, u32) {
        // The in-memory sink always takes the whole buffer in one go, so a successful write reports the full length and never `WouldBlock`.
        let full = bytes.len() as u32;

        match &io {
            Handle::Stdout | Handle::Stderr => {
                self.output.lock().unwrap().extend_from_slice(bytes);

                if matches!(io, Handle::Stderr) {
                    self.errors.lock().unwrap().extend_from_slice(bytes);
                }

                return (Status::Ok, full);
            }
            Handle::Other(_) => {}
            // stdin is not writable; the guest's `/sys/Handle` never issues this.
            Handle::Stdin => panic!("write to stdin"),
        }

        match self.table.lock().unwrap().get(&io) {
            // File-backed handle: append to the in-memory filesystem.
            Some(MockResource::File(open)) => {
                if open.mode == Mode::Read {
                    return (Status::NotFound, 0);
                }

                self.files.append(&open.path, bytes);

                (Status::Ok, full)
            }
            // Inbound (accepted) connection: capture the response bytes so a test can inspect what the server wrote back.
            Some(MockResource::Inbound(conn)) => {
                self.captures.lock().unwrap()[conn.capture].extend_from_slice(bytes);

                (Status::Ok, full)
            }
            // Outbound connection: accept and discard (the in-memory test host does not capture request bytes).
            Some(MockResource::Outbound(_)) => (Status::Ok, full),
            // A child's piped stdin: accepted and discarded too.
            Some(MockResource::Sink) => (Status::Ok, full),
            _ => (Status::NotFound, 0),
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

    fn raw(&self, _io: Handle, on: u32) -> Status {
        match self.tty_size {
            Some(_) => {
                self.raw_modes.lock().unwrap().push(on != 0);

                Status::Ok
            }
            None => Status::Other(ENOTTY),
        }
    }

    fn size(&self, _io: Handle) -> (Status, u32, u32) {
        match self.tty_size {
            Some((cols, rows)) => (Status::Ok, cols, rows),
            None => (Status::Other(ENOTTY), 0, 0),
        }
    }

    fn stat(&self, path: &[u8]) -> (Status, u32, u32, u32, u32, u32, u32) {
        // The scripted disk keeps no timestamps, so a modification time is the epoch.
        match self.files.stat(path) {
            Some((kind, size)) => {
                let size = size as u64;

                (
                    Status::Ok,
                    kind,
                    (size / 1_000_000_000) as u32,
                    (size % 1_000_000_000) as u32,
                    0,
                    0,
                    0,
                )
            }
            None => (Status::NotFound, 0, 0, 0, 0, 0, 0),
        }
    }

    fn remove_file(&self, path: &[u8]) -> Status {
        self.files.remove_file(path)
    }

    fn rename(&self, from: &[u8], to: &[u8]) -> Status {
        self.files.rename(from, to)
    }

    fn list(&self, path: &[u8]) -> (Status, Vec<Vec<u8>>) {
        self.files.list(path)
    }

    fn create_dir(&self, path: &[u8]) -> Status {
        self.files.create_dir(path)
    }

    fn remove_dir(&self, path: &[u8]) -> Status {
        self.files.remove_dir(path)
    }

    fn cwd(&self) -> (Status, Vec<u8>) {
        (Status::Ok, self.cwd.clone())
    }

    fn spawn(
        &self,
        argv: &[Vec<u8>],
        _cwd: &[u8],
        _env: &[Vec<u8>],
        stdin: u32,
        stdout: u32,
        stderr: u32,
    ) -> (Status, Handle) {
        // An unscripted program is one the host cannot find, as an unknown path is to `open`; the script is keyed by `argv[0]`.
        let Some(script) = argv
            .first()
            .and_then(|program| self.children.get(program))
            .cloned()
        else {
            return (Status::NotFound, Handle::none());
        };
        let program = &argv[0];

        // Each stream is filed only where the guest asked for a pipe; the scripted child has already written everything it ever will.
        let piped = |mode: u32, bytes: Vec<u8>| match mode == stdio_mode::PIPE {
            true => self.mint(MockResource::Piped(Chunked::new(vec![bytes]))),
            false => Handle::none(),
        };
        let stdin = match stdin == stdio_mode::PIPE {
            true => self.mint(MockResource::Sink),
            false => Handle::none(),
        };
        let streams = [
            stdin,
            piped(stdout, script.stdout),
            piped(stderr, script.stderr),
        ];
        let child = self.mint(MockResource::Child(MockChild {
            program: program.to_vec(),
            code: script.code,
            signal: script.signal,
            streams,
        }));

        (Status::Ok, child)
    }

    fn stream(&self, child: Handle, which: u32) -> (Status, Handle) {
        match self.table.lock().unwrap().get(&child) {
            Some(MockResource::Child(running)) => match running.streams.get(which as usize) {
                Some(handle) => (Status::Ok, handle.clone()),
                None => (Status::NotFound, Handle::none()),
            },
            _ => (Status::NotFound, Handle::none()),
        }
    }

    fn wait(&self, child: Handle) -> (Status, u32, u32) {
        match self.table.lock().unwrap().remove(&child) {
            Some(MockResource::Child(ended)) => (Status::Ok, ended.code, ended.signal),
            _ => (Status::NotFound, 0, 0),
        }
    }

    fn kill(&self, child: Handle) -> Status {
        match self.table.lock().unwrap().get(&child) {
            Some(MockResource::Child(running)) => {
                self.kills.lock().unwrap().push(running.program.clone());

                Status::Ok
            }
            _ => Status::NotFound,
        }
    }
}

/// Serve up to `count` bytes of `contents` from `*position`, advancing the cursor; `Status::Eof` with empty bytes once it reaches the end. The shape of a file read, which is always ready; a stream reads through [`Chunked`] instead.
fn serve_from(contents: &[u8], position: &mut usize, count: u32) -> (Status, Vec<u8>) {
    if *position >= contents.len() {
        return (Status::Eof, vec![]);
    }

    let stop = contents.len().min(*position + count as usize);
    let bytes = contents[*position..stop].to_vec();
    *position = stop;

    (Status::Ok, bytes)
}

/// The inspectable side of a [`MockHost`]: the shared buffers the run writes into. The host is moved into the runner, so a test holds this handle to read stdout, files, and server captures back out afterwards.
pub struct MockIo {
    output: Arc<Mutex<Vec<u8>>>,
    errors: Arc<Mutex<Vec<u8>>>,
    files: MockFileSystem,
    captures: Arc<Mutex<Vec<Vec<u8>>>>,
    raw_modes: Arc<Mutex<Vec<bool>>>,
    kills: Arc<Mutex<Vec<Vec<u8>>>>,
}

impl MockIo {
    /// Every byte the guest wrote to stdout and stderr, concatenated in write order.
    pub fn output(&self) -> Vec<u8> {
        self.output.lock().unwrap().clone()
    }

    /// The stderr half of [`output`](Self::output) alone, in write order. Reading both is how a fixture shows which stream a byte went to — a diagnostic belongs on this one and a result on the other, and the concatenation cannot tell them apart.
    pub fn errors(&self) -> Vec<u8> {
        self.errors.lock().unwrap().clone()
    }

    /// The contents of `path` in the in-memory filesystem after the run, or `None` if it was never seeded or written.
    pub fn file(&self, path: &[u8]) -> Option<Vec<u8>> {
        self.files.get(path)
    }

    /// The captured server responses: one entry per accepted connection, the concatenation of the bytes its handler wrote back.
    pub fn captures(&self) -> Vec<Vec<u8>> {
        self.captures.lock().unwrap().clone()
    }

    /// Every raw-mode switch the guest asked for, in order: `true` for on, `false` for off.
    pub fn raw_modes(&self) -> Vec<bool> {
        self.raw_modes.lock().unwrap().clone()
    }

    /// The program names of the children the guest killed, in order.
    pub fn kills(&self) -> Vec<Vec<u8>> {
        self.kills.lock().unwrap().clone()
    }
}

/// Fluent seed for a [`MockHost`]: gather the scripted inputs (stdin, files, network endpoints, clocks, …) as plain values, then [`build`](Self::build) wraps them for the run and hands back the host and its [`MockIo`].
#[derive(Default)]
pub struct MockHostBuilder {
    input: Vec<u8>,
    input_chunks: Vec<Vec<u8>>,
    files: HashMap<Vec<u8>, Vec<u8>>,
    endpoints: HashMap<Vec<u8>, Vec<Vec<u8>>>,
    inbound: VecDeque<Vec<Vec<u8>>>,
    connect_pending: bool,
    clock_wall_seq: VecDeque<(u32, u32, u32)>,
    clock_mono_seq: VecDeque<(u32, u32)>,
    args: Vec<Vec<u8>>,
    env: HashMap<Vec<u8>, Vec<u8>>,
    tty_size: Option<(u32, u32)>,
    dirs: BTreeSet<Vec<u8>>,
    cwd: Option<Vec<u8>>,
    children: HashMap<Vec<u8>, MockChildScript>,
}

impl MockHostBuilder {
    /// Script the children `spawn` can start: `(program, stdout, stderr, code, signal)`, the exit a signal when `signal` is nonzero. Spawning an unscripted program is `NotFound`.
    pub fn children<P, O, E, I>(mut self, children: I) -> Self
    where
        P: AsRef<[u8]>,
        O: AsRef<[u8]>,
        E: AsRef<[u8]>,
        I: IntoIterator<Item = (P, O, E, u32, u32)>,
    {
        self.children.extend(children.into_iter().map(
            |(program, stdout, stderr, code, signal)| {
                (
                    program.as_ref().to_vec(),
                    MockChildScript {
                        stdout: stdout.as_ref().to_vec(),
                        stderr: stderr.as_ref().to_vec(),
                        code,
                        signal,
                    },
                )
            },
        ));

        self
    }

    /// Seed empty directories; the directories above every seeded file exist without being named here.
    pub fn dirs<P: AsRef<[u8]>, I: IntoIterator<Item = P>>(mut self, dirs: I) -> Self {
        self.dirs
            .extend(dirs.into_iter().map(|dir| dir.as_ref().to_vec()));

        self
    }

    /// Script the working directory `cwd` answers; `/` when unset.
    pub fn cwd(mut self, path: impl AsRef<[u8]>) -> Self {
        self.cwd = Some(path.as_ref().to_vec());

        self
    }

    /// Give the host a terminal of `cols` by `rows`: `size` answers it and `raw` records its switches. Without one, both rows answer `ENOTTY`.
    pub fn tty_size(mut self, cols: u32, rows: u32) -> Self {
        self.tty_size = Some((cols, rows));

        self
    }

    /// Append one line to scripted stdin; the newline the terminal would deliver is appended for you.
    fn stdin_line(mut self, line: impl AsRef<[u8]>) -> Self {
        self.input.extend_from_slice(line.as_ref());
        self.input.push(b'\n');

        self
    }

    /// Append several newline-terminated lines to scripted stdin, in order. However many calls write them, the lines are one chunk and are due from the start, so a reader of them never waits.
    pub fn stdin_lines<L: AsRef<[u8]>, I: IntoIterator<Item = L>>(mut self, lines: I) -> Self {
        for line in lines {
            self = self.stdin_line(line);
        }

        self
    }

    /// Append the chunks standard input delivers, verbatim: nothing is terminated for you, and each chunk is served only once a `poll` has armed it, so a reader parks between them. This is how a raw-mode program's keystrokes are scripted — one chunk per burst, `x[0x1b, 0x5b, 0x41]` for an arrow key — and the only way a read of standard input that waits is put under test. Whatever [`stdin_lines`](Self::stdin_lines) wrote precedes these, as the one chunk it is.
    pub fn stdin_chunks<C: AsRef<[u8]>>(mut self, chunks: Vec<C>) -> Self {
        self.input_chunks.extend(chunk_list(chunks));

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

    /// Script the network endpoints served by `connect`: `(host:port, response)` pairs, each response served whole and ready at once. Connecting to an unscripted endpoint is refused.
    pub fn net<E, R, I>(self, endpoints: I) -> Self
    where
        E: AsRef<[u8]>,
        R: AsRef<[u8]>,
        I: IntoIterator<Item = (E, R)>,
    {
        self.net_chunks(
            endpoints
                .into_iter()
                .map(|(endpoint, response)| (endpoint, vec![response])),
        )
    }

    /// Script the network endpoints served by `connect` as `(host:port, chunks)` pairs: a read serves one chunk, and the next is served only after a `poll` has reported the connection readable, so a reader that parks between chunks is exercised.
    pub fn net_chunks<E, C, I>(mut self, endpoints: I) -> Self
    where
        E: AsRef<[u8]>,
        C: AsRef<[u8]>,
        I: IntoIterator<Item = (E, Vec<C>)>,
    {
        self.endpoints.extend(
            endpoints
                .into_iter()
                .map(|(endpoint, chunks)| (endpoint.as_ref().to_vec(), chunk_list(chunks))),
        );

        self
    }

    /// Make every `connect` pend: it answers `WouldBlock`, a `poll` marks the socket writable, and `finish_connect` then answers what a synchronous connect would have — the way a connect to a remote peer settles.
    pub fn connect_pending(mut self) -> Self {
        self.connect_pending = true;

        self
    }

    /// Script the inbound requests served by `accept`, one per accepted connection (FIFO), each served whole and ready at once. An exhausted queue makes `accept` fail, which ends a `serve` loop (a real blocking `accept` would park there).
    pub fn inbound<R: AsRef<[u8]>, I: IntoIterator<Item = R>>(self, requests: I) -> Self {
        self.inbound_chunks(requests.into_iter().map(|request| vec![request]))
    }

    /// Script the inbound requests served by `accept` as chunk lists, one list per accepted connection, served as `net_chunks` serves a response.
    pub fn inbound_chunks<C: AsRef<[u8]>, I: IntoIterator<Item = Vec<C>>>(
        mut self,
        requests: I,
    ) -> Self {
        self.inbound.extend(requests.into_iter().map(chunk_list));

        self
    }

    /// Script the wall-clock readings served by `clock_wall`, in order. When the script is exhausted `clock_wall` falls back to `(0, 0, 0)`.
    pub fn wall<I: IntoIterator<Item = (u32, u32, u32)>>(mut self, readings: I) -> Self {
        self.clock_wall_seq.extend(readings);

        self
    }

    /// Script the monotonic readings served by `clock_mono`, in order.
    pub fn mono<I: IntoIterator<Item = (u32, u32)>>(mut self, readings: I) -> Self {
        self.clock_mono_seq.extend(readings);

        self
    }

    /// Set the process arguments served by `args` (`argv[0]` is the program name).
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

    /// Wrap the seeded values into a live host and its [`MockIo`] inspection handle: the host is moved into the runner, the handle stays behind.
    pub fn build(self) -> (MockHost, MockIo) {
        let output = Arc::new(Mutex::new(Vec::new()));
        let errors = Arc::new(Mutex::new(Vec::new()));
        let files = MockFileSystem::new(self.files, self.dirs);
        let captures = Arc::new(Mutex::new(Vec::new()));
        let raw_modes = Arc::new(Mutex::new(Vec::new()));
        let kills = Arc::new(Mutex::new(Vec::new()));

        let io = MockIo {
            output: output.clone(),
            errors: errors.clone(),
            files: files.clone(),
            captures: captures.clone(),
            raw_modes: raw_modes.clone(),
            kills: kills.clone(),
        };

        let host = MockHost {
            input: Mutex::new(Chunked::new(stdin_script(self.input, self.input_chunks))),
            output,
            errors,
            files,
            table: Mutex::new(Table::new()),
            endpoints: self.endpoints,
            inbound: Mutex::new(self.inbound),
            connect_pending: self.connect_pending,
            captures,
            clock_wall_seq: Mutex::new(self.clock_wall_seq),
            clock_mono_seq: Mutex::new(self.clock_mono_seq),
            // A fixed non-zero xorshift64 seed: deterministic across runs.
            rng: Mutex::new(0x2545_F491_4F6C_DD1D),
            args: self.args,
            env: self.env,
            raw_modes,
            tty_size: self.tty_size,
            cwd: self.cwd.unwrap_or_else(|| b"/".to_vec()),
            children: self.children,
            kills,
        };

        (host, io)
    }
}

/// The chunk script standard input serves: the newline-terminated lines first, as the single armed chunk they have always been, then each scripted chunk in its own right. An empty prefix contributes nothing, so a chunk script begins at its own first chunk.
fn stdin_script(lines: Vec<u8>, chunks: Vec<Vec<u8>>) -> Vec<Vec<u8>> {
    let mut script = Vec::with_capacity(chunks.len() + 1);

    if !lines.is_empty() {
        script.push(lines);
    }

    script.extend(chunks);

    script
}

/// The owned chunk list a script's borrowed chunks become.
fn chunk_list<C: AsRef<[u8]>>(chunks: Vec<C>) -> Vec<Vec<u8>> {
    chunks
        .into_iter()
        .map(|chunk| chunk.as_ref().to_vec())
        .collect()
}

#[cfg(test)]
mod tests;
