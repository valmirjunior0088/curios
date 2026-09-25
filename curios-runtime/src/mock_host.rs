use {
    super::{Table, host::*},
    curios_abi::{ClosedCode, event},
    std::{
        collections::{BTreeSet, HashMap, VecDeque},
        num::NonZeroU32,
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

/// `EINVAL`, the errno a serial open reports for a frame outside the row's ranges — `22` on both release targets.
const EINVAL: u32 = 22;

/// One serial open as the scripted host records it: the path and `[baud, data_bits, parity, stop_bits, flow]`.
type SerialOpen = (Vec<u8>, [u64; 5]);

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

    /// Whether `path` names something under a directory that is not there — the refusal `dir_create` and a writing `file_open` share, since the OS answers both with `not_found`. A bare name has no parent to miss, and an empty parent is the root, which is always there.
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

    /// Whether `path` is a file — `file_open`'s existence check in read mode.
    fn contains(&self, path: &[u8]) -> bool {
        self.inner.lock().unwrap().files.contains_key(path)
    }

    /// Reset `path` to empty, creating it if absent — `file_open` in write mode. `NotFound` under a directory that is not there, as the OS answers.
    fn truncate(&self, path: &[u8]) -> Result<(), Failure> {
        let mut disk = self.inner.lock().unwrap();

        if disk.parent_missing(path) {
            return Err(Failure::NotFound);
        }

        disk.files.insert(path.to_vec(), vec![]);

        Ok(())
    }

    /// Create `path` empty if absent, leaving any existing contents — `file_open` in append mode. `NotFound` under a directory that is not there, as `truncate` answers.
    fn ensure(&self, path: &[u8]) -> Result<(), Failure> {
        let mut disk = self.inner.lock().unwrap();

        if disk.parent_missing(path) {
            return Err(Failure::NotFound);
        }

        disk.files.entry(path.to_vec()).or_default();

        Ok(())
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

    /// `file_stat`: the kind and the size of what is at `path`.
    fn stat(&self, path: &[u8]) -> Option<(FileKind, usize)> {
        let disk = self.inner.lock().unwrap();

        match disk.files.get(path) {
            Some(contents) => Some((FileKind::File, contents.len())),
            None => disk.is_dir(path).then_some((FileKind::Directory, 0)),
        }
    }

    fn remove_file(&self, path: &[u8]) -> Result<(), Failure> {
        let mut disk = self.inner.lock().unwrap();

        match disk.files.remove(path) {
            Some(_) => Ok(()),
            None if disk.is_dir(path) => Err(Failure::IsDirectory),
            None => Err(Failure::NotFound),
        }
    }

    /// A file moves alone; a directory moves with everything beneath it, as `rename(2)` does.
    fn rename(&self, from: &[u8], to: &[u8]) -> Result<(), Failure> {
        let mut disk = self.inner.lock().unwrap();

        if let Some(contents) = disk.files.remove(from) {
            disk.files.insert(to.to_vec(), contents);

            return Ok(());
        }

        if !disk.dirs.remove(from) {
            return Err(Failure::NotFound);
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

        Ok(())
    }

    fn list(&self, path: &[u8]) -> Result<Vec<Vec<u8>>, Failure> {
        let disk = self.inner.lock().unwrap();

        match () {
            () if disk.files.contains_key(path) => Err(Failure::NotDirectory),
            () if !disk.is_dir(path) => Err(Failure::NotFound),
            () => Ok(disk.children(path)),
        }
    }

    fn create_dir(&self, path: &[u8]) -> Result<(), Failure> {
        let mut disk = self.inner.lock().unwrap();

        match () {
            () if disk.files.contains_key(path) || disk.is_dir(path) => Err(Failure::AlreadyExists),
            () if disk.parent_missing(path) => Err(Failure::NotFound),
            () => {
                disk.dirs.insert(path.to_vec());

                Ok(())
            }
        }
    }

    fn remove_dir(&self, path: &[u8]) -> Result<(), Failure> {
        let mut disk = self.inner.lock().unwrap();

        match () {
            () if disk.files.contains_key(path) => Err(Failure::NotDirectory),
            () if !disk.is_dir(path) => Err(Failure::NotFound),
            () if !disk.children(path).is_empty() => Err(Failure::NotEmpty),
            // The root cannot be removed even when empty, as `rmdir(2)` reports it.
            () if path == ROOT => Err(Failure::Other(EBUSY)),
            () => {
                disk.dirs.remove(path);

                Ok(())
            }
        }
    }
}

struct MockFile {
    path: Vec<u8>,
    mode: Mode,
    position: usize,
}

/// The scripted bytes a stream serves, chunk by chunk, as a peer would deliver them: a `handle_read` serves from the front chunk and, once that chunk is spent, answers `WouldBlock` until a `handle_poll` arms the next one. A flat script is one chunk, armed from the start, so it reads the way it always did; a multi-chunk script is what puts a scheduler's park-poll-resume path under test, which a host that is always ready never could.
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
    fn read(&mut self, count: u64) -> Result<Option<Vec<u8>>, Failure> {
        while self.chunks.front().is_some_and(|chunk| chunk.is_empty()) {
            self.chunks.pop_front();
        }

        let Some(front) = self.chunks.front() else {
            return Ok(None);
        };

        if !self.due {
            return Err(Failure::WouldBlock);
        }

        let stop = front
            .len()
            .min(self.position.saturating_add(count as usize));
        let bytes = front[self.position..stop].to_vec();
        self.position = stop;

        if stop >= front.len() {
            self.chunks.pop_front();
            self.position = 0;
            self.due = false;
        }

        Ok(Some(bytes))
    }

    /// Make the next chunk due — what a `handle_poll` reporting the handle readable means.
    fn arm(&mut self) {
        self.due = true;
    }

    /// Drop what has arrived and not been read — the rest of the front chunk, when it is due — as a driver's input flush does. A chunk not yet due has not arrived, so it survives.
    fn discard(&mut self) {
        if self.due && self.chunks.pop_front().is_some() {
            self.position = 0;
            self.due = false;
        }
    }
}

/// A live in-memory *inbound* connection minted by `socket_accept`: `handle_read` serves the scripted request, and `handle_write` appends to `captures[capture]` so a test can inspect what the server sent back.
struct MockServer {
    bytes: Chunked,
    capture: usize,
}

/// A scripted child: what it writes on each stream and how it ends — or `None`, a child that runs until it is killed — keyed by program name in the builder.
#[derive(Clone)]
struct MockChildScript {
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    exit: Option<ChildExit>,
}

/// A live scripted child, its piped streams filed and handed out through `proc_stream` — `None` for a stream that was not piped. A child scripted with an exit has ended the moment it was spawned, its handle ready and its exit waiting for `proc_wait`; one scripted without runs until `proc_kill` ends it by `SIGKILL`, and until then its handle is not ready and `proc_wait` answers `would_block`.
struct MockChild {
    program: Vec<u8>,
    exit: Option<ChildExit>,
    streams: [Option<Handle>; 3],
}

/// A live scripted serial port minted by `serial_open`: `handle_read` serves the device's scripted chunks, and `handle_write` appends to the capture filed under `path`.
struct MockSerial {
    path: Vec<u8>,
    bytes: Chunked,
}

/// A non-stdio handle in [`MockHost`]'s unified table — the scripted, in-memory mirror of `OsHost`'s `OsResource`. The BSD lifecycle moves a handle between states: `socket_open` mints a `Socket`, `socket_connect` turns it into an `Outbound` stream, `socket_listen` turns it into a `Listener` that `socket_accept` pulls `Inbound` streams from; `file_open` files a `File`; `proc_spawn` files a `Child` with a `Piped` stream per piped output and a `Sink` for a piped stdin; `serial_open` files a `Serial` port. `handle_close` drops any kind.
enum MockResource {
    File(MockFile),
    Child(MockChild),
    /// A child's piped output as the parent reads it.
    Piped(Chunked),
    /// A piped stdin of a scripted child: writes are accepted and discarded.
    Sink,
    /// A finished name lookup minted by `dns_lookup`, holding the resolved address blobs `dns_resolve` drains. The scripted host resolves synchronously, so the handle is ready the moment it is minted.
    Resolved(Vec<Vec<u8>>),
    /// A scripted connect under way: what `socket_finish_connect` will answer — the response to serve, or the refusal — once a `handle_poll` has marked it due.
    Connecting {
        outcome: Result<Chunked, Failure>,
        due: bool,
    },
    /// A live *outbound* connection: the scripted response. Writes to it are accepted and discarded.
    Outbound(Chunked),
    Inbound(MockServer),
    Socket,
    Listener,
    /// A server TLS config token minted by `tls_server_config`. The scripted host runs cleartext, so it carries no real configuration — it only marks the handle so `tls_start_server` can recognise it.
    TlsConfig,
    Serial(MockSerial),
}

/// The scripted, in-memory `Host` used by the test suite — the mirror of `OsHost`. Build one with [`MockHost::builder`], move it into the runner, and read what the run produced through the [`MockIo`] handle `build` returns.
pub struct MockHost {
    /// Scripted stdin, served chunk by chunk as the terminal or the pipe behind it delivers: `handle_read(Handle::Stdin, …)` drains the front chunk, answers `Failure::WouldBlock` until a `handle_poll` arms the next one, and reports the end of the stream once the script is spent. A script of lines is one chunk, armed from the start, so it reads the way it always did; a multi-chunk script is what puts a fiber's park-poll-resume path over standard input under test, which a host that is always ready never could.
    input: Mutex<Chunked>,
    /// Every byte written to stdout and stderr, concatenated in write order. Shared with [`MockIo::output`], which is what a fixture reads when it only cares that something was written.
    output: Arc<Mutex<Vec<u8>>>,
    /// The stderr half alone, written beside `output` rather than instead of it. Shared with [`MockIo::errors`]: a program that reports a failure on one stream and its result on the other is only pinned by a fixture that can tell them apart.
    errors: Arc<Mutex<Vec<u8>>>,
    /// The in-memory filesystem backing `file_open`/`handle_read`/`handle_write`/`handle_close`. Shared with [`MockIo::file`].
    files: MockFileSystem,
    /// One table for every non-stdio handle, keyed by token bytes: open files, outbound/inbound connections, and unconnected/listening sockets. The BSD lifecycle transitions a handle in place (`socket_open` → `socket_connect`/`socket_listen` → `socket_accept`) and `handle_close` releases any kind uniformly — the scripted mirror of `OsHost`'s real-resource table.
    table: Mutex<Table<MockResource>>,
    /// Scripted network endpoints: `host:port` → the chunks a connection serves on read. Read-only during the run; connecting elsewhere is refused.
    endpoints: HashMap<Vec<u8>, Vec<Vec<u8>>>,
    /// Scripted inbound requests as chunk lists, one served per `socket_accept` (FIFO).
    inbound: Mutex<VecDeque<Vec<Vec<u8>>>>,
    /// Whether `socket_connect` answers `WouldBlock` and settles through `handle_poll` and `socket_finish_connect`, as a connect to a remote peer does, rather than at once as loopback does.
    connect_pending: bool,
    /// Captured server responses: one entry per accepted connection, the concatenation of its writes. Shared with [`MockIo::captures`].
    captures: Arc<Mutex<Vec<Vec<u8>>>>,
    /// How many flushes are still to answer `WouldBlock` before one drains, as a TLS stream's do while its records wait for the socket.
    pending_flushes: Mutex<u64>,
    /// Scripted wall-clock readings, served in order by `clock_wall`.
    clock_wall_seq: Mutex<VecDeque<(u64, u64)>>,
    /// Scripted monotonic readings, served in order by `clock_mono`.
    clock_mono_seq: Mutex<VecDeque<(u64, u64)>>,
    /// Deterministic xorshift64 state backing `rand_bytes`.
    rng: Mutex<u64>,
    /// Scripted process arguments served by `proc_args`.
    args: Vec<Vec<u8>>,
    /// Scripted environment served by `proc_env`: name → value.
    env: HashMap<Vec<u8>, Vec<u8>>,
    /// Every mode `tty_raw` was asked for, in order. Shared with [`MockIo::raw_modes`], so a test can see that a bracket switched raw mode on and back off.
    raw_modes: Arc<Mutex<Vec<bool>>>,
    /// Terminal sizes served in order, repeating the last; an empty script answers `ENOTTY` as a host with no terminal does.
    tty_sizes: Mutex<VecDeque<(u64, u64)>>,
    /// The scripted working directory `proc_cwd` answers.
    cwd: Vec<u8>,
    /// Scripted children by program name: what `proc_spawn` finds.
    children: HashMap<Vec<u8>, MockChildScript>,
    /// The program names of every child `proc_kill` ended, in order — a child that had already ended is not signaled, so it is not recorded. Shared with [`MockIo::kills`], so a test can see that a cancelled task killed what it spawned.
    kills: Arc<Mutex<Vec<Vec<u8>>>>,
    /// Scripted serial devices by path: the chunks a port serves while it is open. Opening an unscripted path is `NotFound`.
    serial_devices: HashMap<Vec<u8>, Vec<Vec<u8>>>,
    /// Every serial open that reached a scripted device, in order: its path and `[baud, data_bits, parity, stop_bits, flow]`. Shared with [`MockIo::serial_opens`].
    serial_opens: Arc<Mutex<Vec<SerialOpen>>>,
    /// Every control applied to an open serial port, in order: the op tag and the level. Shared with [`MockIo::serial_controls`].
    serial_controls: Arc<Mutex<Vec<(u64, bool)>>>,
    /// What the program wrote to each serial port, by path. Shared with [`MockIo::serial_written`].
    serial_written: Arc<Mutex<HashMap<Vec<u8>, Vec<u8>>>>,
}

/// `EBADF`, the errno a stream used in a direction it is not open for reports — `9` on both release targets.
const EBADF: u32 = 9;

/// `SIGKILL`, the signal a killed child ends by — `9` on both release targets, Linux and macOS.
const SIGKILL: NonZeroU32 = NonZeroU32::new(9).unwrap();

/// `ENOTTY`, the errno a terminal `ioctl` reports on a descriptor that is not a terminal — `25` on both release targets, Linux and macOS.
const ENOTTY: u32 = 25;

impl MockHost {
    /// Start seeding a host. Chain the `stdin_lines`/`files`/`net`/… setters, then `build` for the `(host, io)` pair.
    pub fn builder() -> MockHostBuilder {
        MockHostBuilder::default()
    }

    /// Whether `handle` names a stream that moves bytes in `direction`, as the native host answers it: a stream open only the other way — a standard stream, a file opened for the other, a child's pipe — is `EBADF`, and anything that is not a stream at all is `NotFound`.
    fn stream(&self, handle: &Handle, direction: Direction) -> Result<(), Failure> {
        let wrong_way = Err(Failure::Other(EBADF));

        match (handle, direction) {
            (Handle::Stdin, Direction::Read)
            | (Handle::Stdout | Handle::Stderr, Direction::Write) => Ok(()),
            (Handle::Stdin | Handle::Stdout | Handle::Stderr, _) => wrong_way,
            (Handle::Other(_), _) => match (self.table.lock().unwrap().get(handle), direction) {
                (Some(MockResource::File(open)), Direction::Read) if open.mode != Mode::Read => {
                    wrong_way
                }
                (Some(MockResource::File(open)), Direction::Write) if open.mode == Mode::Read => {
                    wrong_way
                }
                (Some(MockResource::Sink), Direction::Read)
                | (Some(MockResource::Piped(_)), Direction::Write) => wrong_way,
                (
                    Some(
                        MockResource::File(_)
                        | MockResource::Inbound(_)
                        | MockResource::Outbound(_)
                        | MockResource::Serial(_)
                        | MockResource::Piped(_)
                        | MockResource::Sink,
                    ),
                    _,
                ) => Ok(()),
                _ => Err(Failure::NotFound),
            },
        }
    }

    /// Mint a fresh handle for `resource` under the table lock (see [`Table::mint`]).
    fn mint(&self, resource: MockResource) -> Handle {
        self.table.lock().unwrap().mint(resource)
    }
}

impl HostOps for MockHost {
    fn file_open(&self, path: Vec<u8>, mode: Mode) -> Result<Handle, Failure> {
        match mode {
            Mode::Read => match self.files.contains(&path) {
                true => Ok(()),
                false => Err(Failure::NotFound),
            },
            Mode::Write => self.files.truncate(&path),
            Mode::Append => self.files.ensure(&path),
        }?;

        Ok(self.mint(MockResource::File(MockFile {
            path,
            mode,
            position: 0,
        })))
    }

    fn dns_lookup(&self, host: Vec<u8>, port: u64) -> Result<Handle, Failure> {
        // One synthetic address blob: the `host:port` key `net` uses, so `socket_connect` can recover the scripted endpoint from the blob. Stashed behind a handle `handle_poll` reports ready and `dns_resolve` drains, mirroring the async OS path without a real pipe.
        let endpoint = format!("{}:{port}", String::from_utf8_lossy(&host)).into_bytes();

        Ok(self.mint(MockResource::Resolved(vec![endpoint])))
    }

    fn dns_resolve(&self, handle: Handle) -> Result<Vec<Vec<u8>>, Failure> {
        match self.table.lock().unwrap().remove(&handle) {
            Some(MockResource::Resolved(addresses)) => Ok(addresses),
            _ => Err(Failure::NotFound),
        }
    }

    fn socket_open(&self, _addr: Vec<u8>) -> Result<Handle, Failure> {
        Ok(self.mint(MockResource::Socket))
    }

    fn socket_bind(&self, io: Handle, _addr: Vec<u8>) -> Result<(), Failure> {
        match self.table.lock().unwrap().get(&io) {
            Some(MockResource::Socket) => Ok(()),
            _ => Err(Failure::NotFound),
        }
    }

    fn socket_connect(&self, io: Handle, addr: Vec<u8>) -> Result<(), Failure> {
        // The handle must be an unconnected socket minted by `socket_open`; consume it up front so a refusal leaves no half-open handle behind.
        {
            let mut table = self.table.lock().unwrap();

            match table.get(&io) {
                Some(MockResource::Socket) => {
                    table.remove(&io);
                }
                _ => return Err(Failure::NotFound),
            }
        }

        let outcome = match self.endpoints.get(&addr) {
            Some(response) => Ok(Chunked::new(response.clone())),
            None => Err(Failure::ConnectionRefused),
        };

        // A pending connect defers its outcome, refusal included, to `socket_finish_connect` after a poll, as the OS reports a refusal through `SO_ERROR`; a synchronous one answers here, as loopback does.
        if self.connect_pending {
            self.table.lock().unwrap().insert(
                &io,
                MockResource::Connecting {
                    outcome,
                    due: false,
                },
            );

            return Err(Failure::WouldBlock);
        }

        let response = outcome?;

        self.table
            .lock()
            .unwrap()
            .insert(&io, MockResource::Outbound(response));

        Ok(())
    }

    fn socket_finish_connect(&self, io: Handle) -> Result<(), Failure> {
        let mut table = self.table.lock().unwrap();

        match table.get(&io) {
            Some(MockResource::Connecting { due: false, .. }) => Err(Failure::WouldBlock),
            Some(MockResource::Connecting { due: true, .. }) => {
                let Some(MockResource::Connecting { outcome, .. }) = table.remove(&io) else {
                    unreachable!("the slot was just read");
                };

                let response = outcome?;

                table.insert(&io, MockResource::Outbound(response));

                Ok(())
            }
            Some(MockResource::Outbound(_)) => Ok(()),
            _ => Err(Failure::NotFound),
        }
    }

    fn tls_start(&self, io: Handle, _sni: Vec<u8>) -> Result<(), Failure> {
        // The scripted host serves cleartext; a client TLS upgrade is a no-op identity over the existing outbound connection.
        match self.table.lock().unwrap().get(&io) {
            Some(MockResource::Outbound(_)) => Ok(()),
            _ => Err(Failure::NotFound),
        }
    }

    fn tls_server_config(&self, _cert: Vec<u8>, _key: Vec<u8>) -> Result<Handle, Failure> {
        // No real config under test — just mint a token the handle table can hand back to `tls_start_server`.
        Ok(self.mint(MockResource::TlsConfig))
    }

    fn tls_start_server(&self, io: Handle, cfg: Handle) -> Result<(), Failure> {
        // A no-op identity over the accepted connection, given a config token.
        let table = self.table.lock().unwrap();

        let has_config = matches!(table.get(&cfg), Some(MockResource::TlsConfig));
        let has_conn = matches!(table.get(&io), Some(MockResource::Inbound(_)));

        match has_config && has_conn {
            true => Ok(()),
            false => Err(Failure::NotFound),
        }
    }

    fn socket_listen(&self, io: Handle, _backlog: u64) -> Result<(), Failure> {
        let mut table = self.table.lock().unwrap();

        match table.get(&io) {
            Some(MockResource::Socket) => {
                table.insert(&io, MockResource::Listener);
                Ok(())
            }
            _ => Err(Failure::NotFound),
        }
    }

    fn socket_accept(&self, io: Handle) -> Result<Handle, Failure> {
        if !matches!(
            self.table.lock().unwrap().get(&io),
            Some(MockResource::Listener)
        ) {
            return Err(Failure::NotFound);
        }

        // Pull the next scripted request. An exhausted queue fails the accept, ending the serve loop (a real blocking accept would park forever).
        let request = match self.inbound.lock().unwrap().pop_front() {
            Some(request) => request,
            None => return Err(Failure::NotFound),
        };

        let capture = {
            let mut captures = self.captures.lock().unwrap();
            let index = captures.len();
            captures.push(Vec::new());
            index
        };

        Ok(self.mint(MockResource::Inbound(MockServer {
            bytes: Chunked::new(request),
            capture,
        })))
    }

    fn socket_set_reuseaddr(&self, _io: Handle, _on: bool) -> Result<(), Failure> {
        Ok(())
    }

    fn handle_poll(
        &self,
        handles: Vec<Handle>,
        events: Vec<Poll>,
        _: i64,
    ) -> Result<Vec<Poll>, Refusal> {
        // Readiness is what the script says is due, and never a wait: the write ends and files mirror the requested interest, standard input and a scripted stream are armed for their next chunk and reported readable (a stream's end counts as readable, as an OS reports a closed peer) plus writable where asked, and an unknown handle reports `ERR`. Arming here is what makes one `handle_poll` one chunk of progress, so a scheduler's park-poll-resume path is taken exactly once per chunk boundary.
        let mut table = self.table.lock().unwrap();

        let ready = handles
            .iter()
            .enumerate()
            .map(|(slot, handle)| {
                let requested = events.get(slot).copied().unwrap_or_else(Poll::empty);
                let readable = Poll::from_bits(event::READ | (requested.bits() & event::WRITE));

                match handle {
                    // Standard input is armed like a scripted stream, so one `handle_poll` is one chunk of progress and a fiber parked on `WouldBlock` resumes into the next chunk. The write ends have nothing to arm.
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
                        Some(MockResource::Serial(port)) => {
                            port.bytes.arm();

                            readable
                        }
                        Some(MockResource::Connecting { due, .. }) => {
                            *due = true;

                            Poll::from_bits(event::WRITE)
                        }
                        // A child is readable once it has ended, which is when `proc_wait` answers.
                        Some(MockResource::Child(child)) => match child.exit {
                            Some(_) => Poll::from_bits(event::READ),
                            None => Poll::empty(),
                        },
                        Some(_) => requested,
                        // An unknown handle — closed, say — reports `ERR`, as the native host does, so a waiter wakes into the call that reports why.
                        None => Poll::from_bits(event::ERR),
                    },
                }
            })
            .collect();

        Ok(ready)
    }

    fn handle_flush(&self, io: Handle) -> Result<(), Failure> {
        if matches!(io, Handle::Stdin | Handle::Stdout | Handle::Stderr) {
            return Ok(());
        }

        if self.table.lock().unwrap().get(&io).is_none() {
            return Err(Failure::NotFound);
        }

        // The scripted host holds nothing of what it accepted, so a flush drains at once — unless the script says a flush waits, as a TLS stream's does while its records wait for the socket.
        let mut pending = self.pending_flushes.lock().unwrap();

        match *pending {
            0 => Ok(()),
            _ => {
                *pending -= 1;

                Err(Failure::WouldBlock)
            }
        }
    }

    fn handle_close(&self, io: Handle) {
        self.table.lock().unwrap().remove(&io);
    }

    fn handle_read(&self, io: Handle, count: u64) -> Result<Option<Vec<u8>>, Failure> {
        // The direction and the kind first, as the native host checks them: a request for nothing reads nothing and never answers the end of a stream it did not look at.
        self.stream(&io, Direction::Read)?;

        if count == 0 {
            return Ok(Some(Vec::new()));
        }

        // Standard input is a scripted stream like any other: the front chunk, `WouldBlock` between chunks, the end once the script is spent. `OsHost` gates its own stdin read by a zero-timeout poll and answers `WouldBlock` when nothing is there, so a script that hands the wait back is the faithful mirror rather than a convenience.
        if matches!(io, Handle::Stdin) {
            return self.input.lock().unwrap().read(count);
        }

        match self.table.lock().unwrap().get_mut(&io) {
            // File-backed handle: serve from the in-memory filesystem.
            Some(MockResource::File(open)) => self.files.with(&open.path, |contents| {
                serve_from(contents, &mut open.position, count)
            }),
            // Inbound (accepted) connection: serve the scripted request.
            Some(MockResource::Inbound(conn)) => conn.bytes.read(count),
            // A serial port: serve the device's scripted chunks.
            Some(MockResource::Serial(port)) => port.bytes.read(count),
            // Outbound connection and a child's piped output: serve the scripted chunks.
            Some(MockResource::Outbound(stream) | MockResource::Piped(stream)) => {
                stream.read(count)
            }
            // A missing or non-stream handle is a fault, not an exhausted stream — mirror write's `NotFound` so use-after-close stays loud.
            _ => Err(Failure::NotFound),
        }
    }

    fn handle_write(&self, io: Handle, bytes: Vec<u8>) -> Result<u64, Failure> {
        // The direction and the kind first, as the native host checks them; an empty write writes nothing.
        self.stream(&io, Direction::Write)?;

        if bytes.is_empty() {
            return Ok(0);
        }

        // The in-memory sink always takes the whole buffer in one go, so a successful write reports the full length and never `WouldBlock`.
        let full = bytes.len() as u64;

        if matches!(io, Handle::Stdout | Handle::Stderr) {
            self.output.lock().unwrap().extend_from_slice(&bytes);

            if matches!(io, Handle::Stderr) {
                self.errors.lock().unwrap().extend_from_slice(&bytes);
            }

            return Ok(full);
        }

        match self.table.lock().unwrap().get(&io) {
            // File-backed handle: append to the in-memory filesystem.
            Some(MockResource::File(open)) => {
                self.files.append(&open.path, &bytes);

                Ok(full)
            }
            // Inbound (accepted) connection: capture the response bytes so a test can inspect what the server wrote back.
            Some(MockResource::Inbound(conn)) => {
                self.captures.lock().unwrap()[conn.capture].extend_from_slice(&bytes);

                Ok(full)
            }
            // Outbound connection: accept and discard (the in-memory test host does not capture request bytes).
            Some(MockResource::Outbound(_)) => Ok(full),
            // A child's piped stdin: accepted and discarded too.
            Some(MockResource::Sink) => Ok(full),
            // A serial port: capture the bytes under its path, so a test can see the command the program sent the device.
            Some(MockResource::Serial(port)) => {
                self.serial_written
                    .lock()
                    .unwrap()
                    .entry(port.path.clone())
                    .or_default()
                    .extend_from_slice(&bytes);

                Ok(full)
            }
            _ => Err(Failure::NotFound),
        }
    }

    fn clock_wall(&self) -> Timestamp {
        reading(self.clock_wall_seq.lock().unwrap().pop_front())
    }

    fn clock_mono(&self) -> Timestamp {
        reading(self.clock_mono_seq.lock().unwrap().pop_front())
    }

    fn rand_bytes(&self, count: u64) -> Result<Vec<u8>, Refusal> {
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

        Ok(output)
    }

    fn proc_args(&self) -> Vec<Vec<u8>> {
        self.args.clone()
    }

    fn proc_env(&self, name: Vec<u8>) -> Option<Vec<u8>> {
        self.env.get(&name).cloned()
    }

    fn proc_exit(&self, code: u8) -> Termination {
        Termination(code)
    }

    fn tty_raw(&self, _io: Handle, on: bool) -> Result<(), Failure> {
        if self.tty_sizes.lock().unwrap().is_empty() {
            return Err(Failure::Other(ENOTTY));
        }

        self.raw_modes.lock().unwrap().push(on);

        Ok(())
    }

    fn tty_size(&self, _io: Handle) -> Result<TtySize, Failure> {
        let mut sizes = self.tty_sizes.lock().unwrap();
        let size = if sizes.len() > 1 {
            sizes.pop_front()
        } else {
            sizes.front().copied()
        };

        match size {
            Some((cols, rows)) => Ok(TtySize { cols, rows }),
            None => Err(Failure::Other(ENOTTY)),
        }
    }

    fn serial_open(
        &self,
        path: Vec<u8>,
        baud: u64,
        data_bits: u64,
        parity: SerialParity,
        stop_bits: u64,
        flow: SerialFlow,
    ) -> Result<Handle, Failure> {
        // Refused in the native host's order: a frame outside the row's ranges before the device is looked for.
        if serial_frame(data_bits, parity, stop_bits, flow).is_none() {
            return Err(Failure::Other(EINVAL));
        }

        let Some(chunks) = self.serial_devices.get(&path) else {
            return Err(Failure::NotFound);
        };

        self.serial_opens.lock().unwrap().push((
            path.clone(),
            [baud, data_bits, parity.code(), stop_bits, flow.code()],
        ));

        Ok(self.mint(MockResource::Serial(MockSerial {
            bytes: Chunked::new(chunks.clone()),
            path,
        })))
    }

    fn serial_control(&self, io: Handle, op: SerialOp, on: bool) -> Result<(), Failure> {
        let mut table = self.table.lock().unwrap();

        let port = match table.get_mut(&io) {
            Some(MockResource::Serial(port)) => port,
            // A descriptor that is a file, a pipe or a socket has no modem lines, and the native host's ioctl says so through the errno lane.
            Some(_) => return Err(Failure::Other(ENOTTY)),
            None => return Err(Failure::NotFound),
        };

        if op == SerialOp::DiscardInput {
            port.bytes.discard();
        }

        self.serial_controls.lock().unwrap().push((op.code(), on));

        Ok(())
    }

    fn file_stat(&self, path: Vec<u8>) -> Result<FileStat, Failure> {
        // The scripted disk keeps no timestamps, so a modification time is the epoch.
        match self.files.stat(&path) {
            Some((kind, size)) => Ok(FileStat {
                kind,
                size: size as u64,
                mtime_secs: 0,
                mtime_nanos: 0,
            }),
            None => Err(Failure::NotFound),
        }
    }

    fn file_remove(&self, path: Vec<u8>) -> Result<(), Failure> {
        self.files.remove_file(&path)
    }

    fn file_rename(&self, from: Vec<u8>, to: Vec<u8>) -> Result<(), Failure> {
        self.files.rename(&from, &to)
    }

    fn dir_list(&self, path: Vec<u8>) -> Result<Vec<Vec<u8>>, Failure> {
        self.files.list(&path)
    }

    fn dir_create(&self, path: Vec<u8>) -> Result<(), Failure> {
        self.files.create_dir(&path)
    }

    fn dir_remove(&self, path: Vec<u8>) -> Result<(), Failure> {
        self.files.remove_dir(&path)
    }

    fn proc_cwd(&self) -> Result<Vec<u8>, Failure> {
        Ok(self.cwd.clone())
    }

    fn proc_spawn(
        &self,
        argv: Vec<Vec<u8>>,
        _cwd: Vec<u8>,
        _env: Vec<Vec<u8>>,
        stdin: StdioMode,
        stdout: StdioMode,
        stderr: StdioMode,
    ) -> Result<Handle, Failure> {
        // An unscripted program is one the host cannot find, as an unknown path is to `file_open`; the script is keyed by `argv[0]`.
        let Some(script) = argv
            .first()
            .and_then(|program| self.children.get(program))
            .cloned()
        else {
            return Err(Failure::NotFound);
        };
        let program = &argv[0];

        // Each stream is filed only where the guest asked for a pipe; the scripted child has already written everything it ever will.
        let piped = |mode: StdioMode, bytes: Vec<u8>| {
            (mode == StdioMode::Pipe)
                .then(|| self.mint(MockResource::Piped(Chunked::new(vec![bytes]))))
        };
        let stdin = (stdin == StdioMode::Pipe).then(|| self.mint(MockResource::Sink));
        let streams = [
            stdin,
            piped(stdout, script.stdout),
            piped(stderr, script.stderr),
        ];
        let child = self.mint(MockResource::Child(MockChild {
            program: program.to_vec(),
            exit: script.exit,
            streams,
        }));

        Ok(child)
    }

    fn proc_stream(&self, child: Handle, which: ChildStream) -> Result<Handle, Failure> {
        match self.table.lock().unwrap().get(&child) {
            // A stream that was not piped has no handle, which is `not_found` as an unknown handle is.
            Some(MockResource::Child(running)) => running.streams[stream_index(which)]
                .clone()
                .ok_or(Failure::NotFound),
            _ => Err(Failure::NotFound),
        }
    }

    fn proc_wait(&self, child: Handle) -> Result<ChildExit, Failure> {
        let mut table = self.table.lock().unwrap();

        // Only an ended child is consumed: a running one answers `would_block`, and anything that is not a child stays filed.
        let exit = match table.get(&child) {
            Some(MockResource::Child(scripted)) => scripted.exit.ok_or(Failure::WouldBlock)?,
            _ => return Err(Failure::NotFound),
        };

        table.remove(&child);

        Ok(exit)
    }

    fn proc_kill(&self, child: Handle) -> Result<(), Failure> {
        match self.table.lock().unwrap().get_mut(&child) {
            // A running child ends by the signal, which is recorded; an ended one is not signaled at all, as the native host never signals a pid it has reaped.
            Some(MockResource::Child(scripted)) => {
                if scripted.exit.is_none() {
                    self.kills.lock().unwrap().push(scripted.program.clone());
                    scripted.exit = Some(ChildExit::Signal(SIGKILL));
                }

                Ok(())
            }
            _ => Err(Failure::NotFound),
        }
    }
}

/// A scripted clock reading, or the epoch once the script is spent.
fn reading(scripted: Option<(u64, u64)>) -> Timestamp {
    let (secs, nanos) = scripted.unwrap_or((0, 0));

    Timestamp { secs, nanos }
}

/// Serve up to `count` bytes of `contents` from `*position`, advancing the cursor; the end of the stream once it reaches the end. The shape of a file read, which is always ready; a stream reads through [`Chunked`] instead.
fn serve_from(
    contents: &[u8],
    position: &mut usize,
    count: u64,
) -> Result<Option<Vec<u8>>, Failure> {
    if *position >= contents.len() {
        return Ok(None);
    }

    let stop = contents.len().min(position.saturating_add(count as usize));
    let bytes = contents[*position..stop].to_vec();
    *position = stop;

    Ok(Some(bytes))
}

/// The inspectable side of a [`MockHost`]: the shared buffers the run writes into. The host is moved into the runner, so a test holds this handle to read stdout, files, and server captures back out afterwards.
pub struct MockIo {
    output: Arc<Mutex<Vec<u8>>>,
    errors: Arc<Mutex<Vec<u8>>>,
    files: MockFileSystem,
    captures: Arc<Mutex<Vec<Vec<u8>>>>,
    raw_modes: Arc<Mutex<Vec<bool>>>,
    kills: Arc<Mutex<Vec<Vec<u8>>>>,
    serial_opens: Arc<Mutex<Vec<SerialOpen>>>,
    serial_controls: Arc<Mutex<Vec<(u64, bool)>>>,
    serial_written: Arc<Mutex<HashMap<Vec<u8>, Vec<u8>>>>,
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

    /// Every serial port the guest opened, in order: its path and `[baud, data_bits, parity, stop_bits, flow]`.
    pub fn serial_opens(&self) -> Vec<(Vec<u8>, [u64; 5])> {
        self.serial_opens.lock().unwrap().clone()
    }

    /// Every control the guest applied to an open serial port, in order: the [`serial_op`](curios_abi::serial_op) tag and the level.
    pub fn serial_controls(&self) -> Vec<(u64, bool)> {
        self.serial_controls.lock().unwrap().clone()
    }

    /// What the guest wrote to the serial port at `path`, concatenated in write order.
    pub fn serial_written(&self, path: &[u8]) -> Vec<u8> {
        self.serial_written
            .lock()
            .unwrap()
            .get(path)
            .cloned()
            .unwrap_or_default()
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
    pending_flushes: u64,
    clock_wall_seq: VecDeque<(u64, u64)>,
    clock_mono_seq: VecDeque<(u64, u64)>,
    args: Vec<Vec<u8>>,
    env: HashMap<Vec<u8>, Vec<u8>>,
    tty_sizes: VecDeque<(u64, u64)>,
    dirs: BTreeSet<Vec<u8>>,
    cwd: Option<Vec<u8>>,
    children: HashMap<Vec<u8>, MockChildScript>,
    serial_devices: HashMap<Vec<u8>, Vec<Vec<u8>>>,
}

impl MockHostBuilder {
    /// Script the children `proc_spawn` can start: `(program, stdout, stderr, exit)`. Spawning an unscripted program is `NotFound`.
    pub fn children<P, O, E, I>(mut self, children: I) -> Self
    where
        P: AsRef<[u8]>,
        O: AsRef<[u8]>,
        E: AsRef<[u8]>,
        I: IntoIterator<Item = (P, O, E, ChildExit)>,
    {
        self.children
            .extend(children.into_iter().map(|(program, stdout, stderr, exit)| {
                (
                    program.as_ref().to_vec(),
                    MockChildScript {
                        stdout: stdout.as_ref().to_vec(),
                        stderr: stderr.as_ref().to_vec(),
                        exit: Some(exit),
                    },
                )
            }));

        self
    }

    /// Script children `proc_spawn` can start that write nothing and run until `proc_kill` ends them.
    pub fn running_children<P: AsRef<[u8]>, I: IntoIterator<Item = P>>(
        mut self,
        programs: I,
    ) -> Self {
        self.children.extend(programs.into_iter().map(|program| {
            (
                program.as_ref().to_vec(),
                MockChildScript {
                    stdout: Vec::new(),
                    stderr: Vec::new(),
                    exit: None,
                },
            )
        }));

        self
    }

    /// Seed empty directories; the directories above every seeded file exist without being named here.
    pub fn dirs<P: AsRef<[u8]>, I: IntoIterator<Item = P>>(mut self, dirs: I) -> Self {
        self.dirs
            .extend(dirs.into_iter().map(|dir| dir.as_ref().to_vec()));

        self
    }

    /// Script the working directory `proc_cwd` answers; `/` when unset.
    pub fn cwd(mut self, path: impl AsRef<[u8]>) -> Self {
        self.cwd = Some(path.as_ref().to_vec());

        self
    }

    /// Give the host a terminal of `cols` by `rows`: `tty_size` answers it and `tty_raw` records its switches. Without one, both rows answer `ENOTTY`.
    pub fn tty_size(self, cols: u64, rows: u64) -> Self {
        self.tty_sizes([(cols, rows)])
    }

    /// Set the terminal sizes served by successive `tty_size` calls, repeating the last after the script ends. An empty script gives the host no terminal. Raw-mode switches do not advance the script.
    pub fn tty_sizes(mut self, sizes: impl IntoIterator<Item = (u64, u64)>) -> Self {
        self.tty_sizes = sizes.into_iter().collect();

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

    /// Append the chunks standard input delivers, verbatim: nothing is terminated for you, and each chunk is served only once a `handle_poll` has armed it, so a reader parks between them. This is how a raw-mode program's keystrokes are scripted — one chunk per burst, `x[0x1b, 0x5b, 0x41]` for an arrow key — and the only way a read of standard input that waits is put under test. Whatever [`stdin_lines`](Self::stdin_lines) wrote precedes these, as the one chunk it is.
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

    /// Script the network endpoints served by `socket_connect`: `(host:port, response)` pairs, each response served whole and ready at once. Connecting to an unscripted endpoint is refused.
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

    /// Script the network endpoints served by `socket_connect` as `(host:port, chunks)` pairs: a read serves one chunk, and the next is served only after a `handle_poll` has reported the connection readable, so a reader that parks between chunks is exercised.
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

    /// Script the serial devices `serial_open` finds: `(path, chunks)`, each port serving its chunks as `net_chunks` serves a response — the first arrived by the open, each later one once a `handle_poll` arms it. Opening an unscripted path is `NotFound`.
    pub fn serial<P, C, I>(mut self, devices: I) -> Self
    where
        P: AsRef<[u8]>,
        C: AsRef<[u8]>,
        I: IntoIterator<Item = (P, Vec<C>)>,
    {
        self.serial_devices.extend(
            devices
                .into_iter()
                .map(|(path, chunks)| (path.as_ref().to_vec(), chunk_list(chunks))),
        );

        self
    }

    /// Make every `socket_connect` pend: it answers `WouldBlock`, a `handle_poll` marks the socket writable, and `socket_finish_connect` then answers what a synchronous connect would have — the way a connect to a remote peer settles.
    pub fn connect_pending(mut self) -> Self {
        self.connect_pending = true;

        self
    }

    /// Make the first `count` flushes of a handle the host knows answer `WouldBlock`, as a TLS stream's do while its records wait for the socket, so a caller that retries a flush is exercised.
    pub fn pending_flushes(mut self, count: u64) -> Self {
        self.pending_flushes = count;

        self
    }

    /// Script the inbound requests served by `socket_accept`, one per accepted connection (FIFO), each served whole and ready at once. An exhausted queue makes `socket_accept` fail, which ends a `serve` loop (a real blocking `socket_accept` would park there).
    pub fn inbound<R: AsRef<[u8]>, I: IntoIterator<Item = R>>(self, requests: I) -> Self {
        self.inbound_chunks(requests.into_iter().map(|request| vec![request]))
    }

    /// Script the inbound requests served by `socket_accept` as chunk lists, one list per accepted connection, served as `net_chunks` serves a response.
    pub fn inbound_chunks<C: AsRef<[u8]>, I: IntoIterator<Item = Vec<C>>>(
        mut self,
        requests: I,
    ) -> Self {
        self.inbound.extend(requests.into_iter().map(chunk_list));

        self
    }

    /// Script the wall-clock readings served by `clock_wall`, in order, each `(secs, nanos)`. When the script is exhausted `clock_wall` falls back to `(0, 0)`.
    pub fn wall<I: IntoIterator<Item = (u64, u64)>>(mut self, readings: I) -> Self {
        self.clock_wall_seq.extend(readings);

        self
    }

    /// Script the monotonic readings served by `clock_mono`, in order.
    pub fn mono<I: IntoIterator<Item = (u64, u64)>>(mut self, readings: I) -> Self {
        self.clock_mono_seq.extend(readings);

        self
    }

    /// Set the process arguments served by `proc_args` (`argv[0]` is the program name).
    pub fn args<A: AsRef<[u8]>, I: IntoIterator<Item = A>>(mut self, args: I) -> Self {
        self.args = args.into_iter().map(|a| a.as_ref().to_vec()).collect();

        self
    }

    /// Set the environment served by `proc_env`: `(name, value)` pairs.
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
        let serial_opens = Arc::new(Mutex::new(Vec::new()));
        let serial_controls = Arc::new(Mutex::new(Vec::new()));
        let serial_written = Arc::new(Mutex::new(HashMap::new()));

        let io = MockIo {
            output: output.clone(),
            errors: errors.clone(),
            files: files.clone(),
            captures: captures.clone(),
            raw_modes: raw_modes.clone(),
            kills: kills.clone(),
            serial_opens: serial_opens.clone(),
            serial_controls: serial_controls.clone(),
            serial_written: serial_written.clone(),
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
            pending_flushes: Mutex::new(self.pending_flushes),
            captures,
            clock_wall_seq: Mutex::new(self.clock_wall_seq),
            clock_mono_seq: Mutex::new(self.clock_mono_seq),
            // A fixed non-zero xorshift64 seed: deterministic across runs.
            rng: Mutex::new(0x2545_F491_4F6C_DD1D),
            args: self.args,
            env: self.env,
            raw_modes,
            tty_sizes: Mutex::new(self.tty_sizes),
            cwd: self.cwd.unwrap_or_else(|| b"/".to_vec()),
            children: self.children,
            kills,
            serial_devices: self.serial_devices,
            serial_opens,
            serial_controls,
            serial_written,
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
