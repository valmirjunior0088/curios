use {
    super::host::*,
    crate::Entropy,
    socket2::{Domain, SockAddr, Socket, Type},
    std::{
        collections::HashMap,
        env,
        fs::OpenOptions,
        io::{ErrorKind, Read, Write, stderr, stdin, stdout},
        net::{SocketAddr, ToSocketAddrs},
        sync::Mutex,
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
    },
};

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

/// A bidirectional byte stream backing a non-stdio handle: a file or a socket.
/// Both `File` and `TcpStream` are `Read + Write + Send`, so one handle map
/// serves them uniformly — `close` (a drop) flushes or disconnects either.
trait Conduit: Read + Write + Send {}
impl<T: Read + Write + Send> Conduit for T {}

/// A non-stdio handle in [`OsHost`]'s unified table. The BSD lifecycle moves
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
/// `SocketAddr`. The encoding is `OsHost`'s private contract with `resolve`.
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

pub struct OsHost {
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

impl Default for OsHost {
    fn default() -> Self {
        Self::new()
    }
}

impl OsHost {
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

impl Host for OsHost {
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
