use {
    super::host::*,
    socket2::{Domain, SockAddr, Socket, Type},
    std::{
        collections::HashMap,
        env,
        fs::{File, OpenOptions},
        io::{Read, Write, stderr, stdin, stdout},
        net::{SocketAddr, ToSocketAddrs},
        sync::{
            Mutex,
            atomic::{AtomicU32, Ordering},
        },
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
    },
};

/// A non-stdio handle in [`OsHost`]'s unified table, tracking the BSD lifecycle
/// with one concrete type per state: `open` files a `File`; `socket` mints an
/// `Unconnected` socket, `connect` turns it into a `Connected` one (as does
/// `accept`), and `listen` turns it into a `Listener`. `read`/`write` serve
/// `File` and `Connected` alike (both are `Read + Write`); `close` drops any
/// kind, releasing its descriptor.
enum OsResource {
    File(File),
    Connected(Socket),
    Unconnected(Socket),
    Listener(Socket),
}

pub struct OsHost {
    handle_seed: AtomicU32,
    /// One table for every non-stdio handle, keyed by token. Files, unconnected
    /// sockets, connected streams, and listeners share it so the BSD lifecycle
    /// can transition a handle in place and `close` releases any kind uniformly.
    table: Mutex<HashMap<u32, OsResource>>,
    /// Monotonic origin: `clock_mono` reports elapsed time since this.
    start: Instant,
    /// The process arguments served by `args` (argv[0] is the program name).
    args: Vec<Vec<u8>>,
}

impl OsHost {
    pub fn new() -> Self {
        Self::with_args(env::args_os().map(|arg| arg.into_encoded_bytes()).collect())
    }

    /// Build a host whose `args` are the given byte strings — used by the CLI to
    /// forward a program's own arguments instead of the `curios` process's.
    pub fn with_args(args: Vec<Vec<u8>>) -> Self {
        Self {
            handle_seed: AtomicU32::new(Io::HANDLE_SEED),
            table: Mutex::new(HashMap::new()),
            start: Instant::now(),
            args,
        }
    }

    /// Pull an unconnected socket out of the table by handle, leaving any other
    /// resource (or none) in place. Lets `connect`/`listen` transition a handle
    /// without holding the lock across the blocking syscall.
    fn take_unconnected(&self, handle: u32) -> Option<Socket> {
        let mut table = self.table.lock().unwrap();

        match table.remove(&handle) {
            Some(OsResource::Unconnected(socket)) => Some(socket),
            Some(other) => {
                table.insert(handle, other);

                None
            }
            None => None,
        }
    }

    /// Apply a `socket2` setter to a configurable handle. Every socket kind —
    /// unconnected, connected, or listening — exposes its typed setters directly;
    /// a `File` has no socket options, so that path records nothing and succeeds.
    fn with_socket<F>(&self, handle: u32, apply: F) -> Status
    where
        F: FnOnce(&Socket) -> std::io::Result<()>,
    {
        let table = self.table.lock().unwrap();

        match table.get(&handle) {
            Some(
                OsResource::Unconnected(socket)
                | OsResource::Connected(socket)
                | OsResource::Listener(socket),
            ) => match apply(socket) {
                Ok(()) => Status::Ok,
                Err(error) => Status::from(error),
            },
            Some(OsResource::File(_)) => Status::Ok,
            None => Status::NotFound,
        }
    }
}

impl Default for OsHost {
    fn default() -> Self {
        Self::new()
    }
}

impl Host for OsHost {
    fn open(&self, path: &[u8], mode: Mode) -> (Status, Io) {
        let path = String::from_utf8_lossy(path).into_owned();

        let mut options = OpenOptions::new();

        match mode {
            Mode::Read => options.read(true),
            Mode::Write => options.write(true).create(true).truncate(true),
            Mode::Append => options.append(true).create(true),
        };

        match options.open(&path) {
            Ok(file) => {
                let handle = self.handle_seed.fetch_add(1, Ordering::Relaxed);
                self.table
                    .lock()
                    .unwrap()
                    .insert(handle, OsResource::File(file));

                (Status::Ok, Io::Other(handle))
            }
            Err(error) => (Status::from(error), Io::Other(0)),
        }
    }

    fn resolve(&self, host: &[u8], port: u32) -> (Status, Vec<Vec<u8>>) {
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
                    (Status::NotFound, vec![])
                } else {
                    (Status::Ok, addresses)
                }
            }
            // Any resolution failure is honestly `NotFound`: the host:port named
            // nothing. The generic conversion can no longer claim this — it now
            // reports errno-less errors as the unclassifiable `other(0)`.
            Err(_) => (Status::NotFound, vec![]),
        }
    }

    fn socket(&self, addr: &[u8]) -> (Status, Io) {
        // The address blob is the canonical "ip:port" string `resolve` minted.
        let address: SocketAddr = match String::from_utf8_lossy(addr).parse() {
            Ok(address) => address,
            Err(_) => return (Status::NotFound, Io::Other(0)),
        };

        match Socket::new(Domain::for_address(address), Type::STREAM, None) {
            Ok(socket) => {
                let handle = self.handle_seed.fetch_add(1, Ordering::Relaxed);
                self.table
                    .lock()
                    .unwrap()
                    .insert(handle, OsResource::Unconnected(socket));

                (Status::Ok, Io::Other(handle))
            }
            Err(error) => (Status::from(error), Io::Other(0)),
        }
    }

    fn bind(&self, io: Io, addr: &[u8]) -> Status {
        // The address blob is the canonical "ip:port" string `resolve` minted.
        let address: SocketAddr = match String::from_utf8_lossy(addr).parse() {
            Ok(address) => address,
            Err(_) => return Status::NotFound,
        };

        match self.table.lock().unwrap().get(&io.token()) {
            Some(OsResource::Unconnected(socket)) => match socket.bind(&SockAddr::from(address)) {
                Ok(()) => Status::Ok,
                Err(error) => Status::from(error),
            },
            _ => Status::NotFound,
        }
    }

    fn connect(&self, io: Io, addr: &[u8]) -> Status {
        // The address blob is the canonical "ip:port" string `resolve` minted.
        let address: SocketAddr = match String::from_utf8_lossy(addr).parse() {
            Ok(address) => address,
            Err(_) => return Status::NotFound,
        };

        // Take the socket out so the blocking connect runs without the table
        // lock held; re-file the connected socket as a byte stream on success.
        let socket = match self.take_unconnected(io.token()) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        match socket.connect(&SockAddr::from(address)) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(io.token(), OsResource::Connected(socket));

                Status::Ok
            }
            Err(error) => Status::from(error),
        }
    }

    fn listen(&self, io: Io, backlog: u32) -> Status {
        let socket = match self.take_unconnected(io.token()) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        match socket.listen(backlog as i32) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(io.token(), OsResource::Listener(socket));

                Status::Ok
            }
            Err(error) => Status::from(error),
        }
    }

    fn accept(&self, io: Io) -> (Status, Io) {
        // `accept` blocks until a connection arrives, so clone the listener fd
        // out and drop the table lock before the wait — never hold it across one.
        let listener = match self.table.lock().unwrap().get(&io.token()) {
            Some(OsResource::Listener(socket)) => socket.try_clone(),
            _ => return (Status::NotFound, Io::Other(0)),
        };

        match listener.and_then(|listener| listener.accept()) {
            Ok((stream, _)) => {
                let conn = self.handle_seed.fetch_add(1, Ordering::Relaxed);
                self.table
                    .lock()
                    .unwrap()
                    .insert(conn, OsResource::Connected(stream));

                (Status::Ok, Io::Other(conn))
            }
            Err(error) => (Status::from(error), Io::Other(0)),
        }
    }

    fn set_nonblocking(&self, io: Io, on: u32) -> Status {
        self.with_socket(io.token(), |socket| socket.set_nonblocking(on != 0))
    }

    fn set_recv_timeout(&self, io: Io, ms: u32) -> Status {
        // `0` ms clears the timeout (`None`); any other value is a duration.
        let timeout = (ms != 0).then(|| Duration::from_millis(ms.into()));
        self.with_socket(io.token(), |socket| socket.set_read_timeout(timeout))
    }

    fn set_send_timeout(&self, io: Io, ms: u32) -> Status {
        // `0` ms clears the timeout (`None`); any other value is a duration.
        let timeout = (ms != 0).then(|| Duration::from_millis(ms.into()));
        self.with_socket(io.token(), |socket| socket.set_write_timeout(timeout))
    }

    fn set_reuseaddr(&self, io: Io, on: u32) -> Status {
        self.with_socket(io.token(), |socket| socket.set_reuse_address(on != 0))
    }

    fn close(&self, io: Io) {
        self.table.lock().unwrap().remove(&io.token());
    }

    fn read(&self, io: Io, count: u32) -> (Status, Vec<u8>) {
        let mut buffer = vec![0; count as usize];

        let result = match io {
            Io::Stdin => stdin().lock().read(&mut buffer),
            Io::Other(handle) => {
                let mut table = self.table.lock().unwrap();
                let stream: &mut dyn Read = match table.get_mut(&handle) {
                    Some(OsResource::File(file)) => file,
                    Some(OsResource::Connected(socket)) => socket,
                    _ => return (Status::Eof, vec![]),
                };

                stream.read(&mut buffer)
            }
            // stdout/stderr are not readable.
            _ => return (Status::Eof, vec![]),
        };

        match result {
            Ok(0) => (Status::Eof, vec![]),
            Ok(n) => {
                buffer.truncate(n);

                (Status::Ok, buffer)
            }
            Err(error) => (Status::from(error), vec![]),
        }
    }

    fn write(&self, io: Io, bytes: &[u8]) -> Status {
        let result = match io {
            Io::Stdout => stdout().write_all(bytes),
            Io::Stderr => stderr().write_all(bytes),
            Io::Other(handle) => {
                let mut table = self.table.lock().unwrap();

                let stream: &mut dyn Write = match table.get_mut(&handle) {
                    Some(OsResource::File(file)) => file,
                    Some(OsResource::Connected(socket)) => socket,
                    _ => return Status::NotFound,
                };

                stream.write_all(bytes)
            }
            // stdin is not writable; the guest's `/std/Io` never issues this.
            Io::Stdin => panic!("write to stdin"),
        };

        match result {
            Ok(()) => Status::Ok,
            Err(error) => Status::from(error),
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

    fn env(&self, name: &[u8]) -> (Status, Vec<u8>) {
        match env::var_os(String::from_utf8_lossy(name).as_ref()) {
            Some(value) => (Status::Ok, value.into_encoded_bytes()),
            None => (Status::NotFound, vec![]),
        }
    }
}
