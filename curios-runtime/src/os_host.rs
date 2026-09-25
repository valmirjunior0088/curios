use {
    super::{OsResolver, Running, Slot, Spawned, Table, host::*, os_child},
    curios_abi::event,
    rustix::{
        event::{PollFd, PollFlags, Timespec, poll},
        fs::OFlags,
        io::Errno,
        ioctl::{Opcode, Setter, ioctl},
        termios::{
            ControlModes, OptionalActions, QueueSelector, Termios, tcflush, tcgetattr,
            tcgetwinsize, tcsetattr,
        },
    },
    rustls::{
        ClientConfig, ClientConnection, ConnectionCommon, RootCertStore, ServerConfig,
        ServerConnection, StreamOwned, crypto::ring, pki_types::ServerName,
    },
    socket2::{Domain, SockAddr, Socket, Type},
    std::{
        env,
        ffi::{OsStr, c_int},
        fs::{self, File, OpenOptions},
        io::{self, Error, ErrorKind, Read, Write, stderr, stdin, stdout},
        net::SocketAddr,
        os::{
            fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd},
            unix::ffi::{OsStrExt, OsStringExt},
        },
        sync::{Arc, LazyLock, Mutex, OnceLock},
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
    },
    webpki_roots::TLS_SERVER_ROOTS,
};

/// The most one `handle_read` reads, whatever it is asked. A request is a count the guest names and the buffer a read fills is allocated before anything arrives, so an uncapped one would let a request for more memory than the host has abort the process; a larger request is answered with the prefix one read takes, which the row allows. 64 KiB is a pipe's buffer, and four of the largest TLS record.
const READ_MAX: u64 = 64 * 1024;

/// The shared client TLS configuration: a bundled `webpki-roots` trust-anchor set with certificate verification on, built once and `Arc`-cloned by every `tls_start`. An explicit `ring` crypto provider is wired in so the config never depends on a process-global default provider being installed.
static CLIENT_CONFIG: LazyLock<Arc<ClientConfig>> = LazyLock::new(|| {
    let mut roots = RootCertStore::empty();

    roots.extend(TLS_SERVER_ROOTS.iter().cloned());

    Arc::new(
        ClientConfig::builder_with_provider(Arc::new(ring::default_provider()))
            .with_safe_default_protocol_versions()
            .expect("ring provider supports the default protocol versions")
            .with_root_certificates(roots)
            .with_no_client_auth(),
    )
});

/// A non-stdio handle in [`OsHost`]'s unified table, tracking the BSD lifecycle with one concrete type per state: `file_open` files a `File`; `socket_open` mints an `Unconnected` socket, `socket_connect` turns it into a `Connected` one at once or into a `Connecting` one that `socket_finish_connect` settles (`socket_accept` mints a `Connected` one directly), and `socket_listen` turns it into a `Listener`. `tls_start` / `tls_start_server` upgrade a `Connected` socket in place to a `ClientTls` / `ServerTls` stream; `tls_server_config` files a host-owned `TlsConfig` token. `handle_read`/`handle_write` serve `File`, `Connected`, and both TLS streams alike (all are `Read + Write`); `handle_close` drops any kind, releasing its descriptor.
///
/// Every kind on which a peer decides — a socket in any state, an accepted stream, a pipe to a child, a serial port — is filed non-blocking at the moment it is minted, so no row waits on a peer: a `handle_read`, `handle_write`, `socket_connect` or `socket_accept` that cannot progress answers `WouldBlock` and `handle_poll` is the one place the host sleeps. A regular file is synchronous, since the disk rather than a peer answers it.
enum OsResource {
    File(File),
    /// An in-flight asynchronous name lookup minted by `dns_lookup`. `done` is the read end of a pipe a worker thread writes one byte to once it has filled `slot` with the `getaddrinfo` result; that write makes `done` poll-`READ` readable, waking the scheduler. `dns_resolve` then drains `slot` and drops the handle (closing `done`). `handle_poll` watches `done` like any other fd.
    Resolving {
        done: OwnedFd,
        slot: Slot,
    },
    /// A bare owned descriptor — one end of a pipe to a child, filed by `proc_spawn`, or a serial port, filed by `serial_open`. Named by what it holds, as `File` and `Listener` are, and the one thing separating it from `File` is that it is non-blocking for real: whoever files one makes it so first — `proc_spawn` through `fcntl`, `serial_open` at the open itself — so a fiber draining it yields on `WouldBlock` instead of blocking the scheduler, while `handle_read`, `handle_write`, `handle_poll` and `handle_close` serve it as they serve a file.
    Descriptor(OwnedFd),
    /// A running child minted by `proc_spawn`: its `done` pipe end becomes `READ`-ready when the reaper has recorded the end, `proc_wait` answers it, `proc_kill` addresses its pid while it runs, and `proc_stream` hands out the handles of its piped standard streams — `None` for a stream that was not piped, filed as `Descriptor`s at spawn time and boxed here so a child costs the table no more than a socket does. Closing the child leaves its streams filed, and the reaper still reaps the process.
    Child {
        running: Running,
        streams: Box<[Option<Handle>; 3]>,
    },
    Connected(Socket),
    Unconnected(Socket),
    /// A non-blocking `socket_connect` under way: `EINPROGRESS` filed it, `handle_poll` watches its descriptor for `WRITE`, and `socket_finish_connect` settles it into `Connected` or reports what refused it.
    Connecting(Socket),
    /// A listening socket. It never blocks, so `socket_accept` runs under the table lock like any other row.
    Listener(Socket),
    /// A client-side TLS stream: the encrypted conduit a `Connected` socket became under `tls_start`, serving the same `handle_read`/`handle_write`/`handle_close`.
    ///
    /// Boxed, and so is [`OsResource::ServerTls`], because an enum is as large as its largest variant and a `rustls` connection carries its record buffers inline — around a kilobyte each. Unboxed they set the size of *every* entry in the handle table, so an open file or a plain socket paid a kilobyte for TLS state it does not have.
    ClientTls(Box<StreamOwned<ClientConnection, Socket>>),
    /// A server-side TLS stream: the encrypted conduit an accepted socket became under `tls_start_server`.
    ServerTls(Box<StreamOwned<ServerConnection, Socket>>),
    /// An opaque server TLS configuration minted by `tls_server_config`, held in the table as a handle and consumed by `tls_start_server`.
    TlsConfig(Arc<ServerConfig>),
}

/// The native-OS `Host`: stdio passes straight through, and every other handle — files, plain and TLS sockets, listeners, in-flight DNS lookups, TLS configs — lives in one token-keyed table so the BSD-style lifecycle can transition a handle in place. This is the host the CLI's `run` and a bundled executable execute under; tests reach for `MockHost` instead. Each instance is self-contained: its own table, monotonic clock origin, `args`, and lazily-started resolver pool.
pub struct OsHost {
    /// One [`Table`] for every non-stdio handle, keyed by token bytes. Files, unconnected sockets, connected streams, and listeners share it so the BSD lifecycle can transition a handle in place and `handle_close` releases any kind uniformly.
    table: Mutex<Table<OsResource>>,
    /// Monotonic origin: `clock_mono` reports elapsed time since this.
    start: Instant,
    /// The process arguments served by `proc_args` (argv\[0\] is the program name).
    args: Vec<Vec<u8>>,
    /// The blocking-DNS worker pool, started on the first `dns_lookup` so programs that never resolve a name pay for no threads.
    resolver: OnceLock<OsResolver>,
    /// The termios of every descriptor `tty_raw` switched, keyed by the handle's token, so `tty_raw(h, false)` and [`Drop`] restore exactly what the program found. The first host state with an exit obligation: a terminal left raw outlives the process that switched it.
    termios: Mutex<Vec<(Vec<u8>, Termios)>>,
}

impl OsHost {
    fn new() -> Self {
        Self::with_args(env::args_os().map(|arg| arg.into_encoded_bytes()).collect())
    }

    /// Build a host whose `proc_args` are the given byte strings — used by the CLI to forward a program's own arguments instead of the `curios` process's.
    pub fn with_args(args: Vec<Vec<u8>>) -> Self {
        Self {
            table: Mutex::new(Table::new()),
            start: Instant::now(),
            args,
            resolver: OnceLock::new(),
            termios: Mutex::new(Vec::new()),
        }
    }

    /// Run `apply` over the descriptor behind `handle` — a standard stream's, or an open file's out of the table. `None` for a handle with no descriptor a terminal `ioctl` could address, which the callers report as `NotFound`.
    fn with_fd<R>(&self, handle: &Handle, apply: impl FnOnce(BorrowedFd<'_>) -> R) -> Option<R> {
        match handle {
            Handle::Stdin => Some(apply(stdin().as_fd())),
            Handle::Stdout => Some(apply(stdout().as_fd())),
            Handle::Stderr => Some(apply(stderr().as_fd())),
            Handle::Other(_) => match self.table.lock().unwrap().get(handle)? {
                OsResource::File(file) => Some(apply(file.as_fd())),
                OsResource::Descriptor(fd) => Some(apply(fd.as_fd())),
                _ => None,
            },
        }
    }

    /// Whether `handle` names a stream that moves bytes in `direction` — the answer to a request of nothing, given without I/O. The standard streams are open one way each, and using one the other way is `EBADF`, as it is for a descriptor opened one way; anything that is not a stream at all is `NotFound`.
    fn stream(&self, handle: &Handle, direction: Direction) -> Result<(), Failure> {
        match (handle, direction) {
            (Handle::Stdin, Direction::Read)
            | (Handle::Stdout | Handle::Stderr, Direction::Write) => Ok(()),
            (Handle::Stdin | Handle::Stdout | Handle::Stderr, _) => Err(bad_descriptor()),
            (Handle::Other(_), _) => match self.table.lock().unwrap().get(handle) {
                Some(
                    OsResource::File(_)
                    | OsResource::Connected(_)
                    | OsResource::ClientTls(_)
                    | OsResource::ServerTls(_)
                    | OsResource::Descriptor(_),
                ) => Ok(()),
                _ => Err(Failure::NotFound),
            },
        }
    }

    /// Mint a fresh handle for `resource` under the table lock (see [`Table::mint`]).
    fn mint(&self, resource: OsResource) -> Handle {
        self.table.lock().unwrap().mint(resource)
    }

    /// Pull an unconnected socket out of the table by handle, leaving any other resource (or none) in place. Lets `socket_connect`/`socket_listen` transition a handle in place.
    fn take_unconnected(&self, handle: &Handle) -> Option<Socket> {
        self.table
            .lock()
            .unwrap()
            .take_if(handle, |resource| match resource {
                OsResource::Unconnected(socket) => Ok(socket),
                other => Err(other),
            })
    }

    /// Pull a connected stream socket out of the table by handle, leaving any other resource (or none) in place. Lets `tls_start`/`tls_start_server` upgrade a handle without holding the lock across the blocking handshake.
    fn take_connected(&self, handle: &Handle) -> Option<Socket> {
        self.table
            .lock()
            .unwrap()
            .take_if(handle, |resource| match resource {
                OsResource::Connected(socket) => Ok(socket),
                other => Err(other),
            })
    }

    /// Apply a `socket2` setter to a configurable handle. Every socket kind — unconnected, connecting, connected, or listening — exposes its typed setters directly; a `File` has no socket options, so that path records nothing and succeeds.
    ///
    /// The match selects a socket rather than answering, so the failure contract — a setter error is a [`failure_from_error`], never a quiet success — is written once instead of once per kind. A resource added later picks its socket or returns; there is no copy of that contract for it to get wrong.
    fn with_socket<F>(&self, handle: &Handle, apply: F) -> Result<(), Failure>
    where
        F: FnOnce(&Socket) -> io::Result<()>,
    {
        let table = self.table.lock().unwrap();

        let socket = match handle {
            // The standard streams are the process's, shared with everything else on the terminal or pipe: no socket option applies to them, so, like a file, they record nothing and succeed. They are never in the table, so asking it would answer `NotFound`, the verdict for a closed handle.
            Handle::Stdin | Handle::Stdout | Handle::Stderr => return Ok(()),
            Handle::Other(_) => match table.get(handle) {
                Some(
                    OsResource::Unconnected(socket)
                    | OsResource::Connecting(socket)
                    | OsResource::Connected(socket)
                    | OsResource::Listener(socket),
                ) => socket,
                // A TLS stream forwards setters to its underlying socket.
                Some(OsResource::ClientTls(stream)) => &stream.sock,
                Some(OsResource::ServerTls(stream)) => &stream.sock,
                // A file, a pipe, a child, a config token, and an in-flight lookup have no socket options: record nothing.
                Some(
                    OsResource::File(_)
                    | OsResource::Descriptor(_)
                    | OsResource::Child { .. }
                    | OsResource::TlsConfig(_)
                    | OsResource::Resolving { .. },
                ) => return Ok(()),
                None => return Err(Failure::NotFound),
            },
        };

        apply(socket).map_err(failure_from_error)
    }
}

impl Default for OsHost {
    fn default() -> Self {
        Self::new()
    }
}

/// Restore every terminal `tty_raw` switched. `instantiate` drops the host after a trap is classified and before the process exits, so a trap or an `exit` leaves the terminal as the program found it, whether or not the program's own bracket ran.
impl Drop for OsHost {
    fn drop(&mut self) {
        let records = std::mem::take(&mut *self.termios.lock().unwrap());

        for (token, saved) in records {
            let handle = Handle::from_bytes(token);
            let _ = self.with_fd(&handle, |fd| tcsetattr(fd, OptionalActions::Now, &saved));
        }
    }
}

impl HostOps for OsHost {
    fn file_open(&self, path: Vec<u8>, mode: Mode) -> Result<Handle, Failure> {
        let path = OsStr::from_bytes(&path);

        let mut options = OpenOptions::new();

        match mode {
            Mode::Read => options.read(true),
            Mode::Write => options.write(true).create(true).truncate(true),
            Mode::Append => options.append(true).create(true),
        };

        options
            .open(path)
            .map(|file| self.mint(OsResource::File(file)))
            .map_err(failure_from_error)
    }

    fn dns_lookup(&self, host: Vec<u8>, port: u64) -> Result<Handle, Failure> {
        let host = String::from_utf8_lossy(&host).into_owned();
        let address = format!("{host}:{port}");

        // Start the lookup on the pool (booted on first use). A saturated pool sheds the load as a retriable `WouldBlock`; on success the read end and result slot become a `Resolving` handle the scheduler polls.
        match self
            .resolver
            .get_or_init(OsResolver::default)
            .start(address)
        {
            Ok(Some(pending)) => Ok(self.mint(OsResource::Resolving {
                done: pending.fd,
                slot: pending.slot,
            })),
            Ok(None) => Err(Failure::WouldBlock),
            Err(failure) => Err(failure),
        }
    }

    fn dns_resolve(&self, handle: Handle) -> Result<Vec<Vec<u8>>, Failure> {
        // Drain the finished lookup. Reached only after `handle_poll` reports the handle ready, so the slot is filled; a stray early call leaves the handle intact and honestly reports `WouldBlock` so the caller can retry.
        let mut table = self.table.lock().unwrap();

        let ready = match table.get(&handle) {
            Some(OsResource::Resolving { slot, .. }) => slot.get(),
            _ => return Err(Failure::NotFound),
        };

        match ready {
            // Drop the handle (closing the pipe read end) only once drained.
            Some(resolved) => {
                table.remove(&handle);
                resolved.into_reply()
            }
            None => Err(Failure::WouldBlock),
        }
    }

    fn socket_open(&self, addr: Vec<u8>) -> Result<Handle, Failure> {
        // The address blob is the canonical "ip:port" string `dns_resolve` minted.
        let address = match String::from_utf8_lossy(&addr).parse::<SocketAddr>() {
            Ok(address) => address,
            Err(_) => return Err(Failure::NotFound),
        };

        // Non-blocking from birth: a peer decides when this socket progresses, so `socket_connect`, `handle_read` and `handle_write` on it answer `WouldBlock` rather than wait, and `handle_poll` is where the wait happens. `Socket::new` then `set_nonblocking` is the spelling both release targets share.
        let created = Socket::new(Domain::for_address(address), Type::STREAM, None)
            .and_then(|socket| socket.set_nonblocking(true).map(|()| socket));

        created
            .map(|socket| self.mint(OsResource::Unconnected(socket)))
            .map_err(failure_from_error)
    }

    fn socket_bind(&self, io: Handle, addr: Vec<u8>) -> Result<(), Failure> {
        // The address blob is the canonical "ip:port" string `dns_resolve` minted.
        let address = match String::from_utf8_lossy(&addr).parse::<SocketAddr>() {
            Ok(address) => address,
            Err(_) => return Err(Failure::NotFound),
        };

        match self.table.lock().unwrap().get(&io) {
            Some(OsResource::Unconnected(socket)) => socket
                .bind(&SockAddr::from(address))
                .map_err(failure_from_error),
            _ => Err(Failure::NotFound),
        }
    }

    fn socket_connect(&self, io: Handle, addr: Vec<u8>) -> Result<(), Failure> {
        // The address blob is the canonical "ip:port" string `dns_resolve` minted.
        let address = match String::from_utf8_lossy(&addr).parse::<SocketAddr>() {
            Ok(address) => address,
            Err(_) => return Err(Failure::NotFound),
        };

        let socket = match self.take_unconnected(&io) {
            Some(socket) => socket,
            None => return Err(Failure::NotFound),
        };

        // A non-blocking connect answers at once: `Ok` when the kernel completed it synchronously, as loopback often does, `EINPROGRESS` when it is under way — the socket is re-filed as connecting for `handle_poll` to watch and `socket_finish_connect` to settle — and its refusal otherwise, on which the socket drops. `EINPROGRESS` and `EALREADY` have no `ErrorKind`, so they are matched by errno; an interrupted connect continues asynchronously by POSIX and is filed the same way.
        match socket.connect(&SockAddr::from(address)) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Connected(socket));

                Ok(())
            }
            Err(error) if is_errno(&error, Errno::ISCONN) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Connected(socket));

                Ok(())
            }
            Err(error)
                if is_errno(&error, Errno::INPROGRESS)
                    || is_errno(&error, Errno::ALREADY)
                    || error.kind() == ErrorKind::Interrupted =>
            {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Connecting(socket));

                Err(Failure::WouldBlock)
            }
            Err(error) => Err(failure_from_error(error)),
        }
    }

    fn socket_finish_connect(&self, io: Handle) -> Result<(), Failure> {
        let mut table = self.table.lock().unwrap();
        let socket = match table.take_if(&io, |resource| match resource {
            OsResource::Connecting(socket) => Ok(socket),
            other => Err(other),
        }) {
            Some(socket) => socket,
            // A connect that completed synchronously was never pending, so settling it is a no-op rather than a fault.
            None => {
                return match table.get(&io) {
                    Some(OsResource::Connected(_)) => Ok(()),
                    _ => Err(Failure::NotFound),
                };
            }
        };

        // `SO_ERROR` is zero both while the connect is pending and after it succeeded, so a clean report is followed by asking for the peer: `ENOTCONN` is the pending answer, and the socket goes back as connecting for another poll. Neither call blocks, so the lock is held across them.
        match socket.take_error() {
            Ok(Some(error)) | Err(error) => Err(failure_from_error(error)),
            Ok(None) => match socket.peer_addr() {
                Ok(_) => {
                    table.insert(&io, OsResource::Connected(socket));

                    Ok(())
                }
                Err(error) if is_errno(&error, Errno::NOTCONN) => {
                    table.insert(&io, OsResource::Connecting(socket));

                    Err(Failure::WouldBlock)
                }
                Err(error) => Err(failure_from_error(error)),
            },
        }
    }

    fn tls_start(&self, io: Handle, sni: Vec<u8>) -> Result<(), Failure> {
        let server_name = match std::str::from_utf8(&sni)
            .ok()
            .and_then(|name| ServerName::try_from(name.to_owned()).ok())
        {
            Some(name) => name,
            None => return Err(Failure::TlsError),
        };

        let socket = match self.take_connected(&io) {
            Some(socket) => socket,
            None => return Err(Failure::NotFound),
        };

        let conn = match ClientConnection::new(CLIENT_CONFIG.clone(), server_name) {
            Ok(conn) => conn,
            Err(_) => return Err(Failure::TlsError),
        };

        // The stream is filed with its handshake still to run: the socket is non-blocking, so the handshake is driven by the reads and writes that follow — `rustls`'s stream completes prior IO before each — and parks the fiber through `handle_poll` like any other progress. A verification or protocol failure surfaces as `TlsError` from the read or write that discovers it.
        self.table.lock().unwrap().insert(
            &io,
            OsResource::ClientTls(Box::new(StreamOwned::new(conn, socket))),
        );

        Ok(())
    }

    fn tls_server_config(&self, cert: Vec<u8>, key: Vec<u8>) -> Result<Handle, Failure> {
        let certs = match rustls_pemfile::certs(&mut cert.as_slice()).collect::<Result<Vec<_>, _>>()
        {
            Ok(certs) if !certs.is_empty() => certs,
            _ => return Err(Failure::TlsError),
        };

        let key = match rustls_pemfile::private_key(&mut key.as_slice()) {
            Ok(Some(key)) => key,
            _ => return Err(Failure::TlsError),
        };

        let config = match ServerConfig::builder_with_provider(Arc::new(ring::default_provider()))
            .with_safe_default_protocol_versions()
            .expect("ring provider supports the default protocol versions")
            .with_no_client_auth()
            .with_single_cert(certs, key)
        {
            Ok(config) => Arc::new(config),
            Err(_) => return Err(Failure::TlsError),
        };

        Ok(self.mint(OsResource::TlsConfig(config)))
    }

    fn tls_start_server(&self, io: Handle, cfg: Handle) -> Result<(), Failure> {
        // Clone the config `Arc` out, never holding the lock across the handshake. The config handle stays in the table for reuse.
        let config = match self.table.lock().unwrap().get(&cfg) {
            Some(OsResource::TlsConfig(config)) => config.clone(),
            _ => return Err(Failure::NotFound),
        };

        let socket = match self.take_connected(&io) {
            Some(socket) => socket,
            None => return Err(Failure::NotFound),
        };

        let conn = match ServerConnection::new(config) {
            Ok(conn) => conn,
            Err(_) => return Err(Failure::TlsError),
        };

        // Filed with the handshake still to run, as `tls_start` files the client side.
        self.table.lock().unwrap().insert(
            &io,
            OsResource::ServerTls(Box::new(StreamOwned::new(conn, socket))),
        );

        Ok(())
    }

    fn socket_listen(&self, io: Handle, backlog: u64) -> Result<(), Failure> {
        let socket = match self.take_unconnected(&io) {
            Some(socket) => socket,
            None => return Err(Failure::NotFound),
        };

        // The kernel clamps the depth to `somaxconn`, so one past what `listen(2)` takes asks for the same queue.
        match socket.listen(i32::try_from(backlog).unwrap_or(i32::MAX)) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Listener(socket));

                Ok(())
            }
            Err(error) => Err(failure_from_error(error)),
        }
    }

    fn socket_accept(&self, io: Handle) -> Result<Handle, Failure> {
        // The listener is non-blocking, so the accept answers at once under the lock: `WouldBlock` with nothing pending, else the stream. `accept4` hands the stream over blocking whatever the listener's flag, so it is switched here, since a fiber will drain it.
        let mut table = self.table.lock().unwrap();
        let accepted = match table.get(&io) {
            Some(OsResource::Listener(socket)) => socket.accept(),
            _ => return Err(Failure::NotFound),
        };

        accepted
            .and_then(|(stream, _)| stream.set_nonblocking(true).map(|()| stream))
            .map(|stream| table.mint(OsResource::Connected(stream)))
            .map_err(failure_from_error)
    }

    fn socket_set_reuseaddr(&self, io: Handle, on: bool) -> Result<(), Failure> {
        self.with_socket(&io, |socket| socket.set_reuse_address(on))
    }

    fn handle_poll(
        &self,
        handles: Vec<Handle>,
        events: Vec<Poll>,
        timeout_ms: i64,
    ) -> Result<Vec<Poll>, Refusal> {
        // The deadline is taken at the call, so a retried interruption does not restart the wait. A negative timeout waits forever, and so does one past what an `Instant` can hold.
        let deadline = u64::try_from(timeout_ms)
            .ok()
            .and_then(|millis| Instant::now().checked_add(Duration::from_millis(millis)));

        let table = self.table.lock().unwrap();

        // Keep the stdio owners alive for the duration of the borrow: each `PollFd` holds a `BorrowedFd` into one of these (or into the table).
        let (in_handle, out_handle, err_handle) = (stdin(), stdout(), stderr());

        // One entry per distinct descriptor, asking for the union of every slot's interest in it, so a descriptor several handles name is polled once and never counts against the system's limit twice. Each slot remembers its entry, its own interest, and the guest's where `rustls`'s replaced it. A handle that is unknown — closed, say — or names a resource with no descriptor is never polled and reports `ERR`, which wakes whoever waits on it into the call that reports why, rather than leaving it parked on readiness that cannot come.
        let mut watched: Vec<(BorrowedFd<'_>, Poll)> = Vec::new();
        let mut slots = Vec::with_capacity(handles.len());
        let mut results = vec![Poll::empty(); handles.len()];

        for (slot, (handle, &requested)) in handles.iter().zip(&events).enumerate() {
            let resolved = match handle {
                Handle::Stdin => Some((in_handle.as_fd(), requested)),
                Handle::Stdout => Some((out_handle.as_fd(), requested)),
                Handle::Stderr => Some((err_handle.as_fd(), requested)),
                Handle::Other(_) => table.get(handle).and_then(|resource| match resource {
                    OsResource::File(file) => Some((file.as_fd(), requested)),
                    // A connecting socket is watched for `WRITE`, which is what the kernel reports once the connect has settled either way.
                    OsResource::Connected(socket)
                    | OsResource::Connecting(socket)
                    | OsResource::Unconnected(socket)
                    | OsResource::Listener(socket) => Some((socket.as_fd(), requested)),
                    // The lookup's pipe read end: `READ`-ready once the worker has written its wakeup byte, which is the completion signal.
                    OsResource::Resolving { done, .. } => Some((done.as_fd(), requested)),
                    OsResource::Descriptor(fd) => Some((fd.as_fd(), requested)),
                    // The reaper's pipe read end: `READ`-ready once the child's end is recorded, which is when `proc_wait` answers.
                    OsResource::Child { running, .. } => Some((running.done.as_fd(), requested)),
                    // A TLS stream is watched through its socket, for the interest `rustls` itself has while the handshake is under way and the guest's own afterwards; the config token has no descriptor.
                    OsResource::ClientTls(stream) => {
                        Some((stream.sock.as_fd(), tls_interest(&stream.conn, requested)))
                    }
                    OsResource::ServerTls(stream) => {
                        Some((stream.sock.as_fd(), tls_interest(&stream.conn, requested)))
                    }
                    OsResource::TlsConfig(_) => None,
                }),
            };

            let Some((fd, interest)) = resolved else {
                results[slot] = Poll::from_bits(event::ERR);

                continue;
            };

            let entry = match watched
                .iter()
                .position(|(known, _)| known.as_raw_fd() == fd.as_raw_fd())
            {
                Some(entry) => {
                    watched[entry].1 = Poll::from_bits(watched[entry].1.bits() | interest.bits());

                    entry
                }
                None => {
                    watched.push((fd, interest));

                    watched.len() - 1
                }
            };

            // Where the watched interest is not the guest's own, readiness is reported in the guest's terms: the guest parked for what it asked, and a wake on what `rustls` needed is a wake for it too — reported as the substituted bits alone, the guest would look for its own, see nothing, and re-poll a socket that answers at once, forever.
            let translated = (interest != requested).then_some(requested);
            slots.push((slot, entry, interest, translated));
        }

        let mut polls = watched
            .iter()
            .map(|(fd, interest)| PollFd::from_borrowed_fd(*fd, poll_to_flags(*interest)))
            .collect::<Vec<_>>();

        // An interruption is not readiness, so the poll resumes for what is left of the wait; any other failure is the system refusing the poll itself, which no readiness report can say, so the call is refused naming it.
        loop {
            let timeout = deadline.map(|deadline| {
                let left = deadline.saturating_duration_since(Instant::now());

                Timespec {
                    tv_sec: left.as_secs() as _,
                    tv_nsec: left.subsec_nanos() as _,
                }
            });

            match poll(&mut polls, timeout.as_ref()) {
                Ok(_) => break,
                Err(Errno::INTR) => {}
                Err(errno) => {
                    return Err(Refusal(format!(
                        "the system refused the poll: {}",
                        Error::from(errno)
                    )));
                }
            }
        }

        // Each slot reads its own interest out of what its descriptor reported, with the result-only `ERR` and `HUP` beside it.
        for (slot, entry, interest, translated) in slots {
            let ready = poll_from_flags(polls[entry].revents()).bits()
                & (interest.bits() | event::ERR | event::HUP);

            results[slot] = match translated {
                Some(requested) if ready != 0 => Poll::from_bits(ready | requested.bits()),
                _ => Poll::from_bits(ready),
            };
        }

        Ok(results)
    }

    fn handle_close(&self, io: Handle) {
        self.table.lock().unwrap().remove(&io);
    }

    fn handle_read(&self, io: Handle, count: u64) -> Result<Option<Vec<u8>>, Failure> {
        // A request for nothing reads nothing: it only asks whether the handle could be read, and never answers the end of a stream it did not look at.
        if count == 0 {
            return self.stream(&io, Direction::Read).map(|()| Some(Vec::new()));
        }

        let mut buffer = vec![0; count.min(READ_MAX) as usize];

        let result = match &io {
            // Standard input is the process's, shared with the terminal or the pipe that feeds it, so its descriptor's flags are never touched; the read is gated by a zero-timeout poll instead, answering `WouldBlock` when nothing is there, which is how a shared descriptor keeps the rule that no row waits on a peer. On the raw descriptor, as the write below is, rather than through `std::io::Stdin`'s buffered reader: a request smaller than what arrived would leave the remainder in a buffer `handle_poll` cannot see, and a fiber waiting on fd 0 would stall with input already inside the process.
            Handle::Stdin => {
                let input = stdin();

                if !readable_now(input.as_fd()) {
                    return Err(Failure::WouldBlock);
                }

                rustix::io::read(&input, &mut buffer[..]).map_err(Error::from)
            }
            // The output streams are open to write only, as a descriptor opened to write is.
            Handle::Stdout | Handle::Stderr => return Err(bad_descriptor()),
            Handle::Other(_) => {
                let mut table = self.table.lock().unwrap();
                let stream: &mut dyn Read = match table.get_mut(&io) {
                    Some(OsResource::File(file)) => file,
                    Some(OsResource::Connected(socket)) => socket,
                    // A TLS read drives whatever handshake or record exchange is pending first, so its failures are `rustls`'s as well as the socket's.
                    Some(OsResource::ClientTls(tls)) => {
                        let result = tls.read(&mut buffer);

                        return tls_read_outcome(result, buffer);
                    }
                    Some(OsResource::ServerTls(tls)) => {
                        let result = tls.read(&mut buffer);

                        return tls_read_outcome(result, buffer);
                    }
                    Some(OsResource::Descriptor(fd)) => {
                        let result = rustix::io::read(&*fd, &mut buffer[..]);

                        return read_outcome(result.map_err(Error::from), buffer);
                    }
                    // A missing or non-stream handle is a fault, not an exhausted stream — mirror write's `NotFound` so use-after-close stays loud.
                    _ => return Err(Failure::NotFound),
                };

                stream.read(&mut buffer)
            }
        };

        read_outcome(result, buffer)
    }

    fn handle_write(&self, io: Handle, bytes: Vec<u8>) -> Result<u64, Failure> {
        // An empty write writes nothing: it only asks whether the handle could be written.
        if bytes.is_empty() {
            return self.stream(&io, Direction::Write).map(|()| 0);
        }

        match io {
            // The standard output streams are written through their raw descriptors, one attempt per call as every other handle is: `std`'s own buffer would hold what the guest wrote where neither the other stream's writes nor a flush can reach it, and the guest's output would arrive out of the order it was written in.
            Handle::Stdout => {
                return write_once(|| rustix::io::write(stdout(), &bytes).map_err(Error::from));
            }
            Handle::Stderr => {
                return write_once(|| rustix::io::write(stderr(), &bytes).map_err(Error::from));
            }
            // Standard input is open to read only, as a descriptor opened to read is.
            Handle::Stdin => return Err(bad_descriptor()),
            Handle::Other(_) => {}
        }

        let mut table = self.table.lock().unwrap();

        match table.get_mut(&io) {
            Some(OsResource::File(file)) => write_once(|| file.write(&bytes)),
            Some(OsResource::Connected(socket)) => write_once(|| socket.write(&bytes)),
            // A TLS write completes the pending handshake first and accepts no plaintext until it has, so `WouldBlock` here reports nothing accepted and the caller resends. Once established it takes plaintext into `rustls`'s records, as much as its buffer holds, and pushes them to the socket as far as the socket allows; what the socket did not take waits for the next read, write or `handle_flush` of the handle, and a `handle_close` drops it.
            Some(OsResource::ClientTls(tls)) => tls_write(|| tls.write(&bytes)),
            Some(OsResource::ServerTls(tls)) => tls_write(|| tls.write(&bytes)),
            Some(OsResource::Descriptor(fd)) => {
                write_once(|| rustix::io::write(&*fd, &bytes).map_err(Error::from))
            }
            _ => Err(Failure::NotFound),
        }
    }

    fn handle_flush(&self, io: Handle) -> Result<(), Failure> {
        if matches!(io, Handle::Stdin | Handle::Stdout | Handle::Stderr) {
            return Ok(());
        }

        // Only a TLS stream holds anything of what a write accepted: its records, which `rustls` pushes to the socket as far as the socket takes them. Every other kind wrote straight through.
        match self.table.lock().unwrap().get_mut(&io) {
            Some(OsResource::ClientTls(tls)) => flush_tls(&mut tls.conn, &mut tls.sock),
            Some(OsResource::ServerTls(tls)) => flush_tls(&mut tls.conn, &mut tls.sock),
            Some(_) => Ok(()),
            None => Err(Failure::NotFound),
        }
    }

    fn clock_wall(&self) -> Timestamp {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();

        Timestamp {
            secs: now.as_secs(),
            nanos: u64::from(now.subsec_nanos()),
        }
    }

    fn clock_mono(&self) -> Timestamp {
        let elapsed = self.start.elapsed();

        Timestamp {
            secs: elapsed.as_secs(),
            nanos: u64::from(elapsed.subsec_nanos()),
        }
    }

    fn rand_bytes(&self, count: u64) -> Result<Vec<u8>, Refusal> {
        // Reserved rather than allocated, so a request for more memory than the host has refuses the call rather than aborting the process.
        let unreserved = || Refusal(format!("{count} random bytes do not fit in memory"));
        let length = usize::try_from(count).map_err(|_| unreserved())?;
        let mut buffer = Vec::new();

        buffer.try_reserve_exact(length).map_err(|_| unreserved())?;
        buffer.resize(length, 0);
        getrandom::fill(&mut buffer)
            .map_err(|error| Refusal(format!("the system gave no randomness: {error}")))?;

        Ok(buffer)
    }

    fn proc_args(&self) -> Vec<Vec<u8>> {
        self.args.clone()
    }

    fn proc_env(&self, name: Vec<u8>) -> Option<Vec<u8>> {
        env::var_os(OsStr::from_bytes(&name)).map(|value| value.into_encoded_bytes())
    }

    // The code leaves as the guest-exit trap; the terminal records `Drop` restores are what the process leaves behind.
    fn proc_exit(&self, code: u8) -> Termination {
        Termination(code)
    }

    fn tty_raw(&self, io: Handle, on: bool) -> Result<(), Failure> {
        let token = io.bytes();

        let outcome = self.with_fd(&io, |fd| {
            let mut records = self.termios.lock().unwrap();
            let recorded = records.iter().position(|(saved, _)| *saved == token);

            match (on, recorded) {
                // The record is taken once, on the first switch, so a second `tty_raw(h, true)` cannot overwrite the settings the program found with raw ones.
                (true, recorded) => {
                    let current = tcgetattr(fd)?;

                    if recorded.is_none() {
                        records.push((token.clone(), current.clone()));
                    }

                    let mut raw = current;
                    raw.make_raw();

                    tcsetattr(fd, OptionalActions::Now, &raw)
                }
                (false, Some(index)) => {
                    let (_, saved) = records.remove(index);

                    tcsetattr(fd, OptionalActions::Now, &saved)
                }
                // Never switched: there is nothing to restore.
                (false, None) => Ok(()),
            }
        });

        match outcome {
            None => Err(Failure::NotFound),
            Some(Ok(())) => Ok(()),
            Some(Err(errno)) => Err(failure_from_error(Error::from(errno))),
        }
    }

    fn tty_size(&self, io: Handle) -> Result<TtySize, Failure> {
        match self.with_fd(&io, |fd| tcgetwinsize(fd)) {
            None => Err(Failure::NotFound),
            Some(Ok(size)) => Ok(TtySize {
                cols: size.ws_col.into(),
                rows: size.ws_row.into(),
            }),
            Some(Err(errno)) => Err(failure_from_error(Error::from(errno))),
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
        // A frame outside the row's ranges is refused before the device is touched, so it can neither leave a half-configured port behind nor reset a board through the open's DTR.
        let Some(frame) = serial_frame(data_bits, parity, stop_bits, flow) else {
            return Err(failure_from_error(Error::from(Errno::INVAL)));
        };

        // Non-blocking from the open rather than switched after it, because opening a port whose carrier line is down waits for carrier until `CLOCAL` is set, and `CLOCAL` is set on a descriptor already open. `NOCTTY` keeps the port from becoming this process's controlling terminal, and `CLOEXEC` keeps a spawned child from holding it past the program's own close. No exclusive hold is taken: whether `TIOCEXCL` refuses a second open differs by kernel, by device and by privilege, so the row promises only what every kernel does.
        let opened = rustix::fs::open(
            path.as_slice(),
            OFlags::RDWR | OFlags::NOCTTY | OFlags::NONBLOCK | OFlags::CLOEXEC,
            rustix::fs::Mode::empty(),
        )
        .and_then(|fd| {
            let mut termios = tcgetattr(&fd)?;

            termios.make_raw();
            termios.control_modes -= ControlModes::CSIZE
                | ControlModes::PARENB
                | ControlModes::PARODD
                | ControlModes::CSTOPB
                | ControlModes::CRTSCTS;
            termios.control_modes |= frame | ControlModes::CLOCAL | ControlModes::CREAD;
            termios.set_speed(u32::try_from(baud).map_err(|_| Errno::INVAL)?)?;

            tcsetattr(&fd, OptionalActions::Now, &termios)?;

            Ok(fd)
        });

        opened
            .map(|fd| self.mint(OsResource::Descriptor(fd)))
            .map_err(|errno| failure_from_error(Error::from(errno)))
    }

    fn serial_control(&self, io: Handle, op: SerialOp, on: bool) -> Result<(), Failure> {
        let outcome = self.with_fd(&io, |fd| match op {
            SerialOp::Dtr => set_modem_lines(fd, TIOCM_DTR, on),
            SerialOp::Rts => set_modem_lines(fd, TIOCM_RTS, on),
            SerialOp::DiscardInput => tcflush(fd, QueueSelector::IFlush),
        });

        match outcome {
            None => Err(Failure::NotFound),
            Some(Ok(())) => Ok(()),
            Some(Err(errno)) => Err(failure_from_error(Error::from(errno))),
        }
    }

    fn file_stat(&self, path: Vec<u8>) -> Result<FileStat, Failure> {
        let path = OsStr::from_bytes(&path);

        let metadata = match fs::metadata(path) {
            Ok(metadata) => metadata,
            // Following the link found nothing. `symlink_metadata` tells a dangling link from a path with nothing at all, and it is the one case the `symlink` kind is reported.
            Err(error) if error.kind() == ErrorKind::NotFound => {
                return match fs::symlink_metadata(path) {
                    Ok(link) if link.file_type().is_symlink() => Ok(FileStat {
                        kind: FileKind::Symlink,
                        size: 0,
                        mtime_secs: 0,
                        mtime_nanos: 0,
                    }),
                    _ => Err(failure_from_error(error)),
                };
            }
            Err(error) => return Err(failure_from_error(error)),
        };

        let file_type = metadata.file_type();
        let kind = match () {
            () if file_type.is_dir() => FileKind::Directory,
            () if file_type.is_file() => FileKind::File,
            () => FileKind::Other,
        };
        let (mtime_secs, mtime_nanos) = metadata
            .modified()
            .ok()
            .and_then(|modified| modified.duration_since(UNIX_EPOCH).ok())
            .map(|since_epoch| (since_epoch.as_secs(), u64::from(since_epoch.subsec_nanos())))
            .unwrap_or((0, 0));

        Ok(FileStat {
            kind,
            size: metadata.len(),
            mtime_secs,
            mtime_nanos,
        })
    }

    fn file_remove(&self, path: Vec<u8>) -> Result<(), Failure> {
        fs::remove_file(OsStr::from_bytes(&path)).map_err(failure_from_error)
    }

    fn file_rename(&self, from: Vec<u8>, to: Vec<u8>) -> Result<(), Failure> {
        fs::rename(OsStr::from_bytes(&from), OsStr::from_bytes(&to)).map_err(failure_from_error)
    }

    fn dir_list(&self, path: Vec<u8>) -> Result<Vec<Vec<u8>>, Failure> {
        let entries = fs::read_dir(OsStr::from_bytes(&path)).map_err(failure_from_error)?;

        let mut names = Vec::new();

        for entry in entries {
            names.push(entry.map_err(failure_from_error)?.file_name().into_vec());
        }

        // The directory's own order is whatever the filesystem keeps; sorted, two listings of one directory agree and a test can pin one.
        names.sort();

        Ok(names)
    }

    fn dir_create(&self, path: Vec<u8>) -> Result<(), Failure> {
        fs::create_dir(OsStr::from_bytes(&path)).map_err(failure_from_error)
    }

    fn dir_remove(&self, path: Vec<u8>) -> Result<(), Failure> {
        fs::remove_dir(OsStr::from_bytes(&path)).map_err(failure_from_error)
    }

    fn proc_cwd(&self) -> Result<Vec<u8>, Failure> {
        env::current_dir()
            .map(|path| path.into_os_string().into_vec())
            .map_err(failure_from_error)
    }

    fn proc_spawn(
        &self,
        argv: Vec<Vec<u8>>,
        cwd: Vec<u8>,
        env: Vec<Vec<u8>>,
        stdin: StdioMode,
        stdout: StdioMode,
        stderr: StdioMode,
    ) -> Result<Handle, Failure> {
        let Spawned {
            child,
            stdin,
            stdout,
            stderr,
        } = os_child::spawn(&argv, &cwd, &env, (stdin, stdout, stderr))?;

        // Each piped stream's parent end, non-blocking already, filed as a `Descriptor` the stream rows serve as they serve any pipe.
        let file = |fd: Option<OwnedFd>| fd.map(|fd| self.mint(OsResource::Descriptor(fd)));
        let streams = Box::new([file(stdin), file(stdout), file(stderr)]);

        Ok(self.mint(OsResource::Child {
            running: child,
            streams,
        }))
    }

    fn proc_stream(&self, child: Handle, which: ChildStream) -> Result<Handle, Failure> {
        match self.table.lock().unwrap().get(&child) {
            // A stream that was not piped has no handle, which is `not_found` as an unknown handle is.
            Some(OsResource::Child { streams, .. }) => streams[stream_index(which)]
                .clone()
                .ok_or(Failure::NotFound),
            _ => Err(Failure::NotFound),
        }
    }

    fn proc_wait(&self, child: Handle) -> Result<ChildExit, Failure> {
        // Reached once `handle_poll` reports the child's handle ready, so the end is recorded; an early call leaves the handle intact and reports `WouldBlock`, as `dns_resolve` does. Once answered — an exit, or the failure that kept the reaper from observing one — the handle is consumed.
        let mut table = self.table.lock().unwrap();

        let end = match table.get(&child) {
            Some(OsResource::Child { running, .. }) => running.end(),
            _ => return Err(Failure::NotFound),
        };

        match end {
            Some(end) => {
                table.remove(&child);

                end
            }
            None => Err(Failure::WouldBlock),
        }
    }

    fn proc_kill(&self, child: Handle) -> Result<(), Failure> {
        match self.table.lock().unwrap().get(&child) {
            Some(OsResource::Child { running, .. }) => running.kill(),
            _ => Err(Failure::NotFound),
        }
    }
}

/// Whether `fd` has input to read right now: a zero-timeout poll for `IN`, with a hang-up or an error counting as readable, since a read then answers at once with the end of the stream or the fault. A failed poll answers `true`, so the read runs and reports the fault itself.
fn readable_now(fd: BorrowedFd<'_>) -> bool {
    let mut polls = [PollFd::from_borrowed_fd(fd, PollFlags::IN)];
    let now = Timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };

    match poll(&mut polls, Some(&now)) {
        Ok(_) => polls[0]
            .revents()
            .intersects(PollFlags::IN | PollFlags::HUP | PollFlags::ERR),
        Err(_) => true,
    }
}

// The modem-line ioctls rustix does not wrap, as each platform numbers them: Linux's generic tty numbers, which both release architectures use, and the BSD family's `_IOW('t', 108, int)` and `_IOW('t', 107, int)`, which macOS keeps. The line bits agree across all of them.
#[cfg(target_os = "linux")]
const TIOCMBIS: Opcode = 0x5416;
#[cfg(target_os = "linux")]
const TIOCMBIC: Opcode = 0x5417;
#[cfg(not(target_os = "linux"))]
const TIOCMBIS: Opcode = 0x8004_746C;
#[cfg(not(target_os = "linux"))]
const TIOCMBIC: Opcode = 0x8004_746B;
const TIOCM_DTR: c_int = 0x002;
const TIOCM_RTS: c_int = 0x004;

/// Raise (`on`) or lower the modem lines `mask` names on `fd`. `TIOCMBIS` and `TIOCMBIC` touch only those lines, where a `TIOCMGET` read back and a `TIOCMSET` of the lot would race whatever else drives the port between the two.
fn set_modem_lines(fd: BorrowedFd<'_>, mask: c_int, on: bool) -> rustix::io::Result<()> {
    // SAFETY: both opcodes take a pointer to an `int` holding the line mask and write nothing back, which is the opcode and input type `Setter` is built for.
    unsafe {
        match on {
            true => ioctl(fd, Setter::<TIOCMBIS, c_int>::new(mask)),
            false => ioctl(fd, Setter::<TIOCMBIC, c_int>::new(mask)),
        }
    }
}

/// Whether `error` carries the OS errno `errno` — the read for the connect statuses that have no `ErrorKind`.
fn is_errno(error: &Error, errno: Errno) -> bool {
    error.raw_os_error() == Some(errno.raw_os_error())
}

/// The failure a TLS stream's read or write reports: `rustls`'s own errors — a failed verification, a protocol violation, a plaintext peer — arrive wrapped in an `InvalidData` error and collapse to `TlsError`, and everything else is the socket's, mapped as every other stream maps it.
fn tls_failure(error: Error) -> Failure {
    if error
        .get_ref()
        .is_some_and(|inner| inner.is::<rustls::Error>())
    {
        return Failure::TlsError;
    }

    failure_from_error(error)
}

/// A TLS read's outcome as the row's reply: a peer that closed without `close_notify` reads as the end of the stream, since a length-framed protocol notices a truncation itself, and the rest as [`tls_failure`] maps it.
fn tls_read_outcome(
    result: io::Result<usize>,
    buffer: Vec<u8>,
) -> Result<Option<Vec<u8>>, Failure> {
    match result {
        Err(error) if error.kind() == ErrorKind::UnexpectedEof => Ok(None),
        Err(error) => Err(tls_failure(error)),
        Ok(_) => read_outcome(result, buffer),
    }
}

/// `EBADF`: a stream used in a direction it is not open for.
fn bad_descriptor() -> Failure {
    failure_from_error(Error::from(Errno::BADF))
}

/// One write attempt's reply, retried while an interruption stops it before any progress. A descriptor that takes nothing of a nonempty buffer and reports nothing has no failure to name, so it is the errno-less `Other(0)` — never a success its caller would resend forever.
fn write_once(mut write: impl FnMut() -> io::Result<usize>) -> Result<u64, Failure> {
    loop {
        match write() {
            Ok(0) => return Err(Failure::Other(0)),
            Ok(accepted) => return Ok(accepted as u64),
            Err(error) if error.kind() == ErrorKind::Interrupted => {}
            Err(error) => return Err(failure_from_error(error)),
        }
    }
}

/// A TLS write's reply, retried while an interruption stops it before any progress. `rustls` accepts nothing of a nonempty buffer when its own is full, which is `WouldBlock`: nothing can be accepted now, and the socket's writability is what frees room.
fn tls_write(mut write: impl FnMut() -> io::Result<usize>) -> Result<u64, Failure> {
    loop {
        match write() {
            Ok(0) => return Err(Failure::WouldBlock),
            Ok(accepted) => return Ok(accepted as u64),
            Err(error) if error.kind() == ErrorKind::Interrupted => {}
            Err(error) => return Err(tls_failure(error)),
        }
    }
}

/// Push a TLS stream's pending records to its socket until `rustls` holds none: `WouldBlock` when the socket takes no more, which `handle_poll` reports writable once it can, and a failure as [`tls_failure`] maps it.
fn flush_tls<Data>(conn: &mut ConnectionCommon<Data>, sock: &mut Socket) -> Result<(), Failure> {
    while conn.wants_write() {
        match conn.write_tls(sock) {
            Ok(_) => {}
            Err(error) if error.kind() == ErrorKind::Interrupted => {}
            Err(error) if error.kind() == ErrorKind::WouldBlock => return Err(Failure::WouldBlock),
            Err(error) => return Err(tls_failure(error)),
        }
    }

    Ok(())
}

/// The interest to watch a TLS stream's socket for. While the handshake is under way `rustls`'s own demand replaces the guest's: a socket is nearly always writable, so a fiber that parked on `WRITE` to send its request would spin while `rustls` was in fact waiting to read the server's reply. Afterwards the guest's interest stands, plus `WRITE` whenever `rustls` still holds records to push.
fn tls_interest(conn: &rustls::CommonState, requested: Poll) -> Poll {
    let read = if conn.wants_read() { event::READ } else { 0 };
    let write = if conn.wants_write() { event::WRITE } else { 0 };

    match conn.is_handshaking() {
        true => Poll::from_bits(read | write),
        false => Poll::from_bits(requested.bits() | write),
    }
}

/// The reply of one `handle_read`: a zero count is end of stream, a positive one the prefix it filled, an error its failure. Shared by every descriptor `handle_read` serves, the raw ones included.
fn read_outcome(
    result: io::Result<usize>,
    mut buffer: Vec<u8>,
) -> Result<Option<Vec<u8>>, Failure> {
    match result {
        Ok(0) => Ok(None),
        Ok(n) => {
            buffer.truncate(n);

            Ok(Some(buffer))
        }
        Err(error) => Err(failure_from_error(error)),
    }
}

#[cfg(test)]
mod tests;
