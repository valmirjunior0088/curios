use {
    super::{OsResolver, Running, Slot, Spawned, Table, host::*, os_child},
    curios_abi::{kind, poll as interest},
    rustix::{
        event::{PollFd, Timespec, poll},
        fs::{OFlags, fcntl_getfl, fcntl_setfl},
        io::Errno,
        termios::{OptionalActions, Termios, tcgetattr, tcgetwinsize, tcsetattr},
    },
    rustls::{
        ClientConfig, ClientConnection, RootCertStore, ServerConfig, ServerConnection, StreamOwned,
        crypto::ring, pki_types::ServerName,
    },
    socket2::{Domain, SockAddr, Socket, Type},
    std::{
        env,
        ffi::OsStr,
        fs::{self, File, OpenOptions},
        io::{ErrorKind, Read, Write, stderr, stdin, stdout},
        net::SocketAddr,
        os::{
            fd::{AsFd, BorrowedFd, OwnedFd},
            unix::ffi::{OsStrExt, OsStringExt},
        },
        sync::{Arc, LazyLock, Mutex, OnceLock},
        time::{Instant, SystemTime, UNIX_EPOCH},
    },
    webpki_roots::TLS_SERVER_ROOTS,
};

/// The shared client TLS configuration: a bundled `webpki-roots` trust-anchor set with certificate verification on, built once and `Arc`-cloned by every `start_tls`. An explicit `ring` crypto provider is wired in so the config never depends on a process-global default provider being installed.
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

/// A non-stdio handle in [`OsHost`]'s unified table, tracking the BSD lifecycle with one concrete type per state: `open` files a `File`; `socket` mints an `Unconnected` socket, `connect` turns it into a `Connected` one at once or into a `Connecting` one that `finish_connect` settles (`accept` mints a `Connected` one directly), and `listen` turns it into a `Listener`. `start_tls` / `start_tls_server` upgrade a `Connected` socket in place to a `ClientTls` / `ServerTls` stream; `tls_server_config` files a host-owned `TlsConfig` token. `read`/`write` serve `File`, `Connected`, and both TLS streams alike (all are `Read + Write`); `close` drops any kind, releasing its descriptor.
///
/// Every kind on which a peer decides — a socket in any state, an accepted stream, a pipe to a child — is filed non-blocking at the moment it is minted, so no row waits on a peer: a `read`, `write`, `connect` or `accept` that cannot progress answers `WouldBlock` and `poll` is the one place the host sleeps. A regular file is synchronous, since the disk rather than a peer answers it.
enum OsResource {
    File(File),
    /// An in-flight asynchronous name lookup minted by `lookup`. `done` is the read end of a pipe a worker thread writes one byte to once it has filled `slot` with the `getaddrinfo` result; that write makes `done` poll-`READ` readable, waking the scheduler. `resolve` then drains `slot` and drops the handle (closing `done`). `poll` watches `done` like any other fd.
    Resolving {
        done: OwnedFd,
        slot: Slot,
    },
    /// A bare owned descriptor — one end of a pipe to a child, filed by `spawn`, and the shape a serial device takes. Named by what it holds, as `File` and `Listener` are, and the one thing separating it from `File` is that it is non-blocking for real: whoever files one applies `O_NONBLOCK` through `fcntl` first, so a fiber draining it yields on `WouldBlock` instead of blocking the scheduler, while `read`, `write`, `poll` and `close` serve it as they serve a file.
    Descriptor(OwnedFd),
    /// A running child minted by `spawn`: its `done` pipe end becomes `READ`-ready when the reaper has recorded the exit, `wait` drains it, `kill` addresses its pid, and `stream` hands out the handles of its piped standard streams — filed as `Pipe`s at spawn time and boxed here so a child costs the table no more than a socket does.
    Child {
        running: Running,
        streams: Box<[Handle; 3]>,
    },
    Connected(Socket),
    Unconnected(Socket),
    /// A non-blocking `connect` under way: `EINPROGRESS` filed it, `poll` watches its descriptor for `WRITE`, and `finish_connect` settles it into `Connected` or reports what refused it.
    Connecting(Socket),
    /// A listening socket. It never blocks, so `accept` runs under the table lock like any other row.
    Listener(Socket),
    /// A client-side TLS stream: the encrypted conduit a `Connected` socket became under `start_tls`, serving the same `read`/`write`/`close`.
    ///
    /// Boxed, and so is [`OsResource::ServerTls`], because an enum is as large as its largest variant and a `rustls` connection carries its record buffers inline — around a kilobyte each. Unboxed they set the size of *every* entry in the handle table, so an open file or a plain socket paid a kilobyte for TLS state it does not have.
    ClientTls(Box<StreamOwned<ClientConnection, Socket>>),
    /// A server-side TLS stream: the encrypted conduit an accepted socket became under `start_tls_server`.
    ServerTls(Box<StreamOwned<ServerConnection, Socket>>),
    /// An opaque server TLS configuration minted by `tls_server_config`, held in the table as a handle and consumed by `start_tls_server`.
    TlsConfig(Arc<ServerConfig>),
}

/// The native-OS `Host`: stdio passes straight through, and every other handle — files, plain and TLS sockets, listeners, in-flight DNS lookups, TLS configs — lives in one token-keyed table so the BSD-style lifecycle can transition a handle in place. This is the host the CLI's `run` and a bundled executable execute under; tests reach for `MockHost` instead. Each instance is self-contained: its own table, monotonic clock origin, `args`, and lazily-started resolver pool.
pub struct OsHost {
    /// One [`Table`] for every non-stdio handle, keyed by token bytes. Files, unconnected sockets, connected streams, and listeners share it so the BSD lifecycle can transition a handle in place and `close` releases any kind uniformly.
    table: Mutex<Table<OsResource>>,
    /// Monotonic origin: `clock_mono` reports elapsed time since this.
    start: Instant,
    /// The process arguments served by `args` (argv\[0\] is the program name).
    args: Vec<Vec<u8>>,
    /// The blocking-DNS worker pool, started on the first `lookup` so programs that never resolve a name pay for no threads.
    resolver: OnceLock<OsResolver>,
    /// The termios of every descriptor `raw` switched, keyed by the handle's token, so `raw(h, false)` and [`Drop`] restore exactly what the program found. The first host state with an exit obligation: a terminal left raw outlives the process that switched it.
    termios: Mutex<Vec<(Vec<u8>, Termios)>>,
}

impl OsHost {
    fn new() -> Self {
        Self::with_args(env::args_os().map(|arg| arg.into_encoded_bytes()).collect())
    }

    /// Build a host whose `args` are the given byte strings — used by the CLI to forward a program's own arguments instead of the `curios` process's.
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

    /// Mint a fresh handle for `resource` under the table lock (see [`Table::mint`]).
    fn mint(&self, resource: OsResource) -> Handle {
        self.table.lock().unwrap().mint(resource)
    }

    /// Pull an unconnected socket out of the table by handle, leaving any other resource (or none) in place. Lets `connect`/`listen` transition a handle in place.
    fn take_unconnected(&self, handle: &Handle) -> Option<Socket> {
        self.table
            .lock()
            .unwrap()
            .take_if(handle, |resource| match resource {
                OsResource::Unconnected(socket) => Ok(socket),
                other => Err(other),
            })
    }

    /// Pull a connected stream socket out of the table by handle, leaving any other resource (or none) in place. Lets `start_tls`/`start_tls_server` upgrade a handle without holding the lock across the blocking handshake.
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
    /// The match selects a socket rather than answering, so the failure contract — a setter error is a [`status_from_error`], never a quiet `Ok` — is written once instead of once per kind. A resource added later picks its socket or returns; there is no copy of that contract for it to get wrong.
    fn with_socket<F>(&self, handle: &Handle, apply: F) -> Status
    where
        F: FnOnce(&Socket) -> std::io::Result<()>,
    {
        let table = self.table.lock().unwrap();

        let socket = match handle {
            // The standard streams are the process's, shared with everything else on the terminal or pipe: no socket option applies to them, so, like a file, they record nothing and succeed. They are never in the table, so asking it would answer `NotFound`, the verdict for a closed handle.
            Handle::Stdin | Handle::Stdout | Handle::Stderr => return Status::Ok,
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
                ) => return Status::Ok,
                None => return Status::NotFound,
            },
        };

        match apply(socket) {
            Ok(()) => Status::Ok,
            Err(error) => status_from_error(error),
        }
    }
}

impl Default for OsHost {
    fn default() -> Self {
        Self::new()
    }
}

/// Restore every terminal `raw` switched. `instantiate` drops the host after a trap is classified and before the process exits, so a trap or an `exit` leaves the terminal as the program found it, whether or not the program's own bracket ran.
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
    fn open(&self, path: &[u8], mode: Mode) -> (Status, Handle) {
        let path = String::from_utf8_lossy(path).into_owned();

        let mut options = OpenOptions::new();

        match mode {
            Mode::Read => options.read(true),
            Mode::Write => options.write(true).create(true).truncate(true),
            Mode::Append => options.append(true).create(true),
        };

        match options.open(&path) {
            Ok(file) => (Status::Ok, self.mint(OsResource::File(file))),
            Err(error) => (status_from_error(error), Handle::Other(Vec::new())),
        }
    }

    fn lookup(&self, host: &[u8], port: u32) -> (Status, Handle) {
        let host = String::from_utf8_lossy(host).into_owned();
        let address = format!("{host}:{port}");

        // Start the lookup on the pool (booted on first use). A saturated pool sheds the load as a retriable `WouldBlock`; on success the read end and result slot become a `Resolving` handle the scheduler polls.
        match self
            .resolver
            .get_or_init(OsResolver::default)
            .start(address)
        {
            Ok(Some(pending)) => (
                Status::Ok,
                self.mint(OsResource::Resolving {
                    done: pending.fd,
                    slot: pending.slot,
                }),
            ),
            Ok(None) => (Status::WouldBlock, Handle::Other(Vec::new())),
            Err(status) => (status, Handle::Other(Vec::new())),
        }
    }

    fn resolve(&self, handle: Handle) -> (Status, Vec<Vec<u8>>) {
        // Drain the finished lookup. Reached only after `poll` reports the handle ready, so the slot is filled; a stray early call leaves the handle intact and honestly reports `WouldBlock` so the caller can retry.
        let mut table = self.table.lock().unwrap();

        let ready = match table.get(&handle) {
            Some(OsResource::Resolving { slot, .. }) => slot.get(),
            _ => return (Status::NotFound, vec![]),
        };

        match ready {
            // Drop the handle (closing the pipe read end) only once drained.
            Some(resolved) => {
                table.remove(&handle);
                resolved.into_parts()
            }
            None => (Status::WouldBlock, vec![]),
        }
    }

    fn socket(&self, addr: &[u8]) -> (Status, Handle) {
        // The address blob is the canonical "ip:port" string `resolve` minted.
        let address = match String::from_utf8_lossy(addr).parse::<SocketAddr>() {
            Ok(address) => address,
            Err(_) => return (Status::NotFound, Handle::Other(Vec::new())),
        };

        // Non-blocking from birth: a peer decides when this socket progresses, so `connect`, `read` and `write` on it answer `WouldBlock` rather than wait, and `poll` is where the wait happens. `Socket::new` then `set_nonblocking` is the spelling both release targets share.
        let created = Socket::new(Domain::for_address(address), Type::STREAM, None)
            .and_then(|socket| socket.set_nonblocking(true).map(|()| socket));

        match created {
            Ok(socket) => (Status::Ok, self.mint(OsResource::Unconnected(socket))),
            Err(error) => (status_from_error(error), Handle::Other(Vec::new())),
        }
    }

    fn bind(&self, io: Handle, addr: &[u8]) -> Status {
        // The address blob is the canonical "ip:port" string `resolve` minted.
        let address = match String::from_utf8_lossy(addr).parse::<SocketAddr>() {
            Ok(address) => address,
            Err(_) => return Status::NotFound,
        };

        match self.table.lock().unwrap().get(&io) {
            Some(OsResource::Unconnected(socket)) => match socket.bind(&SockAddr::from(address)) {
                Ok(()) => Status::Ok,
                Err(error) => status_from_error(error),
            },
            _ => Status::NotFound,
        }
    }

    fn connect(&self, io: Handle, addr: &[u8]) -> Status {
        // The address blob is the canonical "ip:port" string `resolve` minted.
        let address = match String::from_utf8_lossy(addr).parse::<SocketAddr>() {
            Ok(address) => address,
            Err(_) => return Status::NotFound,
        };

        let socket = match self.take_unconnected(&io) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        // A non-blocking connect answers at once: `Ok` when the kernel completed it synchronously, as loopback often does, `EINPROGRESS` when it is under way — the socket is re-filed as connecting for `poll` to watch and `finish_connect` to settle — and its refusal otherwise, on which the socket drops. `EINPROGRESS` and `EALREADY` have no `ErrorKind`, so they are matched by errno; an interrupted connect continues asynchronously by POSIX and is filed the same way.
        match socket.connect(&SockAddr::from(address)) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Connected(socket));

                Status::Ok
            }
            Err(error) if is_errno(&error, Errno::ISCONN) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Connected(socket));

                Status::Ok
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

                Status::WouldBlock
            }
            Err(error) => status_from_error(error),
        }
    }

    fn finish_connect(&self, io: Handle) -> Status {
        let mut table = self.table.lock().unwrap();
        let socket = match table.take_if(&io, |resource| match resource {
            OsResource::Connecting(socket) => Ok(socket),
            other => Err(other),
        }) {
            Some(socket) => socket,
            // A connect that completed synchronously was never pending, so settling it is a no-op rather than a fault.
            None => {
                return match table.get(&io) {
                    Some(OsResource::Connected(_)) => Status::Ok,
                    _ => Status::NotFound,
                };
            }
        };

        // `SO_ERROR` is zero both while the connect is pending and after it succeeded, so a clean report is followed by asking for the peer: `ENOTCONN` is the pending answer, and the socket goes back as connecting for another poll. Neither call blocks, so the lock is held across them.
        match socket.take_error() {
            Ok(Some(error)) | Err(error) => status_from_error(error),
            Ok(None) => match socket.peer_addr() {
                Ok(_) => {
                    table.insert(&io, OsResource::Connected(socket));

                    Status::Ok
                }
                Err(error) if is_errno(&error, Errno::NOTCONN) => {
                    table.insert(&io, OsResource::Connecting(socket));

                    Status::WouldBlock
                }
                Err(error) => status_from_error(error),
            },
        }
    }

    fn start_tls(&self, io: Handle, sni: &[u8]) -> Status {
        let server_name = match std::str::from_utf8(sni)
            .ok()
            .and_then(|name| ServerName::try_from(name.to_owned()).ok())
        {
            Some(name) => name,
            None => return Status::TlsError,
        };

        let socket = match self.take_connected(&io) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        let conn = match ClientConnection::new(CLIENT_CONFIG.clone(), server_name) {
            Ok(conn) => conn,
            Err(_) => return Status::TlsError,
        };

        // The stream is filed with its handshake still to run: the socket is non-blocking, so the handshake is driven by the reads and writes that follow — `rustls`'s stream completes prior IO before each — and parks the fiber through `poll` like any other progress. A verification or protocol failure surfaces as `TlsError` from the read or write that discovers it.
        self.table.lock().unwrap().insert(
            &io,
            OsResource::ClientTls(Box::new(StreamOwned::new(conn, socket))),
        );

        Status::Ok
    }

    fn tls_server_config(&self, mut cert: &[u8], mut key: &[u8]) -> (Status, Handle) {
        let certs = match rustls_pemfile::certs(&mut cert).collect::<Result<Vec<_>, _>>() {
            Ok(certs) if !certs.is_empty() => certs,
            _ => return (Status::TlsError, Handle::Other(Vec::new())),
        };

        let key = match rustls_pemfile::private_key(&mut key) {
            Ok(Some(key)) => key,
            _ => return (Status::TlsError, Handle::Other(Vec::new())),
        };

        let config = match ServerConfig::builder_with_provider(Arc::new(ring::default_provider()))
            .with_safe_default_protocol_versions()
            .expect("ring provider supports the default protocol versions")
            .with_no_client_auth()
            .with_single_cert(certs, key)
        {
            Ok(config) => Arc::new(config),
            Err(_) => return (Status::TlsError, Handle::Other(Vec::new())),
        };

        (Status::Ok, self.mint(OsResource::TlsConfig(config)))
    }

    fn start_tls_server(&self, io: Handle, cfg: Handle) -> Status {
        // Clone the config `Arc` out, never holding the lock across the handshake. The config handle stays in the table for reuse.
        let config = match self.table.lock().unwrap().get(&cfg) {
            Some(OsResource::TlsConfig(config)) => config.clone(),
            _ => return Status::NotFound,
        };

        let socket = match self.take_connected(&io) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        let conn = match ServerConnection::new(config) {
            Ok(conn) => conn,
            Err(_) => return Status::TlsError,
        };

        // Filed with the handshake still to run, as `start_tls` files the client side.
        self.table.lock().unwrap().insert(
            &io,
            OsResource::ServerTls(Box::new(StreamOwned::new(conn, socket))),
        );

        Status::Ok
    }

    fn listen(&self, io: Handle, backlog: u32) -> Status {
        let socket = match self.take_unconnected(&io) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        match socket.listen(backlog as i32) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Listener(socket));

                Status::Ok
            }
            Err(error) => status_from_error(error),
        }
    }

    fn accept(&self, io: Handle) -> (Status, Handle) {
        // The listener is non-blocking, so the accept answers at once under the lock: `WouldBlock` with nothing pending, else the stream. `accept4` hands the stream over blocking whatever the listener's flag, so it is switched here, since a fiber will drain it.
        let mut table = self.table.lock().unwrap();
        let accepted = match table.get(&io) {
            Some(OsResource::Listener(socket)) => socket.accept(),
            _ => return (Status::NotFound, Handle::Other(Vec::new())),
        };

        match accepted.and_then(|(stream, _)| stream.set_nonblocking(true).map(|()| stream)) {
            Ok(stream) => (Status::Ok, table.mint(OsResource::Connected(stream))),
            Err(error) => (status_from_error(error), Handle::Other(Vec::new())),
        }
    }

    fn set_reuseaddr(&self, io: Handle, on: u32) -> Status {
        self.with_socket(&io, |socket| socket.set_reuse_address(on != 0))
    }

    fn poll(&self, handles: &[Handle], events: &[Poll], timeout_ms: i32) -> Vec<Poll> {
        let table = self.table.lock().unwrap();

        // Keep the stdio owners alive for the duration of the borrow: each `PollFd` holds a `BorrowedFd` into one of these (or into the table).
        let (in_handle, out_handle, err_handle) = (stdin(), stdout(), stderr());

        // Build a `PollFd` only for resolvable handles, remembering which input slot each maps to so revents land back in parallel; an unknown handle keeps its `empty()` slot and is never polled.
        let mut polls = Vec::with_capacity(handles.len());
        let mut slots = Vec::with_capacity(handles.len());
        let mut results = vec![Poll::empty(); handles.len()];

        for (slot, handle) in handles.iter().enumerate() {
            let requested = events.get(slot).copied().unwrap_or_else(Poll::empty);
            let watched = match handle {
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
                    // The reaper's pipe read end: `READ`-ready once the child has exited, which is when `wait` answers.
                    OsResource::Child { running, .. } => Some((running.done.as_fd(), requested)),
                    // A TLS stream is watched through its socket, for the interest `rustls` itself has while the handshake is under way and the guest's own afterwards; the config token has no descriptor and reports as unrecognized.
                    OsResource::ClientTls(stream) => {
                        Some((stream.sock.as_fd(), tls_interest(&stream.conn, requested)))
                    }
                    OsResource::ServerTls(stream) => {
                        Some((stream.sock.as_fd(), tls_interest(&stream.conn, requested)))
                    }
                    OsResource::TlsConfig(_) => None,
                }),
            };

            if let Some((fd, interest)) = watched {
                polls.push(PollFd::from_borrowed_fd(fd, poll_to_flags(interest)));

                // Where the watched interest is not the guest's own, readiness is reported in the guest's terms: the guest parked for what it asked, and a wake on what `rustls` needed is a wake for it too — reported as the substituted bits alone, the guest would look for its own, see nothing, and re-poll a socket that answers at once, forever.
                let translated = (interest != requested).then_some(requested);
                slots.push((slot, translated));
            }
        }

        // `Int` timeout, poll(2)-style: negative waits forever (no `Timespec`), otherwise a millisecond deadline (`0` returns immediately).
        let timeout = (timeout_ms >= 0).then(|| {
            let ms = i64::from(timeout_ms);

            Timespec {
                tv_sec: ms / 1000,
                tv_nsec: ((ms % 1000) * 1_000_000) as _,
            }
        });

        // A failed poll (e.g. `EINTR`) reports no readiness; the scheduler re-polls. On success, scatter each revents back to its input slot.
        if poll(&mut polls, timeout.as_ref()).is_ok() {
            for (index, &(slot, translated)) in slots.iter().enumerate() {
                let ready = poll_from_flags(polls[index].revents());

                results[slot] = match translated {
                    Some(requested) if ready.bits() != 0 => {
                        Poll::from_bits(ready.bits() | requested.bits())
                    }
                    _ => ready,
                };
            }
        }

        results
    }

    fn close(&self, io: Handle) {
        self.table.lock().unwrap().remove(&io);
    }

    fn read(&self, io: Handle, count: u32) -> (Status, Vec<u8>) {
        let mut buffer = vec![0; count as usize];

        let result = match &io {
            // On the raw descriptor, as the write below is, rather than through `std::io::Stdin`'s buffered reader: a request smaller than what arrived would leave the remainder in a buffer `poll` cannot see, and a fiber waiting on fd 0 would stall with input already inside the process.
            Handle::Stdin => {
                rustix::io::read(stdin(), &mut buffer[..]).map_err(std::io::Error::from)
            }
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

                        return read_outcome(result.map_err(std::io::Error::from), buffer);
                    }
                    // A missing or non-stream handle is a fault, not an exhausted stream — mirror write's `NotFound` so use-after-close stays loud.
                    _ => return (Status::NotFound, vec![]),
                };

                stream.read(&mut buffer)
            }
            // stdout/stderr are not readable.
            _ => return (Status::Eof, vec![]),
        };

        read_outcome(result, buffer)
    }

    fn write(&self, io: Handle, bytes: &[u8]) -> (Status, u32) {
        // The blocking std streams write the whole buffer or fail; report the full length on success so callers see the write completed.
        match io {
            Handle::Stdout => {
                return match stdout().write_all(bytes) {
                    Ok(()) => (Status::Ok, bytes.len() as u32),
                    Err(error) => (status_from_error(error), 0),
                };
            }
            Handle::Stderr => {
                return match stderr().write_all(bytes) {
                    Ok(()) => (Status::Ok, bytes.len() as u32),
                    Err(error) => (status_from_error(error), 0),
                };
            }
            // POSIX semantics: stdin is plain fd 0, so the write succeeds when the process was handed a read-write descriptor (a terminal) and reports `EBADF` when it was opened read-only.
            Handle::Stdin => {
                return match rustix::io::write(stdin(), bytes) {
                    Ok(written) => (Status::Ok, written as u32),
                    Err(errno) => (status_from_error(std::io::Error::from(errno)), 0),
                };
            }
            Handle::Other(_) => {}
        }

        let mut table = self.table.lock().unwrap();

        let stream = match table.get_mut(&io) {
            Some(OsResource::File(file)) => file as &mut dyn Write,
            Some(OsResource::Connected(socket)) => socket,
            // A TLS write completes the pending handshake first and accepts no plaintext until it has, so `WouldBlock` here reports `written` 0 and the caller resends. Once established it buffers the plaintext, reports it all accepted, and flushes as far as the socket allows: the next read or write on the handle pushes the remainder, and a `close` drops what never left — acceptable for a request that is always followed by a read, and the limitation a streaming protocol would meet.
            Some(OsResource::ClientTls(tls)) => {
                return match tls.write(bytes) {
                    Ok(written) => (Status::Ok, written as u32),
                    Err(error) => (tls_status(error), 0),
                };
            }
            Some(OsResource::ServerTls(tls)) => {
                return match tls.write(bytes) {
                    Ok(written) => (Status::Ok, written as u32),
                    Err(error) => (tls_status(error), 0),
                };
            }
            Some(OsResource::Descriptor(fd)) => {
                return match rustix::io::write(&*fd, bytes) {
                    Ok(written) => (Status::Ok, written as u32),
                    Err(errno) => (status_from_error(std::io::Error::from(errno)), 0),
                };
            }
            _ => return (Status::NotFound, 0),
        };

        // A single non-blocking `write`: the kernel takes a prefix and reports its length. We return that count rather than looping (`write_all`), because a loop that hits `WouldBlock` mid-buffer would lose the count of what already went out and the caller would resend it.
        match stream.write(bytes) {
            Ok(written) => (Status::Ok, written as u32),
            Err(error) => (status_from_error(error), 0),
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

    fn raw(&self, io: Handle, on: u32) -> Status {
        let token = io.bytes();

        let outcome = self.with_fd(&io, |fd| {
            let mut records = self.termios.lock().unwrap();
            let recorded = records.iter().position(|(saved, _)| *saved == token);

            match (on != 0, recorded) {
                // The record is taken once, on the first switch, so a second `raw(h, true)` cannot overwrite the settings the program found with raw ones.
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
            None => Status::NotFound,
            Some(Ok(())) => Status::Ok,
            Some(Err(errno)) => status_from_error(std::io::Error::from(errno)),
        }
    }

    fn size(&self, io: Handle) -> (Status, u32, u32) {
        match self.with_fd(&io, |fd| tcgetwinsize(fd)) {
            None => (Status::NotFound, 0, 0),
            Some(Ok(size)) => (Status::Ok, size.ws_col.into(), size.ws_row.into()),
            Some(Err(errno)) => (status_from_error(std::io::Error::from(errno)), 0, 0),
        }
    }

    fn stat(&self, path: &[u8]) -> (Status, u32, u32, u32, u32, u32, u32) {
        let path = OsStr::from_bytes(path);

        let metadata = match fs::metadata(path) {
            Ok(metadata) => metadata,
            // Following the link found nothing. `symlink_metadata` tells a dangling link from a path with nothing at all, and it is the one case the `symlink` kind is reported.
            Err(error) if error.kind() == ErrorKind::NotFound => {
                return match fs::symlink_metadata(path) {
                    Ok(link) if link.file_type().is_symlink() => {
                        (Status::Ok, kind::SYMLINK, 0, 0, 0, 0, 0)
                    }
                    _ => (status_from_error(error), 0, 0, 0, 0, 0, 0),
                };
            }
            Err(error) => return (status_from_error(error), 0, 0, 0, 0, 0, 0),
        };

        let file_type = metadata.file_type();
        let kind = match () {
            () if file_type.is_dir() => kind::DIRECTORY,
            () if file_type.is_file() => kind::FILE,
            () => kind::OTHER,
        };
        let (size_hi, size_lo) = split_billions(metadata.len());
        let (mtime_hi, mtime_lo, mtime_nanos) = metadata
            .modified()
            .ok()
            .and_then(|modified| modified.duration_since(UNIX_EPOCH).ok())
            .map(|since_epoch| {
                let (hi, lo) = split_billions(since_epoch.as_secs());

                (hi, lo, since_epoch.subsec_nanos())
            })
            .unwrap_or((0, 0, 0));

        (
            Status::Ok,
            kind,
            size_hi,
            size_lo,
            mtime_hi,
            mtime_lo,
            mtime_nanos,
        )
    }

    fn remove_file(&self, path: &[u8]) -> Status {
        outcome(fs::remove_file(OsStr::from_bytes(path)))
    }

    fn rename(&self, from: &[u8], to: &[u8]) -> Status {
        outcome(fs::rename(OsStr::from_bytes(from), OsStr::from_bytes(to)))
    }

    fn list(&self, path: &[u8]) -> (Status, Vec<Vec<u8>>) {
        let entries = match fs::read_dir(OsStr::from_bytes(path)) {
            Ok(entries) => entries,
            Err(error) => return (status_from_error(error), vec![]),
        };

        let mut names = Vec::new();

        for entry in entries {
            match entry {
                Ok(entry) => names.push(entry.file_name().into_vec()),
                Err(error) => return (status_from_error(error), vec![]),
            }
        }

        // The directory's own order is whatever the filesystem keeps; sorted, two listings of one directory agree and a test can pin one.
        names.sort();

        (Status::Ok, names)
    }

    fn create_dir(&self, path: &[u8]) -> Status {
        outcome(fs::create_dir(OsStr::from_bytes(path)))
    }

    fn remove_dir(&self, path: &[u8]) -> Status {
        outcome(fs::remove_dir(OsStr::from_bytes(path)))
    }

    fn cwd(&self) -> (Status, Vec<u8>) {
        match env::current_dir() {
            Ok(path) => (Status::Ok, path.into_os_string().into_vec()),
            Err(error) => (status_from_error(error), vec![]),
        }
    }

    fn spawn(
        &self,
        argv: &[Vec<u8>],
        cwd: &[u8],
        env: &[Vec<u8>],
        stdin: u32,
        stdout: u32,
        stderr: u32,
    ) -> (Status, Handle) {
        match os_child::spawn(argv, cwd, env, (stdin, stdout, stderr)) {
            Ok(Spawned {
                child,
                stdin,
                stdout,
                stderr,
            }) => {
                // An unpiped stream is the empty handle a failed `open` returns; a piped one is filed as a `Descriptor` with `O_NONBLOCK` applied, since a fiber drains it and a read that blocked on one pipe while the child filled the other is the deadlock every process library documents. A flag that cannot be set leaves that stream as the empty handle with the child running; `wait` and `kill` still reach it.
                let file = |fd: Option<OwnedFd>| match fd {
                    Some(fd) => match nonblocking(&fd) {
                        Ok(()) => self.mint(OsResource::Descriptor(fd)),
                        Err(_) => Handle::Other(Vec::new()),
                    },
                    None => Handle::Other(Vec::new()),
                };
                let streams = Box::new([file(stdin), file(stdout), file(stderr)]);

                (
                    Status::Ok,
                    self.mint(OsResource::Child {
                        running: child,
                        streams,
                    }),
                )
            }
            Err(error) => (status_from_error(error), Handle::Other(Vec::new())),
        }
    }

    fn stream(&self, child: Handle, which: u32) -> (Status, Handle) {
        match self.table.lock().unwrap().get(&child) {
            Some(OsResource::Child { streams, .. }) => match streams.get(which as usize) {
                Some(handle) => (Status::Ok, handle.clone()),
                None => (Status::NotFound, Handle::Other(Vec::new())),
            },
            _ => (Status::NotFound, Handle::Other(Vec::new())),
        }
    }

    fn wait(&self, child: Handle) -> (Status, u32, u32) {
        // Reached once `poll` reports the child's handle ready, so the slot is filled; an early call leaves the handle intact and reports `WouldBlock`, as `resolve` does.
        let mut table = self.table.lock().unwrap();

        let exit = match table.get(&child) {
            Some(OsResource::Child { running, .. }) => running.exit.get(),
            _ => return (Status::NotFound, 0, 0),
        };

        match exit {
            Some(exit) => {
                table.remove(&child);

                (Status::Ok, exit.code, exit.signal)
            }
            None => (Status::WouldBlock, 0, 0),
        }
    }

    fn kill(&self, child: Handle) -> Status {
        match self.table.lock().unwrap().get(&child) {
            Some(OsResource::Child { running, .. }) => running.kill(),
            _ => Status::NotFound,
        }
    }
}

/// Apply `O_NONBLOCK` to `fd` through `fcntl`, the one flag a pipe end takes.
fn nonblocking(fd: &OwnedFd) -> std::io::Result<()> {
    let flags = fcntl_getfl(fd)?;

    fcntl_setfl(fd, flags | OFlags::NONBLOCK).map_err(std::io::Error::from)
}

/// Whether `error` carries the OS errno `errno` — the read for the connect statuses that have no `ErrorKind`.
fn is_errno(error: &std::io::Error, errno: Errno) -> bool {
    error.raw_os_error() == Some(errno.raw_os_error())
}

/// The status a TLS stream's read or write failure lowers to: `rustls`'s own errors — a failed verification, a protocol violation, a plaintext peer — arrive wrapped in an `InvalidData` error and collapse to `TlsError`, and everything else is the socket's, mapped as every other stream maps it.
fn tls_status(error: std::io::Error) -> Status {
    if error
        .get_ref()
        .is_some_and(|inner| inner.is::<rustls::Error>())
    {
        return Status::TlsError;
    }

    status_from_error(error)
}

/// A TLS read's outcome in the row's `(status, bytes)` shape: a peer that closed without `close_notify` reads as the end of the stream, since a length-framed protocol notices a truncation itself, and the rest as [`tls_status`] maps it.
fn tls_read_outcome(result: std::io::Result<usize>, buffer: Vec<u8>) -> (Status, Vec<u8>) {
    match result {
        Err(error) if error.kind() == ErrorKind::UnexpectedEof => (Status::Eof, vec![]),
        Err(error) => (tls_status(error), vec![]),
        Ok(_) => read_outcome(result, buffer),
    }
}

/// The interest to watch a TLS stream's socket for. While the handshake is under way `rustls`'s own demand replaces the guest's: a socket is nearly always writable, so a fiber that parked on `WRITE` to send its request would spin while `rustls` was in fact waiting to read the server's reply. Afterwards the guest's interest stands, plus `WRITE` whenever `rustls` still holds records to push.
fn tls_interest(conn: &rustls::CommonState, requested: Poll) -> Poll {
    let read = if conn.wants_read() { interest::READ } else { 0 };
    let write = if conn.wants_write() {
        interest::WRITE
    } else {
        0
    };

    match conn.is_handshaking() {
        true => Poll::from_bits(read | write),
        false => Poll::from_bits(requested.bits() | write),
    }
}

/// A status-only row's reply: the failure's status, or `Ok`.
fn outcome(result: std::io::Result<()>) -> Status {
    match result {
        Ok(()) => Status::Ok,
        Err(error) => status_from_error(error),
    }
}

/// A count split base-10⁹ into two limbs that each fit an i31, the way `clock_wall` splits its seconds.
fn split_billions(count: u64) -> (u32, u32) {
    (
        (count / 1_000_000_000) as u32,
        (count % 1_000_000_000) as u32,
    )
}

/// The reply of one `read`: a zero count is end of stream, a positive one the prefix it filled, an error its status. Shared by every descriptor `read` serves, the raw ones included.
fn read_outcome(result: std::io::Result<usize>, mut buffer: Vec<u8>) -> (Status, Vec<u8>) {
    match result {
        Ok(0) => (Status::Eof, vec![]),
        Ok(n) => {
            buffer.truncate(n);

            (Status::Ok, buffer)
        }
        Err(error) => (status_from_error(error), vec![]),
    }
}

#[cfg(test)]
mod tests;
