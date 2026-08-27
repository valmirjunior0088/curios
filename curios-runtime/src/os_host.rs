use {
    super::{OsResolver, Slot, Table, host::*},
    rustix::event::{PollFd, Timespec, poll},
    rustls::{
        ClientConfig, ClientConnection, RootCertStore, ServerConfig, ServerConnection, StreamOwned,
        crypto::ring, pki_types::ServerName,
    },
    socket2::{Domain, SockAddr, Socket, Type},
    std::{
        env,
        fs::{File, OpenOptions},
        io::{Read, Write, stderr, stdin, stdout},
        net::SocketAddr,
        os::fd::{AsFd, OwnedFd},
        sync::{Arc, LazyLock, Mutex, OnceLock},
        time::{Duration, Instant, SystemTime, UNIX_EPOCH},
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

/// A non-stdio handle in [`OsHost`]'s unified table, tracking the BSD lifecycle with one concrete type per state: `open` files a `File`; `socket` mints an `Unconnected` socket, `connect` turns it into a `Connected` one (as does `accept`), and `listen` turns it into a `Listener`. `start_tls` / `start_tls_server` upgrade a `Connected` socket in place to a `ClientTls` / `ServerTls` stream; `tls_server_config` files a host-owned `TlsConfig` token. `read`/`write` serve `File`, `Connected`, and both TLS streams alike (all are `Read + Write`); `close` drops any kind, releasing its descriptor.
enum OsResource {
    File(File),
    /// An in-flight asynchronous name lookup minted by `lookup`. `done` is the read end of a pipe a worker thread writes one byte to once it has filled `slot` with the `getaddrinfo` result; that write makes `done` poll-`READ` readable, waking the scheduler. `resolve` then drains `slot` and drops the handle (closing `done`). `poll` watches `done` like any other fd.
    Resolving {
        done: OwnedFd,
        slot: Slot,
    },
    Connected(Socket),
    Unconnected(Socket),
    /// A listening socket, held behind an `Arc` so `accept` can share it out and drop the table lock across the blocking wait with a cheap refcount bump instead of a per-call `dup`/`close`.
    Listener(Arc<Socket>),
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
    /// The process arguments served by `args` (argv[0] is the program name).
    args: Vec<Vec<u8>>,
    /// The blocking-DNS worker pool, started on the first `lookup` so programs that never resolve a name pay for no threads.
    resolver: OnceLock<OsResolver>,
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
        }
    }

    /// Mint a fresh handle for `resource` under the table lock (see [`Table::mint`]).
    fn mint(&self, resource: OsResource) -> Handle {
        self.table.lock().unwrap().mint(resource)
    }

    /// Pull an unconnected socket out of the table by handle, leaving any other resource (or none) in place. Lets `connect`/`listen` transition a handle without holding the lock across the blocking syscall.
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

    /// Apply a `socket2` setter to a configurable handle. Every socket kind — unconnected, connected, or listening — exposes its typed setters directly; a `File` has no socket options, so that path records nothing and succeeds.
    ///
    /// The match selects a socket rather than answering, so the failure contract — a setter error is a [`status_from_error`], never a quiet `Ok` — is written once instead of once per kind. A resource added later picks its socket or returns; there is no copy of that contract for it to get wrong.
    fn with_socket<F>(&self, handle: &Handle, apply: F) -> Status
    where
        F: FnOnce(&Socket) -> std::io::Result<()>,
    {
        let table = self.table.lock().unwrap();

        let socket = match table.get(handle) {
            Some(OsResource::Unconnected(socket) | OsResource::Connected(socket)) => socket,
            // The listener is `Arc`-held, and an arm's value position is not a coercion site the way the old `apply(socket)` call argument was.
            Some(OsResource::Listener(socket)) => socket.as_ref(),
            // A TLS stream forwards setters to its underlying socket, so a timeout set after the upgrade still takes effect.
            Some(OsResource::ClientTls(stream)) => &stream.sock,
            Some(OsResource::ServerTls(stream)) => &stream.sock,
            // A file, a config token, and an in-flight lookup have no socket options: record nothing.
            Some(OsResource::File(_) | OsResource::TlsConfig(_) | OsResource::Resolving { .. }) => {
                return Status::Ok;
            }
            None => return Status::NotFound,
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

        match Socket::new(Domain::for_address(address), Type::STREAM, None) {
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

        // Take the socket out so the blocking connect runs without the table lock held; re-file the connected socket as a byte stream on success.
        let socket = match self.take_unconnected(&io) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        match socket.connect(&SockAddr::from(address)) {
            Ok(()) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::Connected(socket));

                Status::Ok
            }
            Err(error) => status_from_error(error),
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

        // Take the connected socket out so the blocking handshake runs without the table lock held. A failed upgrade drops the (now unusable) connection rather than re-filing it as cleartext.
        let socket = match self.take_connected(&io) {
            Some(socket) => socket,
            None => return Status::NotFound,
        };

        let conn = match ClientConnection::new(CLIENT_CONFIG.clone(), server_name) {
            Ok(conn) => conn,
            Err(_) => return Status::TlsError,
        };

        let mut stream = StreamOwned::new(conn, socket);

        // Drive the handshake to completion inline so a verification or protocol failure surfaces here, at the upgrade, not on a later read.
        match stream.conn.complete_io(&mut stream.sock) {
            Ok(_) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::ClientTls(Box::new(stream)));

                Status::Ok
            }
            Err(_) => Status::TlsError,
        }
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

        let mut stream = StreamOwned::new(conn, socket);

        match stream.conn.complete_io(&mut stream.sock) {
            Ok(_) => {
                self.table
                    .lock()
                    .unwrap()
                    .insert(&io, OsResource::ServerTls(Box::new(stream)));

                Status::Ok
            }
            Err(_) => Status::TlsError,
        }
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
                    .insert(&io, OsResource::Listener(Arc::new(socket)));

                Status::Ok
            }
            Err(error) => status_from_error(error),
        }
    }

    fn accept(&self, io: Handle) -> (Status, Handle) {
        // `accept` blocks until a connection arrives, so share the `Arc`-held listener out and drop the table lock before the wait — never hold it across one. The `Arc` clone is a refcount bump, not a `dup` syscall.
        let listener = match self.table.lock().unwrap().get(&io) {
            Some(OsResource::Listener(socket)) => Arc::clone(socket),
            _ => return (Status::NotFound, Handle::Other(Vec::new())),
        };

        match listener.accept() {
            Ok((stream, _)) => (Status::Ok, self.mint(OsResource::Connected(stream))),
            Err(error) => (status_from_error(error), Handle::Other(Vec::new())),
        }
    }

    fn set_nonblocking(&self, io: Handle, on: u32) -> Status {
        self.with_socket(&io, |socket| socket.set_nonblocking(on != 0))
    }

    fn set_recv_timeout(&self, io: Handle, ms: u32) -> Status {
        // `0` ms clears the timeout (`None`); any other value is a duration.
        self.with_socket(&io, |socket| {
            socket.set_read_timeout((ms != 0).then(|| Duration::from_millis(ms.into())))
        })
    }

    fn set_send_timeout(&self, io: Handle, ms: u32) -> Status {
        // `0` ms clears the timeout (`None`); any other value is a duration.
        self.with_socket(&io, |socket| {
            socket.set_write_timeout((ms != 0).then(|| Duration::from_millis(ms.into())))
        })
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
            let fd = match handle {
                Handle::Stdin => Some(in_handle.as_fd()),
                Handle::Stdout => Some(out_handle.as_fd()),
                Handle::Stderr => Some(err_handle.as_fd()),
                Handle::Other(_) => table.get(handle).and_then(|resource| match resource {
                    OsResource::File(file) => Some(file.as_fd()),
                    OsResource::Connected(socket) | OsResource::Unconnected(socket) => {
                        Some(socket.as_fd())
                    }
                    // `&Arc<Socket>` deref-coerces for the `as_fd` method call.
                    OsResource::Listener(socket) => Some(socket.as_fd()),
                    // The lookup's pipe read end: `READ`-ready once the worker has written its wakeup byte, which is the completion signal.
                    OsResource::Resolving { done, .. } => Some(done.as_fd()),
                    // TLS record readiness is not socket readiness (rustls buffers records, and an app read can require a socket write), so a TLS stream is not polled at the socket layer under the sync model; the config token has no fd. Both report as unrecognized — an `empty()` revents slot.
                    OsResource::ClientTls(_)
                    | OsResource::ServerTls(_)
                    | OsResource::TlsConfig(_) => None,
                }),
            };

            if let Some(fd) = fd {
                polls.push(PollFd::from_borrowed_fd(
                    fd,
                    poll_to_flags(events.get(slot).copied().unwrap_or_else(Poll::empty)),
                ));

                slots.push(slot);
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
            for (index, &slot) in slots.iter().enumerate() {
                results[slot] = poll_from_flags(polls[index].revents());
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
            Handle::Stdin => stdin().lock().read(&mut buffer),
            Handle::Other(_) => {
                let mut table = self.table.lock().unwrap();
                let stream: &mut dyn Read = match table.get_mut(&io) {
                    Some(OsResource::File(file)) => file,
                    Some(OsResource::Connected(socket)) => socket,
                    Some(OsResource::ClientTls(tls)) => &mut **tls,
                    Some(OsResource::ServerTls(tls)) => &mut **tls,
                    // A missing or non-stream handle is a fault, not an exhausted stream — mirror write's `NotFound` so use-after-close stays loud.
                    _ => return (Status::NotFound, vec![]),
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
            Err(error) => (status_from_error(error), vec![]),
        }
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
            Some(OsResource::ClientTls(tls)) => &mut **tls,
            Some(OsResource::ServerTls(tls)) => &mut **tls,
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
}

#[cfg(test)]
mod tests;
