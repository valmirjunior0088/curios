use {
    super::{Status, status_from_error},
    std::{
        net::ToSocketAddrs,
        os::fd::OwnedFd,
        sync::{
            Arc, Mutex,
            mpsc::{SyncSender, sync_channel},
        },
        thread,
    },
};

/// A finished lookup the worker hands back through the slot: the resolved address blobs and the status reporting whether the name resolved at all. Built only via [`Resolved::found`] / [`Resolved::not_found`], which keep the invariant that `Ok` carries a non-empty blob list and `NotFound` carries none. Distinct from the host's ABI reply, which also encodes `WouldBlock` (still in flight) — a state a completed `Resolved` never holds.
pub(crate) struct Resolved {
    status: Status,
    addresses: Vec<Vec<u8>>,
}

impl Resolved {
    /// The outcome of a successful `getaddrinfo`: `Ok` with the resolved blobs, or `NotFound` when the name resolved to an empty set.
    fn found(addresses: Vec<Vec<u8>>) -> Self {
        if addresses.is_empty() {
            Self::not_found()
        } else {
            Self {
                status: Status::Ok,
                addresses,
            }
        }
    }

    /// The name resolved to nothing — an empty result or a failed lookup.
    fn not_found() -> Self {
        Self {
            status: Status::NotFound,
            addresses: Vec::new(),
        }
    }

    /// Unpack the finished lookup for the host's ABI reply.
    pub(crate) fn into_parts(self) -> (Status, Vec<Vec<u8>>) {
        (self.status, self.addresses)
    }
}

/// The shared cell a worker fills with a finished lookup's result, drained later by the host's `resolve`. Empty until the worker completes; the [`Resolved`] it then holds is exactly what `resolve` hands back. Cloning shares the one underlying cell — the worker holds one handle, the host the other.
#[derive(Clone)]
pub(crate) struct Slot {
    cell: Arc<Mutex<Option<Resolved>>>,
}

impl Slot {
    /// A fresh, empty slot.
    fn new() -> Self {
        Self {
            cell: Arc::new(Mutex::new(None)),
        }
    }

    /// Worker side: deposit the finished lookup's result. Called exactly once, just before the worker wakes the poller.
    fn set(&self, resolved: Resolved) {
        *self.cell.lock().unwrap() = Some(resolved);
    }

    /// Host side: take the result if the worker has filled it, else `None` (the lookup is still in flight, so the caller retries). Drains the cell, so a second `get` before another `set` yields `None`.
    pub(crate) fn get(&self) -> Option<Resolved> {
        self.cell.lock().unwrap().take()
    }
}

/// The worker's half of a lookup's wakeup pipe: the write end whose single byte makes the host's polled read end fire. The matching read end travels to the host as [`Pending::fd`].
struct Signal {
    fd: OwnedFd,
}

impl Signal {
    /// Open a wakeup pipe, returning the host's read end and the worker's `Signal`. `std::io::pipe` sets `CLOEXEC` on both ends, so a forked child never inherits them.
    fn pipe() -> std::io::Result<(OwnedFd, Self)> {
        let (read_end, write_end) = std::io::pipe()?;

        Ok((
            OwnedFd::from(read_end),
            Self {
                fd: OwnedFd::from(write_end),
            },
        ))
    }

    /// Wake the host's poller. The byte's *value* is irrelevant: the read end is only ever polled for readability and dropped, never `read`, so any single byte serves as the edge — `&[1]` is just a conventional one-event marker. A closed read end (the guest cancelled and dropped the handle) makes this `EPIPE`, which is fine — the result is just discarded. The Rust runtime ignores `SIGPIPE`, so the write returns an error rather than killing the process.
    fn signal(&self) {
        let _ = rustix::io::write(&self.fd, &[1]);
    }
}

/// One in-flight lookup handed to a worker: the `host:port` string to resolve, the wakeup `signal` fired on completion, and the slot the result lands in.
struct Job {
    address: String,
    signal: Signal,
    slot: Slot,
}

impl Job {
    /// Mint a job and its host-facing [`Pending`] as a pair. They share one result `slot` and the two ends of one wakeup pipe — the worker keeps `signal`, the host keeps `ready`. The job is queued to a worker; the `Pending` stays with the caller to poll and drain.
    fn new(address: String) -> std::io::Result<(Self, Pending)> {
        let (fd, signal) = Signal::pipe()?;
        let slot = Slot::new();

        Ok((
            Self {
                address,
                signal,
                slot: slot.clone(),
            },
            Pending::new(fd, slot),
        ))
    }

    /// Deliver a finished lookup and retire the job: fill the result slot, then wake the host's poller. The order is load-bearing — the host drains the slot the moment the signal makes its `fd` fire, so the slot must be set first. Consumes the job; the worker is done with it once delivered.
    fn complete(self, resolved: Resolved) {
        self.slot.set(resolved);
        self.signal.signal();
    }
}

/// A queued lookup's two host-facing handles: `fd` becomes poll-`READ` readable when the worker finishes, and `slot` then holds the result.
pub(crate) struct Pending {
    pub(crate) fd: OwnedFd,
    pub(crate) slot: Slot,
}

impl Pending {
    /// Bundle the host's two handles for a queued lookup. Minted by [`Job::new`], which owns the shared `slot` and the pipe whose read end is `fd`.
    fn new(fd: OwnedFd, slot: Slot) -> Self {
        Self { fd, slot }
    }
}

/// The blocking-DNS thread pool: a fixed set of workers draining a bounded queue. Dropping it drops the sender, which wakes the idle workers out of `recv` so they exit; an in-flight `getaddrinfo` runs to completion first.
pub(crate) struct OsResolver {
    sender: SyncSender<Job>,
}

impl OsResolver {
    /// Build a pool of `threads` workers draining a queue that holds at most `length` pending lookups. The pool caps both the threads and — since each queued job already holds a pipe — the file descriptors a flood of lookups can tie up: at most `threads + length` lookups are ever in flight, so [`start`](Self::start) sheds load (a retriable `WouldBlock`) rather than spawning a thread and two fds per call. `getaddrinfo` is uncancellable, so a worker stuck on a dead name stays busy until the system resolver times out; the bound contains the blast radius to a fixed slice of capacity.
    fn new(threads: usize, length: usize) -> Self {
        let (sender, receiver) = sync_channel::<Job>(length);
        let receiver = Arc::new(Mutex::new(receiver));

        for _ in 0..threads {
            let receiver = Arc::clone(&receiver);

            thread::spawn(move || {
                loop {
                    // Hold the lock only across the brief `recv`; release it before the long `getaddrinfo` so peers pick up other jobs.
                    let job = match receiver.lock().unwrap().recv() {
                        Ok(job) => job,
                        // The sender is gone (the host was dropped): no more work will ever arrive, so the worker exits.
                        Err(_) => break,
                    };

                    // The blocking `getaddrinfo`, off the scheduler thread. Each blob is the canonical "ip:port" string — debuggable, and `socket` recovers the family from it.
                    let resolved = match job.address.to_socket_addrs() {
                        Ok(addresses) => Resolved::found(
                            addresses
                                .map(|addr| addr.to_string().into_bytes())
                                .collect(),
                        ),
                        // Any resolution failure is honestly `NotFound`: the host:port named nothing.
                        Err(_) => Resolved::not_found(),
                    };

                    job.complete(resolved);
                }
            });
        }

        Self { sender }
    }

    /// Start an asynchronous lookup of `address`. `Ok(Some)` queued the work — poll the returned `ready` fd and drain `slot` once it fires. `Ok(None)` means the pool is saturated, so the caller sheds the load. `Err(status)` means the wakeup pipe could not be created.
    pub(crate) fn start(&self, address: String) -> Result<Option<Pending>, Status> {
        let (job, pending) = Job::new(address).map_err(status_from_error)?;

        match self.sender.try_send(job) {
            Ok(()) => Ok(Some(pending)),
            // Saturated: dropping the job closes the write end and the unsent read end (in the dropped `pending`) closes with it; report the shed so the caller can retry.
            Err(_) => Ok(None),
        }
    }
}

impl Default for OsResolver {
    /// The standard pool: 8 workers over a queue of 256 pending lookups.
    fn default() -> Self {
        Self::new(8, 256)
    }
}
