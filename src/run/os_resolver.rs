use {
    super::host::Status,
    rustix::io::write,
    std::{
        io::pipe,
        net::ToSocketAddrs,
        os::fd::OwnedFd,
        sync::{
            Arc, Mutex,
            mpsc::{SyncSender, sync_channel},
        },
        thread,
    },
};

/// The shared cell a worker fills with a finished lookup's result, drained later
/// by the host's `resolve`. `None` until the worker completes; the
/// `Status`/address-blob pair is exactly what `resolve` hands back.
pub type Slot = Arc<Mutex<Option<(Status, Vec<Vec<u8>>)>>>;

/// One in-flight lookup handed to a worker: the `host:port` string to resolve,
/// the pipe write end whose wakeup byte signals completion, and the slot the
/// result lands in.
struct Job {
    address: String,
    done: OwnedFd,
    slot: Slot,
}

/// A queued lookup's two host-facing handles: `ready` becomes poll-`READ`
/// readable when the worker finishes, and `slot` then holds the result.
pub struct Pending {
    pub ready: OwnedFd,
    pub slot: Slot,
}

/// The blocking-DNS thread pool: a fixed set of workers draining a bounded
/// queue. Dropping it drops the sender, which wakes the idle workers out of
/// `recv` so they exit; an in-flight `getaddrinfo` runs to completion first.
pub struct OsResolver {
    sender: SyncSender<Job>,
}

impl OsResolver {
    /// Worker threads servicing blocking name lookups, and the depth of the
    /// queue feeding them. The pool caps both the threads and — since each
    /// queued job already holds a pipe — the file descriptors a flood of
    /// lookups can tie up: at most `THREADS + QUEUE_DEPTH` lookups are ever in
    /// flight, so the host sheds load (a retriable `WouldBlock`) rather than
    /// spawning a thread and two fds per call. `getaddrinfo` is uncancellable,
    /// so a worker stuck on a dead name stays busy until the system resolver
    /// times out; the bound contains the blast radius to a fixed slice of
    /// capacity.
    const THREADS: usize = 8;
    const QUEUE_DEPTH: usize = 256;

    pub fn new() -> Self {
        let (sender, receiver) = sync_channel::<Job>(Self::QUEUE_DEPTH);
        let receiver = Arc::new(Mutex::new(receiver));

        for _ in 0..Self::THREADS {
            let receiver = Arc::clone(&receiver);

            thread::spawn(move || {
                loop {
                    // Hold the lock only across the brief `recv`; release it
                    // before the long `getaddrinfo` so peers pick up other jobs.
                    let job = match receiver.lock().unwrap().recv() {
                        Ok(job) => job,
                        // The sender is gone (the host was dropped): no more work
                        // will ever arrive, so the worker exits.
                        Err(_) => break,
                    };

                    // The blocking `getaddrinfo`, off the scheduler thread.
                    let result = match job.address.to_socket_addrs() {
                        Ok(addresses) => {
                            // Each blob is the canonical "ip:port" string —
                            // debuggable, and `socket` recovers the family from it.
                            let addresses = addresses
                                .map(|addr| addr.to_string().into_bytes())
                                .collect::<Vec<_>>();

                            if addresses.is_empty() {
                                (Status::NotFound, vec![])
                            } else {
                                (Status::Ok, addresses)
                            }
                        }
                        // Any resolution failure is honestly `NotFound`: the
                        // host:port named nothing.
                        Err(_) => (Status::NotFound, vec![]),
                    };

                    *job.slot.lock().unwrap() = Some(result);

                    // Wake the poller. A closed read end (the guest cancelled and
                    // dropped the handle) makes this `EPIPE`, which is fine — the
                    // result is just discarded. The Rust runtime ignores
                    // `SIGPIPE`, so the write returns an error rather than killing
                    // the process.
                    let _ = write(&job.done, &[1]);
                }
            });
        }

        Self { sender }
    }

    /// Start an asynchronous lookup of `address`. `Ok(Some)` queued the work —
    /// poll the returned `ready` fd and drain `slot` once it fires. `Ok(None)`
    /// means the pool is saturated, so the caller sheds the load. `Err(status)`
    /// means the wakeup pipe could not be created.
    pub fn start(&self, address: String) -> Result<Option<Pending>, Status> {
        // The pipe is the wakeup channel: the read end is polled, the write end
        // moves to the worker. `std::io::pipe` sets `CLOEXEC` on both ends, so a
        // forked child never inherits them.
        let (read_end, write_end) =
            pipe().map_err(|err| Status::Other(err.raw_os_error().unwrap_or(0) as u32))?;

        let (read_end, write_end) = (OwnedFd::from(read_end), OwnedFd::from(write_end));

        let slot = Arc::new(Mutex::new(None));

        match self.sender.try_send(Job {
            address,
            done: write_end,
            slot: Arc::clone(&slot),
        }) {
            Ok(()) => Ok(Some(Pending {
                ready: read_end,
                slot,
            })),
            // Saturated: dropping the job closes the write end and the unsent read
            // end closes with it; report the shed so the caller can retry.
            Err(_) => Ok(None),
        }
    }
}
