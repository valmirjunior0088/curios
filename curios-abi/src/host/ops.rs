//! The single authored table of builtin host operations, and what `curios-abi` derives from it.
//!
//! `for_each_host_op!` is the one place a builtin operation is written. A row is the operation's [`HostOp`] variant and its Rust signature — `HandleRead: fn handle_read(h: Handle, n: u64) -> Result<Option<Vec<u8>>, Failure>` — then where the guest surfaces it, `as Handle/read`, then a brace group of what its types cannot say: `yields`, the label a lone payload crosses under (`value` when omitted); `marks`, the failures it may answer beyond the operating system's ([`Mark`]); `requires`, what its operands must hold before the call ([`Requirement`]); and `checks`, what a success answers to relative to its operands ([`Check`]), each naming the operand it reads. The types say the rest. Each operand's type states the wire type it arrives as, the reply's payload the slots a success fills, and the reply's own shape its [`Outcome`] — [`WireOperand`], [`WirePayload`](super::WirePayload) and [`WireReply`] — so the table spells no second description of a type, and a row whose type has no crossing does not compile.
//!
//! The table is an exported X-macro: invoked with the name of a callback macro, it applies that callback to every row. `curios-abi` applies one, generating the [`HostOp`] enum, the roster its accessors read and the [`HostOps`] trait; `curios-runtime` applies its own to bind every import to its method, so the bindings are read off the table too. A callback matches the row grammar and passes each type through untouched — none maps a type to anything — which keeps the table's vocabulary the type system's rather than a macro's.
//!
//! A row whose reply is a [`Termination`] diverges: its call never returns, which is distinct from a row returning nothing, and it crosses no result. `proc_exit` is the one such row. No host implementation can return into the guest, since nothing but the adapter's guest-exit trap is made of a termination, and the guest refuses a host that returns anyway.
//!
//! Each row also states where the guest surfaces it, as `as Subject/label`. The `Subject/label` pair is the `/sys` placement, and it is a column of this table rather than a lookup beside it so a new row cannot acquire a placement nothing checks. The wire name is that pair spelled flat — the subject lowercased, an underscore, the label, so `Handle/read` is `handle_read` — which keeps two rows sharing a label, `file/open` and `serial/open`, from contending for one import name; `a_wire_name_is_its_placement_spelled_flat` holds every row to it. A subject capitalized names a type module the operation joins (`Handle`), a lowercase one a module of operations alone (`socket_open`, `clock`).

use {
    super::{
        Check, ChildExit, ChildStream, Failure, FileStat, ForeignFunction, ForeignStore, Handle,
        Mark, Mode, Outcome, Poll, Refusal, Requirement, SerialFlow, SerialOp, SerialParity,
        StdioMode, Termination, Timestamp, TtySize, WireOperand, WireReply, WireSignature,
    },
    std::sync::LazyLock,
};

/// The one authored table of builtin host operations. Invoked with the name of a callback macro (`for_each_host_op!(my_callback)`), it applies that callback to the whole table, so every projection comes off this single source. Each row is `Variant: fn method(param: Type, …) -> Reply as Subject/label { yields: label, marks: [..], requires: [..], checks: [..] }`, each clause optional: the variant is the row's [`HostOp`], the method name the wasm import name and the [`HostOps`] method; `Subject/label` is where the guest surfaces it under `/sys`; and the brace group holds what the types cannot say.
///
/// Exported so `curios-runtime` can generate its bindings from the rows it binds, and hidden because nothing else should read the table as tokens: every other consumer reads [`HostOp`] and [`HostOps`]. A callback must have in scope every type it expands, since the rows expand where the callback does.
#[doc(hidden)]
#[macro_export]
macro_rules! for_each_host_op {
    ($callback:ident) => {
        $callback! {
            /// Read up to `n` bytes from `h`. `(status, bytes)`: `Ok` with between one and `n` bytes, or with none and no I/O at all when `n` is `0`; `Eof` with none once the stream has ended; or a failure. A handle a peer decides on — a socket, a pipe to a child, standard input — answers `WouldBlock` rather than waiting, and `handle_poll` is where the wait happens; a regular file is read synchronously, since the disk answers it. The native host reads at most 64 KiB in one call, whatever `n` asks. Reading `stdout` or `stderr` fails with `EBADF`, as reading any descriptor opened only to write does.
            HandleRead: fn handle_read(h: Handle, n: u64) -> Result<Option<Vec<u8>>, Failure> as Handle/read { yields: bytes, marks: [Blocks, Tls], checks: [Progress { request: n }] }

            /// Write `b` to `h` in one attempt, returning `(status, written)` — the bytes it accepted, between one and all of them, or `0` for an empty `b`, which checks the handle and writes nothing. The contract is every handle's, the standard streams included: a handle may take only a prefix, and the caller resends the tail. A handle that can accept nothing now answers `WouldBlock`; an interruption before any progress is retried rather than reported; and a descriptor that accepts nothing and reports no error fails with the errno-less `Other(0)`. The host holds nothing of what a write accepted except a TLS stream's records, which `handle_flush` drains. Writing `stdin` fails with `EBADF`.
            HandleWrite: fn handle_write(h: Handle, b: Vec<u8>) -> Result<u64, Failure> as Handle/write { yields: written, marks: [Blocks, Tls], checks: [Accepted { buffer: b }] }

            /// Drain what the host still holds for `h`: a TLS stream's pending records, answering `WouldBlock` until `rustls` holds nothing — `handle_poll` reports the handle writable meanwhile — and `Ok` at once for any other kind, whose accepted writes the host holds nothing of. It promises the host's own buffers are empty, not that a peer has received the bytes or a disk has stored them. `NotFound` for an unknown handle.
            HandleFlush: fn handle_flush(h: Handle) -> Result<(), Failure> as Handle/flush { marks: [Blocks, Tls] }

            /// Open the file at `path` in `mode`. `(status, handle)`; the handle is meaningful only when the status is `Ok`.
            FileOpen: fn file_open(path: Vec<u8>, mode: Mode) -> Result<Handle, Failure> as file/open { yields: handle }

            /// Start an asynchronous lookup of `host`:`port`. `(status, handle)`; on `Ok` the handle becomes `READ`-ready once resolution completes, at which point `dns_resolve` forces the address list off it. The blocking resolution runs off the calling thread. A `host` that is not UTF-8 names nothing a resolver can find and is `NotFound`, and a `port` past 65535 is `EINVAL`, both before any lookup starts.
            DnsLookup: fn dns_lookup(host: Vec<u8>, port: u64) -> Result<Handle, Failure> as dns/lookup { yields: handle, marks: [Blocks] }

            /// Force a finished lookup `handle` to its list of opaque address blobs, consuming it. `(status, addresses)`; non-empty on `Ok`, each blob the host's private encoding the guest only shuttles back into `socket_open`/`socket_bind`/`socket_connect`. `WouldBlock` before readiness.
            DnsResolve: fn dns_resolve(handle: Handle) -> Result<Vec<Vec<u8>>, Failure> as dns/resolve { yields: addresses, marks: [Blocks], checks: [NonEmpty] }

            /// Create an unconnected, non-blocking socket for the address family encoded in `addr`. `(status, handle)` like `file_open`; transitioned by `socket_bind`/`socket_connect`/`socket_listen`.
            SocketOpen: fn socket_open(addr: Vec<u8>) -> Result<Handle, Failure> as socket/open { yields: handle }

            /// Bind socket `h` to the local address `addr`.
            SocketBind: fn socket_bind(h: Handle, addr: Vec<u8>) -> Result<(), Failure> as socket/bind {}

            /// Start connecting socket `h` to the resolved address `addr`. `Ok` when the kernel completed it at once, on which the handle is an ordinary byte stream `handle_read`/`handle_write`/`handle_close` serve; `WouldBlock` while it is under way, on which `handle_poll` reports `h` `WRITE`-ready once it has settled and `socket_finish_connect` reads the outcome; a refusal otherwise, on which the socket drops.
            SocketConnect: fn socket_connect(h: Handle, addr: Vec<u8>) -> Result<(), Failure> as socket/connect { marks: [Blocks] }

            /// Complete a `socket_connect` that answered `WouldBlock`, once `handle_poll` reports `h` `WRITE`-ready. `Ok` re-files `h` as a connected byte stream; a refusal or other failure reports its status and drops the socket; `WouldBlock` while the connect is still pending. `Ok` on a connect that never went pending.
            SocketFinishConnect: fn socket_finish_connect(h: Handle) -> Result<(), Failure> as socket/finish_connect { marks: [Blocks] }

            /// Mark bound socket `h` as listening with accept-queue depth `backlog` (OS-clamped to `somaxconn`). A refused listen leaves the socket unconnected, as it was.
            SocketListen: fn socket_listen(h: Handle, backlog: u64) -> Result<(), Failure> as socket/listen {}

            /// Pull the next connection from listener `h`: `WouldBlock` when none is pending, else `(Ok, handle)`, a non-blocking byte stream like a connected socket.
            SocketAccept: fn socket_accept(h: Handle) -> Result<Handle, Failure> as socket/accept { yields: handle, marks: [Blocks] }

            /// Upgrade connected socket `h` to a TLS client stream in place. `sni` is the server name to present and verify against. The handshake is driven by the reads and writes that follow, each answering `WouldBlock` while it waits on the peer; a failed verification or protocol surfaces as `TlsError` from the read or write that discovers it, with the handle still filed for `handle_close`. An upgrade that cannot start — an invalid server name among its reasons — fails `TlsError` and leaves the socket connected.
            TlsStart: fn tls_start(h: Handle, sni: Vec<u8>) -> Result<(), Failure> as tls/start { marks: [Tls] }

            /// Build an opaque server-side TLS configuration from a PEM certificate chain and private key. `(status, handle)` like `socket_open`: a host-owned config token consumed by `tls_start_server` and released by `handle_close`.
            TlsServerConfig: fn tls_server_config(cert: Vec<u8>, key: Vec<u8>) -> Result<Handle, Failure> as tls/server_config { yields: handle, marks: [Tls] }

            /// Upgrade accepted socket `h` to a TLS server stream in place using configuration handle `cfg`; the handshake is driven by the reads and writes that follow, as `tls_start`'s is. The configuration stays filed for the next connection, and an upgrade that cannot start leaves the socket connected.
            TlsStartServer: fn tls_start_server(h: Handle, cfg: Handle) -> Result<(), Failure> as tls/start_server { marks: [Tls] }

            /// Set socket `h`'s `SO_REUSEADDR` flag; set before `socket_bind`.
            SocketSetReuseaddr: fn socket_set_reuseaddr(h: Handle, on: bool) -> Result<(), Failure> as socket/set_reuseaddr {}

            /// The readiness oracle. Wait until at least one of `handles` is ready for the interest in the parallel `events` mask, or `timeout` milliseconds elapse (`poll(2)` sign convention: negative waits forever, `0` returns immediately). Returns the parallel `revents` masks, one per handle, each within the interest it asked for plus `ERR` and `HUP`; a handle that is unknown — closed, say — has no descriptor, or names a descriptor the system calls invalid reports `ERR`, so its waiter wakes into the call that says why. Each distinct descriptor is polled once, whatever number of handles name it, and an interruption is retried against a deadline taken at the call. A poll the system refuses outright refuses the call, naming the error.
            HandlePoll: fn handle_poll(handles: Vec<Handle>, events: Vec<Poll>, timeout: i64) -> Result<Vec<Poll>, Refusal> as Handle/poll { yields: revents, requires: [SameLength { a: handles, b: events }], checks: [Parallel { list: handles }] }

            /// Close `h`. Closing an unknown handle is a no-op.
            HandleClose: fn handle_close(h: Handle) -> () as Handle/close {}

            /// Read the wall clock. `(secs, nanos)`: seconds since the Unix epoch, and the nanoseconds within the second.
            ClockWall: fn clock_wall() -> Timestamp as clock/wall {}

            /// Read the monotonic clock. `(secs, nanos)` elapsed since a fixed origin; only differences are meaningful.
            ClockMono: fn clock_mono() -> Timestamp as clock/mono {}

            /// Exactly `n` random bytes. A host that cannot find the memory or the entropy for them refuses the call, naming why, rather than answer fewer.
            RandBytes: fn rand_bytes(n: u64) -> Result<Vec<u8>, Refusal> as rand/bytes { yields: bytes, checks: [Exact { request: n }] }

            /// The process arguments, each an opaque byte string.
            ProcArgs: fn proc_args() -> Vec<Vec<u8>> as proc/args { yields: argv }

            /// Look up the environment variable `name`. `(status, value)`: `Ok` with the value, possibly empty, or `NotFound` with empty bytes. A name that is empty or holds `=` or NUL names no variable, and is `NotFound`.
            ProcEnv: fn proc_env(name: Vec<u8>) -> Option<Vec<u8>> as proc/env { yields: value }

            /// End the instance with `code`, the status every host hands its parent whole. The call never returns: the native host carries the code out as its guest-exit trap and the browser as its exit signal, neither ending the embedding process, and a host that returns anyway is refused rather than resumed.
            ProcExit: fn proc_exit(code: u8) -> Termination as proc/exit {}

            /// Put terminal `h` in raw mode (`on`) — the descriptor's termios recorded on first use, then no canonical mode, no echo, no signal keys, no output post-processing, `VMIN` 1, `VTIME` 0 — or restore the record (`off`). The native host also restores every record when it is dropped, so a trap or an `exit` leaves the terminal usable. `ENOTTY` through the errno lane is how a program learns it has no terminal.
            TtyRaw: fn tty_raw(h: Handle, on: bool) -> Result<(), Failure> as tty/raw {}

            /// The terminal's dimensions (`TIOCGWINSZ`). `(status, cols, rows)`; the counts are meaningful only under `Ok`.
            TtySize: fn tty_size(h: Handle) -> Result<TtySize, Failure> as tty/size {}

            /// Open the serial device at `path`: read-write, no controlling terminal and non-blocking, then raw termios with `CLOCAL` and `CREAD`, `baud` as the speed, and the frame `data_bits` (7 or 8), `parity` (a [`serial_parity`](crate::serial_parity) tag), `stop_bits` (1 or 2) and `flow` (a [`serial_flow`](crate::serial_flow) tag). No exclusive hold is taken, so whether another open of the same device is refused is the device's to say. `(status, handle)`: on `Ok` a non-blocking byte stream `handle_read`, `handle_write`, `handle_poll` and `handle_close` serve as they serve a pipe to a child. A setting outside those ranges answers `EINVAL` through the errno lane without opening; a speed the platform cannot set answers what `tcsetattr` reports. Opening asserts DTR on Linux whatever the program wants, so a board that resets on DTR resets on open — a program that cares discards the boot noise afterwards.
            SerialOpen: fn serial_open(path: Vec<u8>, baud: u64, data_bits: u64, parity: SerialParity, stop_bits: u64, flow: SerialFlow) -> Result<Handle, Failure> as serial/open { yields: handle }

            /// Drive serial port `h`: `op` is a [`serial_op`](crate::serial_op) tag — `DTR` or `RTS` set to the level `on`, or `DISCARD_INPUT`, which drops what the device sent and the program has not read (`on` ignored). Break, the four status lines and drain are deliberately absent until a program needs them; drain in particular waits on the wire, which no row does.
            SerialControl: fn serial_control(h: Handle, op: SerialOp, on: bool) -> Result<(), Failure> as serial/control {}

            /// What is at `path`, following symbolic links. `kind` is a [`file_kind`](crate::file_kind) tag, `size` the size in bytes, and `mtime_secs` and `mtime_nanos` the modification time as `clock_wall` reads the clock. A dangling link reports the `SYMLINK` kind with zero sizes; every field but `status` is meaningful only under `Ok`.
            FileStat: fn file_stat(path: Vec<u8>) -> Result<FileStat, Failure> as file/stat {}

            /// Remove the file at `path`. `IsDirectory` on a directory.
            FileRemove: fn file_remove(path: Vec<u8>) -> Result<(), Failure> as file/remove {}

            /// Rename `from` to `to`, file or directory, replacing an existing `to` as `rename(2)` does.
            FileRename: fn file_rename(from: Vec<u8>, to: Vec<u8>) -> Result<(), Failure> as file/rename {}

            /// The names in directory `path`, as the bytes the directory holds — no `.` or `..`, sorted so two listings agree. `NotDirectory` on a file.
            DirList: fn dir_list(path: Vec<u8>) -> Result<Vec<Vec<u8>>, Failure> as dir/list { yields: names }

            /// Create the directory at `path`; its parent must exist. `AlreadyExists` when anything is there.
            DirCreate: fn dir_create(path: Vec<u8>) -> Result<(), Failure> as dir/create {}

            /// Remove the empty directory at `path`. `NotEmpty` when it has entries, `NotDirectory` on a file.
            DirRemove: fn dir_remove(path: Vec<u8>) -> Result<(), Failure> as dir/remove {}

            /// The process's working directory, as bytes. WASI has preopens instead, so the browser denies it.
            ProcCwd: fn proc_cwd() -> Result<Vec<u8>, Failure> as proc/cwd { yields: path }

            /// Start the program `argv[0]` with the arguments after it — `execve`'s own shape — in `cwd` (the parent's when empty) and with `env`'s `NAME=VALUE` entries laid over the inherited environment, each standard stream wired by its [`stdio_mode`](crate::stdio_mode) tag. `(status, child)`: the child handle becomes `READ`-ready when the child exits, which is when `proc_wait` answers, and its piped streams are fetched one at a time through `proc_stream`, because a row carries at most one reference result and it is the last. An empty `argv`, a NUL in any argument, the working directory or an environment entry, or an entry with no name before its `=` fails `EINVAL` without starting anything.
            ProcSpawn: fn proc_spawn(argv: Vec<Vec<u8>>, cwd: Vec<u8>, env: Vec<Vec<u8>>, stdin: StdioMode, stdout: StdioMode, stderr: StdioMode) -> Result<Handle, Failure> as proc/spawn { yields: child }

            /// One of `child`'s piped streams, `which` being the [`stdio`](crate::stdio) index of the stream (`0` stdin, `1` stdout, `2` stderr). `(status, handle)`: a piped stream is a non-blocking handle `handle_read`, `handle_write`, `handle_poll` and `handle_close` serve; a stream that was not piped has none, and is `NotFound`. Closing the child leaves its streams filed.
            ProcStream: fn proc_stream(child: Handle, which: ChildStream) -> Result<Handle, Failure> as proc/stream { yields: handle }

            /// How `child` ended, once its handle is readable: `(status, code, signal)`, `signal` nonzero when a signal ended it and `code` the exit code otherwise. `WouldBlock` while it still runs. An answer consumes the handle, and so does an end the host could not observe, which fails with the errno that kept it from knowing.
            ProcWait: fn proc_wait(child: Handle) -> Result<ChildExit, Failure> as proc/wait { marks: [Blocks] }

            /// Send `child` `SIGKILL` if it still runs; `proc_wait` then reports the signal. A child whose end is already recorded is not signaled and answers `Ok`, since the host may have reaped its pid and the system handed it on.
            ProcKill: fn proc_kill(child: Handle) -> Result<(), Failure> as proc/kill {}
        }
    };
}

/// Project the table to the [`HostOp`] enum, the roster its accessors read, and the typed host interface.
macro_rules! declare_host_rows {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty as $subject:ident / $label:ident {
            $(yields: $yields:ident $(,)?)?
            $(marks: [$($mark:ident),* $(,)?] $(,)?)?
            $(requires: [$($requirement:ident { $($requirement_field:ident: $requirement_operand:ident),* $(,)? }),* $(,)?] $(,)?)?
            $(checks: [$($check:ident $({ $($check_field:ident: $check_operand:ident),* $(,)? })?),* $(,)?] $(,)?)?
        }
    )*) => {
        /// A builtin host operation, one variant per row of the table, named as the row names it: the variant is the wire name in CamelCase, and `a_variant_is_its_wire_name_in_camel_case` holds the two spellings together.
        ///
        /// **A term carries this and nothing else about the row.** Its signature, placement, outcome and description are read back from the table wherever they are needed, so no copy exists that could disagree with it: equality, interning, linking and cache admission compare variants, and a conflicting description of a builtin cannot be written down. A user's `foreign` declaration has no table to point into and carries its own signature instead, as [`ForeignFunction::Declared`]. An archived variant is validated as its discriminant when it is read back, so a stored unit cannot name a row the table does not hold.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[curios_archive::archived]
        pub enum HostOp {
            $($(#[doc = $doc])* $variant,)*
        }

        impl HostOp {
            /// Every builtin, in the table's order.
            pub const ALL: &[HostOp] = &[$(HostOp::$variant),*];
        }

        /// Every builtin row, in the table's order — the declaration order `/sys` binds them in — indexed by [`HostOp`]'s discriminant.
        fn roster() -> &'static [Row] {
            static ROWS: LazyLock<Vec<Row>> = LazyLock::new(|| {
                vec![$({
                    // The label `yields` names, or `value`.
                    let label = [$(stringify!($yields),)? "value"][0];
                    let marks: &[Mark] = &[$($(Mark::$mark),*)?];
                    // The row's own checks, which read its operands by name, then its reply type's.
                    let mut checks = vec![$($(Check::$check $({ $($check_field: stringify!($check_operand)),* })?),*)?];
                    checks.extend(<$r as WireReply>::checks(label));

                    Row {
                        name: stringify!($name),
                        subject: stringify!($subject),
                        label: stringify!($label),
                        signature: WireSignature {
                            params: vec![$((stringify!($p).to_string(), <$t as WireOperand>::WIRE)),*],
                            results: <$r as WireReply>::results(label),
                        },
                        outcome: <$r as WireReply>::OUTCOME,
                        blocks: marks.contains(&Mark::Blocks),
                        tls: marks.contains(&Mark::Tls),
                        requirements: vec![$($(Requirement::$requirement { $($requirement_field: stringify!($requirement_operand)),* }),*)?],
                        checks,
                        // The row's own `///`, which is where a builtin's meaning is already written down.
                        description: concat!($($doc),*).trim(),
                    }
                },)*]
            });

            &ROWS
        }

        /// The host side of the builtin import surface: one method per row, typed as the row is written. Operands arrive owned, so a host may keep a buffer without copying it; a reply is its row's outcome, which the adapter encodes. One shared `Arc<H>` backs every import closure, so methods take `&self` and implementations synchronize internally. Implemented by `OsHost` over real OS resources and by `MockHost` over scripted in-memory ones.
        pub trait HostOps {
            $($(#[doc = $doc])* fn $name(&self, $($p: $t),*) -> $r;)*
        }
    };
}

for_each_host_op!(declare_host_rows);

/// One row as the table states it. Private: a caller names a row by its [`HostOp`] and reads it through that, so the table has no second spelling outside this module.
struct Row {
    name: &'static str,
    subject: &'static str,
    label: &'static str,
    signature: WireSignature,
    outcome: Outcome,
    blocks: bool,
    tls: bool,
    requirements: Vec<Requirement>,
    checks: Vec<Check>,
    description: &'static str,
}

impl HostOp {
    /// The builtin whose wire name is `name`.
    pub fn named(name: &str) -> Option<HostOp> {
        Self::ALL.iter().copied().find(|op| op.name() == name)
    }

    fn row(self) -> &'static Row {
        &roster()[self as usize]
    }

    /// The wasm import name and [`HostOps`] method — the row's placement spelled flat.
    pub fn name(self) -> &'static str {
        self.row().name
    }

    /// The `/sys` module the row surfaces in.
    pub fn subject(self) -> &'static str {
        self.row().subject
    }

    /// The binding the row surfaces as within its subject.
    pub fn label(self) -> &'static str {
        self.row().label
    }

    /// The row's operands and results.
    pub fn signature(self) -> &'static WireSignature {
        &self.row().signature
    }

    /// What the row's reply promises, read off its type.
    pub fn outcome(self) -> Outcome {
        self.row().outcome
    }

    /// Whether the row never returns: its call ends the instance, so it takes the result type it describes as an operand, and nothing resumes after it.
    pub fn diverges(self) -> bool {
        self.outcome() == Outcome::Diverges
    }

    /// Whether the row may answer `would_block`: it is marked [`Mark::Blocks`].
    pub fn blocks(self) -> bool {
        self.row().blocks
    }

    /// Whether the row may answer `tls`: it is marked [`Mark::Tls`].
    pub fn tls(self) -> bool {
        self.row().tls
    }

    /// What the row's operands must hold before the host is asked.
    pub fn requirements(self) -> &'static [Requirement] {
        &self.row().requirements
    }

    /// What a successful reply answers to: the row's own checks, then its payload type's.
    pub fn checks(self) -> &'static [Check] {
        &self.row().checks
    }

    /// What the operation does, in the words the table states it in.
    pub fn description(self) -> &'static str {
        self.row().description
    }
}

/// The builtin store: every host operation the standard library consumes, in prelude (= declaration) order. The method name is the wasm import name; the subject and label are the `/sys` module and binding the guest surfaces it as; parameter names match those declarations; result labels are the record fields the guest projects. The runtime binds its implementations from the same rows, so the two ends cannot drift.
pub fn host_ops() -> ForeignStore {
    let mut store = ForeignStore::new();

    for &op in HostOp::ALL {
        store.register(ForeignFunction::Builtin(op));
    }

    store
}
