//! The single authored table of the host's operations, and what `curios-abi` derives from it.
//!
//! `for_each_host_op!` is the one place a host operation is written. A row is the operation's [`HostOp`] variant and its Rust signature — `HandleRead: fn handle_read(h: Handle, n: u64) -> Result<Option<Vec<u8>>, Failure>` — then a brace group of what its types cannot say: `module`, the `/std` module that declares it, and `yields`, the label a lone value crosses under beside a status, which a row states exactly where its reply has one, since the reply's type takes the label or takes none ([`WireReply::Label`]). The types say the rest, and they are all a call is held to: no row asks of its operands what their types do not state. Each operand's type states the wire type it arrives as, the reply's payload the slots a success fills, and the reply's own shape what crosses beside them — [`WireOperand`], [`WirePayload`](super::WirePayload) and [`WireReply`] — so the table spells no second description of a type, and a row whose type has no crossing does not compile.
//!
//! **Every row shares three clauses beside its own.** A handle that names nothing, or a resource of a kind that cannot do the operation at all, fails `NotFound` and leaves every resource as it was; a stream used in a direction it is not open for — reading `stdout` or `stderr`, writing `stdin`, or a file against its open mode — fails with `EBADF` through the errno lane, on every host. Every operand is copied in and every reply copied out, so no host keeps a reference to guest memory across a call. An operand that is no value of its type is refused by the host's adapter. A reply is lowered from its own type, which is what makes it a value of the row's wire types, and nothing checks it further: what a row's rustdoc promises beyond its types is its host's to keep, and the hosts' own tests hold them to it.
//!
//! The table is an exported X-macro: invoked with the name of a callback macro, it applies that callback to every row. `curios-abi` applies one, generating the [`HostOp`] enum, the roster its accessors read and the [`HostOps`] trait; `curios-runtime` applies its own to bind every import to its method, so the bindings are read off the table too. A callback matches the row grammar and passes each type through untouched — none maps a type to anything — which keeps the table's vocabulary the type system's rather than a macro's.
//!
//! A row whose reply is a [`Termination`] diverges: its call never returns, which is distinct from a row returning nothing, and it crosses no result. `proc_exit` is the one such row. No host implementation can return into the guest, since nothing but the adapter's guest-exit trap is made of a termination, and the guest holds no code for a return from it.
//!
//! **A row is a `foreign` declaration of the standard library.** `module` names the `/std` module that declares it, by the host's own method name, so `handle_read` in `Handle` is `/std/Handle/handle_read` ([`HostOp::declared`]): the name a program imports, and the name a host answers. It is a clause of the row rather than a lookup beside the table, so a new row cannot go without one, and `curios-prelude-archive`'s `the_library_declares_every_row_a_host_answers` holds the library's declarations to the table, by name and by signature, in both directions. No program names a row any other way: what a term carries is the declaration ([`ForeignFunction`](super::ForeignFunction)), as it carries a package's.

use {
    super::{
        ChildExit, ChildHandles, Failure, FileStat, Handle, Mode, Refusal, SerialFlow, SerialOp,
        SerialParity, StdioMode, Termination, Timestamp, TtySize, WireOperand, WireReply,
        WireSignature,
    },
    std::sync::LazyLock,
};

/// The one authored table of the host's operations. Invoked with the name of a callback macro (`for_each_host_op!(my_callback)`), it applies that callback to the whole table, so every projection comes off this single source. Each row is `Variant: fn method(param: Type, …) -> Reply { module: path, yields: label }`, `yields` there where the reply's type takes a label: the variant is the row's [`HostOp`], the method name the [`HostOps`] method and the label `module` declares the row by, and the brace group holds what the types cannot say.
///
/// Exported so `curios-runtime` can generate its bindings from the rows it binds, and hidden because nothing else should read the table as tokens: every other consumer reads [`HostOp`] and [`HostOps`]. A callback must have in scope every type it expands, since the rows expand where the callback does.
#[doc(hidden)]
#[macro_export]
macro_rules! for_each_host_op {
    ($callback:ident) => {
        $callback! {
            /// Read up to `n` bytes from `h`: a success with between one and `n` bytes, or with none and no I/O at all when `n` is `0`; the end of the stream once it has ended; or a failure. A handle a peer decides on — a socket, a pipe to a child, standard input — answers `WouldBlock` rather than waiting, and what reads it waits on a watch of it; a regular file is read synchronously, since the disk answers it. A TLS stream ends only where its peer sent the closure alert: one closed without it fails with `TlsError`, since what arrived cannot be told from a stream cut short. The native host reads at most 64 KiB in one call, whatever `n` asks. Reading `stdout` or `stderr` fails with `EBADF`, as reading any descriptor opened only to write does.
            HandleRead: fn handle_read(h: Handle, n: u64) -> Result<Option<Vec<u8>>, Failure> { module: Handle, yields: bytes }

            /// Write `b` to `h` in one attempt, succeeding with how many bytes it accepted — between one and all of them, or `0` for an empty `b`, which checks the handle and writes nothing. The contract is every handle's, the standard streams included: a handle may take only a prefix, and the caller resends the tail. A handle that can accept nothing now answers `WouldBlock`; an interruption before any progress is retried rather than reported; and a descriptor that accepts nothing and reports no error fails with the errno-less `Other(0)`. The host holds nothing of what a write accepted except a TLS stream's records, which `handle_flush` drains. Writing `stdin` fails with `EBADF`.
            HandleWrite: fn handle_write(h: Handle, b: Vec<u8>) -> Result<u64, Failure> { module: Handle, yields: written }

            /// Drain what the host still holds for `h`: a TLS stream's pending records, answering `WouldBlock` until `rustls` holds nothing — a watch of the handle for writing is ready meanwhile — and `Ok` at once for any other kind, whose accepted writes the host holds nothing of. It promises the host's own buffers are empty, not that a peer has received the bytes or a disk has stored them. `NotFound` for an unknown handle.
            HandleFlush: fn handle_flush(h: Handle) -> Result<(), Failure> { module: Handle }

            /// End `h`'s sending half and leave its receiving half as it is: the peer reads the end of the stream, and `h` still reads what the peer sends. A connected socket shuts down its write side. A TLS stream sends its closure alert first — queued once, answering `WouldBlock` until `rustls` holds none of it, a watch of the handle for writing ready meanwhile — and shuts the socket's write side down after. It is how a stream tells its peer it has finished before `handle_close`, which tells it nothing and resets a connection that holds unread bytes. `NotFound` for an unknown handle and for a kind with no sending half of its own to end: a standard stream, a file, a listener, a pipe to a child, a serial port.
            HandleFinish: fn handle_finish(h: Handle) -> Result<(), Failure> { module: Handle }

            /// Open the file at `path` in `mode`, succeeding with its handle. A regular file or a directory is read and written synchronously, since the disk answers it; anything else — a FIFO, a device — is opened without waiting for a peer and served as a stream is, a read or a write that cannot progress answering `WouldBlock`. A FIFO opened to read reads nothing, rather than its end, until a writer has opened it; one opened to write with no reader fails with `ENXIO` through the errno lane, as the system reports it.
            FileOpen: fn file_open(path: Vec<u8>, mode: Mode) -> Result<Handle, Failure> { module: File, yields: handle }

            /// Start an asynchronous lookup of `host`:`port`, succeeding with a handle that becomes `READ`-ready once resolution completes, at which point `dns_resolve` forces the address list off it. The blocking resolution runs off the calling thread. A `host` that is not UTF-8 names nothing a resolver can find and is `NotFound`, and a `port` past 65535 is `EINVAL`, both before any lookup starts.
            DnsLookup: fn dns_lookup(host: Vec<u8>, port: u64) -> Result<Handle, Failure> { module: tcp/resolve, yields: handle }

            /// Force a finished lookup `handle` to its list of opaque address blobs, consuming it: every address the resolver found, none included, each blob the host's private encoding the guest only shuttles back into `socket_open`/`socket_connect`/`socket_listen`. A lookup that failed is `NotFound`. `WouldBlock` before readiness.
            DnsResolve: fn dns_resolve(handle: Handle) -> Result<Vec<Vec<u8>>, Failure> { module: tcp/resolve, yields: addresses }

            /// Create an unconnected, non-blocking socket for the address family encoded in `addr`, succeeding with its handle; `socket_connect` is what makes it a stream.
            SocketOpen: fn socket_open(addr: Vec<u8>) -> Result<Handle, Failure> { module: tcp/Socket, yields: handle }

            /// Start connecting socket `h` to the resolved address `addr`. `Ok` when the kernel completed it at once, on which the handle is an ordinary byte stream `handle_read`/`handle_write`/`handle_close` serve; `WouldBlock` while it is under way, on which a watch of `h` for writing is ready once it has settled and `socket_finish_connect` reads the outcome; a refusal otherwise, on which the socket drops.
            SocketConnect: fn socket_connect(h: Handle, addr: Vec<u8>) -> Result<(), Failure> { module: tcp/Socket }

            /// Complete a `socket_connect` that answered `WouldBlock`, once a watch of `h` for writing is ready. `Ok` re-files `h` as a connected byte stream; a refusal or other failure reports its status and drops the socket; `WouldBlock` while the connect is still pending. `Ok` on a connect that never went pending.
            SocketFinishConnect: fn socket_finish_connect(h: Handle) -> Result<(), Failure> { module: tcp/Socket }

            /// A listener at the local address `addr`, succeeding with its handle: a non-blocking socket of the address's family with `SO_REUSEADDR` set, so a restart does not wait out the previous socket, bound to `addr` and listening with an accept queue the host chooses. Whichever step fails is the row's failure, and it leaves no socket behind.
            SocketListen: fn socket_listen(addr: Vec<u8>) -> Result<Handle, Failure> { module: tcp/Listener, yields: handle }

            /// Pull the next connection from listener `h`: `WouldBlock` when none is pending, else a success with its handle, a non-blocking byte stream like a connected socket.
            SocketAccept: fn socket_accept(h: Handle) -> Result<Handle, Failure> { module: tcp/Listener, yields: handle }

            /// Upgrade connected socket `h` to a TLS client stream in place. `sni` is the server name to present and verify against. The handshake is driven by the reads and writes that follow, each answering `WouldBlock` while it waits on the peer; a failed verification or protocol surfaces as `TlsError` from the read or write that discovers it, with the handle still filed for `handle_close`. An upgrade that cannot start — an invalid server name among its reasons — fails `TlsError` and leaves the socket connected.
            TlsStart: fn tls_start(h: Handle, sni: Vec<u8>) -> Result<(), Failure> { module: tcp/Socket }

            /// Build an opaque server-side TLS configuration from a PEM certificate chain and private key, succeeding with a host-owned config token consumed by `tls_start_server` and released by `handle_close`.
            TlsServerConfig: fn tls_server_config(cert: Vec<u8>, key: Vec<u8>) -> Result<Handle, Failure> { module: tcp/Listener, yields: handle }

            /// Upgrade accepted socket `h` to a TLS server stream in place using configuration handle `cfg`; the handshake is driven by the reads and writes that follow, as `tls_start`'s is. The configuration stays filed for the next connection, and an upgrade that cannot start leaves the socket connected.
            TlsStartServer: fn tls_start_server(h: Handle, cfg: Handle) -> Result<(), Failure> { module: tcp/Listener }

            /// A handle that is ready once `h` is ready to read where `readable` asks, to write where `writable` asks, or has failed or hung up: what a description suspends on to wait for a stream. A watch of a handle that names nothing a wait can watch — closed, say — is ready at once, so its waiter wakes into the call that says why. Closing the watch leaves `h` as it is.
            HandleWatch: fn handle_watch(h: Handle, readable: bool, writable: bool) -> Handle { module: Handle }

            /// Whether each handle of `on` is ready now, without waiting: one byte for each, in the list's order, [`event::READY`](crate::event::READY) or nothing. A watch is ready as its handle is, a timer once its instant has passed, and any other handle at once, since nothing would hold a wait on it.
            HandleReady: fn handle_ready(on: Vec<Handle>) -> Vec<u8> { module: Handle }

            /// Close `h`. Closing an unknown handle is a no-op.
            HandleClose: fn handle_close(h: Handle) -> () { module: Handle }

            /// Read the wall clock. `(secs, nanos)`: seconds since the Unix epoch, and the nanoseconds within the second.
            ClockWall: fn clock_wall() -> Timestamp { module: time/Timestamp }

            /// Read the monotonic clock. `(secs, nanos)` elapsed since a fixed origin; only differences are meaningful.
            ClockMono: fn clock_mono() -> Timestamp { module: time/Instant }

            /// A handle that is ready once the monotonic clock, as `clock_mono` reads it, reaches `secs` seconds and `nanos` nanoseconds: what a description suspends on to sleep. An instant already passed is ready at once, and nanoseconds past a second carry into the seconds.
            ClockTimer: fn clock_timer(secs: u64, nanos: u64) -> Handle { module: time/Instant }

            /// Exactly `n` random bytes. A host that cannot find the memory or the entropy for them refuses the call, naming why, rather than answer fewer.
            RandBytes: fn rand_bytes(n: u64) -> Result<Vec<u8>, Refusal> { module: rand }

            /// The process arguments, each an opaque byte string. A host that runs a program with no arguments it could call the program's refuses the call, naming why, rather than answer an empty list a program would read as a fact.
            ProcArgs: fn proc_args() -> Result<Vec<Vec<u8>>, Refusal> { module: proc }

            /// Look up the environment variable `name`: its value, possibly empty, or its absence. A name that is empty or holds `=` or NUL names no variable, and is absent.
            ProcEnv: fn proc_env(name: Vec<u8>) -> Option<Vec<u8>> { module: proc, yields: value }

            /// End the instance with `code`, the status every host hands its parent whole. The call never returns: the native host carries the code out as its guest-exit trap and the browser as its exit signal, neither ending the embedding process, and a host that returns anyway is refused rather than resumed.
            ProcExit: fn proc_exit(code: u8) -> Termination { module: proc }

            /// Put terminal `h` in raw mode (`on`) — the descriptor's termios recorded on first use, then no canonical mode, no echo, no signal keys, no output post-processing, `VMIN` 1, `VTIME` 0 — or restore the record (`off`). The native host also restores every record when it is dropped, so a trap or an `exit` leaves the terminal usable. `ENOTTY` through the errno lane is how a program learns it has no terminal.
            TtyRaw: fn tty_raw(h: Handle, on: bool) -> Result<(), Failure> { module: Tty }

            /// The terminal's dimensions (`TIOCGWINSZ`): a success with its `cols` and `rows`.
            TtySize: fn tty_size(h: Handle) -> Result<TtySize, Failure> { module: Tty }

            /// Open the serial device at `path`: read-write, no controlling terminal and non-blocking, then raw termios with `CLOCAL` and `CREAD`, no input mode at all, `baud` as the speed (from 1 through 4294967295), and the frame `data_bits` (7 or 8), `parity` (a [`serial_parity`](crate::serial_parity) tag), `stop_bits` (1 or 2) and `flow` (a [`serial_flow`](crate::serial_flow) tag). With no input mode set, whatever the port's last user left behind, the system sends no flow-control characters of its own, a character that fails its parity check arrives as it was received, and a break arrives as a zero byte. No exclusive hold is taken, so whether another open of the same device is refused is the device's to say. A success is a non-blocking byte stream `handle_read`, `handle_write`, `handle_watch` and `handle_close` serve as they serve a pipe to a child, except that closing it discards what it accepted and has not sent, so the close never waits on the wire; `serial_drain` is the wait. A setting outside those ranges answers `EINVAL` through the errno lane without opening. The frame and the speed are read back once set, and a device that took another answers `EINVAL` too, after the open: on macOS a speed outside the driver's table is set by `IOSSIOSPEED`, and on Linux a speed the device cannot do is one it reports having replaced. Opening asserts DTR whatever the program wants, so a board that resets on DTR resets on open — a program that cares discards the boot noise afterwards.
            SerialOpen: fn serial_open(path: Vec<u8>, baud: u64, data_bits: u64, parity: SerialParity, stop_bits: u64, flow: SerialFlow) -> Result<Handle, Failure> { module: Serial, yields: handle }

            /// Drive serial port `h`: `op` is a [`serial_op`](crate::serial_op) tag — `DTR` or `RTS` set to the level `on`, or `DISCARD_INPUT`, which drops what the device sent and the program has not read (`on` ignored). A handle that is not a serial port — a standard stream, a file, a pipe, a socket — is `NotFound`. Break and the four status lines are deliberately absent until a program needs them.
            SerialControl: fn serial_control(h: Handle, op: SerialOp, on: bool) -> Result<(), Failure> { module: Serial }

            /// Start waiting for serial port `h` to send what it has accepted, off the calling thread, succeeding with a handle to the wait: a stream that holds nothing and reaches its end once the output has left the system, or can no longer leave it. a watch of that handle for reading is ready then, `handle_read` answers the end, and `handle_close` releases it, which abandons the wait without stopping the output. Closing the port discards what is unsent, which ends a wait under way. `NotFound` for a handle that is not a serial port.
            SerialDrain: fn serial_drain(h: Handle) -> Result<Handle, Failure> { module: Serial, yields: handle }

            /// The serial devices the system has, each by a path `serial_open` takes, sorted so two listings agree; none is the empty list. On Linux, every terminal with a device behind it whose port the driver found, by its `/dev/serial/by-id` name where the system keeps one — a name that says what the device is and outlives the order it was plugged in — and by its node otherwise, as an onboard port is; on macOS, the callout devices, `/dev/cu.*`.
            SerialList: fn serial_list() -> Result<Vec<Vec<u8>>, Failure> { module: Serial, yields: paths }

            /// What is at `path`, following symbolic links. `kind` is a [`file_kind`](crate::file_kind) tag, `size` the size in bytes, and `mtime_secs` and `mtime_nanos` the modification time as `clock_wall` reads the clock. A dangling link reports the `SYMLINK` kind with zero sizes.
            FileStat: fn file_stat(path: Vec<u8>) -> Result<FileStat, Failure> { module: fs }

            /// Remove the file at `path`. `IsDirectory` on a directory.
            FileRemove: fn file_remove(path: Vec<u8>) -> Result<(), Failure> { module: fs }

            /// Rename `from` to `to`, file or directory, replacing an existing `to` as `rename(2)` does.
            FileRename: fn file_rename(from: Vec<u8>, to: Vec<u8>) -> Result<(), Failure> { module: fs }

            /// The names in directory `path`, as the bytes the directory holds — no `.` or `..`, sorted so two listings agree. `NotDirectory` on a file.
            DirList: fn dir_list(path: Vec<u8>) -> Result<Vec<Vec<u8>>, Failure> { module: fs, yields: names }

            /// Create the directory at `path`; its parent must exist. `AlreadyExists` when anything is there.
            DirCreate: fn dir_create(path: Vec<u8>) -> Result<(), Failure> { module: fs }

            /// Remove the empty directory at `path`. `NotEmpty` when it has entries, `NotDirectory` on a file.
            DirRemove: fn dir_remove(path: Vec<u8>) -> Result<(), Failure> { module: fs }

            /// The process's working directory, as bytes. WASI has preopens instead, so the browser denies it.
            ProcCwd: fn proc_cwd() -> Result<Vec<u8>, Failure> { module: fs, yields: path }

            /// Start the program `argv[0]` with the arguments after it — `execve`'s own shape — in `cwd` (the parent's when empty) and with `env`'s `NAME=VALUE` entries laid over the inherited environment, each standard stream wired by its [`stdio_mode`](crate::stdio_mode) tag. A success is the child's handle, which becomes `READ`-ready when the child exits, which is when `proc_wait` answers, beside the parent's end of each stream the call piped: a non-blocking handle `handle_read`, `handle_write`, `handle_watch` and `handle_close` serve, and the empty token for a stream that was not piped. Closing the child leaves its streams filed. An empty `argv`, a NUL in any argument, the working directory or an environment entry, or an entry with no name before its `=` fails `EINVAL` without starting anything.
            ProcSpawn: fn proc_spawn(argv: Vec<Vec<u8>>, cwd: Vec<u8>, env: Vec<Vec<u8>>, stdin: StdioMode, stdout: StdioMode, stderr: StdioMode) -> Result<ChildHandles, Failure> { module: Command }

            /// How `child` ended, once its handle is readable: a success with `code` and `signal`, `signal` nonzero when a signal ended it and `code` the exit code otherwise. `WouldBlock` while it still runs. An answer consumes the handle, and so does an end the host could not observe, which fails with the errno that kept it from knowing.
            ProcWait: fn proc_wait(child: Handle) -> Result<ChildExit, Failure> { module: Command/Child }

            /// Send `child` `SIGKILL` if it still runs; `proc_wait` then reports the signal. A child whose end is already recorded is not signaled and answers `Ok`, since the host may have reaped its pid and the system handed it on.
            ProcKill: fn proc_kill(child: Handle) -> Result<(), Failure> { module: Command/Child }
        }
    };
}

/// What a row's `yields` clause hands its reply: the label written, and nothing where the row has no clause. The reply's type takes one or the other ([`WireReply::Label`]).
macro_rules! yields_label {
    () => {
        ()
    };
    ($yields:ident) => {
        stringify!($yields)
    };
}

/// Project the table to the [`HostOp`] enum, the roster its accessors read, and the typed host interface.
macro_rules! declare_host_rows {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty {
            module: $($module:ident)/+ $(,)?
            $(yields: $yields:ident $(,)?)?
        }
    )*) => {
        /// A host operation, one variant per row of the table, named as the row names it: the variant is the method name in CamelCase, and `a_variant_is_its_method_name_in_camel_case` holds the two spellings together.
        ///
        /// **No program carries one.** A program reaches a row through the `foreign` declaration of `/std` that states it, and what its terms carry is that declaration. A variant is how a host's side names a row: what [`HostOps`] implements, what an adapter admits a call's operands against, and what a test scripts a reply for.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum HostOp {
            $($(#[doc = $doc])* $variant,)*
        }

        impl HostOp {
            /// Every row, in the table's order.
            pub const ALL: &[HostOp] = &[$(HostOp::$variant),*];
        }

        /// Every row, in the table's order, indexed by [`HostOp`]'s discriminant.
        fn roster() -> &'static [Row] {
            static ROWS: LazyLock<Vec<Row>> = LazyLock::new(|| {
                vec![$(
                    Row {
                        name: stringify!($name),
                        module: concat!($(stringify!($module), "/"),+),
                        signature: WireSignature {
                            params: vec![$(<$t as WireOperand>::WIRE),*],
                            returns: <$r as WireReply>::shape().returns(yields_label!($($yields)?)),
                        },
                    },
                )*]
            });

            &ROWS
        }

        /// The host's side of its rows: one method per row, typed as the row is written. Operands arrive owned, so a host may keep a buffer without copying it; a reply is its row's outcome, which the adapter encodes. One shared `Arc<H>` backs every import closure, so methods take `&self` and implementations synchronize internally. Implemented by `OsHost` over real OS resources and by `MockHost` over scripted in-memory ones.
        pub trait HostOps {
            $($(#[doc = $doc])* fn $name(&self, $($p: $t),*) -> $r;)*

            /// Wait until a handle of `on` is ready, as `handle_ready` would report it — at once where one already is, and never before: a sleep the system ends early is slept again. No program calls it: a program suspends on `on`, and the host's loop waits here before advancing it, so the rest it advances finds what it waited for. A system that refuses the wait refuses the run, naming why. `on` is never empty, which is a program waiting on nothing the host holds, and the loop's to report.
            fn wait(&self, on: Vec<Handle>) -> Result<(), Refusal>;
        }
    };
}

for_each_host_op!(declare_host_rows);

/// One row as the table states it. Private: a caller names a row by its [`HostOp`] and reads it through that, so the table has no second spelling outside this module.
struct Row {
    name: &'static str,
    /// The `/std` module that declares the row, each segment closed by its slash.
    module: &'static str,
    signature: WireSignature,
}

impl HostOp {
    fn row(self) -> &'static Row {
        &roster()[self as usize]
    }

    /// The [`HostOps`] method that answers the row, which is the label the standard library declares it by.
    pub fn name(self) -> &'static str {
        self.row().name
    }

    /// The name the standard library declares the row by, which is the name a program imports it under: its module, then the host's own method name, so `handle_read` is one spelling in `/std/Handle` and in every host.
    pub fn declared(self) -> String {
        format!("/std/{}{}", self.row().module, self.name())
    }

    /// The row's operands and results, as they cross.
    pub fn signature(self) -> &'static WireSignature {
        &self.row().signature
    }
}
