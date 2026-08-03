//! The single authored list of builtin host operations, and the two projections `curios-abi` derives from it.
//!
//! [`host_ops!`] is the one place a builtin operation is written. It is an X-macro: invoked with the name of a callback macro, it expands to that callback applied to the whole table, so each projection is generated from this single source and none can drift. `curios-abi` projects two — the [`host_ops`] wire store and the typed [`HostOps`] trait — and the native adapter (`curios-runtime`) projects a third, its codec bindings, off the same list.
//!
//! Each operand and result is one of a closed vocabulary of slot kinds (`Handle`, `Nat`, `Bool`, `Int`, `Bytes`, `Mode`, `Status`, `ListBytes`, `ListHandle`, `ListPoll`), each a fixed `(wire type, trait parameter, trait result)` triple the `*_of!` helpers below encode. Result arity fixes the guest-facing shape exactly as the prelude's `host_fn` reads it: `0` results is the unit value, `1` the bare result, `2..` a record of the named fields. If an operation ever needs an eleventh slot kind, reconsider the vocabulary before extending it. `exit` is deliberately absent — it traps rather than returns, its guest type is the polymorphic bottom `(@A : Type) -> Nat -> A`, and it is wired directly as a trap, so it stays a hardcoded primitive outside both the store and the trait.

use super::{
    ForeignFunction, ForeignStore, Handle, Mode, Poll, Status, WireLeaf, WireSignature, WireType,
};

/// The one authored list of builtin host operations. Invoked with the name of a callback macro (`host_ops!(my_callback)`), it applies that callback to the whole table so every projection comes off this single source. Each row is `method [param: Slot, …] [result: Slot, …];` — the method name doubles as the wasm import name and the guest binding label, and each `Slot` is one of the closed vocabulary the `*_of!` helpers map to concrete types.
///
/// This macro is private to `curios-abi`: the two projections it drives — [`host_ops`] and [`HostOps`] — are the only interfaces the rest of the system uses. The native adapter's codec bindings are hand-written against that pair (they marshal wasmtime values, which cannot live in this leaf), and cross-checked against it — a `define` name must be a real store row and each call must match the trait.
macro_rules! host_ops {
    ($callback:ident) => {
        $callback! {
            /// Read up to `n` bytes from `h`, blocking until at least one is available. `(status, bytes)`: `Ok` with 1..n bytes, `Eof` with none, or an error status.
            read [h: Handle, n: Nat] [status: Status, bytes: Bytes];

            /// Write `b` to `h`, returning `(status, written)` — the bytes accepted this call. A non-blocking handle may take only a prefix (so the caller resends the tail without duplicating); `WouldBlock` reports `written` 0, the blocking std streams the full length.
            write [h: Handle, b: Bytes] [status: Status, written: Nat];

            /// Open the file at `path` in `mode`. `(status, handle)`; the handle is meaningful only when the status is `Ok`.
            open [path: Bytes, mode: Mode] [status: Status, handle: Handle];

            /// Start an asynchronous lookup of `host`:`port`. `(status, handle)`; on `Ok` the handle becomes `READ`-ready once resolution completes, at which point `resolve` forces the address list off it. The blocking resolution runs off the calling thread.
            lookup [host: Bytes, port: Nat] [status: Status, handle: Handle];

            /// Force a finished lookup `handle` to its list of opaque address blobs, consuming it. `(status, addresses)`; non-empty on `Ok`, each blob the host's private encoding the guest only shuttles back into `socket`/`bind`/`connect`. `WouldBlock` before readiness.
            resolve [handle: Handle] [status: Status, addresses: ListBytes];

            /// Create an unconnected socket for the address family encoded in `addr`. `(status, handle)` like `open`; configured via the setters, then transitioned by `bind`/`connect`/`listen`.
            socket [addr: Bytes] [status: Status, handle: Handle];

            /// Bind socket `h` to the local address `addr`.
            bind [h: Handle, addr: Bytes] [status: Status];

            /// Connect socket `h` to the resolved address `addr`. On `Ok` the handle is an ordinary byte stream `read`/`write`/`close` serve.
            connect [h: Handle, addr: Bytes] [status: Status];

            /// Mark bound socket `h` as listening with accept-queue depth `backlog` (OS-clamped to `somaxconn`).
            listen [h: Handle, backlog: Nat] [status: Status];

            /// Pull the next connection from listener `h`, blocking until one arrives. `(status, handle)`; an ordinary byte stream like a connected socket.
            accept [h: Handle] [status: Status, handle: Handle];

            /// Upgrade connected socket `h` to a TLS client stream in place, running the handshake inline. `sni` is the server name to present and verify against; on failure the connection drops with `TlsError`.
            start_tls [h: Handle, sni: Bytes] [status: Status];

            /// Build an opaque server-side TLS configuration from a PEM certificate chain and private key. `(status, handle)` like `socket`: a host-owned config token consumed by `start_tls_server` and released by `close`.
            tls_server_config [cert: Bytes, key: Bytes] [status: Status, handle: Handle];

            /// Upgrade accepted socket `h` to a TLS server stream in place using configuration handle `cfg`, running the handshake inline.
            start_tls_server [h: Handle, cfg: Handle] [status: Status];

            /// Set socket `h`'s non-blocking flag. A no-op on a file handle.
            set_nonblocking [h: Handle, on: Bool] [status: Status];

            /// Set socket `h`'s receive timeout to `ms` milliseconds (`0` clears).
            set_recv_timeout [h: Handle, ms: Nat] [status: Status];

            /// Set socket `h`'s send timeout to `ms` milliseconds (`0` clears).
            set_send_timeout [h: Handle, ms: Nat] [status: Status];

            /// Set socket `h`'s `SO_REUSEADDR` flag; set before `bind`.
            set_reuseaddr [h: Handle, on: Bool] [status: Status];

            /// The readiness oracle. Wait until at least one of `handles` is ready for the interest in the parallel `events` mask, or `timeout` milliseconds elapse (`poll(2)` sign convention: negative waits forever, `0` returns immediately). Returns the parallel `revents` masks, one per handle.
            poll [handles: ListHandle, events: ListPoll, timeout: Int] [revents: ListPoll];

            /// Close `h`. Closing an unknown handle is a no-op.
            close [h: Handle] [];

            /// Read the wall clock. `(secs_hi, secs_lo, nanos)`: seconds since the Unix epoch split base-10⁹ so each limb fits an i31, plus sub-second nanoseconds.
            clock_wall [] [secs_hi: Nat, secs_lo: Nat, nanos: Nat];

            /// Read the monotonic clock. `(secs, nanos)` elapsed since a fixed origin; only differences are meaningful.
            clock_mono [] [secs: Nat, nanos: Nat];

            /// Return `n` random bytes.
            random [n: Nat] [bytes: Bytes];

            /// The process arguments, each an opaque byte string.
            args [] [argv: ListBytes];

            /// Look up the environment variable `name`. `(status, value)`: `Ok` with the value, or `NotFound` with empty bytes.
            env [name: Bytes] [status: Status, value: Bytes];
        }
    };
}

/// One slot kind → the [`WireType`] it crosses the boundary as.
macro_rules! wire_of {
    (Handle) => {
        WireType::Handle
    };
    (Nat) => {
        WireType::Nat
    };
    (Bool) => {
        WireType::Bool
    };
    (Int) => {
        WireType::Int
    };
    (Bytes) => {
        WireType::Bytes
    };
    (Mode) => {
        WireType::Nat
    };
    (Status) => {
        WireType::Nat
    };
    (ListBytes) => {
        WireType::Lst(WireLeaf::Bytes)
    };
    (ListHandle) => {
        WireType::Lst(WireLeaf::Handle)
    };
    (ListPoll) => {
        WireType::Lst(WireLeaf::Nat)
    };
}

/// One slot kind → the Rust type it takes as a [`HostOps`] method parameter. The list/bytes kinds borrow; the scalars and handle are owned.
macro_rules! trait_param_of {
    (Handle) => {
        Handle
    };
    (Nat) => {
        u32
    };
    (Bool) => {
        u32
    };
    (Int) => {
        i32
    };
    (Bytes) => {
        &[u8]
    };
    (Mode) => {
        Mode
    };
    (ListHandle) => {
        &[Handle]
    };
    (ListPoll) => {
        &[Poll]
    };
}

/// One slot kind → the Rust type it produces as a [`HostOps`] method result.
macro_rules! trait_result_of {
    (Handle) => { Handle };
    (Nat) => { u32 };
    (Bytes) => { Vec<u8> };
    (Status) => { Status };
    (ListBytes) => { Vec<Vec<u8>> };
    (ListPoll) => { Vec<Poll> };
}

/// A method's result slots → its Rust return type, following the same arity rule the guest shape does: no results is the unit `()`, one is the bare type, two or more is a tuple.
macro_rules! result_ty {
    () => { () };
    ($single:ident) => { trait_result_of!($single) };
    ($($many:ident),+ $(,)?) => { ($(trait_result_of!($many)),+) };
}

/// Project the op list to the typed host interface.
macro_rules! declare_host_trait {
    ($($(#[$attr:meta])* $method:ident [$($p:ident : $ps:ident),* $(,)?] [$($r:ident : $rs:ident),* $(,)?];)*) => {
        /// The host side of the builtin import surface: one method per [`host_ops`] store row, generated from the [`host_ops!`] list so the store and this trait cannot drift. Handles cross as [`Handle`], failures as [`Status`]; one shared `Arc<H>` backs every import closure, so methods take `&self` and implementations synchronize internally. Implemented by `OsHost` over real OS resources and by `MockHost` over scripted in-memory ones.
        pub trait HostOps {
            $(
                $(#[$attr])*
                fn $method(&self $(, $p: trait_param_of!($ps))*) -> result_ty!($($rs),*);
            )*
        }
    };
}

/// Project the op list to the wire store the compiler and runtime derive from.
macro_rules! declare_host_store {
    ($($(#[$attr:meta])* $method:ident [$($p:ident : $ps:ident),* $(,)?] [$($r:ident : $rs:ident),* $(,)?];)*) => {
        /// The builtin store: every host operation the standard library consumes, in prelude (= declaration) order. The method name is the wasm import name and the guest binding label; parameter names match the `/sys` declarations; result labels are the record fields the guest projects. The runtime seeds its implementations from the same rows, so the two ends cannot drift.
        pub fn host_ops() -> ForeignStore {
            let mut store = ForeignStore::new();

            $(
                store.register(ForeignFunction {
                    namespace: "sys",
                    name: stringify!($method).to_string(),
                    label: stringify!($method).to_string(),
                    signature: WireSignature {
                        params: vec![$((stringify!($p).to_string(), wire_of!($ps))),*],
                        results: vec![$((stringify!($r).to_string(), wire_of!($rs))),*],
                    },
                });
            )*

            store
        }
    };
}

host_ops!(declare_host_trait);
host_ops!(declare_host_store);
