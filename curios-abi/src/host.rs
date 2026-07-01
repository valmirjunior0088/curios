//! The host-function table — the single description of every `/sys/Io` host
//! operation, from which each consumer derives its own view of the boundary.
//!
//! A [`HostFunction`] is one host call; its [`WireSignature`] names the operands
//! and results and gives each a [`WireType`]. Four derivations read the table,
//! replacing four independently hand-written spellings that previously had to be
//! kept in lockstep:
//!
//! - the `/sys/Io` prelude declaration (surface parameter types and the named
//!   result record the guest projects),
//! - the core elaborator's operand checks and result type,
//! - the wasm emitter's `env.*` import types and call-site operand loads,
//! - the runtime linker's `wasmtime::FuncType`s.
//!
//! [`HostFunction::name`] is the `env` import name — the wire-level ABI contract
//! between the emitter and the linker; the unit tests pin all 24 strings.
//! `exit` is deliberately absent: it traps rather than returns and its guest
//! type is the polymorphic bottom `(@A : Type) -> Nat -> A`, which a first-order
//! [`WireSignature`] cannot express, so it stays a hardcoded primitive.

/// The type of one value crossing the host boundary. The whole vocabulary is
/// six shapes; everything a host op consumes or produces is one of them.
///
/// The scalar cases matter to codegen: a `Nat`/`Bln` operand is unboxed from
/// its i31 carrier *unsigned* (`i31.get_u`) and crosses as a raw wasm `i32`,
/// while `Int` is unboxed *signed* (`i31.get_s`) — `poll`'s timeout keeps the
/// `poll(2)` sign convention. Scalar results re-enter pre-boxed as i31 refs.
/// `Io` rides the same wire shape as `Bin` (a handle is its token bytes) but
/// stays a distinct guest type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WireType {
    Nat,
    Int,
    Bln,
    Bin,
    Io,
    Arr(&'static WireType),
}

/// The signature of one host function: named operands and named results. The
/// result count fixes the guest-facing shape — `0` is the unit value, `1` is
/// the bare result forwarded through, `2..` is a record of the named fields
/// (the labels are load-bearing: the standard library projects `.status`,
/// `.secs_hi`, …).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WireSignature {
    pub params: &'static [(&'static str, WireType)],
    pub results: &'static [(&'static str, WireType)],
}

/// One host operation. Declared in `/sys/Io` prelude order, and fieldless, so
/// `f as usize` indexes per-function tables ([`HostFunction::ALL`] mirrors the
/// declaration order exactly — pinned by a unit test).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HostFunction {
    Read,
    Write,
    Open,
    Lookup,
    Resolve,
    Socket,
    Bind,
    Connect,
    Listen,
    Accept,
    StartTls,
    TlsServerConfig,
    StartTlsServer,
    SetNonblocking,
    SetRecvTimeout,
    SetSendTimeout,
    SetReuseaddr,
    Poll,
    Close,
    ClockWall,
    ClockMono,
    Random,
    Args,
    Env,
}

use WireType::{Arr, Bin, Bln, Int, Io, Nat};

impl HostFunction {
    /// Every host function, in declaration (= `/sys/Io` prelude) order. The
    /// canonical iteration order wherever all functions are visited — the
    /// prelude declarations and the wasm import section both follow it.
    pub const ALL: [HostFunction; 24] = [
        HostFunction::Read,
        HostFunction::Write,
        HostFunction::Open,
        HostFunction::Lookup,
        HostFunction::Resolve,
        HostFunction::Socket,
        HostFunction::Bind,
        HostFunction::Connect,
        HostFunction::Listen,
        HostFunction::Accept,
        HostFunction::StartTls,
        HostFunction::TlsServerConfig,
        HostFunction::StartTlsServer,
        HostFunction::SetNonblocking,
        HostFunction::SetRecvTimeout,
        HostFunction::SetSendTimeout,
        HostFunction::SetReuseaddr,
        HostFunction::Poll,
        HostFunction::Close,
        HostFunction::ClockWall,
        HostFunction::ClockMono,
        HostFunction::Random,
        HostFunction::Args,
        HostFunction::Env,
    ];

    /// The `env` module import name — the wire-level ABI string shared by the
    /// wasm emitter and the runtime linker. Never change one of these without
    /// changing what the other end expects; the unit tests snapshot all 24.
    pub fn name(self) -> &'static str {
        match self {
            HostFunction::Read => "io_read",
            HostFunction::Write => "io_write",
            HostFunction::Open => "io_open",
            HostFunction::Lookup => "io_lookup",
            HostFunction::Resolve => "io_resolve",
            HostFunction::Socket => "io_socket",
            HostFunction::Bind => "io_bind",
            HostFunction::Connect => "io_connect",
            HostFunction::Listen => "io_listen",
            HostFunction::Accept => "io_accept",
            HostFunction::StartTls => "io_start_tls",
            HostFunction::TlsServerConfig => "io_tls_server_config",
            HostFunction::StartTlsServer => "io_start_tls_server",
            HostFunction::SetNonblocking => "io_set_nonblocking",
            HostFunction::SetRecvTimeout => "io_set_recv_timeout",
            HostFunction::SetSendTimeout => "io_set_send_timeout",
            HostFunction::SetReuseaddr => "io_set_reuseaddr",
            HostFunction::Poll => "io_poll",
            HostFunction::Close => "io_close",
            HostFunction::ClockWall => "io_clock_wall",
            HostFunction::ClockMono => "io_clock_mono",
            HostFunction::Random => "io_random",
            HostFunction::Args => "io_args",
            HostFunction::Env => "io_env",
        }
    }

    /// Where the function surfaces in the guest: `(module, label)` under `/sys`
    /// — `("Io", "start_tls")` is `/sys/Io/start_tls`. Also the dotted head the
    /// stage printers render (`Io.start_tls`).
    pub fn path(self) -> (&'static str, &'static str) {
        let label = match self {
            HostFunction::Read => "read",
            HostFunction::Write => "write",
            HostFunction::Open => "open",
            HostFunction::Lookup => "lookup",
            HostFunction::Resolve => "resolve",
            HostFunction::Socket => "socket",
            HostFunction::Bind => "bind",
            HostFunction::Connect => "connect",
            HostFunction::Listen => "listen",
            HostFunction::Accept => "accept",
            HostFunction::StartTls => "start_tls",
            HostFunction::TlsServerConfig => "tls_server_config",
            HostFunction::StartTlsServer => "start_tls_server",
            HostFunction::SetNonblocking => "set_nonblocking",
            HostFunction::SetRecvTimeout => "set_recv_timeout",
            HostFunction::SetSendTimeout => "set_send_timeout",
            HostFunction::SetReuseaddr => "set_reuseaddr",
            HostFunction::Poll => "poll",
            HostFunction::Close => "close",
            HostFunction::ClockWall => "clock_wall",
            HostFunction::ClockMono => "clock_mono",
            HostFunction::Random => "random",
            HostFunction::Args => "args",
            HostFunction::Env => "env",
        };

        ("Io", label)
    }

    /// The signature: named operands and named results, in order. Parameter
    /// names match the `/sys/Io` prelude declarations; result labels are the
    /// record fields the guest projects.
    pub fn signature(self) -> WireSignature {
        let (params, results): (
            &'static [(&'static str, WireType)],
            &'static [(&'static str, WireType)],
        ) = match self {
            HostFunction::Read => (&[("h", Io), ("n", Nat)], &[("status", Nat), ("bytes", Bin)]),
            HostFunction::Write => (
                &[("h", Io), ("b", Bin)],
                &[("status", Nat), ("written", Nat)],
            ),
            HostFunction::Open => (
                &[("path", Bin), ("mode", Nat)],
                &[("status", Nat), ("handle", Io)],
            ),
            HostFunction::Lookup => (
                &[("host", Bin), ("port", Nat)],
                &[("status", Nat), ("handle", Io)],
            ),
            HostFunction::Resolve => (
                &[("handle", Io)],
                &[("status", Nat), ("addresses", Arr(&Bin))],
            ),
            HostFunction::Socket => (&[("addr", Bin)], &[("status", Nat), ("handle", Io)]),
            HostFunction::Bind => (&[("h", Io), ("addr", Bin)], &[("status", Nat)]),
            HostFunction::Connect => (&[("h", Io), ("addr", Bin)], &[("status", Nat)]),
            HostFunction::Listen => (&[("h", Io), ("backlog", Nat)], &[("status", Nat)]),
            HostFunction::Accept => (&[("h", Io)], &[("status", Nat), ("handle", Io)]),
            HostFunction::StartTls => (&[("h", Io), ("sni", Bin)], &[("status", Nat)]),
            HostFunction::TlsServerConfig => (
                &[("cert", Bin), ("key", Bin)],
                &[("status", Nat), ("handle", Io)],
            ),
            HostFunction::StartTlsServer => (&[("h", Io), ("cfg", Io)], &[("status", Nat)]),
            HostFunction::SetNonblocking => (&[("h", Io), ("on", Bln)], &[("status", Nat)]),
            HostFunction::SetRecvTimeout => (&[("h", Io), ("ms", Nat)], &[("status", Nat)]),
            HostFunction::SetSendTimeout => (&[("h", Io), ("ms", Nat)], &[("status", Nat)]),
            HostFunction::SetReuseaddr => (&[("h", Io), ("on", Bln)], &[("status", Nat)]),
            HostFunction::Poll => (
                &[
                    ("handles", Arr(&Io)),
                    ("events", Arr(&Nat)),
                    ("timeout", Int),
                ],
                &[("revents", Arr(&Nat))],
            ),
            HostFunction::Close => (&[("h", Io)], &[]),
            HostFunction::ClockWall => (&[], &[("secs_hi", Nat), ("secs_lo", Nat), ("nanos", Nat)]),
            HostFunction::ClockMono => (&[], &[("secs", Nat), ("nanos", Nat)]),
            HostFunction::Random => (&[("n", Nat)], &[("bytes", Bin)]),
            HostFunction::Args => (&[], &[("argv", Arr(&Bin))]),
            HostFunction::Env => (&[("name", Bin)], &[("status", Nat), ("value", Bin)]),
        };

        WireSignature { params, results }
    }
}
