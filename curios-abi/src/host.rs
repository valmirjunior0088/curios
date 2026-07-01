//! The foreign-function store — the single description of every host
//! operation, from which each consumer derives its own view of the boundary.
//!
//! A [`ForeignFunction`] is one host call, and it is self-describing: its
//! `name` is the wasm `env` import string (the wire-level ABI contract between
//! the emitter and the runtime linker), its [`WireSignature`] names the
//! operands and results and gives each a [`WireType`], and its [`Reduction`]
//! says what the call does under type-level reduction. The IR nodes carry the
//! function as an `Arc`, so every stage reads what it needs straight off the
//! node instead of keeping an independently hand-written spelling in lockstep:
//!
//! - the `/sys/Io` prelude declaration (surface parameter types and the named
//!   result record the guest projects),
//! - the core elaborator's operand checks and result type,
//! - the wasm emitter's `env.*` import types and call-site operand loads,
//! - the runtime linker's `wasmtime::FuncType`s.
//!
//! A [`ForeignStore`] is the set of foreign functions one compilation
//! declares. Today the only seed is [`sys_io`] — the `/sys/Io` builtins,
//! consumable only by the standard library — created per compilation by the
//! pipeline driver; user-declared foreign functions are future work. `exit` is
//! deliberately absent from the store: it traps rather than returns and its
//! guest type is the polymorphic bottom `(@A : Type) -> Nat -> A`, which a
//! first-order [`WireSignature`] cannot express, so it stays a hardcoded
//! primitive.

use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

/// The type of one value crossing the host boundary. The whole vocabulary is
/// six shapes; everything a host op consumes or produces is one of them.
///
/// The scalar cases matter to codegen: a `Nat`/`Bln` operand is unboxed from
/// its i31 carrier *unsigned* (`i31.get_u`) and crosses as a raw wasm `i32`,
/// while `Int` is unboxed *signed* (`i31.get_s`) — `poll`'s timeout keeps the
/// `poll(2)` sign convention. Scalar results re-enter pre-boxed as i31 refs.
/// `Io` rides the same wire shape as `Bin` (a handle is its token bytes) but
/// stays a distinct guest type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WireType {
    Nat,
    Int,
    Bln,
    Bin,
    Io,
    Arr(Box<WireType>),
}

/// The signature of one foreign function: named operands and named results.
/// The result count fixes the guest-facing shape — `0` is the unit value, `1`
/// is the bare result forwarded through, `2..` is a record of the named fields
/// (the labels are load-bearing: the standard library projects `.status`,
/// `.secs_hi`, …).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WireSignature {
    pub params: Vec<(String, WireType)>,
    pub results: Vec<(String, WireType)>,
}

/// What a foreign call does under type-level reduction. Every call is
/// effectful at runtime; this only decides how the *kernel* treats one that
/// shows up while a type is being computed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reduction {
    /// Reducing the call at the type level is an error — the effect cannot
    /// happen at compile time. Every host op but `args` is this.
    Opaque,
    /// The call is stuck-to-self: it reduces to itself like an inert value, and
    /// becomes a host call only at erasure. For snapshots of immutable host
    /// state — `args`, whose result is fixed for the whole process — so a
    /// top-level value binding does not trip the IO guard.
    Inert,
}

/// One foreign (host-provided) function. `name` is the `env` import string —
/// the wire ABI shared by the wasm emitter and the runtime linker; never
/// change one without changing what the other end expects (the unit tests
/// snapshot the `/sys/Io` set). `label` is the binding name the function
/// surfaces under in the guest.
#[derive(Debug, Clone)]
pub struct ForeignFunction {
    pub name: String,
    pub label: String,
    pub signature: WireSignature,
    pub reduction: Reduction,
}

impl ForeignFunction {
    /// Where the function surfaces in the guest: `(module, label)` under `/sys`
    /// — `("Io", "start_tls")` is `/sys/Io/start_tls`. Also the dotted head the
    /// stage printers render (`Io.start_tls`). Every foreign function lives in
    /// `/sys/Io` today; where user-declared ones surface is future work.
    pub fn path(&self) -> (&'static str, &str) {
        ("Io", &self.label)
    }
}

// Identity is the import name: a [`ForeignStore`] never holds two functions
// with one name (`register` enforces it), so the name determines the whole
// row. This keeps term-level equality and hashing O(1) instead of walking the
// signature — and makes rows from *different* stores with the same content
// compare equal, so a cached prelude term matches a freshly minted one.
impl PartialEq for ForeignFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for ForeignFunction {}

impl Hash for ForeignFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

/// The foreign functions one compilation declares, in declaration order — the
/// order the prelude surfaces them under `/sys/Io`. Rows are `Arc`ed so the IR
/// nodes share them; cloning a store is a handful of reference bumps.
#[derive(Debug, Clone, Default)]
pub struct ForeignStore {
    functions: Vec<Arc<ForeignFunction>>,
}

impl ForeignStore {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a function. The import name is the identity every stage links
    /// on, so registering a duplicate is a construction bug and panics.
    pub fn register(&mut self, function: ForeignFunction) {
        assert!(
            self.get(&function.name).is_none(),
            "foreign function '{}' is already registered",
            function.name
        );

        self.functions.push(Arc::new(function));
    }

    pub fn get(&self, name: &str) -> Option<&Arc<ForeignFunction>> {
        self.functions.iter().find(|function| function.name == name)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Arc<ForeignFunction>> {
        self.functions.iter()
    }

    pub fn len(&self) -> usize {
        self.functions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }
}

/// The `/sys/Io` builtin store: every host operation the standard library
/// consumes, in prelude (= declaration) order. Parameter names match the
/// `/sys/Io` declarations; result labels are the record fields the guest
/// projects. The runtime seeds its implementations from the same rows, so the
/// two ends cannot drift.
pub fn sys_io() -> ForeignStore {
    use {
        Reduction::{Inert, Opaque},
        WireType::{Bin, Bln, Int, Io, Nat},
    };

    fn arr(element: WireType) -> WireType {
        WireType::Arr(Box::new(element))
    }

    fn slots(slots: &[(&str, WireType)]) -> Vec<(String, WireType)> {
        slots
            .iter()
            .map(|(name, type_)| (name.to_string(), type_.clone()))
            .collect()
    }

    let mut store = ForeignStore::new();

    let mut define = |name: &str,
                      label: &str,
                      params: &[(&str, WireType)],
                      results: &[(&str, WireType)],
                      reduction: Reduction| {
        store.register(ForeignFunction {
            name: name.to_string(),
            label: label.to_string(),
            signature: WireSignature {
                params: slots(params),
                results: slots(results),
            },
            reduction,
        });
    };

    define(
        "io_read",
        "read",
        &[("h", Io), ("n", Nat)],
        &[("status", Nat), ("bytes", Bin)],
        Opaque,
    );
    define(
        "io_write",
        "write",
        &[("h", Io), ("b", Bin)],
        &[("status", Nat), ("written", Nat)],
        Opaque,
    );
    define(
        "io_open",
        "open",
        &[("path", Bin), ("mode", Nat)],
        &[("status", Nat), ("handle", Io)],
        Opaque,
    );
    define(
        "io_lookup",
        "lookup",
        &[("host", Bin), ("port", Nat)],
        &[("status", Nat), ("handle", Io)],
        Opaque,
    );
    define(
        "io_resolve",
        "resolve",
        &[("handle", Io)],
        &[("status", Nat), ("addresses", arr(Bin))],
        Opaque,
    );
    define(
        "io_socket",
        "socket",
        &[("addr", Bin)],
        &[("status", Nat), ("handle", Io)],
        Opaque,
    );
    define(
        "io_bind",
        "bind",
        &[("h", Io), ("addr", Bin)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_connect",
        "connect",
        &[("h", Io), ("addr", Bin)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_listen",
        "listen",
        &[("h", Io), ("backlog", Nat)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_accept",
        "accept",
        &[("h", Io)],
        &[("status", Nat), ("handle", Io)],
        Opaque,
    );
    define(
        "io_start_tls",
        "start_tls",
        &[("h", Io), ("sni", Bin)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_tls_server_config",
        "tls_server_config",
        &[("cert", Bin), ("key", Bin)],
        &[("status", Nat), ("handle", Io)],
        Opaque,
    );
    define(
        "io_start_tls_server",
        "start_tls_server",
        &[("h", Io), ("cfg", Io)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_set_nonblocking",
        "set_nonblocking",
        &[("h", Io), ("on", Bln)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_set_recv_timeout",
        "set_recv_timeout",
        &[("h", Io), ("ms", Nat)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_set_send_timeout",
        "set_send_timeout",
        &[("h", Io), ("ms", Nat)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_set_reuseaddr",
        "set_reuseaddr",
        &[("h", Io), ("on", Bln)],
        &[("status", Nat)],
        Opaque,
    );
    define(
        "io_poll",
        "poll",
        &[("handles", arr(Io)), ("events", arr(Nat)), ("timeout", Int)],
        &[("revents", arr(Nat))],
        Opaque,
    );
    define("io_close", "close", &[("h", Io)], &[], Opaque);
    define(
        "io_clock_wall",
        "clock_wall",
        &[],
        &[("secs_hi", Nat), ("secs_lo", Nat), ("nanos", Nat)],
        Opaque,
    );
    define(
        "io_clock_mono",
        "clock_mono",
        &[],
        &[("secs", Nat), ("nanos", Nat)],
        Opaque,
    );
    define(
        "io_random",
        "random",
        &[("n", Nat)],
        &[("bytes", Bin)],
        Opaque,
    );
    define("io_args", "args", &[], &[("argv", arr(Bin))], Inert);
    define(
        "io_env",
        "env",
        &[("name", Bin)],
        &[("status", Nat), ("value", Bin)],
        Opaque,
    );

    store
}
