//! The foreign-function store — the single description of every host
//! operation, from which each consumer derives its own view of the boundary.
//!
//! A [`ForeignFunction`] is one host call, and it is self-describing: its
//! `name` is the wasm import string (under [`NAMESPACE_SYS`] for a builtin,
//! [`NAMESPACE_ENV`] for a user's own `foreign` declaration — the wire-level
//! ABI contract between the emitter and the runtime linker), and its
//! [`WireSignature`] names the operands and results and gives each a
//! [`WireType`]. Every host call is effectful, so reducing one at the type
//! level is always an error — the effect cannot happen at compile time. The
//! IR nodes carry the function as an `Arc`, so every stage reads what it needs
//! straight off the node instead of keeping an independently hand-written
//! spelling in lockstep:
//!
//! - the `/sys/Io` prelude declaration, or a user's own `foreign` declaration
//!   (surface parameter types and the named result record the guest projects),
//! - the core elaborator's operand checks and result type,
//! - the wasm emitter's `sys.*`/`env.*` import types and call-site operand loads,
//! - the runtime linker's `wasmtime::FuncType`s.
//!
//! A [`ForeignStore`] is the set of foreign functions declared under one
//! tier. [`sys_io`] seeds the fixed `/sys/Io` builtin tier, consumable only by
//! the standard library, created per compilation by the pipeline driver; a
//! second store, accumulated from a program's own `foreign` declarations
//! (`curios_text::prelude::foreign_signature`), holds the `env` tier. The two
//! are never merged — which store a row lives in is what fixes its wasm
//! namespace, not a field on the row. `exit` is deliberately absent from
//! either store: it traps rather than returns and its guest type is the
//! polymorphic bottom `(@A : Type) -> Nat -> A`, which a first-order
//! [`WireSignature`] cannot express, so it stays a hardcoded primitive.

use {
    crate::RootId,
    std::{
        hash::{Hash, Hasher},
        sync::Arc,
    },
};

/// The wasm import namespace the fixed `/sys/Io` builtins are declared under.
pub const NAMESPACE_SYS: &str = "sys";

/// The wasm import namespace every user-declared `foreign` function is
/// declared under — flat, not per-module.
pub const NAMESPACE_ENV: &str = "env";

/// The exported entrypoint the runtime invokes. `cont`'s wasm emitter names
/// every function `func/<name>` and exports the entry — always `main` — under
/// that scheme.
pub const MAIN_EXPORT: &str = "func/main";

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
    Lst(Box<WireType>),
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

/// One foreign (host-provided) function. `name` is the wasm import string —
/// the wire ABI shared by the wasm emitter and the runtime linker; never
/// change one without changing what the other end expects (the unit tests
/// snapshot the `/sys/Io` set). `label` is the binding name the function
/// surfaces under in the guest. `root` is the compilation root that declared
/// it (`RootId::SYS` for every [`sys_io`] row) — consulted by codegen instead
/// of re-deriving "is this a `/sys/Io` builtin?" by rebuilding [`sys_io`] and
/// testing membership.
#[derive(Debug, Clone)]
pub struct ForeignFunction {
    pub name: String,
    pub label: String,
    pub signature: WireSignature,
    pub root: RootId,
}

// Identity is the import name: a [`ForeignStore`] never holds two functions
// with one name (`register` enforces it), so the name determines the whole
// row. This keeps term-level equality and hashing O(1) instead of walking the
// signature — and makes rows from *different* stores with the same content
// compare equal, so a cached prelude term matches a freshly minted one. `root`
// is deliberately excluded: it is provenance, not identity.
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
    /// An empty store, ready for [`register`](Self::register) calls.
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

    /// The row registered under `name` — the wasm import string, the identity every stage links on. Linear scan; stores hold a few dozen rows at most.
    pub fn get(&self, name: &str) -> Option<&Arc<ForeignFunction>> {
        self.functions.iter().find(|function| function.name == name)
    }

    /// The rows in registration order — the declaration order the prelude binds them in and the runtime seeds its implementations by.
    pub fn iter(&self) -> impl Iterator<Item = &Arc<ForeignFunction>> {
        self.functions.iter()
    }

}

#[cfg(test)]
impl ForeignStore {
    /// Test-only: the number of rows in the store.
    pub(crate) fn len(&self) -> usize {
        self.functions.len()
    }
}

fn lst(element: WireType) -> WireType {
    WireType::Lst(Box::new(element))
}

fn slots(slots: Vec<(&str, WireType)>) -> Vec<(String, WireType)> {
    slots
        .into_iter()
        .map(|(name, type_)| (name.to_string(), type_))
        .collect()
}

/// The `/sys/Io` builtin store: every host operation the standard library
/// consumes, in prelude (= declaration) order. Parameter names match the
/// `/sys/Io` declarations; result labels are the record fields the guest
/// projects. The runtime seeds its implementations from the same rows, so the
/// two ends cannot drift.
pub fn sys_io() -> ForeignStore {
    let mut store = ForeignStore::new();

    for (name, label, params, results) in [
        (
            "io_read",
            "read",
            vec![("h", WireType::Io), ("n", WireType::Nat)],
            vec![("status", WireType::Nat), ("bytes", WireType::Bin)],
        ),
        (
            "io_write",
            "write",
            vec![("h", WireType::Io), ("b", WireType::Bin)],
            vec![("status", WireType::Nat), ("written", WireType::Nat)],
        ),
        (
            "io_open",
            "open",
            vec![("path", WireType::Bin), ("mode", WireType::Nat)],
            vec![("status", WireType::Nat), ("handle", WireType::Io)],
        ),
        (
            "io_lookup",
            "lookup",
            vec![("host", WireType::Bin), ("port", WireType::Nat)],
            vec![("status", WireType::Nat), ("handle", WireType::Io)],
        ),
        (
            "io_resolve",
            "resolve",
            vec![("handle", WireType::Io)],
            vec![("status", WireType::Nat), ("addresses", lst(WireType::Bin))],
        ),
        (
            "io_socket",
            "socket",
            vec![("addr", WireType::Bin)],
            vec![("status", WireType::Nat), ("handle", WireType::Io)],
        ),
        (
            "io_bind",
            "bind",
            vec![("h", WireType::Io), ("addr", WireType::Bin)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_connect",
            "connect",
            vec![("h", WireType::Io), ("addr", WireType::Bin)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_listen",
            "listen",
            vec![("h", WireType::Io), ("backlog", WireType::Nat)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_accept",
            "accept",
            vec![("h", WireType::Io)],
            vec![("status", WireType::Nat), ("handle", WireType::Io)],
        ),
        (
            "io_start_tls",
            "start_tls",
            vec![("h", WireType::Io), ("sni", WireType::Bin)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_tls_server_config",
            "tls_server_config",
            vec![("cert", WireType::Bin), ("key", WireType::Bin)],
            vec![("status", WireType::Nat), ("handle", WireType::Io)],
        ),
        (
            "io_start_tls_server",
            "start_tls_server",
            vec![("h", WireType::Io), ("cfg", WireType::Io)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_set_nonblocking",
            "set_nonblocking",
            vec![("h", WireType::Io), ("on", WireType::Bln)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_set_recv_timeout",
            "set_recv_timeout",
            vec![("h", WireType::Io), ("ms", WireType::Nat)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_set_send_timeout",
            "set_send_timeout",
            vec![("h", WireType::Io), ("ms", WireType::Nat)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_set_reuseaddr",
            "set_reuseaddr",
            vec![("h", WireType::Io), ("on", WireType::Bln)],
            vec![("status", WireType::Nat)],
        ),
        (
            "io_poll",
            "poll",
            vec![
                ("handles", lst(WireType::Io)),
                ("events", lst(WireType::Nat)),
                ("timeout", WireType::Int),
            ],
            vec![("revents", lst(WireType::Nat))],
        ),
        ("io_close", "close", vec![("h", WireType::Io)], vec![]),
        (
            "io_clock_wall",
            "clock_wall",
            vec![],
            vec![
                ("secs_hi", WireType::Nat),
                ("secs_lo", WireType::Nat),
                ("nanos", WireType::Nat),
            ],
        ),
        (
            "io_clock_mono",
            "clock_mono",
            vec![],
            vec![("secs", WireType::Nat), ("nanos", WireType::Nat)],
        ),
        (
            "io_random",
            "random",
            vec![("n", WireType::Nat)],
            vec![("bytes", WireType::Bin)],
        ),
        (
            "io_args",
            "args",
            vec![],
            vec![("argv", lst(WireType::Bin))],
        ),
        (
            "io_env",
            "env",
            vec![("name", WireType::Bin)],
            vec![("status", WireType::Nat), ("value", WireType::Bin)],
        ),
    ] {
        store.register(ForeignFunction {
            name: name.to_string(),
            label: label.to_string(),
            signature: WireSignature {
                params: slots(params),
                results: slots(results),
            },
            root: RootId::SYS,
        });
    }

    store
}
