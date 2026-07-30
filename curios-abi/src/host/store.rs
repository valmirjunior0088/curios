//! The foreign-function store — the generic description of a host call, from which each consumer derives its own view of the boundary.
//!
//! A [`ForeignFunction`] is one host call, and it is self-describing: its `namespace`/`name` pair is the wasm import (`sys` and a fixed name for a builtin, `ffi` and the declaration's fully qualified name for a user's own `foreign` declaration — the wire-level ABI contract between the emitter and the runtime linker), and its [`WireSignature`] names the operands and results and gives each a [`WireType`]. Every host call is effectful, so reducing one at the type level is always an error — the effect cannot happen at compile time. The IR nodes carry the function as an `Arc`, so every stage reads what it needs straight off the node instead of keeping an independently hand-written spelling in lockstep:
//!
//! - the `/sys` prelude declaration, or a user's own `foreign` declaration (surface parameter types and the named result record the guest projects),
//! - the core elaborator's operand checks and result type,
//! - the wasm emitter's `sys.*`/`ffi.*` import types and call-site operand loads,
//! - the runtime linker's `wasmtime::FuncType`s.
//!
//! A [`ForeignStore`] is the set of foreign functions declared under one tier. [`host_ops`](super::host_ops) seeds the fixed builtin (`sys`) tier, consumable only by the standard library, created per compilation by the pipeline driver; a second store, accumulated from a program's own `foreign` declarations (`curios_text`'s generated foreign signature), holds the `ffi` tier. The two are never merged, but the wasm namespace is the row's own `namespace` field, stamped at declaration time — the store split only governs who may consume a tier. `exit` is deliberately absent from either store: it traps rather than returns and its guest type is the polymorphic bottom `(@A : Type) -> Nat -> A`, which a first-order [`WireSignature`] cannot express, so it stays a hardcoded primitive.

use std::{
    hash::{Hash, Hasher},
    sync::Arc,
};

/// The type of one value crossing the host boundary. The whole vocabulary is six shapes; everything a host op consumes or produces is one of them.
///
/// The scalar cases matter to codegen: a `Nat`/`Bool` operand is unboxed from its i31 carrier *unsigned* (`i31.get_u`) and crosses as a raw wasm `i32`, while `Int` is unboxed *signed* (`i31.get_s`) — `poll`'s timeout keeps the `poll(2)` sign convention. Scalar results re-enter pre-boxed as i31 refs. `Handle` rides the same wire shape as `Bin` (a handle is its token bytes) but stays a distinct guest type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(
        serialize_bounds(__S: rkyv::ser::Writer + rkyv::ser::Allocator, __S::Error: rkyv::rancor::Source),
        deserialize_bounds(__D::Error: rkyv::rancor::Source),
        bytecheck(bounds(__C: rkyv::validation::ArchiveContext))
    )
)]
pub enum WireType {
    Nat,
    Int,
    Bool,
    Bin,
    Handle,
    Lst(#[cfg_attr(feature = "archive", rkyv(omit_bounds))] Box<WireType>),
}

/// The signature of one foreign function: named operands and named results. The result count fixes the guest-facing shape — `0` is the unit value, `1` is the bare result forwarded through, `2..` is a record of the named fields (the labels are load-bearing: the standard library projects `.status`, `.secs_hi`, …).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct WireSignature {
    pub params: Vec<(String, WireType)>,
    pub results: Vec<(String, WireType)>,
}

/// One foreign (host-provided) function. `namespace`/`name` is the wasm import pair — the wire ABI shared by the wasm emitter and the runtime linker; never change one without changing what the other end expects (the unit tests snapshot the builtin set). `namespace` is `sys` for a builtin and `ffi` for a user's `foreign` declaration, whose `name` is its fully qualified name (leading `/`). `label` is the binding name the function surfaces under in the guest.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct ForeignFunction {
    #[cfg_attr(feature = "archive", rkyv(with = crate::Namespace))]
    pub namespace: &'static str,
    pub name: String,
    pub label: String,
    pub signature: WireSignature,
}

// Identity is the wasm import pair: a [`ForeignStore`] never holds two functions with one name (`register` enforces it), so `(namespace, name)` determines the whole row. This keeps term-level equality and hashing O(1) instead of walking the signature — and makes rows from *different* stores with the same content compare equal, so a cached prelude term matches a freshly minted one.
impl PartialEq for ForeignFunction {
    fn eq(&self, other: &Self) -> bool {
        self.namespace == other.namespace && self.name == other.name
    }
}

impl Eq for ForeignFunction {}

impl Hash for ForeignFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.namespace.hash(state);
        self.name.hash(state);
    }
}

/// The foreign functions one compilation declares, in declaration order — the order the prelude surfaces them under `/sys`. Rows are `Arc`ed so the IR nodes share them; cloning a store is a handful of reference bumps.
#[derive(Debug, Clone, Default)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct ForeignStore {
    functions: Vec<Arc<ForeignFunction>>,
}

impl ForeignStore {
    /// An empty store, ready for [`register`](Self::register) calls.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a function. The import name is the identity every stage links on, so registering a duplicate is a construction bug and panics.
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
