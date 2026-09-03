//! The foreign-function store — the generic description of a host call, from which each consumer derives its own view of the boundary.
//!
//! A [`ForeignFunction`] is one host call, and it is self-describing: its `namespace`/`name` pair is the wasm import (`sys` and a fixed name for a builtin, `ffi` and the declaration's fully qualified name for a user's own `foreign` declaration — the wire-level ABI contract between the emitter and the runtime linker), and its [`WireSignature`] names the operands and results and gives each a [`WireType`]. Every host call is effectful, so reducing one at the type level is always an error — the effect cannot happen at compile time. The IR nodes carry the function as an `Arc`, so every stage reads what it needs straight off the node instead of keeping an independently hand-written spelling in lockstep:
//!
//! - the `/sys` prelude declaration, or a user's own `foreign` declaration (surface parameter types and the named result record the guest projects),
//! - the core elaborator's operand checks and result type,
//! - the wasm emitter's `sys.*`/`ffi.*` import types and call-site operand loads,
//! - the runtime linker's `wasmtime::FuncType`s.
//!
//! A [`ForeignStore`] is the set of foreign functions declared under one tier. [`host_ops`](super::host_ops) seeds the fixed builtin (`sys`) tier, consumable only by the standard library, created per compilation by the pipeline driver; a second store, accumulated from a program's own `foreign` declarations (`curios_text`'s generated foreign signature), holds the `ffi` tier. The two are never merged, but the wasm namespace is the row's own `namespace` field, stamped at declaration time — the store split only governs who may consume a tier. `exit` is in neither store; only its import name lives here, as [`EXIT`].

use std::{
    fmt::{self, Display, Formatter},
    hash::{Hash, Hasher},
    sync::Arc,
};

/// The element type of a wire [`WireType::List`] — the same vocabulary minus `List` itself, so a list of lists is unrepresentable rather than merely unchecked; `README.md` states why one level is all the boundary handles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireLeaf {
    Nat,
    Int,
    Bool,
    Bytes,
    Handle,
}

/// The type of one value crossing the host boundary — a closed *subset of guest types*, not a vocabulary of wire shapes. Nothing below the type distinguishes `Bytes` from `Handle`: they share a wasm `ValType`, a wasmtime `FuncType` slot, and a load/force/embed path. What separates them is only the guest type `curios-core`'s `wire_term` builds, which is why each variant is spelled the way its guest type is.
///
/// The scalar cases matter to codegen: a `Nat`/`Bool` operand is unboxed from its i31 carrier *unsigned* (`i31.get_u`) and crosses as a raw wasm `i32`, while `Int` is unboxed *signed* (`i31.get_s`) — `poll`'s timeout keeps the `poll(2)` sign convention. Scalar results re-enter pre-boxed as i31 refs. `Bytes` is the byte grain alone: `Bits` and `Byte` are guest types with no wire spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireType {
    Nat,
    Int,
    Bool,
    Bytes,
    Handle,
    List(WireLeaf),
}

// A leaf is a wire type in its own right — the widening every projection over `List` takes to read its element, so the five-way match lives here once instead of in each of them.
impl From<WireLeaf> for WireType {
    fn from(leaf: WireLeaf) -> Self {
        match leaf {
            WireLeaf::Nat => WireType::Nat,
            WireLeaf::Int => WireType::Int,
            WireLeaf::Bool => WireType::Bool,
            WireLeaf::Bytes => WireType::Bytes,
            WireLeaf::Handle => WireType::Handle,
        }
    }
}

/// A wire type that crosses as a raw scalar: unboxed to an `i32` on the way out, re-boxed as an i31 on the way back.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireScalar {
    Nat,
    Int,
    Bool,
}

/// A wire type that crosses as a reference: a flat payload the guest forces on the way out and embeds back into a rope on the way in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireReference {
    Bytes,
    Handle,
    List(WireLeaf),
}

/// A wire type by how it crosses: the two halves of [`WireType`], which every consumer that lowers or lifts a value tells apart.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WireShape {
    Scalar(WireScalar),
    Reference(WireReference),
}

impl WireType {
    /// How this type crosses the boundary.
    pub fn shape(self) -> WireShape {
        match self {
            WireType::Nat => WireShape::Scalar(WireScalar::Nat),
            WireType::Int => WireShape::Scalar(WireScalar::Int),
            WireType::Bool => WireShape::Scalar(WireScalar::Bool),
            WireType::Bytes => WireShape::Reference(WireReference::Bytes),
            WireType::Handle => WireShape::Reference(WireReference::Handle),
            WireType::List(leaf) => WireShape::Reference(WireReference::List(leaf)),
        }
    }
}

impl From<WireScalar> for WireType {
    fn from(scalar: WireScalar) -> Self {
        match scalar {
            WireScalar::Nat => WireType::Nat,
            WireScalar::Int => WireType::Int,
            WireScalar::Bool => WireType::Bool,
        }
    }
}

impl From<WireReference> for WireType {
    fn from(reference: WireReference) -> Self {
        match reference {
            WireReference::Bytes => WireType::Bytes,
            WireReference::Handle => WireType::Handle,
            WireReference::List(leaf) => WireType::List(leaf),
        }
    }
}

/// The named results of one foreign function, in the order they cross: any number of scalars, then at most one reference, last.
///
/// **The shape is the type's, so a row cannot spell a reference anywhere else.** Codegen embeds only the final result back into a rope — an earlier reference would sit under later stack values and need juggling through locals — and the runtime lowers references on the same assumption; `README.md` states the decision. The count fixes the guest-facing shape — `0` is the unit value, `1` the bare result forwarded through, `2..` a record of the named fields, whose labels are load-bearing: the standard library projects `.status`, `.secs_hi`, ….
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct WireResults {
    scalars: Vec<(String, WireScalar)>,
    reference: Option<(String, WireReference)>,
}

impl WireResults {
    /// No results: the unit value.
    pub fn none() -> Self {
        Self {
            scalars: Vec::new(),
            reference: None,
        }
    }

    /// One result of any wire type — a user `foreign` declaration's shape, well-formed whatever the type.
    pub fn single(label: String, wire_type: WireType) -> Self {
        Self::ending(Vec::new(), label, wire_type.shape())
    }

    /// `scalars` followed by `last`, which is the one slot a reference may take.
    pub fn ending(mut scalars: Vec<(String, WireScalar)>, label: String, last: WireShape) -> Self {
        let reference = match last {
            WireShape::Scalar(scalar) => {
                scalars.push((label, scalar));

                None
            }
            WireShape::Reference(reference) => Some((label, reference)),
        };

        Self { scalars, reference }
    }

    /// How many results cross — the count the guest-facing shape is read off.
    pub fn len(&self) -> usize {
        self.scalars.len() + usize::from(self.reference.is_some())
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Every result by label and wire type, in the order they cross: the scalars, then the reference when there is one.
    pub fn iter(&self) -> impl Iterator<Item = (&str, WireType)> + '_ {
        self.scalars
            .iter()
            .map(|(label, scalar)| (label.as_str(), WireType::from(*scalar)))
            .chain(
                self.reference
                    .iter()
                    .map(|(label, reference)| (label.as_str(), WireType::from(*reference))),
            )
    }

    /// The reference result, when there is one — always the last to cross, which is what lets codegen embed it with nothing above it on the stack.
    pub fn reference(&self) -> Option<(&str, WireReference)> {
        self.reference
            .as_ref()
            .map(|(label, reference)| (label.as_str(), *reference))
    }
}

/// The signature of one foreign function: named operands and named results, the results shaped as [`WireResults`] states.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct WireSignature {
    pub params: Vec<(String, WireType)>,
    pub results: WireResults,
}

/// The wasm import namespace a foreign function links under — the closed pair both ends agree on. `Sys` is the fixed builtin substrate, consumable only by the standard library; `Ffi` is a user's own `foreign` declaration.
///
/// Two variants rather than a `&'static str`, so the namespaces that exist are exactly the namespaces that can be written — `README.md`'s decision, with what the string type it replaced could not give.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Namespace {
    Sys,
    Ffi,
}

/// The one `sys` import that is not a store row: `exit` traps rather than returns, so no [`WireSignature`] describes it and it stays a hardcoded intrinsic — but its *name* is still wire, stamped by the emitter and matched by the runtime linker, so it is spelled here where both ends read it rather than once at each; the guest declaration and its type are `curios-text`'s prelude's.
pub const EXIT: &str = "exit";

/// The one export both ends link on: the entrypoint the emitter exports and the runtime looks up to run a program. It spells the entry function's own name under the compiler's naming scheme, so a module dump reads as it links — but the name is a contract stated here, not a consequence of that scheme or of the hint the entry happens to carry, which is what let a debug name decide a wire string before this row existed.
pub const ENTRY: &str = "func/main";

impl Namespace {
    /// The wasm import string: what the emitter stamps on the import and the runtime linker matches.
    pub const fn as_str(self) -> &'static str {
        match self {
            Namespace::Sys => "sys",
            Namespace::Ffi => "ffi",
        }
    }
}

impl Display for Namespace {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

/// One foreign (host-provided) function. `namespace`/`name` is the wasm import pair — the wire ABI shared by the wasm emitter and the runtime linker; never change one without changing what the other end expects (the unit tests snapshot the builtin set). `namespace` is `sys` for a builtin and `ffi` for a user's `foreign` declaration, whose `name` is its fully qualified name (leading `/`). `label` is the binding name the function surfaces under in the guest, and `subject` the module that binding sits in: `Some` for a builtin, whose placement the [`host_ops!`](super::host_ops) table states, and `None` for a user's `foreign` declaration, which the guest already places by writing it where it wants it. The two are independent of the wire pair — a row moves in the module tree without the import moving.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ForeignFunction {
    pub namespace: Namespace,
    pub name: String,
    pub subject: Option<String>,
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
#[curios_archive::archived]
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

    /// Take on every row of `other`, in its order — the union a compilation of several units hands its embedder.
    ///
    /// A duplicate stays a construction bug, and the reason is structural rather than hopeful: an `ffi` row's import name is its declaration's fully qualified name, so a unit mounted at `/a` contributes only `/a/…`, and mount prefixes are checked disjoint before any of this is reached. The one shape that could collide — a mounted `/foo` beside an entry's own `mod foo` — is refused as a mount collision, upstream of here.
    pub fn absorb(&mut self, other: &ForeignStore) {
        for function in other.iter() {
            assert!(
                self.get(&function.name).is_none(),
                "foreign function '{}' is declared by two units; their mount prefixes were not disjoint",
                function.name
            );

            self.functions.push(Arc::clone(function));
        }
    }
}
