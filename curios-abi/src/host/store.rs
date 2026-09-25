//! The foreign-function store — the generic description of a host call, from which each consumer derives its own view of the boundary.
//!
//! A [`ForeignFunction`] is one host call: a builtin, which is its [`HostOp`](super::HostOp) identity and reads every fact about itself back from the roster, or a user's own `foreign` declaration, which carries its own. Either way its `namespace`/`name` pair is the wasm import (`sys` and the roster's wire name for a builtin, `ffi` and the declaration's fully qualified name for a declared row — the wire-level ABI contract between the emitter and the runtime linker), and its [`WireSignature`] names the operands and results and gives each a [`WireType`]. Every host call is effectful, so reducing one at the type level is always an error — the effect cannot happen at compile time. The IR nodes carry the function as an `Arc`, so every stage reads what it needs through the node instead of keeping an independently hand-written spelling in lockstep:
//!
//! - the `/sys` prelude declaration, or a user's own `foreign` declaration (surface parameter types and the named result record the guest projects),
//! - the core elaborator's operand checks and result type,
//! - the wasm emitter's `sys.*`/`ffi.*` import types and call-site operand loads,
//! - the runtime linker's `wasmtime::FuncType`s.
//!
//! A [`ForeignStore`] is the set of foreign functions declared under one tier. [`host_ops`](super::host_ops) seeds the fixed builtin (`sys`) tier, consumable only by the standard library, created per compilation by the pipeline driver; a second store, accumulated from a program's own `foreign` declarations (`curios_text`'s generated foreign signature), holds the `ffi` tier. The two are never merged, but the wasm namespace is the row's own `namespace` field, stamped at declaration time — the store split only governs who may consume a tier. `exit` is in neither store; only its import name lives here, as [`EXIT`].

use {
    super::HostOp,
    std::{
        fmt::{self, Display, Formatter},
        hash::{Hash, Hasher},
        sync::Arc,
    },
};

/// The element type of a wire [`WireType::List`] — the same vocabulary minus `List` itself, so a list of lists is unrepresentable rather than merely unchecked; `README.md` states why one level is all the boundary handles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireLeaf {
    Nat,
    Int,
    Bool,
    Bytes,
    Bits,
    Handle,
}

/// The type of one value crossing the host boundary — a closed *subset of guest types*, not a vocabulary of wire shapes. Nothing below the type distinguishes `Bytes` from `Handle`: they share a wasm `ValType`, a wasmtime `FuncType` slot, and a load/force/embed path. What separates them is only the guest type `curios-core`'s `wire_term` builds, which is why each variant is spelled the way its guest type is.
///
/// The scalar cases matter to codegen: a `Bool` or a `Byte` crosses as its raw wasm `i32` word, and a `Nat` or `Int` is narrowed to a raw wasm `i64` — an i31 read signed and widened, and a boxed magnitude the `i64` cannot hold refused, the one place either narrows by refusing — so `handle_poll`'s timeout keeps the `poll(2)` sign convention. An `Flt` is read out of its boxed `f64` and crosses as a raw wasm `f64`, every bit kept by the native runtime; a JavaScript host receives a `Number`, and the WebAssembly JavaScript interface leaves a NaN's sign and payload to the engine in both directions, so a NaN's bits are guaranteed only against the native runtime. A scalar result comes back the same way, as the raw number, and the guest boxes it ([`WireScalar`] says why). `Bytes` and `Bits` are the two packed grains, distinct guest types over one payload: a row states which grain it means, and the guest seals the returned payload at that grain's length. A `Byte` is a word below 256 in both directions and is not a list element: `List(Byte)` is the `Bytes` a row already spells.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireType {
    Nat,
    Int,
    Bool,
    Byte,
    Flt,
    Bytes,
    Bits,
    Handle,
    List(WireLeaf),
}

// A leaf is a wire type in its own right — the widening every projection over `List` takes to read its element, stated here once instead of in each of them.
impl From<WireLeaf> for WireType {
    fn from(leaf: WireLeaf) -> Self {
        match leaf {
            WireLeaf::Nat => WireType::Nat,
            WireLeaf::Int => WireType::Int,
            WireLeaf::Bool => WireType::Bool,
            WireLeaf::Bytes => WireType::Bytes,
            WireLeaf::Bits => WireType::Bits,
            WireLeaf::Handle => WireType::Handle,
        }
    }
}

/// A wire type that crosses as a raw wasm value rather than a reference — which is what decides that it may stand in any result slot, where a reference must stand last.
///
/// **Every scalar re-enters raw, and the guest boxes it.** `Nat` and `Int` cross as `i64`, `Bool` and `Byte` as `i32` and `Flt` as `f64`, in both directions. Every box is a layout `curios-emit` defines — an `Flt`'s struct, and a `Nat` or `Int` past the i31 a boxed magnitude — so a host that allocated one would be a second crate needing to know it, and a host that minted only the i31 would have to refuse a result past it. The guest doing the reinterpretation is the discipline `FltOfLeBytes` already keeps, where a float arriving as eight bytes is decoded on the guest side and never crosses as one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireScalar {
    Nat,
    Int,
    Bool,
    Byte,
    Flt,
}

/// A wire type that crosses as a reference: a flat payload the guest forces on the way out and embeds back into a rope on the way in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireReference {
    Bytes,
    Bits,
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
            WireType::Byte => WireShape::Scalar(WireScalar::Byte),
            WireType::Flt => WireShape::Scalar(WireScalar::Flt),
            WireType::Bytes => WireShape::Reference(WireReference::Bytes),
            WireType::Bits => WireShape::Reference(WireReference::Bits),
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
            WireScalar::Byte => WireType::Byte,
            WireScalar::Flt => WireType::Flt,
        }
    }
}

impl From<WireReference> for WireType {
    fn from(reference: WireReference) -> Self {
        match reference {
            WireReference::Bytes => WireType::Bytes,
            WireReference::Bits => WireType::Bits,
            WireReference::Handle => WireType::Handle,
            WireReference::List(leaf) => WireType::List(leaf),
        }
    }
}

/// The named results of one foreign function, in the order they cross: any number of scalars, then at most one reference, last.
///
/// **The shape is the type's, so a row cannot spell a reference anywhere else.** Codegen embeds only the final result back into a rope — an earlier reference would sit under later stack values and need juggling through locals — and the runtime lowers references on the same assumption; `README.md` states the decision. The count fixes the guest-facing shape — `0` is the unit value, `1` the bare result forwarded through, `2..` a record of the named fields, whose labels are load-bearing: the standard library projects `.status`, `.secs`, ….
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

    /// The shape the guest sees these results in, read off their count — the one statement of the arity rule the prelude's declaration, the elaborator's and the kernel's types are all built from.
    pub fn shape(&self) -> ResultShape<'_> {
        let mut results = self.iter();

        match (results.next(), results.next()) {
            (None, _) => ResultShape::Unit,
            (Some((_, wire_type)), None) => ResultShape::Single(wire_type),
            (Some(_), Some(_)) => ResultShape::Record(self.iter().collect()),
        }
    }
}

/// How a row's results reach the guest: no result is the unit value, one is the bare value forwarded through, two or more a record of the named fields — whose labels are load-bearing, since the standard library projects `.status`, `.secs`, ….
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResultShape<'a> {
    Unit,
    Single(WireType),
    Record(Vec<(&'a str, WireType)>),
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

/// The other `sys` import that is not a store row: the emitter's own refusal. `panic` takes a byte string and traps rather than returns, so no [`WireSignature`] describes it; no `/sys` declaration names it either, since no program can spell it — `curios-emit` calls it wherever it refuses a computation, with one constant sentence per refusal class, and both runtimes render what arrives as `panicked: …`. Only the name is wire.
pub const PANIC: &str = "panic";

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

/// One foreign (host-provided) function: a builtin the roster names, or a user's own `foreign` declaration.
///
/// **A builtin is its identity and nothing else.** [`HostOp`] names a row of the one authored table, and every fact about the row — its wire name, `/sys` placement, signature and description — is read back from the table, so no term carries a description of a builtin that could disagree with it. A declared row has no table to point into and carries its signature itself. The accessors below answer both alike: the wasm import pair is `(namespace, name)` — `sys` and the roster's wire name for a builtin, `ffi` and the declaration's fully qualified name (leading `/`) for a declared row — the wire ABI shared by the wasm emitter and the runtime linker. `label` is the binding the function surfaces under in the guest and `subject` the module that binding sits in: `Some` for a builtin, whose placement the table states, and `None` for a declared row, which the guest already places by writing it where it wants it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum ForeignFunction {
    Builtin(HostOp),
    Declared(DeclaredForeign),
}

/// A user's own `foreign` declaration, as its row: the fully qualified name it imports under in the `ffi` namespace, the label it binds, and the signature it declared.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct DeclaredForeign {
    pub name: String,
    pub label: String,
    pub signature: WireSignature,
}

// A declared row's identity is its import name: a [`ForeignStore`] never holds two functions with one name (`register` enforces it), and qualified names are unique per compilation, so the name determines the whole row. Equality and hashing stay O(1) rather than walking the signature.
impl PartialEq for DeclaredForeign {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for DeclaredForeign {}

impl Hash for DeclaredForeign {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl ForeignFunction {
    /// The wasm import namespace: `sys` for a builtin, `ffi` for a declared row.
    pub fn namespace(&self) -> Namespace {
        match self {
            ForeignFunction::Builtin(_) => Namespace::Sys,
            ForeignFunction::Declared(_) => Namespace::Ffi,
        }
    }

    /// The wasm import name: the roster's wire name, or the declaration's fully qualified name.
    pub fn name(&self) -> &str {
        match self {
            ForeignFunction::Builtin(op) => op.name(),
            ForeignFunction::Declared(declared) => &declared.name,
        }
    }

    /// The `/sys` module a builtin surfaces in; a declared row is placed where it was written.
    pub fn subject(&self) -> Option<&str> {
        match self {
            ForeignFunction::Builtin(op) => Some(op.subject()),
            ForeignFunction::Declared(_) => None,
        }
    }

    /// The binding the function surfaces as in the guest.
    pub fn label(&self) -> &str {
        match self {
            ForeignFunction::Builtin(op) => op.label(),
            ForeignFunction::Declared(declared) => &declared.label,
        }
    }

    /// The operands and results: the roster's for a builtin, the declaration's own otherwise.
    pub fn signature(&self) -> &WireSignature {
        match self {
            ForeignFunction::Builtin(op) => op.signature(),
            ForeignFunction::Declared(declared) => &declared.signature,
        }
    }

    /// What the operation does, in the words the roster states it in — the guest's own documentation of the row, so a page showing a builtin says the same thing the table says. Empty for a declared row, whose prose sits on the declaration the author wrote.
    pub fn description(&self) -> &str {
        match self {
            ForeignFunction::Builtin(op) => op.description(),
            ForeignFunction::Declared(_) => "",
        }
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
            self.get(function.name()).is_none(),
            "foreign function '{}' is already registered",
            function.name()
        );

        self.functions.push(Arc::new(function));
    }

    /// The row registered under `name` — the wasm import string, the identity every stage links on. Linear scan; stores hold a few dozen rows at most.
    pub fn get(&self, name: &str) -> Option<&Arc<ForeignFunction>> {
        self.functions
            .iter()
            .find(|function| function.name() == name)
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
                self.get(function.name()).is_none(),
                "foreign function '{}' is declared by two units; their mount prefixes were not disjoint",
                function.name()
            );

            self.functions.push(Arc::clone(function));
        }
    }
}
