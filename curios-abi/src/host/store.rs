//! The foreign-function store — the generic description of a host call, from which each consumer derives its own view of the boundary.
//!
//! A [`ForeignFunction`] is one host call as its `foreign` declaration states it: the fully qualified name it is declared under, which is its wasm import name in the `ffi` namespace — the wire-level ABI contract between the wasm emitter and whatever links the program — and a [`WireSignature`] that names the results and gives each operand and result a [`WireType`]. Every host call is effectful, so reducing one at the type level is always an error — the effect cannot happen at compile time. The IR nodes carry the function as an `Arc`, so every stage reads what it needs through the node instead of keeping an independently hand-written spelling in lockstep:
//!
//! - the `foreign` declaration itself (surface parameter types and the named result record the guest projects),
//! - the core elaborator's operand checks and result type,
//! - the wasm emitter's `ffi.*` import types and call-site operand loads,
//! - the runtime linker's `wasmtime::FuncType`s.
//!
//! A [`ForeignStore`] is the set of foreign functions a unit declares, and a compilation hands its embedder the union of the ones its program reaches. A function says nothing of who answers it: a host's own operations are declarations of the standard library, as a package's are its own, and whether a name is answered by a module, an embedder or the host is decided where the program is linked.

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

/// A wire type that crosses as a raw wasm value rather than a reference.
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

/// What a wire type crosses the boundary as at the Wasm level, in either direction: the one statement the emitter types an import by and a runtime answers it by, so the two ends have no mapping of their own to keep alike.
///
/// A scalar is the number it is. A reference is a flat payload, an array the guest forces a value to on the way out and embeds back on the way in: bytes for the two packed grains and for a handle's token, and for a list one element a value, a scalar as a lone one crosses and a reference as its own payload.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Carrier {
    /// A raw `i64`: a `Nat` or an `Int`.
    I64,
    /// A raw `i32`: a `Bool` or a `Byte`.
    I32,
    /// A raw `f64`: an `Flt`.
    F64,
    /// An array of `i8`: a `Bytes`, a `Bits`, or the token a `Handle` is.
    Bytes,
    /// An array of `i64`: a list of `Nat` or of `Int`.
    Longs,
    /// An array of `i32`: a list of `Bool`.
    Words,
    /// An array of references, each the payload its element crosses as: a list of `Bytes`, `Bits` or `Handle`.
    Elems,
}

impl WireType {
    /// What this type crosses the boundary as.
    pub fn carrier(self) -> Carrier {
        match self {
            WireType::Nat | WireType::Int => Carrier::I64,
            WireType::Bool | WireType::Byte => Carrier::I32,
            WireType::Flt => Carrier::F64,
            WireType::Bytes | WireType::Bits | WireType::Handle => Carrier::Bytes,
            WireType::List(WireLeaf::Nat | WireLeaf::Int) => Carrier::Longs,
            WireType::List(WireLeaf::Bool) => Carrier::Words,
            WireType::List(WireLeaf::Bytes | WireLeaf::Bits | WireLeaf::Handle) => Carrier::Elems,
        }
    }

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

/// A wire type as a `foreign` declaration spells it — `Nat`, `List(Bool)` — the one spelling the printer and every description of a row handed to an embedder read.
impl Display for WireType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            WireType::Nat => formatter.write_str("Nat"),
            WireType::Int => formatter.write_str("Int"),
            WireType::Bool => formatter.write_str("Bool"),
            WireType::Byte => formatter.write_str("Byte"),
            WireType::Flt => formatter.write_str("Flt"),
            WireType::Bytes => formatter.write_str("Bytes"),
            WireType::Bits => formatter.write_str("Bits"),
            WireType::Handle => formatter.write_str("Handle"),
            WireType::List(leaf) => write!(formatter, "List({})", WireType::from(*leaf)),
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

/// The results of one foreign function, in the order they cross, a scalar or a reference in whatever slot the row gives it.
///
/// The count fixes the guest-facing shape, as [`ResultShape`] states, and a result is labelled only where that shape reads a label: the fields of a record. One result crosses bare, so a name for it is nothing the wire carries, and a declaration writes none. Each shape has its own constructor, so no label is given where none is kept and a record cannot be built of fewer than two fields. Where a reference stands is no constraint on anyone: the guest waits a row's results out in locals and embeds each reference where it stands, and the runtime lowers each result into its own slot; `README.md` states the decision and what it rejected.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct WireResults {
    slots: Slots,
}

/// The results as their count shapes them, so no label stands where nothing reads one.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
enum Slots {
    None,
    Single(WireType),
    Record(Vec<(String, WireType)>),
}

impl WireResults {
    /// No results: the unit value.
    pub fn none() -> Self {
        Self { slots: Slots::None }
    }

    /// One result of any wire type, forwarded through as itself — a user `foreign` declaration's bare result, well-formed whatever the type.
    pub fn single(wire_type: WireType) -> Self {
        Self {
            slots: Slots::Single(wire_type),
        }
    }

    /// The record of the fields named, in the order they cross: `first`, `second`, then `rest`. Two at least, since one result crosses bare and keeps no label.
    pub fn record(
        first: (String, WireType),
        second: (String, WireType),
        rest: Vec<(String, WireType)>,
    ) -> Self {
        Self {
            slots: Slots::Record([first, second].into_iter().chain(rest).collect()),
        }
    }

    /// How many results cross — the count the guest-facing shape is read off.
    pub fn len(&self) -> usize {
        match &self.slots {
            Slots::None => 0,
            Slots::Single(_) => 1,
            Slots::Record(fields) => fields.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.slots, Slots::None)
    }

    /// Every result's wire type, in the order they cross.
    pub fn types(&self) -> Vec<WireType> {
        match &self.slots {
            Slots::None => Vec::new(),
            Slots::Single(wire_type) => vec![*wire_type],
            Slots::Record(fields) => fields.iter().map(|(_, wire_type)| *wire_type).collect(),
        }
    }

    /// The shape the guest sees these results in — the one statement of the arity rule the prelude's declaration, the elaborator's and the kernel's types are all built from.
    pub fn shape(&self) -> ResultShape<'_> {
        match &self.slots {
            Slots::None => ResultShape::Unit,
            Slots::Single(wire_type) => ResultShape::Single(*wire_type),
            Slots::Record(fields) => ResultShape::Record(
                fields
                    .iter()
                    .map(|(label, wire_type)| (label.as_str(), *wire_type))
                    .collect(),
            ),
        }
    }
}

/// How a row's results reach the guest: no result is the unit value, one is the bare value forwarded through, two or more a record of the named fields — whose labels are load-bearing, since the standard library projects `.status`, `.bytes`, `.secs`, `.nanos`, … in reading a row's reply.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResultShape<'a> {
    Unit,
    Single(WireType),
    Record(Vec<(&'a str, WireType)>),
}

/// What a call of a row comes back with: the results it answers, or nothing ever. A row that answers no result still returns, and a row that does not return answers none because no call of it ends, so the two are different rows: a declaration writes `{}` for the one and `Never` for the other.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum WireReturn {
    /// The call returns, with these results.
    Answers(WireResults),
    /// The call does not return.
    Never,
}

impl WireReturn {
    /// The wire types that cross back, in order: what the row's import is typed as answering. A call that does not return crosses none.
    pub fn types(&self) -> Vec<WireType> {
        match self {
            WireReturn::Answers(results) => results.types(),
            WireReturn::Never => Vec::new(),
        }
    }

    /// How many values cross back.
    pub fn arity(&self) -> usize {
        self.types().len()
    }
}

/// The signature of one foreign function: what crosses. Each operand is its wire type, in order, and what comes back is a [`WireReturn`]. An operand has no name here: a declaration's operands are positions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct WireSignature {
    pub params: Vec<WireType>,
    pub returns: WireReturn,
}

/// The wasm import namespaces — the closed pair both ends agree on. `Ffi` holds every foreign function, under the name its declaration has, the standard library's among them; `Sys` holds [`PANIC`] alone, the one import no declaration states.
///
/// Two variants rather than a `&'static str`, so the namespaces that exist are exactly the namespaces that can be written — `README.md`'s decision.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Namespace {
    Sys,
    Ffi,
}

/// The one `sys` import, which no store row describes: the emitter's own refusal. `panic` takes a byte string and traps rather than returns, so no [`WireSignature`] describes it; no `/sys` declaration names it either, since no program can spell it — `curios-emit` calls it wherever it refuses a computation, with one constant sentence per refusal class, and both runtimes render what arrives as `panicked: …`. Only the name is wire.
pub const PANIC: &str = "panic";

/// The one export both ends link on: the entrypoint the emitter exports and the runtime looks up to run a program. It spells the entry function's own name under the compiler's naming scheme, so a module dump reads as it links — but the name is a contract stated here, not a consequence of that scheme or of the hint the entry happens to carry, so no debug name decides a wire string.
///
/// The entry takes nothing and hands back what the program's description yields, as one reference. A program's yields the unit, which no host reads. A test program's yields a `Bool`, whether every test it ran held, as the reference a `Bool` is boxed into: the i31 `0` or `1`.
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

/// One foreign function, as its `foreign` declaration states it: the fully qualified name it is declared under, which is the name it imports under in the `ffi` namespace (leading `/`), and the signature it declared. It has that one name: what a report or a dump spells it by is read off its declaration. The pair `(ffi, name)` is the wire ABI the wasm emitter and the runtime linker share. Nothing here says who answers the call — a module, an embedder or the host — which is decided where the program is linked.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ForeignFunction {
    pub name: String,
    pub signature: WireSignature,
}

// A function's identity is its import name: a [`ForeignStore`] never holds two with one name (`register` enforces it), and qualified names are unique per compilation, so the name determines the whole row. Equality and hashing stay O(1) rather than walking the signature.
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

impl ForeignFunction {
    /// The wasm import name: the declaration's fully qualified name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Whether a call never returns: the declaration states `Never`.
    pub fn diverges(&self) -> bool {
        matches!(self.signature.returns, WireReturn::Never)
    }

    /// The operands and results the declaration states.
    pub fn signature(&self) -> &WireSignature {
        &self.signature
    }
}

/// The foreign functions a unit declares, in declaration order. Rows are `Arc`ed so the IR nodes share them; cloning a store is a handful of reference bumps.
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

    /// The rows in registration order, which is the order they were declared in.
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
