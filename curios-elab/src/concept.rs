//! Witness registry entries and the keys resolution looks them up by.
//!
//! The [`ConceptDecl`](curios_core::ConceptDecl) entry itself is representation and lives in `curios-core` beside the other registry entries; what stays here is the instance-argument machinery: a [`Witness`] keys an ordinary top-level definition in the program-wide table under `(concept name, tuple of parameter heads)` — the [`WitnessKey`] of [`HeadKey`]s — and resolution searches that table.

#[cfg(test)]
mod tests;

use {
    curios_core::{
        Free, FuncType, Global, Intrinsic, Subterm, Telescope, Term, TupleType, UniverseContext,
    },
    curios_utilities::{Grain, Plicity, Qualifier},
    std::fmt,
};

/// One registered witness: the qualified name of its backing definition and that definition's elaborated type `∀ tele. C(t₁, …)`. Resolution instantiates the telescope fresh at every use.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub(crate) struct Witness {
    pub name: Global,
    /// The module this witness was declared in, carried from its definition's `island`. Witnesses are anonymous, so this — not `name` — is the coordinate a coherence diagnostic reports; recovering it by splitting the compiler-minted `name` would re-derive what the declaration already knew.
    pub module: Qualifier,
    pub universe_context: UniverseContext,
    pub signature: Term,
}

/// The tuple of rigid heads a witness is keyed on: one [`HeadKey`] per concept parameter, in declaration order. Displays bare for arity one (`Nat`) and as a tuple otherwise (`(Nat, Str)`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[curios_archive::archived]
pub struct WitnessKey(pub Vec<HeadKey>);

impl fmt::Display for WitnessKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.as_slice() {
            [single] => write!(f, "{single}"),
            heads => {
                write!(f, "(")?;
                for (index, head) in heads.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{head}")?;
                }
                write!(f, ")")
            }
        }
    }
}

/// One rigid head inside a [`WitnessKey`]: the nominal (inductive or struct) qualified name, an intrinsic type constructor, an anonymous product's shape, or a function type's plicity vector. Parameters past the heads are checked by unification at resolution time, not by the key.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[curios_archive::archived]
pub enum HeadKey {
    Nominal(Global),
    Nat,
    Byte,
    Int,
    Flt,
    Bool,
    Bin(Grain),
    Handle,
    List,
    Cell,
    Io,
    /// A tuple type, keyed by its *shape*: the label at each field position, `""` where the field is positional, arity implied by the length. A tuple type has no name to be headed by, so the shape plays the role a nominal name plays — it is precisely the half of the type's identity that conversion does not delegate to the fields (`compare_tuple_type` refuses differing labels before enqueuing one), so keying here splits the type along the seam conversion already splits it on. `{Nat, Bool}` and `{x: Nat, y: Bool}` are two keys because they are two types.
    TupleType(Vec<String>),
    /// A function type, keyed by its *plicity vector*: the mark at each parameter position, arity implied by the length, domains and result excluded. The marks are the non-subterm half of a function type's identity, exactly as a tuple's labels are (`compare_func_type` refuses differing vectors before enqueuing one domain), so `(A) -> B` and `(@A: T) -> B` are two keys because they are two types — while `(a: Nat) -> Nat` and `(b: Nat) -> Nat` are one, because binder names feed freshness, never equality: the opposite of tuple labels, and the same rule seen from the other side, since the key is whatever half of identity conversion keeps for itself.
    FuncType(Vec<Plicity>),
}

impl HeadKey {
    /// The key of a term already in weak-head normal form, if its head is rigid and nominal/intrinsic/tuple/function. A `Func` head is the higher-kinded case (a type constructor like `Option` reduces to `λA. Option-normal-form`): its *body* supplies the key, so `Monad(Option)` keys on `Option`. `None` for anything else — variables, metavariables, `Type`/`Prop` — which are not keyable.
    pub(crate) fn of_whnf(term: &Term) -> Option<HeadKey> {
        match &**term {
            Subterm::InductType(induct_decl) => Some(HeadKey::Nominal(induct_decl.name.clone())),
            Subterm::StructType(struct_decl) => Some(HeadKey::Nominal(struct_decl.name.clone())),
            Subterm::Intrinsic(intrinsic) => Self::of_intrinsic(intrinsic),
            // The weak-head form of a tuple type is the node itself and its labels are structural, so keying costs a walk down the spine and reduces no field type.
            Subterm::TupleType(tuple_type) => Some(Self::of_tuple_type(tuple_type)),
            // The weak-head form of a function type is likewise the node itself and its marks sit on it, so keying costs a copy of the vector and no evaluation.
            Subterm::FuncType(func_type) => Some(Self::of_func_type(func_type)),
            // The higher-kinded head: the type-constructor function's body is the normal form the applied constructor would reduce to (`λA. InductType(Option, [A])`, or `λT. ListType(T)` for an intrinsic former like `/sys/List`). The binders need not be opened — the name/former sits on the node.
            Subterm::Func(func) => {
                let mut telescope = &func.telescope;
                while let Telescope::Cons(_, rest) = telescope {
                    telescope = rest.body();
                }
                let Telescope::Done(body) = telescope else {
                    unreachable!("telescope spine ends in Done");
                };
                match &***body {
                    Subterm::InductType(induct_decl) => {
                        Some(HeadKey::Nominal(induct_decl.name.clone()))
                    }
                    Subterm::StructType(struct_decl) => {
                        Some(HeadKey::Nominal(struct_decl.name.clone()))
                    }
                    Subterm::Intrinsic(intrinsic) => Self::of_intrinsic(intrinsic),
                    // A constructor whose body is an anonymous product — `let Pair(A: Type) -> Type = {Nat, A};` reduces to `(A: Type) => {Nat, A}` — keys on that body's shape, so `Functor(Pair)` registers where `Monad(Option)` does. Symmetry with the nominal case, not a consumer's demand; it does not extend imitation, since `?M(?A) ≡ {Nat, Nat}` has no unique solution.
                    Subterm::TupleType(tuple_type) => Some(Self::of_tuple_type(tuple_type)),
                    // A constructor whose body is a function type — `let Reader(A: Type) -> Type = (Nat) -> A;` reduces to `(A: Type) => (Nat) -> A` — keys on that body's plicity vector, by the same symmetry. Imitation is likewise not extended: `?M(?A) ≡ (Nat) -> Nat` has no unique solution, so a goal reaches such a witness only where it spells the constructor.
                    Subterm::FuncType(func_type) => Some(Self::of_func_type(func_type)),
                    // A *partially applied* family: `(A : Type) => State(S, A)` leaves the body a stuck application under the binder, since weak-head reduction never descends into a `Func`. Its head names the former — a registry entry and its type-former definition share one finalized context, so the reference's global *is* the declaration's key — and the universes riding an `Instance` wrapper are irrelevant to keying, which reads names alone. Arguments below the head stay unification's job at resolution time, exactly as for a saturated node.
                    Subterm::Apply(apply) => {
                        let name = match &*apply.head {
                            Subterm::Instance(instance) => instance.head.head_name(),
                            Subterm::Var(var) => var.as_free(),
                            _ => None,
                        };
                        match name? {
                            Free::Global(global) => Some(HeadKey::Nominal(global.clone())),
                            Free::Local(_) => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// The key of a tuple type, shared by the first-order and higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf). `labels` walks the spine without opening a binder, which is the same read `TupleType`'s own `Eq` performs.
    fn of_tuple_type(tuple_type: &TupleType) -> HeadKey {
        HeadKey::TupleType(
            tuple_type
                .telescope
                .labels()
                .into_iter()
                .map(str::to_owned)
                .collect(),
        )
    }

    /// The key of a function type, shared by the first-order and higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf). The marks sit on the node, so the read opens no binder and evaluates nothing.
    fn of_func_type(func_type: &FuncType) -> HeadKey {
        HeadKey::FuncType(func_type.plicities().to_vec())
    }

    /// The key of an intrinsic type former, shared by the first-order and higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf).
    fn of_intrinsic(intrinsic: &Intrinsic) -> Option<HeadKey> {
        match intrinsic {
            Intrinsic::NatType => Some(HeadKey::Nat),
            Intrinsic::ByteType => Some(HeadKey::Byte),
            Intrinsic::IntType => Some(HeadKey::Int),
            Intrinsic::FltType => Some(HeadKey::Flt),
            Intrinsic::BoolType => Some(HeadKey::Bool),
            Intrinsic::BinType(grain) => Some(HeadKey::Bin(*grain)),
            Intrinsic::HandleType => Some(HeadKey::Handle),
            Intrinsic::ListType(_) => Some(HeadKey::List),
            Intrinsic::CellType(_) => Some(HeadKey::Cell),
            Intrinsic::IoType(_) => Some(HeadKey::Io),
            _ => None,
        }
    }
}

impl fmt::Display for HeadKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HeadKey::Nominal(name) => write!(f, "{name}"),
            HeadKey::Nat => write!(f, "Nat"),
            HeadKey::Byte => write!(f, "Byte"),
            HeadKey::Int => write!(f, "Int"),
            HeadKey::Flt => write!(f, "Flt"),
            HeadKey::Bool => write!(f, "Bool"),
            HeadKey::Bin(Grain::B) => write!(f, "Bits"),
            HeadKey::Bin(Grain::X) => write!(f, "Bytes"),
            HeadKey::Handle => write!(f, "Handle"),
            HeadKey::List => write!(f, "List"),
            HeadKey::Cell => write!(f, "Cell"),
            HeadKey::Io => write!(f, "Io"),
            // A shape displays as the type it stands for with every field type elided: `{}`, `{_, _}`, `{x: _, y: _}`. The field types are not in the key, so there is nothing truthful to print in their place.
            HeadKey::TupleType(labels) => {
                write!(f, "{{")?;
                for (index, label) in labels.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    match label.is_empty() {
                        true => write!(f, "_")?,
                        false => write!(f, "{label}: _")?,
                    }
                }
                write!(f, "}}")
            }
            // A key displays as the type it stands for with everything but the marks elided: `() -> _`, `(_) -> _`, `(@_, use _, _) -> _`. The domains and result are not in the key, so there is nothing truthful to print in their place.
            HeadKey::FuncType(plicities) => {
                write!(f, "(")?;
                for (index, plicity) in plicities.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    match plicity {
                        Plicity::Explicit => write!(f, "_")?,
                        Plicity::Implicit => write!(f, "@_")?,
                        Plicity::Witness => write!(f, "use _")?,
                    }
                }
                write!(f, ") -> _")
            }
        }
    }
}
