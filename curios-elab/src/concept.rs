//! Witness registry entries and the keys resolution looks them up by.
//!
//! The [`ConceptDecl`](curios_core::ConceptDecl) entry itself is representation and lives in `curios-core` beside the other registry entries; what stays here is the instance-argument machinery: a [`Witness`] keys an ordinary top-level definition in the program-wide table under `(concept name, tuple of parameter heads)` — the [`WitnessKey`] of [`HeadKey`]s — and resolution searches that table.

#[cfg(test)]
mod tests;

use {
    curios_base::{Grain, Qualifier, RootId},
    curios_core::{Free, Global, Intrinsic, Subterm, Telescope, Term, UniverseContext},
    std::fmt,
};

/// One registered witness: the qualified name of its backing definition and that definition's elaborated type `∀ tele. C(t₁, …)`. Resolution instantiates the telescope fresh at every use.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(crate) struct Witness {
    pub name: Global,
    /// The module this witness was declared in, carried from its definition's `island`. Witnesses are anonymous, so this — not `name` — is the coordinate a coherence diagnostic reports; recovering it by splitting the compiler-minted `name` would re-derive what the declaration already knew.
    pub module: Qualifier,
    pub universe_context: UniverseContext,
    pub signature: Term,
    /// The compilation root that declares this witness — consulted by the orphan-rule ownership check alongside `ConceptDecl::root` and the key's head roots. Derived from `Context::island()` at registration, the same source `module_of` reads for the (unrelated) representation-privacy check.
    pub root: RootId,
}

/// The tuple of rigid heads a witness is keyed on: one [`HeadKey`] per concept parameter, in declaration order. Displays bare for arity one (`Nat`) and as a tuple otherwise (`(Nat, Str)`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct WitnessKey(pub Vec<HeadKey>);

impl std::fmt::Display for WitnessKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

/// One rigid head inside a [`WitnessKey`]: the nominal (inductive or struct) qualified name, or an intrinsic type constructor. Parameters past the heads are checked by unification at resolution time, not by the key.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum HeadKey {
    Nominal(Global),
    Nat,
    Byte,
    Int,
    Flt,
    Bool,
    Bin(Grain),
    Handle,
    Lst,
    Cell,
    Io,
}

impl HeadKey {
    /// The key of a term already in weak-head normal form, if its head is rigid and nominal/intrinsic. A `Func` head is the higher-kinded case (a type constructor like `Option` reduces to `λA. Option-normal-form`): its *body* supplies the key, so `Monad(Option)` keys on `Option`. `None` for anything else — variables, metavariables, Π/Σ types, `Type`/`Prop` — which are not keyable.
    pub(crate) fn of_whnf(term: &Term) -> Option<HeadKey> {
        match &**term {
            Subterm::InductType(induct_decl) => Some(HeadKey::Nominal(induct_decl.name.clone())),
            Subterm::StructType(struct_decl) => Some(HeadKey::Nominal(struct_decl.name.clone())),
            Subterm::Intrinsic(intrinsic) => Self::of_intrinsic(intrinsic),
            // The higher-kinded head: the type-constructor function's body is the normal form the applied constructor would reduce to (`λA. InductType(Option, [A])`, or `λT. LstType(T)` for an intrinsic former like `/sys/Lst`). The binders need not be opened — the name/former sits on the node.
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
                    // A *partially applied* family: `(A : Type) => State(S, A)` leaves the body a stuck application under the binder, since weak-head reduction never descends into a `Func`. Its head names the former — a registry entry and its type-former definition share one finalized context, so the reference's global *is* the declaration's key — and the universes riding a `UniverseInst` wrapper are irrelevant to keying, which reads names alone. Arguments below the head stay unification's job at resolution time, exactly as for a saturated node.
                    Subterm::Apply(apply) => {
                        let head = match &*apply.head {
                            Subterm::UniverseInst(instance) => &instance.head,
                            _ => &apply.head,
                        };
                        match &**head {
                            Subterm::Var(var) => match var.as_free()? {
                                Free::Global(global) => Some(HeadKey::Nominal(global.clone())),
                                Free::Local(_) => None,
                            },
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// The key of an intrinsic type former, shared by the first-order and higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf).
    fn of_intrinsic(intrinsic: &Intrinsic) -> Option<HeadKey> {
        use curios_core::Intrinsic;
        match intrinsic {
            Intrinsic::NatType => Some(HeadKey::Nat),
            Intrinsic::ByteType => Some(HeadKey::Byte),
            Intrinsic::IntType => Some(HeadKey::Int),
            Intrinsic::FltType => Some(HeadKey::Flt),
            Intrinsic::BoolType => Some(HeadKey::Bool),
            Intrinsic::BinType(grain) => Some(HeadKey::Bin(*grain)),
            Intrinsic::HandleType => Some(HeadKey::Handle),
            Intrinsic::LstType(_) => Some(HeadKey::Lst),
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
            HeadKey::Lst => write!(f, "Lst"),
            HeadKey::Cell => write!(f, "Cell"),
            HeadKey::Io => write!(f, "Io"),
        }
    }
}
