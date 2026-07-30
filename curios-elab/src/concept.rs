//! Witness registry entries and the keys resolution looks them up by.
//!
//! The [`Concept`](curios_core::Concept) entry itself is representation and lives in `curios-core` beside the other registry entries; what stays here is the instance-argument machinery: a [`Witness`] keys an ordinary top-level definition in the program-wide table under `(concept name, tuple of parameter heads)` — the [`WitnessKey`] of [`HeadKey`]s — and resolution searches that table.

#[cfg(test)]
mod tests;

use {
    curios_base::{Grain, Qualifier, RootId},
    curios_core::{Global, Prim, Subterm, Telescope, Term, UniverseContext},
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
    /// The compilation root that declares this witness — consulted by the orphan-rule ownership check alongside `Concept::root` and the key's head roots. Derived from `Context::island()` at registration, the same source `module_of` reads for the (unrelated) representation-privacy check.
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

/// One rigid head inside a [`WitnessKey`]: the nominal (inductive or struct) qualified name, or a primitive type constructor. Parameters past the heads are checked by unification at resolution time, not by the key.
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
}

impl HeadKey {
    /// The key of a term already in weak-head normal form, if its head is rigid and nominal/primitive. A `Func` head is the higher-kinded case (a type constructor like `Option` reduces to `λA. Option-normal-form`): its *body* supplies the key, so `Monad(Option)` keys on `Option`. `None` for anything else — variables, metavariables, Π/Σ types, `Type`/`Prop` — which are not keyable.
    pub(crate) fn of_whnf(term: &Term) -> Option<HeadKey> {
        match &**term {
            Subterm::InductType(induct_decl) => Some(HeadKey::Nominal(induct_decl.name.clone())),
            Subterm::StructType(struct_decl) => Some(HeadKey::Nominal(struct_decl.name.clone())),
            Subterm::Prim(prim) => Self::of_prim(prim),
            // The higher-kinded head: the type-constructor function's body is the normal form the applied constructor would reduce to (`λA. InductType(Option, [A])`, or `λT. LstType(T)` for a primitive former like `/sys/Lst`). The binders need not be opened — the name/former sits on the node.
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
                    Subterm::Prim(prim) => Self::of_prim(prim),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// The key of a primitive type former, shared by the first-order and higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf).
    fn of_prim(prim: &Prim) -> Option<HeadKey> {
        use curios_core::Prim;
        match prim {
            Prim::NatType => Some(HeadKey::Nat),
            Prim::ByteType => Some(HeadKey::Byte),
            Prim::IntType => Some(HeadKey::Int),
            Prim::FltType => Some(HeadKey::Flt),
            Prim::BoolType => Some(HeadKey::Bool),
            Prim::BinType(grain) => Some(HeadKey::Bin(*grain)),
            Prim::HandleType => Some(HeadKey::Handle),
            Prim::LstType(_) => Some(HeadKey::Lst),
            Prim::CellType(_) => Some(HeadKey::Cell),
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
        }
    }
}
