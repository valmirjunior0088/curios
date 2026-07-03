//! Registry entries for concepts (record-shaped interfaces) and witnesses
//! (their registered inhabitants) — the instance-argument machinery's flat
//! stores, carried on the [`Module`](super::Module) and mirrored into the
//! [`Context`](super::Context) exactly like `inductives`/`structures`.
//!
//! A concept lowers to an ordinary nominal record (its [`Structure`]
//! (super::Structure) entry drives literals and projections); the [`Concept`]
//! entry here adds what resolution needs on top: the field labels, the
//! superclass mask, the parameter telescope, and the input mask. A witness
//! lowers to an ordinary top-level definition; its [`Witness`] entry keys that
//! definition in the program-wide table under `(concept name, tuple of the
//! rigid heads of the input parameters)`, the [`WitnessKey`] of [`HeadKey`]s.

use super::{Subterm, Telescope, Term};

/// One concept declaration's registry entry.
#[derive(Debug, Clone, PartialEq)]
pub struct Concept {
    /// The declaration's parameter telescope, e.g. `(A : Type)` for
    /// `concept Show(A : Type)`. Ends in `()` like a `Structure`'s.
    pub params: Telescope<()>,
    /// Field labels in declaration order — the positions witness struct
    /// literals fill and method wrappers project.
    pub fields: Vec<String>,
    /// Superclass edges: `(field position, super concept qualified name)` for
    /// each `use`-marked field. The graph over all concepts must be acyclic
    /// (checked when the registries are seeded).
    pub supers: Vec<(usize, String)>,
    /// The parameter positions witnesses key on, in declaration order — every
    /// position not marked `out` in the declaration. A concept without `out`
    /// markers has `inputs = [0, 1, …, n-1]`; `out` positions are excluded
    /// from the key and pinned by the witness's terminal unification instead
    /// (functional dependencies).
    pub inputs: Vec<usize>,
}

/// One registered witness: the qualified name of its backing definition and
/// that definition's elaborated type `∀ tele. C(t₁, …)`. Resolution
/// instantiates the telescope fresh at every use.
#[derive(Debug, Clone, PartialEq)]
pub struct Witness {
    pub name: String,
    pub signature: Term,
}

/// The tuple of rigid heads a witness is keyed on: one [`HeadKey`] per input
/// position of its concept, in declaration order. Displays bare for arity one
/// (`Nat`) and as a tuple otherwise (`(Nat, Str)`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

/// One rigid head inside a [`WitnessKey`]: the nominal (inductive or struct)
/// qualified name, or a primitive type constructor. Parameters past the heads
/// are checked by unification at resolution time, not by the key.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum HeadKey {
    Nominal(String),
    Nat,
    Int,
    Flt,
    Bln,
    Bin,
    Io,
    Arr,
    Cell,
}

impl std::fmt::Display for HeadKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeadKey::Nominal(name) => write!(f, "{name}"),
            HeadKey::Nat => write!(f, "Nat"),
            HeadKey::Int => write!(f, "Int"),
            HeadKey::Flt => write!(f, "Flt"),
            HeadKey::Bln => write!(f, "Bln"),
            HeadKey::Bin => write!(f, "Bin"),
            HeadKey::Io => write!(f, "Io"),
            HeadKey::Arr => write!(f, "Arr"),
            HeadKey::Cell => write!(f, "Cell"),
        }
    }
}

impl HeadKey {
    /// The key of a term already in weak-head normal form, if its head is
    /// rigid and nominal/primitive. A `Func` head is the higher-kinded case (a
    /// type constructor like `Option` reduces to `λA. Option-normal-form`):
    /// its *body* supplies the key, so `Monad(Option)` keys on `Option`.
    /// `None` for anything else — variables, metavariables, Π/Σ types,
    /// `Type`/`Prop` — which are not keyable.
    pub fn of_whnf(term: &Term) -> Option<HeadKey> {
        match &**term {
            Subterm::InductiveType(inductive) => Some(HeadKey::Nominal(inductive.name.clone())),
            Subterm::StructType(structure) => Some(HeadKey::Nominal(structure.name.clone())),
            Subterm::Prim(prim) => Self::of_prim(prim),
            // The higher-kinded head: the type-constructor function's body is
            // the normal form the applied constructor would reduce to (`λA.
            // InductiveType(Option, [A])`, or `λT. ArrType(T)` for a primitive
            // former like `/sys/Arr`). The binders need not be opened — the
            // name/former sits on the node.
            Subterm::Func(func) => {
                let mut telescope = &func.telescope;
                while let Telescope::Cons(_, rest) = telescope {
                    telescope = rest.body();
                }
                let Telescope::Done(body) = telescope else {
                    unreachable!("telescope spine ends in Done");
                };
                match &***body {
                    Subterm::InductiveType(inductive) => {
                        Some(HeadKey::Nominal(inductive.name.clone()))
                    }
                    Subterm::StructType(structure) => {
                        Some(HeadKey::Nominal(structure.name.clone()))
                    }
                    Subterm::Prim(prim) => Self::of_prim(prim),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// The key of a primitive type former, shared by the first-order and
    /// higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf).
    fn of_prim(prim: &super::Prim) -> Option<HeadKey> {
        use super::Prim;
        match prim {
            Prim::NatType => Some(HeadKey::Nat),
            Prim::IntType => Some(HeadKey::Int),
            Prim::FltType => Some(HeadKey::Flt),
            Prim::BlnType => Some(HeadKey::Bln),
            Prim::BinType => Some(HeadKey::Bin),
            Prim::IoType => Some(HeadKey::Io),
            Prim::ArrType(_) => Some(HeadKey::Arr),
            Prim::CellType(_) => Some(HeadKey::Cell),
            _ => None,
        }
    }
}
