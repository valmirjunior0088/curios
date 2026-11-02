//! The eraser: a certified Core unit erased into the erased representation through the checked builder. Its entry points are `unit`'s and the expression walk is `walk`'s; every other child is an `impl Eraser` block erasing one family of forms. The `Eraser` itself is declared here, where every child reads its private state.

use {
    crate::{AtSorts, Environment, Sorts},
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

mod walk;
use walk::*;
pub use walk::{Declared, ErasedNames, Outcome};

mod unit;
pub use unit::{ErasedProgram, erase_program, erase_unit, erase_unit_over};

mod binding;

mod function;

mod aggregate;

mod eliminate;

mod recursion;

mod intrinsic;
use intrinsic::*;

mod suspension;
use suspension::*;

mod crossing;

mod typed;

#[cfg(test)]
pub mod test_support;

/// One item's erasure state: the checked builder constructing its fragment and the environment mapping Core names to their operands there.
pub struct Eraser<'s> {
    builder: curios_anf::Builder,
    environment: Environment,
    /// The erased forms of the items this one may name.
    externals: Externals<'s>,
    /// The `let` definitions this one may name, as their items declare them.
    declared: Declared<'s>,
    /// The names the item declares, and the sorts this form of it is read at: an occurrence of one at those sorts is the item's own, bound in this fragment at none.
    own: (Vec<curios_core::Global>, Sorts),
    /// The name and sorts each value the fragment imports stands for, in the order the builder minted them.
    values: Vec<AtSorts>,
    /// The name and sorts each function the fragment imports stands for, in the order the builder minted them.
    functions: Vec<AtSorts>,
    /// The definition each emitted function descends from, innermost last, paired with how many anonymous ones have been minted under it. See [`Eraser::derived_hint`].
    owners: Vec<(String, usize)>,
    /// The family identity of each inductive whose row is being registered right now. An inductive is legitimately recursive, so classifying its fields reaches itself; the identity is minted before that walk begins, so the recursive lookup answers from here rather than re-entering the registration.
    pending_families: BTreeMap<AtSorts, curios_anf::FamilyId>,
    /// The structures whose row is being registered right now. A self-referential structure is uninhabited but elaborates, and unlike an inductive its schema is only decided *after* its fields are classified — so the cycle is cut by declining to name a schema rather than by naming one early.
    in_flight: BTreeSet<AtSorts>,
    /// The type each term was read at ([`Eraser::type_of`]), for the item being erased and the arm being walked. Dropped at every top-level item and before the entry, which is what bounds it, and set aside for each arm.
    types: HashMap<curios_core::Term, curios_core::Term>,
    /// The extent of each computed recursive member's initializer, by the identity it was minted as. The erased verifier refuses a member by that identity after the module holding it is gone, so the frame for the refusal is kept here rather than in the representation, whose nodes are the stored-unit format and carry no extents.
    extents: BTreeMap<curios_anf::ValueId, curios_core::Extent>,
    /// The name each member of a group and each import was minted for, for the same refusal: it knows identities alone.
    names: ErasedNames,
}
