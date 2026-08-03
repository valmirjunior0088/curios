//! Congruence for primitive operations.
//!
//! Two primitives are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived in weak-head normal form and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation. The roster has upwards of a hundred entries, and a hand-written pair match over it is a list that must be extended every time an operation is added — a list whose omissions are silent. Here the shape and the operands are both read through the traversal that already defines what a primitive's operands are, so a new operation is covered the moment it is representable.

use {
    super::{History, ground},
    crate::{Kernel, KernelError},
    curios_core::{Peel, Prim, Term, Var, Visit, peel_bin, peel_lst, peel_nat},
};

/// Whether `this` and `that` are the same primitive operation on convertible operands.
pub(super) fn convert_prim(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Prim,
    that: &Prim,
) -> Result<bool, KernelError> {
    // `Nat`, `Bin`, and `Lst` are free monoids, so two values of one are equal exactly when they agree after their longest common prefix is peeled off. This is shared spine algebra over the representation, not a rule: it decides `x + 2 ≡ y + 2` by comparing `x` with `y` rather than by comparing two opaque literals. `Stuck` falls through to the congruence below, which still compares like-shaped symbolic operands, so the peel can only ever strengthen conversion.
    if let Some(peel) = peel_nat_pair(this, that)
        .or_else(|| peel_bin(this, that))
        .or_else(|| peel_lst(this, that))
    {
        match peel {
            Peel::Equal => return Ok(true),
            Peel::Clash => return Ok(false),
            Peel::Continue(left, right) => return ground(kernel, history, &left, &right),
            Peel::Stuck => {}
        }
    }

    let (this_shape, this_operands) = decompose(this);
    let (that_shape, that_operands) = decompose(that);

    // The shapes carry everything that is *not* a term: which operation, which grain, which literal, which successor floor. Comparing them settles the whole of the operation's identity in one derived equality.
    if this_shape != that_shape || this_operands.len() != that_operands.len() {
        return Ok(false);
    }

    for (left, right) in this_operands.iter().zip(&that_operands) {
        if !ground(kernel, history, left, right)? {
            return Ok(false);
        }
    }

    Ok(true)
}

/// The free-monoid peel for two `Nat`s, which unlike `Bin`/`Lst` is spelled against the carrier rather than the primitive.
fn peel_nat_pair(this: &Prim, that: &Prim) -> Option<Peel> {
    match (this, that) {
        (Prim::Nat(left), Prim::Nat(right)) => Some(peel_nat(left, right)),
        _ => None,
    }
}

/// Split a primitive into its shape — itself, with every term operand stood down to one placeholder — and those operands in traversal order.
///
/// Both halves come from `Prim::traverse`, which is the single definition of what a primitive's term operands are. Nothing here enumerates operations, so nothing here can forget one.
fn decompose(prim: &Prim) -> (Prim, Vec<Term>) {
    let mut visit = Visit::masking(|_, _: &Var| None, Term::type_ground());
    let shape = prim.traverse(&mut visit);

    (shape, visit.take_masked_children())
}
