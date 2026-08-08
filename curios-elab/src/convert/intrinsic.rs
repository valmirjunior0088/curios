//! Congruence for intrinsic operations.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived reduced and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation, which is also how `curios-cert` states it. A hand-written pair match over a roster of upwards of a hundred entries is a list that must be extended every time an operation is added, and its omissions are silent: `convert` short-circuits on syntactic identity before reaching here, so a missing arm only surfaces on two spellings that are convertible without being identical, as a *hard mismatch* rather than a postponement. `Bool`, `BoolType`, and `LstMap` had gone missing that way. Here the shape and the operands are both read through the traversal that already defines what an intrinsic's operands are, so a new operation is covered the moment it is representable — and covered identically on both sides of the checker seam.

use {
    super::Convert,
    curios_core::{Intrinsic, Peel, ReduceError, Term, Var, Visit, peel_bin, peel_lst, peel_nat},
};

pub(crate) fn convert_intrinsic(
    cmp: &mut Convert,
    this: Intrinsic,
    that: Intrinsic,
) -> Result<bool, ReduceError> {
    // `Nat`, `Bin`, and `Lst` are free monoids, so two values of one are equal exactly when they agree after their longest common prefix is peeled off (`core::spine`). This is shared spine algebra over the representation, not a rule: it decides `x + 2 ≡ y + 2` by comparing `x` with `y` rather than by comparing two opaque literals. `Stuck` falls through to the congruence below, which still compares like-shaped symbolic operands, so the peel can only ever strengthen conversion.
    if let Some(peel) = peel_nat_pair(&this, &that)
        .or_else(|| peel_bin(&this, &that))
        .or_else(|| peel_lst(&this, &that))
    {
        match peel {
            Peel::Equal => return Ok(true),
            Peel::Clash => return Ok(false),
            Peel::Continue(left, right) => {
                cmp.enqueue(Term::type_ground(), left, right);
                return Ok(true);
            }
            Peel::Stuck => {}
        }
    }

    let (this_shape, this_operands) = decompose(&this);
    let (that_shape, that_operands) = decompose(&that);

    // The shapes carry everything that is *not* a term: which operation, which grain, which literal, which successor floor, which foreign row. Comparing them settles the whole of the operation's identity in one derived equality.
    if this_shape != that_shape || this_operands.len() != that_operands.len() {
        return Ok(false);
    }

    for (left, right) in this_operands.into_iter().zip(that_operands) {
        cmp.enqueue(Term::type_ground(), left, right);
    }

    Ok(true)
}

/// The free-monoid peel for two `Nat`s, which unlike `Bin`/`Lst` is spelled against the carrier rather than the intrinsic.
fn peel_nat_pair(this: &Intrinsic, that: &Intrinsic) -> Option<Peel> {
    match (this, that) {
        (Intrinsic::Nat(left), Intrinsic::Nat(right)) => Some(peel_nat(left, right)),
        _ => None,
    }
}

/// Split an intrinsic into its shape — itself, with every term operand stood down to one placeholder — and those operands in traversal order.
///
/// Both halves come from `Intrinsic::traverse`, which is the single definition of what an intrinsic's term operands are. Nothing here enumerates operations, so nothing here can forget one.
fn decompose(intrinsic: &Intrinsic) -> (Intrinsic, Vec<Term>) {
    let mut visit = Visit::masking(|_, _: &Var| None, Term::type_ground());
    let shape = intrinsic.traverse(&mut visit);

    (shape, visit.take_masked_children())
}
