//! Congruence for intrinsic operations.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived in weak-head normal form and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation. The roster has upwards of a hundred entries, and a hand-written pair match over it is a list that must be extended every time an operation is added — a list whose omissions are silent. Here the shape and the operands are both read through the traversal that already defines what an intrinsic's operands are, so a new operation is covered the moment it is representable.

use {
    super::{History, compare, ground},
    crate::{Kernel, KernelError},
    curios_core::{
        Intrinsic, Nat, Operand, Peel, Subterm, Term, Var, Visit, peel_bin, peel_list,
        peel_nat_pair,
    },
};

/// Whether `this` and `that` are the same intrinsic operation on convertible operands.
pub(super) fn convert_intrinsic(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Intrinsic,
    that: &Intrinsic,
) -> Result<bool, KernelError> {
    // **A pair of `Nat`s decides how much of itself to build.** Both sides arrived head-forced, not merged. A literal against a sum with nothing left to force clashes from the head — a stuck symbolic summand is not definitionally a literal — and that is the answer a ten-definition web used to build 1 222 222 monomials to reach. Anything else is forced to its linear combination first, and the peel below reads the pair that produced; a `Stuck` verdict then falls into the operand congruence on those normalized operands rather than back through `ground`, which would re-enter here.
    // **Two symbolic `Nat`s are distributed before they are peeled.** The fold leaves a product of two symbolic sums as a stuck node, so `(a + b) · (c + d)` and its expansion arrive as two shapes the peel cannot cancel against each other; normalizing both sides is the one demand that relates them, and it is asked for here by name. A literal on either side needs nothing: sums and differences are already merged and cancelled by the fold, so the peel decides those as it always did.
    let as_intrinsic = |term: &Term| match &**term {
        Subterm::Intrinsic(intrinsic) => Some(intrinsic.clone()),
        _ => None,
    };
    let stuck = |intrinsic: &Intrinsic| Nat::has_stuck_product(&Term::intrinsic(intrinsic.clone()));
    // A literal on either side is the peel's: sums and differences are merged and cancelled by the fold, so a side with a symbolic summand — and a stuck product is one — is never a literal, and distributing it would build the polynomial to answer what the first summand settles.
    let literal = |intrinsic: &Intrinsic| matches!(intrinsic, Intrinsic::Nat(value) if value.to_natural().is_some());
    let (this, that) = match !(literal(this) || literal(that)) && (stuck(this) || stuck(that)) {
        false => (this.clone(), that.clone()),
        true => {
            let this = Nat::normalize(kernel, Term::intrinsic(this.clone()))?;
            let that = Nat::normalize(kernel, Term::intrinsic(that.clone()))?;
            match (as_intrinsic(&this), as_intrinsic(&that)) {
                (Some(this), Some(that)) => (this, that),
                _ => return ground(kernel, history, &this, &that),
            }
        }
    };
    let (this, that) = (&this, &that);
    // `Nat`, `Bin`, and `List` are free monoids, so two values of one are equal exactly when they agree after their longest common prefix is peeled off. This is shared spine algebra over the representation, not a rule: it decides `x + 2 ≡ y + 2` by comparing `x` with `y` rather than by comparing two opaque literals. `Stuck` falls through to the congruence below, which still compares like-shaped symbolic operands, so the peel can only ever strengthen conversion.
    if let Some(peel) = peel_nat_pair(this, that)
        .or_else(|| peel_bin(this, that))
        .or_else(|| peel_list(this, that))
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

    // Each operand compares at the type its operation declares for it, not at a flat one — the same discipline `convert`'s variant, struct and tuple comparisons already follow, and the reason it matters here is the bounds: a proof compares at its *proposition*, where definitional proof irrelevance discharges the goal without looking at either side. Two differently-derived proofs of one bound are therefore convertible, rather than convertible only when both happen to be the canonical inhabitant.
    //
    // Reading the demand off `Intrinsic::signature` is what keeps that from being a rule about bounds. There is no proof-shaped case here; irrelevance fires through the ordinary gate because the goal carries a proposition, exactly as it would for any other operand whose type is one.
    let signature = this.signature(&kernel.syntax());

    for (index, (left, right)) in this_operands.iter().zip(&that_operands).enumerate() {
        let converted = match signature.operands.get(index) {
            Some(Operand::At(type_)) => compare(kernel, history, type_, left, right)?,
            // A type operand and a function operand keep the flat comparison: the first is compared *as* a type, and the second would need a binder minted here to state its own.
            _ => ground(kernel, history, left, right)?,
        };

        if !converted {
            return Ok(false);
        }
    }

    Ok(true)
}

/// Split an intrinsic into its shape — itself, with every term operand stood down to one placeholder — and those operands in traversal order.
///
/// Both halves come from `Intrinsic::traverse`, which is the single definition of what an intrinsic's term operands are. Nothing here enumerates operations, so nothing here can forget one.
fn decompose(intrinsic: &Intrinsic) -> (Intrinsic, Vec<Term>) {
    let mut visit = Visit::masking(|_, _: &Var| None, Term::type_ground());
    let shape = intrinsic.traverse(&mut visit);

    (shape, visit.take_masked_children())
}
