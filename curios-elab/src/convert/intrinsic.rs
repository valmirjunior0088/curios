//! Congruence for intrinsic operations.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived reduced and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation, which is also how `curios-cert` states it. A hand-written pair match over a roster of upwards of a hundred entries is a list that must be extended every time an operation is added, and its omissions are silent: `convert` short-circuits on syntactic identity before reaching here, so a missing arm only surfaces on two spellings that are convertible without being identical, as a *hard mismatch* rather than a postponement. `Bool`, `BoolType`, and `ListMap` had gone missing that way. Here the shape and the operands are both read through the traversal that already defines what an intrinsic's operands are, so a new operation is covered the moment it is representable — and covered identically on both sides of the checker seam.

use {
    super::Convert,
    curios_core::{
        Intrinsic, Peel, ReduceError, Subterm, Term, Var, Visit, peel_bin, peel_list, peel_nat,
    },
    curios_utilities::{Grain, PackedBin},
};

pub(crate) fn convert_intrinsic(
    cmp: &mut Convert,
    this: Intrinsic,
    that: Intrinsic,
) -> Result<bool, ReduceError> {
    // `Nat`, `Bin`, and `List` are free monoids, so two values of one are equal exactly when they agree after their longest common prefix is peeled off (`core::spine`). This is shared spine algebra over the representation, not a rule: it decides `x + 2 ≡ y + 2` by comparing `x` with `y` rather than by comparing two opaque literals. `Stuck` falls through to the congruence below, which still compares like-shaped symbolic operands, so the peel can only ever strengthen conversion.
    if let Some(peel) = peel_nat_pair(&this, &that)
        .or_else(|| peel_bin(&this, &that))
        .or_else(|| peel_list(&this, &that))
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

    // The packed-literal view: a `Bin` literal opposite an `append`/`concat` spine decomposes length-directedly, undoing exactly the constant folding that removed the spine spelling from the literal side (`append(b[], true)` folds to `b[1]`, and no shape congruence can relate the folded form to `append(b[], ?h)`). Solving-side only: the reducer's laws are untouched, and once the minted goals commit their solutions the folded spellings agree by plain reduction, so the kernel needs no matching rule.
    if let Some(view) = packed_literal_view(cmp, &this, &that) {
        return Ok(view);
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

/// Decompose a nonempty (or empty) packed literal against an `append`/`concat` spine of the same grain, when the spine's segment lengths determine the split. `None` when the pair is not literal-versus-spine or a middle segment's length is unknown — the caller's congruence (and the drain's metavariable parking) keep their own handling. `Some(false)` is a definite structural length clash: segment lengths are fixed by the spine's shape, so no metavariable solution can repair them.
fn packed_literal_view(cmp: &mut Convert, this: &Intrinsic, that: &Intrinsic) -> Option<bool> {
    match (literal_of(this), literal_of(that)) {
        (Some((grain, lit)), None) => split_against(cmp, grain, lit, that),
        (None, Some((grain, lit))) => split_against(cmp, grain, lit, this),
        _ => None,
    }
}

fn literal_of(intrinsic: &Intrinsic) -> Option<(Grain, &PackedBin)> {
    match intrinsic {
        Intrinsic::Bin(grain, value) => Some((*grain, value)),
        _ => None,
    }
}

/// The number of atoms `intrinsic` denotes when its shape determines it: a literal's stored length, an `append`'s base plus one, a `concat`'s segment sum. `None` for anything symbolic-length (a variable, a slice, a metavariable).
fn known_len(grain: Grain, term: &Term) -> Option<usize> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };
    match intrinsic {
        Intrinsic::Bin(found, value) if *found == grain => Some(literal_len(grain, value)),
        Intrinsic::BinAppend(found, base, _) if *found == grain => {
            known_len(grain, base).map(|len| len + 1)
        }
        Intrinsic::BinConcat(found, operands) if *found == grain => operands
            .iter()
            .map(|operand| known_len(grain, operand))
            .sum(),
        _ => None,
    }
}

fn literal_len(grain: Grain, value: &PackedBin) -> usize {
    match grain {
        Grain::B => value.bit_length(),
        Grain::X => value
            .to_bytes()
            .expect("an X-grain literal packs whole bytes")
            .len(),
    }
}

/// The literal's atoms `lo..hi` as a `Bin` literal of the same grain.
fn literal_slice(grain: Grain, value: &PackedBin, lo: usize, hi: usize) -> Term {
    Term::intrinsic(Intrinsic::Bin(
        grain,
        match grain {
            Grain::B => PackedBin::from_bits((lo..hi).map(|index| value.bit(index).unwrap())),
            Grain::X => PackedBin::from_bytes(value.to_bytes().unwrap()[lo..hi].to_vec()),
        },
    ))
}

/// The literal's atom at `index` as the element intrinsic an `append` operand carries: a `Bool` for `Bits`, a `Byte` for `Bytes`.
fn literal_atom(grain: Grain, value: &PackedBin, index: usize) -> Term {
    Term::intrinsic(match grain {
        Grain::B => Intrinsic::Bool(value.bit(index).unwrap()),
        Grain::X => Intrinsic::Byte(value.to_bytes().unwrap()[index]),
    })
}

/// Split `lit` against one spine node, enqueuing the aligned sub-goals.
fn split_against(
    cmp: &mut Convert,
    grain: Grain,
    lit: &PackedBin,
    spine: &Intrinsic,
) -> Option<bool> {
    let len = literal_len(grain, lit);
    match spine {
        // `append(base, atom) = base ++ [atom]`: the last literal atom pairs with `atom`, the rest with `base`. An empty literal against an always-nonempty `append` is a definite clash.
        Intrinsic::BinAppend(found, base, atom) if *found == grain => {
            if len == 0 {
                return Some(false);
            }
            cmp.enqueue(
                Term::type_ground(),
                base.clone(),
                literal_slice(grain, lit, 0, len - 1),
            );
            cmp.enqueue(
                Term::type_ground(),
                atom.clone(),
                literal_atom(grain, lit, len - 1),
            );
            Some(true)
        }
        // `concat` splits at its segments' known lengths, consumed left to right; one trailing unknown-length segment takes the remainder. An unknown-length segment anywhere else abstains — the split is not determined.
        Intrinsic::BinConcat(found, operands) if *found == grain => {
            let mut offset = 0usize;
            for (index, operand) in operands.iter().enumerate() {
                match known_len(grain, operand) {
                    Some(segment) => {
                        if offset + segment > len {
                            return Some(false);
                        }
                        cmp.enqueue(
                            Term::type_ground(),
                            operand.clone(),
                            literal_slice(grain, lit, offset, offset + segment),
                        );
                        offset += segment;
                    }
                    None if index == operands.len() - 1 => {
                        cmp.enqueue(
                            Term::type_ground(),
                            operand.clone(),
                            literal_slice(grain, lit, offset, len),
                        );
                        return Some(true);
                    }
                    None => return None,
                }
            }
            match offset == len {
                true => Some(true),
                false => Some(false),
            }
        }
        _ => None,
    }
}

/// The free-monoid peel for two `Nat`s, which unlike `Bin`/`List` is spelled against the carrier rather than the intrinsic.
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
