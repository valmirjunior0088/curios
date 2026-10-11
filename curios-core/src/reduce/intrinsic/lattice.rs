//! The folds of `min` and `max` over `Nat` and `Int`, and what a comparison reads of one.
//!
//! **A semilattice with a successor.** A tree of one operation is a set of leaves ([`Extremum::leaves`]): the fold drops each leaf another answers for, by the plain comparison's verdict (`curios-algebra`'s `Lattice::keeps`), and stands the least literal offset its leaves share outside the node, as a sum stands its floor. So `min(x, x + y)` is `x`, `max(x, 0)` is `x`, `min(a + 1, b + 1)` is `min(a, b) + 1`, and two literals fold to one.
//!
//! **The fold computes from its operands alone.** A leaf is dropped on what `compare_nat` and `compare_int` decide of the two leaves as terms, and on nothing an arm's equation says, so one node is one value in every arm it is read in, and commutation and association hold the same under a guard as outside it. `min(a, b)` under a guard on `a <= b` stays stuck, and a `/std` lemma converts it.
//!
//! **A node the fold changes nothing of keeps its spelling.** Where no leaf is dropped and no offset hoisted the node stands as written over its reduced operands; the operation's commutation, association and idempotence are decided where two trees are compared, by their leaf sets, and no fold sorts a tree.

use {
    super::{Asked, compare_int, compare_nat, operand_bound},
    crate::{Cost, Extremum, ReduceError, Reducer, Subterm, Term},
    curios_algebra::{Carrier, Comparison, Kept, Lattice},
    curios_num::Integer,
};

/// What the carrier's plain comparison decides of two terms.
fn compare(
    reducer: &mut impl Reducer,
    carrier: Carrier,
    left: Term,
    right: Term,
) -> Result<Comparison, ReduceError> {
    Ok(match carrier {
        Carrier::Natural => compare_nat(reducer, left, right)?.0,
        _ => compare_int(reducer, left, right)?.0,
    })
}

/// Reduce a `min` or a `max`: its operands reduced, two literals folded, and otherwise the tree's leaves with each one another answers for dropped and the least offset they share stood outside. A node nothing is dropped from or hoisted out of is rebuilt over its reduced operands as it was written.
pub(super) fn reduce_extremum(
    reducer: &mut impl Reducer,
    extremum: Extremum,
    left: &Term,
    right: &Term,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;
    if let Some(folded) = folded(reducer, extremum, &left, &right)? {
        return Ok(folded);
    }

    let mut leaves = extremum.leaves(&left);
    leaves.extend(extremum.leaves(&right));
    let pairs = (leaves.len() as u64).saturating_mul(leaves.len() as u64);
    reducer.spend(Cost::collection(pairs))?;

    let mut kept: Vec<Term> = Vec::new();
    let mut dropped = false;
    'leaves: for leaf in leaves {
        let mut at = 0;
        while at < kept.len() {
            let verdict = compare(reducer, extremum.carrier, kept[at].clone(), leaf.clone())?;
            match extremum.lattice.keeps(verdict) {
                Kept::First => {
                    dropped = true;
                    continue 'leaves;
                }
                Kept::Second => {
                    dropped = true;
                    kept.remove(at);
                }
                Kept::Both => at += 1,
            }
        }
        kept.push(leaf);
    }
    if let [only] = kept.as_slice() {
        return Ok(Term::unwrap_or_clone(only.clone()));
    }

    let parts = kept
        .iter()
        .map(|leaf| extremum.offset(leaf))
        .collect::<Vec<_>>();
    let least = parts
        .iter()
        .map(|(offset, _)| offset.clone())
        .min()
        .unwrap_or_else(|| Integer::from(0i32));
    if !dropped && least.is_zero() {
        return Ok(Subterm::Intrinsic(extremum.node(left, right)));
    }

    reducer.spend(Cost::term(2).saturating_mul(kept.len() as u64))?;
    let tree = parts
        .into_iter()
        .map(|(offset, rest)| extremum.offset_by(rest, &(offset - least.clone())))
        .reduce(|tree, leaf| Term::intrinsic(extremum.node(tree, leaf)))
        .expect("a node has two operands, so at least two leaves stood before one was dropped");
    Ok(Term::unwrap_or_clone(extremum.offset_by(tree, &least)))
}

/// The literal two literal operands fold to, charged by their sizes as every closed fold is.
fn folded(
    reducer: &mut impl Reducer,
    extremum: Extremum,
    left: &Term,
    right: &Term,
) -> Result<Option<Subterm>, ReduceError> {
    let least = extremum.lattice == Lattice::Meet;
    Ok(match extremum.carrier {
        Carrier::Natural => {
            let (Some(this), Some(that)) = (
                left.as_nat().and_then(|value| value.to_natural()),
                right.as_nat().and_then(|value| value.to_natural()),
            ) else {
                return Ok(None);
            };
            reducer.spend(operand_bound(this.bits(), that.bits()))?;
            let first = (this <= that) == least;
            Some(Term::unwrap_or_clone(match first {
                true => left.clone(),
                false => right.clone(),
            }))
        }
        _ => {
            let (Some(this), Some(that)) = (left.as_int(), right.as_int()) else {
                return Ok(None);
            };
            reducer.spend(operand_bound(this.bits(), that.bits()))?;
            let first = (this <= that) == least;
            Some(Term::unwrap_or_clone(match first {
                true => left.clone(),
                false => right.clone(),
            }))
        }
    })
}

/// What the lattice forces of `left` against `right`, each a reduced term of `carrier`: a `min` is at most each of its leaves and at least what is at most all of them, and a `max` the mirror, each read over the verdicts the plain comparison reaches of the leaves (`curios-algebra`'s `Lattice::below` and `Lattice::above`). `Stuck` where neither side is a `min` or a `max` under a literal offset, or the leaves' verdicts force nothing.
///
/// Each verdict asked is of a leaf, a strict subterm of the side it stands in, so the reading ends, and each is asked of `asked`, the comparison this one is part of.
pub(super) fn lattice_verdict(
    reducer: &mut impl Reducer,
    asked: &mut Asked,
    carrier: Carrier,
    left: &Term,
    right: &Term,
) -> Result<Comparison, ReduceError> {
    let forward = at_most(reducer, asked, carrier, left, right)?;
    if forward == Some(Comparison::Lt) {
        return Ok(Comparison::Lt);
    }
    let backward = at_most(reducer, asked, carrier, right, left)?;
    Ok(match (forward, backward) {
        (_, Some(Comparison::Lt)) => Comparison::Gt,
        (Some(_), Some(_)) => Comparison::Eq,
        (Some(_), None) => Comparison::Le,
        (None, Some(_)) => Comparison::Ge,
        (None, None) => Comparison::Stuck,
    })
}

/// `Lt` or `Le` where the lattice forces `lower` at most `upper`, and `None` where it forces nothing.
fn at_most(
    reducer: &mut impl Reducer,
    asked: &mut Asked,
    carrier: Carrier,
    lower: &Term,
    upper: &Term,
) -> Result<Option<Comparison>, ReduceError> {
    for lattice in [Lattice::Meet, Lattice::Join] {
        let extremum = Extremum { carrier, lattice };
        if extremum.operands(lower).is_some() {
            let leaves = extremum.leaves(lower);
            reducer.spend(Cost::collection(leaves.len() as u64))?;
            let mut verdicts = Vec::new();
            for leaf in leaves {
                verdicts.push(asked.verdict(reducer, carrier, leaf, upper.clone())?);
            }
            if let Some(forced) = lattice.below(&verdicts) {
                return Ok(Some(forced));
            }
        }
        if extremum.operands(upper).is_some() {
            let leaves = extremum.leaves(upper);
            reducer.spend(Cost::collection(leaves.len() as u64))?;
            let mut verdicts = Vec::new();
            for leaf in leaves {
                verdicts.push(asked.verdict(reducer, carrier, lower.clone(), leaf)?);
            }
            if let Some(forced) = lattice.above(&verdicts) {
                return Ok(Some(forced));
            }
        }
    }
    Ok(None)
}
