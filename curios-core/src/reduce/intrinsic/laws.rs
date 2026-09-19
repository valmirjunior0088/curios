//! The algebraic rewrites a fold falls through to when its operands are not both literals.
//!
//! A total fold answers from values alone; these answer from *form* — an idempotent lattice operation on one operand, a ring identity, a self-comparison — so a term with a symbol in it still decides. Each is applied by [`then_laws`] only after the value fold declined, which is what keeps a law from ever contradicting arithmetic.

use {
    super::dual_comparison,
    crate::{Intrinsic, Nat, Subterm, Term},
};

/// A binary fold's laws beside its two-literal case, tried on what that case left neutral: a literal unit on one side yields the other operand, a literal absorbing element yields itself, and two structurally identical operands yield what idempotence or self-cancellation says. Every one is an equation on the carrier's values that holds for every value of its symbolic side, which is what makes it admissible in a fold both checkers share — see `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md`. Run after the fold rather than inside it because every binary helper already rebuilds its neutral from the operands it reduced, so the laws read them back off the neutral and the helpers keep one signature; a fold that produced a literal has no operands to read and passes through. `reduce_bool_binary` leaves a connective's right operand as written under a stuck left — deliberately, see `a_stuck_left_operand_leaves_the_right_as_written` — so a `&&` or `||` law sees that operand unreduced; a literal or a repeated binder is visible either way, and a law missed on an unreduced operand is a neutral the next demand reduces, never a wrong answer. An equality, and the `xor` that `!=` lowers through, reads both, since its laws do.
pub(super) fn then_laws(
    result: Subterm,
    laws: impl FnOnce(&Term, &Term) -> Option<Term>,
) -> Subterm {
    let Subterm::Intrinsic(intrinsic) = &result else {
        return result;
    };
    let operands = intrinsic.operands();
    let [left, right] = operands.as_slice() else {
        return result;
    };
    match laws(left, right) {
        Some(term) => Term::unwrap_or_clone(term),
        None => result,
    }
}

/// `&&` with `unit = true` and `||` with `unit = false`: the other literal absorbs, a repeated operand is itself, and an operand beside its own negation is the absorber — the complement law of the Boolean algebra on the value type, `b && not b = false` and `b || not b = true`, which holds by cases on `b` and says nothing about propositions.
pub(super) fn bool_lattice_laws(left: &Term, right: &Term, unit: bool) -> Option<Term> {
    match (left.as_bool(), right.as_bool()) {
        (Some(l), _) => Some(if l == unit {
            right.clone()
        } else {
            left.clone()
        }),
        (_, Some(r)) => Some(if r == unit {
            left.clone()
        } else {
            right.clone()
        }),
        _ if left == right => Some(left.clone()),
        _ if complementary(left, right) => Some(Term::intrinsic(Intrinsic::Bool(!unit))),
        _ => None,
    }
}

/// Whether two reduced operands are one value and its negation: `not` is `xor(_, true)` once unfolded, and a comparison's negation is its dual, `not(a < b)` being `b <= a` — the table `dual_comparison` keeps, which is the total order's and so leaves `Flt` out. Read either way round, so a law asks once. A connective's leaf the fold left as written is missed here and met by the converters, which force every leaf of a tree before they compare it; an equality reads both operands and meets it at the fold.
fn complementary(left: &Term, right: &Term) -> bool {
    let negation = |term: &Term| match &**term {
        Subterm::Intrinsic(Intrinsic::BoolXor(a, b)) if b.as_bool() == Some(true) => {
            Some(a.clone())
        }
        Subterm::Intrinsic(Intrinsic::BoolXor(a, b)) if a.as_bool() == Some(true) => {
            Some(b.clone())
        }
        Subterm::Intrinsic(intrinsic) => dual_comparison(intrinsic).map(Term::intrinsic),
        _ => None,
    };
    negation(left).is_some_and(|negated| negated == *right)
        || negation(right).is_some_and(|negated| negated == *left)
}

/// `xor`: `false` is the unit, a repeated operand cancels to `false`, and a shared operand cancels through one nesting — `(a ⊕ c) ⊕ c = a` — which is what takes `not(not(b))` back to `b`, `not` being `xor(·, true)`. A literal `true` stays: `xor(b, true)` *is* `not b`, and there is nothing shorter to spell it as.
pub(super) fn bool_xor_laws(left: &Term, right: &Term) -> Option<Term> {
    if left.as_bool() == Some(false) {
        return Some(right.clone());
    }
    if right.as_bool() == Some(false) {
        return Some(left.clone());
    }
    if left == right {
        return Some(Term::intrinsic(Intrinsic::Bool(false)));
    }
    if let Subterm::Intrinsic(Intrinsic::BoolXor(a, c)) = &**left {
        if c == right {
            return Some(a.clone());
        }
        if a == right {
            return Some(c.clone());
        }
    }
    if let Subterm::Intrinsic(Intrinsic::BoolXor(a, c)) = &**right {
        if c == left {
            return Some(a.clone());
        }
        if a == left {
            return Some(c.clone());
        }
    }
    None
}

/// `==` with `same = true` and `!=` with `same = false`: identical operands decide, complementary operands decide the other way, a literal equal to `same` yields the other operand, and the opposite literal negates it — as `xor(·, true)`, the spelling `not` already has.
pub(super) fn bool_eql_laws(left: &Term, right: &Term, same: bool) -> Option<Term> {
    if left == right {
        return Some(Term::intrinsic(Intrinsic::Bool(same)));
    }
    if complementary(left, right) {
        return Some(Term::intrinsic(Intrinsic::Bool(!same)));
    }
    let (literal, other) = match (left.as_bool(), right.as_bool()) {
        (Some(l), _) => (l, right),
        (_, Some(r)) => (r, left),
        _ => return None,
    };
    Some(match literal == same {
        true => other.clone(),
        false => Term::intrinsic(Intrinsic::BoolXor(
            other.clone(),
            Term::intrinsic(Intrinsic::Bool(true)),
        )),
    })
}

/// The bitwise lattice on ℕ: `and` has `0` absorbing and no unit (there is no all-ones natural), `or` and `xor` have `0` as unit; `and` and `or` are idempotent and `xor` self-cancels.
pub(super) fn nat_bitwise_laws(left: &Term, right: &Term, op: &Intrinsic) -> Option<Term> {
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let (left_zero, right_zero) = (Nat::is_zero(left), Nat::is_zero(right));
    match op {
        Intrinsic::NatAnd(..) => {
            if left_zero || right_zero {
                return Some(zero());
            }
            (left == right).then(|| left.clone())
        }
        Intrinsic::NatOr(..) => {
            if left_zero {
                return Some(right.clone());
            }
            if right_zero || left == right {
                return Some(left.clone());
            }
            None
        }
        Intrinsic::NatXor(..) => {
            if left_zero {
                return Some(right.clone());
            }
            if right_zero {
                return Some(left.clone());
            }
            (left == right).then(zero)
        }
        _ => None,
    }
}

/// A shift by `0` is the value, and a shifted `0` is `0`. No other shift law is taken here, and the reason is resources rather than arithmetic: `shl(x, k) = 2ᵏ · x` holds on the unbounded ℕ the type level folds, and the run time refuses a shift that leaves the carrier rather than truncating it, but the rule would build a coefficient whose size the operands do not bound, and a law beside a fold takes no reducer to charge for it.
pub(super) fn nat_shift_laws(left: &Term, right: &Term) -> Option<Term> {
    if Nat::is_zero(right) || Nat::is_zero(left) {
        return Some(left.clone());
    }
    None
}
