//! The algebraic rewrites a fold falls through to when its operands are not both literals.
//!
//! A total fold answers from values alone; these answer from *form* — an idempotent lattice operation on one operand, a ring identity, a self-comparison — so a term with a symbol in it still decides. Each is applied by [`then_laws`](super::then_laws) only after the value fold declined, which is what keeps a law from ever contradicting arithmetic.

use {
    crate::{Intrinsic, Nat, Subterm, Term},
    curios_num::Integer,
};

/// A binary fold's laws beside its two-literal case, tried on what that case left neutral: a literal unit on one side yields the other operand, a literal absorbing element yields itself, and two structurally identical operands yield what idempotence or self-cancellation says. Every one is an equation on the carrier's values that holds for every value of its symbolic side, which is what makes it admissible in a fold both checkers share — see `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md`. Run after the fold rather than inside it because every binary helper already rebuilds its neutral from the operands it reduced, so the laws read them back off the neutral and the helpers keep one signature; a fold that produced a literal has no operands to read and passes through. `reduce_bool_binary` leaves its right operand as written under a stuck left — deliberately, see `a_stuck_left_operand_leaves_the_right_as_written` — so a `Bool` law sees that operand unreduced; a literal or a repeated binder is visible either way, and a law missed on an unreduced operand is a neutral the next demand reduces, never a wrong answer.
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

/// `&&` with `unit = true` and `||` with `unit = false`: the other literal absorbs, and a repeated operand is itself.
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
        _ => (left == right).then(|| left.clone()),
    }
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

/// `==` with `same = true` and `!=` with `same = false`: identical operands decide, a literal equal to `same` yields the other operand, and the opposite literal negates it — as `xor(·, true)`, the spelling `not` already has.
pub(super) fn bool_eql_laws(left: &Term, right: &Term, same: bool) -> Option<Term> {
    if left == right {
        return Some(Term::intrinsic(Intrinsic::Bool(same)));
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

/// A shift by `0` is the value, and a shifted `0` is `0` — both hold on the unbounded ℕ the type level folds and on the truncating carrier the runtime imposes, which is why no other shift law is taken here: `shl(x, k) = 2ᵏ · x` holds only on the former.
pub(super) fn nat_shift_laws(left: &Term, right: &Term) -> Option<Term> {
    if Nat::is_zero(right) || Nat::is_zero(left) {
        return Some(left.clone());
    }
    None
}

/// The ring laws `Int` has literally: `0` is `+`'s unit and `-`'s right unit, `1` is `*`'s unit and `0` its absorber, and `i - i` is `0`. Commutativity is deliberately not here — it needs the summand normal form `Nat` has, which `Int` does not, and a law that fires on one operand order is not a law.
pub(super) fn int_ring_laws(left: &Term, right: &Term, op: &Intrinsic) -> Option<Term> {
    let is = |term: &Term, value: i32| term.as_int() == Some(Integer::from(value));
    let zero = || Term::intrinsic(Intrinsic::Int(Integer::from(0)));
    match op {
        Intrinsic::IntAdd(..) => {
            if is(left, 0) {
                return Some(right.clone());
            }
            is(right, 0).then(|| left.clone())
        }
        Intrinsic::IntSub(..) => {
            if is(right, 0) {
                return Some(left.clone());
            }
            (left == right).then(zero)
        }
        Intrinsic::IntMul(..) => {
            if is(left, 0) || is(right, 0) {
                return Some(zero());
            }
            if is(left, 1) {
                return Some(right.clone());
            }
            is(right, 1).then(|| left.clone())
        }
        _ => None,
    }
}

/// Identical operands decide `==` and `!=` on any carrier whose equality is the value's: two structurally identical reduced terms denote one value.
pub(super) fn identity_laws(left: &Term, right: &Term, same: bool) -> Option<Term> {
    (left == right).then(|| Term::intrinsic(Intrinsic::Bool(same)))
}
