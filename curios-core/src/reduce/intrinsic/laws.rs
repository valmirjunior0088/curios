//! The algebraic rewrites a fold falls through to when its operands are not both literals.
//!
//! A total fold answers from values alone; these answer from *form* — an idempotent lattice operation on one operand, a ring identity, a self-comparison — so a term with a symbol in it still decides. Each is applied by [`then_laws`] only after the value fold declined, which is what keeps a law from ever contradicting arithmetic.

use {
    super::dual_comparison,
    crate::{Declaration, Intrinsic, Nat, Subterm, Term},
    curios_algebra::{BooleanPair, Carrier, Connected, Nested, Operation, Pair, Reduct},
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

/// The Boolean connectives' identities, as `curios-algebra`'s `Operation::boolean_identity` states them: `&&` and `||` with their units and absorbers, idempotence and the complement law; `xor` with its unit, self-cancellation and cancellation through one nesting, which takes `not(not(b))` back to `b`; `==` and `!=` over identical, complementary and literal operands. What is read here is each operand's literal value, whether the two are one term, whether one is the other's negation, and for `xor` which nested operand cancels; a law that answers a negation builds it as `xor(·, true)`, the spelling `not` already has.
pub(super) fn bool_laws(left: &Term, right: &Term, op: &Intrinsic) -> Option<Term> {
    let Declaration::Operation {
        carrier: Carrier::Boolean,
        operation,
        ..
    } = op.algebra()
    else {
        return None;
    };
    let lattice_or_equality = matches!(
        operation,
        Operation::And | Operation::Or | Operation::Equal | Operation::Unequal
    );
    let pair = BooleanPair {
        left: left.as_bool(),
        right: right.as_bool(),
        same: left == right,
        complementary: lattice_or_equality && complementary(left, right),
        nested: match operation {
            Operation::Xor => nested(left, right),
            _ => None,
        },
    };
    let negated = |term: &Term| {
        Term::intrinsic(Intrinsic::BoolXor(
            term.clone(),
            Term::intrinsic(Intrinsic::Bool(true)),
        ))
    };
    operation
        .boolean_identity(pair)
        .map(|connected| match connected {
            Connected::Left => left.clone(),
            Connected::Right => right.clone(),
            Connected::Literal(value) => Term::intrinsic(Intrinsic::Bool(value)),
            Connected::NegatedLeft => negated(left),
            Connected::NegatedRight => negated(right),
            Connected::Nested(nested) => inner(left, right, nested),
        })
}

/// Where one operand is a `xor` one of whose operands is the other operand, which of its operands is left once the pair cancels — the left `xor` asked first, its second operand before its first.
fn nested(left: &Term, right: &Term) -> Option<Nested> {
    if let Subterm::Intrinsic(Intrinsic::BoolXor(a, c)) = &**left {
        if c == right {
            return Some(Nested::LeftFirst);
        }
        if a == right {
            return Some(Nested::LeftSecond);
        }
    }
    if let Subterm::Intrinsic(Intrinsic::BoolXor(a, c)) = &**right {
        if c == left {
            return Some(Nested::RightFirst);
        }
        if a == left {
            return Some(Nested::RightSecond);
        }
    }
    None
}

/// The operand [`nested`] named.
fn inner(left: &Term, right: &Term, nested: Nested) -> Term {
    let operands = |term: &Term| match &**term {
        Subterm::Intrinsic(Intrinsic::BoolXor(a, c)) => (a.clone(), c.clone()),
        _ => unreachable!("a nested cancellation names a `xor` operand"),
    };
    match nested {
        Nested::LeftFirst => operands(left).0,
        Nested::LeftSecond => operands(left).1,
        Nested::RightFirst => operands(right).0,
        Nested::RightSecond => operands(right).1,
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

/// The bitwise lattice on ℕ, as `curios-algebra`'s `Operation::bitwise_identity` states it: `and` has `0` absorbing and no unit, `or` and `xor` have `0` as unit; `and` and `or` are idempotent and `xor` self-cancels. What is read here is whether an operand is zero and whether the two are one term.
pub(super) fn nat_bitwise_laws(left: &Term, right: &Term, op: &Intrinsic) -> Option<Term> {
    let reduct = match op.algebra() {
        Declaration::Operation {
            carrier: Carrier::Natural,
            operation,
            ..
        } => operation.bitwise_identity(Pair {
            left_zero: Nat::is_zero(left),
            right_zero: Nat::is_zero(right),
            same: left == right,
        }),
        _ => None,
    };
    reduct.map(|reduct| match reduct {
        Reduct::Left => left.clone(),
        Reduct::Right => right.clone(),
        Reduct::Zero => Term::intrinsic(Intrinsic::Nat(Nat::Zero)),
    })
}

/// A shift by `0` is the value, and a shifted `0` is `0` — the two shift laws that build nothing, which is why they are the two stated here: a law beside a fold takes no reducer to charge. The one that builds, `shl(x, k) = 2ᵏ · x` for a literal `k`, is `then_coefficient`'s, which has the reducer in hand and charges the coefficient before it exists.
pub(super) fn nat_shift_laws(left: &Term, right: &Term, op: &Intrinsic) -> Option<Term> {
    let Declaration::Operation { operation, .. } = op.algebra() else {
        return None;
    };
    let pair = Pair {
        left_zero: Nat::is_zero(left),
        right_zero: Nat::is_zero(right),
        same: false,
    };
    operation.shift_identity(pair).map(|_| left.clone())
}
