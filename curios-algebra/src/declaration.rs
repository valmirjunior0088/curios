//! What an operation is, algebraically: the kinds of operation this crate gives a meaning to, and what that meaning says about each one's operands.
//!
//! A caller maps each of its concrete operations to an [`Operation`] over a [`Carrier`] — or to nothing, which is opaque — and reads what it needs from the mapping: which operands bound the result, which operands the result never exceeds. The mapping is the caller's; what each kind means is this module's, stated once for every operation of that kind.

use curios_num::Natural;

/// The carrier an operation is over.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Carrier {
    /// ℕ, with truncated subtraction.
    Natural,
    /// ℤ.
    Integer,
}

/// An operation this crate's reasoning gives a meaning to.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Operation {
    Sum,
    /// Truncated at `Nat`, the group's at `Int`.
    Difference,
    Product,
    /// Floored at `Nat`, truncated at `Int`, over a nonzero divisor.
    Quotient,
    Remainder,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,
    Equal,
    Unequal,
    Less,
    AtMost,
    /// A byte's value as a natural: `0..=255` by its carrier.
    FromByte,
    /// A natural as an integer, ℕ → ℤ.
    Widening,
}

/// What one operand of an operation was observed to be: an upper bound on every value it takes, and its value when it is a literal. A caller observes only what [`Operation::bound_reads`] asks for; anything else may be left `None`.
#[derive(Clone, Debug, Default)]
pub struct Observed {
    pub bound: Option<Natural>,
    pub literal: Option<Natural>,
}

impl Operation {
    /// The operands whose bounds [`Operation::upper_bound`] reads, and the operands whose literal values it reads, by position — so a caller observes nothing the bound does not use.
    ///
    /// **A bound is read of every operand the result is not *antitone* in**, and of no other. That is the criterion, and stating it is the point: a value that only shrinks as an operand grows needs no bound on that operand at all, which is why a subtrahend, a divisor and a shift amount are read past — as values, where a literal one tightens the result, and never as bounds.
    pub fn bound_reads(self) -> (&'static [usize], &'static [usize]) {
        match self {
            Operation::Sum | Operation::Product => (&[0, 1], &[]),
            Operation::Difference => (&[0], &[]),
            Operation::Quotient | Operation::ShiftRight => (&[0], &[1]),
            Operation::Remainder => (&[], &[1]),
            Operation::And | Operation::Or | Operation::Xor => (&[0, 1], &[]),
            _ => (&[], &[]),
        }
    }

    /// An upper bound on every value this operation takes over ℕ, from what its operands were observed to be; `None` where it has none.
    ///
    /// Every arm is unconditional, which is what lets a caller turn a bound into a definitional equation: an over-report only withholds a rule, and an *under*-report is a false equation. A product is monotone in each factor and still needs both bounded, since either one left free makes it unbounded. A byte is `0..=255` by its carrier, whatever produced it. `x % n < n` holds by definition over a nonzero `n`. A join or a difference of bits reaches no bit neither side can, so its bound is the widest value of the wider bound's bit length. **A left shift has no arm**, and the reason is resources rather than arithmetic: its result size is not bounded by its operands', and a bound is computed where nothing charges for it.
    pub fn upper_bound(self, operands: &[Observed]) -> Option<Natural> {
        let bound = |at: usize| operands.get(at).and_then(|operand| operand.bound.clone());
        let literal = |at: usize| operands.get(at).and_then(|operand| operand.literal.clone());
        match self {
            Operation::FromByte => Some(Natural::from(u8::MAX)),
            Operation::Remainder => {
                let divisor = literal(1)?;
                (!divisor.is_zero()).then(|| divisor - Natural::one())
            }
            Operation::Difference => bound(0),
            Operation::Quotient => {
                let bound = bound(0)?;
                match literal(1) {
                    Some(divisor) => bound.div(&divisor).ok(),
                    None => Some(bound),
                }
            }
            Operation::ShiftRight => {
                let bound = bound(0)?;
                match literal(1) {
                    Some(amount) => Some(&bound >> &amount),
                    None => Some(bound),
                }
            }
            Operation::And => match (bound(0), bound(1)) {
                (Some(left), Some(right)) => Some(left.min(right)),
                (Some(either), None) | (None, Some(either)) => Some(either),
                (None, None) => None,
            },
            Operation::Or | Operation::Xor => {
                let width = bound(0)?.bits().max(bound(1)?.bits());
                let width = u32::try_from(width).ok()?;
                Some(Natural::from(2u32).pow(width) - Natural::one())
            }
            Operation::Sum => Some(bound(0)? + bound(1)?),
            Operation::Product => Some(bound(0)? * bound(1)?),
            _ => None,
        }
    }

    /// The operands the result of this operation over ℕ never exceeds, by position, each with whether it is exceeded strictly: a result antitone in one operand is at most its other, so the minuend, the dividend, either operand of `and` and the shifted value each bound their result, and a remainder is below its divisor outright.
    ///
    /// Every pair is unconditional, as [`Operation::upper_bound`]'s arms are, and for the same reason: a caller turns a dominator into a verdict.
    pub fn dominators(self) -> &'static [(usize, bool)] {
        match self {
            Operation::Difference | Operation::Quotient | Operation::ShiftRight => &[(0, false)],
            Operation::Remainder => &[(0, false), (1, true)],
            Operation::And => &[(0, false), (1, false)],
            _ => &[],
        }
    }
}
