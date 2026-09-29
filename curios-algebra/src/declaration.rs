//! What an operation is, algebraically: the kinds of operation this crate gives a meaning to, and what that meaning says about each one's operands.
//!
//! A caller maps each of its concrete operations to an [`Operation`] over a [`Carrier`] — or to nothing, which is opaque — and reads what it needs from the mapping: which operands bound the result, which operands the result never exceeds, which conversion undoes which. The mapping is the caller's; what each kind means is this module's, stated once for every operation of that kind.

use curios_num::{Grain, Natural};

/// The carrier an operation is over.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Carrier {
    /// ℕ, with truncated subtraction.
    Natural,
    /// ℤ.
    Integer,
    /// The two-element Boolean algebra.
    Boolean,
    /// The 256 values of a byte.
    Byte,
    /// binary64.
    Float,
    /// A packed sequence of bits or bytes.
    Packed(Grain),
    /// A sequence of elements.
    List,
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
    /// Two words juxtaposed.
    Concat,
    /// A word's length, a natural.
    Length,
    /// A word with one element after it.
    Append,
    /// A value of `from` as a value of the carrier the operation is declared at — a byte's value as a natural, `0..=255` by its carrier; a natural as an integer, ℕ → ℤ; a run regrouped at the other grain. Which conversions undo which is [`Operation::undoes`].
    Conversion {
        from: Carrier,
    },
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
            Operation::Conversion {
                from: Carrier::Byte,
            } => Some(Natural::from(u8::MAX)),
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

    /// Whether this operation denotes one value with its operands in either order, as far as the reasoning that reads it goes: the equalities and inequalities, and `and`, `or` and `xor`. Sum and product commute too, and their cancellation already reads them as combinations, so they are not listed here.
    pub fn commutes(self) -> bool {
        matches!(
            self,
            Operation::Equal | Operation::Unequal | Operation::And | Operation::Or | Operation::Xor
        )
    }

    /// The comparison true exactly when this one is false, and whether its operands are swapped — on a total order `a < b` is false exactly when `b <= a`, and `==` and `!=` negate each other in place; on `Bool`, where `!=` is `xor`, an equality's negation is that `xor` and a `xor`'s is the equality, operands in place. `None` for anything that is no comparison of these carriers: a floating-point comparison is none, since against a NaN both directions of an ordered comparison are false and its negation is not the mirror.
    pub fn negation(self, carrier: Carrier) -> Option<(Operation, bool)> {
        match (carrier, self) {
            (Carrier::Natural | Carrier::Integer, Operation::Less) => {
                Some((Operation::AtMost, true))
            }
            (Carrier::Natural | Carrier::Integer, Operation::AtMost) => {
                Some((Operation::Less, true))
            }
            (Carrier::Natural | Carrier::Integer, Operation::Equal) => {
                Some((Operation::Unequal, false))
            }
            (Carrier::Natural | Carrier::Integer, Operation::Unequal) => {
                Some((Operation::Equal, false))
            }
            (Carrier::Boolean, Operation::Equal) => Some((Operation::Xor, false)),
            (Carrier::Boolean, Operation::Unequal | Operation::Xor) => {
                Some((Operation::Equal, false))
            }
            _ => None,
        }
    }

    /// Whether this operation, declared at `carrier`, applied to the result of `inner`, declared at `inner_carrier`, is `inner`'s operand: a conversion undoing the conversion it is applied to. That holds where the two convert between one pair of carriers in opposite directions and the round trip through `inner` is the identity on its source — [`round_trip`]'s table.
    ///
    /// A carrier pair names one conversion in each direction, which is what lets a round trip be read off the carriers alone.
    pub fn undoes(self, carrier: Carrier, inner: Operation, inner_carrier: Carrier) -> bool {
        match (self, inner) {
            (Operation::Conversion { from }, Operation::Conversion { from: source }) => {
                from == inner_carrier && carrier == source && round_trip(source, inner_carrier)
            }
            _ => false,
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

/// Whether converting a value of `source` into `target` and back gives the value again, as far as the rule reading it takes it.
///
/// - **`Nat` and `Byte`, both ways.** A byte's value is a natural below 256, and the narrowing states `n < 256` of the natural it is handed, so each direction is the other's inverse on what it accepts.
/// - **`Nat` and `Int`, both ways.** ℕ embeds in ℤ, and the narrowing states `0 <= i`.
/// - **A run and its regrouping at the other grain, both ways.** Regrouping moves no bit, and the alignment the inner regrouping demanded is what makes the composite well formed.
/// - **`Flt` through its eight little-endian bytes, one way.** Every one of the 2⁶⁴ bit patterns is a distinct float, so decoding what encoding wrote is the float it was given, NaNs and both zeros included. The other direction holds of the model as well, since no pattern is merged; it is not taken, and a symbolic eight-byte run is not inverted.
pub fn round_trip(source: Carrier, target: Carrier) -> bool {
    match (source, target) {
        (Carrier::Natural, Carrier::Byte) | (Carrier::Byte, Carrier::Natural) => true,
        (Carrier::Natural, Carrier::Integer) | (Carrier::Integer, Carrier::Natural) => true,
        (Carrier::Packed(grain), Carrier::Packed(other)) => grain != other,
        (Carrier::Float, Carrier::Packed(Grain::X)) => true,
        _ => false,
    }
}
