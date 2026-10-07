//! The identities of `and`, `or` and `xor` on ℕ and on ℤ, and of the shifts, that read no value: a zero operand, an operand with every bit set, or the same operand twice.

use {crate::Operation, curios_num::Natural};

/// What an operation's two operands were observed to be, as its identities read them.
#[derive(Clone, Copy, Debug, Default)]
pub struct Pair {
    pub left_zero: bool,
    pub right_zero: bool,
    /// The left operand is `-1`, every bit of the two's-complement expansion set, which no natural is.
    pub left_ones: bool,
    pub right_ones: bool,
    /// The two operands are one term.
    pub same: bool,
}

/// What an operation is by an identity: one of its operands, zero, or `-1`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reduct {
    Left,
    Right,
    Zero,
    Ones,
}

impl Operation {
    /// The bitwise lattice, on ℕ and on ℤ's two's-complement expansion: `and` has `0` absorbing and `-1` as unit, so no unit on ℕ, where no value has every bit set; `or` has `0` as unit and `-1` absorbing; `xor` has `0` as unit; `and` and `or` are idempotent and `xor` self-cancels. `None` where no identity applies, or for any other operation.
    pub fn bitwise_identity(self, pair: Pair) -> Option<Reduct> {
        match self {
            Operation::And => match pair {
                Pair {
                    left_zero: true, ..
                }
                | Pair {
                    right_zero: true, ..
                } => Some(Reduct::Zero),
                Pair {
                    left_ones: true, ..
                } => Some(Reduct::Right),
                Pair {
                    right_ones: true, ..
                }
                | Pair { same: true, .. } => Some(Reduct::Left),
                _ => None,
            },
            Operation::Or => match pair {
                Pair {
                    left_ones: true, ..
                }
                | Pair {
                    right_ones: true, ..
                } => Some(Reduct::Ones),
                Pair {
                    left_zero: true, ..
                } => Some(Reduct::Right),
                Pair {
                    right_zero: true, ..
                }
                | Pair { same: true, .. } => Some(Reduct::Left),
                _ => None,
            },
            Operation::Xor => match pair {
                Pair {
                    left_zero: true, ..
                } => Some(Reduct::Right),
                Pair {
                    right_zero: true, ..
                } => Some(Reduct::Left),
                Pair { same: true, .. } => Some(Reduct::Zero),
                _ => None,
            },
            _ => None,
        }
    }

    /// A shift by `0` is the value, and a shifted `0` is `0`, on ℕ and on ℤ alike: the shift identities that build nothing. `None` for any other pair, or any other operation.
    pub fn shift_identity(self, pair: Pair) -> Option<Reduct> {
        match self {
            Operation::ShiftLeft | Operation::ShiftRight if pair.left_zero || pair.right_zero => {
                Some(Reduct::Left)
            }
            _ => None,
        }
    }
}

/// `2ᵏ`, the coefficient a left shift by `k` is — `shl(x, k) = 2ᵏ · x` on the unbounded carriers, below zero too. `None` past what can be built, which a caller has already priced by `k`.
pub fn power_of_two(exponent: &Natural) -> Option<Natural> {
    Natural::one().shl_within(exponent, u64::MAX)
}

#[cfg(test)]
mod tests;
