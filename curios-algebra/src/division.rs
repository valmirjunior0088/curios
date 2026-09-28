//! Division by the laws its operands' values do not decide: a zero dividend, a unit or equal divisor, and a literal divisor splitting a sum.
//!
//! Every law here is unconditional — it holds for every value of every atom, over a divisor its caller's division states nonzero — and that is what admits it into a fold. `(a + b) / n = a / n + b / n` is not one: `1 / 2 + 1 / 2` is `0`, not `1`, and a law holding for only some values of a symbolic part would be a false definitional equation.

use curios_num::Natural;

/// Which half of a Euclidean division is asked for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Half {
    Quotient,
    Remainder,
}

/// What a division is by a law that needs no value: zero, its dividend, or one.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Divided {
    Zero,
    Dividend,
    One,
}

impl Half {
    /// `0 / d = 0` and `0 % d = 0`, by any divisor.
    pub fn of_zero(self) -> Divided {
        Divided::Zero
    }

    /// `x / 1 = x` and `x % 1 = 0`.
    pub fn by_one(self) -> Divided {
        match self {
            Half::Quotient => Divided::Dividend,
            Half::Remainder => Divided::Zero,
        }
    }

    /// `x / x = 1` and `x % x = 0`, on the division's own precondition that its divisor is nonzero.
    pub fn by_itself(self) -> Divided {
        match self {
            Half::Quotient => Divided::One,
            Half::Remainder => Divided::Zero,
        }
    }
}

/// A floor split by a literal divisor into the whole divisors it carries and what is left of it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FloorSplit {
    pub whole: Natural,
    pub rest: Natural,
}

/// The floor law, the division twin of addition's: `(i + f) / n = f / n + (i + f % n) / n` and `(i + f) % n = (i + f % n) % n`, for every `i`, because `f = (f / n) · n + f % n` contributes exactly `f / n` whole divisors whatever `i` is. `None` where the floor carries no whole divisor, so the rest cannot fire the law a second time.
pub fn floor_law(floor: &Natural, divisor: &Natural) -> Option<FloorSplit> {
    (floor >= divisor).then(|| FloorSplit {
        whole: floor / divisor,
        rest: floor % divisor,
    })
}

/// The quotient a summand with literal coefficient `coefficient` contributes to a split by `divisor`: its cofactor, where the divisor divides the coefficient.
pub fn cofactor(coefficient: &Natural, divisor: &Natural) -> Option<Natural> {
    (coefficient % divisor)
        .is_zero()
        .then(|| coefficient / divisor)
}

/// The Euclidean split of a sum against a literal divisor: every summand either a multiple of the divisor — contributing its [`cofactor`] to the quotient — or bounded, and when the bounded summands together with the floor's remainder stay below the divisor, none of them can carry into the next multiple, so the split is exact for every value the symbolic parts take. That is what makes `(256 · x + b) / 256` be `x` for a byte `b`. `residual_bounds` are the bounds of the summands that are not multiples; `None` where they reach the divisor.
pub fn euclid_split(
    floor: &Natural,
    divisor: &Natural,
    residual_bounds: impl IntoIterator<Item = Natural>,
) -> Option<FloorSplit> {
    let rest = floor % divisor;
    let ceiling = residual_bounds
        .into_iter()
        .fold(rest.clone(), |ceiling, bound| ceiling + bound);
    (ceiling < *divisor).then(|| FloorSplit {
        whole: floor / divisor,
        rest,
    })
}
