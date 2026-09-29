//! Linear combinations over monomials: collection, and cancellation at the two carriers' strengths.
//!
//! A [`Combination`] is a constant beside summands, each a coefficient times a [`Monomial`], held once per monomial in the order the monomial first appeared. First appearance is what makes collecting a combination already collected the identity, which is what lets a caller read a normal form back and rebuild it unchanged.
//!
//! Cancellation takes off both sides of an equation or a comparison what they hold in common, and the two carriers do it at different strengths. `Nat` is a cancellative commutative monoid: `x + a ⋈ x + b` exactly when `a ⋈ b`, for every order relation and for truncated subtraction, so the smaller coefficient of every shared monomial comes off both sides, and the smaller constant. `Int` is a group: `a ⋈ b` exactly when `a - b ⋈ 0`, so the difference is split by sign, every monomial to the side that keeps its coefficient positive. Both preserve the answer rather than approximate it, which is what makes a residual [`Deduction::Equivalent`].

use {
    crate::{Deduction, Monomial},
    curios_num::{Integer, Natural},
    std::{collections::HashMap, ops::Add},
};

/// What a coefficient must support to be collected: a zero, and addition.
pub trait Coefficient: Clone + Add<Output = Self> {
    /// The coefficient of nothing.
    fn zero() -> Self;
    /// Whether this is [`Coefficient::zero`].
    fn is_zero(&self) -> bool;
}

impl Coefficient for Natural {
    fn zero() -> Self {
        Natural::zero()
    }

    fn is_zero(&self) -> bool {
        Natural::is_zero(self)
    }
}

impl Coefficient for Integer {
    fn zero() -> Self {
        Integer::from(0)
    }

    fn is_zero(&self) -> bool {
        Integer::is_zero(self)
    }
}

/// One summand: a coefficient times a monomial, and the origin its caller rebuilds it from.
///
/// The origin is carried and never read. Collection keeps the origin of a monomial's first appearance, and every result hands back the origins it kept, so the caller rebuilds from the terms it was given rather than from a respelling of them.
#[derive(Clone, Debug)]
pub struct Summand<C, O> {
    pub coefficient: C,
    pub monomial: Monomial,
    pub origin: O,
}

/// A constant beside a linear combination of monomials, each held once, in the order it first appeared.
#[derive(Clone, Debug)]
pub struct Combination<C, O> {
    pub constant: C,
    pub summands: Vec<Summand<C, O>>,
}

impl<C: Coefficient, O> Combination<C, O> {
    /// `summands` beside `constant`, like monomials merged by adding their coefficients. A merged monomial keeps the position and the origin of its first appearance.
    pub fn collect(constant: C, summands: impl IntoIterator<Item = Summand<C, O>>) -> Self {
        let mut collected: Vec<Summand<C, O>> = Vec::new();
        let mut index_of: HashMap<Monomial, usize> = HashMap::new();
        for summand in summands {
            match index_of.get(&summand.monomial) {
                Some(&index) => {
                    let held = &mut collected[index];
                    held.coefficient = held.coefficient.clone() + summand.coefficient;
                }
                None => {
                    index_of.insert(summand.monomial.clone(), collected.len());
                    collected.push(summand);
                }
            }
        }

        Combination {
            constant,
            summands: collected,
        }
    }

    /// This combination without the summands whose coefficients cancelled to zero, the rest in their order.
    pub fn without_zeros(mut self) -> Self {
        self.summands
            .retain(|summand| !summand.coefficient.is_zero());
        self
    }

    /// Whether the two combinations hold a monomial in common.
    fn shares_summand(&self, other: &Self) -> bool {
        self.summands.iter().any(|summand| {
            other
                .summands
                .iter()
                .any(|candidate| candidate.monomial == summand.monomial)
        })
    }

    /// Whether this is the zero: no summand and a zero constant.
    pub fn is_zero(&self) -> bool {
        self.summands.is_empty() && self.constant.is_zero()
    }
}

/// How much a cancellation took off, which is what its caller's reconstruction reads: nothing leaves both sides as they were, a constant alone leaves every summand where it stood, and a summand means the combinations were rewritten.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Progress {
    Nothing,
    Constant,
    Summands,
}

/// Two combinations with what they held in common taken off, and how much that was.
#[derive(Clone, Debug)]
pub struct Cancelled<C, O> {
    pub left: Combination<C, O>,
    pub right: Combination<C, O>,
    pub progress: Progress,
}

impl<O> Combination<Natural, O> {
    /// `self` against `right` with what they share taken off both, over `Nat`'s cancellative commutative monoid: the smaller coefficient of every monomial both hold, and the smaller constant.
    ///
    /// **A multiset, never a set.** `2 · a + b` against `a + c` takes off one `a` and leaves `a + b` against `c`; taking both would read `a + b ⋈ c` off `2 · a + b ⋈ a + c`, which is false. `right`'s summands keep its order and `self`'s keep theirs, a summand whose coefficient reached zero dropping out where it stood.
    ///
    /// Both combinations must already be collected, so that each holds a monomial at most once.
    pub fn cancel_common(mut self, mut right: Self) -> Cancelled<Natural, O> {
        let index_of: HashMap<Monomial, usize> = self
            .summands
            .iter()
            .enumerate()
            .map(|(index, summand)| (summand.monomial.clone(), index))
            .collect();

        let mut shared_summand = false;
        let mut residual = Vec::with_capacity(right.summands.len());
        for summand in right.summands {
            let Some(&index) = index_of.get(&summand.monomial) else {
                residual.push(summand);
                continue;
            };
            let held = &mut self.summands[index].coefficient;
            let shared = held.clone().min(summand.coefficient.clone());
            *held = held.clone() - &shared;
            shared_summand = true;

            let rest = summand.coefficient - &shared;
            if !rest.is_zero() {
                residual.push(Summand {
                    coefficient: rest,
                    ..summand
                });
            }
        }
        self.summands
            .retain(|summand| !summand.coefficient.is_zero());
        right.summands = residual;

        let shared = self.constant.clone().min(right.constant.clone());
        self.constant = self.constant - &shared;
        right.constant = right.constant - &shared;

        let progress = match (shared_summand, shared.is_zero()) {
            (true, _) => Progress::Summands,
            (false, false) => Progress::Constant,
            (false, true) => Progress::Nothing,
        };
        Cancelled {
            left: self,
            right,
            progress,
        }
    }
}

impl<O> Cancelled<Natural, O> {
    /// What the cancellation concludes about `left = right` over `Nat`: both sides emptied is equality, and a positive constant against an emptied side is impossible, since every summand is at least zero. A cancellation that took anything off leaves an equivalent pair; one that took nothing off concludes nothing, so a caller comparing again would meet the pair it started from.
    pub fn deduction(self) -> Deduction<Self> {
        match (self.left.is_zero(), self.right.is_zero()) {
            (true, true) => Deduction::Equal,
            (true, false) if !self.right.constant.is_zero() => Deduction::Impossible,
            (false, true) if !self.left.constant.is_zero() => Deduction::Impossible,
            _ => match self.progress {
                Progress::Nothing => Deduction::Undecided,
                Progress::Constant | Progress::Summands => Deduction::Equivalent(self),
            },
        }
    }
}

impl<O> Combination<Integer, O> {
    /// `self` against `right` over `Int`'s group, split by sign where they share anything: a monomial both hold, or a nonzero constant on each side.
    ///
    /// **A pair sharing nothing is handed back as it was**, [`Progress::Nothing`], for the stability a caller rebuilding from the result needs: a split taken where nothing cancels is still a respelling, and a stuck comparison respelled on every pass is never found again. [`Combination::split_by_sign`] is the split taken whatever is shared, for a caller that wants one spelling of every difference.
    pub fn cancel_common(self, right: Self) -> Cancelled<Integer, O> {
        let shared =
            self.shares_summand(&right) || !(self.constant.is_zero() || right.constant.is_zero());
        match shared {
            false => Cancelled {
                left: self,
                right,
                progress: Progress::Nothing,
            },
            true => {
                let (left, right) = self.split_by_sign(right);
                Cancelled {
                    left,
                    right,
                    progress: Progress::Summands,
                }
            }
        }
    }

    /// `self - right`, split by sign into the two sides it is spelled as: every monomial on the side that keeps its coefficient positive, and the constant likewise, so two pairs with one difference are one pair — `0 < j - i` and `i < j`, `-i < -j` and `j < i`.
    ///
    /// The difference is collected in `self`'s order and then `right`'s, so a monomial both hold keeps `self`'s position and origin, and one that cancelled to zero is gone.
    pub fn split_by_sign(self, right: Self) -> (Self, Self) {
        let zero = Integer::zero();
        let constant = self.constant - right.constant;
        let negated = right.summands.into_iter().map(|summand| Summand {
            coefficient: -summand.coefficient,
            ..summand
        });
        let difference =
            Combination::collect(Integer::zero(), self.summands.into_iter().chain(negated))
                .without_zeros();

        let mut kept_left = Vec::new();
        let mut kept_right = Vec::new();
        for summand in difference.summands {
            match summand.coefficient > zero {
                true => kept_left.push(summand),
                false => kept_right.push(Summand {
                    coefficient: -summand.coefficient,
                    ..summand
                }),
            }
        }

        let (constant_left, constant_right) = match constant > zero {
            true => (constant, Integer::zero()),
            false => (Integer::zero(), -constant),
        };
        (
            Combination {
                constant: constant_left,
                summands: kept_left,
            },
            Combination {
                constant: constant_right,
                summands: kept_right,
            },
        )
    }
}

impl<O> Combination<Integer, O> {
    /// `-self`: every coefficient and the constant negated.
    pub fn negated(self) -> Self {
        Combination {
            constant: -self.constant,
            summands: self
                .summands
                .into_iter()
                .map(|summand| Summand {
                    coefficient: -summand.coefficient,
                    ..summand
                })
                .collect(),
        }
    }

    /// This combination over ℕ, when every coefficient and the constant are non-negative — the only combinations ℕ → ℤ can have an image of whatever its atoms are, given the caller has established that every atom is itself a widened natural. `None` otherwise: a negative coefficient can make the value negative, and nothing here reads a bound.
    pub fn natural(self) -> Option<Combination<Natural, O>> {
        let constant = Natural::try_from(&self.constant).ok()?;
        let summands = self
            .summands
            .into_iter()
            .map(|summand| {
                Some(Summand {
                    coefficient: Natural::try_from(&summand.coefficient).ok()?,
                    monomial: summand.monomial,
                    origin: summand.origin,
                })
            })
            .collect::<Option<Vec<_>>>()?;
        Some(Combination { constant, summands })
    }
}

impl<O> Cancelled<Integer, O> {
    /// What the cancellation concludes about `left = right` over `Int`: two constants decide by value, and a split that took anything off leaves an equivalent pair. No sign bound is read, since a summand may be negative, so a constant against a symbolic side is never impossible here.
    pub fn deduction(self) -> Deduction<Self> {
        match (
            self.left.summands.is_empty(),
            self.right.summands.is_empty(),
        ) {
            (true, true) => match self.left.constant == self.right.constant {
                true => Deduction::Equal,
                false => Deduction::Impossible,
            },
            _ => match self.progress {
                Progress::Nothing => Deduction::Undecided,
                Progress::Constant | Progress::Summands => Deduction::Equivalent(self),
            },
        }
    }
}

#[cfg(test)]
mod tests;
