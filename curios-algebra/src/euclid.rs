//! Euclid's identity read in the direction that shrinks a sum: `k` remainders `x % d` beside `k` copies of the divisor times the matching quotient, `d · (x / d)`, are `k · x`.
//!
//! **Unconditional, which is what admits it.** `d · (x / d) + x % d = x` holds for every `x` and every nonzero `d`, under flooring and under truncation alike, so no value of an atom can falsify a recombination; the caller's division exists only over its proof that the divisor is nonzero. The caller recognizes a remainder and matches its multiple's monomials against a combination — both term-level, the second proof-insensitive — and this module decides how many copies the combination holds and rewrites it.
//!
//! Each recombination removes a remainder and adds the summands of its dividend, a strict subterm, so a caller that recombines until nothing is left terminates.

use {
    crate::{Coefficient, Combination, Summand},
    curios_num::{Integer, Natural},
    std::ops::{Mul, Sub},
};

/// One application of Euclid's identity to a combination: how many copies of the pair it takes, and what each summand it reads gives up, by position in the combination.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Recombination<C> {
    pub copies: C,
    pub spent: Vec<(usize, C)>,
}

/// One monomial of a remainder's multiple, as its caller matched it: the coefficient one copy of the pair needs, and the summand of the combination holding that monomial — its position and its coefficient — or `None` where the combination holds none.
pub type Wanted<C> = (C, Option<(usize, C)>);

impl Recombination<Natural> {
    /// The copies of a remainder's pair a `Nat` combination holds: the remainder at `remainder` held `held` times, and each monomial of its multiple as `wanted`. `None` where a monomial of the multiple is not held, or not a single copy is.
    pub fn natural(remainder: usize, held: &Natural, wanted: &[Wanted<Natural>]) -> Option<Self> {
        let mut copies = held.clone();
        let mut matched = Vec::with_capacity(wanted.len());
        for (per_copy, holding) in wanted {
            let (at, available) = holding.as_ref()?;
            copies = copies.min(available / per_copy);
            matched.push((*at, per_copy.clone()));
        }
        if copies.is_zero() {
            return None;
        }

        let mut spent = vec![(remainder, copies.clone())];
        spent.extend(
            matched
                .into_iter()
                .map(|(at, per_copy)| (at, per_copy * &copies)),
        );
        Some(Recombination { copies, spent })
    }
}

impl Recombination<Integer> {
    /// The copies of a remainder's pair an `Int` combination holds, signed: the remainder at `remainder` held `held` times, and each monomial of its multiple as `wanted`.
    ///
    /// **A copy counts only where every coefficient agrees in sign with it.** A group lets any combination be rewritten around `x`, but a recombination that left a negative remainder of a multiple behind would trade one spelling for a longer one; taking only the copies the combination actually holds is what keeps the rewrite a shrinking one, and keeps `x - d · (x / d)` and `x % d` the two terms they were — an incompleteness, never a false equation.
    pub fn integer(remainder: usize, held: &Integer, wanted: &[Wanted<Integer>]) -> Option<Self> {
        let zero = Integer::from(0);
        let positive = *held > zero;
        let mut copies = held.magnitude();
        let mut matched = Vec::with_capacity(wanted.len());
        for (per_copy, holding) in wanted {
            let (at, available) = holding.as_ref()?;
            if (*available > zero) != (positive == (*per_copy > zero)) {
                return None;
            }
            copies = copies.min(available.magnitude() / per_copy.magnitude());
            matched.push((*at, per_copy.clone()));
        }
        if copies.is_zero() {
            return None;
        }

        let copies = match positive {
            true => Integer::from(copies),
            false => -Integer::from(copies),
        };
        let mut spent = vec![(remainder, copies.clone())];
        spent.extend(
            matched
                .into_iter()
                .map(|(at, per_copy)| (at, per_copy * copies.clone())),
        );
        Some(Recombination { copies, spent })
    }
}

impl<C, O> Combination<C, O>
where
    C: Coefficient + Sub<Output = C> + Mul<Output = C>,
{
    /// This combination with `recombination` applied: every summand it reads gives up what it spent, `copies` times `dividend` is added — its constant into this one's, its summands after this one's — and the result is collected again, a summand spent to nothing dropping out where it stood.
    ///
    /// `dividend` must be read over the same atoms as this combination, so that its summands meet this one's.
    pub fn recombined(
        mut self,
        recombination: Recombination<C>,
        dividend: Combination<C, O>,
    ) -> Self {
        let Recombination { copies, spent } = recombination;
        for (index, amount) in spent {
            let summand = &mut self.summands[index];
            summand.coefficient = summand.coefficient.clone() - amount;
        }
        let constant = self.constant + copies.clone() * dividend.constant;
        let scaled = dividend.summands.into_iter().map(|summand| Summand {
            coefficient: copies.clone() * summand.coefficient,
            ..summand
        });
        let kept = self
            .summands
            .into_iter()
            .filter(|summand| !summand.coefficient.is_zero());
        Combination::collect(constant, kept.chain(scaled)).without_zeros()
    }
}
