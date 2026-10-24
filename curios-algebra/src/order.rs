//! Comparing two numbers under atoms: which orderings they may still take, and the facts that narrow it.

#[cfg(test)]
mod tests;

use {
    curios_num::{Integer, Natural},
    std::cmp::Ordering,
};

/// The orderings two operands may still take: `Lt`, `Eq` and `Gt` pin one; `Le` and `Ge` record a *non-strict* bound the operands force without pinning equality — `succ x ≥ 1` — so `<` and `>=` decide where `==` still cannot; `Ne` is forced unequal with the order undecided, which divisibility proves; and `Stuck` is all three. Every comparison of the family reads this one verdict, each mapping it to a `Bool` or to nothing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Ne,
    Stuck,
}

/// One side of a `Nat` comparison once cancellation has taken off what both sides shared: its floor, and whether a symbolic part is left above it.
#[derive(Clone, Copy, Debug)]
pub struct Side<'a> {
    pub floor: &'a Natural,
    pub symbolic: bool,
}

impl Comparison {
    /// The verdict an ordering of two values is.
    pub fn of(ordering: Ordering) -> Self {
        match ordering {
            Ordering::Less => Comparison::Lt,
            Ordering::Equal => Comparison::Eq,
            Ordering::Greater => Comparison::Gt,
        }
    }

    /// The same verdict read from the other side: `a < b` is `b > a`.
    pub fn mirrored(self) -> Self {
        match self {
            Comparison::Lt => Comparison::Gt,
            Comparison::Gt => Comparison::Lt,
            Comparison::Le => Comparison::Ge,
            Comparison::Ge => Comparison::Le,
            unchanged => unchanged,
        }
    }

    /// This verdict met with the fact that the operands are unequal: the orderings left once `Eq` is struck.
    pub fn unequal(self) -> Self {
        match self {
            Comparison::Stuck => Comparison::Ne,
            Comparison::Le => Comparison::Lt,
            Comparison::Ge => Comparison::Gt,
            decided => decided,
        }
    }

    /// Two `Nat`s after cancellation, read off their floors alone: with one symbolic part standing on both sides — `same` — the floors decide; otherwise whichever side keeps successors past the other's floor is larger exactly when the other side has nothing above its floor, equal floors with one side bare give the non-strict bound, and anything else is undecided. A symbolic part is at least zero, which is the only fact read of one.
    pub fn of_floors(left: Side<'_>, right: Side<'_>, same: bool) -> Self {
        if same {
            return Comparison::of(left.floor.cmp(right.floor));
        }
        match left.floor.cmp(right.floor) {
            Ordering::Greater if !right.symbolic => Comparison::Gt,
            Ordering::Less if !left.symbolic => Comparison::Lt,
            Ordering::Equal if !left.symbolic => Comparison::Le,
            Ordering::Equal if !right.symbolic => Comparison::Ge,
            _ => Comparison::Stuck,
        }
    }

    /// A side bounded by `bound` against a `limit` the other side is at least — a literal, or the floor under whatever stands above it: a bound below the limit forces `<` for every value the two sides take, and a bound met exactly forces `<=`, the non-strict verdict `<=` reads and `<` cannot — so `x % 7 <= 6` decides as `x % 7 < 7` does, and `x % 8 <= y + 7` as `x % 8 <= 7` does.
    pub fn below(bound: &Natural, limit: &Natural) -> Self {
        match bound.cmp(limit) {
            Ordering::Less => Comparison::Lt,
            Ordering::Equal => Comparison::Le,
            Ordering::Greater => Comparison::Stuck,
        }
    }

    /// The verdict a side forces through an operand it never exceeds, given this verdict of that operand against the other side: at most there is at most here, and strictly so where either step is strict. `None` where the operand's verdict forces nothing about the side.
    pub fn through_dominator(self, strict: bool) -> Option<Self> {
        match (self, strict) {
            (Comparison::Lt, _) | (Comparison::Le | Comparison::Eq, true) => Some(Comparison::Lt),
            (Comparison::Le | Comparison::Eq, false) => Some(Comparison::Le),
            _ => None,
        }
    }

    /// The verdict a side forces through an operand it stays strictly below, given this verdict of that operand against the *successor* of the other side: strictly below an operand that is at most one past the other side is at most the other side, so `x % 8 <= y + 7` decides by `8 <= y + 8`. `None` where the operand's verdict forces nothing.
    ///
    /// What [`Comparison::through_dominator`] leaves a strict dominator: there the operand is compared with the other side itself, which decides `<` and misses the `<=` one step short of it, so the two spellings of one order fact across the `<`/`<=` seam would part.
    pub fn through_strict_dominator(self) -> Option<Self> {
        match self {
            Comparison::Lt | Comparison::Le | Comparison::Eq => Some(Comparison::Le),
            _ => None,
        }
    }
}

/// Whether two sums are equal at no value because their floors differ modulo the gcd of their coefficients: every symbolic summand is a coefficient times an integer, so each side is its floor modulo that gcd whatever the atoms take, and two floors apart there never meet — `2 · x + 1` against `2 · y`, the first step of the omega test. A summand with no literal coefficient has coefficient `1`, which takes the gcd to `1` and the test quiet exactly where it has nothing to say; two literals — no coefficient at all — are left to the ordering, which already decided them.
pub fn apart_modulo(
    floors: (&Natural, &Natural),
    coefficients: impl IntoIterator<Item = Natural>,
) -> bool {
    let divisor = coefficients
        .into_iter()
        .fold(Natural::zero(), |divisor, coefficient| {
            divisor.gcd(&coefficient)
        });

    !divisor.is_zero() && !divisor.is_one() && floors.0 % &divisor != floors.1 % &divisor
}

/// A sum's floor and the greatest common divisor of its symbolic coefficients, read off its layers without distributing a product. Every symbolic summand is a coefficient times an integer, so the sum is its floor modulo the divisor whatever its atoms take: a literal is its value over no symbolic part, an atom is zero over the divisor one, a sum of two is the floors' sum over the divisors' gcd, and a product of two is the floors' product over the gcd of each floor's magnitude times the other's divisor and of the two divisors. Every coefficient a distribution would write is a multiple of one of those three, and merging like terms keeps each a multiple of what it was, so the divisor read here divides the distributed sum's own, and what it holds apart the distributed sum's would. The floor is an integer so that one reading serves `Nat` and `Int`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Modular {
    pub floor: Integer,
    pub divisor: Natural,
}

impl Modular {
    /// A literal: its value, over no symbolic part.
    pub fn literal(value: Integer) -> Self {
        Self {
            floor: value,
            divisor: Natural::zero(),
        }
    }

    /// An atom: nothing known of its value, under a coefficient of one.
    pub fn atom() -> Self {
        Self {
            floor: Integer::from(0),
            divisor: Natural::one(),
        }
    }

    /// The reading of two summed.
    pub fn sum(&self, other: &Self) -> Self {
        Self {
            floor: self.floor.clone() + other.floor.clone(),
            divisor: self.divisor.gcd(&other.divisor),
        }
    }

    /// The reading of two multiplied.
    pub fn product(&self, other: &Self) -> Self {
        let across = (self.floor.magnitude() * other.divisor.clone())
            .gcd(&(other.floor.magnitude() * self.divisor.clone()));
        Self {
            floor: self.floor.clone() * other.floor.clone(),
            divisor: across.gcd(&(self.divisor.clone() * other.divisor.clone())),
        }
    }

    /// Whether two sums are equal at no value: their floors differ modulo the gcd of their divisors ([`apart_modulo`]), below zero as above it, since the argument needs every monomial to be an integer and nothing more.
    pub fn apart(&self, other: &Self) -> bool {
        let difference = (self.floor.clone() - other.floor.clone()).magnitude();
        apart_modulo(
            (&difference, &Natural::zero()),
            [self.divisor.clone(), other.divisor.clone()],
        )
    }
}
