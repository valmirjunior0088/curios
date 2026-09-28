//! Comparing two numbers under atoms: which orderings they may still take, and the facts that narrow it.

use {curios_num::Natural, std::cmp::Ordering};

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

    /// A side bounded by `bound` against a literal `limit`: a bound below the limit forces `<` for every value the side takes, and a bound met exactly forces `<=`, the non-strict verdict `<=` reads and `<` cannot — so `x % 7 <= 6` decides as `x % 7 < 7` does.
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
