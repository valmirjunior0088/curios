//! Fourier–Motzkin elimination over the integers: non-negative multipliers for the facts whose combination is a false constant, or an assignment satisfying every one of them.
//!
//! **Rows stay exact.** A row `Σ cᵢ·mᵢ + c₀ <= 0` over monomials is combined with another only by positive integer multiples, and divided only by the gcd of every coefficient, its constant and its multipliers together — never by its coefficients' gcd alone, whose rounding of the constant is the cut only an integer procedure may make. So a certificate is a consequence over the rationals, which is what the procedure claims, and every multiplier is an integer the proof scales a fact by.
//!
//! **A monomial is an unknown.** A product of atoms is eliminated as one variable, which is what linear arithmetic over products reads and all it reads.
//!
//! **A case split is opened only where it is needed.** Where the facts and the negated goal are satisfied, the next split its caller read — `b <= a`, for a truncated `a - b` — is opened and each case searched with its own facts added, as `omega` splits ([`plan`]); a refutation is then one combination per case ([`Certificate`]).
//!
//! **Deterministic, and capped in its own units.** The monomial eliminated next is the one whose elimination derives the fewest rows beyond those it removes, ties broken by the view's canonical order, and rows keep the order they were derived in — so one program gets one certificate on every machine. The cap counts derived rows across every case ([`DERIVED_ROWS`]); exhausting it refuses, as the reduction budget does, and never selects anything. Which monomial goes first changes what a search costs, not what it can find: elimination is complete over the rationals in any order.

use {
    curios_algebra::{Atom, LinearForm, Monomial},
    curios_num::{Integer, Natural},
    std::{cmp::Ordering, fmt},
};

/// The rows one search may derive before it refuses: a count in the theory's units, never the host's.
pub(super) const DERIVED_ROWS: usize = 4096;

/// What one search over `<= 0` rows concluded.
pub(super) enum Search {
    /// Multipliers, one per row, whose combination is a positive constant `<= 0`.
    Refuted(Vec<Natural>),
    /// A value for every unknown satisfying every row: an integer wherever one lies in the unknown's interval, a rational otherwise.
    Satisfied(Vec<(Monomial, Rational)>),
    /// The cap ran out first.
    Exhausted,
}

/// The two arms of a case split the search may open: the forms each adds to the ones it holds.
pub(super) struct Arms {
    /// Where the split's decision holds.
    pub(super) holds: Vec<LinearForm>,
    /// Where it fails.
    pub(super) fails: Vec<LinearForm>,
}

/// A refutation, per case of the splits it opened.
#[derive(Debug, PartialEq)]
pub(super) enum Certificate {
    /// One combination: a multiplier per form of its case — the forms, then each opened case's own in the order they were opened, then the negated goal.
    Leaf(Vec<Natural>),
    /// The next split, a refutation in each of its cases. Splits open in order along every path, so the one a `Split` opens is the first its path has not.
    Split {
        holds: Box<Certificate>,
        fails: Box<Certificate>,
    },
}

/// What a search over the facts and their case splits concluded.
pub(super) enum Plan {
    Certified(Certificate),
    /// An assignment satisfying every form of a case no split refutes.
    Satisfied(Vec<(Monomial, Rational)>),
    Exhausted,
}

/// Search `forms` with the negated goal last for a refutation, opening the splits in `splits` in order, each only where the search without it found an assignment: an assignment that leaves a truncated subtraction free may be one its definition excludes. One budget runs across every case.
pub(super) fn plan(
    forms: &[LinearForm],
    negated: Option<&LinearForm>,
    splits: &[Arms],
    budget: &mut usize,
) -> Plan {
    let with_goal = forms.iter().chain(negated).cloned().collect::<Vec<_>>();
    let assignment = match refute(&with_goal, budget) {
        Search::Refuted(multipliers) => return Plan::Certified(Certificate::Leaf(multipliers)),
        Search::Exhausted => return Plan::Exhausted,
        Search::Satisfied(assignment) => assignment,
    };
    let Some((cases, later)) = splits.split_first() else {
        return Plan::Satisfied(assignment);
    };
    let case = |extra: &[LinearForm], budget: &mut usize| {
        let with = forms.iter().chain(extra).cloned().collect::<Vec<_>>();
        plan(&with, negated, later, budget)
    };
    let holds = match case(&cases.holds, budget) {
        Plan::Certified(certificate) => certificate,
        other => return other,
    };
    let fails = match case(&cases.fails, budget) {
        Plan::Certified(certificate) => certificate,
        other => return other,
    };
    Plan::Certified(Certificate::Split {
        holds: Box::new(holds),
        fails: Box::new(fails),
    })
}

/// One row: `Σ coefficient · monomial + constant <= 0`, with the multipliers of the input rows it was derived from.
#[derive(Clone)]
struct Row {
    /// Canonically ordered, no zero coefficient.
    terms: Vec<(Monomial, Integer)>,
    constant: Integer,
    multipliers: Vec<Natural>,
}

/// The rows eliminated with one unknown, kept for the assignment.
struct Level {
    unknown: Monomial,
    rows: Vec<Row>,
}

/// Search `forms`, each read `form <= 0`, spending at most `budget` derived rows and leaving what is left of it.
pub(super) fn refute(forms: &[LinearForm], budget: &mut usize) -> Search {
    curios_profile::profile!("entailment::refute");
    let width = forms.len();
    let mut rows = forms
        .iter()
        .enumerate()
        .map(|(index, form)| {
            let mut multipliers = vec![Natural::from(0u32); width];
            multipliers[index] = Natural::from(1u32);
            let mut terms = form
                .terms
                .iter()
                .map(|(coefficient, monomial)| (monomial.clone(), coefficient.clone()))
                .collect::<Vec<_>>();
            terms.sort_by(|(left, _), (right, _)| canonical(left, right));
            Row {
                terms,
                constant: form.constant.clone(),
                multipliers,
            }
        })
        .collect::<Vec<_>>();
    let mut levels: Vec<Level> = Vec::new();

    loop {
        match settle(&mut rows) {
            Some(multipliers) => return Search::Refuted(multipliers),
            None if rows.is_empty() => break,
            None => {}
        }
        let unknown = next_unknown(&rows);
        let (mut upper, mut lower, mut derived) = (Vec::new(), Vec::new(), Vec::new());
        for row in rows {
            match coefficient(&row, &unknown).map(|c| c > Integer::from(0)) {
                Some(true) => upper.push(row),
                Some(false) => lower.push(row),
                None => derived.push(row),
            }
        }
        for up in &upper {
            for down in &lower {
                if *budget == 0 {
                    return Search::Exhausted;
                }
                *budget -= 1;
                derived.push(combine(up, down, &unknown));
            }
        }
        upper.append(&mut lower);
        levels.push(Level {
            unknown,
            rows: upper,
        });
        rows = derived;
    }

    Search::Satisfied(assign(&levels))
}

/// Divide every row exactly, drop the ones that say nothing, keep the stronger of two with one left side, and answer the multipliers of a row that is a false constant.
fn settle(rows: &mut Vec<Row>) -> Option<Vec<Natural>> {
    let zero = Integer::from(0);
    let mut kept: Vec<Row> = Vec::with_capacity(rows.len());
    for mut row in rows.drain(..) {
        if row.terms.is_empty() {
            if row.constant > zero {
                return Some(row.multipliers);
            }
            continue;
        }
        reduce(&mut row);
        match kept.iter_mut().find(|other| other.terms == row.terms) {
            // `Σ + c <= 0` with the larger `c` is the stronger row. An equal one keeps the earlier, derived from fewer.
            Some(other) if row.constant > other.constant => *other = row,
            Some(_) => {}
            None => kept.push(row),
        }
    }
    *rows = kept;
    None
}

/// Divide `row` by the gcd of every coefficient, its constant and its multipliers, which keeps it exact.
fn reduce(row: &mut Row) {
    let divisor = row
        .terms
        .iter()
        .map(|(_, coefficient)| coefficient.magnitude())
        .chain([row.constant.magnitude()])
        .chain(row.multipliers.iter().cloned())
        .fold(Natural::from(0u32), |gcd, value| gcd.gcd(&value));
    if divisor <= Natural::from(1u32) {
        return;
    }
    let by = Integer::from(divisor.clone());
    let exact = "a gcd greater than one divides every part";
    for (_, coefficient) in &mut row.terms {
        *coefficient = coefficient.div(&by).expect(exact);
    }
    row.constant = row.constant.div(&by).expect(exact);
    for multiplier in &mut row.multipliers {
        *multiplier = multiplier.div(&divisor).expect(exact);
    }
}

/// The unknown whose elimination derives the fewest rows beyond those it removes, ties broken by canonical order.
fn next_unknown(rows: &[Row]) -> Monomial {
    let mut unknowns: Vec<(Monomial, usize, usize)> = Vec::new();
    for row in rows {
        for (monomial, coefficient) in &row.terms {
            let index = match unknowns.iter().position(|(known, ..)| known == monomial) {
                Some(index) => index,
                None => {
                    unknowns.push((monomial.clone(), 0, 0));
                    unknowns.len() - 1
                }
            };
            match *coefficient > Integer::from(0) {
                true => unknowns[index].1 += 1,
                false => unknowns[index].2 += 1,
            }
        }
    }
    // Eliminating an unknown bounded `up` times above and `down` times below derives `up · down` rows and removes `up + down`.
    let growth = |up: usize, down: usize| (up * down) as i128 - (up + down) as i128;
    unknowns
        .into_iter()
        .min_by(
            |(left, left_up, left_down), (right, right_up, right_down)| {
                growth(*left_up, *left_down)
                    .cmp(&growth(*right_up, *right_down))
                    .then_with(|| canonical(left, right))
            },
        )
        .map(|(unknown, ..)| unknown)
        .expect("a row with a term names an unknown")
}

/// `up`, bounding `unknown` above, and `down`, below, combined so `unknown` cancels: `|b|·up + a·down`, `a` and `b` their coefficients of it.
fn combine(up: &Row, down: &Row, unknown: &Monomial) -> Row {
    let a = coefficient(up, unknown).expect("the upper row names the unknown");
    let b = -coefficient(down, unknown).expect("the lower row names the unknown");

    let mut terms: Vec<(Monomial, Integer)> = Vec::new();
    let scaled = up
        .terms
        .iter()
        .map(|(monomial, c)| (monomial, c.clone() * b.clone()))
        .chain(
            down.terms
                .iter()
                .map(|(monomial, c)| (monomial, c.clone() * a.clone())),
        );
    for (monomial, value) in scaled {
        match terms.iter_mut().find(|(known, _)| known == monomial) {
            Some((_, sum)) => *sum = sum.clone() + value,
            None => terms.push((monomial.clone(), value)),
        }
    }
    terms.retain(|(_, value)| !value.is_zero());
    terms.sort_by(|(left, _), (right, _)| canonical(left, right));

    let (a_natural, b_natural) = (a.magnitude(), b.magnitude());
    Row {
        terms,
        constant: up.constant.clone() * b + down.constant.clone() * a,
        multipliers: up
            .multipliers
            .iter()
            .zip(&down.multipliers)
            .map(|(u, d)| u.clone() * b_natural.clone() + d.clone() * a_natural.clone())
            .collect(),
    }
}

fn coefficient(row: &Row, unknown: &Monomial) -> Option<Integer> {
    row.terms
        .iter()
        .find(|(monomial, _)| monomial == unknown)
        .map(|(_, coefficient)| coefficient.clone())
}

/// Monomials by their atoms, lexicographically by rank and then by the order they were handed out — the view's own order.
fn canonical(left: &Monomial, right: &Monomial) -> Ordering {
    let key = |atom: &Atom| (atom.rank(), atom.index());
    left.atoms()
        .iter()
        .map(key)
        .cmp(right.atoms().iter().map(key))
}

/// A value for every unknown, the last eliminated first: each is bounded only by unknowns eliminated after it, which are assigned by then.
fn assign(levels: &[Level]) -> Vec<(Monomial, Rational)> {
    let mut values: Vec<(Monomial, Rational)> = Vec::new();
    for level in levels.iter().rev() {
        let mut lower: Option<Rational> = None;
        let mut upper: Option<Rational> = None;
        for row in &level.rows {
            // `a·x + rest <= 0`: `x <= -rest/a` where `a > 0`, and `x >= -rest/a` where `a < 0`.
            let mut rest = Rational::from(row.constant.clone());
            let mut own = Integer::from(0);
            for (monomial, coefficient) in &row.terms {
                match *monomial == level.unknown {
                    true => own = coefficient.clone(),
                    false => {
                        let value = values
                            .iter()
                            .find(|(known, _)| known == monomial)
                            .map(|(_, value)| value.clone())
                            .unwrap_or_default();
                        rest = rest.plus(&value.times(coefficient));
                    }
                }
            }
            let bound = rest.negated().divided(&own);
            match own > Integer::from(0) {
                true => upper = Some(upper.map_or(bound.clone(), |upper| upper.min(bound))),
                false => lower = Some(lower.map_or(bound.clone(), |lower| lower.max(bound))),
            }
        }
        values.push((level.unknown.clone(), Rational::choose(lower, upper)));
    }
    values
}

/// An exact rational, for an assignment: `numerator / denominator` in lowest terms, the denominator positive. A counterexample the search finds is over the rationals, and one that is not an integer is reported as the fraction it is.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Rational {
    numerator: Integer,
    denominator: Integer,
}

impl Default for Rational {
    fn default() -> Self {
        Rational::from(Integer::from(0))
    }
}

impl From<Integer> for Rational {
    fn from(numerator: Integer) -> Self {
        Rational {
            numerator,
            denominator: Integer::from(1),
        }
    }
}

impl Rational {
    /// `numerator / denominator`, reduced; the denominator nonzero.
    pub(super) fn of(numerator: impl Into<Integer>, denominator: impl Into<Integer>) -> Self {
        let (numerator, denominator) = (numerator.into(), denominator.into());
        let (numerator, denominator) = match denominator < Integer::from(0) {
            true => (-numerator, -denominator),
            false => (numerator, denominator),
        };
        let gcd = Integer::from(numerator.magnitude().gcd(&denominator.magnitude()));
        match gcd.is_zero() || gcd == Integer::from(1) {
            true => Rational {
                numerator,
                denominator,
            },
            false => Rational {
                numerator: numerator.div(&gcd).expect("a nonzero gcd"),
                denominator: denominator.div(&gcd).expect("a nonzero gcd"),
            },
        }
    }

    pub(super) fn plus(&self, other: &Rational) -> Rational {
        Rational::of(
            self.numerator.clone() * other.denominator.clone()
                + other.numerator.clone() * self.denominator.clone(),
            self.denominator.clone() * other.denominator.clone(),
        )
    }

    pub(super) fn times(&self, factor: &Integer) -> Rational {
        Rational::of(
            self.numerator.clone() * factor.clone(),
            self.denominator.clone(),
        )
    }

    fn negated(&self) -> Rational {
        Rational::of(-self.numerator.clone(), self.denominator.clone())
    }

    /// `self / divisor`, the divisor nonzero.
    fn divided(&self, divisor: &Integer) -> Rational {
        Rational::of(
            self.numerator.clone(),
            self.denominator.clone() * divisor.clone(),
        )
    }

    /// The greatest integer not above it: truncating division, one less where a negative value had a remainder.
    pub(super) fn floor(&self) -> Integer {
        let quotient = self
            .numerator
            .div(&self.denominator)
            .expect("a positive denominator");
        let exact = quotient.clone() * self.denominator.clone() == self.numerator;
        match exact || self.numerator >= Integer::from(0) {
            true => quotient,
            false => quotient - Integer::from(1),
        }
    }

    /// The least integer not below it.
    pub(super) fn ceil(&self) -> Integer {
        -self.negated().floor()
    }

    /// A value for an unknown bounded by `lower` and `upper`, either absent: zero where it lies between them, else the integer nearest it — `ceil(lower)` where that does not pass `upper`, `floor(upper)` where there is no lower bound — and `lower` itself where no integer fits, which is the rational counterexample the report states as a fraction.
    pub(super) fn choose(lower: Option<Rational>, upper: Option<Rational>) -> Rational {
        let zero = Rational::default();
        let above = |value: &Rational| lower.as_ref().is_none_or(|lower| lower <= value);
        let below = |value: &Rational| upper.as_ref().is_none_or(|upper| value <= upper);
        if above(&zero) && below(&zero) {
            return zero;
        }
        match (&lower, &upper) {
            (Some(lower), _) => {
                let ceiling = Rational::from(lower.ceil());
                match below(&ceiling) {
                    true => ceiling,
                    false => lower.clone(),
                }
            }
            (None, Some(upper)) => Rational::from(upper.floor()),
            (None, None) => zero,
        }
    }
}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rational {
    /// By cross-multiplication, both denominators being positive.
    fn cmp(&self, other: &Self) -> Ordering {
        let this = self.numerator.clone() * other.denominator.clone();
        let that = other.numerator.clone() * self.denominator.clone();
        this.cmp(&that)
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.denominator == Integer::from(1) {
            true => write!(f, "{}", self.numerator),
            false => write!(f, "{}/{}", self.numerator, self.denominator),
        }
    }
}
