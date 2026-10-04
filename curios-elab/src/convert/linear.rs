//! A metavariable an equation over `Nat` or `Int` is linear in, solved as the one term that satisfies it.
//!
//! **What is solved.** Two sums of products the carriers' readers settle nothing of, with one unsolved metavariable standing in them as a factor: `?w * (y + z)` against `x * y + x * z`. Read as the difference of its sides over the equation's atoms, the equation is `A · ?w + B = 0` with `A` and `B` free of `?w`, and its solution is `-B / A` where that division is exact: here `x`, and `x + 1` against `(x + 1) * y + (x + 1) * z`, however either side's summands are written.
//!
//! **Why this is no pick.** Conversion holds two sums of products equal where their normal forms agree, so over the equation's atoms as indeterminates the equation is one in a ring of polynomials, which has no zero divisors: `A · w = -B` has at most one solution, and any term that solves the equation converts with it. A commutative operation's operands are never compared by position because the order they stand in would then choose between solutions; here there is one.
//!
//! **Proposed, never trusted.** The quotient is put to conversion as `?w ≡ term`, in a bracket that is rolled back unless it converts, so the metavariable is solved by the route every solution takes — its scope, its occurrences and its type all checked there — and the equation it came from is then compared again as any pair is. A division that is not exact, a quotient that is wrong, a term outside the metavariable's scope each leave the equation where it was. Nothing here is in the trusted base: the kernel is handed the solved term and decides the equation by its own conversion.
//!
//! **Where it declines**, leaving the equation parked as it would have been: a second unsolved metavariable, since one equation does not determine two unknowns and another may yet solve one; a metavariable inside an atom, `f(?m)`, or at a power, `?w * ?w`, neither of which is linear; and a quotient no term of the metavariable's carrier spells.

use {
    super::convert,
    crate::{Context, zonk_solved_term_metas},
    curios_algebra::{Atom, Carrier, LinearForm, Monomial, Operation, Part},
    curios_core::{
        Intrinsic, LinearViews, Nat, Probe, Produced, ReduceError, Subterm, Term, int_normalize,
    },
    curios_num::Integer,
    std::cmp::Ordering,
};

#[cfg(test)]
mod tests;

/// A polynomial over a view's atoms with integer coefficients: each monomial once, its atoms in [`atom_order`], a constant being the monomial over no atom.
type Polynomial = Vec<(Integer, Vec<Atom>)>;

/// How many terms a quotient may have before the division gives up. A proposal's limit and not the theory's: past it the equation stays parked, as an equation this module does not read does.
const QUOTIENT_TERMS: usize = 64;

/// Whether `this ≡ that`, two intrinsics of one carrier the chain settled nothing of, was linear in its one unsolved metavariable and that metavariable is now solved to the equation's solution. `false` leaves everything as it was.
pub(super) fn solved_linearly(
    context: &mut Context,
    this: &Intrinsic,
    that: &Intrinsic,
) -> Result<bool, ReduceError> {
    // No unsolved metavariable, no unknown: the pair is what it is, and most pairs that reach here are.
    let holds_one = |intrinsic: &Intrinsic| {
        Term::intrinsic(intrinsic.clone())
            .metavars()
            .iter()
            .any(|id| context.metavar_solution(*id).is_none())
    };
    if !holds_one(this) && !holds_one(that) {
        return Ok(false);
    }
    let Some(carrier) = carrier(context, this) else {
        return Ok(false);
    };
    let this = normalized(context, carrier, this)?;
    let that = normalized(context, carrier, that)?;
    let Some(equation) = Intrinsic::comparison(carrier, Operation::Equal, this, that) else {
        return Ok(false);
    };
    let mut views = LinearViews::default();
    let Some(view) = views.view(&equation) else {
        return Ok(false);
    };
    let form = view.form();
    let Some(unknown) = unknown(context, &views, form) else {
        return Ok(false);
    };
    let Some((coefficient, rest)) = split(form, unknown) else {
        return Ok(false);
    };
    let numerator = rest
        .into_iter()
        .map(|(coefficient, atoms)| (-coefficient, atoms))
        .collect();
    let Some(quotient) = divide(numerator, &coefficient) else {
        return Ok(false);
    };

    // A natural unknown is solved by a natural: every atom of the quotient one, which the spelling then checks the coefficients of.
    let natural = form.nonnegative.contains(&unknown);
    if natural
        && quotient
            .iter()
            .any(|(_, atoms)| atoms.iter().any(|atom| !form.nonnegative.contains(atom)))
    {
        return Ok(false);
    }
    let (solution_carrier, type_) = match natural {
        true => (Carrier::Natural, Intrinsic::NatType),
        false => (Carrier::Integer, Intrinsic::IntType),
    };
    let Some(solution) = views.spell(solution_carrier, &part(quotient), &form.nonnegative) else {
        return Ok(false);
    };

    // Hand-paired rather than bracketed by a closure, as the witness probe's bracket is: this sits on conversion's recursion.
    let mark = context.solution_mark();
    let solved = convert(
        context,
        &Term::intrinsic(type_),
        views.term(unknown),
        &solution,
    );
    if !matches!(solved, Ok(true)) {
        context.rollback_solutions(mark);
    }
    context.end_solutions(mark);
    solved
}

/// The carrier `intrinsic` produces a value of, where it is `Nat` or `Int`.
fn carrier(context: &Context, intrinsic: &Intrinsic) -> Option<Carrier> {
    let Produced::Fixed(type_) = intrinsic.signature(&context.syntax()).produced else {
        return None;
    };
    match &*type_ {
        Subterm::Intrinsic(Intrinsic::NatType) => Some(Carrier::Natural),
        Subterm::Intrinsic(Intrinsic::IntType) => Some(Carrier::Integer),
        _ => None,
    }
}

/// One side as the view reads it: its solved metavariables spliced, and every product of two sums distributed, since the view distributes none and reads a standing product as one atom.
fn normalized(
    context: &mut Context,
    carrier: Carrier,
    intrinsic: &Intrinsic,
) -> Result<Term, ReduceError> {
    let term = zonk_solved_term_metas(context, &Term::intrinsic(intrinsic.clone()));
    let normalized = match carrier {
        Carrier::Natural => Nat::normalize(context, term.clone()),
        _ => int_normalize(context, term.clone()),
    };
    Ok(normalized.probed()?.unwrap_or(term))
}

/// The one atom of `form` that is an unsolved metavariable, where no other atom holds one.
fn unknown(context: &Context, views: &LinearViews, form: &LinearForm) -> Option<Atom> {
    let unsolved = |term: &Term| {
        term.metavars()
            .iter()
            .any(|id| context.metavar_solution(*id).is_none())
    };
    let mut unknown = None;
    for (_, monomial) in &form.terms {
        for atom in monomial.atoms() {
            let term = views.term(*atom);
            if !unsolved(term) {
                continue;
            }
            if !matches!(&**term, Subterm::Metavar(_)) {
                return None;
            }
            match unknown {
                None => unknown = Some(*atom),
                Some(known) if known == *atom => {}
                Some(_) => return None,
            }
        }
    }
    unknown
}

/// `form` as `coefficient · unknown + rest`, where `unknown` stands in each of its monomials once; `None` where it stands at a power.
fn split(form: &LinearForm, unknown: Atom) -> Option<(Polynomial, Polynomial)> {
    let mut coefficient = Polynomial::new();
    let mut rest = Polynomial::new();
    if !form.constant.is_zero() {
        rest.push((form.constant.clone(), Vec::new()));
    }
    for (scale, monomial) in &form.terms {
        let mut atoms = monomial.atoms().to_vec();
        atoms.sort_by(|left, right| atom_order(*left, *right));
        match atoms.iter().filter(|atom| **atom == unknown).count() {
            0 => rest.push((scale.clone(), atoms)),
            1 => {
                atoms.retain(|atom| *atom != unknown);
                coefficient.push((scale.clone(), atoms));
            }
            _ => return None,
        }
    }
    Some((coefficient, rest))
}

/// `numerator / divisor` where the division is exact: the polynomial whose product with `divisor` is `numerator`. `None` where there is none, or where it has more than [`QUOTIENT_TERMS`] terms.
///
/// Division by the leading term, under an order of monomials that multiplication respects — by degree, then by their atoms — so the leading monomial of an exact quotient's product is the product of the leading monomials, and each step retires the remainder's.
fn divide(mut remainder: Polynomial, divisor: &Polynomial) -> Option<Polynomial> {
    let (lead_scale, lead_atoms) = leading(divisor)?.clone();
    let mut quotient = Polynomial::new();
    for _ in 0..=QUOTIENT_TERMS {
        let Some((scale, atoms)) = leading(&remainder).cloned() else {
            return Some(quotient);
        };
        let over = without(&atoms, &lead_atoms)?;
        if !scale.rem(&lead_scale).ok()?.is_zero() {
            return None;
        }
        let scale = scale.div(&lead_scale).ok()?;
        for (factor, atoms) in divisor {
            subtract(
                &mut remainder,
                scale.clone() * factor.clone(),
                times(&over, atoms),
            );
        }
        quotient.push((scale, over));
    }
    None
}

/// The term of `polynomial` whose monomial is greatest, `None` for the zero polynomial.
fn leading(polynomial: &Polynomial) -> Option<&(Integer, Vec<Atom>)> {
    polynomial
        .iter()
        .max_by(|(_, left), (_, right)| monomial_order(left, right))
}

/// `monomial` with the atoms of `divisor` taken out, each once: their quotient, where `divisor` divides it.
fn without(monomial: &[Atom], divisor: &[Atom]) -> Option<Vec<Atom>> {
    let mut left = monomial.to_vec();
    for atom in divisor {
        let at = left.iter().position(|candidate| candidate == atom)?;
        left.remove(at);
    }
    Some(left)
}

/// The product of two monomials, its atoms in order.
fn times(left: &[Atom], right: &[Atom]) -> Vec<Atom> {
    let mut atoms = [left, right].concat();
    atoms.sort_by(|left, right| atom_order(*left, *right));
    atoms
}

/// `polynomial` less `scale · monomial`, a term that cancels dropped.
fn subtract(polynomial: &mut Polynomial, scale: Integer, monomial: Vec<Atom>) {
    match polynomial.iter().position(|(_, atoms)| *atoms == monomial) {
        Some(at) => {
            let left = polynomial[at].0.clone() - scale;
            match left.is_zero() {
                true => {
                    polynomial.remove(at);
                }
                false => polynomial[at].0 = left,
            }
        }
        None => polynomial.push((-scale, monomial)),
    }
}

/// Atoms by rank, then by the order their reader handed them out in: the view's own order.
fn atom_order(left: Atom, right: Atom) -> Ordering {
    left.rank()
        .cmp(&right.rank())
        .then(left.index().cmp(&right.index()))
}

/// Monomials by degree, then by their atoms in order. Multiplying two monomials by a third keeps them in order, which is what a division by the leading term needs.
fn monomial_order(left: &[Atom], right: &[Atom]) -> Ordering {
    left.len().cmp(&right.len()).then_with(|| {
        left.iter()
            .zip(right)
            .map(|(left, right)| atom_order(*left, *right))
            .find(|ordering| ordering.is_ne())
            .unwrap_or(Ordering::Equal)
    })
}

/// A polynomial as the combination a view spells: its constant apart, each other monomial a product of atoms.
fn part(polynomial: Polynomial) -> Part {
    let mut constant = Integer::from(0);
    let mut terms = Vec::new();
    for (scale, atoms) in polynomial {
        match atoms.is_empty() {
            true => constant = constant + scale,
            false => terms.push((scale, Monomial::product(atoms))),
        }
    }
    Part { constant, terms }
}
