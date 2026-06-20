//! Rung C of the indexed-union ladder: *inversion*. When a union match's
//! scrutinee carries indices in constructor form, first-order unification of
//! the actual indices against each case's target indices either pins arm
//! binders to the values they are forced to take (`m + 1 ~ n + 1` pins
//! `m := n`) or proves the case unreachable outright (`0` against `n + 1`) —
//! which is what lets the arm be omitted, checker-verified, with no
//! `impossible` keyword.

use {
    super::{Context, Error, Peel, Subterm, Telescope, Term, UnionType, peel_prim, reduce_with},
    std::collections::BTreeSet,
};

/// The outcome of inversion: either every index position decomposed (or
/// refused) cleanly, yielding the forced arm-binder solutions, or some
/// position clashed definitely and the arm is unreachable.
pub enum Invert {
    Solved(Vec<(String, Term)>),
    Impossible,
}

/// One (sub)problem's outcome inside the inversion walk.
enum Step {
    Ok,
    Clash,
    Refuse,
}

/// Open a constructor's instantiated telescope with `vars` and read the
/// terminal's index expressions.
pub fn case_target_indices(mut telescope: Telescope<Term>, vars: &[Term]) -> Vec<Term> {
    for var in vars {
        telescope = match telescope {
            Telescope::Cons(_, rest) => rest.open(&[var]),
            Telescope::Done(_) => unreachable!("telescope arity matches the binder count"),
        };
    }

    match telescope {
        Telescope::Done(terminal) => match &**terminal {
            Subterm::UnionType(UnionType { indices, .. }) => indices.clone(),
            _ => unreachable!("constructor terminal is its union type"),
        },
        Telescope::Cons(..) => unreachable!("telescope arity matches the binder count"),
    }
}

/// The unifier, deliberately tiny: first-order, constructor-form, linear.
/// Per index position it decomposes matching constructor forms (`Nat`
/// successor spines, variants by tag, tuples pointwise), solves an unbound
/// arm binder against the rigid term it is forced to equal, and declares a
/// clash on distinct constructors. Everything else — non-linear binders
/// (shared across positions, the K-shaped reasoning this refuses),
/// metavariables, opaque applications, key-shaped actuals at the top of a
/// position (Rung B's territory) — it *refuses*: the arm stays mandatory and
/// the binder unsolved.
pub fn invert_indices(
    context: &mut Context,
    actuals: &[Term],
    targets: &[Term],
    flex: &[String],
) -> Result<Invert, Error> {
    let mut seen = BTreeSet::new();
    let mut tainted = BTreeSet::new();

    for target in targets {
        for name in target.free_vars() {
            if flex.contains(&name) && !seen.insert(name.clone()) {
                tainted.insert(name);
            }
        }
    }

    let mut solutions = Vec::new();

    for (actual, target) in actuals.iter().zip(targets) {
        let mut position = Vec::new();

        match unify_index(context, actual, target, flex, &tainted, true, &mut position)? {
            Step::Clash => return Ok(Invert::Impossible),
            // A refused position contributes nothing — solutions found on
            // the way in are discarded with it, conservatively.
            Step::Refuse => {}
            Step::Ok => solutions.extend(position),
        }
    }

    Ok(Invert::Solved(solutions))
}

fn unify_index(
    context: &mut Context,
    actual: &Term,
    target: &Term,
    flex: &[String],
    tainted: &BTreeSet<String>,
    top: bool,
    solutions: &mut Vec<(String, Term)>,
) -> Result<Step, Error> {
    let actual = reduce_with(context, actual)?;
    let target = reduce_with(context, target)?;

    // Solve a flex arm binder against the rigid term it is forced to equal —
    // forced because every decomposition step above it was injective.
    if let Subterm::Var(var) = &*target
        && var.as_bound().is_none()
        && flex.iter().any(|l| l == var.unwrap())
    {
        let label = var.unwrap().to_string();

        if tainted.contains(&label) || solutions.iter().any(|(l, _)| *l == label) {
            return Ok(Step::Refuse);
        }
        // At the top of a position a key-shaped actual is Rung B's: it was
        // refined *to* this binder, and solving the binder back to it would
        // tie a reduction cycle. A flex actual (metavariable) is refused
        // outright.
        if top && matches!(&*actual, Subterm::Var(_) | Subterm::Proj(_))
            || matches!(&*actual, Subterm::Metavar(_))
        {
            return Ok(Step::Refuse);
        }
        // The forced value must be *rigid*: a key-shaped actual that Rung B
        // already refined reduces back into this very arm's binders, and
        // aliasing a binder to a term mentioning the arm's binders (itself
        // included — `m := m`) would tie a reduction cycle.
        if actual
            .free_vars()
            .iter()
            .any(|free| flex.iter().any(|l| l == free))
        {
            return Ok(Step::Refuse);
        }

        solutions.push((label, actual.clone()));

        return Ok(Step::Ok);
    }

    match (&*actual, &*target) {
        (Subterm::Metavar(_), _) | (_, Subterm::Metavar(_)) => Ok(Step::Refuse),

        (Subterm::Prim(this), Subterm::Prim(that)) => match peel_prim(this, that) {
            Some(Peel::Equal) => Ok(Step::Ok),
            Some(Peel::Clash) => Ok(Step::Clash),
            Some(Peel::Stuck) => Ok(Step::Refuse),
            Some(Peel::Continue(left, right)) => {
                unify_index(context, &left, &right, flex, tainted, false, solutions)
            }
            None => Ok(Step::Refuse),
        }

        // The same rigid variable on both sides is trivially forced.
        (Subterm::Var(a), Subterm::Var(t))
            if a.as_bound().is_none() && t.as_bound().is_none() && a.unwrap() == t.unwrap() =>
        {
            Ok(Step::Ok)
        }

        (Subterm::Variant(a), Subterm::Variant(t)) => {
            // A different union is incomparable; but two *different constructors*
            // of the same union definitely clash — even though they legitimately
            // differ in payload arity, so the tag check must precede the arity
            // check (which is then just defensive: equal tags share an arity).
            if a.name != t.name {
                return Ok(Step::Refuse);
            }
            if a.tag != t.tag {
                return Ok(Step::Clash);
            }
            if a.payload.len() != t.payload.len() {
                return Ok(Step::Refuse);
            }
            for (pa, pt) in a.payload.iter().zip(&t.payload) {
                match unify_index(context, pa, pt, flex, tainted, false, solutions)? {
                    Step::Ok => {}
                    other => return Ok(other),
                }
            }
            Ok(Step::Ok)
        }

        (Subterm::Tuple(a), Subterm::Tuple(t)) => {
            if a.fields.len() != t.fields.len() {
                return Ok(Step::Refuse);
            }
            for (fa, ft) in a.fields.iter().zip(&t.fields) {
                match unify_index(context, fa, ft, flex, tainted, false, solutions)? {
                    Step::Ok => {}
                    other => return Ok(other),
                }
            }
            Ok(Step::Ok)
        }

        _ => Ok(Step::Refuse),
    }
}
