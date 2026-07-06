//! Rung C of the indexed-inductive ladder: *inversion*. When an inductive match's
//! scrutinee carries indices in constructor form, first-order unification of
//! the actual indices against each case's target indices either pins arm
//! binders to the values they are forced to take (`m + 1 ~ n + 1` pins
//! `m := n`) or proves the case unreachable outright (`0` against `n + 1`) —
//! which is what lets the arm be omitted, checker-verified, with no
//! `impossible` keyword.

use {
    super::{
        Context, Error, InductiveType, Peel, Subterm, Telescope, Term, convert_at, peel_prim,
        reduce_with,
    },
    std::collections::BTreeSet,
};

/// The outcome of inversion: either every index position decomposed (or
/// refused) cleanly, yielding the forced arm-binder solutions, or some
/// position clashed definitely and the arm is unreachable.
pub(crate) enum Invert {
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
pub(crate) fn case_target_indices(telescope: Telescope<Term>, vars: &[Term]) -> Vec<Term> {
    match telescope.open_params(vars) {
        Telescope::Done(terminal) => match &**terminal {
            Subterm::InductiveType(InductiveType { indices, .. }) => indices.clone(),
            _ => unreachable!("constructor terminal is its inductive type"),
        },
        Telescope::Cons(..) => unreachable!("telescope arity matches the binder count"),
    }
}

/// The unifier, deliberately tiny: first-order, constructor-form. Per index
/// position it decomposes matching constructor forms (`Nat` successor spines,
/// variants by tag, tuples pointwise), solves an unbound arm binder against
/// the rigid term it is forced to equal, and declares a clash on distinct
/// constructors. A binder forced in more than one position is reconciled by
/// the *deletion* rule (`consolidate`): its forcings must be convertible —
/// sound because `Eq : Prop` makes the system definitionally K. Everything
/// else — metavariables, opaque applications, key-shaped actuals at the top of
/// a position (Rung B's territory) — it *refuses*: the arm stays mandatory and
/// the binder unsolved.
pub(crate) fn invert_indices(
    context: &mut Context,
    actuals: &[Term],
    targets: &[Term],
    flex: &[String],
) -> Result<Invert, Error> {
    let mut solutions = Vec::new();

    for (actual, target) in actuals.iter().zip(targets) {
        let mut position = Vec::new();

        match unify_index(context, actual, target, flex, true, &mut position)? {
            Step::Clash => return Ok(Invert::Impossible),
            // A refused position contributes nothing — solutions found on
            // the way in are discarded with it, conservatively.
            Step::Refuse => {}
            // A solved position's forcings join the pool; a binder forced here
            // *and* in another position becomes a duplicate, reconciled by the
            // deletion rule below.
            Step::Ok => solutions.extend(position),
        }
    }

    consolidate(context, solutions).map(Invert::Solved)
}

/// The deletion rule (Goguen–McBride–McKinna), the last of the first-order
/// set, restored here as a *semantic* test in place of the old syntactic
/// non-linearity refusal. A flex arm binder forced in more than one index
/// position must take convertible values; since `Eq : Prop` makes the system
/// definitionally K, deleting the redundant constraint is sound. A *definite*
/// yes from the boolean oracle (`Prop`-typed positions convert by irrelevance,
/// so they delete for free) keeps one solution; anything short — a `Mismatch`,
/// a `Blocked`, or a binder whose type is out of scope (the prune site, which
/// only reads `Impossible` vs `Solved`) — drops that binder's solutions,
/// conservatively. Never a `Clash`, so `Impossible`/prune semantics hold.
fn consolidate(
    context: &mut Context,
    solutions: Vec<(String, Term)>,
) -> Result<Vec<(String, Term)>, Error> {
    let mut kept: Vec<(String, Term)> = Vec::new();
    let mut refused = BTreeSet::new();

    for (label, value) in solutions {
        if refused.contains(&label) {
            continue;
        }

        let Some(index) = kept.iter().position(|(l, _)| *l == label) else {
            kept.push((label, value));
            continue;
        };

        // A re-forcing: keep the prior solution iff the two are convertible at
        // the binder's declared type (cloned to release the context borrow).
        let prior = kept[index].1.clone();
        let deletes = match context.assumption(&label).cloned() {
            Some(type_) => convert_at(context, &type_, &prior, &value)?,
            None => false,
        };

        if !deletes {
            kept.remove(index);
            refused.insert(label);
        }
    }

    Ok(kept)
}

fn unify_index(
    context: &mut Context,
    actual: &Term,
    target: &Term,
    flex: &[String],
    top: bool,
    solutions: &mut Vec<(String, Term)>,
) -> Result<Step, Error> {
    let actual = reduce_with(context, actual)?;
    let target = reduce_with(context, target)?;

    // Solve a flex arm binder against the rigid term it is forced to equal —
    // forced because every decomposition step above it was injective. A binder
    // already forced elsewhere is recorded again, not refused: the deletion
    // rule in `consolidate` reconciles the two forcings by convertibility.
    if let Subterm::Var(var) = &*target
        && var.as_bound().is_none()
        && flex.iter().any(|l| l == var.unwrap())
    {
        let label = var.unwrap().to_string();

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
                unify_index(context, &left, &right, flex, false, solutions)
            }
            None => Ok(Step::Refuse),
        },

        // The same rigid variable on both sides is trivially forced.
        (Subterm::Var(a), Subterm::Var(t))
            if a.as_bound().is_none() && t.as_bound().is_none() && a.unwrap() == t.unwrap() =>
        {
            Ok(Step::Ok)
        }

        (Subterm::Variant(a), Subterm::Variant(t)) => {
            // A different inductive is incomparable; but two *different constructors*
            // of the same inductive definitely clash — even though they legitimately
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
            unify_all(context, &a.payload, &t.payload, flex, solutions)
        }

        (Subterm::Tuple(a), Subterm::Tuple(t)) => {
            if a.fields.len() != t.fields.len() {
                return Ok(Step::Refuse);
            }
            unify_all(context, &a.fields, &t.fields, flex, solutions)
        }

        _ => Ok(Step::Refuse),
    }
}

/// Unify a sequence of sub-positions pairwise, short-circuiting on the first
/// that does not cleanly decompose. Each sub-position is non-`top`.
fn unify_all(
    context: &mut Context,
    actuals: &[Term],
    targets: &[Term],
    flex: &[String],
    solutions: &mut Vec<(String, Term)>,
) -> Result<Step, Error> {
    for (actual, target) in actuals.iter().zip(targets) {
        match unify_index(context, actual, target, flex, false, solutions)? {
            Step::Ok => {}
            other => return Ok(other),
        }
    }
    Ok(Step::Ok)
}
