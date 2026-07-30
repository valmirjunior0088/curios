//! Rung C of the indexed-inductive ladder: *inversion*. When an inductive match's
//! scrutinee carries indices in constructor form, first-order unification of
//! the actual indices against each case's target indices either pins arm
//! binders to the values they are forced to take (`m + 1 ~ n + 1` pins
//! `m := n`) or proves the case unreachable outright (`0` against `n + 1`) —
//! which is what lets the arm be omitted, checker-verified, with no
//! `impossible` keyword.
//!
//! # Shared, not duplicated
//!
//! Both checkers run *this* unifier. It is a total function of finished terms,
//! and it runs post-zonk on both sides, so a second implementation would be a
//! second run of the same function on the same input rather than a second
//! opinion. What each side supplies for itself is reduction and conversion,
//! through [`Judge`] — see that module for the line, and for the concession
//! borrowing conversion represents.

use {
    crate::Judge,
    curios_core::{Free, InductType, Peel, Subterm, Telescope, Term, Variant, peel_prim},
    std::collections::BTreeSet,
};

/// The payload binders an index target set *determines* — the singleton rung's
/// side condition, shared by both checkers exactly as the unifier below is: a
/// total function of the targets alone, so a second copy would be a second run
/// rather than a second opinion.
///
/// A binder is determined when matching a value against the target recovers it.
/// That holds when the target is the binder itself, and — recursively — when it
/// is a constructor application and the binder sits in one of its arguments,
/// because constructors are injective and their tags are disjoint.
///
/// It does **not** hold for a binder under anything else. `blur(a)` is an
/// arbitrary function of `a`: knowing its value recovers nothing, since `blur`
/// need not be injective and in the case that motivated this rule is the
/// constant zero. Reading occurrence as determination — asking only whether `a`
/// appears anywhere in the target — is the mistake, and it is the difference
/// between a singleton and a proposition with a payload a program can read.
///
/// Total by construction: a target this cannot decompose contributes nothing,
/// so a shape nobody anticipated yields *fewer* determined binders and a
/// stricter guard, never a looser one.
pub fn pinned_by_targets(targets: &[Term]) -> Vec<Free> {
    fn walk(target: &Term, determined: &mut Vec<Free>) {
        match &**target {
            Subterm::Var(var) => determined.push(var.unwrap().clone()),
            // Constructors are injective and their tags disjoint, so matching
            // recovers every argument. Parameters are not walked: they are the
            // family's, fixed before this constructor was reached.
            Subterm::Variant(Variant { payload, .. }) => {
                for component in payload {
                    walk(component, determined);
                }
            }
            // Anything else — an application, a projection, a primitive, a
            // stuck match — determines nothing.
            _ => {}
        }
    }

    let mut determined = Vec::new();
    for target in targets {
        walk(target, &mut determined);
    }

    determined
}

/// The outcome of inversion: either every index position decomposed (or
/// refused) cleanly, yielding the forced arm-binder solutions, or some
/// position clashed definitely and the arm is unreachable.
pub enum Invert {
    Solved(Vec<(Free, Term)>),
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
pub fn case_target_indices(telescope: Telescope<Term>, vars: &[Term]) -> Vec<Term> {
    match telescope.open_params(vars) {
        Telescope::Done(terminal) => match &**terminal {
            Subterm::InductType(InductType { indices, .. }) => indices.clone(),
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
pub fn invert_indices<J: Judge>(
    judge: &mut J,
    actuals: &[Term],
    targets: &[Term],
    flex: &[Free],
) -> Result<Invert, J::Error> {
    invert_with(judge, actuals, targets, flex, false)
}

/// [`invert_indices`] for the outer direction: the flex variables are the
/// *context's*, not an arm's, so a key-shaped actual at the top of a position
/// is not something the flex side was refined to — there is nothing for a
/// solution to cycle through — and a variable actual may therefore solve the
/// flex target it meets. This is the kernel's spelling of the elaborator's
/// `refine_head` orientation: the outer variable stands refined to the arm's
/// term. The first direction keeps the guard, because there the flex side is
/// the arm's own binders and rung B has already refined keys to them.
pub fn invert_indices_outer<J: Judge>(
    judge: &mut J,
    actuals: &[Term],
    targets: &[Term],
    flex: &[Free],
) -> Result<Invert, J::Error> {
    invert_with(judge, actuals, targets, flex, true)
}

fn invert_with<J: Judge>(
    judge: &mut J,
    actuals: &[Term],
    targets: &[Term],
    flex: &[Free],
    solve_keys: bool,
) -> Result<Invert, J::Error> {
    let mut solutions = Vec::new();

    for (actual, target) in actuals.iter().zip(targets) {
        let mut position = Vec::new();

        match unify_index(judge, actual, target, flex, true, solve_keys, &mut position)? {
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

    consolidate(judge, solutions).map(Invert::Solved)
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
fn consolidate<J: Judge>(
    judge: &mut J,
    solutions: Vec<(Free, Term)>,
) -> Result<Vec<(Free, Term)>, J::Error> {
    let mut kept: Vec<(Free, Term)> = Vec::new();
    let mut refused = BTreeSet::new();

    for (binder, value) in solutions {
        if refused.contains(&binder) {
            continue;
        }

        let Some(index) = kept.iter().position(|(bound, _)| *bound == binder) else {
            kept.push((binder, value));
            continue;
        };

        // A re-forcing: keep the prior solution iff the two are convertible at
        // the binder's declared type (cloned to release the borrow).
        let prior = kept[index].1.clone();
        let deletes = match judge.assumption(&binder).cloned() {
            Some(type_) => judge.convert_at(&type_, &prior, &value)?,
            None => false,
        };

        if !deletes {
            kept.remove(index);
            refused.insert(binder);
        }
    }

    Ok(kept)
}

fn unify_index<J: Judge>(
    judge: &mut J,
    actual: &Term,
    target: &Term,
    flex: &[Free],
    top: bool,
    solve_keys: bool,
    solutions: &mut Vec<(Free, Term)>,
) -> Result<Step, J::Error> {
    let actual = judge.force(actual)?;
    let target = judge.force(target)?;

    // Solve a flex arm binder against the rigid term it is forced to equal —
    // forced because every decomposition step above it was injective. A binder
    // already forced elsewhere is recorded again, not refused: the deletion
    // rule in `consolidate` reconciles the two forcings by convertibility.
    if let Subterm::Var(var) = &*target
        && var.as_bound().is_none()
        && flex.iter().any(|bound| bound == var.unwrap())
    {
        let binder = var.unwrap().clone();

        // At the top of a position a key-shaped actual is Rung B's: it was
        // refined *to* this binder, and solving the binder back to it would
        // tie a reduction cycle. A flex actual (metavariable) is refused
        // outright.
        if (top && !solve_keys) && matches!(&*actual, Subterm::Var(_) | Subterm::Proj(_))
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
            .any(|free| flex.iter().any(|bound| bound == free))
        {
            return Ok(Step::Refuse);
        }

        solutions.push((binder, actual.clone()));

        return Ok(Step::Ok);
    }

    match (&*actual, &*target) {
        (Subterm::Metavar(_), _) | (_, Subterm::Metavar(_)) => Ok(Step::Refuse),

        (Subterm::Prim(this), Subterm::Prim(that)) => match peel_prim(this, that) {
            Some(Peel::Equal) => Ok(Step::Ok),
            Some(Peel::Clash) => Ok(Step::Clash),
            Some(Peel::Stuck) => Ok(Step::Refuse),
            Some(Peel::Continue(left, right)) => {
                unify_index(judge, &left, &right, flex, false, solve_keys, solutions)
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
            unify_all(judge, &a.payload, &t.payload, flex, solve_keys, solutions)
        }

        (Subterm::Tuple(a), Subterm::Tuple(t)) => {
            if a.fields.len() != t.fields.len() {
                return Ok(Step::Refuse);
            }
            unify_all(judge, &a.fields, &t.fields, flex, solve_keys, solutions)
        }

        _ => Ok(Step::Refuse),
    }
}

/// Unify a sequence of sub-positions pairwise, short-circuiting on the first
/// that does not cleanly decompose. Each sub-position is non-`top`.
fn unify_all<J: Judge>(
    judge: &mut J,
    actuals: &[Term],
    targets: &[Term],
    flex: &[Free],
    solve_keys: bool,
    solutions: &mut Vec<(Free, Term)>,
) -> Result<Step, J::Error> {
    for (actual, target) in actuals.iter().zip(targets) {
        match unify_index(judge, actual, target, flex, false, solve_keys, solutions)? {
            Step::Ok => {}
            other => return Ok(other),
        }
    }
    Ok(Step::Ok)
}
