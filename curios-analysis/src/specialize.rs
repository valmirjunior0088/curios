//! What one case's solution does to the context its arm is checked in, stated once for both checkers.
//!
//! [`solve_indices`](crate::solve_indices) finds what an arm learns about the scrutinee's indices, and a variable scrutinee adds the zero-index instance of the same equations, itself standing for the case's value ([`scrutinee_solution`]). The kernel substitutes that solution into the arm's body and expectation; the elaborator, which is still producing the body, records it as refinements instead. Neither reaches the *types* of the locals already in scope, and those are read where no refinement is consulted — a metavariable born in the arm keeps the types of its birth context and checks its solution against them, retried outside the arm's frame. So both checkers re-assume every local whose type mentions a solved variable at its specialized type ([`retyped`]).
//!
//! # Shared, not duplicated
//!
//! Which locals a case re-types, and at what type, is a total function of the solution and the scope, so a second copy would be a second transcription rather than a second opinion — and two copies that drift read one arm two ways, one checker accepting an arm the other refuses.
//!
//! # Through local definitions
//!
//! The kernel substitutes a `let` before it checks what follows; the elaborator keeps one as a local definition. Both rules read through local definitions by the one reading [`Unfolding`] and [`beneath`] state, so they are stated over what the kernel sees: a scrutinee that is a `let` of a variable solves that variable, and a local whose type mentions a `let` reaching a solved variable is re-typed with that definition substituted. The kernel's locals carry no definitions, so for it the reading is the identity.

#[cfg(test)]
mod tests;

use {
    crate::{Env, Unfolding, beneath},
    curios_core::{Free, Term},
};

/// The scrutinee's own solution for one case: `name := value` when the scrutinee is, through local definitions, a local variable. `None` for any other scrutinee — an expression has no binder to solve, and its equation is recorded against its spelling instead.
///
/// `value` is taken as given: a caller holding index solutions substitutes them into it first, since a case value built from the payload may mention a binder they pinned.
pub fn scrutinee_solution<E: Env>(env: &E, scrutinee: &Term, value: &Term) -> Option<(Free, Term)> {
    beneath(env, scrutinee).map(|name| (*name, value.clone()))
}

/// The locals one case re-types, each at its specialized type: every local whose type mentions a solved variable, reading through local definitions, at that type with those definitions inlined and `solutions` substituted.
///
/// A solved variable's own entry is left alone. Its occurrences in the arm are substituted away or refined, so nothing reads it at its stale type.
///
/// `locals` is the scope outermost first, and so is the answer: a caller re-assuming each entry in order leaves a name bound twice innermost at its innermost binding.
pub fn retyped<E: Env>(
    env: &E,
    locals: &[(Free, Term)],
    solutions: &[(Free, Term)],
) -> Vec<(Free, Term)> {
    if solutions.is_empty() {
        return Vec::new();
    }

    let solved = |term: &Term| solutions.iter().any(|(name, _)| term.mentions_free(name));
    let mut unfolding = Unfolding::toward(env, solutions);

    locals
        .iter()
        .filter(|(name, _)| !solutions.iter().any(|(solved, _)| solved == name))
        .filter_map(|(name, type_)| {
            let unfolded = unfolding.term(type_);
            solved(&unfolded).then(|| (*name, unfolded.substitute(solutions)))
        })
        .collect()
}
