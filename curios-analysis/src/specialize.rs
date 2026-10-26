//! What one case's solution does to the context its arm is checked in, stated once for both checkers.
//!
//! [`solve_indices`](crate::solve_indices) finds what an arm learns about the scrutinee's indices, and a variable scrutinee adds the zero-index instance of the same equations, itself standing for the case's value ([`scrutinee_solution`]). The kernel binds each solved variable to its solution, as a `let` binds a local, and reads the arm by value; the elaborator, which is still producing the body, records it as refinements instead. Neither reaches the *types* of the locals already in scope, and those are read where no refinement is consulted — a metavariable born in the arm keeps the types of its birth context and checks its solution against them, retried outside the arm's frame. So both checkers re-assume every local whose type mentions a solved variable at its specialized type ([`retyped`]).
//!
//! # Shared, not duplicated
//!
//! Which locals a case re-types, and at what type, is a total function of the solution and the scope, so a second copy would be a second transcription rather than a second opinion — and two copies that drift read one arm two ways, one checker accepting an arm the other refuses.
//!
//! # Through local definitions
//!
//! The kernel hands these rules its terms by value, each `let`-bound name unfolded; the elaborator keeps a `let` as a local definition. Both rules read through local definitions by the one reading [`Unfolding`] and [`beneath`] state, so they are stated over what the kernel sees: a scrutinee that is a `let` of a variable solves that variable, and a local whose type mentions a `let` reaching a solved variable is re-typed with that definition substituted. The kernel answers no local definition, so for it the reading is the identity.

#[cfg(test)]
mod tests;

use {
    crate::{Env, Unfolding, beneath},
    curios_core::{Free, Names, Term},
    std::collections::HashSet,
};

/// The scrutinee's own solution for one case: `name := value` when the scrutinee is, through local definitions, a local variable. `None` for any other scrutinee — an expression has no binder to solve, and its equation is recorded against its spelling instead.
///
/// `value` is taken as given: a caller holding index solutions substitutes them into it first, since a case value built from the payload may mention a binder they pinned.
pub fn scrutinee_solution<E: Env>(env: &E, scrutinee: &Term, value: &Term) -> Option<(Free, Term)> {
    beneath(env, scrutinee).map(|name| (*name, value.clone()))
}

/// The locals one case re-types, each at its specialized type: every local whose type mentions a solved variable, reading through local definitions, at that type with those definitions inlined and `solutions` substituted.
///
/// A solved variable's own entry is left alone. Its occurrences in the arm are read as its solution or refined, so nothing reads it at its stale type.
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

    let names = Names::new(solutions.iter().map(|(name, _)| *name));
    let solved = |term: &Term| term.mentions_any(&names);
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

/// Whether reading `term` reads a variable `solutions` solves: `term` names one, or names a local whose reading — its type, and its definition where it has one, as `reads` answers them — reads one in turn, followed to the end.
///
/// **What an equation's reduced spelling rests on.** A settlement reduces the equation's key with the equations inside it withheld, and reads, besides the definition store and the equations outside it, each local the key names: a local definition by its value, and every local at its type, where a question reduction puts to conversion is answered. A case moves exactly those readings: it re-types each local whose type names a variable it solves ([`retyped`]), and the kernel binds again each local definition whose value names one. So a spelling settled under the case's arm rests on its solution where this holds and nowhere else, and such an equation is restated in the arm rather than settled there (`curios-cert`'s `Scope::restate`, `curios-elab`'s `Context::restate_equations`).
///
/// `reads` answers nothing for a name no binder in scope opened: a top-level name's meaning is no case's to move.
pub fn reads_solved(
    term: &Term,
    solutions: &[(Free, Term)],
    mut reads: impl FnMut(&Free) -> Vec<Term>,
) -> bool {
    if solutions.is_empty() {
        return false;
    }
    let mut seen = HashSet::new();
    let mut pending = term.free_vars().iter().copied().collect::<Vec<_>>();
    while let Some(name) = pending.pop() {
        if !seen.insert(name) {
            continue;
        }
        if solutions.iter().any(|(solved, _)| *solved == name) {
            return true;
        }
        for read in reads(&name) {
            pending.extend(read.free_vars().iter().copied());
        }
    }

    false
}
