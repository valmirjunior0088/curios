//! What one case's solution does to the context its arm is checked in, stated once for both checkers.
//!
//! [`solve_indices`](crate::solve_indices) finds what an arm learns about the scrutinee's indices, and a variable scrutinee adds the zero-index instance of the same equations, itself standing for the case's value ([`scrutinee_solution`]). The kernel substitutes that solution into the arm's body and expectation; the elaborator, which is still producing the body, records it as refinements instead. Neither reaches the *types* of the locals already in scope, and those are read where no refinement is consulted — a metavariable born in the arm keeps the types of its birth context and checks its solution against them, retried outside the arm's frame. So both checkers re-assume every local whose type mentions a solved variable at its specialized type ([`retyped`]).
//!
//! # Shared, not duplicated
//!
//! This was two rules, and they had drifted three ways. The kernel re-typed by its whole solution in every arm. The elaborator re-typed only by the variables that *were* an index or the scrutinee, only at an ambient goal, and even when the index equations clashed: it accepted an unreachable arm the kernel refused, and refused a written motive's arm, or an arm learning a variable from inside an index, that the kernel certified. Which locals a case re-types, and at what type, is a total function of the solution and the scope, so a second copy was never a second opinion.
//!
//! # Through local definitions
//!
//! The kernel substitutes a `let` away before it checks what follows it; the elaborator keeps one as a local definition. Both rules read through local definitions ([`Env::unfold`]), so they are stated over the variables the kernel sees: a scrutinee that is a `let` of a variable solves that variable, and a local whose type mentions a `let` reaching a solved variable is re-typed with that definition inlined. The kernel's locals carry no definitions, so for it the reading is the identity.

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_core::{Free, Subterm, Term},
    std::collections::{BTreeMap, BTreeSet},
};

/// The scrutinee's own solution for one case: `name := value` when the scrutinee is, through local definitions, a local variable. `None` for any other scrutinee — an expression has no binder to solve, and its equation is recorded against its spelling instead.
///
/// `value` is taken as given: a caller holding index solutions substitutes them into it first, since a case value built from the payload may mention a binder they pinned.
pub fn scrutinee_solution<'a, E: Env>(
    env: &'a E,
    scrutinee: &'a Term,
    value: &Term,
) -> Option<(Free, Term)> {
    let mut seen = BTreeSet::new();
    let mut scrutinee = scrutinee;

    loop {
        let Subterm::Var(var) = &**scrutinee else {
            return None;
        };
        let name = var.as_free()?;
        if !seen.insert(name) || !env.is_local(name) {
            return None;
        }

        match env.unfold(name) {
            Some(definition) => scrutinee = definition,
            None => return Some((name.clone(), value.clone())),
        }
    }
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
    let mut inliner = Inliner {
        env,
        solutions,
        expansions: BTreeMap::new(),
    };

    locals
        .iter()
        .filter(|(name, _)| !solutions.iter().any(|(solved, _)| solved == name))
        .filter_map(|(name, type_)| {
            let inlined = inliner.inline(type_);
            solved(&inlined).then(|| (name.clone(), inlined.substitute(solutions)))
        })
        .collect()
}

/// The local definitions that reach a solved variable, each inlined as far as it does.
///
/// Memoized for one [`retyped`] call, because a scope's types share their `let`s. A name is recorded before its definition is read, so a definition that mentions itself — which the elaborator's scope holds — is read once rather than without end.
struct Inliner<'a, E> {
    env: &'a E,
    solutions: &'a [(Free, Term)],
    expansions: BTreeMap<Free, Option<Term>>,
}

impl<E: Env> Inliner<'_, E> {
    /// `term` with every local definition it mentions that reaches a solved variable replaced by that definition, inlined in turn.
    fn inline(&mut self, term: &Term) -> Term {
        let expansions = term
            .free_vars_shared()
            .iter()
            .filter_map(|name| Some((name.clone(), self.expansion(name)?)))
            .collect::<Vec<_>>();

        term.substitute(&expansions)
    }

    fn expansion(&mut self, name: &Free) -> Option<Term> {
        if let Some(known) = self.expansions.get(name) {
            return known.clone();
        }
        self.expansions.insert(name.clone(), None);

        let definition = match self.env.is_local(name) {
            true => self.env.unfold(name)?.clone(),
            false => return None,
        };
        let inlined = self.inline(&definition);
        let expansion = self
            .solutions
            .iter()
            .any(|(solved, _)| inlined.mentions_free(solved))
            .then_some(inlined);

        self.expansions.insert(name.clone(), expansion.clone());
        expansion
    }
}
