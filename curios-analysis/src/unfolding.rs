//! What the kernel sees of a term that one of its checkers holds under local definitions.
//!
//! The kernel substitutes a `let` before it checks what follows; the elaborator keeps one as a local definition, so a term it holds may name a `let` where the kernel's copy of the same term has the value. Every rule that records or compares what the kernel sees — the locals a case's solution re-types, the variables an index equation may solve, the variable a scrutinee is — has to read through those definitions, and this module is that reading, stated once. It reads through [`Env::unfold`] and nothing else, so a driver whose locals carry no definitions, as the kernel's do not, sees every term unchanged.
//!
//! # Why one reading
//!
//! It was three. The re-typing inlined the definitions that reached a solved variable, the index solver collected the locals behind definitions, and the case's own solution followed a chain of variables, each with its own guard against a definition that mentions itself. A reading written once is one to test.

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_core::{Free, Subterm, Term},
    std::collections::{BTreeMap, BTreeSet},
};

/// A term with its local definitions substituted, as the kernel has them — every definition, or only the ones that reach given variables.
///
/// Memoized for one use, because a scope's terms share their `let`s. A name is recorded before its definition is read, so a definition that mentions itself — which the elaborator's scope holds — is read once rather than without end.
pub struct Unfolding<'a, E> {
    env: &'a E,
    reach: Reach<'a>,
    expansions: BTreeMap<Free, Option<Term>>,
}

/// Which local definitions an [`Unfolding`] substitutes.
enum Reach<'a> {
    /// All of them: the term as the kernel spells it.
    Everything,
    /// Only those whose definition reaches one of these variables, so a term is respelled no further than a substitution for them needs.
    Toward(&'a [(Free, Term)]),
}

impl<'a, E: Env> Unfolding<'a, E> {
    /// Every local definition substituted: the term as the kernel spells it.
    pub fn everything(env: &'a E) -> Self {
        Self {
            env,
            reach: Reach::Everything,
            expansions: BTreeMap::new(),
        }
    }

    /// Only the local definitions that reach a variable `solutions` solves.
    pub fn toward(env: &'a E, solutions: &'a [(Free, Term)]) -> Self {
        Self {
            env,
            reach: Reach::Toward(solutions),
            expansions: BTreeMap::new(),
        }
    }

    /// `term` with every local definition it mentions that this unfolding reaches replaced by that definition, unfolded in turn.
    pub fn term(&mut self, term: &Term) -> Term {
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
        let unfolded = self.term(&definition);
        let expansion = match self.reach {
            Reach::Everything => Some(unfolded),
            Reach::Toward(solutions) => solutions
                .iter()
                .any(|(solved, _)| unfolded.mentions_free(solved))
                .then_some(unfolded),
        };

        self.expansions.insert(name.clone(), expansion.clone());
        expansion
    }
}

/// The local variable `term` is, through local definitions that are themselves variables: `let t = s; … t …` is `s` to the kernel. `None` for a term that is, beneath its definitions, anything but a local variable.
pub fn beneath<'a, E: Env>(env: &'a E, term: &'a Term) -> Option<&'a Free> {
    let mut seen = BTreeSet::new();
    let mut term = term;

    loop {
        let Subterm::Var(var) = &**term else {
            return None;
        };
        let name = var.as_free()?;
        if !seen.insert(name) || !env.is_local(name) {
            return None;
        }

        match env.unfold(name) {
            Some(definition) => term = definition,
            None => return Some(name),
        }
    }
}

/// The locals the kernel's spelling of `terms` names: each local they mention, and in place of a local definition, the locals its definition names in turn. Their order is immaterial to every caller, which reads them only for membership.
pub fn locals_beneath<E: Env>(env: &E, terms: &[Term]) -> Vec<Free> {
    let mut locals = Vec::new();
    let mut seen = BTreeSet::new();
    let mut pending = terms
        .iter()
        .flat_map(|term| term.free_vars())
        .collect::<Vec<_>>();

    while let Some(name) = pending.pop() {
        if !seen.insert(name.clone()) || !env.is_local(&name) {
            continue;
        }
        match env.unfold(&name) {
            Some(definition) => pending.extend(definition.free_vars()),
            None => locals.push(name),
        }
    }

    locals
}
