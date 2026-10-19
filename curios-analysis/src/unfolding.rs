//! What the kernel sees of a term that one of its checkers holds under local definitions.
//!
//! The kernel hands every rule here its terms by value, each `let`-bound name unfolded; the elaborator keeps a `let` as a local definition, so a term it holds may name a `let` where the kernel's copy of the same term has the value. Every rule that records or compares what the kernel sees — the locals a case's solution re-types, the variables an index equation may solve, the variable a scrutinee is — has to read through those definitions, and this module is that reading, stated once. It reads through [`Env::unfold`] and nothing else, so a driver that answers no local definition, as the kernel does not, sees every term unchanged.
//!
//! The spelling a guard's equation is recorded under reads a term whole, every definition substituted, and is not read here: the elaborator holds each definition by value from where it is made, as the kernel holds a `let` (`curios-elab`'s `Context::by_value`), where a reading from scratch would unfold a chain of definitions again at every spelling.
//!
//! # Why one reading
//!
//! The re-typing inlines the definitions that reach a solved variable, the index solver collects the locals behind definitions, and the case's own solution follows a chain of variables — three readers, each of which would otherwise carry its own guard against a definition that mentions itself. A reading written once is one to test.

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_core::{Free, Subterm, Term},
    std::collections::{BTreeMap, BTreeSet},
};

/// A term with the local definitions that reach given variables substituted, as the kernel has them, so a term is respelled no further than a substitution for those variables needs.
///
/// Memoized for one use, because a scope's terms share their `let`s. A name is recorded before its definition is read, so a definition that mentions itself — which the elaborator's scope holds — is read once rather than without end.
pub struct Unfolding<'a, E> {
    env: &'a E,
    solutions: &'a [(Free, Term)],
    expansions: BTreeMap<Free, Option<Term>>,
}

impl<'a, E: Env> Unfolding<'a, E> {
    /// The local definitions that reach a variable `solutions` solves.
    pub fn toward(env: &'a E, solutions: &'a [(Free, Term)]) -> Self {
        Self {
            env,
            solutions,
            expansions: BTreeMap::new(),
        }
    }

    /// `term` with every local definition it mentions that this unfolding reaches replaced by that definition, unfolded in turn.
    pub fn term(&mut self, term: &Term) -> Term {
        let expansions = term
            .free_vars_shared()
            .iter()
            .filter_map(|name| Some((*name, self.expansion(name)?)))
            .collect::<Vec<_>>();

        term.substitute(&expansions)
    }

    fn expansion(&mut self, name: &Free) -> Option<Term> {
        if let Some(known) = self.expansions.get(name) {
            return known.clone();
        }
        self.expansions.insert(*name, None);

        let definition = match self.env.is_local(name) {
            true => self.env.unfold(name)?.clone(),
            false => return None,
        };
        let unfolded = self.term(&definition);
        let expansion = self
            .solutions
            .iter()
            .any(|(solved, _)| unfolded.mentions_free(solved))
            .then_some(unfolded);

        self.expansions.insert(*name, expansion.clone());
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
        if !seen.insert(name) || !env.is_local(&name) {
            continue;
        }
        match env.unfold(&name) {
            Some(definition) => pending.extend(definition.free_vars()),
            None => locals.push(name),
        }
    }

    locals
}
