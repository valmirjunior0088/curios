//! What the walk in progress has opened: the local telescope, and the case equations assumed inside arms.
//!
//! The two belong to one component because they retract as one. A [`Mark`] is a checkpoint into both stacks and [`Scope::retract`] truncates both, so an arm that opened binders and assumed its scrutinee's case value gives up exactly both on the way out. Splitting them would be two checkpoints that must agree, which is a thing to get wrong rather than a thing to state.
//!
//! `mark` and `retract` are `pub(super)` and `Kernel::scoped` is their only caller anywhere — that is what makes the bracket the one way to open a binder scope, and the reason this component exposes no other way to shrink either stack.

use curios_core::{Free, Term, project_erased_universes};

/// A checkpoint into both stacks, restored together so neither can outlive the arm that opened it.
#[derive(Clone, Copy)]
pub(super) struct Mark {
    locals: usize,
    refinements: usize,
}

#[derive(Default)]
pub(super) struct Scope {
    /// Binders opened by the walk in progress, with their types, outermost first. A local has a type and never a value: `let` substitutes rather than binding, so nothing in scope here can be unfolded.
    locals: Vec<(Free, Term)>,
    /// The case equations of the arms currently being checked, innermost last: within an arm, the scrutinee expression *is* the case's value, definitionally — the built-in face of the convoy pattern, which is how the elaborator's refinement store reads inside an arm. The reducer consults these at stuck heads.
    refinements: Vec<(Term, Term)>,
}

impl Scope {
    /// Open a binder: bring `name : type_` into scope for the walk in progress.
    pub(super) fn assume(&mut self, name: &Free, type_: &Term) {
        self.locals.push((name.clone(), type_.clone()));
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` — already in weak-head normal form — is `value`, definitionally.
    ///
    /// A local-free scrutinee is skipped rather than recorded: local-free terms reduce to their case values instead of sticking, and the skip is also what keeps the evaluation memos sound — they store local-free terms only, and reduction of a local-free term never encounters a local-bearing stuck form, so no memoized reduct can depend on an equation that was later retracted.
    ///
    /// Keyed with universe instances erased, which is the rule `canonical_scrutinee` keys the elaborator's store by and for the same reason: a universe argument cannot affect computation, so two occurrences of one applied definition differing only in the levels inference happened to mint are one key. Comparing verbatim made them two, and the divergence was doing unearned work — the elaborator would accept a program the kernel then refused for a spelling difference, which reads as a disagreement about the *rule* and is not one.
    pub(super) fn refine(&mut self, scrutinee: Term, value: Term) {
        if scrutinee.has_local_free() {
            self.refinements
                .push((project_erased_universes(&scrutinee), value));
        }
    }

    /// The case value the stuck form `term` is refined to, innermost arm first.
    ///
    /// The empty check is what keeps the projection off the reducer's hot path: `whnf` asks this at every stuck form it reaches, and outside an arm there is nothing to ask about.
    pub(super) fn refinement_of(&self, term: &Term) -> Option<Term> {
        if self.refinements.is_empty() {
            return None;
        }

        let probe = project_erased_universes(term);

        self.refinements
            .iter()
            .rev()
            .find(|(key, _)| *key == probe)
            .map(|(_, value)| value.clone())
    }

    /// The types of the binders currently in scope, outermost first. The conversion history keys on this: the same goal under a different context is a different goal.
    pub(super) fn local_types(&self) -> Vec<Term> {
        self.locals.iter().map(|(_, type_)| type_.clone()).collect()
    }

    /// The identities of the binders currently in scope, outermost first — parallel to [`Scope::local_types`]. What the conversion history renames away, so that a goal reached again on a later round of an unfolding cycle is recognized as the goal it already is.
    pub(super) fn local_names(&self) -> Vec<Free> {
        self.locals.iter().map(|(name, _)| name.clone()).collect()
    }

    /// The type `name` was opened at, if it is a binder currently in scope.
    ///
    /// Innermost first — which cannot actually matter, since binder identities are minted unique, but scanning in that order means the rule does not depend on that being true.
    pub(super) fn local_type(&self, name: &Free) -> Option<&Term> {
        self.locals
            .iter()
            .rev()
            .find(|(bound, _)| bound == name)
            .map(|(_, type_)| type_)
    }

    /// The current depth of both stacks, to be handed back to [`Scope::retract`].
    pub(super) fn mark(&self) -> Mark {
        Mark {
            locals: self.locals.len(),
            refinements: self.refinements.len(),
        }
    }

    /// Close every binder opened — and drop every case equation assumed — since `mark`.
    pub(super) fn retract(&mut self, mark: Mark) {
        self.locals.truncate(mark.locals);
        self.refinements.truncate(mark.refinements);
    }
}
