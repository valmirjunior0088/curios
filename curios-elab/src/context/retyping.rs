//! A reader of finished terms.
//!
//! Erasure walks terms that are elaborated, zonked and projected out of their universes, and asks of them what the elaborator's engine answers: a type's head, its sort, what a name is assumed at, which equations an arm stands under. It elaborates nothing, so nothing that exists because a unit is being elaborated is its to reach — no attempt's metavariables, no unfinished declaration, no use site's module. [`Retyping`] is how that is held: it owns a context it made over the scope it is handed, which it reads through and brings nothing in of (`InScope`), whose elaboration half is the empty one a fresh context has, and it offers the binder supply, the frame bracket, the lookups, the budget and the engine's entries a walk over finished terms needs, each by name, and nothing else of the context.
//!
//! **Its reduction asks conversion, as the reduction that accepted the term did.** A type's head may show only where a stuck fold's atoms are classed or an arm's equation is met at another spelling, and erasure needs the head of every callee, record and scrutinee it eliminates. A reader whose reduction asked nothing (`Context::plainly`) would meet a type both checkers read and have no head to read off it.
//!
//! It lives in the context's module tree because its frame bracket hands the closure the reader itself: that takes the two halves of `Context::with_frame`, which no module outside the tree can call apart, so the bracket stays the only way into a frame.

use {
    super::{Answered, Context, InScope, Question},
    crate::{Binders, Error, is_erasable_in, reduce_with, refine_head, sort_term, synth_neutral},
    curios_analysis::{Invert, solve_indices},
    curios_core::{
        Advance, Cost, DefinitionKind, Free, Global, InductDecl, Level, ReduceError, StructDecl,
        Term, Trace, UniverseContext, UniverseError,
    },
    curios_utilities::SyntaxRegistry,
};

#[cfg(test)]
use curios_core::Consumption;

/// A context that reads finished terms, made by its reader and handed to no one.
#[derive(Debug)]
pub(crate) struct Retyping<'s> {
    context: Context<'s>,
}

impl<'s> Retyping<'s> {
    /// A reader over `scope`, in which each item read may spend `budget` units, naming what `syntax` registers. What it reads of a declaration it reads through to `scope`, and its own stores start empty.
    pub(crate) fn over(budget: u64, syntax: SyntaxRegistry, scope: &'s dyn InScope) -> Self {
        let mut context = Context::new(budget, syntax);
        context.read_through(scope);

        Self { context }
    }

    /// A reader with nothing in scope, for a test that reads terms naming no declaration.
    #[cfg(test)]
    pub(crate) fn new(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            context: Context::new(budget, syntax),
        }
    }

    /// Begin the trace of what erasing the item declaring `own` asks of the declarations around it: the next question is its first.
    pub(crate) fn begin_trace(&mut self, own: &[&Global]) {
        self.context.typing.traced.begin(own);
    }

    /// Note that the erasure under way read the erased form of the item declaring `name`.
    pub(crate) fn asks_erased(&self, name: &Global) {
        self.context
            .typing
            .traced
            .note(Question::Erased(*name), Answered::Part);
    }

    /// Note that the erasure under way evaluated its item after the erased form of the item declaring `name`, whole.
    pub(crate) fn asks_ran(&self, name: &Global) {
        self.context
            .typing
            .traced
            .note(Question::Ran(*name), Answered::Part);
    }

    /// What the erasure under way asked, taken ([`Context::take_trace`]).
    pub(crate) fn take_trace(&mut self) -> Trace<Question, Answered> {
        self.context.take_trace()
    }

    /// [`Context::syntax`].
    pub(crate) fn syntax(&self) -> SyntaxRegistry {
        self.context.syntax()
    }

    /// [`Context::fresh`].
    pub(crate) fn fresh(&mut self, hint: Option<&str>) -> Free {
        self.context.fresh(hint)
    }

    /// [`Context::spend`].
    pub(crate) fn spend(&self, cost: Cost) -> Result<(), ReduceError> {
        self.context.spend(cost)
    }

    /// [`Context::restore_budget`]: an item is read on a budget restored for it.
    pub(crate) fn restore_budget(&mut self) {
        self.context.restore_budget();
    }

    /// [`Context::consumed`], which this crate's measurements of what a reading spends take.
    #[cfg(test)]
    pub(crate) fn consumed(&self) -> Consumption {
        self.context.consumed()
    }

    /// [`Context::with_frame`], the closure handed the reader.
    pub(crate) fn with_frame<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.context.enter_frame();
        let result = f(self);
        self.context.leave_frame();

        result
    }

    /// [`Context::assume`].
    pub(crate) fn assume(&mut self, name: &Free, type_: &Term) {
        self.context.assume(name, type_);
    }

    /// [`Context::advance_assumed`].
    pub(crate) fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free {
        self.context.advance_assumed(walk, domain)
    }

    /// [`Context::set_assumption_universe_context`].
    pub(crate) fn set_assumption_universe_context(
        &mut self,
        name: &Free,
        universe_context: UniverseContext,
    ) {
        self.context
            .set_assumption_universe_context(name, universe_context);
    }

    /// [`Context::define`].
    pub(crate) fn define(&mut self, name: &Free, term: &Term, kind: Option<&DefinitionKind>) {
        self.context.define(name, term, kind);
    }

    /// [`Context::define_assuming`].
    pub(crate) fn define_assuming(
        &mut self,
        name: &Free,
        type_: &Term,
        term: &Term,
        kind: Option<&DefinitionKind>,
    ) {
        self.context.define_assuming(name, type_, term, kind);
    }

    /// [`Context::definition_kind`].
    pub(crate) fn definition_kind(&self, name: &Free) -> Option<&DefinitionKind> {
        self.context.definition_kind(name)
    }

    /// [`Context::definition_body`].
    pub(crate) fn definition_body(&self, name: &Free) -> Option<&Term> {
        self.context.definition_body(name)
    }

    /// [`Context::refine`].
    pub(crate) fn refine(&mut self, name: &Free, term: &Term) {
        self.context.refine(name, term);
    }

    /// [`Context::induct_decl`].
    pub(crate) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.context.induct_decl(name)
    }

    /// [`Context::instantiate_induct_decl_at`].
    pub(crate) fn instantiate_induct_decl_at(
        &mut self,
        induct_decl: &InductDecl,
        levels: &[Level],
    ) -> Result<InductDecl, UniverseError> {
        self.context.instantiate_induct_decl_at(induct_decl, levels)
    }

    /// [`Context::struct_decl`].
    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.context.struct_decl(name)
    }

    /// [`Context::instantiate_struct_decl_at`].
    pub(crate) fn instantiate_struct_decl_at(
        &mut self,
        struct_decl: &StructDecl,
        levels: &[Level],
    ) -> Result<StructDecl, UniverseError> {
        self.context.instantiate_struct_decl_at(struct_decl, levels)
    }

    /// `term` reduced to its head ([`reduce_with`]).
    pub(crate) fn reduce_with(&mut self, term: &Term) -> Result<Term, Error> {
        reduce_with(&mut self.context, term)
    }

    /// The sort of `type_`, as a term ([`sort_term`]).
    pub(crate) fn sort_term(&mut self, type_: &Term) -> Result<Term, Error> {
        sort_term(&mut self.context, type_)
    }

    /// Whether a value of `type_` carries nothing at run time, under the binders `opened` ([`is_erasable_in`]).
    pub(crate) fn is_erasable_in(
        &mut self,
        opened: &mut Vec<(Free, Term)>,
        type_: &Term,
    ) -> Result<bool, Error> {
        is_erasable_in(&mut self.context, opened, type_)
    }

    /// The type of the neutral `term`, read with no binder a conversion opened ([`synth_neutral`]): a walk over finished terms assumes the binders it stands under.
    pub(crate) fn synth_neutral(&mut self, term: &Term) -> Result<Option<Term>, ReduceError> {
        synth_neutral(&mut self.context, &Binders::default(), &[], term)
    }

    /// Install the equation an arm stands under: `head` is `value` there ([`refine_head`]).
    pub(crate) fn refine_head(&mut self, head: &Term, value: &Term) -> Result<(), Error> {
        refine_head(&mut self.context, head, value)
    }

    /// Solve a match's index equations for an arm's `binders` ([`solve_indices`]), the one place a reader puts a question to conversion: two forcings of one binder are compared by `Judge::convert_at`.
    pub(crate) fn solve_indices(
        &mut self,
        actuals: &[Term],
        targets: &[Term],
        binders: &[Free],
    ) -> Result<Invert, Error> {
        solve_indices(&mut self.context, actuals, targets, binders)
    }
}
