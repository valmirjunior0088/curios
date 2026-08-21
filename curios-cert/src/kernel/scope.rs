//! What the walk in progress has opened: the local telescope, and the case equations assumed inside arms.
//!
//! The two belong to one component because they retract as one. A [`Mark`] is a checkpoint into both stacks and [`Scope::retract`] truncates both, so an arm that opened binders and assumed its scrutinee's case value gives up exactly both on the way out. Splitting them would be two checkpoints that must agree, which is a thing to get wrong rather than a thing to state.
//!
//! `mark` and `retract` are `pub(super)` and `Kernel::scoped` is their only caller anywhere — that is what makes the bracket the one way to open a binder scope, and the reason this component exposes no other way to shrink either stack.

use curios_core::{Free, Term};

/// A checkpoint into both stacks, restored together so neither can outlive the arm that opened it.
#[derive(Clone, Copy)]
pub(super) struct Mark {
    locals: usize,
    refinements: usize,
}

/// One arm's case equation, under the two spellings a probe may present its subject in.
struct Refinement {
    /// The scrutinee **as written** — the spelling the equation is recorded under, and the one a probe is asked about first.
    key: Term,
    /// The value this case assumes the scrutinee is.
    value: Term,
    /// The weak-head normal form of `key`, computed at most once and only when a probe has already missed the written spelling. See [`Scope::unasked_refinement`].
    reduct: Reduct,
}

/// Whether an equation's reduced spelling has been asked for yet.
enum Reduct {
    /// No probe has needed it. Nothing has been reduced for this equation.
    Unasked,
    /// Settled: the reduct, or `None` where reducing it refused.
    Known(Option<Term>),
}

#[derive(Default)]
pub(super) struct Scope {
    /// Binders opened by the walk in progress, with their types, outermost first. A local has a type and never a value: `let` substitutes rather than binding, so nothing in scope here can be unfolded.
    locals: Vec<(Free, Term)>,
    /// The case equations of the arms currently being checked, innermost last: within an arm, the scrutinee expression *is* the case's value, definitionally — the built-in face of the convoy pattern, which is how the elaborator's refinement store reads inside an arm. The reducer consults these at stuck heads.
    refinements: Vec<Refinement>,
    /// How many equations are currently in force, when that is fewer than there are. `Some(n)` withholds everything from `n` inwards for the duration of one [`Scope::unasked_refinement`] settlement — see [`Scope::hide_refinements_from`].
    hidden: Option<usize>,
}

impl Scope {
    /// Open a binder: bring `name : type_` into scope for the walk in progress.
    pub(super) fn assume(&mut self, name: &Free, type_: &Term) {
        self.locals.push((name.clone(), type_.clone()));
    }

    /// Whether any arm's case equation is currently in force — the judgment-side half of the closed machine's gate: inside an arm a closed scrutinee *is* the assumed value, so closed evaluation must stand aside for the strategy that consults these.
    ///
    /// Reads the equations *in force*, not the equations recorded: while a settlement withholds the inner ones, the machine is entitled to run wherever nothing is left to consult.
    pub(super) fn has_refinements(&self) -> bool {
        self.in_force() != 0
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` is `value`, definitionally.
    ///
    /// **Keyed on the scrutinee as written.** This used to be keyed on the scrutinee's weak-head normal form, which the caller obtained by reducing it once per arm — and a scrutinee mentioning a local can be memoized by nothing, so a web of combinator definitions each naming the one before it twice unfolded exponentially to produce a key that a literal arm body then never probed. The written spelling costs nothing to record; the reduced one is computed only when a probe misses, at most once per equation, by [`Scope::unasked_refinement`] and its caller.
    ///
    /// A local-free scrutinee is skipped rather than recorded, and that gate now sits on the written spelling. Local-free terms reduce to their case values instead of sticking, and the skip is also what keeps the evaluation memos sound — they store local-free terms only, and reduction of a local-free term never encounters a local-bearing stuck form, so no memoized reduct can depend on an equation that was later retracted. The reduced spelling a settlement computes is *not* covered by this gate, and does not need to be: the probe that consults it is asked only about local-bearing terms, so a local-free reduct can be recorded and can never fire.
    ///
    /// An equation is a claim about *one* term, so the only sound key is one that identifies terms already definitionally equal, and structural equality is the under-approximation of that which costs nothing to justify. Both spellings satisfy it — the written one *is* the scrutinee, and the reduced one is what the kernel's own reduction says it computes to.
    ///
    /// This used to key through `project_erased_universes`, on the premise that a universe argument cannot affect computation. The premise is false: Core has no eliminator over levels, but `Type u` embeds one *in a term*, so a definition carrying its parameter into a constructor payload reduces to genuinely different values at two instances — and that projection rebuilds every `Type` payload at one ground level, because it was written for the Core-to-Ersd hand-off where levels really are irrelevant. Read as a quotient by definitional equality it identified `Type 0` with `Type 1`, which is the universe hierarchy's whole content. See `crate::recheck::tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance`.
    pub(super) fn refine(&mut self, scrutinee: Term, value: Term) {
        if scrutinee.has_local_free() {
            self.refinements.push(Refinement {
                key: scrutinee,
                value,
                reduct: Reduct::Unasked,
            });
        }
    }

    /// The case value the term `term` is refined to under the *written* spelling, innermost arm first.
    ///
    /// Probed by the same key [`Scope::refine`] stores under, which is the scrutinee itself.
    pub(super) fn refinement_of(&self, term: &Term) -> Option<Term> {
        self.in_force_innermost_first()
            .find(|entry| entry.key == *term)
            .map(|entry| entry.value.clone())
    }

    /// The case value `term` is refined to under a *reduced* spelling already settled, innermost arm first.
    ///
    /// The escalation the written spelling's probe misses reach, and it settles nothing itself: an equation whose reduced spelling has never been asked for cannot answer here.
    pub(super) fn refinement_of_reduct(&self, term: &Term) -> Option<Term> {
        self.in_force_innermost_first()
            .find(|entry| match &entry.reduct {
                Reduct::Known(Some(reduct)) => reduct == term,
                _ => false,
            })
            .map(|entry| entry.value.clone())
    }

    /// The innermost equation in force whose reduced spelling has not been asked for and *could* be `candidate`, as its position and the term to reduce.
    ///
    /// The position is always inside the current limit, which is what lets [`Scope::hide_refinements_from`] take it as the new limit rather than the smaller of the two: a settlement can only ever reach further out than the one it is nested in.
    ///
    /// **Reading the limit here is also what makes the settlement loop finite**, and that is a second job rather than a restatement of the first. An entry being settled is outside the limit for the whole of its own reduction, so a probe reached from inside cannot select it again; relaxing this while keeping the probes' half sends `refined_reduct` back into the entry it is already settling.
    /// **`candidate` is what decides whether the reduction happens at all**, and without that test the deferral buys nothing. A settlement is the whole cost the two-tier key exists to avoid, and a probe reached under freshly opened binders — `Sort::of` walking a telescope, an arm body's own erasure obligations — presents a stuck form on almost every reduction, so *some* term would trigger a settlement in any arm whatever. What [`could_reduce_to`] tests is the one thing a reduct's spelling cannot lie about.
    pub(super) fn unasked_refinement(&self, candidate: &Term) -> Option<(usize, Term)> {
        self.refinements[..self.in_force()]
            .iter()
            .enumerate()
            .rev()
            .find(|(_, entry)| {
                matches!(entry.reduct, Reduct::Unasked) && could_reduce_to(&entry.key, candidate)
            })
            .map(|(index, entry)| (index, entry.key.clone()))
    }

    /// Record what the equation at `index` reduces to, or that reducing it refused.
    pub(super) fn settle_refinement(&mut self, index: usize, reduct: Option<Term>) {
        self.refinements[index].reduct = Reduct::Known(reduct);
    }

    /// Withhold the equation at `index` and every equation inside it, handing back the previous limit for [`Scope::show_refinements`].
    ///
    /// **This is what makes an equation's reduced spelling rest only on equations outside it.** Those retract no earlier than it does, so a reduct computed under them stays true for exactly as long as the entry holding it. The eager reduction this replaced had that guarantee for free, by running before the equation was pushed and while the stack below it was already frozen; computing the same reduct later has to reconstruct the same view, and withholding is how.
    ///
    /// Withholding the entry itself is the other half: without it, reducing `key` meets `key` at the reducer's own first probe and answers the case value, so the equation would settle its reduced spelling to whatever it was assuming.
    pub(super) fn hide_refinements_from(&mut self, index: usize) -> Option<usize> {
        self.hidden.replace(index)
    }

    /// Put back what [`Scope::hide_refinements_from`] withheld.
    pub(super) fn show_refinements(&mut self, previous: Option<usize>) {
        self.hidden = previous;
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
    ///
    /// No settlement can be in progress here, and that is structural rather than checked by discipline: `Kernel::scoped` is this method's only caller, and reduction — the only thing a settlement runs — never opens a binder scope.
    pub(super) fn retract(&mut self, mark: Mark) {
        debug_assert!(
            self.hidden.is_none(),
            "a scope retracted while an equation's reduced spelling was being settled"
        );

        self.locals.truncate(mark.locals);
        self.refinements.truncate(mark.refinements);
    }

    /// How many equations are in force: all of them, unless a settlement is withholding the inner ones.
    fn in_force(&self) -> usize {
        self.hidden.unwrap_or(self.refinements.len())
    }

    /// The equations in force, innermost first — the order every probe reads them in.
    fn in_force_innermost_first(&self) -> impl Iterator<Item = &Refinement> {
        self.refinements[..self.in_force()].iter().rev()
    }
}

/// Whether reducing `key` could possibly produce `candidate` — a necessary condition, tested without reducing anything.
///
/// Reduction substitutes only closed definition bodies and subterms of the term it is reducing, so it can introduce a *global* name and can drop a local, but can never introduce a local the term did not already mention. A candidate naming a binder the key does not is therefore one no reduct of the key will ever equal, whatever the key reduces to.
///
/// That makes this a filter and not a rule: every candidate the eager key would have matched still passes it, because such a candidate *is* a reduct of the key. What it excludes is the traffic — every stuck form produced under a binder some other judgment opened, which is most of what a probe at a stuck reduct sees.
///
/// Globals are deliberately not tested, and the asymmetry is the point: a reduct's globals are not bounded by the key's, so testing them would exclude exactly the unfoldings a settlement exists to perform.
///
/// Being a filter, relaxing it to admit everything changes no verdict and moves no fixture — what it moves is `curios`' `scrutinee_refinement_measurements`, from flat back to the exponential this whole key exists to remove. Tightening it does change verdicts, silently, by dropping refinements; `whnf::tests::a_reduct_that_drops_a_local_is_still_reached` is the guard on that direction.
fn could_reduce_to(key: &Term, candidate: &Term) -> bool {
    let allowed = key.free_vars_shared();

    candidate
        .free_vars_shared()
        .iter()
        .filter(|name| name.is_local())
        .all(|name| allowed.contains(name))
}
