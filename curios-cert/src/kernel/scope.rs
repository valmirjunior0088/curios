//! What the walk in progress has opened: the local telescope, and the case equations assumed inside arms.
//!
//! The two belong to one component because they retract as one. A [`Mark`] is a checkpoint into both stacks and [`Scope::retract`] truncates both, so an arm that opened binders and assumed its scrutinee's case value gives up exactly both on the way out. Splitting them would be two checkpoints that must agree, which is a thing to get wrong rather than a thing to state.
//!
//! `mark` and `retract` are `pub(super)` and `Kernel::scoped` is their only caller anywhere — that is what makes the bracket the one way to open a binder scope, and the reason this component exposes no other way to shrink either stack.

use {
    curios_analysis::{could_reduce_to, records_case_equation},
    curios_core::{Bound, Free, Term},
    std::collections::HashMap,
};

/// A checkpoint into both stacks, restored together so neither can outlive the arm that opened it.
#[derive(Clone, Copy)]
pub(super) struct Mark {
    locals: usize,
    refinements: usize,
}

/// The binders in scope at one moment, as a later moment can ask whether they all still stand: how many there were, and which opening the innermost of them was.
///
/// Locals are a stack, so the innermost opening identifies everything beneath it: a binder is only ever closed after every binder opened inside it, and an opening is never numbered twice. A judgment that read a local's type is true for as long as its prefix stands — and no longer, whatever the binder is called, since a name a caller hands in can come back at another type once the first is closed.
#[derive(Clone, Copy)]
pub(super) struct Prefix {
    depth: usize,
    opening: u64,
}

/// One arm's case equation, under the three spellings a probe may present its subject in.
struct Refinement {
    /// The scrutinee **as written** — the spelling the equation is recorded under, and the one a probe is asked about first.
    key: Term,
    /// The spelling the written one's dispatch resolves to — `(?w).1(a, hi)` as `NatLe(a, hi)` — recorded beside it on entry and asked at the same point, or `None` where the written spelling needs no resolving. See [`resolved_spelling`](super::whnf::resolved_spelling).
    resolved: Option<Term>,
    /// The value this case assumes the scrutinee is.
    value: Term,
    /// The weak-head normal form of `key`, computed at most once and only when a probe has already missed the written spelling. See [`Scope::unasked_refinement`].
    reduct: Reduct,
    /// The depth the equation stack stood at when an arm restated this equation under its solution, or `None` while it answers as recorded. See [`Scope::restate`].
    restated_at: Option<usize>,
}

/// Whether an equation's reduced spelling has been asked for yet.
enum Reduct {
    /// No probe has needed it. Nothing has been reduced for this equation.
    Unasked,
    /// Settled: the reduct, or `None` where reducing it refused.
    Known(Option<Term>),
}

/// What a `let` bound a local to.
struct Value {
    /// The value as the `let` states it, over the binders in scope where it stands — what an arm binds the local again from.
    written: Term,
    /// The value by value: every `let`-bound local it names unfolded in turn. What [`Scope::by_value`] puts in the local's place.
    term: Term,
    /// Whether checking the value, or a `let`-bound local it names, enclosed a group that does not descend.
    encloses_partial: bool,
}

/// A `let`-bound local as an arm reads it, to bind again the ones its solution moves.
pub(crate) struct LetBound {
    pub(crate) name: Free,
    pub(crate) type_: Term,
    /// What it stands for, as its `let` states it.
    pub(crate) written: Term,
    /// What it stands for, by value — which names a variable exactly where a solution for that variable moves it.
    pub(crate) value: Term,
    pub(crate) encloses_partial: bool,
}

/// A binder the walk in progress opened.
struct Local {
    name: Free,
    /// What it was opened at, by value.
    type_: Term,
    /// What a `let` bound it to — `None` for a binder that stands for nothing: a parameter, a payload, a member under check. Typing walks a `let`'s tail over such a local; whatever reads a term for more than its type reads this in its place ([`Scope::by_value`]).
    value: Option<Value>,
    /// Where the binder of the same name this one shadows sits, for [`Scope::retract`] to put back.
    shadowed: Option<usize>,
    /// `type_` as the conversion history keys it, with every binder opened before it renamed to its position — or `None` for a type with loose indices, whose renaming depends on how many binders stand beside it and is taken at each key instead. See [`Scope::history_context`].
    keyed: Option<Term>,
    /// Which opening this is, counted over the whole walk — what a [`Prefix`] names.
    opening: u64,
}

#[derive(Default)]
pub(super) struct Scope {
    /// Binders opened by the walk in progress, outermost first.
    locals: Vec<Local>,
    /// Where each name's innermost binder sits in `locals`. A `let` is a binder, so a chain of them is as deep a scope as it is long, and a lookup that scanned for its name would cost the chain at every variable.
    innermost: HashMap<Free, usize>,
    /// How many of the binders in scope a `let` bound.
    bound: usize,
    /// The case equations of the arms currently being checked, innermost last: within an arm, the scrutinee expression *is* the case's value, definitionally — the built-in face of the convoy pattern, which is how the elaborator's refinement store reads inside an arm. The reducer consults these at stuck heads.
    refinements: Vec<Refinement>,
    /// How many equations are currently in force, when that is fewer than there are. `Some(n)` withholds everything from `n` inwards for the duration of one [`Scope::unasked_refinement`] settlement — see [`Scope::hide_refinements_from`].
    hidden: Option<usize>,
    /// How many binders this walk has opened, closed ones included.
    openings: u64,
}

impl Scope {
    /// Open a binder that stands for nothing: bring `name : type_` into scope for the walk in progress, answering whether that re-types a binder already in scope — an arm re-assumes a local at its specialized type, and the shadow is what a lookup finds for as long as the arm stands, which changes what a term naming it is typed at.
    pub(super) fn assume(&mut self, name: &Free, type_: &Term) -> bool {
        self.open(name, type_, None)
    }

    /// Open a `let`'s binder: bring `name : type_` into scope standing for `value`, which the caller has checked at `type_` where the `let` stands — `encloses_partial` saying that check enclosed a group that does not descend. Answers as [`Scope::assume`] does, an arm binding a local again at the value its solution gives it.
    pub(super) fn bind(
        &mut self,
        name: &Free,
        type_: &Term,
        value: &Term,
        encloses_partial: bool,
    ) -> bool {
        let value = Value {
            encloses_partial: encloses_partial || self.names_partial(value),
            term: self.by_value(value),
            written: value.clone(),
        };

        self.open(name, type_, Some(value))
    }

    /// The type is held by value, so everything that reads a local's type — a variable's typing, the arm rule's re-typing, the conversion history's key — reads the type it reads where a `let` is substituted.
    fn open(&mut self, name: &Free, type_: &Term, value: Option<Value>) -> bool {
        let type_ = self.by_value(type_);
        let keyed = (type_.reach() == 0).then(|| {
            let opened = self
                .locals
                .iter()
                .map(|local| &local.name)
                .collect::<Vec<_>>();
            type_.capture(&opened)
        });
        let shadowed = self.innermost.insert(*name, self.locals.len());
        self.openings += 1;
        self.bound += usize::from(value.is_some());
        self.locals.push(Local {
            name: *name,
            type_,
            value,
            shadowed,
            keyed,
            opening: self.openings,
        });

        shadowed.is_some()
    }

    /// The innermost binder called `name`.
    fn local(&self, name: &Free) -> Option<&Local> {
        self.innermost.get(name).map(|&at| &self.locals[at])
    }

    /// `term` by value: every `let`-bound local it names replaced by what the local stands for, itself by value. This is the term a `let` substituted into its tail leaves in `term`'s place, and it is what every reader of a spelling is handed — reduction and conversion, an elimination's scrutinee and result, an arm's equation, a recorded position, a graded call. Typing alone walks a tail by name, which is what types each value once.
    pub(super) fn by_value(&self, term: &Term) -> Term {
        if self.bound == 0 || !term.has_local_free() {
            return term.clone();
        }
        let values = term
            .free_vars_shared()
            .iter()
            .filter_map(|name| Some((*name, self.local(name)?.value.as_ref()?.term.clone())))
            .collect::<Vec<_>>();

        term.substitute(&values)
    }

    /// Whether `name` is a local a `let` bound.
    pub(super) fn binds(&self, name: &Free) -> bool {
        self.local(name).is_some_and(|local| local.value.is_some())
    }

    /// Whether `term` names a `let`-bound local whose value enclosed a group that does not descend.
    pub(super) fn names_partial(&self, term: &Term) -> bool {
        self.bound != 0
            && term.has_local_free()
            && term.free_vars_shared().iter().any(|name| {
                self.local(name)
                    .and_then(|local| local.value.as_ref())
                    .is_some_and(|value| value.encloses_partial)
            })
    }

    /// The `let`-bound locals in scope, outermost first. A name bound twice is reported once, at its innermost binding.
    pub(super) fn bound_locals(&self) -> Vec<LetBound> {
        self.locals
            .iter()
            .enumerate()
            .filter(|(at, local)| self.innermost.get(&local.name) == Some(at))
            .filter_map(|(_, local)| {
                let value = local.value.as_ref()?;
                Some(LetBound {
                    name: local.name,
                    type_: local.type_.clone(),
                    written: value.written.clone(),
                    value: value.term.clone(),
                    encloses_partial: value.encloses_partial,
                })
            })
            .collect()
    }

    /// The binders in scope now, for [`Scope::stands`] to be asked about later.
    pub(super) fn prefix(&self) -> Prefix {
        Prefix {
            depth: self.locals.len(),
            opening: self.locals.last().map_or(0, |local| local.opening),
        }
    }

    /// Whether every binder `prefix` held is still in scope, as the opening it was.
    pub(super) fn stands(&self, prefix: Prefix) -> bool {
        match prefix.depth.checked_sub(1) {
            None => true,
            Some(innermost) => self
                .locals
                .get(innermost)
                .is_some_and(|local| local.opening == prefix.opening),
        }
    }

    /// Whether any arm's case equation is currently in force — the judgment-side half of the closed machine's gate: inside an arm a closed scrutinee *is* the assumed value, so closed evaluation must stand aside for the strategy that consults these.
    ///
    /// Reads the equations *in force*, not the equations recorded: while a settlement withholds the inner ones, the machine is entitled to run wherever nothing is left to consult.
    pub(super) fn has_refinements(&self) -> bool {
        self.in_force() != 0
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` is `value`, definitionally. Answers whether one was recorded — a scrutinee naming no local records none, and then nothing in force has changed.
    ///
    /// **Keyed on the scrutinee as written.** Keying on its weak-head normal form would reduce it once per arm, and a scrutinee mentioning a local can be memoized by nothing, so a web of combinator definitions each naming the one before it twice would unfold exponentially to produce a key a literal arm body never probes. The written spelling costs nothing to record; the reduced one is computed only when a probe misses, at most once per equation, by [`Scope::unasked_refinement`] and its caller.
    ///
    /// A local-free scrutinee is skipped rather than recorded — [`records_case_equation`], the rule the elaborator records by as well — and that gate sits on the written spelling. Local-free terms reduce to their case values instead of sticking, and the skip is also what keeps the evaluation memos sound — a local-free term's entry outlives the arm, and reduction of a local-free term never encounters a local-bearing stuck form, so no such reduct can depend on an equation that was later retracted; a local-bearing term's entry may, and is cleared with the equation. The reduced spelling a settlement computes is *not* covered by this gate, and does not need to be: the probe that consults it is asked only about local-bearing terms, so a local-free reduct can be recorded and can never fire. The resolved spelling *is* gated, separately, because it is asked at the probe before decomposition, which every term reaches: opening a dispatch substitutes its arguments into the method's body, and a method that ignores its local argument resolves to a local-free comparison that would then answer local-free terms inside the arm and hand the memos an entry resting on the equation.
    ///
    /// An equation is a claim about *one* term, so the only sound key is one that identifies terms already definitionally equal, and structural equality is the under-approximation of that which costs nothing to justify. Both spellings satisfy it — the written one *is* the scrutinee, and the reduced one is what the kernel's own reduction says it computes to.
    ///
    /// Keying through `project_erased_universes` would rest on the premise that a universe argument cannot affect computation, and the premise is false: Core has no eliminator over levels, but `Type u` embeds one *in a term*, so a definition carrying its parameter into a constructor payload reduces to genuinely different values at two instances — and that projection rebuilds every `Type` payload at one ground level, because it is written for the Core-to-Ersd hand-off where levels really are irrelevant. Read as a quotient by definitional equality it identifies `Type 0` with `Type 1`, which is the universe hierarchy's whole content. See `crate::recheck::universes_tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance`.
    pub(super) fn refine(&mut self, scrutinee: Term, resolved: Option<Term>, value: Term) -> bool {
        let recorded = records_case_equation(&scrutinee);
        if recorded {
            self.refinements.push(Refinement {
                key: scrutinee,
                resolved: resolved.filter(records_case_equation),
                value,
                reduct: Reduct::Unasked,
                restated_at: None,
            });
        }

        recorded
    }

    /// Restate the equations in force under an arm's solution, answering whether any was: each that names a solved variable — in its key, its resolved spelling or its value — is assumed again with the solution substituted through all three, and stops answering until the arm retracts.
    ///
    /// **An arm is checked under its solution, and so are the equations it was opened under.** The kernel holds no refinement store, so it substitutes a case's solution through the arm's body, its expectation and the types of the locals it re-types: within the arm a solved variable is spelled as its value. An equation recorded outside still names the variable, so its key is a term the arm never holds, and a guard's fact would be lost at the first match on a variable the guard names. The restated equation is the recorded one at the instance the arm is checked at — what holds of the scrutinee at every value of the variable holds at the one the case fixes — so it is the same hypothesis, read where the arm reads everything else.
    ///
    /// **The recorded equation steps aside rather than standing beside its instance.** Nothing the arm holds names a solved variable, so the recorded spelling answers nothing there; left in force, it would be restated again by every arm inside, and an equation naming the variables of `n` nested matches would stand `2^n` times. Stepping aside, each equation stands once however deep the arms go.
    ///
    /// A restated equation goes through [`Scope::refine`] like any other, so one the solution leaves with no local is not recorded, for the reason no local-free equation is. It is pushed inside the arm's bracket, and [`Scope::retract`] both drops it and lets the recorded one answer again.
    pub(super) fn restate(&mut self, solutions: &[(Free, Term)]) -> bool {
        debug_assert!(
            self.hidden.is_none(),
            "an arm's solution restated while an equation's reduced spelling was being settled"
        );

        let solved = |term: &Term| solutions.iter().any(|(name, _)| term.mentions_free(name));
        let depth = self.refinements.len();
        let mut restated = Vec::new();
        for entry in &mut self.refinements {
            if entry.restated_at.is_none()
                && (solved(&entry.key)
                    || solved(&entry.value)
                    || entry.resolved.as_ref().is_some_and(solved))
            {
                entry.restated_at = Some(depth);
                restated.push((
                    entry.key.substitute(solutions),
                    entry
                        .resolved
                        .as_ref()
                        .map(|resolved| resolved.substitute(solutions)),
                    entry.value.substitute(solutions),
                ));
            }
        }

        let any = !restated.is_empty();
        for (key, resolved, value) in restated {
            self.refine(key, resolved, value);
        }
        any
    }

    /// The case value the term `term` is refined to under the *written* spelling or its resolved one, innermost arm first.
    ///
    /// Probed by the keys [`Scope::refine`] stores under: the scrutinee itself, and the spelling its dispatch resolves to, which is what a probe presents once reduction has opened the same dispatch — so an intrinsic comparison meets a guard written through a concept before anything folds it.
    pub(super) fn refinement_of(&self, term: &Term) -> Option<Term> {
        self.in_force_innermost_first()
            .find(|entry| entry.key == *term || entry.resolved.as_ref() == Some(term))
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

    /// The equations in force whose reduced spelling is settled and that `candidate` could be a reduct of, innermost first: each as its position, that spelling and the value it assumes. What [`answers`](curios_analysis::answers) is asked of.
    pub(super) fn settled_refinements(&self, candidate: &Term) -> Vec<(usize, Term, Term)> {
        self.refinements[..self.in_force()]
            .iter()
            .enumerate()
            .rev()
            .filter(|(_, entry)| {
                entry.restated_at.is_none() && could_reduce_to(&entry.key, candidate)
            })
            .filter_map(|(index, entry)| match &entry.reduct {
                Reduct::Known(Some(reduct)) => Some((index, reduct.clone(), entry.value.clone())),
                _ => None,
            })
            .collect()
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
                entry.restated_at.is_none()
                    && matches!(entry.reduct, Reduct::Unasked)
                    && could_reduce_to(&entry.key, candidate)
            })
            .map(|(index, entry)| (index, entry.key.clone()))
    }

    /// How many equations in force `candidate` could be a reduct of — what a profile counts a missed probe by.
    #[cfg(feature = "profile")]
    pub(super) fn reachable_refinements(&self, candidate: &Term) -> usize {
        self.refinements[..self.in_force()]
            .iter()
            .filter(|entry| entry.restated_at.is_none() && could_reduce_to(&entry.key, candidate))
            .count()
    }

    /// Record what the equation at `index` reduces to, or that reducing it refused.
    pub(super) fn settle_refinement(&mut self, index: usize, reduct: Option<Term>) {
        self.refinements[index].reduct = Reduct::Known(reduct);
    }

    /// Withhold the equation at `index` and every equation inside it, handing back the previous limit for [`Scope::show_refinements`].
    ///
    /// **This is what makes an equation's reduced spelling rest only on equations outside it.** Those retract no earlier than it does, so a reduct computed under them stays true for exactly as long as the entry holding it. An eager reduction, run before the equation is pushed and while the stack below it is frozen, would have that guarantee for free; computing the same reduct later has to reconstruct the same view, and withholding is how.
    ///
    /// Withholding the entry itself is the other half: without it, reducing `key` meets `key` at the reducer's own first probe and answers the case value, so the equation would settle its reduced spelling to whatever it was assuming.
    pub(super) fn hide_refinements_from(&mut self, index: usize) -> Option<usize> {
        self.hidden.replace(index)
    }

    /// Put back what [`Scope::hide_refinements_from`] withheld.
    pub(super) fn show_refinements(&mut self, previous: Option<usize>) {
        self.hidden = previous;
    }

    /// The types of the binders currently in scope, outermost first.
    pub(super) fn local_types(&self) -> Vec<Term> {
        self.locals
            .iter()
            .map(|local| local.type_.clone())
            .collect()
    }

    /// The identities of the binders currently in scope, outermost first — parallel to [`Scope::local_types`]. What the conversion history renames away, so that a goal reached again on a later round of an unfolding cycle is recognized as the goal it already is.
    pub(super) fn local_names(&self) -> Vec<Free> {
        self.locals.iter().map(|local| local.name).collect()
    }

    /// The types of the binders currently in scope, outermost first, each with every binder renamed to its position: the context the conversion history keys a goal on, since the same goal under a different context is a different goal.
    ///
    /// **Renamed once per binder, when it opens, rather than at every goal.** A local's type mentions only binders opened before it, and `capture` gives a binder the index of its position in the list — so renaming it against the binders in scope at a goal, all of them, gives what renaming it against those before it gave, and the history's keys are unchanged. What it saves is the cost: the history keys every comparison it enters, and renaming the whole context at each would spend a type-level text search — compared under binders whose types carry its positions, whose graphs are large — most of its time renaming the same context over again. A type with loose indices is the exception, because its renaming shifts them past however many binders stand beside it, so it is renamed at each key.
    pub(super) fn history_context(&self) -> Vec<Term> {
        let names = self
            .locals
            .iter()
            .map(|local| &local.name)
            .collect::<Vec<_>>();
        self.locals
            .iter()
            .map(|local| match &local.keyed {
                Some(keyed) => keyed.clone(),
                None => local.type_.capture(&names),
            })
            .collect()
    }

    /// The type `name` was opened at, if it is a binder currently in scope.
    ///
    /// The innermost binder of the name: an arm opens a local again at its specialized type, and that is the one a lookup finds while the arm stands.
    pub(super) fn local_type(&self, name: &Free) -> Option<&Term> {
        self.local(name).map(|local| &local.type_)
    }

    /// The current depth of both stacks, to be handed back to [`Scope::retract`].
    pub(super) fn mark(&self) -> Mark {
        Mark {
            locals: self.locals.len(),
            refinements: self.refinements.len(),
        }
    }

    /// Close every binder opened — and drop every case equation assumed — since `mark`, answering whether the equations in force changed: one was dropped, or one an arm inside had restated answers as recorded again.
    ///
    /// No settlement can be in progress here, and that is structural rather than checked by discipline: `Kernel::scoped` is this method's only caller, and reduction — the only thing a settlement runs — never opens a binder scope.
    pub(super) fn retract(&mut self, mark: Mark) -> bool {
        debug_assert!(
            self.hidden.is_none(),
            "a scope retracted while an equation's reduced spelling was being settled"
        );

        // Innermost first, so a name opened twice inside the bracket ends at the binder that stood before it.
        for local in self.locals.drain(mark.locals..).rev() {
            self.bound -= usize::from(local.value.is_some());
            match local.shadowed {
                Some(at) => self.innermost.insert(local.name, at),
                None => self.innermost.remove(&local.name),
            };
        }
        let dropped = self.refinements.len() > mark.refinements;
        self.refinements.truncate(mark.refinements);

        // An equation restated at or past the mark was restated by an arm this bracket held, which is gone with its restatement.
        let mut revived = false;
        for entry in &mut self.refinements {
            if entry
                .restated_at
                .is_some_and(|depth| depth >= mark.refinements)
            {
                entry.restated_at = None;
                revived = true;
            }
        }

        dropped || revived
    }

    /// How many equations are in force: all of them, unless a settlement is withholding the inner ones.
    fn in_force(&self) -> usize {
        self.hidden.unwrap_or(self.refinements.len())
    }

    /// The equations in force that answer, innermost first — the order every probe reads them in. One an arm has restated answers through its restatement.
    fn in_force_innermost_first(&self) -> impl Iterator<Item = &Refinement> {
        self.refinements[..self.in_force()]
            .iter()
            .rev()
            .filter(|entry| entry.restated_at.is_none())
    }
}
