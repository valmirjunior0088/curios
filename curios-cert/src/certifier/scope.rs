//! What the walk in progress has opened: the local telescope, and the case equations assumed inside arms.
//!
//! The two belong to one component because they retract as one. A [`Mark`] is a checkpoint into both stacks and [`Scope::retract`] truncates both, so an arm that opened binders and assumed its scrutinee's case value gives up exactly both on the way out. Splitting them would be two checkpoints that must agree, which is a thing to get wrong rather than a thing to state.
//!
//! `mark` and `retract` are `pub(super)` and the certifier's bracket is their only caller anywhere, opened by `Certifier::scoped` and by a reading's `ByValue::scoped` — that is what makes the bracket the one way to open a binder scope, and the reason this component exposes no other way to shrink either stack.

use {
    curios_analysis::{reads_one_of, records_case_equation},
    curios_core::{Free, Term},
    std::collections::{BTreeMap, HashMap},
};

/// A checkpoint into both stacks, restored together so neither can outlive the arm that opened it.
#[derive(Clone, Copy)]
pub struct Mark {
    locals: usize,
    refinements: usize,
}

/// The binders in scope at one moment, as a later moment can ask whether they all still stand: how many there were, and which opening the innermost of them was.
///
/// Locals are a stack, so the innermost opening identifies everything beneath it: a binder is only ever closed after every binder opened inside it, and an opening is never numbered twice. A judgment that read a local's type is true for as long as its prefix stands — and no longer, whatever the binder is called, since a name a caller hands in can come back at another type once the first is closed.
#[derive(Clone, Copy)]
pub struct Prefix {
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
    /// The weak-head normal form of `key` twice over — as a judgment reduces it, and as plain reduction does, a reading's mode ([`ByValue::plainly`](super::ByValue::plainly)) — each computed at most once and only when a probe of that reduction has already missed the written spelling. See [`Scope::unasked_refinement`].
    ///
    /// One for each because a probe is compared with the spelling reduced the way the probe was: plain reduction asks its checker's conversion nothing, so where a key holds a fold only a question decides, its two reducts are two terms.
    ///
    /// **Its own spelling**: settled with the equation and every equation inside it withheld, so it holds while the equation does.
    reducts: [Reduct; 2],
    /// The reduced spelling again at each height past its own a probe has asked for it at, twice over as [`Refinement::reducts`] is: its own spelling reduced on with the equations recorded since in view and this one alone withheld. See [`Scope::height`].
    ///
    /// **An arm on an expression moves what an equation is spelled as, and nothing else of it.** The arm assumes an equation of its own, which an equation recorded outside it did not read: under a guard `pick(c)` whose definition matches on `e(c)`, an arm on `e(c)` is where the guard's spelling is a case of that match. Which equations an arm's own moves is known only by settling them under it, so every equation is settled again at each height it is asked at, and none until a probe asks; it keeps its place among the equations and the value it assumes, so an arm's own equation still answers before one recorded outside it.
    ///
    /// **Held while the equations it was settled under stand**: a retraction drops each settled with a dropped equation in view ([`Scope::retract`]).
    again: BTreeMap<usize, [Reduct; 2]>,
    /// The depth the locals stood at when an arm restated this equation under its solution, or `None` while it answers as recorded. See [`Scope::restate`].
    restated_at: Option<usize>,
}

/// The equations a settlement, or a question asked in a settlement's view, withholds.
#[derive(Clone, Copy, Debug)]
pub struct Withheld {
    /// Every equation from this position inwards.
    limit: usize,
    /// One equation before the limit, alone: the one whose spelling is settled again with the others in view.
    masked: Option<usize>,
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
    /// The value as a graded call reads it: every `let`-bound local it names unfolded in turn but one an arm's index inversion solved. What [`Scope::graded`] puts in the local's place.
    graded: Term,
    /// Whether checking the value, or a `let`-bound local it names, enclosed a group that does not descend.
    encloses_partial: bool,
    /// Whether an arm's index inversion bound it to its solution, which refines for typing alone: the size order reads nothing an inversion concluded, so a graded call reads the local as written.
    solved: bool,
}

/// A `let`-bound local as an arm reads it, to bind again the ones its solution moves.
pub struct LetBound {
    pub name: Free,
    pub type_: Term,
    /// What it stands for, as its `let` states it.
    pub written: Term,
    /// What it stands for, by value — which names a variable exactly where a solution for that variable moves it.
    pub value: Term,
    pub encloses_partial: bool,
    /// Whether an arm's index inversion bound it, which a binding again keeps.
    pub solved: bool,
}

/// A binder the walk in progress opened.
struct Local {
    name: Free,
    /// What it was opened at, by value.
    type_: Term,
    /// What a `let` bound it to — `None` for a binder that stands for nothing: a parameter, a payload, a member under check, which reduction alone unfolds once it is known to terminate (`Calls::member_value`). Typing walks a `let`'s tail over such a local; whatever reads a term for more than its type reads this in its place ([`Scope::by_value`]).
    value: Option<Value>,
    /// Where the binder of the same name this one shadows sits, for [`Scope::retract`] to put back.
    shadowed: Option<usize>,
    /// Which opening this is, counted over the whole walk — what a [`Prefix`] names.
    opening: u64,
    /// Whether the binder is itself a proof — opened at a proposition — once asked: `None` until a stuck form naming it is put to the equations in force ([`ByValue::proofs_named`](super::ByValue::proofs_named)).
    proof: Option<bool>,
}

#[derive(Default)]
pub struct Scope {
    /// Binders opened by the walk in progress, outermost first.
    locals: Vec<Local>,
    /// Where each name's innermost binder sits in `locals`. A `let` is a binder, so a chain of them is as deep a scope as it is long, and a lookup that scanned for its name would cost the chain at every variable.
    innermost: HashMap<Free, usize>,
    /// How many of the binders in scope a `let` bound.
    bound: usize,
    /// The case equations of the arms currently being checked, innermost last: within an arm, the scrutinee expression *is* the case's value, definitionally — the built-in face of the convoy pattern, which is how the elaborator's refinement store reads inside an arm. The reducer consults these at stuck heads.
    refinements: Vec<Refinement>,
    /// The equations withheld, when any is: for the duration of one settlement, or of one question asked in a settlement's view — see [`Scope::hide_refinements_from`] and [`Scope::mask_refinement`].
    hidden: Option<Withheld>,
    /// How many binders this walk has opened, closed ones included.
    openings: u64,
}

impl Scope {
    /// Open a binder that stands for nothing: bring `name : type_` into scope for the walk in progress, answering whether that re-types a binder already in scope — an arm re-assumes a local at its specialized type, and the shadow is what a lookup finds for as long as the arm stands, which changes what a term naming it is typed at.
    pub fn assume(&mut self, name: &Free, type_: &Term) -> bool {
        self.open(name, type_, None)
    }

    /// Open a `let`'s binder: bring `name : type_` into scope standing for `value`, which the caller has checked at `type_` where the `let` stands — `encloses_partial` saying that check enclosed a group that does not descend. Answers as [`Scope::assume`] does, an arm binding a local again at the value its solution gives it.
    pub fn bind(
        &mut self,
        name: &Free,
        type_: &Term,
        value: &Term,
        encloses_partial: bool,
        solved: bool,
    ) -> bool {
        let value = Value {
            encloses_partial: encloses_partial || self.names_partial(value),
            term: self.by_value(value),
            graded: self.graded(value),
            written: value.clone(),
            solved,
        };

        self.open(name, type_, Some(value))
    }

    /// The type is held by value, so everything that reads a local's type — a variable's typing, the arm rule's re-typing — reads the type it reads where a `let` is substituted.
    fn open(&mut self, name: &Free, type_: &Term, value: Option<Value>) -> bool {
        let type_ = self.by_value(type_);
        let shadowed = self.innermost.insert(*name, self.locals.len());
        self.openings += 1;
        self.bound += usize::from(value.is_some());
        self.locals.push(Local {
            name: *name,
            type_,
            value,
            shadowed,
            opening: self.openings,
            proof: None,
        });

        shadowed.is_some()
    }

    /// The innermost binder called `name`.
    fn local(&self, name: &Free) -> Option<&Local> {
        self.innermost.get(name).map(|&at| &self.locals[at])
    }

    /// `term` by value: every `let`-bound local it names replaced by what the local stands for, itself by value. This is the term a `let` substituted into its tail leaves in `term`'s place, and it is what every reader of a spelling is handed — reduction and conversion, an elimination's scrutinee and result, an arm's equation, a recorded position, a graded call. Typing alone walks a tail by name, which is what types each value once.
    pub fn by_value(&self, term: &Term) -> Term {
        if self.bound == 0 || !term.has_local_free() {
            return term.clone();
        }
        let values = term
            .mentions()
            .locals()
            .filter_map(|name| Some((*name, self.local(name)?.value.as_ref()?.term.clone())))
            .collect::<Vec<_>>();

        term.substitute(&values)
    }

    /// `term` as a graded call reads it: [`Scope::by_value`] but for a local an arm's index inversion solved, which stays as written, since the size order reads nothing an inversion concluded. A `let` bound over one reads it as written too, its own graded value having kept it.
    pub fn graded(&self, term: &Term) -> Term {
        if self.bound == 0 || !term.has_local_free() {
            return term.clone();
        }
        let values = term
            .mentions()
            .locals()
            .filter_map(|name| {
                let value = self.local(name)?.value.as_ref()?;
                (!value.solved).then(|| (*name, value.graded.clone()))
            })
            .collect::<Vec<_>>();

        term.substitute(&values)
    }

    /// Whether the innermost local called `name` stands for nothing, so that a `let` binding it binds over a binder.
    pub fn assumes(&self, name: &Free) -> bool {
        self.local(name).is_some_and(|local| local.value.is_none())
    }

    /// Whether `term` names a `let`-bound local whose value enclosed a group that does not descend.
    pub fn names_partial(&self, term: &Term) -> bool {
        self.bound != 0
            && term.has_local_free()
            && term.mentions().locals().any(|name| {
                self.local(name)
                    .and_then(|local| local.value.as_ref())
                    .is_some_and(|value| value.encloses_partial)
            })
    }

    /// The `let`-bound locals in scope, outermost first. A name bound twice is reported once, at its innermost binding.
    pub fn bound_locals(&self) -> Vec<LetBound> {
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
                    solved: value.solved,
                })
            })
            .collect()
    }

    /// How many binders this walk has opened, closed ones included: a binder opened later is a higher opening.
    pub fn openings(&self) -> u64 {
        self.openings
    }

    /// The newest of the binders the locals `term` names stand at, each the innermost of its name, or zero where it names none in scope.
    pub fn newest_binder_named(&self, term: &Term) -> u64 {
        term.mentions()
            .locals()
            .filter_map(|name| self.local(name))
            .map(|local| local.opening)
            .max()
            .unwrap_or(0)
    }

    /// The binders in scope now, for [`Scope::stands`] to be asked about later.
    pub fn prefix(&self) -> Prefix {
        Prefix {
            depth: self.locals.len(),
            opening: self.locals.last().map_or(0, |local| local.opening),
        }
    }

    /// Whether every binder `prefix` held is still in scope, as the opening it was.
    pub fn stands(&self, prefix: Prefix) -> bool {
        match prefix.depth.checked_sub(1) {
            None => true,
            Some(innermost) => self
                .locals
                .get(innermost)
                .is_some_and(|local| local.opening == prefix.opening),
        }
    }

    /// Whether any arm's case equation is currently in force: what files a judgment read off the scope apart from one that is the declaration's ([`Memos`](super::memos::Memos)), and what spares a stuck form the probe where there is nothing to consult.
    ///
    /// Reads the equations *in force*, not the equations recorded: while a settlement withholds the inner ones, nothing is left to consult where none stands.
    pub fn has_refinements(&self) -> bool {
        self.in_force() != 0
    }

    /// Assume an arm's case equation: within the arm, `scrutinee` is `value`, definitionally. Answers whether one was recorded — a scrutinee naming no local records none, and then nothing in force has changed. Both are by value: the door that records one unfolds them (`Certifier::refine`), and a restatement substitutes a solution into a recorded one ([`Scope::restate`]).
    ///
    /// **Keyed on the scrutinee as written.** Keying on its weak-head normal form would reduce it once per arm, and a scrutinee mentioning a local can be memoized by nothing, so a web of combinator definitions each naming the one before it twice would unfold exponentially to produce a key a literal arm body never probes. The written spelling costs nothing to record; the reduced one is computed only when a probe misses, at most once per equation, by [`Scope::unasked_refinement`] and its caller.
    ///
    /// A local-free scrutinee is skipped rather than recorded — [`records_case_equation`], the rule the elaborator records by as well — and that gate sits on the written spelling. Local-free terms reduce to their case values instead of sticking, and the skip is also what keeps the evaluation memos sound — a local-free term's entry outlives the arm, and reduction of a local-free term never encounters a local-bearing stuck form, so no such reduct can depend on an equation that was later retracted; a local-bearing term's entry may, and is cleared with the equation. The reduced spelling a settlement computes is *not* covered by this gate, and does not need to be: the probe that consults it is asked only about local-bearing terms, so a local-free reduct can be recorded and can never fire. The resolved spelling *is* gated, separately, because it is asked at the probe before decomposition, which every term reaches: opening a dispatch substitutes its arguments into the method's body, and a method that ignores its local argument resolves to a local-free comparison that would then answer local-free terms inside the arm and hand the memos an entry resting on the equation.
    ///
    /// An equation is a claim about *one* term, so the only sound key is one that identifies terms already definitionally equal, and structural equality is the under-approximation of that which costs nothing to justify. Both spellings satisfy it — the written one *is* the scrutinee, and the reduced one is what the certifier's own reduction says it computes to.
    ///
    /// Keying through `project_erased_universes` would rest on the premise that a universe argument cannot affect what a term computes to, and the premise is false: Core has no eliminator over levels, but `Type u` embeds one *in a term*, so a definition carrying its parameter into a constructor payload reduces to genuinely different values at two instances — and that projection rebuilds every `Type` payload at one ground level, because it is written for the Core-to-ANF hand-off where levels really are irrelevant. Read as a quotient by definitional equality it identifies `Type 0` with `Type 1`, which is the universe hierarchy's whole content. See `crate::recheck::universes_tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance`.
    pub fn refine(&mut self, scrutinee: Term, resolved: Option<Term>, value: Term) -> bool {
        let recorded = records_case_equation(&scrutinee);
        if recorded {
            self.refinements.push(Refinement {
                key: scrutinee,
                resolved: resolved.filter(records_case_equation),
                value,
                reducts: [Reduct::Unasked, Reduct::Unasked],
                again: BTreeMap::new(),
                restated_at: None,
            });
        }

        recorded
    }

    /// Restate the equations in force under an arm's solution, answering each as it was restated, its scrutinee and the case it assumes: each that names a solved variable — in its key, its resolved spelling or its value — is assumed again with the solution substituted through all three, and stops answering until the arm retracts.
    ///
    /// **An arm is checked under its solution, and so are the equations it was opened under.** The certifier binds each variable a case solves to its solution, re-types the locals that name it, and reads the arm by value: to every reader in the arm a solved variable is spelled as its value. An equation recorded outside still names the variable, so its key is a term no reader in the arm is handed, and a guard's fact would be lost at the first match on a variable the guard names. The restated equation is the recorded one at the instance the arm is checked at — what holds of the scrutinee at every value of the variable holds at the one the case fixes — so it is the same hypothesis, read where the arm reads everything else.
    ///
    /// **The recorded equation steps aside rather than standing beside its instance.** Nothing a reader in the arm is handed names a solved variable, so the recorded spelling answers nothing there; left in force, it would be restated again by every arm inside, and an equation naming the variables of `n` nested matches would stand `2^n` times. Stepping aside, each equation stands once however deep the arms go.
    ///
    /// **An equation whose settlement would read the solution is restated too**, though it names no solved variable: one that names a local whose type, or whose value where a `let` bound it or `member_value` knows it as a member of a group being checked, names one, read through to the end ([`reads_one_of`]). Binding a solved variable re-types the first kind of local and binds the second again (`Certifier::bind`), and a settlement reads all three, a `let` by its value, a member known to terminate by its selection and every local at its type, so a spelling settled under the arm would rest on what the arm assumes while the equation outlives the arm. Restated, the spelling is settled in the arm and goes with it, and the recorded equation is settled where the arm does not stand.
    ///
    /// **An arm on an expression restates nothing.** It solves no variable, so every reader in it is handed each equation as it was recorded, and what its own equation moves of one is that equation's spelling there, which the equation settles again where it is asked ([`Scope::height`]).
    ///
    /// A restated equation goes through [`Scope::refine`] like any other, so one the solution leaves with no local is not recorded, for the reason no local-free equation is. It is pushed inside the arm's bracket, and [`Scope::retract`] both drops it and lets the recorded one answer again. It is answered whether or not it was recorded: the arm assumes it either way, and an arm refused under one that computes to another case is one no reduction takes.
    pub fn restate(
        &mut self,
        solutions: &[(Free, Term)],
        member_value: impl Fn(&Free) -> Option<Term>,
    ) -> Vec<(Term, Term)> {
        assert!(
            self.hidden.is_none(),
            "an arm's solution restated while an equation's reduced spelling was being settled"
        );

        // A local's type is held by value, and so is a `let`'s value, so neither names a `let` the reading would have to follow further; a member's selection names the locals its group's bodies captured.
        let reads = |name: &Free| {
            let local = self.innermost.get(name).map(|&at| &self.locals[at]);
            local
                .map(|local| local.type_.clone())
                .into_iter()
                .chain(local.and_then(|local| local.value.as_ref().map(|value| value.term.clone())))
                .chain(member_value(name))
                .collect::<Vec<_>>()
        };
        let solved_names = solutions.iter().map(|(name, _)| *name).collect::<Vec<_>>();
        let solved = |term: &Term| reads_one_of(term, &solved_names, &reads);
        let restating = self
            .refinements
            .iter()
            .enumerate()
            .filter(|(_, entry)| {
                entry.restated_at.is_none()
                    && (solved(&entry.key)
                        || solved(&entry.value)
                        || entry.resolved.as_ref().is_some_and(solved))
            })
            .map(|(index, _)| index)
            .collect::<Vec<_>>();

        // Filed against the locals, which the arm binds its solution among next, so every bracket the arm opens from then on marks deeper and only the arm's own closing, or an outer one's, revives the equation. The equation stack would not do: a restatement naming no local is not recorded, so a bracket the arm opens marks the depth it was restated at.
        let depth = self.locals.len();
        let mut restated = Vec::new();
        for index in restating {
            let entry = &mut self.refinements[index];
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

        restated
            .into_iter()
            .map(|(key, resolved, value)| {
                self.refine(key.clone(), resolved, value.clone());

                (key, value)
            })
            .collect()
    }

    /// The case value the term `term` is refined to under the *written* spelling or its resolved one, innermost arm first.
    ///
    /// Probed by the keys [`Scope::refine`] stores under: the scrutinee itself, and the spelling its dispatch resolves to, which is what a probe presents once reduction has opened the same dispatch — so an intrinsic comparison meets a guard written through a concept before anything folds it.
    pub fn refinement_of(&self, term: &Term) -> Option<Term> {
        self.in_force_innermost_first()
            .find(|(_, entry)| entry.key == *term || entry.resolved.as_ref() == Some(term))
            .map(|(_, entry)| entry.value.clone())
    }

    /// The case value `term` is refined to under a *reduced* spelling already settled, innermost arm first.
    ///
    /// The escalation the written spelling's probe misses reach, and it settles nothing itself: an equation whose reduced spelling has never been asked for at the height in view cannot answer here.
    pub fn refinement_of_reduct(&self, term: &Term, plain: bool) -> Option<Term> {
        self.in_force_innermost_first()
            .find(|(index, _)| match self.reduct(*index, plain) {
                Reduct::Known(Some(reduct)) => reduct == term,
                _ => false,
            })
            .map(|(_, entry)| entry.value.clone())
    }

    /// The height the view in force reads a reduced spelling at: how many equations it holds one up against. With one equation withheld alone, whose spelling is being settled again or asked about, that is one short of the view: the others answer as they did before the innermost of them was recorded.
    ///
    /// **An equation's spelling at a height is a function of the spellings beneath it.** At its own height, one past its position, it is its key reduced with the equation and every one inside it withheld ([`Scope::hide_refinements_from`]). At each height past that it is its own spelling reduced on, with every equation before the height in view but itself ([`Scope::mask_refinement`]), each of those read one height down. So a spelling at a height rests only on spellings at lower ones, and which of two equations a probe asked first moves neither.
    pub fn height(&self) -> usize {
        self.in_force() - usize::from(self.masked().is_some())
    }

    /// The reduced spelling of the equation at `index` as the view in force reads it: its own up to its own height, and past that the one settled again at the view's height.
    fn reduct(&self, index: usize, plain: bool) -> &Reduct {
        let entry = &self.refinements[index];
        let height = self.height();
        match height <= index + 1 {
            true => &entry.reducts[usize::from(plain)],
            false => entry
                .again
                .get(&height)
                .map_or(&Reduct::Unasked, |reducts| &reducts[usize::from(plain)]),
        }
    }

    /// What the equation at `index` settled to as its own spelling: `None` where no probe has asked for it, `Some(None)` where reducing its key refused.
    pub fn own_reduct(&self, index: usize, plain: bool) -> Option<Option<Term>> {
        match &self.refinements[index].reducts[usize::from(plain)] {
            Reduct::Unasked => None,
            Reduct::Known(reduct) => Some(reduct.clone()),
        }
    }

    /// The equations in force a stuck form may be put to, as their positions and their keys: each no arm has restated. What [`could_answer`](curios_analysis::could_answer) is asked of, once a probe, to admit the positions the two lookups below read.
    pub fn keys_in_force(&self) -> Vec<(usize, Term)> {
        self.in_force_innermost_first()
            .map(|(index, entry)| (index, entry.key.clone()))
            .collect()
    }

    /// The equations in force among `admitted` whose reduced spelling is settled at the height in view, innermost first: each as its position, that spelling and the value it assumes. What [`answers`](curios_analysis::answers) is asked of.
    pub fn settled_refinements(&self, admitted: &[usize], plain: bool) -> Vec<(usize, Term, Term)> {
        self.in_force_innermost_first()
            .filter(|(index, _)| admitted.contains(index))
            .filter_map(|(index, entry)| match self.reduct(index, plain) {
                Reduct::Known(Some(reduct)) => Some((index, reduct.clone(), entry.value.clone())),
                _ => None,
            })
            .collect()
    }

    /// The innermost equation in force among `admitted` whose reduced spelling has not been asked for at the height in view, as its position and its key.
    ///
    /// The position is always inside the current limit, which is what lets [`Scope::hide_refinements_from`] take it as the new limit rather than the smaller of the two: a settlement can only ever reach further out than the one it is nested in.
    ///
    /// **Reading the view here is also what makes the settlement loop finite**, and that is a second job rather than a restatement of the first. An entry whose own spelling is being settled is outside the limit for the whole of its reduction, and one being settled again is the one withheld alone, so a probe reached from inside cannot select either again at the height it is being settled at; and what a probe inside settles is read one height down ([`Scope::height`]), so the heights a nest of settlements asks at only fall.
    /// **`admitted` is what decides whether the reduction happens at all**, and without that test the deferral buys nothing. A settlement is the whole cost the two-tier key exists to avoid, and a probe reached under freshly opened binders — `Sort::of` walking a telescope, an arm body's own erasure obligations — presents a stuck form on almost every reduction, so *some* term would trigger a settlement in any arm whatever. What [`could_answer`](curios_analysis::could_answer) tests is the one thing a reduct's spelling cannot lie about: the binders conversion reads of it.
    pub fn unasked_refinement(&self, admitted: &[usize], plain: bool) -> Option<(usize, Term)> {
        self.in_force_innermost_first()
            .find(|(index, _)| {
                admitted.contains(index) && matches!(self.reduct(*index, plain), Reduct::Unasked)
            })
            .map(|(index, entry)| (index, entry.key.clone()))
    }

    /// Record what the equation at `index` reduces to at `height`, or that reducing it refused: its own spelling up to its own height, and its spelling again past it.
    pub fn settle_refinement(
        &mut self,
        index: usize,
        height: usize,
        plain: bool,
        reduct: Option<Term>,
    ) {
        let entry = &mut self.refinements[index];
        let reducts = match height <= index + 1 {
            true => &mut entry.reducts,
            false => entry
                .again
                .entry(height)
                .or_insert([Reduct::Unasked, Reduct::Unasked]),
        };
        reducts[usize::from(plain)] = Reduct::Known(reduct);
    }

    /// Withhold the equation at `index` and every equation inside it, handing back what was withheld before for [`Scope::show_refinements`].
    ///
    /// **This is what makes an equation's own spelling rest only on equations outside it.** Those retract no earlier than it does, so a reduct computed under them stays true for exactly as long as the entry holding it. An eager reduction, run before the equation is pushed and while the stack below it is frozen, would have that guarantee for free; computing the same reduct later has to reconstruct the same view, and withholding is how.
    ///
    /// Withholding the entry itself is the other half: without it, reducing `key` meets `key` at the reducer's own first probe and answers the case value, so the equation would settle its reduced spelling to whatever it was assuming.
    pub fn hide_refinements_from(&mut self, index: usize) -> Option<Withheld> {
        self.hidden.replace(Withheld {
            limit: index,
            masked: None,
        })
    }

    /// Withhold the equation at `index` alone among those before `height`, and every equation from `height` inwards, handing back what was withheld before for [`Scope::show_refinements`]: the view an equation's spelling is settled again in at `height`, and asked about.
    ///
    /// **The equations recorded since are in view, which is what the spelling is settled again for**; the equation itself is withheld for the reason its own settlement withholds it. A spelling settled so rests on equations inside the one holding it, which retract first, so it is held under its height and dropped with them ([`Scope::retract`]).
    pub fn mask_refinement(&mut self, height: usize, index: usize) -> Option<Withheld> {
        self.hidden.replace(Withheld {
            limit: height,
            masked: Some(index),
        })
    }

    /// Put back what [`Scope::hide_refinements_from`] or [`Scope::mask_refinement`] withheld.
    pub fn show_refinements(&mut self, previous: Option<Withheld>) {
        self.hidden = previous;
    }

    /// The types of the binders currently in scope, outermost first.
    pub fn local_types(&self) -> Vec<Term> {
        self.locals
            .iter()
            .map(|local| local.type_.clone())
            .collect()
    }

    /// The identities of the binders currently in scope, outermost first — parallel to [`Scope::local_types`].
    pub fn local_names(&self) -> Vec<Free> {
        self.locals.iter().map(|local| local.name).collect()
    }

    /// The type `name` was opened at, if it is a binder currently in scope.
    ///
    /// The innermost binder of the name: an arm opens a local again at its specialized type, and that is the one a lookup finds while the arm stands.
    pub fn local_type(&self, name: &Free) -> Option<&Term> {
        self.local(name).map(|local| &local.type_)
    }

    /// Whether the binder `name` is a proof, where that has been asked: `None` where it has not, or where `name` is no binder in scope.
    pub fn classified(&self, name: &Free) -> Option<bool> {
        self.local(name)?.proof
    }

    /// Remember whether the binder `name` is a proof.
    pub fn classify(&mut self, name: &Free, proof: bool) {
        if let Some(&at) = self.innermost.get(name) {
            self.locals[at].proof = Some(proof);
        }
    }

    /// The current depth of both stacks, to be handed back to [`Scope::retract`].
    pub fn mark(&self) -> Mark {
        Mark {
            locals: self.locals.len(),
            refinements: self.refinements.len(),
        }
    }

    /// Close every binder opened — and drop every case equation assumed — since `mark`, answering whether what a term naming a local reduces to changed: an equation was dropped, one an arm inside had restated answers as recorded again, or a local an arm inside had re-typed stands at the type it had before.
    ///
    /// Where equations are withheld — for a settlement, or for a question asked under the view one was made in ([`Scope::hide_refinements_from`]) — only a scope opened inside the withholding closes here. That is structural rather than checked by discipline: `Certifier::scoped` is this method's only caller and a withholding is a bracket as well, so the two nest. A settlement runs reduction, which opens no binder scope; a question runs conversion, which opens them and assumes no equation in them.
    pub fn retract(&mut self, mark: Mark) -> bool {
        assert!(
            self.hidden
                .is_none_or(|withheld| withheld.limit <= mark.refinements),
            "a scope closed past the equations a settlement or a question was withholding"
        );

        // Innermost first, so a name opened twice inside the bracket ends at the binder that stood before it.
        let mut retyped = false;
        for local in self.locals.drain(mark.locals..).rev() {
            self.bound -= usize::from(local.value.is_some());
            match local.shadowed {
                Some(at) => {
                    // The shadow of a binder that stands for nothing is an arm's re-typing of it ([`Scope::assume`]), and a `let` bound again is read by value, so its type is read by no reduct.
                    retyped |= local.value.is_none();
                    self.innermost.insert(local.name, at)
                }
                None => self.innermost.remove(&local.name),
            };
        }
        let dropped = self.refinements.len() > mark.refinements;
        self.refinements.truncate(mark.refinements);

        // An equation restated at or past the mark was restated by an arm this bracket held, which is gone with its restatement; and a spelling settled again past the mark was settled with an equation this bracket dropped in view, which another arm's at the same position would be read under.
        let mut revived = false;
        for entry in &mut self.refinements {
            if entry.restated_at.is_some_and(|depth| depth >= mark.locals) {
                entry.restated_at = None;
                revived = true;
            }
            if dropped {
                entry.again.retain(|height, _| *height <= mark.refinements);
            }
        }

        dropped || revived || retyped
    }

    /// How many equations are in force: all of them, unless a settlement or a question is withholding the inner ones.
    fn in_force(&self) -> usize {
        self.hidden
            .map_or(self.refinements.len(), |withheld| withheld.limit)
    }

    /// The one equation withheld alone before the limit, where one is ([`Scope::mask_refinement`]).
    fn masked(&self) -> Option<usize> {
        self.hidden.and_then(|withheld| withheld.masked)
    }

    /// The equations in force that answer, each under its written spelling and its resolved one where it has one, beside the value it stands for: what a reader of a term naming a key's locals may meet ([`curios_analysis::Driver::refined`]).
    pub fn refined(&self) -> Vec<(Term, Term)> {
        self.in_force_innermost_first()
            .flat_map(|(_, entry)| {
                [Some(&entry.key), entry.resolved.as_ref()]
                    .into_iter()
                    .flatten()
                    .map(|key| (key.clone(), entry.value.clone()))
            })
            .collect()
    }

    /// The equations in force that answer, each beside its position, innermost first — the order every probe reads them in. One an arm has restated answers through its restatement, and one withheld alone answers nothing.
    fn in_force_innermost_first(&self) -> impl Iterator<Item = (usize, &Refinement)> {
        let masked = self.masked();
        self.refinements[..self.in_force()]
            .iter()
            .enumerate()
            .rev()
            .filter(move |(index, entry)| entry.restated_at.is_none() && Some(*index) != masked)
    }
}
