//! *Inversion*. A case of an inductive match is specialized by the most-general solution of its index equations, `actual indices ~ case targets`, the Goguen–McBride–McKinna step: the solution pins a case's binders to the values they are forced to take (`m + 1 ~ n + 1` pins `m := n`), refines the outer variables the scrutinee's indices name (`n + 1 ~ 1` refines `n := 0`), or proves the case unreachable outright (`0` against `n + 1`) — which is what lets the arm be omitted, checker-verified, with no `impossible` keyword.
//!
//! # One problem, one rule set
//!
//! The equations are solved as one problem, in order, over one set of flexible variables, each equation keeping its two sides, the scrutinee's and the case's, through everything done to it:
//!
//! - **Deletion.** An equation whose sides the walk meets as one value is removed: one variable on both sides, a pair the peel answers `Equal`, and two inhabitants of a proposition, at which inversion decides nothing.
//! - **Solution.** A flexible variable on one side is solved to the other where it is not among the locals that side reaches through what is in force ([`Unifier::reaches`]) and the value fits it ([`fits_its_variable`]). Where both sides are flexible variables the scrutinee's is refined to the case's, the orientation `curios_core::goal_at` reads a variable actual index in, and the case's is solved instead where that does not fit. Every equation after a solution, and every solution before it, is read through it.
//! - **Injectivity.** Two applications of one constructor of a relevant family, two tuples of one arity, a pair the peel reduces to another, and two words that each start with a known element ([`leading_element`]) become an equation per component.
//! - **Conflict.** Two constructors of a relevant family under different tags, and a pair the peel answers `Impossible`, make the case unreachable.
//! - **Anything else is undecided.** It stands aside without taking its siblings with it and is read again after a later solution; what is still undecided at the end specializes nothing, and keeps the arm mandatory.
//!
//! Nothing here asks conversion: a pair conversion alone holds equal is undecided, which neither specializes nor excuses anything, and two forms the walk tells apart are never convertible.
//!
//! # Shared, not duplicated
//!
//! Both checkers run *this* unifier. It is a total function of finished terms, and it runs post-zonk on both sides, each handing the indices as the certifier reads them, so a second implementation would be a second run of the same function on the same input rather than a second opinion. They share its composition too: [`solve_indices`] is the one substitution an arm is specialized by, so what the arm learns about its indices is computed by one function whichever checker asks. What each side supplies for itself is reduction, its refinements in force and a judgment of sorts, through [`Judge`] — see that module for the line, and for the concession borrowing a judgment represents.

#[cfg(test)]
mod tests;

use {
    crate::{Env, Judge, locals_beneath},
    curios_algebra::Deduction,
    curios_common::recurse,
    curios_core::{
        Free, Names, Quality, Sort, Subterm, Telescope, Term, leading_element, peel_intrinsic,
    },
    std::{
        collections::{BTreeSet, HashMap},
        mem,
    },
};

/// The payload binders an index target set *determines* — the singleton rung's side condition, shared by both checkers exactly as the unifier below is: a total function of the targets alone, so a second copy would be a second run rather than a second opinion.
///
/// A binder is determined when matching a value against the target recovers it, which holds when the target *is* the binder and in no other case.
///
/// It does **not** hold for a binder under anything else. `blur(a)` is an arbitrary function of `a`: knowing its value recovers nothing, since `blur` need not be injective and in the case that motivated this rule is the constant zero. Reading occurrence as determination — asking only whether `a` appears anywhere in the target — is the mistake, and it is the difference between a singleton and a proposition with a payload a program can read.
///
/// A constructor application reads like it should qualify, since constructors are injective, and this walk deliberately does not descend into one. At a `Prop`-sorted family that would be unsound, since irrelevance denies precisely the injectivity it assumes — `mk(a : Nat) : (Tag/t(a))` would report `a` as recovered although `Tag/t(0)` and `Tag/t(7)` are the same value, which is a payload a program can read out of a proposition and from there a closed inhabitant of `False`. And an elaborated target is stored as an application of the constructor's *function wrapper*, never as a saturated variant, so a descent into variants would reach nothing. Descending needs the sort condition *and* targets normalized to variant form; without both, not descending is the stricter guard.
///
/// Total by construction: a target this cannot decompose contributes nothing, so a shape nobody anticipated yields *fewer* determined binders and a stricter guard, never a looser one.
///
/// Each binder is answered with the first index position that pins it, because that is where a value of the family carries it. Both large-elimination guards ask only *whether* a binder is pinned; erasure asks *where*, since a `Prop` family's constructor is deleted and a pinned payload survives only as the scrutinee's index at that position.
pub fn pinned_by_targets(targets: &[Term]) -> Pinned {
    let mut pinned: Vec<(Free, usize)> = Vec::new();
    for (position, target) in targets.iter().enumerate() {
        // Anything but a bare binder — an application, a constructor's included, as well as a projection, an intrinsic, or a stuck match — determines nothing.
        if let Subterm::Var(var) = &**target
            && let Some(binder) = var.as_free()
            && !pinned.iter().any(|(pinned, _)| pinned == binder)
        {
            pinned.push((*binder, position));
        }
    }
    Pinned(pinned)
}

/// The payload binders [`pinned_by_targets`] found determined, each at the first index position whose target it is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pinned(Vec<(Free, usize)>);

impl Pinned {
    /// Whether the targets determine `binder`.
    pub fn contains(&self, binder: &Free) -> bool {
        self.position(binder).is_some()
    }

    /// The index position a value of the family carries `binder` at, when the targets determine it.
    pub fn position(&self, binder: &Free) -> Option<usize> {
        self.0
            .iter()
            .find(|(pinned, _)| pinned == binder)
            .map(|(_, position)| *position)
    }
}

/// Whether the targets determine every one of `binders`: the one shape of a proposition's constructor a match may take apart into data, since a value of the family then holds nothing its indices do not state. A payload that is itself a proof is no exception: the arm would be handed the proof the match was given, and a match that hands its proof on has read it.
pub fn wholly_pinned(binders: &[Free], targets: &[Term]) -> bool {
    let pinned = pinned_by_targets(targets);

    binders.iter().all(|binder| pinned.contains(binder))
}

/// What a scrutinee's indices `actuals` state of a wholly pinned constructor: its payload, each of `binders` read off the first index position that pins it, and the indices of the instance it builds at that payload. A match on such a proposition puts that instance beside the scrutinee's, by its checker's conversion: where the two are one the proof can be no other value, and the match is its arm at that payload with the proof unreduced. `None` where a target pins some binder at no position.
pub fn pinned_instance(
    binders: &[Free],
    targets: &[Term],
    actuals: &[Term],
) -> Option<(Vec<Term>, Vec<Term>)> {
    let pinned = pinned_by_targets(targets);
    let read = binders
        .iter()
        .map(|binder| Some((*binder, actuals.get(pinned.position(binder)?)?.clone())))
        .collect::<Option<Vec<_>>>()?;
    let built = targets
        .iter()
        .map(|target| target.substitute(&read))
        .collect();

    Some((read.into_iter().map(|(_, value)| value).collect(), built))
}

/// The outcome of inversion: the solution of a case's index equations, one idempotent substitution, or a definite clash that makes the case unreachable.
pub enum Invert {
    Solved(Vec<(Free, Term)>),
    Impossible,
}

/// Open a constructor's instantiated telescope with `vars` and read the index targets it terminates in.
pub fn case_target_indices(telescope: Telescope<Vec<Term>>, vars: &[Term]) -> Vec<Term> {
    match telescope.open_params(vars) {
        Telescope::Done(targets) => *targets,
        Telescope::Cons(..) => unreachable!("telescope arity matches the binder count"),
    }
}

/// The unifier: `actuals ~ targets`, one equation per index position, solved as one problem over the flexible variables `flex` by the rules the module states. A solution of a case's binders alone, which a constructor a goal admits (`suggest`) and a value's case (`another_case`) ask for, takes the binders as `flex`; an arm is specialized by [`solve_indices`], which adds the outer variables.
pub fn invert_indices<J: Judge>(
    judge: &mut J,
    actuals: &[Term],
    targets: &[Term],
    flex: &[Free],
) -> Result<Invert, J::Error> {
    curios_profile::profile!("invert_indices");
    Unifier::new(&*judge, flex).solve(judge, actuals, targets)
}

/// The most-general solution of `actuals ~ targets` for one case, as one idempotent substitution: what an arm is specialized by, computed here so both checkers compute it by one function.
///
/// The flexible variables are the case's `binders` and the outer variables, the locals the actual indices name (`locals_beneath`), never a top-level name, whose meaning no case can refine: a local carrying a definition contributes the locals its definition names instead, which is what the index mentions read by value. Each checker hands the indices as the certifier reads them, so the outer variables are one set of locals to both.
///
/// `Impossible` where the equations definitely clash, which makes the case unreachable.
pub fn solve_indices<J: Judge>(
    judge: &mut J,
    actuals: &[Term],
    targets: &[Term],
    binders: &[Free],
) -> Result<Invert, J::Error> {
    let mut flex = binders.to_vec();
    for local in locals_beneath(judge, actuals) {
        if !flex.contains(&local) {
            flex.push(local);
        }
    }

    invert_indices(judge, actuals, targets, &flex)
}

/// Whether `value` may stand for the variable `name` everywhere `name` stands. A variable is checked at its index's type by subsumption, so its own type may sit below that type, while the value it would be solved to is typed at the index's: where the variable's type is a sort, the value is a type whose own sort the variable's may not hold, and the solution is kept only where the judge holds it below. A solution both checkers apply alike, the elaborator through its store and the certifier by binding, is one whose value has the variable's own type, which depends on nothing about where the variable was bound; one left out keeps the variable as it stands, an answer both read alike. Any other variable's type is its index's by conversion, which the value inhabits.
fn fits_its_variable<J: Judge>(judge: &mut J, name: &Free, value: &Term) -> Result<bool, J::Error> {
    let Some(type_) = judge.assumption(name).cloned() else {
        return Ok(true);
    };
    let forced = judge.force(&type_)?;

    match &*forced {
        Subterm::Sort(_) => judge.sort_below(value, &forced),
        _ => Ok(true),
    }
}

/// One equation's outcome inside the walk.
enum Step {
    /// Decided without a clash: every part of it solved, deleted, or set aside where it stands.
    Ok,
    /// Undecided as it stands, and set aside to be read again after a later solution.
    Aside,
    /// A definite clash: the case is unreachable.
    Clash,
}

/// One problem's state: the variables it may solve, what the arms in force refine, the solutions found so far, the equations set aside, and the pairs of nodes it has unified, each of which answered `Ok`.
///
/// **Solutions are kept as found.** A solution is not substituted through the equations after it: the walk reads a solved variable as its value where it meets one, so a pair of nodes once unified stays unified as solutions grow, and an index that holds a node twice is walked once per node and not once per path. A pair is keyed by its nodes' identities and holds the two nodes, so no identity is reused while the problem runs: a peel and a forcing hand back nodes of their own. The solutions are made one idempotent substitution at the end.
struct Unifier<'f> {
    flex: &'f [Free],
    refined: Vec<(Term, Term)>,
    solutions: Vec<(Free, Term)>,
    aside: Vec<(Term, Term)>,
    unified: HashMap<(usize, usize), (Term, Term)>,
}

impl<'f> Unifier<'f> {
    fn new<J: Judge>(judge: &J, flex: &'f [Free]) -> Self {
        Self {
            flex,
            refined: judge.refined(),
            solutions: Vec::new(),
            aside: Vec::new(),
            unified: HashMap::new(),
        }
    }

    /// Every position in order, then what was set aside, read again after each pass that solved a variable until one solves none: each such pass solves a variable, so there are no more of them than there are flexible variables.
    fn solve<J: Judge>(
        mut self,
        judge: &mut J,
        actuals: &[Term],
        targets: &[Term],
    ) -> Result<Invert, J::Error> {
        for (actual, target) in actuals.iter().zip(targets) {
            if let Step::Clash = self.unify(judge, actual, target)? {
                return Ok(Invert::Impossible);
            }
        }

        let mut read_at = 0;
        while !self.aside.is_empty() && self.solutions.len() > read_at {
            read_at = self.solutions.len();
            for (actual, target) in mem::take(&mut self.aside) {
                if let Step::Clash = self.unify(judge, &actual, &target)? {
                    return Ok(Invert::Impossible);
                }
            }
        }

        Ok(Invert::Solved(self.idempotent()))
    }

    /// Depth is bought with stack, as every walk over data-shaped depth buys it: this walk, `unify_node` and `unify_all` descend a pair of index terms one level per constructor or tuple layer, forcing both sides at each, and nothing between the levels is a loop or a charge. Without the bracket the native stack goes first and the process dies, which is no refusal ([The soundness board](../../documentation/design/soundness/the-soundness-board.md)).
    fn unify<J: Judge>(
        &mut self,
        judge: &mut J,
        actual: &Term,
        target: &Term,
    ) -> Result<Step, J::Error> {
        recurse(|| self.unify_within(judge, actual, target))
    }

    fn unify_within<J: Judge>(
        &mut self,
        judge: &mut J,
        actual: &Term,
        target: &Term,
    ) -> Result<Step, J::Error> {
        let pair = (actual.identity(), target.identity());
        if self.unified.contains_key(&pair) {
            return Ok(Step::Ok);
        }
        let step = self.unify_node(judge, actual, target)?;
        match step {
            Step::Ok => {
                self.unified.insert(pair, (actual.clone(), target.clone()));
            }
            Step::Aside => self.aside.push((actual.clone(), target.clone())),
            Step::Clash => {}
        }

        Ok(step)
    }

    /// One equation, its scrutinee's side first: both sides forced, and a solved variable read as its value.
    fn unify_node<J: Judge>(
        &mut self,
        judge: &mut J,
        actual: &Term,
        target: &Term,
    ) -> Result<Step, J::Error> {
        let actual = self.read(judge, actual)?;
        let target = self.read(judge, target)?;

        // Solution, the scrutinee's side refined to the case's where both are variables, and the case's solved where that refinement does not fit or would reach its own variable.
        let solved = match (self.flexible(&actual), self.flexible(&target)) {
            (Some(this), Some(that)) if this == that => return Ok(Step::Ok),
            (Some(this), Some(that)) => {
                self.solve_one(judge, this, &target)? || self.solve_one(judge, that, &actual)?
            }
            (Some(this), None) => self.solve_one(judge, this, &target)?,
            (None, Some(that)) => self.solve_one(judge, that, &actual)?,
            (None, None) => return self.decompose(judge, &actual, &target),
        };

        Ok(match solved {
            true => Step::Ok,
            false => Step::Aside,
        })
    }

    /// Deletion, conflict and injectivity, between two sides neither of which is a flexible variable.
    fn decompose<J: Judge>(
        &mut self,
        judge: &mut J,
        actual: &Term,
        target: &Term,
    ) -> Result<Step, J::Error> {
        match (&**actual, &**target) {
            (Subterm::Metavar(_), _) | (_, Subterm::Metavar(_)) => Ok(Step::Aside),

            (Subterm::Intrinsic(_), Subterm::Intrinsic(_))
                if self.names_a_solution(actual, target) =>
            {
                // A peel compares its atoms as written, so a pair naming a solved variable is read through the solutions first.
                let actual = judge.force(&self.resolved(actual))?;
                let target = judge.force(&self.resolved(target))?;
                self.decompose(judge, &actual, &target)
            }

            (Subterm::Intrinsic(this), Subterm::Intrinsic(that)) => {
                match peel_intrinsic(this, that) {
                    Some(Deduction::Equal) => Ok(Step::Ok),
                    Some(Deduction::Impossible) => Ok(Step::Clash),
                    Some(Deduction::Equivalent((left, right))) => {
                        self.unify_all(judge, &[left], &[right])
                    }
                    // A free monoid cancels a generator: two words that each start with a known element are one value exactly where the two elements are one and so are the words after them. Two literals of one length are this taken element by element.
                    Some(Deduction::Undecided) | None => {
                        match (leading_element(this), leading_element(that)) {
                            (Some((head, tail)), Some((other_head, other_tail))) => {
                                self.unify_all(judge, &[head, tail], &[other_head, other_tail])
                            }
                            _ => Ok(Step::Aside),
                        }
                    }
                }
            }

            // The same rigid variable on both sides is one value.
            (Subterm::Var(a), Subterm::Var(t))
                if a.as_free().is_some_and(|a| t.as_free() == Some(a)) =>
            {
                Ok(Step::Ok)
            }

            (Subterm::Variant(a), Subterm::Variant(t)) => {
                // A different inductive is incomparable; but two *different constructors* of the same inductive definitely clash — even though they legitimately differ in payload arity, so the tag check must precede the arity check (which is then just defensive: equal tags share an arity).
                if a.name != t.name {
                    return Ok(Step::Aside);
                }
                // Injectivity and tag disjointness are claims about values a program can tell apart, and a proposition's inhabitants are not: proof irrelevance makes every one of them definitionally equal. Deciding this equation by either claim contradicts conversion — `Two/a()` against `Two/b()` reads as a clash while conversion calls them the same value, and `Tag/t(a)` against `Tag/t(7)` manufactures the *relevant* equation `a := 7` from a premise that `Tag/t(0)` satisfies equally. Both are inconsistent, and each would be a closed inhabitant of `False`: the first through a vacuous elimination, the second through the singleton rung of the large-elimination guard. So the equation is deleted — the two sides really are equal — and it yields nothing: no clash, no equations. That is strictly the refusing direction, since an arm a clash would excuse stays mandatory and a variable forced only here stays unsolved.
                //
                // Only a variant can reach this: an `Intrinsic` carrier is a relevant type, and a `Prop`-sorted tuple or structure has none but non-informative components, so decomposing one yields equations between proofs, which any inhabitant satisfies.
                // Matched on the nose rather than reduced: `check_induct_decl` requires a declared result sort to be a literal sort, which is the clause this test is licensed by. Without it a family declared at a redex unfolding to `Prop` reads as relevant here and as a proposition everywhere else, and the clash below excuses an arm that irrelevance says is reachable.
                // A family the registry cannot answer for is set aside rather than read as relevant: the tag test's license is the declared sort, so a lookup that answers nothing licenses nothing — falling through to the clash would decide the strong verdict out of blindness, the one direction the analyses on this seam never take.
                let Some(declaration) = judge.induct_decl(&a.name) else {
                    return Ok(Step::Aside);
                };
                if matches!(
                    **declaration.result_sort(),
                    Subterm::Sort(Sort {
                        quality: Quality::Prop,
                        level: _
                    })
                ) {
                    return Ok(Step::Ok);
                }
                if a.tag != t.tag {
                    return Ok(Step::Clash);
                }
                if a.payload.len() != t.payload.len() {
                    return Ok(Step::Aside);
                }
                self.unify_all(judge, &a.payload, &t.payload)
            }

            (Subterm::Tuple(a), Subterm::Tuple(t)) => {
                if a.fields.len() != t.fields.len() {
                    return Ok(Step::Aside);
                }
                self.unify_all(judge, &a.fields, &t.fields)
            }

            _ => Ok(Step::Aside),
        }
    }

    /// The equations of components, each its own: one that stands aside leaves its siblings to be decided, and only a clash ends them.
    fn unify_all<J: Judge>(
        &mut self,
        judge: &mut J,
        actuals: &[Term],
        targets: &[Term],
    ) -> Result<Step, J::Error> {
        for (actual, target) in actuals.iter().zip(targets) {
            if let Step::Clash = self.unify(judge, actual, target)? {
                return Ok(Step::Clash);
            }
        }
        Ok(Step::Ok)
    }

    /// `term` forced, and a solved variable it forces to read as its value, forced in turn: no solution reaches its own variable, so the reading ends.
    fn read<J: Judge>(&self, judge: &mut J, term: &Term) -> Result<Term, J::Error> {
        let mut term = judge.force(term)?;
        while let Some(value) = self.solution_of(&term) {
            term = judge.force(value)?;
        }

        Ok(term)
    }

    fn solution_of(&self, term: &Term) -> Option<&Term> {
        let Subterm::Var(var) = &**term else {
            return None;
        };
        let name = var.as_free()?;
        self.solutions
            .iter()
            .find(|(solved, _)| solved == name)
            .map(|(_, value)| value)
    }

    /// The flexible variable `term` is, where it is one: [`Unifier::read`] has already read a solved one as its value.
    fn flexible(&self, term: &Term) -> Option<Free> {
        let Subterm::Var(var) = &**term else {
            return None;
        };
        let name = var.as_free()?;
        self.flex.contains(name).then_some(*name)
    }

    /// Solve `name` to `value`, where `value` is no metavariable, does not reach `name`, and fits it.
    fn solve_one<J: Judge>(
        &mut self,
        judge: &mut J,
        name: Free,
        value: &Term,
    ) -> Result<bool, J::Error> {
        if matches!(&**value, Subterm::Metavar(_)) || self.reaches(&*judge, value, &name) {
            return Ok(false);
        }
        if !fits_its_variable(judge, &name, value)? {
            return Ok(false);
        }
        self.solutions.push((name, value.clone()));

        Ok(true)
    }

    /// The occurs check, read through what is in force: whether `name` is among the locals `value` reaches — those it names, closed under the solutions found, the definitions in scope ([`Env::unfold`]), and every refinement in force whose refined term names one, its value's locals joining ([`Env::refined`]).
    ///
    /// Reduction brings a local into a term only through a definition, a solution or a refinement, so the closure holds every local a reader of the solution can meet: a solution that reaches its own variable ties a cycle, as `t := node(p.0)` does where an arm refined `p.0` to `node(t)`, which the certifier would read back to `t` by name. The closure reduces nothing, and it may refuse a value naming a local some refined term names without the value naming that term, which leaves an equation undecided and nothing worse.
    fn reaches<E: Env>(&self, env: &E, value: &Term, name: &Free) -> bool {
        let mut pending = value.mentions().locals().copied().collect::<Vec<_>>();
        let mut seen = BTreeSet::new();

        while let Some(local) = pending.pop() {
            if local == *name {
                return true;
            }
            if !seen.insert(local) {
                continue;
            }
            if let Some((_, solution)) = self.solutions.iter().find(|(solved, _)| *solved == local)
            {
                pending.extend(solution.mentions().locals().copied());
            }
            if let Some(definition) = env.unfold(&local) {
                pending.extend(definition.mentions().locals().copied());
            }
            for (refined, standing) in &self.refined {
                if refined.mentions_free(&local) {
                    pending.extend(standing.mentions().locals().copied());
                }
            }
        }

        false
    }

    /// Whether either side names a variable solved so far.
    fn names_a_solution(&self, actual: &Term, target: &Term) -> bool {
        let solved = Names::new(self.solutions.iter().map(|(name, _)| *name));
        actual.mentions_any(&solved) || target.mentions_any(&solved)
    }

    /// `term` with every solution substituted until it names none: as many rounds as the longest chain of solutions, which no solution reaching its own variable bounds by their count.
    fn resolved(&self, term: &Term) -> Term {
        let solved = Names::new(self.solutions.iter().map(|(name, _)| *name));
        let mut term = term.clone();
        for _ in 0..self.solutions.len() {
            if !term.mentions_any(&solved) {
                break;
            }
            term = term.substitute(&self.solutions);
        }

        term
    }

    /// The solutions as one idempotent substitution: each round substitutes every solution into every value, so a chain of them is resolved in as many rounds as it is long, and no chain is longer than the solutions are many, none reaching its own variable.
    fn idempotent(self) -> Vec<(Free, Term)> {
        let solved = Names::new(self.solutions.iter().map(|(name, _)| *name));
        let mut solutions = self.solutions;
        for _ in 0..solutions.len() {
            if !solutions
                .iter()
                .any(|(_, value)| value.mentions_any(&solved))
            {
                break;
            }
            let round = solutions.clone();
            for (_, value) in &mut solutions {
                *value = value.substitute(&round);
            }
        }

        solutions
    }
}
