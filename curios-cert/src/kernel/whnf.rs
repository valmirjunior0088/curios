//! Weak-head normalization: the kernel's reduction strategy.
//!
//! This is the kernel's answer to "what does this term compute to, as far as its head". It is deliberately the *whole* strategy — beta, delta, iota, projection, universe instantiation, intrinsic folding, and `rec` unfolding — and deliberately nothing else. In particular there is no metavariable resolution, part of what makes the elaborator's reducer fast and forgiving; here it would be one more way for the answer to come from somewhere other than the term. The two stores the loop does consult stay on the term's side of that line: the evaluation memo replays the kernel's own pure function of the definition store rather than remembering anyone's claim — [`memos`](super::memos) owns that argument — and a case-equation refinement is the arm's own definitional hypothesis, assumed by the elimination check that justified it and scoped to its arm.
//!
//! It resembles the elaborator's reducer closely, and that resemblance is the point of writing it out rather than sharing it. Reduction decides which programs convert, and conversion decides which programs typecheck; a bug shared by both checkers is a bug neither can catch. The crate boundary enforces this — `curios-elab`'s reducer is not visible from here — so the duplication cannot quietly collapse back into a call.
//!
//! Two things *are* shared, and both are representation rather than judgment: the binder discipline that `open`/`release` implement, and [`reduce_intrinsic`], which decides what `2 + 2` folds to. Neither can admit an ill-typed program on its own.

#[cfg(test)]
mod budget_tests;
#[cfg(test)]
mod closed_machine_tests;
#[cfg(test)]
mod equations_tests;
#[cfg(test)]
mod memo_tests;
#[cfg(test)]
mod rules_tests;
#[cfg(test)]
mod test_support;

use {
    super::Kernel,
    curios_analysis::RESOLVED_SPELLING_LAYERS,
    curios_core::{
        Apply, Bound, Carrier, Cases, ClosedHost, Cost, Demand, Field, Free, FreeMonoid, Func,
        Instance, InstanceHead, Intrinsic, Layer, Let, Match, MatchResult, Nat, Probe, Proj, Rec,
        RecGroup, ReduceError, Reducer, Struct, Subterm, Term, Tuple, Var, Variant, Visit,
        accelerable, instantiate_universe_levels_scoped, probe_spellings, reduce_closed,
        reduce_intrinsic,
    },
    curios_utilities::recurse,
};

#[cfg(feature = "profile")]
use curios_core::{atoms_within, classable};

/// The kernel's side of the closed-machine seam: the same delta `step_var` and `step_instance` perform, handed to the shared machine so a closed term evaluates at machine depth under this strategy's own charges.
impl ClosedHost for Kernel {
    fn closed_body(&self, name: &Free) -> Option<&Term> {
        self.value(name)
    }

    fn closed_body_at(&self, name: &Free) -> Option<&Term> {
        self.value_at(name)
    }
}

/// Whether the closed machine may take `term` under this judgment: the kernel admits it at all (the `machine` field is false only in the differential fixture's strategy arm), the representation-side gate ([`accelerable`]) holds, and no case equation is in scope — because inside an arm a closed scrutinee *is* the arm's assumed value.
fn machine_admissible(kernel: &Kernel, term: &Term) -> bool {
    kernel.machine && accelerable(term) && !kernel.has_refinements()
}

/// The kernel's reduction strategy: everything unfolds, and what a term unfolds to is remembered — for the declaration if it is local-free, for as long as the equations in force stand if it is not — see the `memos` and `spend` modules for what that does and does not concede, and for why a hit is free.
impl Reducer for Kernel {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        curios_profile::profile!("reduce");
        whnf(self, term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        // By value before the table is asked, as [`whnf`] is: its keys are the spellings reduction sees.
        if !self.reads_by_value() {
            let term = self.by_value(&term);
            return self.reading(|kernel| kernel.reduce_forced(term));
        }
        if let Some(replayed) = self.whnf_hit(&term, true) {
            return Ok(replayed);
        }
        // Past the memo, so the span times reductions the table did not answer: a hit costs a lookup, and a span per hit would cost the profile more rows than any other site and bill each its own row-writing.
        curios_profile::profile!("reduce_forced");

        let before = self.consumption();
        let reduced = whnf(self, term.clone())?;
        let reduct = force(self, reduced)?;
        let replay = self.replay_since(reduct.clone(), before);
        self.whnf_store(term, true, replay);

        Ok(reduct)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        Kernel::spend(self, cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        self.fresh(hint)
    }
}

/// One step's outcome: another term to reduce, or a weak-head normal form.
enum Step {
    Continue(Term),
    Stop(Term),
}

/// Reduce `term` until its head constructor is stable.
///
/// Guarded by [`recurse`] for the same reason the crate duplicates the strategy at all: the kernel has to accept every term the elaborator produced, on the same thread stack, so a depth it aborts at that the elaborator does not is a term that typechecks and then fails to certify. An intrinsic's operands re-enter through [`reduce_intrinsic`], which is shared, so a deep `add` chain puts one native frame per link on this side exactly as it does on the other.
///
/// **Reduction reads by value.** Typing walks a `let`'s tail over its binder, so the term it hands over may name one. Where reduction is entered from outside, the name gives way to what it stands for, once, and everything beneath — the tables' keys, the case equations' probes, the closed machine's gate — meets the spelling a substituted `let` leaves, which is the spelling each was argued over. A remembered reduct therefore never rests on a `let`'s binding, which an arm may replace and no key of these tables carries.
pub(crate) fn whnf(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    if !kernel.reads_by_value() {
        let term = kernel.by_value(&term);
        return kernel.reading(|kernel| whnf(kernel, term));
    }

    // The level itself, charged when it is deeper than any this judgment has reached — see `Spend::enter_level`. What it buys is that depth is bounded by the budget rather than by how much stack the host handed the process, which would otherwise be the one resource this walk consumes without being counted.
    kernel.enter_level()?;
    let reduct = recurse(|| whnf_within(kernel, term));
    kernel.leave_level();

    reduct
}

fn whnf_within(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    // **The memo is consulted here, at every level, and not only where something outside reduction asks for one.**
    //
    // Probing only at the two `Reducer` methods would leave every internal call below — a scrutinee, an application's head, each turn of `force`'s loop, `expose_rec_tail`, `unfold_spelling` — re-deriving what the table already holds; the elaborator's reducer probes at its own entry and recurses through it, so probing here keeps the two strategies alike in reach as well as in rule. Reaching every level is safe because the table a term belongs to is decided inside [`Memos`](super::Memos), on the lookup as well as the store: a local-free term by the tables that live for the declaration, a local-bearing one by the tables that live only as long as the equations in force.
    //
    // **The memo is asked before the case equations below, and that order rests on the recording rule.** A term the declaration-lived tables answer is local-free, and no equation is recorded under a local-free spelling (`curios_analysis::records_case_equation`), so no entry there can stand in for an answer an equation in force would give; a local-bearing entry is cleared wherever the equations in force change. `a_remembered_closed_term_answers_inside_an_arm_as_an_uncached_kernel_does` holds the first half, and fails once a local-free equation is recorded with this order kept; `a_remembered_reduct_does_not_outlive_the_equations_it_was_taken_under` holds the second.
    if let Some(replayed) = kernel.whnf_hit(&term, false) {
        return Ok(replayed);
    }

    // A closed term takes the machine: same rules, same counter, machine depth instead of one native frame per element. Stored under the same memo entry a strategy-derived reduct would be.
    if machine_admissible(kernel, &term) {
        let entry = term.clone();
        let before = kernel.consumption();
        let value = reduce_closed(kernel, term, Demand::Whnf)?;
        let replay = kernel.replay_since(value.clone(), before);
        kernel.whnf_store(entry, false, replay);

        return Ok(value);
    }

    let entry = term.clone();
    let before = kernel.consumption();
    let mut term = term;

    loop {
        kernel.spend(Cost::STEP)?;

        // An arm's case equation is consulted *before* the term is taken apart, not only on the value it reduced to. Both points are sound for the same reason and by the same test: [`Scope::refinement_of`] matches by `Term`'s structural equality, universe instances included, so a hit means this term *is* the registered scrutinee and the arm's hypothesis applies to it directly — and the value it substitutes is a constructor, a normal form, so nothing cycles.
        //
        // Asking only afterwards would make the answer depend on affording the reduction: `Lt(i, len(b))` under a guard that refined exactly that comparison would fold the intrinsic first — evaluating `b` — and consult the equation about a value it had already spent the budget to compute. The elaborator's reducer asks first as well, so a program the elaborator accepts is not refused here for a reason that reads as a disagreement about the rule and is not one.
        //
        // The written spelling alone here. An equation is *recorded* under it, so this point answers the scrutinee's own occurrences — the common case, and the one whose whole cost is this comparison. The reduced spelling is what [`refined_reduct`] escalates to, at the other point, where the terms that need it arrive.
        if let Some(refined) = kernel.refinement_of(&term) {
            term = refined;
            continue;
        }
        if let Some(refined) = refined_spelling(kernel, &term) {
            term = refined;
            continue;
        }

        let step = match Term::unwrap_or_clone(term) {
            Subterm::Intrinsic(intrinsic) => {
                Step::Stop(reduce_intrinsic(kernel, &intrinsic)?.into())
            }
            Subterm::Var(var) => step_var(kernel, var)?,
            Subterm::Apply(apply) => step_apply(kernel, apply)?,
            Subterm::Proj(proj) => step_proj(kernel, proj)?,
            Subterm::Func(func) => step_func(kernel, func)?,
            Subterm::Let(let_) => Step::Continue(step_let(kernel, let_)?),
            Subterm::Instance(instance) => step_instance(kernel, instance)?,
            // The scrutinee is reduced by a nested call, so a tower of matches over a deep closed spine — the scan-state chain a string literal lowers to — costs one native frame per link. That is data-shaped depth, which is what [`recurse`] at the entry point is for.
            Subterm::Match(Match {
                head,
                result,
                cases,
            }) => {
                let value = whnf(kernel, head)?;

                step_match(force(kernel, value)?, result, cases)
            }
            // `InductType`/`Variant`, `StructType`/`Struct`, `Tuple`, `FuncType`, `Type`/`Prop`, `Rec`, and a `Metavar` no kernel input should contain are all weak-head normal already: their sub-terms are not reduced in this position.
            other => Step::Stop(other.into()),
        };

        match step {
            Step::Continue(next) => term = next,
            Step::Stop(value) => {
                // A stuck form standing under an arm's case equation *is* that case's value, definitionally; continue from it. This is the *second* of the two probe points, and it does not merge with the one above: that one asks about a term before it is taken apart, this one about a form reduction produced, and routing this one back to the top would re-decompose a normal form forever.
                match refined_reduct(kernel, &value)? {
                    Some(refined) => term = refined,
                    None => {
                        sample_classable(kernel, &value)?;
                        // Remembered under the term this level was *entered* with, not the one the loop finished on — the same key the probe above will present.
                        let replay = kernel.replay_since(value.clone(), before);
                        kernel.whnf_store(entry, false, replay);

                        return Ok(value);
                    }
                }
            }
        }
    }
}

/// A stuck comparison under a guard recorded on another spelling of it, asked under each of [`probe_spellings`] in turn. The false arm of `n < m` records `n < m` alone, and `m <= n` is that fact read the other way, so the dual answers with the literal negated. A bound reaches the probe as `i + 1 <= len(l)` while the guard that decided it was written `i < len(l)`, one fact on `Nat` and on `Int`, so the successor spelling answers with the literal carried across — not negated, these being the same proposition rather than opposite ones. And the two compose: the false arm of `x <= 4` is the fact `5 <= x`, which the dual of the successor spelling answers. Lookup only — nothing is recorded under any of them, so a guard still refines exactly what it was written as.
fn refined_spelling(kernel: &Kernel, term: &Term) -> Option<Term> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };
    probe_spellings(intrinsic).find_map(|(spelling, negated)| {
        let literal = kernel
            .refinement_of(&Term::intrinsic(spelling))?
            .as_bool()?;
        Some(Term::intrinsic(Intrinsic::Bool(literal != negated)))
    })
}

/// The refinement probe at a stuck reduct: the written spelling first, then the reduced one, settling reduced spellings until one answers or none is left to settle.
///
/// **Why the escalation is here and not at the other probe point.** An equation is recorded under the scrutinee as written, and reduction reaches the scrutinee's *reduct* — `Le(s + l, len b)` instantiated at a call arrives as `Le(0 + n, len b)` and folds to a spelling the written key does not carry. The point before decomposition sees terms on the way in, where the written spelling is what matches; this point sees the forms reduction produced, which is exactly where a spelling that exists only as a reduct can appear.
///
/// **The local-bearing gate is the guard, not an optimization.** `Scope::refine` records only local-bearing written spellings, but a *reduced* spelling is whatever reduction returned and may well be local-free. Refusing to probe on a local-free term is what keeps every refined term local-bearing, which is the half of the evaluation memos' first invariant this component owns: a local-free term's entry outlives the arm, so a local-free term whose reduct came from a case equation is precisely the entry that could outlive the arm that justified it — where a local-bearing term's entry is cleared with the arm's equations and may. It is also what makes the deferral pay — an arm body of literals reduces to local-free forms and settles nothing at all.
///
/// **The reduced spellings meet in operand-canonical form.** A settled reduct is the key's weak-head form with each operand weak-head reduced, and the value probed against it is brought to the same form here, so that `x && true` under a `match x && g(7)` meets the key whose `g(7)` the settlement folded, and `x && h(7)` meets it too. A weak-head form alone would not do: a `&&` behind a stuck left leaves its right as written, and the two spellings would then differ by exactly the fold the escalation exists to see through. It is computed only on a miss under a live equation and only for a tagged intrinsic — a `head_key` — which is the same path the elaborator's `refined_after_fold` canonicalizes on, and what keeps the two checkers reaching the same occurrences.
fn refined_reduct(kernel: &mut Kernel, value: &Term) -> Result<Option<Term>, ReduceError> {
    if let Some(refined) = kernel.refinement_of(value) {
        return Ok(Some(refined));
    }
    if let Some(refined) = refined_spelling(kernel, value) {
        return Ok(Some(refined));
    }

    if !value.has_local_free() || !kernel.has_refinements() {
        return Ok(None);
    }

    let canonical = canonical_operands(kernel, value)?;
    // The other spellings under the reduced spelling too, for the reason the written pass asks them: a guard whose operands the probe presents folded — the dispatch's resolved spelling carries them as written, and a guard `i < List/len(l)` records the call its author wrote while the bound arrives with that call folded to its intrinsic — answers only once its reduct is settled, so every spelling is asked of the settled reducts exactly as the written and resolved spellings were asked of the record. Nothing is recorded under any of them, and the elaborator looks in the same places.
    let spellings = match &*canonical {
        Subterm::Intrinsic(intrinsic) => probe_spellings(intrinsic)
            .map(|(spelling, negated)| (Term::intrinsic(spelling), negated))
            .collect(),
        _ => Vec::new(),
    };

    loop {
        if let Some(refined) = kernel.refinement_of_reduct(&canonical) {
            return Ok(Some(refined));
        }
        // A dual's literal is the guard's negated, and the successor spelling's is the guard's own: one proposition, where the dual's are opposite ones.
        if let Some(answer) = spellings.iter().find_map(|(spelling, negated)| {
            let literal = kernel.refinement_of_reduct(spelling)?.as_bool()?;
            Some(Term::intrinsic(Intrinsic::Bool(literal != *negated)))
        }) {
            return Ok(Some(answer));
        }

        let unasked = kernel.unasked_refinement(&canonical).or_else(|| {
            spellings
                .iter()
                .find_map(|(spelling, _)| kernel.unasked_refinement(spelling))
        });
        let Some((index, key)) = unasked else {
            // What a lookup that asked conversion would have been put to: the equations this stuck form could be a reduct of, each settled and none answering.
            #[cfg(feature = "profile")]
            if let asked @ 1.. = kernel.reachable_refinements(&canonical) {
                curios_profile::sample!("whnf::missed_lookup", asked);
            }
            return Ok(None);
        };

        kernel.settle_refinement(index, key)?;
    }
}

/// Report, under `profile`, what a stuck operation would put to the kernel's conversion were reduction to class its atoms: how many atoms the readers read in it, where some two of them may be one. Counted before the rule that asks, so what the rule costs over `/std` is known first.
#[cfg(feature = "profile")]
fn sample_classable(kernel: &mut Kernel, value: &Term) -> Result<(), ReduceError> {
    let Subterm::Intrinsic(operation) = &**value else {
        return Ok(());
    };
    let atoms = atoms_within(kernel, operation)?;
    if classable(&atoms) {
        curios_profile::sample!("whnf::classable_fold", atoms.len());
    }
    Ok(())
}

#[cfg(not(feature = "profile"))]
fn sample_classable(_kernel: &mut Kernel, _value: &Term) -> Result<(), ReduceError> {
    Ok(())
}

/// `term` with each operand in weak-head normal form, where it is a tagged intrinsic — the form a refinement's reduced spelling and the value probed against it are both held in. Anything else is its own canonical form.
///
/// Each operand is a [`Probe`], as the elaborator's twin reads it: an operand with no value at the type level is kept as written, so a probe that meets one misses rather than refusing the judgment it serves.
pub(crate) fn canonical_operands(kernel: &mut Kernel, term: &Term) -> Result<Term, ReduceError> {
    if term.head_key().is_none() {
        return Ok(term.clone());
    }
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return Ok(term.clone());
    };

    let mut masking = Visit::masking(|_, _: &Var| None, Term::type_ground());
    intrinsic.traverse(&mut masking);

    let mut operands = Vec::new();
    for operand in masking.take_masked_children() {
        operands.push(whnf(kernel, operand.clone()).probed()?.unwrap_or(operand));
    }

    let mut index = 0;
    let rebuilt = intrinsic.traverse(&mut Visit::rewriting(
        |_, _: &Var| None,
        Box::new(move |_, operand: &Term| {
            let value = operands.get(index).cloned();
            index += 1;

            Some(value.unwrap_or_else(|| operand.clone()))
        }),
    ));

    Ok(Subterm::Intrinsic(rebuilt).into())
}

/// Delta: unfold a definition, or leave the variable as the normal form it is.
///
/// The body is reduced by a nested `whnf`, which remembers it under the body's term for the rest of the declaration, so the next occurrence of the name in this declaration continues from the reduct instead of re-deriving it — and nothing is remembered past the declaration. The nested `whnf` recurses one native frame per link of a definition-reference chain, which is authored depth, not data depth.
fn step_var(kernel: &mut Kernel, var: Var) -> Result<Step, ReduceError> {
    debug_assert!(
        !kernel.binds(var.unwrap()),
        "a `let`-bound local reached reduction by name"
    );
    let Some(body) = kernel.value(var.unwrap()).cloned() else {
        return Ok(Step::Stop(Term::var(var)));
    };

    Ok(Step::Continue(whnf(kernel, body)?))
}

/// Beta: open a function's telescope over the arguments applied to it.
///
/// A `rec` head is exposed but not unfolded — the folded spelling stays the normal form of a recursive call, and [`force`] is what demands otherwise.
fn step_apply(kernel: &mut Kernel, apply: Apply) -> Result<Step, ReduceError> {
    let Apply { head, arguments } = apply;

    let head = whnf(kernel, head)?;
    let head = expose_rec_tail(kernel, head)?;

    Ok(match Term::unwrap_or_clone(head) {
        // Saturation is the precondition of the β step, not an assumption about it: `Telescope::open` asserts on a count mismatch, so an under- or over-applied lambda would abort the walk rather than be refused. An application that does not saturate its lambda is stuck instead, which is the conservative direction — it leaves the term for the typing rules to reject with a diagnostic, and reduction that declines to fire can never admit anything.
        Subterm::Func(Func { telescope, .. }) if telescope.len() == arguments.len() => {
            // The argument ref vector, and what `Telescope::open` costs on top of it: it clones the whole boxed chain and then substitutes once per binder, so an `n`-ary beta step is `n` boxes and `n` passes rather than one.
            kernel.spend(
                Cost::collection(arguments.len() as u64)
                    .saturating_add(Cost::term(1).saturating_mul(arguments.len() as u64)),
            )?;

            let refs = arguments
                .iter()
                .map(|argument| &argument.term)
                .collect::<Vec<_>>();
            Step::Continue(telescope.open(&refs))
        }
        head => Step::Stop(Term::from(Subterm::Apply(Apply {
            head: head.into(),
            arguments,
        }))),
    })
}

/// The spelling a dispatched scrutinee resolves to: its application spine opened a layer at a time through heads that reduce to functions — the intrinsic a concept method elaborates to, `(?w).1(a, hi)` reaching `NatLe(a, hi)` — or `None` where nothing opened, where what it reached carries no head a probe can present, or where [`RESOLVED_SPELLING_LAYERS`] did not settle it.
///
/// **Bounded rather than reduced, which is what lets an arm record it on entry.** Only heads are reduced and no argument is forced, so a guard over an expensive subject costs no evaluation of that subject; a β step fires only at the arity it saturates, as [`step_apply`]'s does. That is the line the elaborator's `spine_whnf` draws, and this is the spelling it registers beside the written one. Both checkers holding it is what keeps them answering the same occurrences at the same point: met only through its lazily settled reduct — after the decision procedure has already folded the probe, and as that fold's result — a dispatched guard's intrinsic shape would be answered from the procedure in an arm whose guard the procedure decides against, where the elaborator answers the dual spelling from the arm's equation.
pub(crate) fn resolved_spelling(
    kernel: &mut Kernel,
    scrutinee: &Term,
) -> Result<Option<Term>, ReduceError> {
    let mut current = scrutinee.clone();

    // Bounded: each step consumes one application layer of an elaborated dispatch, and a spine that has not settled in the layers both checkers open is not a dispatch.
    for _ in 0..RESOLVED_SPELLING_LAYERS {
        let Subterm::Apply(Apply { head, arguments }) = &*current else {
            return Ok((current != *scrutinee && current.head_key().is_some()).then_some(current));
        };
        let head = whnf(kernel, head.clone())?;
        let Subterm::Func(Func { telescope, .. }) = &*head else {
            return Ok((current != *scrutinee && current.head_key().is_some()).then_some(current));
        };
        if telescope.len() != arguments.len() {
            return Ok((current != *scrutinee && current.head_key().is_some()).then_some(current));
        }

        kernel.spend(
            Cost::collection(arguments.len() as u64)
                .saturating_add(Cost::term(1).saturating_mul(arguments.len() as u64)),
        )?;
        let refs = arguments
            .iter()
            .map(|argument| &argument.term)
            .collect::<Vec<_>>();
        let opened = telescope.open(&refs);
        current = opened;
    }

    Ok(None)
}

/// Projection: select a component out of a tuple, a struct, or a constructor's payload.
///
/// A `Variant` is projected through the flat runtime view `(tag, payload...)`, so field `i + 1` is payload component `i`; a `Struct` has no tag and is projected positionally. A label that survived to here has no positional meaning yet and stays stuck.
fn step_proj(kernel: &mut Kernel, proj: Proj) -> Result<Step, ReduceError> {
    let Proj { head, field } = proj;

    let Field::Index(index) = field else {
        return Ok(Step::Stop(Term::from(Subterm::Proj(Proj { head, field }))));
    };

    let head = whnf(kernel, head)?;
    let head = force(kernel, head)?;

    Ok(match Term::unwrap_or_clone(head) {
        Subterm::Tuple(Tuple { fields, .. }) if index < fields.len() => {
            Step::Continue(fields.into_iter().nth(index).expect("index bounded above"))
        }
        Subterm::Variant(ctor) if (1..=ctor.payload.len()).contains(&index) => Step::Continue(
            ctor.payload
                .into_iter()
                .nth(index - 1)
                .expect("index bounded above"),
        ),
        Subterm::Struct(Struct { fields, .. }) if index < fields.len() => {
            Step::Continue(fields.into_iter().nth(index).expect("index bounded above"))
        }
        head => Step::Stop(Term::proj(Term::from(head), index)),
    })
}

/// Eta for functions: `(x) => f(x)` is `f`, provided `f` does not itself mention `x`.
///
/// Contracting here rather than only at conversion means the two spellings have one normal form, so every consumer of a weak-head normal form sees them as the same term without having to know the rule.
fn step_func(kernel: &mut Kernel, func: Func) -> Result<Step, ReduceError> {
    let arity = func.telescope.len();

    // Three arity-sized vectors — the probe binders, their occurrences, and the refs handed to `open` — plus the opening itself. Charged even though the probe usually fails, because the probe is what allocates.
    kernel.spend(
        Cost::collection(arity as u64)
            .saturating_mul(3)
            .saturating_add(Cost::term(1).saturating_mul(arity as u64)),
    )?;

    let binders = (0..arity).map(|_| kernel.fresh(None)).collect::<Vec<_>>();
    let occurrences = binders.iter().map(Term::free_var).collect::<Vec<_>>();
    let refs = occurrences.iter().collect::<Vec<_>>();

    Ok(match Term::unwrap_or_clone(func.telescope.open(&refs)) {
        Subterm::Apply(Apply { head, arguments })
            if arguments.len() == arity
                && arguments.iter().enumerate().all(|(i, argument)| {
                    matches!(argument.term.as_ref(), Subterm::Var(var) if var.unwrap() == &binders[i])
                })
                && binders.iter().all(|binder| !head.free_vars().contains(binder)) =>
        {
            Step::Continue(head)
        }
        _ => Step::Stop(Term::from(Subterm::Func(func))),
    })
}

/// Zeta: substitute a `let`'s bindings into its tail.
///
/// The elaborator's reducer substitutes by the same rule. Binding each value as a fresh definition and opening the tail over *those* would avoid copying a value into every use, but spell reducts with names the other checker's reduction does not have. A substitution is visibly the rule, and an environment is a second place a variable's meaning can come from. Bindings are non-recursive and bind left to right, so binding `i` sees exactly the values before it.
fn step_let(kernel: &mut Kernel, let_: Let) -> Result<Term, ReduceError> {
    // One values vector, and a fresh ref vector at every binding — so the ref vectors together are triangular in the run's length, which the surface language makes as long as a program likes.
    let bindings = let_.bindings.len() as u64;
    kernel.spend(
        Cost::collection(bindings)
            .saturating_add(Cost::buffer(
                bindings.saturating_mul(bindings.saturating_add(1)) / 2,
            ))
            .saturating_add(Cost::term(1).saturating_mul(bindings)),
    )?;

    let mut values: Vec<Term> = Vec::with_capacity(let_.bindings.len());

    for binding in &let_.bindings {
        let refs = values.iter().collect::<Vec<_>>();
        values.push(binding.value().release(&refs));
    }

    let refs = values.iter().collect::<Vec<_>>();

    Ok(let_.tail.open(&refs))
}

/// Instantiate a universe-polymorphic definition at a stated instance.
///
/// This is the only position from which a polymorphic definition unfolds — a bare occurrence of one denotes no particular instance, so [`Kernel::value`](super::Kernel) withholds it there.
fn step_instance(kernel: &mut Kernel, instance: Instance) -> Result<Step, ReduceError> {
    let Instance { head, levels } = instance;

    let reduct = match &head {
        InstanceHead::Var(var) => match kernel.value_at(var.unwrap()).cloned() {
            Some(reduct) => reduct,
            None => return Ok(Step::Stop(Term::instance(head, levels))),
        },
        InstanceHead::RecProj(group, index) => {
            return Ok(Step::Continue(Term::rec_proj(
                group
                    .instantiate_universes(&levels)
                    .map_err(ReduceError::Universe)?,
                *index,
            )));
        }
    };

    // The variable's stored value may itself be a projection, whose group takes the instance whole rather than a per-level rewrite.
    Ok(Step::Continue(match reduct.as_rec_proj() {
        Some((group, index)) => Term::rec_proj(
            group
                .instantiate_universes(&levels)
                .map_err(ReduceError::Universe)?,
            index,
        ),
        None => {
            instantiate_universe_levels_scoped(&reduct, &levels).map_err(ReduceError::Universe)?
        }
    }))
}

/// Iota: dispatch a `match` on the value `forced` its scrutinee reduced to.
///
/// An arm binds that value's payload components directly. They are themselves unreduced — a `Variant` is a weak-head normal form whose sub-terms this strategy never entered — so binding them is call-by-name, not call-by-value.
///
/// The elaborator instead binds each arm to a *projection of the scrutinee as written*, because a reduced payload can carry annotation holes its zonker would then have to solve. The kernel has no zonker and no holes, so it takes the direct route.
fn step_match(forced: Term, result: MatchResult, cases: Cases) -> Step {
    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => match forced.as_bool() {
            Some(false) => Step::Continue(false_case),
            Some(true) => Step::Continue(true_case),
            None => Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                result,
                cases: Cases::Bool {
                    false_case,
                    true_case,
                },
            }))),
        },

        // A literal `Nat` is a floor over a `Zero` inner, so a zero inner is exactly "this is a concrete `k`". A literal takes its case, or the default when no case names it; anything symbolic rebuilds the neutral switch.
        Cases::Switch { cases, default } => {
            let (value, inner) = Nat::decompose(&forced);

            match Nat::is_zero(&inner) {
                true => Step::Continue(
                    cases
                        .iter()
                        .find(|(key, _)| key == &value)
                        .map(|(_, body)| body)
                        .unwrap_or(&default)
                        .clone(),
                ),
                false => Step::Stop(Term::from(Subterm::Match(Match {
                    head: forced,
                    result,
                    cases: Cases::Switch { cases, default },
                }))),
            }
        }

        Cases::Induct { cases, default } => {
            if let Subterm::Variant(Variant { tag, payload, .. }) = &*forced {
                // The arm's binders must match the payload it is opened at. `check_arm` establishes that, but only once typing reaches the elimination — and a `match` standing in a type position is reduced *before* anything types it, which is the ordering typing itself depends on. `Scope::open` asserts, so an arm that does not match would abort the walk; it is left stuck instead.
                if let Some((_, arm)) = cases
                    .iter()
                    .find(|(candidate, _)| candidate == tag)
                    .filter(|(_, arm)| arm.arity() == payload.len())
                {
                    let refs = payload.iter().collect::<Vec<_>>();
                    return Step::Continue(arm.open(&refs));
                }

                // A constructor with no arm of its own takes the catch-all, which binds nothing.
                if let Some(default) = &default {
                    return Step::Continue(default.clone());
                }
            }

            Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                result,
                cases: Cases::Induct { cases, default },
            })))
        }

        // Structural induction over a native free-monoid carrier (`Nat`/`Bin`/`List`). `FreeMonoid::uncons` owns the carrier-specific one-step decode; this is the catamorphism over it. The cons arm binds the peeled generator (absent for the unary `Nat`), the tail, and an induction hypothesis that recurses symbolically on that tail.
        Cases::FreeMonoid { carrier } => {
            let layer = match &carrier {
                Carrier::Nat { .. } => FreeMonoid::Unary,
                Carrier::Bin { grain, .. } => FreeMonoid::Bin(*grain),
                Carrier::List { .. } => FreeMonoid::List,
            }
            .uncons(Term::unwrap_or_clone(forced));

            match layer {
                Layer::Empty => Step::Continue(match carrier {
                    Carrier::Nat { empty_case, .. }
                    | Carrier::Bin { empty_case, .. }
                    | Carrier::List { empty_case, .. } => empty_case,
                }),
                Layer::Cons { head: elem, tail } => {
                    let hypothesis: Term = Subterm::Match(Match {
                        head: tail.clone(),
                        result: result.clone(),
                        cases: Cases::FreeMonoid {
                            carrier: carrier.clone(),
                        },
                    })
                    .into();

                    Step::Continue(match &carrier {
                        Carrier::Nat { cons_case, .. } => cons_case.open(&[&tail, &hypothesis]),
                        Carrier::Bin { cons_case, .. } | Carrier::List { cons_case, .. } => {
                            cons_case.open(&[
                                elem.as_ref().expect("a Bin/List cons layer carries a head"),
                                &tail,
                                &hypothesis,
                            ])
                        }
                    })
                }
                Layer::Stuck(stuck) => Step::Stop(Term::from(Subterm::Match(Match {
                    head: stuck.into(),
                    result,
                    cases: Cases::FreeMonoid { carrier },
                }))),
            }
        }
    }
}

/// Strip `rec` binding syntax without unfolding a member's fixed point, leaving the projection that `rec f = ...; f` denotes.
fn expose_rec_tail(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    let mut term = term;

    loop {
        // A projection already *is* the member it denotes: opening its tail over the group yields the same term, so this is where stripping stops rather than a step it could take.
        if term.as_rec_proj().is_some() {
            return Ok(term);
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => term = whnf(kernel, unfold_rec(rec))?,
            other => return Ok(other.into()),
        }
    }
}

/// Open a `rec` group's tail over its members. A pure binder operation: it mints nothing and unfolds no fixed point.
pub(crate) fn unfold_rec(rec: Rec) -> Term {
    let members = rec.group.members();
    let refs = members.iter().collect::<Vec<_>>();

    rec.tail.open(&refs)
}

/// Unfold a `rec` head that some eliminator demands the value of.
///
/// The main loop treats a `rec` as a normal form, which is what keeps a recursive definition from unfolding forever at every occurrence. An eliminator that actually needs the value calls this, which unfolds and re-reduces until it reaches one.
///
/// An unfolding is kept when it achieved something, and there are two ways to have achieved something. A **head constructor** means an eliminator can absorb the result — a productive definition exposing `cons(x, f(k))` has made progress even though `f` is still named underneath. A reduct **carrying no member of the group** means the recursion is finished — `f(0, acc)` reducing to `acc` is an answer, and an answer is not less of one for being a variable. What is discarded is the remaining case: still neutral, and still naming the group. That is an unfolding that came back to where it started, and returning the folded spelling instead is what stops the unfold-and-restuck cycle — without it a recursive function on a symbolic argument grows one more copy of its own body at every demand and never reaches a normal form.
///
/// Reading the head alone cannot separate *stuck* from *finished*, since both are neutral; reading the occurrence alone cannot separate *restuck* from *productive*, since both name the group. The clause needs both, and the group it asks about is the one `folded` is a call on, because the cycle being ruled out is this term growing under repeated demand.
///
/// A non-productive group still spins until the budget runs out, exactly as a top-level `rec` does — every outcome here is idempotent, so this decides which reducts survive, never whether the walk stops.
fn force(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    // A closed term takes the machine at the eliminator's demand; the recursive loop below is the strategy for everything the gate declines.
    if machine_admissible(kernel, &term) {
        return reduce_closed(kernel, term, Demand::Forced);
    }

    let folded = term.clone();
    let mut term = term;

    loop {
        kernel.spend(Cost::STEP)?;

        // Unfolding a projection means stepping to the member's body; unfolding any other `rec` means opening its tail. Both are the same rule read at the two tail shapes.
        if let Some((group, index)) = term.as_rec_proj() {
            let body = group.member_body(index);
            term = whnf(kernel, body)?;
            continue;
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => term = whnf(kernel, unfold_rec(rec))?,
            Subterm::Apply(apply) => match unfold_rec_apply(kernel, apply)? {
                Some(unfolded) => term = whnf(kernel, unfolded)?,
                None => return Ok(folded),
            },
            other => {
                let stuck = matches!(
                    other,
                    Subterm::Match(_) | Subterm::Var(_) | Subterm::Metavar(_) | Subterm::Proj(_)
                );
                let value: Term = other.into();

                if !stuck {
                    return Ok(value);
                }

                return Ok(match forced_group(kernel, &folded)? {
                    Some(group) if value.mentions_rec_member(&group) => folded,
                    _ => value,
                });
            }
        }
    }
}

/// The group `folded` denotes a call on, which is what [`force`] asks its occurrence question about.
///
/// Read off the term three ways because a folded call has three spellings: a member selection carries the group on the projection, a `rec` value carries it directly, and an application carries it at the head of its spine — the same place [`unfold_rec_apply`] reads it, reached again through the evaluation memo rather than by a fresh walk. A term that denotes no recursive call answers `None`, and nothing can restick in it.
fn forced_group(kernel: &mut Kernel, folded: &Term) -> Result<Option<RecGroup>, ReduceError> {
    if let Some((group, _)) = folded.as_rec_proj() {
        return Ok(Some(group.clone()));
    }

    match &**folded {
        Subterm::Rec(rec) => Ok(Some(rec.group.clone())),
        Subterm::Apply(Apply { head, .. }) => {
            let head = whnf(kernel, head.clone())?;
            let head = expose_rec_tail(kernel, head)?;

            Ok(head.spine_rec_proj().map(|(group, _)| group.clone()))
        }
        _ => Ok(None),
    }
}

/// The one definitional unfolding [`force`] withholds: a folded recursive spelling — a `rec` value, a member selection, or a member application — stepped to the weak-head form of its body. `None` for every other shape.
///
/// `force` keeps the folded spelling as a recursive call's normal form, while an arm's induction hypothesis is the raw stuck fold-match on the same argument; conversion consults this to see the two spellings as one.
pub(crate) fn unfold_spelling(
    kernel: &mut Kernel,
    term: &Term,
) -> Result<Option<Term>, ReduceError> {
    if let Some((group, index)) = term.as_rec_proj() {
        let body = group.member_body(index);

        return Ok(Some(whnf(kernel, body)?));
    }

    match &**term {
        Subterm::Rec(rec) => Ok(Some(whnf(kernel, unfold_rec(rec.clone()))?)),
        Subterm::Apply(apply) => match unfold_rec_apply(kernel, apply.clone())? {
            Some(unfolded) => Ok(Some(whnf(kernel, unfolded)?)),
            None => Ok(None),
        },
        _ => Ok(None),
    }
}

/// Unfold one folded recursive application, when its result shape is demanded.
fn unfold_rec_apply(kernel: &mut Kernel, apply: Apply) -> Result<Option<Term>, ReduceError> {
    let Apply { head, arguments } = apply;

    let head = whnf(kernel, head)?;
    let head = expose_rec_tail(kernel, head)?;

    // A projection is the shape a *recursive* member keeps: opening the group's tail over its own members reproduces it, which is where `expose_rec_tail` stops. A member that does not occur in its own body has no fixed point to keep, so the same opening reduces past the projection to the member's value, and the applicable term is then the exposed head itself. Both are the one beta step this function exists to take, and the elaborator's twin takes them the same way — an `induct`'s type constructor lowers into a `rec` whatever its arity, since otherwise a caller reaching the unfolded spelling would see a nominal type where one reaching the folded spelling sees a stuck application.
    let body = match head.as_rec_proj() {
        Some((group, index)) => {
            let body = whnf(kernel, group.member_body(index))?;

            force(kernel, body)?
        }
        None => head,
    };

    let telescope = match Term::unwrap_or_clone(body) {
        Subterm::Func(Func { telescope, .. }) => telescope,
        // The head is itself a folded call: a member whose result is a function, applied past its own parameters. Unfolding that call one step and applying what it becomes to what is left is this application's one step, as the elaborator's twin takes it — read one level deep, `f(a)(b)` would be a neutral no demand unfolds.
        Subterm::Apply(inner) => {
            return Ok(unfold_rec_apply(kernel, inner)?.map(|unfolded| {
                Subterm::Apply(Apply {
                    head: unfolded,
                    arguments,
                })
                .into()
            }));
        }
        _ => return Ok(None),
    };
    // Saturation, for the reason `step_apply` needs it: this is the recursive twin of the β step, and `Telescope::open` asserts. An application that does not saturate its member declines to unfold rather than aborting the walk.
    if telescope.len() != arguments.len() {
        return Ok(None);
    }

    let refs = arguments
        .iter()
        .map(|argument| &argument.term)
        .collect::<Vec<_>>();
    Ok(Some(telescope.open(&refs)))
}
