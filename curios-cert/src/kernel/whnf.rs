//! Weak-head normalization: the kernel's reduction strategy.
//!
//! This is the kernel's answer to "what does this term compute to, as far as its head". It is deliberately the *whole* strategy — beta, delta, iota, projection, universe instantiation, intrinsic folding, and `rec` unfolding — and deliberately nothing else. In particular there is no metavariable resolution, part of what makes the elaborator's reducer fast and forgiving; here it would be one more way for the answer to come from somewhere other than the term. The two stores the loop does consult stay on the term's side of that line: the evaluation memo replays the kernel's own pure function of the definition store rather than remembering anyone's claim — [`memos`](super::memos) owns that argument — and a case-equation refinement is the arm's own definitional hypothesis, assumed by the elimination check that justified it and scoped to its arm.
//!
//! It resembles the elaborator's reducer closely, and that resemblance is the point of writing it out rather than sharing it. Reduction decides which programs convert, and conversion decides which programs typecheck; a bug shared by both checkers is a bug neither can catch. The crate boundary enforces this — `curios-elab`'s reducer is not visible from here — so the duplication cannot quietly collapse back into a call.
//!
//! Two things *are* shared, and both are representation rather than judgment: the binder discipline that `open`/`release` implement, and [`reduce_intrinsic`](curios_core::reduce_intrinsic), which decides what `2 + 2` folds to. Neither can admit an ill-typed program on its own.

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
    curios_core::{
        Apply, Bound, Carrier, Cases, ClosedHost, Cost, Demand, Field, Free, FreeMonoid, Func,
        Layer, Let, Many, Match, Nat, Proj, Rec, RecGroup, ReduceError, Reducer, Scope, Struct,
        Subterm, Term, Tuple, UniverseInst, Var, Variant, Visit, accelerable,
        instantiate_universe_levels_scoped, reduce_closed, reduce_intrinsic,
    },
    curios_utilities::recurse,
};

/// The kernel's side of the closed-machine seam: the same delta `step_var` and `step_universe_inst` perform, handed to the shared machine so a closed term evaluates at machine depth under this strategy's own charges.
impl ClosedHost for Kernel {
    fn closed_body(&self, name: &Free) -> Option<&Term> {
        self.value(name)
    }

    fn closed_body_at(&self, name: &Free) -> Option<&Term> {
        self.value_at(name)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        self.fresh(hint)
    }
}

/// Whether the closed machine may take `term` under this judgment: the kernel admits it at all (the `machine` field is false only in the differential fixture's strategy arm), the representation-side gate ([`accelerable`]) holds, and no case equation is in scope — because inside an arm a closed scrutinee *is* the arm's assumed value.
fn machine_admissible(kernel: &Kernel, term: &Term) -> bool {
    kernel.machine && accelerable(term) && !kernel.has_refinements()
}

/// The kernel's reduction strategy: everything unfolds, and what a term unfolds to is remembered — for the declaration if it is local-free, for as long as the equations in force stand if it is not — see the `memos` and `spend` modules for what that does and does not concede, and for why a hit on this entry point is free while a definition unfold's is charged.
impl Reducer for Kernel {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        whnf(self, term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        if let Some(replayed) = self.whnf_hit(&term, true) {
            return Ok(replayed);
        }

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
}

/// One step's outcome: another term to reduce, or a weak-head normal form.
enum Step {
    Continue(Term),
    Stop(Term),
}

/// Reduce `term` until its head constructor is stable.
///
/// Guarded by [`recurse`] for the same reason the crate duplicates the strategy at all: the kernel has to accept every term the elaborator produced, on the same thread stack, so a depth it aborts at that the elaborator does not is a term that typechecks and then fails to certify. That is how the need was found — the elaborator was given its reserve first, and the abort simply moved here. An intrinsic's operands re-enter through [`reduce_intrinsic`](curios_core::reduce_intrinsic), which is shared, so a deep `add` chain puts one native frame per link on this side exactly as it does on the other.
pub(crate) fn whnf(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    // The level itself, charged when it is deeper than any this judgment has reached — see `Spend::enter_level`. What it buys is that depth is bounded by the budget rather than by how much stack the host handed the process, which is the one resource this walk could previously consume without being counted.
    kernel.enter_level()?;
    let reduct = recurse(|| whnf_within(kernel, term));
    kernel.leave_level();

    reduct
}

fn whnf_within(kernel: &mut Kernel, term: Term) -> Result<Term, ReduceError> {
    // **The memo is consulted here, at every level, and not only where something outside reduction asks for one.**
    //
    // It used to sit on the two `Reducer` methods alone, so the fifteen internal calls below — a scrutinee, an application's head, each turn of `force`'s loop, `expose_rec_tail`, `unfold_spelling` — re-derived what the table already held, and `reduce_forced` re-derived its own weak-head half because it probed only the `forced` table before calling this directly. The elaborator's reducer has always probed at its own entry and recursed through that same entry, so the two strategies differed in reach rather than in rule; measured over a `Str` literal's UTF-8 scan the kernel charged 72× what the elaborator did for the *same* reduction, at the same peak depth, and the whole of that gap was this.
    //
    // What makes reaching further safe is that the table a term belongs to is decided inside [`Memos`](super::Memos), on the lookup as well as the store, so every new site here is protected by the same dispatch the two old ones were: a local-free term by the tables that live for the declaration, a local-bearing one by the tables that live only as long as the equations in force.
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
        // Asking only afterwards made the answer depend on affording the reduction. `Lt(i, len(b))` under a guard that refined exactly that comparison would fold the intrinsic first — evaluating `b` — and consult the equation about a value it had already spent the budget to compute. The elaborator's reducer asks first and this did not, which is the divergence [`Scope::refine`] records as a defect in its own right: a program the elaborator accepts and the kernel then refuses reads as a disagreement about the rule, and is not one.
        //
        // The written spelling alone here. An equation is *recorded* under it, so this point answers the scrutinee's own occurrences — the common case, and the one whose whole cost is this comparison. The reduced spelling is what [`refined_reduct`] escalates to, at the other point, where the terms that need it arrive.
        if let Some(refined) = kernel.refinement_of(&term) {
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
            Subterm::UniverseInst(instance) => step_universe_inst(kernel, instance)?,
            // The scrutinee is reduced by a nested call, so a tower of matches over a deep closed spine — the scan-state chain a string literal lowers to — costs one native frame per link. That is data-shaped depth, which is what [`recurse`] at the entry point is for.
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                let value = whnf(kernel, head)?;

                step_match(force(kernel, value)?, motive, cases)
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

    if !value.has_local_free() || !kernel.has_refinements() {
        return Ok(None);
    }

    let canonical = canonical_operands(kernel, value)?;

    loop {
        if let Some(refined) = kernel.refinement_of_reduct(&canonical) {
            return Ok(Some(refined));
        }

        let Some((index, key)) = kernel.unasked_refinement(&canonical) else {
            return Ok(None);
        };

        kernel.settle_refinement(index, key)?;
    }
}

/// `term` with each operand in weak-head normal form, where it is a tagged intrinsic — the form a refinement's reduced spelling and the value probed against it are both held in. Anything else is its own canonical form.
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
        operands.push(whnf(kernel, operand)?);
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
/// The body is reduced once and its reduct remembered, so the next occurrence of the name — in this spine or any later one — continues from the reduct instead of re-deriving it. A definition body is closed, so the memo entry depends on nothing but the definition store. The nested `whnf` recurses one native frame per link of a definition-reference chain, which is authored depth, not data depth.
fn step_var(kernel: &mut Kernel, var: Var) -> Result<Step, ReduceError> {
    let Some(body) = kernel.value(var.unwrap()).cloned() else {
        return Ok(Step::Stop(Term::var(var)));
    };

    // A `None` here is "no entry, or one this budget cannot afford", and both mean the same thing: fall through and evaluate. The direct path then exhausts at the charge that could not be paid rather than at a recorded total.
    if let Some(replayed) = kernel.unfold_hit(var.unwrap()) {
        return Ok(Step::Continue(replayed));
    }

    let before = kernel.consumption();
    let reduct = whnf(kernel, body)?;
    let replay = kernel.replay_since(reduct.clone(), before);
    kernel.unfold_store(var.unwrap().clone(), replay);

    Ok(Step::Continue(reduct))
}

/// Beta: open a function's telescope over the arguments applied to it.
///
/// A `rec` head is exposed but not unfolded — the folded spelling stays the normal form of a recursive call, and [`force`] is what demands otherwise.
fn step_apply(kernel: &mut Kernel, apply: Apply) -> Result<Step, ReduceError> {
    let Apply {
        head,
        params,
        plicities,
    } = apply;

    let head = whnf(kernel, head)?;
    let head = expose_rec_tail(kernel, head)?;

    Ok(match Term::unwrap_or_clone(head) {
        // Saturation is the precondition of the β step, not an assumption about it: `Telescope::open` asserts on a count mismatch, so an under- or over-applied lambda would abort the walk rather than be refused. An application that does not saturate its lambda is stuck instead, which is the conservative direction — it leaves the term for the typing rules to reject with a diagnostic, and reduction that declines to fire can never admit anything.
        Subterm::Func(Func { telescope, .. }) if telescope.len() == params.len() => {
            // The argument ref vector, and what `Telescope::open` costs on top of it: it clones the whole boxed chain and then substitutes once per binder, so an `n`-ary beta step is `n` boxes and `n` passes rather than one.
            kernel.spend(
                Cost::collection(params.len() as u64)
                    .saturating_add(Cost::term(1).saturating_mul(params.len() as u64)),
            )?;

            let refs = params.iter().collect::<Vec<_>>();
            Step::Continue(telescope.open(&refs))
        }
        head => Step::Stop(Term::from(Subterm::Apply(Apply {
            head: head.into(),
            params,
            plicities,
        }))),
    })
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
        Subterm::Apply(Apply { head, params, .. })
            if params.len() == arity
                && params.iter().enumerate().all(|(i, param)| {
                    matches!(param.as_ref(), Subterm::Var(var) if var.unwrap() == &binders[i])
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
/// The elaborator instead binds each value as a fresh definition and opens the tail over *those*, which avoids copying a value into every use. The kernel substitutes, because a substitution is visibly the rule and an environment is a second place a variable's meaning can come from. Bindings are non-recursive and bind left to right, so binding `i` sees exactly the values before it.
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
fn step_universe_inst(kernel: &mut Kernel, instance: UniverseInst) -> Result<Step, ReduceError> {
    let UniverseInst { head, levels } = instance;

    let reduct = match &*head {
        Subterm::Var(var) => kernel.value_at(var.unwrap()).cloned(),
        _ => Some(head.clone()),
    };

    let Some(reduct) = reduct else {
        return Ok(Step::Stop(Term::universe_inst(head, levels)));
    };

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
fn step_match(forced: Term, motive: Scope<Many>, cases: Cases) -> Step {
    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => match forced.as_bool() {
            Some(false) => Step::Continue(false_case),
            Some(true) => Step::Continue(true_case),
            None => Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                motive,
                cases: Cases::Bool {
                    false_case,
                    true_case,
                },
            }))),
        },

        // A literal `Nat` is a floor over a `Zero` inner, so a zero inner is exactly "this is a concrete `k`". A literal takes its case, or the default when no case names it (including a value past the `u32` keys); anything symbolic rebuilds the neutral switch.
        Cases::Switch { cases, default } => {
            let (value, inner) = Nat::decompose(&forced);

            match Nat::is_zero(&inner) {
                true => Step::Continue(
                    value
                        .to_u32()
                        .and_then(|key| cases.get(&key))
                        .unwrap_or(&default)
                        .clone(),
                ),
                false => Step::Stop(Term::from(Subterm::Match(Match {
                    head: forced,
                    motive,
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
                motive,
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
                        motive: motive.clone(),
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
                    motive,
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
/// Read off the term three ways because a folded call has three spellings: a member selection carries the group on the projection, a `rec` value carries it directly, and an application carries it on its head — the same place [`unfold_rec_apply`] reads it, reached again through the evaluation memo rather than by a fresh walk. A term that denotes no recursive call answers `None`, and nothing can restick in it.
fn forced_group(kernel: &mut Kernel, folded: &Term) -> Result<Option<RecGroup>, ReduceError> {
    if let Some((group, _)) = folded.as_rec_proj() {
        return Ok(Some(group.clone()));
    }

    match &**folded {
        Subterm::Rec(rec) => Ok(Some(rec.group.clone())),
        Subterm::Apply(Apply { head, .. }) => {
            let head = whnf(kernel, head.clone())?;
            let head = expose_rec_tail(kernel, head)?;

            Ok(head.as_rec_proj().map(|(group, _)| group.clone()))
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
    let Apply { head, params, .. } = apply;

    let head = whnf(kernel, head)?;
    let head = expose_rec_tail(kernel, head)?;

    let Some((group, index)) = head.as_rec_proj() else {
        return Ok(None);
    };

    let body = whnf(kernel, group.member_body(index))?;
    let body = force(kernel, body)?;

    let Subterm::Func(Func { telescope, .. }) = Term::unwrap_or_clone(body) else {
        return Ok(None);
    };
    // Saturation, for the reason `step_apply` needs it: this is the recursive twin of the β step, and `Telescope::open` asserts. An application that does not saturate its member declines to unfold rather than aborting the walk.
    if telescope.len() != params.len() {
        return Ok(None);
    }

    let refs = params.iter().collect::<Vec<_>>();
    Ok(Some(telescope.open(&refs)))
}
