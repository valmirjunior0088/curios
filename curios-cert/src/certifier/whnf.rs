//! Weak-head normalization: the certifier's reduction strategy.
//!
//! This is the certifier's answer to "what does this term compute to, as far as its head". It is deliberately the *whole* strategy — beta, delta, iota, projection, universe instantiation, intrinsic folding, and `rec` unfolding — and deliberately nothing else. In particular there is no metavariable resolution, part of what makes the elaborator's reducer fast and forgiving; here it would be one more way for the answer to come from somewhere other than the term. The two stores the loop does consult stay on the term's side of that line: the evaluation memo replays the certifier's own pure function of the definition store rather than remembering anyone's claim — [`memos`](super::memos) owns that argument — and a case-equation refinement is the arm's own definitional hypothesis, assumed by the elimination check that justified it and scoped to its arm.
//!
//! It resembles the elaborator's reducer closely, and that resemblance is the point of writing it out rather than sharing it. Reduction decides which programs convert, and conversion decides which programs typecheck; a bug shared by both checkers is a bug neither can catch. The crate boundary enforces this — `curios-elab`'s reducer is not visible from here — so the duplication cannot quietly collapse back into a call.
//!
//! Two things *are* shared, and both are representation rather than judgment: the binder discipline that `open`/`release` implement, and [`reduce_intrinsic`], which decides what `2 + 2` folds to. Neither can admit an ill-typed program on its own.

#[cfg(test)]
mod budget_tests;
#[cfg(test)]
mod closed_machine_tests;
#[cfg(test)]
mod equation_scope_tests;
#[cfg(test)]
mod guard_tests;
#[cfg(test)]
mod memo_tests;
#[cfg(test)]
mod question_tests;
#[cfg(test)]
mod rules_tests;
#[cfg(test)]
mod test_support;

use {
    super::{ByValue, Certifier, asked_classes, asked_within, stated_payload, unread_payload},
    curios_analysis::{
        Answered, RESOLVED_SPELLING_LAYERS, ScrutineeRead, answers, answers_classed, atoms_within,
        could_answer, read_operands, read_scrutinee, read_scrutinee_classed, refold,
    },
    curios_common::recurse,
    curios_core::{
        Apply, Atom, Bound, Carrier, Cases, ClosedHost, ClosedMemo, Cost, Demand, Field, Free,
        FreeMonoid, Func, Global, Instance, Intrinsic, Keys, Layer, Let, Match, Nat, Probe, Proj,
        Rec, RecGroup, ReduceError, Reducer, Struct, Subterm, Term, Tuple, Universes, Var, Variant,
        Visit, accelerable, instantiate_universes, probe_spellings, reduce_closed,
        reduce_intrinsic, weak_head_value,
    },
};

/// The certifier's side of the closed-machine seam: the same delta `step_instance` performs, handed to the shared machine so a closed term evaluates at machine depth under this strategy's own charges.
impl ClosedHost for ByValue<'_, '_> {
    /// None: a reading's terms name no `let`-bound local, a door having unfolded each before the reading began ([`ByValue`]).
    fn closed_body(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn stated_payload(
        &mut self,
        stated: &Term,
        tag: &Atom,
    ) -> Result<Option<Vec<Term>>, ReduceError> {
        stated_payload(self, stated, tag)
    }

    fn closed_body_at(&self, name: &Global, universes: &Universes) -> Option<&Term> {
        self.value_at(name, universes)
    }

    fn payload_proofs(&self, family: &Global, tag: &Atom) -> Vec<bool> {
        Certifier::payload_proofs(self, family, tag)
    }

    fn closed_memo(&mut self) -> &mut ClosedMemo {
        self.closed_memo_mut()
    }
}

/// A run of the closed machine over `term`, on the memo the certifier keeps for its runs.
fn closed_run(certifier: &mut ByValue, term: Term, demand: Demand) -> Result<Term, ReduceError> {
    let outer = certifier.begin_closed_run();
    let value = reduce_closed(certifier, term, demand);
    certifier.end_closed_run(outer);

    value
}

/// Whether the closed machine may take `term` under this judgment: the certifier admits it at all (the `machine` field is false only in the differential fixture's strategy arm) and the representation-side gate ([`accelerable`]) holds. An equation in force is no reason to stand aside, since none answers a term that names no local: `Scope::refine` records only a local-bearing scrutinee and [`refined_reduct`] probes only a local-bearing form.
fn machine_admissible(certifier: &Certifier, term: &Term) -> bool {
    certifier.machine && accelerable(term)
}

/// The certifier's reduction strategy: everything unfolds, and what a term unfolds to is remembered — for the declaration if it is local-free, for as long as the equations in force stand if it is not — see the `memos` and `spend` modules for what that does and does not concede, and for why a hit is free.
///
/// **A reading's terms are by value already**, so the strategy reads what it is handed: a shared fold or analysis the reading called hands back terms built from the reading's own ([`ByValue`]).
impl Reducer for ByValue<'_, '_> {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        curios_common::profile!("reduce");
        self.whnf(term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        // A weak-head value is its own forced form, asked of no table ([`weak_head_value`]).
        if weak_head_value(&term) {
            return Ok(term);
        }
        if let Some(remembered) = self.whnf_hit(&term, true) {
            return Ok(remembered);
        }
        // Past the memo, so the span times reductions the table did not answer: a hit costs a lookup, and a span per hit would cost the profile more rows than any other site and bill each its own row-writing.
        curios_common::profile!("reduce_forced");

        let reduced = self.whnf(term.clone())?;
        let reduct = force(self, reduced)?;
        self.whnf_store(term, true, reduct.clone());

        Ok(reduct)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        ByValue::spend(self, cost)
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        Certifier::fresh(self, hint)
    }
}

/// One step's outcome: another term to reduce, or a weak-head normal form.
enum Step {
    Continue(Term),
    Stop(Term),
}

impl ByValue<'_, '_> {
    /// Reduce `term` until its head constructor is stable.
    ///
    /// Guarded by [`recurse`] for the same reason the crate duplicates the strategy at all: the certifier has to accept every term the elaborator produced, on the same thread stack, so a depth it aborts at that the elaborator does not is a term that typechecks and then fails to certify. An intrinsic's operands re-enter through [`reduce_intrinsic`], which is shared, so a deep `add` chain puts one native frame per link on this side exactly as it does on the other.
    ///
    /// **Reduction reads by value.** Typing walks a `let`'s tail over its binder, so the term it hands over may name one; the door it enters by gives the name way to what it stands for, once, and everything beneath — the tables' keys, the case equations' probes, the closed machine's gate — meets the spelling a substituted `let` leaves, which is the spelling each was argued over ([`ByValue`]). A remembered reduct therefore never rests on a `let`'s binding, which an arm may replace and no key of these tables carries.
    pub(crate) fn whnf(&mut self, term: Term) -> Result<Term, ReduceError> {
        // The level itself, charged when it is deeper than any this judgment has reached — see `Spend::enter_level`. What it buys is that depth is bounded by the budget rather than by how much stack the host handed the process, which would otherwise be the one resource this walk consumes without being counted.
        self.enter_level()?;
        let reduct = recurse(|| whnf_within(self, term));
        self.leave_level();

        reduct
    }
}

fn whnf_within(certifier: &mut ByValue, term: Term) -> Result<Term, ReduceError> {
    // A weak-head value is its own reduct, asked of no table ([`weak_head_value`]): read by value already, it names no binder a `let` could give way for.
    if weak_head_value(&term) {
        return Ok(term);
    }

    // **The memo is consulted here, at every level, and not only where something outside reduction asks for one.**
    //
    // Probing only at the two `Reducer` methods would leave every internal call below — a scrutinee, an application's head, each turn of `force`'s loop, `expose_rec_tail`, `unfold_spelling` — re-deriving what the table already holds; the elaborator's reducer probes at its own entry and recurses through it, so probing here keeps the two strategies alike in reach as well as in rule. Reaching every level is safe because the table a term belongs to is decided inside [`Memos`](super::Memos), on the lookup as well as the store: a local-free term by the tables that live for the declaration, a local-bearing one by the tables that live only as long as the equations in force.
    //
    // **The memo is asked before the case equations below, and that order rests on the recording rule.** A term the declaration-lived tables answer is local-free, and no equation is recorded under a local-free spelling (`curios_analysis::records_case_equation`), so no entry there can stand in for an answer an equation in force would give; a local-bearing entry is cleared wherever the equations in force change. `a_remembered_closed_term_answers_inside_an_arm_as_an_uncached_certifier_does` holds the first half, and fails once a local-free equation is recorded with this order kept; `a_remembered_reduct_does_not_outlive_the_equations_it_was_taken_under` holds the second.
    if let Some(remembered) = certifier.whnf_hit(&term, false) {
        return Ok(remembered);
    }

    // A closed term takes the machine: same rules, same counter, machine depth instead of one native frame per element. Stored under the same memo entry a strategy-derived reduct would be.
    if machine_admissible(certifier, &term) {
        let entry = term.clone();
        let value = closed_run(certifier, term, Demand::Whnf)?;
        certifier.whnf_store(entry, false, value.clone());

        return Ok(value);
    }

    let entry = term.clone();
    let mut term = term;

    loop {
        certifier.spend(Cost::STEP)?;

        // An arm's case equation is consulted *before* the term is taken apart, not only on the value it reduced to. Both points are sound for the same reason and by the same test: [`Scope::refinement_of`] matches by `Term`'s structural equality, universe instances included, so a hit means this term *is* the registered scrutinee and the arm's hypothesis applies to it directly — and the value it substitutes is a constructor, a normal form, so nothing cycles.
        //
        // Asking only afterwards would make the answer depend on affording the reduction: `Lt(i, len(b))` under a guard that refined exactly that comparison would fold the intrinsic first — evaluating `b` — and consult the equation about a value it had already spent the budget to compute. The elaborator's reducer asks first as well, so a program the elaborator accepts is not refused here for a reason that reads as a disagreement about the rule and is not one.
        //
        // The written spelling alone here. An equation is *recorded* under it, so this point answers the scrutinee's own occurrences — the common case, and the one whose whole cost is this comparison. The reduced spelling is what [`refined_reduct`] escalates to, at the other point, where the terms that need it arrive.
        if let Some(refined) = certifier.refinement_of(&term) {
            term = refined;
            continue;
        }
        if let Some(refined) = refined_spelling(certifier, &term) {
            term = refined;
            continue;
        }

        let closed = !term.has_local_free();
        // An intrinsic's allocation, for a fold that leaves it as it was to hand back ([`Term::rebuilt`]).
        let intrinsic = matches!(&*term, Subterm::Intrinsic(_)).then(|| term.clone());
        let step = match Term::unwrap_or_clone(term) {
            Subterm::Intrinsic(written) => Step::Stop(
                intrinsic
                    .expect("an intrinsic's allocation is kept")
                    .rebuilt(reduce_intrinsic(certifier, &written)?),
            ),
            Subterm::Var(var) => step_var(certifier, var)?,
            Subterm::Apply(apply) => step_apply(certifier, apply)?,
            Subterm::Proj(proj) => step_proj(certifier, proj)?,
            Subterm::Func(func) => step_func(certifier, func)?,
            Subterm::Let(let_) => Step::Continue(step_let(certifier, let_)?),
            Subterm::Instance(instance) => step_instance(certifier, instance)?,
            // The scrutinee is reduced by a nested call, so a tower of matches over a deep closed spine — the scan-state chain a string literal lowers to — costs one native frame per link. That is data-shaped depth, which is what [`recurse`] at the entry point is for.
            Subterm::Match(Match { head, goal, cases }) => {
                // A match the instance it states decides does not reduce its scrutinee, and reads nothing of it: a match that states one eliminates a proposition, whose closed proofs are total and are its one constructor. Closed, it is its arm: at no payload where the arm names none, as the closed machine takes it, and at the payload the stated instance's indices say where it names one. Open, stating an instance of a proposition whose indices pin its payload, it is its arm at that payload where the instance is the one the constructor builds there, and stands where it is not. A match that states none is reduced by its scrutinee's value, a match on data among them, whose closed scrutinee may be a loop no checker unfolds.
                let fired = match (cases.sole_arm(), cases.stated()) {
                    (None, _) => None,
                    (Some(_), Some(_)) if closed && cases.unread().is_some() => {
                        Some(cases.unread())
                    }
                    (Some(_), None) => None,
                    (Some((tag, arm)), Some(stated)) => match closed {
                        true => stated_payload(certifier, stated, tag)?
                            .filter(|payload| payload.len() == arm.arity())
                            .map(|payload| Some(arm.open(&payload.iter().collect::<Vec<_>>()))),
                        false => unread_payload(certifier, stated, tag)?.map(|payload| {
                            payload
                                .filter(|payload| payload.len() == arm.arity())
                                .map(|payload| arm.open(&payload.iter().collect::<Vec<_>>()))
                        }),
                    },
                };

                match fired {
                    Some(Some(body)) => Step::Continue(body),
                    Some(None) => {
                        Step::Stop(Term::from(Subterm::Match(Match { head, goal, cases })))
                    }
                    None => {
                        let value = certifier.whnf(head)?;
                        let forced = force(certifier, value)?;

                        step_match(scrutinee_read(certifier, &cases, forced)?, goal, cases)
                    }
                }
            }
            // `InductType`/`Variant`, `StructType`/`Struct`, `Tuple`, `FuncType`, `Type`/`Prop`, `Rec`, and a `Metavar` no certifier input should contain are all weak-head normal already: their sub-terms are not reduced in this position.
            other => Step::Stop(other.into()),
        };

        match step {
            Step::Continue(next) => term = next,
            Step::Stop(value) => {
                // A stuck form standing under an arm's case equation *is* that case's value, definitionally; continue from it. This is the *second* of the two probe points, and it does not merge with the one above: that one asks about a term before it is taken apart, this one about a form reduction produced, and routing this one back to the top would re-decompose a normal form forever.
                match refined_reduct(certifier, &value)? {
                    Some(refined) => term = refined,
                    None => match refolded(certifier, &value)? {
                        // A fold the certifier's own conversion decided: what it folded to is reduced on, as an equation's answer is.
                        Some(folded) => term = folded,
                        None => {
                            // Remembered under the term this level was *entered* with, not the one the loop finished on — the same key the probe above will present.
                            certifier.whnf_store(entry, false, value.clone());

                            return Ok(value);
                        }
                    },
                }
            }
        }
    }
}

/// A stuck comparison under a guard recorded on another spelling of it, asked under each of [`probe_spellings`] in turn. The false arm of `n < m` records `n < m` alone, and `m <= n` is that fact read the other way, so the dual answers with the literal negated. A bound reaches the probe as `i + 1 <= len(l)` while the guard that decided it was written `i < len(l)`, one fact on `Nat` and on `Int`, so the successor spelling answers with the literal carried across — not negated, these being the same proposition rather than opposite ones. And the two compose: the false arm of `x <= 4` is the fact `5 <= x`, which the dual of the successor spelling answers. Lookup only — nothing is recorded under any of them, so a guard still refines exactly what it was written as.
fn refined_spelling(certifier: &Certifier, term: &Term) -> Option<Term> {
    let Subterm::Intrinsic(intrinsic) = &**term else {
        return None;
    };
    probe_spellings(intrinsic).find_map(|(spelling, negated)| {
        let literal = certifier
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
/// **A settled spelling answers what the readers hold equal to it.** Where no settled spelling is the probe's, each is put to `curios_analysis::answers` with it: the rule the elaborator's reducer asks at the same point, so a guard respelled inside the theory — a sum commuted, a dual with its sides moved — is answered by both or by neither. A reduct's dual and its spelling across the successor seam are the rule's to answer as well, so no spelling but the probe's own is compared with a reduct here; [`refined_spelling`] asks them of the written key, before anything is reduced.
///
/// **The reduced spellings meet in operand-canonical form.** A settled reduct is the key's weak-head form with each operand weak-head reduced, and the value probed against it is brought to the same form here, so that `x && true` under a `match x && g(7)` meets the key whose `g(7)` the settlement folded, and `x && h(7)` meets it too. A weak-head form alone would not do: a `&&` behind a stuck left leaves its right as written, and the two spellings would then differ by exactly the fold the escalation exists to see through. It is computed only on a miss under a live equation and only for a tagged intrinsic — a `head_key` — which is the same path the elaborator's `refined_after_fold` canonicalizes on, and what keeps the two checkers reaching the same occurrences.
fn refined_reduct(certifier: &mut ByValue, value: &Term) -> Result<Option<Term>, ReduceError> {
    if let Some(refined) = certifier.refinement_of(value) {
        return Ok(Some(refined));
    }
    if let Some(refined) = refined_spelling(certifier, value) {
        return Ok(Some(refined));
    }

    if !value.has_local_free() || !certifier.has_refinements() {
        return Ok(None);
    }

    let canonical = canonical_operands(certifier, value)?;
    // The binders the form names that are proofs, which the filter in front of every equation passes over.
    let proofs = certifier.proofs_named(&canonical)?;
    let admitted = admitted(certifier, &canonical, &proofs)?;

    // The equations already put to the shared rule, so each is asked once however many settlements the loop makes.
    let mut consulted = Vec::new();

    loop {
        if let Some(refined) = certifier.refinement_of_reduct(&canonical) {
            return Ok(Some(refined));
        }
        // The settled spellings once more, by the rule both reducers share: a term the carriers' readers hold equal to a key, or to a key negated, is the key's term, and so is one the certifier's own conversion says is.
        for (index, reduct, value) in certifier.settled_refinements(&admitted) {
            if consulted.contains(&index) {
                continue;
            }
            consulted.push(index);
            if let Some(answer) = answered(certifier, index, &canonical, &reduct, &value)? {
                return Ok(Some(answer));
            }
        }

        let Some((index, key)) = certifier.unasked_refinement(&admitted) else {
            // What a lookup that asked conversion would have been put to: the equations this stuck form could be the term of, each settled and none answering.
            if let asked @ 1.. = admitted.len() {
                curios_common::sample!("whnf::missed_lookup", asked);
            }
            return Ok(None);
        };

        certifier.settle_refinement(index, key)?;
    }
}

/// The positions of the equations in force `candidate` could be the term of (`curios_analysis::could_answer`), each asked once a probe: what the probe puts it to, and settles. `proofs` is the binders `candidate` names that are proofs.
///
/// **Read with every equation withheld and asking nothing** ([`ByValue::withholding`], [`ByValue::plainly`]). A part the filter reduces is then a reduction no probe interrupts: with the equations in force, reducing a stuck part would probe them in turn, its own filter reducing the part's parts, and probes would nest as deep as the form goes. A part only an equation takes apart is read as it stands, toward refusal, and what the bracket read goes with it.
fn admitted(
    certifier: &mut ByValue,
    candidate: &Term,
    proofs: &[Free],
) -> Result<Vec<usize>, ReduceError> {
    let keys = certifier.keys_in_force();
    certifier.withholding(0, |certifier| {
        certifier.plainly(|certifier| {
            let mut admitted = Vec::new();
            for (index, key) in keys {
                if could_answer(certifier, &key, candidate, proofs)? {
                    admitted.push(index);
                }
            }

            Ok(admitted)
        })
    })
}

/// What the equation at `index` says of the stuck form `term`, its reduced spelling `key` and its case `value`: the shared rule's answer, and where the rule hands a question back, the certifier's conversion's — unless reduction is plain, which asks nothing.
///
/// The elaborator's reducer takes the same steps in the same order, each checker asking its own conversion.
fn answered(
    certifier: &mut ByValue,
    index: usize,
    term: &Term,
    key: &Term,
    value: &Term,
) -> Result<Option<Term>, ReduceError> {
    Ok(match answers(certifier, term, key, value)? {
        Answered::Value(value) => Some(value),
        Answered::Silent => None,
        _ if certifier.plain() => None,
        Answered::Atoms(atoms) => {
            let mut classes = asked_classes(certifier, &atoms)?;
            answers_classed(certifier, term, key, value, &mut classes)?
        }
        Answered::Whole => certifier
            .asked_scrutinee(index, term, key, value)?
            .then(|| value.clone()),
    })
}

/// A stuck operation folded once more over its operands as classed, its opaque atoms by the certifier's own conversion and the operations among them by their own readers, where that decides more than the fold did (`curios_analysis::refold`): `None` where the algebra declares it no operation, or where the fold over the classed operands still stands at the operation. Conversion is asked only where some two of its atoms may be one.
///
/// **Reduction asks conversion here, at a missed equation, at a stuck elimination's atoms ([`scrutinee_read`]) and at a one-arm match of a proposition ([`unread_payload`]), and nowhere else.** A fold pairs atoms by identity, so two atoms that convert without being identical — a call under two proofs of its bound, two calls whose arguments commute a sum — left `f(a + b) == f(b + a)` stuck where conversion holds its sides one term, and conversion was no congruence under the fold. Each checker asks its own conversion, the elaborator's reducer at the same point.
///
/// **Only of a term that names a local, and never by plain reduction.** A local-free term's reduct is remembered for the declaration and must be one function of the definition store whichever reduction takes it, so nothing is asked of it; and plain reduction asks nothing at all ([`ByValue::plainly`]), which is what answers a question and what a shared analysis reads by.
///
/// **What a stuck fold's walk found is kept for the fold above it.** A reduction folds a spine from its leaves, so each operand of a stuck operation was a stuck operation a moment before: its classification is looked up ([`Certifier::classification`]) and extended, and the operation's own is filed where its fold stands, so a long spine is classified once and not once for each level.
fn refolded(certifier: &mut ByValue, value: &Term) -> Result<Option<Term>, ReduceError> {
    if certifier.plain() || !value.has_local_free() {
        return Ok(None);
    }
    let Subterm::Intrinsic(operation) = &**value else {
        return Ok(None);
    };
    // What each operand's own stuck fold found, so the walk extends it rather than walking the operand again.
    let kept = read_operands(operation)
        .iter()
        .map(|operand| certifier.classification(operand))
        .collect::<Vec<_>>();
    let mut within = atoms_within(certifier, operation, &kept)?;
    match within.classable() {
        true => {
            // What a stuck fold puts to conversion: the atoms it pairs, where some two may be one.
            curios_common::sample!("whnf::classable_fold", within.atoms());
            asked_within(certifier, &mut within)?;
        }
        // Nothing is put to conversion, and the readers still class the operation's nodes.
        false => within.open(),
    }
    let (folded, classification) = refold(certifier, operation, within, &kept)?;
    if folded.is_none()
        && let Some(classification) = classification
    {
        certifier.keep_classification(value.clone(), classification);
    }
    Ok(folded)
}

/// `term` with each operand in weak-head normal form, where it is a tagged intrinsic — the form a refinement's reduced spelling and the value probed against it are both held in. Anything else is its own canonical form.
///
/// Each operand is a [`Probe`], as the elaborator's twin reads it: an operand with no value at the type level is kept as written, so a probe that meets one misses rather than refusing the judgment it serves.
pub fn canonical_operands(certifier: &mut ByValue, term: &Term) -> Result<Term, ReduceError> {
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
        operands.push(certifier.whnf(operand.clone()).probed()?.unwrap_or(operand));
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

/// A free variable is the normal form it is: a reading names no `let`-bound local, its door having unfolded each ([`ByValue`]). The one local that unfolds is a member known to terminate, in its group's bodies ([`Calls::member_value`](super::Calls::member_value)).
fn step_var(certifier: &mut ByValue, var: Var) -> Result<Step, ReduceError> {
    // A bound index stops here as a local with no binding does: one reaching here is refused where the module entered (`curios_core::open_positions_of`).
    if let Some(free) = var.as_free() {
        // A member known to terminate unfolds in its group's bodies to its selection out of the known members (`check_group`), read by value as the scope stands here, since an arm inside a body may since have bound a local the selection names; every other local is a neutral.
        if let Some(member) = certifier.calls.member_value(free) {
            return Ok(Step::Continue(certifier.scope.by_value(&member)));
        }
    }

    Ok(Step::Stop(Term::var(var)))
}

/// Beta: open a function's telescope over the arguments applied to it.
///
/// A `rec` head is exposed but not unfolded — the folded spelling stays the normal form of a recursive call, and [`force`] is what demands otherwise.
fn step_apply(certifier: &mut ByValue, apply: Apply) -> Result<Step, ReduceError> {
    let Apply { head, arguments } = apply;

    let head = certifier.whnf(head)?;
    let head = expose_rec_tail(certifier, head)?;

    // A function a folded call's field holds, applied, is the rest of the projection's demand ([`step_proj`]): the path is forced whole, and stays folded where the field's call comes back restuck.
    if matches!(&*head, Subterm::Proj(_)) && head.is_folded() {
        let folded = Term::from(Subterm::Apply(Apply { head, arguments }));

        return Ok(Step::Stop(force(certifier, folded)?));
    }

    Ok(match Term::unwrap_or_clone(head) {
        // Saturation is the precondition of the β step, not an assumption about it: `Telescope::open` asserts on a count mismatch, so an under- or over-applied lambda would abort the walk rather than be refused. An application that does not saturate its lambda is stuck instead, which is the conservative direction — it leaves the term for the typing rules to reject with a diagnostic, and reduction that declines to fire can never admit anything.
        Subterm::Func(Func { telescope, .. }) if telescope.len() == arguments.len() => {
            // The argument ref vector, and what `Telescope::open` costs on top of it: it clones the whole boxed chain and then substitutes once per binder, so an `n`-ary beta step is `n` boxes and `n` passes rather than one.
            certifier.spend(
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
pub fn resolved_spelling(
    certifier: &mut ByValue,
    scrutinee: &Term,
) -> Result<Option<Term>, ReduceError> {
    let mut current = scrutinee.clone();

    // Bounded: each step consumes one application layer of an elaborated dispatch, and a spine that has not settled in the layers both checkers open is not a dispatch.
    for _ in 0..RESOLVED_SPELLING_LAYERS {
        let Subterm::Apply(Apply { head, arguments }) = &*current else {
            return Ok((current != *scrutinee && current.head_key().is_some()).then_some(current));
        };
        let head = certifier.whnf(head.clone())?;
        let Subterm::Func(Func { telescope, .. }) = &*head else {
            return Ok((current != *scrutinee && current.head_key().is_some()).then_some(current));
        };
        if telescope.len() != arguments.len() {
            return Ok((current != *scrutinee && current.head_key().is_some()).then_some(current));
        }

        certifier.spend(
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
/// A slot reads the literal of what it is a slot of: a tuple's slot a tuple literal's field or, through the flat runtime view `(tag, payload...)`, a constructor's payload, where slot `i + 1` is payload component `i`; and a structure's slot a field of that structure's literal, which has no tag to skip. A label that survived to here has no positional meaning yet and stays stuck, as a slot over anything else does: a slot past the literal, or a slot of one thing over a literal of another.
///
/// The rule is stated here for the certifier and in `curios_core::project_literal` for the closed machine and the elaborator, so the two checkers read a slot by two statements of it, held together where both reduce one term (`curios-elab`'s `certifier_agreement_tests`).
fn step_proj(certifier: &mut ByValue, proj: Proj) -> Result<Step, ReduceError> {
    let Proj { head, field } = proj;

    if field.slot().is_none() {
        return Ok(Step::Stop(Term::from(Subterm::Proj(Proj { head, field }))));
    }

    // A projection demands the call it projects, and the demand is judged over the call's path ([`Subterm::called_member`]), not at the record the member answers: forced whole ([`force`]), it is kept where the path got anywhere and stays folded where it restuck. A function the field holds is no progress until it is applied, as a call answering a function is none, so the projection stays folded and its application takes the path on ([`step_apply`]) — taken for progress, `w(a).0(l)` would come back a `match` on `l` that names `w` again, as the elaborator's twin says.
    let head = certifier.whnf(head)?;
    if head.is_folded() {
        let folded = Term::from(Subterm::Proj(Proj { head, field }));
        let forced = force(certifier, folded.clone())?;

        return Ok(Step::Stop(match &*forced {
            Subterm::Func(_) => folded,
            _ => forced,
        }));
    }
    let head = force(certifier, head)?;

    Ok(match slot_of(&head, &field) {
        Some(component) => Step::Continue(component),
        None => Step::Stop(Term::from(Subterm::Proj(Proj { head, field }))),
    })
}

/// The component `field` reads of the literal `head` is, where `head` is one that holds it ([`step_proj`]'s rule).
fn slot_of(head: &Term, field: &Field) -> Option<Term> {
    let component = match (&**head, field) {
        (Subterm::Tuple(Tuple { fields, .. }), Field::Tuple(index)) => fields.get(*index),
        (Subterm::Variant(Variant { payload, .. }), Field::Tuple(index)) => index
            .checked_sub(1)
            .and_then(|component| payload.get(component)),
        (Subterm::Struct(Struct { name, fields, .. }), Field::Struct(structure, index))
            if name == structure =>
        {
            fields.get(*index)
        }
        _ => None,
    };

    component.cloned()
}

/// Eta for functions: `(x) => f(x)` is `f`, provided each argument stands under its binder's mark and `f` does not itself mention `x`. Across a mark the two are functions at two types plicity holds apart, so `(@x) => f(x)` stays as written. The closed machine and the elaborator contract through `curios_core::eta_head`, and this is the certifier's own statement of the rule, so a mistake in either shows where the two checkers reduce one term.
///
/// Contracting here rather than only at conversion means the two spellings have one normal form, so every consumer of a weak-head normal form sees them as the same term without having to know the rule.
fn step_func(certifier: &mut ByValue, func: Func) -> Result<Step, ReduceError> {
    let arity = func.telescope.len();

    // Three arity-sized vectors — the probe binders, their occurrences, and the refs handed to `open` — plus the opening itself. Charged even though the probe usually fails, because the probe is what allocates.
    certifier.spend(
        Cost::collection(arity as u64)
            .saturating_mul(3)
            .saturating_add(Cost::term(1).saturating_mul(arity as u64)),
    )?;

    let binders = (0..arity)
        .map(|_| certifier.fresh(None))
        .collect::<Vec<_>>();
    let occurrences = binders.iter().map(Term::free_var).collect::<Vec<_>>();
    let refs = occurrences.iter().collect::<Vec<_>>();

    Ok(match Term::unwrap_or_clone(func.telescope.open(&refs)) {
        Subterm::Apply(Apply { head, arguments })
            if arguments.len() == arity
                && arguments
                    .iter()
                    .zip(&binders)
                    .zip(func.telescope.members())
                    .all(|((argument, binder), (mark, _))| {
                        argument.plicity == mark
                            && matches!(argument.term.as_ref(), Subterm::Var(var) if var.as_free() == Some(binder))
                    })
                && binders.iter().all(|binder| !head.mentions_free(binder)) =>
        {
            Step::Continue(head)
        }
        _ => Step::Stop(Term::from(Subterm::Func(func))),
    })
}

/// Zeta: substitute a `let`'s bindings into its tail.
///
/// The elaborator's reducer substitutes by the same rule. Binding each value as a fresh definition and opening the tail over *those* would avoid copying a value into every use, but spell reducts with names the other checker's reduction does not have. A substitution is visibly the rule, and an environment is a second place a variable's meaning can come from. Bindings are non-recursive and bind left to right, so binding `i` sees exactly the values before it.
fn step_let(certifier: &mut ByValue, let_: Let) -> Result<Term, ReduceError> {
    // One values vector, and a fresh ref vector at every binding — so the ref vectors together are triangular in the run's length, which the surface language makes as long as a program likes.
    let bindings = let_.bindings().len() as u64;
    certifier.spend(
        Cost::collection(bindings)
            .saturating_add(Cost::buffer(
                bindings.saturating_mul(bindings.saturating_add(1)) / 2,
            ))
            .saturating_add(Cost::term(1).saturating_mul(bindings)),
    )?;

    let mut values: Vec<Term> = Vec::with_capacity(let_.bindings().len());

    for binding in let_.bindings() {
        let refs = values.iter().collect::<Vec<_>>();
        values.push(binding.value().release(&refs));
    }

    let refs = values.iter().collect::<Vec<_>>();

    Ok(let_.tail().open(&refs))
}

/// Delta: unfold a definition at the instance its reference states, which is the one position a definition unfolds from, and only where the instance states its scheme whole ([`curios_core::UniverseContext::stated_by`]); otherwise the reference is the normal form it is.
fn step_instance(certifier: &mut ByValue, instance: Instance) -> Result<Step, ReduceError> {
    let Instance { name, universes } = instance;

    let reduct = match certifier.value_at(&name, &universes).cloned() {
        Some(reduct) => reduct,
        None => return Ok(Step::Stop(Term::instance(name, universes))),
    };

    // A stored value holding a projection needs no special case: a group binds no universe, so the levels its members spell are this declaration's and one walk substitutes them with every other level the instance states.
    Ok(Step::Continue(
        instantiate_universes(&reduct, &universes).map_err(ReduceError::Universe)?,
    ))
}

/// The scrutinee `forced` as the elimination `cases` reads it, where the carriers' algebra decides more of it than the fold did (`curios_analysis::read_scrutinee`): a `Bool` connective as the literal its decision diagram is, a structural `Nat` as the successor its stuck products distribute to, and `forced` itself where the algebra decides nothing.
///
/// **An elimination reads its scrutinee as conversion does.** Conversion holds the scrutinee equal to that value and is a congruence, so it holds the match equal to the value's arm, and a match left stuck there would be refused against its arm where two steps of conversion hold the two one. Both reductions take it, the diagram and the distribution asking no judgment, as a fold asks none. The elaborator's reducer reads at the same point by the same rule.
///
/// **Where the diagram hands back atoms that may be one, the certifier's conversion classes them** and the connective is read again over the classes, once for both literals: the third point reduction asks the certifier's conversion, under a stuck fold's two conditions ([`refolded`]) — only of a scrutinee that names a local, and never by plain reduction.
fn scrutinee_read(
    certifier: &mut ByValue,
    cases: &Cases,
    forced: Term,
) -> Result<Term, ReduceError> {
    Ok(match read_scrutinee(certifier, cases, &forced)? {
        ScrutineeRead::Value(value) => value,
        ScrutineeRead::Atoms(atoms) if !certifier.plain() && forced.has_local_free() => {
            let mut classes = asked_classes(certifier, &atoms)?;
            read_scrutinee_classed(certifier, &forced, &mut classes)?.unwrap_or(forced)
        }
        ScrutineeRead::Silent | ScrutineeRead::Atoms(_) => forced,
    })
}

/// Iota: dispatch a `match` on the value `forced` its scrutinee reduced to.
///
/// An arm binds that value's payload components directly. They are themselves unreduced — a `Variant` is a weak-head normal form whose sub-terms this strategy never entered — so binding them is call-by-name, not call-by-value.
///
/// The elaborator instead binds each arm to a *projection of the scrutinee as written*, because a reduced payload can carry annotation holes its zonker would then have to solve. The certifier has no zonker and no holes, so it takes the direct route.
fn step_match(forced: Term, goal: Term, cases: Cases) -> Step {
    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => match forced.as_bool() {
            Some(false) => Step::Continue(false_case),
            Some(true) => Step::Continue(true_case),
            None => Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                goal,
                cases: Cases::Bool {
                    false_case,
                    true_case,
                },
            }))),
        },

        // A literal `Nat` is a floor over a `Zero` inner, so a zero inner is exactly "this is a concrete `k`": it takes its case, or the default when no case names it. A symbolic inner over a floor past every key is no key whatever it stands for, so it takes the default too; anything else rebuilds the neutral switch. Stated apart from `curios_core::switch_arm`, which the closed machine and the elaborator read.
        Cases::Switch {
            keys: Keys::Nat(keyed),
            default,
        } => {
            let arms = keyed.arms();
            let (floor, inner) = Nat::decompose(&forced);

            match (
                Nat::is_zero(&inner),
                arms.iter().all(|(key, _)| *key < floor),
            ) {
                (true, _) => Step::Continue(
                    arms.iter()
                        .find(|(key, _)| *key == floor)
                        .map(|(_, body)| body)
                        .unwrap_or(&default)
                        .clone(),
                ),
                (false, true) => Step::Continue(default),
                (false, false) => Step::Stop(Term::from(Subterm::Match(Match {
                    head: forced,
                    goal,
                    cases: Cases::Switch {
                        keys: Keys::Nat(keyed),
                        default,
                    },
                }))),
            }
        }

        // An `Int` literal takes its key's case, or the default when no key names it; an integer that is no literal is stuck whatever its sum, since no floor bounds it.
        Cases::Switch {
            keys: Keys::Int(keyed),
            default,
        } => match &*forced {
            Subterm::Intrinsic(Intrinsic::Int(value)) => Step::Continue(
                keyed
                    .arms()
                    .iter()
                    .find(|(key, _)| key == value)
                    .map(|(_, body)| body)
                    .unwrap_or(&default)
                    .clone(),
            ),
            _ => Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                goal,
                cases: Cases::Switch {
                    keys: Keys::Int(keyed),
                    default,
                },
            }))),
        },

        // A `Byte` literal takes its key's case, or the default when no key names it; a byte that is no literal is stuck.
        Cases::Switch {
            keys: Keys::Byte(keyed),
            default,
        } => match &*forced {
            Subterm::Intrinsic(Intrinsic::Byte(value)) => Step::Continue(
                keyed
                    .arms()
                    .iter()
                    .find(|(key, _)| key == value)
                    .map(|(_, body)| body)
                    .unwrap_or(&default)
                    .clone(),
            ),
            _ => Step::Stop(Term::from(Subterm::Match(Match {
                head: forced,
                goal,
                cases: Cases::Switch {
                    keys: Keys::Byte(keyed),
                    default,
                },
            }))),
        },

        Cases::Induct {
            cases,
            default,
            stated,
        } => {
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
                goal,
                cases: Cases::Induct {
                    cases,
                    default,
                    stated,
                },
            })))
        }

        // The case split of a native free-monoid carrier (`Nat`/`Bytes`/`List`). `FreeMonoid::uncons` owns the carrier-specific one-step decode; the cons arm binds the peeled generator (absent for the unary `Nat`) and the tail.
        Cases::FreeMonoid { carrier } => {
            let layer = match &carrier {
                Carrier::Nat { .. } => FreeMonoid::Unary,
                Carrier::Bytes { .. } => FreeMonoid::Bytes,
                Carrier::List { .. } => FreeMonoid::List,
            }
            .uncons(Term::unwrap_or_clone(forced));

            match layer {
                Layer::Empty => Step::Continue(match carrier {
                    Carrier::Nat { empty_case, .. }
                    | Carrier::Bytes { empty_case, .. }
                    | Carrier::List { empty_case, .. } => empty_case,
                }),
                Layer::Cons { head: elem, tail } => Step::Continue(match &carrier {
                    Carrier::Nat { cons_case, .. } => cons_case.open(&[&tail]),
                    Carrier::Bytes { cons_case, .. } | Carrier::List { cons_case, .. } => cons_case
                        .open(&[
                            elem.as_ref()
                                .expect("a Bytes/List cons layer carries a head"),
                            &tail,
                        ]),
                }),
                Layer::Stuck(stuck) => Step::Stop(Term::from(Subterm::Match(Match {
                    head: stuck.into(),
                    goal,
                    cases: Cases::FreeMonoid { carrier },
                }))),
            }
        }
    }
}

/// Strip `rec` binding syntax without unfolding a member's fixed point, leaving the projection that `rec f = ...; f` denotes.
fn expose_rec_tail(certifier: &mut ByValue, term: Term) -> Result<Term, ReduceError> {
    let mut term = term;

    loop {
        // A projection already *is* the member it denotes: opening its tail over the group yields the same term, so this is where stripping stops rather than a step it could take.
        if term.as_rec_proj().is_some() {
            return Ok(term);
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => term = certifier.whnf(unfold_rec(rec))?,
            other => return Ok(other.into()),
        }
    }
}

/// Open a `rec` group's tail over its members. A pure binder operation: it mints nothing and unfolds no fixed point.
pub fn unfold_rec(rec: Rec) -> Term {
    let members = rec.group().members();
    let refs = members.iter().collect::<Vec<_>>();

    rec.tail().open(&refs)
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
fn force(certifier: &mut ByValue, term: Term) -> Result<Term, ReduceError> {
    // A closed term takes the machine at the eliminator's demand; the recursive loop below is the strategy for everything the gate declines.
    if machine_admissible(certifier, &term) {
        return closed_run(certifier, term, Demand::Forced);
    }

    let folded = term.clone();
    let mut term = term;

    loop {
        certifier.spend(Cost::STEP)?;

        // Unfolding a projection means stepping to the member's body; unfolding any other `rec` means opening its tail. Both are the same rule read at the two tail shapes.
        if let Some((group, index)) = term.as_rec_proj() {
            let body = group.member_body(index);
            term = certifier.whnf(body)?;
            continue;
        }

        match Term::unwrap_or_clone(term) {
            Subterm::Rec(rec) => term = certifier.whnf(unfold_rec(rec))?,
            // An application no unfolding opens is stuck as a `match` on a variable is: kept where the recursion is finished, its group gone from it, and the folded call otherwise.
            Subterm::Apply(apply) => match unfold_rec_apply(certifier, apply.clone())? {
                Some(unfolded) => term = certifier.whnf(unfolded)?,
                None => return finished(certifier, folded, Subterm::Apply(apply).into()),
            },
            // A projection of a folded call is that call's path, unfolded and kept by the same test; any other projection no unfolding opens is stuck.
            Subterm::Proj(proj) => match unfold_rec_proj(certifier, proj.clone())? {
                Some(unfolded) => term = certifier.whnf(unfolded)?,
                None => return finished(certifier, folded, Subterm::Proj(proj).into()),
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

                return finished(certifier, folded, value);
            }
        }
    }
}

/// What [`force`] keeps of a neutral `value` the unfolding of `folded` reached: `value` where the recursion is finished, no member of the group `folded` is a call on standing in it, and `folded` otherwise.
fn finished(certifier: &mut ByValue, folded: Term, value: Term) -> Result<Term, ReduceError> {
    Ok(match forced_group(certifier, &folded)? {
        Some(group) if value.mentions_rec_member(&group) => folded,
        _ => value,
    })
}

/// Whether one unfolding of the folded call `folded`, at its weak-head form `reduct`, re-sticks: a stuck form, no folded call, that still mentions the group `folded` is a call on — the test [`force`] keeps a restuck call folded by, asked of one step. A head constructor, a reduct free of the group, or another folded call is an unfolding that took its match apart.
pub fn restuck(certifier: &mut ByValue, folded: &Term, reduct: &Term) -> Result<bool, ReduceError> {
    // An application or a projection no unfolding opens is stuck as a `match` on a variable is, as `force` reads one.
    let stuck = !reduct.is_folded()
        && matches!(
            &**reduct,
            Subterm::Match(_)
                | Subterm::Var(_)
                | Subterm::Metavar(_)
                | Subterm::Proj(_)
                | Subterm::Apply(_)
        );

    Ok(stuck
        && forced_group(certifier, folded)?.is_some_and(|group| reduct.mentions_rec_member(&group)))
}

/// The group `folded` denotes a call on, which is what [`force`] asks its occurrence question about.
///
/// Read off the term three ways because a folded call has three spellings: a member selection carries the group on the projection, a `rec` value carries it directly, and an application or a field's projection carries it at the root of its call's path — the same place [`unfold_rec_apply`] and [`unfold_rec_proj`] read it, reached again through the evaluation memo rather than by a fresh walk. A term that denotes no recursive call answers `None`, and nothing can restick in it.
fn forced_group(certifier: &mut ByValue, folded: &Term) -> Result<Option<RecGroup>, ReduceError> {
    // A path whose root is reduced already names its group: a projection's head reduced further would be forced to the record the call answers, and name none.
    if let Some((group, _)) = folded.called_member() {
        return Ok(Some(group.clone()));
    }

    let head = match &**folded {
        Subterm::Rec(rec) => return Ok(Some(rec.group().clone())),
        Subterm::Apply(Apply { head, .. }) => head,
        Subterm::Proj(Proj { head, field }) if field.slot().is_some() => head,
        _ => return Ok(None),
    };
    let head = certifier.whnf(head.clone())?;
    let head = expose_rec_tail(certifier, head)?;

    Ok(head.called_member().map(|(group, _)| group.clone()))
}

/// The one definitional unfolding [`force`] withholds: a folded recursive spelling — a `rec` value, a member selection, or a member application — stepped to the weak-head form of its body. `None` for every other shape.
///
/// `force` keeps the folded spelling as a recursive call's normal form, while the same call reached by unfolding is the member's body at the argument, a match stuck where the argument is a variable; conversion consults this to see the two spellings as one.
pub fn unfold_spelling(certifier: &mut ByValue, term: &Term) -> Result<Option<Term>, ReduceError> {
    if let Some((group, index)) = term.as_rec_proj() {
        let body = group.member_body(index);

        return Ok(Some(certifier.whnf(body)?));
    }

    let unfolded = match &**term {
        Subterm::Rec(rec) => return Ok(Some(certifier.whnf(unfold_rec(rec.clone()))?)),
        Subterm::Apply(apply) => unfold_rec_apply(certifier, apply.clone())?,
        Subterm::Proj(proj) => unfold_rec_proj(certifier, proj.clone())?,
        _ => None,
    };

    match unfolded {
        Some(unfolded) => Ok(Some(certifier.whnf(unfolded)?)),
        None => Ok(None),
    }
}

/// Unfold one folded recursive application, when its result shape is demanded.
fn unfold_rec_apply(certifier: &mut ByValue, apply: Apply) -> Result<Option<Term>, ReduceError> {
    let Apply { head, arguments } = apply;

    let head = certifier.whnf(head)?;
    let head = expose_rec_tail(certifier, head)?;

    // A projection is the shape a *recursive* member keeps: opening the group's tail over its own members reproduces it, which is where `expose_rec_tail` stops. A member that does not occur in its own body has no fixed point to keep, so the same opening reduces past the projection to the member's value, and the applicable term is then the exposed head itself. Both are the one beta step this function exists to take, and the elaborator's twin takes them the same way — an `induct`'s type constructor lowers into a `rec` whatever its arity, since otherwise a caller reaching the unfolded spelling would see a nominal type where one reaching the folded spelling sees a stuck application.
    let body = match head.as_rec_proj() {
        Some((group, index)) => {
            let body = certifier.whnf(group.member_body(index))?;

            force(certifier, body)?
        }
        None => head,
    };

    let telescope = match Term::unwrap_or_clone(body) {
        Subterm::Func(Func { telescope, .. }) => telescope,
        // The head is itself a folded call: a member whose result is a function, applied past its own parameters. Unfolding that call one step and applying what it becomes to what is left is this application's one step, as the elaborator's twin takes it — read one level deep, `f(a)(b)` would be a neutral no demand unfolds.
        Subterm::Apply(inner) => {
            return Ok(unfold_rec_apply(certifier, inner)?.map(|unfolded| {
                Subterm::Apply(Apply {
                    head: unfolded,
                    arguments,
                })
                .into()
            }));
        }
        // The head is a folded call projected by a field: a member whose result is a record, its field applied, as a recursive witness's method is called. Unfolding the projected call one step and applying what the field becomes is this application's one step, as the elaborator's twin takes it.
        Subterm::Proj(inner) => {
            return Ok(unfold_rec_proj(certifier, inner)?.map(|unfolded| {
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

/// Unfold one folded call projected by a field: the call unfolded one step, and the field of the record it becomes. `None` where the projection is of no folded call, or the step exposes no record that holds the field.
fn unfold_rec_proj(certifier: &mut ByValue, proj: Proj) -> Result<Option<Term>, ReduceError> {
    let Proj { head, field } = proj;
    if field.slot().is_none() {
        return Ok(None);
    }
    let head = certifier.whnf(head)?;
    let head = expose_rec_tail(certifier, head)?;

    let unfolded = match head.as_rec_proj() {
        Some((group, index)) => certifier.whnf(group.member_body(index))?,
        None => {
            let step = match Term::unwrap_or_clone(head) {
                Subterm::Apply(inner) => unfold_rec_apply(certifier, inner)?,
                Subterm::Proj(inner) => unfold_rec_proj(certifier, inner)?,
                _ => None,
            };
            match step {
                Some(step) => certifier.whnf(step)?,
                None => return Ok(None),
            }
        }
    };

    Ok(slot_of(&force(certifier, unfolded)?, &field))
}
