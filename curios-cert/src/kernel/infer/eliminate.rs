//! Verifying an elimination: that each arm inhabits the motive at its own case, and that a proposition is not eliminated into a relevant result.
//!
//! An elimination is the only term form whose *type* says nothing about whether it is sound. `infer` reads the result off the motive, and the motive is whatever the term claims — so the whole content of the rule is here.
//!
//! # The arm rule
//!
//! For each constructor, the arm body must inhabit the motive **at that constructor's own index targets**, not at the scrutinee's. `Vec/nil` targets index `0` and `Vec/cons` targets `succ(n)`, so the two arms are checked against two different instances of the motive, and that is exactly what makes a dependent elimination worth having. Checking every arm at the scrutinee's indices would be both wrong and useless.
//!
//! # Context specialization
//!
//! Opening the motive at the case's targets teaches the *goal* the case's equations, and nothing else: the ambient locals, the body's occurrences of outer variables, and the scrutinee variable itself would all stay at their unrefined types. The other half of the rule is [`specialize`]: the arm is checked in a context specialized by the most-general solution of `actual indices ~ case targets` (plus `scrutinee ~ constructed value` when the scrutinee is a variable). Definitional K — `Eq : Prop` plus proof irrelevance, recorded permanent in `documentation/design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md` — is the license for solving those equations by first-order unification and substituting.
//!
//! Both directions run the *shared* unifier from [`curios_analysis::invert_indices`]. Pinning an arm binder to the rigid actual it must equal is the call as the elaborator makes it; refining an outer variable to the target it must equal is the same call with its sides swapped, and the swap lands the guards exactly right — the occurs check refuses the parameter cycle (`b := b + 1` through a family parameter), and the top guard leaves the variable-variable case to the first direction. Both directions and their composition are one shared function, [`curios_analysis::solve_indices`], which the elaborator calls too: it records the solution in its refinement store, while the kernel holds no store, so it substitutes into the arm instead. Which locals the solution re-types, and at what type, is shared as well ([`curios_analysis::retyped`]), and so is the scrutinee's own solution ([`curios_analysis::scrutinee_solution`]); the existing `mark`/`retract` bracket scopes the re-typed entries exactly to the arm.
//!
//! # The large-elimination guard
//!
//! Proof irrelevance says any two inhabitants of a proposition are interchangeable. If a program could eliminate a proposition into a relevant result, it could extract *which* proof it received, and the two facts together are inconsistent. So the elimination is allowed only when the proposition carries no information to extract: when it has no constructors at all, or exactly one whose every payload component is already determined.
//!
//! "Already determined" is the load-bearing phrase, and [`wholly_pinned`] is where it is decided, over the walk `curios_analysis::pinned_by_targets` shares with erasure. A component is determined when the constructor's index targets *pin* it — when matching the target against a value recovers the component. Occurring in a target is not the same thing: `mk(a : Nat) : (blur(a))` mentions `a` in its index, but `blur` is an arbitrary function and knowing `blur(a)` recovers nothing. Reading occurrence as determination is precisely how a proposition with a real payload gets eliminated into a relevant type, and from there `False` follows.

#[cfg(test)]
mod ambient_tests;
#[cfg(test)]
mod fold_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;

use {
    super::check,
    crate::{
        Counted, Error, InductAt, Kernel, Sort, another_case, retyped, scrutinee_solution,
        solve_indices, wholly_pinned,
    },
    curios_analysis::Invert,
    curios_core::{
        Atom, Bound, Exhaustion, Free, InductArm, InductType, MatchResult, Probe, ReduceError,
        Reducer, Subterm, Telescope, Term, Variant,
    },
};

/// Check every arm of an elimination of `scrutinee_type` at `result`.
pub(super) fn check_induct_arms(
    kernel: &mut Kernel,
    at: &InductAt,
    family: &InductType,
    result: &MatchResult,
    cases: &[(Atom, InductArm)],
    default: Option<&Term>,
    scrutinee: &Term,
) -> Result<(), Error> {
    for (tag, arm) in cases {
        check_arm(kernel, at, family, result, scrutinee, tag, arm)?;
    }

    // A catch-all binds nothing and stands for the scrutinee itself, so it is checked at the scrutinee's own indices *and at the scrutinee* — the one arm with no case value of its own, and therefore the one whose instance can only come from the term being eliminated. That is the instance `infer` reads the elimination's type off, so any other one proves something other than what the elimination hands its caller.
    if let Some(default) = default {
        check(kernel, default, &result.of(scrutinee, &family.indices))?;

        return Ok(());
    }

    // Coverage: an absent arm must justify its absence. With no catch-all, every constructor with no arm must be *impossible* at the scrutinee's indices — its targets must clash with the actuals, decided by the call of the shared unifier that specializes the present arms, both its directions, so an arm may be absent exactly where one written is typed by no rule. A case the unifier merely cannot decide is a refusal, not a pass: undecided is not absent.
    for (tag, _) in &at.declaration().constructors {
        if cases.iter().any(|(present, _)| present == tag) {
            continue;
        }

        let signature = at
            .signature(tag)
            .ok_or_else(|| Error::Undeclared(family.name))?;

        let outcome = kernel.scoped(|kernel| {
            open_payload(kernel, signature, |kernel, binders, _payload, targets| {
                solve_indices(kernel, &family.indices, targets, binders)
            })
        });

        if !matches!(outcome?, Invert::Impossible) {
            return Err(Error::MissingArm {
                family: family.name,
                tag: tag.clone(),
            });
        }
    }

    Ok(())
}

/// One arm: open the constructor's payload under fresh binders at its declared field types, specialize the context by this case's forced equations, then require the body to inhabit the motive at this constructor's index targets and at the value it constructs.
fn check_arm(
    kernel: &mut Kernel,
    at: &InductAt,
    family: &InductType,
    result: &MatchResult,
    scrutinee: &Term,
    tag: &Atom,
    arm: &InductArm,
) -> Result<(), Error> {
    let signature = at
        .signature(tag)
        .ok_or_else(|| Error::Undeclared(family.name))?;

    if signature.len() != arm.arity() {
        return Err(Error::Arity {
            counted: Counted::ArmBinders,
            expected: signature.len(),
            actual: arm.arity(),
        });
    }
    // An arm binds each payload under the payload's own mark, as an argument stands under its parameter's.
    for (position, (declared, written)) in signature.marks().iter().zip(arm.plicities()).enumerate()
    {
        if declared != written {
            return Err(Error::Mark {
                counted: Counted::ArmBinders,
                position: position + 1,
                declared: *declared,
                written: *written,
            });
        }
    }

    let declared = signature.proofs();

    kernel.scoped(|kernel| {
        open_payload(kernel, signature, |kernel, binders, payload, targets| {
            // An arm binds as written only what its constructor's payload says is a proof, which the declaration was held to where it was checked.
            for (position, binder) in binders.iter().enumerate() {
                if arm.body.binds_proof(position) && !declared[position] {
                    let domain = kernel
                        .local_type(binder)
                        .cloned()
                        .expect("a payload binder is assumed at its type");

                    return Err(Error::NoProof(domain));
                }
            }

            // The forced equations of this case, as one substitution. Where they clash no value of the scrutinee's type is this case, so no reduction takes the arm and it is not typed: an arm a reader wrote there is the elaborator's to refuse, and one a substitution left there was typed where its indices were open.
            let Some(mut solutions) = specialize(kernel, family, targets, binders)? else {
                return Ok(());
            };

            // The value this arm's scrutinee is: the constructor at its payload.
            let value: Term = Subterm::Variant(Variant {
                name: family.name,
                universes: family.universes.clone(),
                params: family.params.clone(),
                tag: tag.clone(),
                payload: payload.to_vec(),
            })
            .into();

            if never_taken(kernel, scrutinee, &value, binders)? {
                return Ok(());
            }

            let mut assumed = Vec::new();
            let checked = kernel.tried(|kernel| {
                assumed = assume_case_value(kernel, scrutinee, &value, &mut solutions)?;
                // What the call recorder grades under within the arm: the equations just put in force, and the payload binders — an application of one reads as the payload it came from.
                kernel.assume_arm(scrutinee, &value, &solutions)?;
                kernel.assume_payloads(binders)?;

                let refs = payload.iter().collect::<Vec<_>>();
                let body = arm.open(&refs).substitute(&solutions);

                let expected = result
                    .at(scrutinee, &family.indices, targets, &value)
                    .substitute(&solutions);

                shadow(kernel, &solutions);

                check(kernel, &body, &expected)
            });

            unless_never_taken(kernel, checked, &assumed, binders)
        })
    })
}

/// A refused arm's verdict, withdrawn where the arm is one no reduction takes: an equation it assumed, read with the arm's bracket closed, computes to another case than it states.
///
/// **The other way an arm taken once is accepted.** An arm is typed under what its case says of the scrutinee, and where that is false the arm is never reached: a scrutinee that reduces to a value of another case, under the equations around the arm, is that value wherever the arm would be taken, so reduction opens another arm and conversion compares none of this one. The two ways are each sound alone, and the order they are tried in decides what a verdict costs: nothing is computed for an arm that types.
///
/// **Why an arm that typed where it was written is refused here.** The elaborator writes a type it derived by putting a value in a variable's place — a `let`'s annotation, a lambda's domain, a match's goal — and types none of them. An arm of such a type was typed under an equation over the variable; at the value its scrutinee names no local, so no equation is recorded for it ([`records_case_equation`](curios_analysis::records_case_equation)), or it is one an inner arm's solution closed. Where the scrutinee computes to the arm's own case, what rested on the equation holds by reduction, and where it computes to another the arm is this one.
///
/// **`assumed` is every equation the arm assumed**, its own and those its solution restated ([`assume_case_value`]), each computed where the arm's own equations no longer answer: under its own equation a scrutinee is its case, and would refute nothing.
///
/// **A spent budget is never withdrawn**, from the arm or from the reduction asked here, and a scrutinee with no value at the type level refutes nothing ([`Probe`]). What the refused arm recorded is taken back by the bracket it was typed in ([`Kernel::tried`]), so an arm withdrawn is one that was not typed.
pub(super) fn unless_never_taken(
    kernel: &mut Kernel,
    checked: Result<(), Error>,
    assumed: &[(Term, Term)],
    binders: &[Free],
) -> Result<(), Error> {
    let Err(refusal) = checked else {
        return Ok(());
    };
    if refusal.is_exhausted() {
        return Err(refusal);
    }

    for (scrutinee, case) in assumed {
        let Some(reduced) = kernel.reduce_forced(scrutinee.clone()).probed()? else {
            continue;
        };
        if another_case(kernel, &reduced, case, binders)? {
            return Ok(());
        }
    }

    Err(refusal)
}

/// Whether the arm of a case split at `value` is one reduction never takes: the scrutinee already is a literal or a constructed value of another case than `value` ([`another_case`]). Such an arm is not typed.
///
/// **Nothing reads it.** A value is its case under every substitution, so a case split over one is its one arm wherever it stands: reduction opens no other, and conversion compares none, since the elimination is never stuck. Typing the arm would establish a term no rule reads, and would refuse one that was typed already: an arm is typed under the equation between its scrutinee and its case, an equation between two values is recorded nowhere ([`records_case_equation`](curios_analysis::records_case_equation)), and the result an arm is checked at puts a case in its scrutinee's place through the goal ([`MatchResult::at`]), an elimination of that scrutinee inside the goal among it. A proof that stood under that elimination's other arm, resting on its guard, then stands under a literal.
///
/// **A fold is never asked.** Its reduct holds the fold at the tail, so a value of either case takes the identity arm in the end: `match 3 : (_) => P | 0 => … | k + 1; ih => ih end` is its identity arm, which skipped would inhabit any `P` untyped. The callers ask for an arm taken once, at the scrutinee itself.
///
/// **The scrutinee is read as it stands and not computed here.** Computing it would decide more — an elimination over `0 < 0` is its `false` arm — and typing must not evaluate a term that runs to accept what it accepts without: that is asked only of an arm that was refused ([`unless_never_taken`]). The shared [`another_case`] answers of a term as it stands.
pub(super) fn never_taken(
    kernel: &mut Kernel,
    scrutinee: &Term,
    value: &Term,
    binders: &[Free],
) -> Result<bool, Error> {
    let scrutinee = kernel.by_value(scrutinee);

    another_case(kernel, &scrutinee, value, binders)
}

/// Teach an arm that its scrutinee **is** this case's value, which is what specializes the context the body is checked in.
///
/// A variable scrutinee becomes a solution the arm is substituted through ([`scrutinee_solution`], which the elaborator's arms read too) — for a nominal arm the zero-index instance of the same index equations, and for an intrinsic carrier the whole of the refinement it gets. Any other scrutinee has no binder to solve, so the equation is recorded against its written spelling for the reducer to consult instead.
///
/// **Recording costs nothing: no reduction happens at registration.** `Scope::refine` and `whnf`'s `refined_reduct` carry a two-tier key, and reduce at most once per equation, only when a probe presents a term the written spelling does not answer. Reducing the scrutinee here, once per arm, merely to obtain a key would unfold a web of combinator definitions each naming the one before it twice exponentially before a single arm was checked — the scrutinee mentions a local, which is exactly the term the evaluation memos may not store — where the elaborator, registering on the written spelling, checks the same program flat.
///
/// Every scrutinee of an arm taken once gets its equation, because a term of non-`Io` type denotes one value: an `Io` is opaque and cannot be eliminated, so it never reaches a scrutinee position, and no inhabitant of an ordinary arrow performs an effect.
///
/// Stated once because the three arm rules that need it — nominal, boolean-and-dispatch, and a free-monoid case split — were three chances to state it differently, and what a case teaches its arm is precisely what coverage and obligation (V) read back out.
///
/// Appends to `solutions` rather than replacing them, so [`check_arm`] can hand over the index equations it has already solved; `value` is substituted through those first, since a case value built from the constructor's payload may mention a binder they pinned. Must be called inside the arm's [`Kernel::scoped`] bracket — that bracket is what scopes the refinement to the arm.
///
/// **The solution reaches the equations already in force.** An arm's body and expectation are substituted, so an equation an outer arm recorded over a variable this case solves would be keyed on a term the arm no longer holds; [`Kernel::restate_refinements`] assumes each such equation again at the solution, for the arm and no longer.
///
/// **Answers every equation the arm assumes**, each as its scrutinee and the case it states: its own, where the scrutinee is no variable, and those restated under its solution, recorded or not. They are what [`unless_never_taken`] puts to reduction where the arm is refused.
pub(super) fn assume_case_value(
    kernel: &mut Kernel,
    scrutinee: &Term,
    value: &Term,
    solutions: &mut Vec<(Free, Term)>,
) -> Result<Vec<(Term, Term)>, ReduceError> {
    let value = value.substitute(solutions);
    let mut assumed = Vec::new();

    match scrutinee_solution(&*kernel, scrutinee, &value) {
        Some(solution) => solutions.push(solution),
        None => {
            assumed.push((scrutinee.clone(), value.clone()));
            kernel.refine(scrutinee.clone(), value)?;
        }
    }
    // The arm is checked with `solutions` substituted through it, so every equation in force — the one just assumed among them, where the index equations solved a variable its scrutinee names — is restated under them.
    assumed.extend(kernel.restate_refinements(solutions));

    Ok(assumed)
}

/// The most-general solution of `actual indices ~ case targets`, both directions, as one idempotent substitution — the shared [`solve_indices`], which carries the rule. Empty when the equations force nothing, and `None` where they *clash*: the case is one no value of the scrutinee's type is.
fn specialize(
    kernel: &mut Kernel,
    family: &InductType,
    targets: &[Term],
    binders: &[Free],
) -> Result<Option<Vec<(Free, Term)>>, Error> {
    Ok(
        match solve_indices(kernel, &family.indices, targets, binders)? {
            Invert::Impossible => None,
            Invert::Solved(solutions) => Some(solutions),
        },
    )
}

/// Re-assume every local the solution re-types at its specialized type — the shared [`retyped`], which the elaborator's arms apply too — and bind again every `let`-bound local the solution moves. The shadow is what a lookup finds, and the enclosing `mark`/`retract` bracket retracts it with the arm.
///
/// **A `let`-bound local is moved where its value, read by value, names a solved variable.** The arm's body and expectation have the solution substituted through them, and a `let` substituted into either would have had it substituted through its value too. Bound, the local a body names still stands for the value as it was outside the arm; bound again, it stands for what the substitution would have left in its place. One the solution only re-types is bound again at the value it had — assumed, it would stand for nothing.
///
/// **Bound again from the value as written, in scope order.** A `let`'s value names the `let`s before it, so with those already bound again its own value is its written one with the solution substituted, read by value where they now stand. Substituting through each value by value instead would walk, for every `let` in scope, everything beneath it — the square of a chain of `let`s at every arm.
pub(super) fn shadow(kernel: &mut Kernel, solutions: &[(Free, Term)]) {
    if solutions.is_empty() {
        return;
    }

    let locals = kernel
        .local_names()
        .into_iter()
        .zip(kernel.local_types())
        .collect::<Vec<_>>();
    let retyped = retyped(&*kernel, &locals, solutions);
    let bound = kernel.bound_locals();

    for (name, type_) in &retyped {
        if !bound.iter().any(|local| local.name == *name) {
            kernel.assume(name, type_);
        }
    }
    for local in &bound {
        // The innermost of the name's re-typings, which is the one a lookup finds.
        let specialized = retyped
            .iter()
            .rev()
            .find(|(name, _)| *name == local.name)
            .map(|(_, type_)| type_);
        let moved = solutions
            .iter()
            .any(|(solved, _)| local.value.mentions_free(solved));

        if moved || specialized.is_some() {
            kernel.bind(
                &local.name,
                specialized.unwrap_or(&local.type_),
                &local.written.substitute(solutions),
                local.encloses_partial,
            );
        }
    }
}

/// Open a constructor signature's payload binders into scope, hand the binder names, the occurrences, and the constructed terminal to `body`.
fn open_payload<T, B: Bound>(
    kernel: &mut Kernel,
    signature: Telescope<B>,
    body: impl FnOnce(&mut Kernel, &[Free], &[Term], &B) -> Result<T, Error>,
) -> Result<T, Error> {
    let mut binders = Vec::new();
    let mut cursor = signature.cursor();

    while let Some((_, field)) = cursor.entry() {
        binders.push(kernel.advance_assumed(&mut cursor, &field));
    }

    let constructed = cursor.body().expect("a cursor past every entry");
    let payload = cursor.into_args();

    body(kernel, &binders, &payload, &constructed)
}

/// Refuse eliminating a proposition into a relevant result unless the family declares no constructor, or one whose every payload its index targets pin.
///
/// The guard fires only when both halves hold: the scrutinee's family is `Prop`-sorted, and the motive lands in `Type`. A proposition eliminated into another proposition is always fine — irrelevance makes the result indistinguishable either way.
pub(super) fn guard_large_elimination(
    kernel: &mut Kernel,
    at: &InductAt,
    family: &InductType,
    motive_sort: Sort,
) -> Result<(), Error> {
    let scrutinee_type: Term = Subterm::InductType(family.clone()).into();
    if !Sort::of(kernel, &scrutinee_type)?.is_prop() {
        return Ok(());
    }

    // The motive's sort is taken from `check_motive`, which derived it by typing the motive under its real binders. Re-asking `Sort::of` under binders assumed at `Type` would be a second reading of a question already answered, and one that can be lied to: a motive stating `Prop` over arms inhabiting `Type` would read `Prop` here and skip the whole guard.
    if motive_sort.is_prop() {
        return Ok(());
    }

    match at.declaration().constructors.as_slice() {
        // Empty: there is nothing to have received, so nothing to extract.
        [] => Ok(()),
        // One constructor: allowed exactly where the indices determine every payload component, so knowing the value tells a program nothing its type had not said. A component that is a proof is not excused: the arm would be handed the proof this match was given.
        [(tag, _)] => {
            let signature = at
                .signature(tag)
                .ok_or_else(|| Error::Undeclared(family.name))?;

            let pinned = kernel.scoped(|kernel| {
                open_payload(kernel, signature, |_kernel, binders, _payload, targets| {
                    Ok(wholly_pinned(binders, targets))
                })
            });

            match pinned? {
                true => Ok(()),
                false => Err(Error::LargeElimination(family.name)),
            }
        }
        _ => Err(Error::LargeElimination(family.name)),
    }
}
