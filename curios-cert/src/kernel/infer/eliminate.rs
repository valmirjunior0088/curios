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
//! Opening the motive at the case's targets teaches the *goal* the case's equations, and nothing else: the ambient locals, the body's occurrences of outer variables, and the scrutinee variable itself would all stay at their unrefined types. The other half of the rule is [`specialize`]: the arm is checked in a context specialized by the most-general solution of `actual indices ~ case targets` (plus `scrutinee ~ constructed value` when the scrutinee is a variable). Definitional K — `Eq : Prop` plus proof irrelevance, recorded permanent in `documentation/design/types/prop-is-strict-proof-irrelevant-and-definitionally-k.md` — is the license for solving those equations by first-order unification and substituting.
//!
//! Both directions run the *shared* unifier from [`curios_analysis::invert_indices`]. Pinning an arm binder to the rigid actual it must equal is the call as the elaborator makes it; refining an outer variable to the target it must equal is the same call with its sides swapped, and the swap lands the guards exactly right — the occurs check refuses the parameter cycle (`b := b + 1` through a family parameter), and the top guard leaves the variable-variable case to the first direction. Both directions and their composition are one shared function, [`curios_analysis::solve_indices`], which the elaborator calls too: it records the solution in its refinement store, while the kernel holds no store, so it substitutes into the arm instead. Which locals the solution re-types, and at what type, is shared as well ([`curios_analysis::retyped`]), and so is the scrutinee's own solution ([`curios_analysis::scrutinee_solution`]); the existing `mark`/`retract` bracket scopes the re-typed entries exactly to the arm.
//!
//! # The large-elimination guard
//!
//! Proof irrelevance says any two inhabitants of a proposition are interchangeable. If a program could eliminate a proposition into a relevant result, it could extract *which* proof it received, and the two facts together are inconsistent. So the elimination is allowed only when the proposition carries no information to extract: when it has no constructors at all, or exactly one whose every payload component is already determined.
//!
//! "Already determined" is the load-bearing phrase, and [`pinned_by_targets`] is where it is decided. A component is determined when the constructor's index targets *pin* it — when matching the target against a value recovers the component. Occurring in a target is not the same thing: `mk(a : Nat) : (blur(a))` mentions `a` in its index, but `blur` is an arbitrary function and knowing `blur(a)` recovers nothing. Reading occurrence as determination is precisely how a proposition with a real payload gets eliminated into a relevant type, and from there `False` follows.

#[cfg(test)]
mod ambient_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;

use {
    super::{check, infer},
    crate::{Counted, InductAt, Kernel, KernelError, Sort, carries_information},
    curios_analysis::{
        Invert, invert_indices, pinned_by_targets, retyped, scrutinee_solution, solve_indices,
    },
    curios_core::{
        Atom, Bound, Free, InductArm, InductType, MatchResult, ReduceError, Subterm, Telescope,
        Term, Variant,
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
) -> Result<(), KernelError> {
    for (tag, arm) in cases {
        check_arm(kernel, at, family, result, scrutinee, tag, arm)?;
    }

    // A catch-all binds nothing and stands for the scrutinee itself, so it is checked at the scrutinee's own indices *and at the scrutinee* — the one arm with no case value of its own, and therefore the one whose instance can only come from the term being eliminated. That is the instance `infer` reads the elimination's type off, so any other one proves something other than what the elimination hands its caller.
    if let Some(default) = default {
        check(kernel, default, &result.of(scrutinee, &family.indices))?;

        return Ok(());
    }

    // Coverage: an absent arm must justify its absence. With no catch-all, every constructor with no arm must be *impossible* at the scrutinee's indices — its targets must clash with the actuals, decided by the same shared unifier that specializes the present arms. A case the unifier merely cannot decide is a refusal, not a pass: undecided is not absent.
    for (tag, _) in &at.declaration().constructors {
        if cases.iter().any(|(present, _)| present == tag) {
            continue;
        }

        let signature = at
            .signature(tag)
            .ok_or_else(|| KernelError::Undeclared(family.name))?;

        let outcome = kernel.scoped(|kernel| {
            open_payload(kernel, signature, |kernel, binders, _payload, targets| {
                invert_indices(kernel, &family.indices, targets, binders)
            })
        });

        if !matches!(outcome?, Invert::Impossible) {
            return Err(KernelError::MissingArm {
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
) -> Result<(), KernelError> {
    let signature = at
        .signature(tag)
        .ok_or_else(|| KernelError::Undeclared(family.name))?;

    if signature.len() != arm.arity() {
        return Err(KernelError::Arity {
            counted: Counted::ArmBinders,
            expected: signature.len(),
            actual: arm.arity(),
        });
    }

    kernel.scoped(|kernel| {
        open_payload(kernel, signature, |kernel, binders, payload, targets| {
            // The forced equations of this case, as one substitution. An unreachable arm (a definite clash) is checked as written, exactly as the elaborator checks one.
            let mut solutions = specialize(kernel, family, targets, binders)?;

            // The value this arm's scrutinee is: the constructor at its payload.
            let value: Term = Subterm::Variant(Variant {
                name: family.name,
                universes: family.universes.clone(),
                params: family.params.clone(),
                tag: tag.clone(),
                payload: payload.to_vec(),
            })
            .into();

            assume_case_value(kernel, scrutinee, &value, &mut solutions)?;
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
        })
    })
}

/// Teach an arm that its scrutinee **is** this case's value, which is what specializes the context the body is checked in.
///
/// A variable scrutinee becomes a solution the arm is substituted through ([`scrutinee_solution`], which the elaborator's arms read too) — for a nominal arm the zero-index instance of the same index equations, and for an intrinsic carrier the whole of the refinement it gets. Any other scrutinee has no binder to solve, so the equation is recorded against its written spelling for the reducer to consult instead.
///
/// **Recording costs nothing: no reduction happens at registration.** `Scope::refine` and `whnf`'s `refined_reduct` carry a two-tier key, and reduce at most once per equation, only when a probe presents a term the written spelling does not answer. Reducing the scrutinee here, once per arm, merely to obtain a key would unfold a web of combinator definitions each naming the one before it twice exponentially before a single arm was checked — the scrutinee mentions a local, which is exactly the term the evaluation memos may not store — where the elaborator, registering on the written spelling, checks the same program flat.
///
/// Every scrutinee gets its equation, because a term of non-`Io` type denotes one value: an `Io` is opaque and cannot be eliminated, so it never reaches a scrutinee position, and no inhabitant of an ordinary arrow performs an effect.
///
/// Stated once because the three arm rules that need it — nominal, boolean-and-dispatch, and free-monoid — were three chances to state it differently, and what a case teaches its arm is precisely what coverage and obligation (V) read back out.
///
/// Appends to `solutions` rather than replacing them, so [`check_arm`] can hand over the index equations it has already solved; `value` is substituted through those first, since a case value built from the constructor's payload may mention a binder they pinned. Must be called inside the arm's [`Kernel::scoped`] bracket — that bracket is what scopes the refinement to the arm.
pub(super) fn assume_case_value(
    kernel: &mut Kernel,
    scrutinee: &Term,
    value: &Term,
    solutions: &mut Vec<(Free, Term)>,
) -> Result<(), ReduceError> {
    let value = value.substitute(solutions);

    if let Some(solution) = scrutinee_solution(&*kernel, scrutinee, &value) {
        solutions.push(solution);
        return Ok(());
    }

    kernel.refine(scrutinee.clone(), value)
}

/// The most-general solution of `actual indices ~ case targets`, both directions, as one idempotent substitution — the shared [`solve_indices`], which carries the rule. Empty when the equations force nothing, including when they *clash*, which makes the arm unreachable and therefore checked as written.
fn specialize(
    kernel: &mut Kernel,
    family: &InductType,
    targets: &[Term],
    binders: &[Free],
) -> Result<Vec<(Free, Term)>, KernelError> {
    Ok(
        match solve_indices(kernel, &family.indices, targets, binders)? {
            Invert::Impossible => Vec::new(),
            Invert::Solved(solutions) => solutions,
        },
    )
}

/// Re-assume every local the solution re-types at its specialized type — the shared [`retyped`], which the elaborator's arms apply too. The shadow is what a lookup finds — locals resolve innermost-first — and the enclosing `mark`/`retract` bracket retracts it with the arm.
pub(super) fn shadow(kernel: &mut Kernel, solutions: &[(Free, Term)]) {
    if solutions.is_empty() {
        return;
    }

    let locals = kernel
        .local_names()
        .into_iter()
        .zip(kernel.local_types())
        .collect::<Vec<_>>();

    for (name, type_) in retyped(&*kernel, &locals, solutions) {
        kernel.assume(&name, &type_);
    }
}

/// Open a constructor signature's payload binders into scope, hand the binder names, the occurrences, and the constructed terminal to `body`.
fn open_payload<T, B: Bound>(
    kernel: &mut Kernel,
    signature: Telescope<B>,
    body: impl FnOnce(&mut Kernel, &[Free], &[Term], &B) -> Result<T, KernelError>,
) -> Result<T, KernelError> {
    let mut binders = Vec::new();
    let mut cursor = signature.cursor();

    while let Some((_, field)) = cursor.entry() {
        binders.push(kernel.advance_assumed(&mut cursor, &field));
    }

    let constructed = cursor.body().expect("a cursor past every entry");
    let payload = cursor.into_args();

    body(kernel, &binders, &payload, &constructed)
}

/// Refuse eliminating a proposition into a relevant result unless the proposition is empty or a singleton.
///
/// The guard fires only when both halves hold: the scrutinee's family is `Prop`-sorted, and the motive lands in `Type`. A proposition eliminated into another proposition is always fine — irrelevance makes the result indistinguishable either way.
pub(super) fn guard_large_elimination(
    kernel: &mut Kernel,
    at: &InductAt,
    family: &InductType,
    motive_sort: Sort,
) -> Result<(), KernelError> {
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
        // Singleton: allowed exactly when every payload component is already determined, so knowing the value tells a program nothing it did not already know.
        [(tag, _)] => {
            let signature = at
                .signature(tag)
                .ok_or_else(|| KernelError::Undeclared(family.name))?;

            let outcome = kernel.scoped(|kernel| {
                open_payload(kernel, signature, |kernel, _binders, payload, targets| {
                    let determined = pinned_by_targets(targets);

                    for component in payload {
                        // Loud rather than a `continue`, because the quiet skip fails open: a component this loop passed over would stand exempt from the information check.
                        let Subterm::Var(var) = &**component else {
                            unreachable!("open_payload mints a variable occurrence per binder")
                        };
                        let name = var.unwrap();

                        if determined.contains(name) {
                            continue;
                        }
                        // A component that is itself a proof carries no information a relevant result could depend on: irrelevance makes any two of them interchangeable. A *type*-valued component does not qualify, however completely erasure deletes it — see `carries_information`.
                        let type_ = infer(kernel, component)?;
                        if !carries_information(kernel, &type_)? {
                            continue;
                        }

                        return Ok(false);
                    }

                    Ok(true)
                })
            });

            match outcome? {
                true => Ok(()),
                false => Err(KernelError::LargeElimination(family.name)),
            }
        }
        _ => Err(KernelError::LargeElimination(family.name)),
    }
}
