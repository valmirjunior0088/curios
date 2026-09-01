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
//! Opening the motive at the case's targets teaches the *goal* the case's equations, and nothing else: the ambient locals, the body's occurrences of outer variables, and the scrutinee variable itself would all stay at their unrefined types. The other half of the rule is [`specialize`]: the arm is checked in a context specialized by the most-general solution of `actual indices ~ case targets` (plus `scrutinee ~ constructed value` when the scrutinee is a variable). Definitional K — `Eq : Prop` plus proof irrelevance, recorded permanent in `documentation/design/language/totality-of-the-erased-program.md` — is the license for solving those equations by first-order unification and substituting.
//!
//! Both directions run the *shared* unifier from [`curios_analysis::invert_indices`]. Pinning an arm binder to the rigid actual it must equal is the call as the elaborator makes it; refining an outer variable to the target it must equal is the same call with its sides swapped, and the swap lands the guards exactly right — the occurs check refuses the parameter cycle (`b := b + 1` through a family parameter), and the top guard leaves the variable-variable case to the first direction. The elaborator reaches the same specialization through its refinement store (`refine_head`); the kernel holds no store, so it substitutes into the arm and shadows the affected locals instead, which the existing `mark`/`retract` bracket scopes exactly to the arm.
//!
//! # The large-elimination guard
//!
//! Proof irrelevance says any two inhabitants of a proposition are interchangeable. If a program could eliminate a proposition into a relevant result, it could extract *which* proof it received, and the two facts together are inconsistent. So the elimination is allowed only when the proposition carries no information to extract: when it has no constructors at all, or exactly one whose every payload component is already determined.
//!
//! "Already determined" is the load-bearing phrase, and [`pinned_by_targets`] is where it is decided. A component is determined when the constructor's index targets *pin* it — when matching the target against a value recovers the component. Occurring in a target is not the same thing: `mk(a : Nat) : (blur(a))` mentions `a` in its index, but `blur` is an arbitrary function and knowing `blur(a)` recovers nothing. Reading occurrence as determination is precisely how a proposition with a real payload gets eliminated into a relevant type, and from there `False` follows.

#[cfg(test)]
mod tests;

use {
    super::{check, infer},
    crate::{Counted, InductAt, Kernel, KernelError, Sort, carries_information},
    curios_analysis::{Invert, invert_indices, invert_indices_outer, pinned_by_targets},
    curios_core::{
        Atom, Bound, Free, InductArm, InductType, Many, Scope, Subterm, Telescope, Term, Variant,
        Visit,
    },
};

/// Check every arm of an elimination of `scrutinee_type` under `motive`.
pub(super) fn check_induct_arms(
    kernel: &mut Kernel,
    at: &InductAt,
    family: &InductType,
    motive: &Scope<Many>,
    cases: &[(Atom, InductArm)],
    default: Option<&Term>,
    scrutinee: &Term,
) -> Result<(), KernelError> {
    for (tag, arm) in cases {
        check_arm(kernel, at, family, motive, scrutinee, tag, arm)?;
    }

    // A catch-all binds nothing and stands for the scrutinee itself, so it is checked at the scrutinee's own indices *and at the scrutinee* — the one arm with no case value of its own, and therefore the one whose instance can only come from the term being eliminated. That is the instance `infer` reads the elimination's type off, so any other one proves something other than what the elimination hands its caller.
    if let Some(default) = default {
        let mut arguments = family.indices.clone();
        arguments.push(scrutinee.clone());
        let refs = arguments.iter().collect::<Vec<_>>();

        check(kernel, default, &motive.open(&refs))?;

        return Ok(());
    }

    // Coverage: an absent arm must justify its absence. With no catch-all, every constructor with no arm must be *impossible* at the scrutinee's indices — its targets must clash with the actuals, decided by the same shared unifier that specializes the present arms. A case the unifier merely cannot decide is a refusal, not a pass: undecided is not absent.
    for (tag, _) in &at.declaration().constructors {
        if cases.iter().any(|(present, _)| present == tag) {
            continue;
        }

        let signature = at
            .signature(tag)
            .ok_or_else(|| KernelError::Undeclared(family.name.clone()))?;

        let outcome = kernel.scoped(|kernel| {
            open_payload(kernel, signature, |kernel, binders, _payload, targets| {
                invert_indices(kernel, &family.indices, targets, binders)
            })
        });

        if !matches!(outcome?, Invert::Impossible) {
            return Err(KernelError::MissingArm {
                family: family.name.clone(),
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
    motive: &Scope<Many>,
    scrutinee: &Term,
    tag: &Atom,
    arm: &InductArm,
) -> Result<(), KernelError> {
    let signature = at
        .signature(tag)
        .ok_or_else(|| KernelError::Undeclared(family.name.clone()))?;

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
                name: family.name.clone(),
                universes: family.universes.clone(),
                params: family.params.clone(),
                tag: tag.clone(),
                payload: payload.to_vec(),
            })
            .into();

            assume_case_value(kernel, scrutinee, &value, &mut solutions);

            let refs = payload.iter().collect::<Vec<_>>();
            let body = substitute(&arm.open(&refs), &solutions);

            let mut arguments = targets.clone();
            arguments.push(value);
            let refs = arguments.iter().collect::<Vec<_>>();
            let expected = substitute(&motive.open(&refs), &solutions);

            shadow(kernel, &solutions);

            check(kernel, &body, &expected)
        })
    })
}

/// Teach an arm that its scrutinee **is** this case's value, which is what specializes the context the body is checked in.
///
/// A variable scrutinee becomes a solution the arm is substituted through — for a nominal arm the zero-index instance of the same index equations, and for an intrinsic carrier the whole of the refinement it gets. Any other scrutinee has no binder to solve, so the equation is recorded against its written spelling for the reducer to consult instead.
///
/// **Recording costs nothing, which it did not use to.** This reduced the scrutinee to weak-head normal form here, once per arm, purely to obtain a key — and the scrutinee mentions a local, which is exactly the term the evaluation memos may not store, so a web of combinator definitions each naming the one before it twice unfolded exponentially before a single arm was checked. Fourteen such definitions refused on the reduction budget while the elaborator, which registers on the written spelling, checked the same program flat. `Scope::refine` and `whnf`'s `refined_reduct` carry the two-tier key that replaced it; the reduction happens there, at most once per equation, and only when a probe presents a term the written spelling does not answer.
///
/// The reduction's own failure used to be swallowed by an `unwrap_or_else`, so exhausting the budget here surfaced as a refusal on whatever judgment ran next. There is nothing left to swallow: no reduction happens at registration at all.
///
/// Every scrutinee gets its equation, because a term of non-`Io` type denotes one value. This used to ask a shared walk whether the spelling fixed one — an operation the host performs did not, nor did a call whose callee the walk could not read, since `f(true)` for a parameter `f` computes whatever the caller bound. Retyping the host surface to return `Io` answered both by construction: an `Io` is opaque and cannot be eliminated, so it never reaches a scrutinee position, and no inhabitant of an ordinary arrow performs an effect. The equation the walk had to withhold from a pure opaque head is admitted again.
///
/// Stated once because the three arm rules that need it — nominal, boolean-and-dispatch, and free-monoid — were three chances to state it differently, and what a case teaches its arm is precisely what coverage and obligation (V) read back out.
///
/// Appends to `solutions` rather than replacing them, so [`check_arm`] can hand over the index equations it has already solved; `value` is substituted through those first, since a case value built from the constructor's payload may mention a binder they pinned. Must be called inside the arm's [`Kernel::scoped`] bracket — that bracket is what scopes the refinement to the arm.
pub(super) fn assume_case_value(
    kernel: &mut Kernel,
    scrutinee: &Term,
    value: &Term,
    solutions: &mut Vec<(Free, Term)>,
) {
    let value = substitute(value, solutions);

    if let Subterm::Var(var) = &**scrutinee
        && var.as_bound().is_none()
        && kernel.local_type(var.unwrap()).is_some()
    {
        solutions.push((var.unwrap().clone(), value));
        return;
    }

    kernel.refine(scrutinee.clone(), value);
}

/// The most-general solution of `actual indices ~ case targets`, both directions, as one idempotent substitution. Empty when the equations force nothing — including when they *clash*, which makes the arm unreachable and therefore checked as written.
///
/// Direction one pins a payload binder to the rigid actual it must equal. Direction two refines an outer variable to the target it must equal, and is the same shared unifier with its sides swapped: solving flexible variables on what is now the target side, with the occurs check refusing a solution that mentions any other refinable variable — which is exactly the parameter cycle (`b := b + 1` through a family parameter) that must not substitute. Running the directions in sequence, with the first solution applied to the targets before the second runs, keeps one equation from being solved twice in opposite orientations.
fn specialize(
    kernel: &mut Kernel,
    family: &InductType,
    targets: &[Term],
    binders: &[Free],
) -> Result<Vec<(Free, Term)>, KernelError> {
    let pinned = match invert_indices(kernel, &family.indices, targets, binders)? {
        Invert::Impossible => return Ok(Vec::new()),
        Invert::Solved(solutions) => solutions,
    };

    let residual = targets
        .iter()
        .map(|target| substitute(target, &pinned))
        .collect::<Vec<_>>();

    // The outer variables the actual indices mention: local assumptions, never top-level names, which have fixed meanings no case can refine.
    let mut outer: Vec<Free> = Vec::new();
    for actual in &family.indices {
        for name in actual.free_vars() {
            if kernel.local_type(&name).is_some() && !outer.contains(&name) {
                outer.push(name);
            }
        }
    }

    let refined = match invert_indices_outer(kernel, &residual, &family.indices, &outer)? {
        Invert::Impossible => return Ok(Vec::new()),
        Invert::Solved(solutions) => solutions,
    };

    // Triangular composition: a pinned binder's value may mention an outer variable the second direction refined. The reverse cannot happen — the second direction ran on targets the first was already applied to — so one pass makes the union idempotent.
    let mut solutions = pinned
        .into_iter()
        .map(|(name, value)| (name, substitute(&value, &refined)))
        .collect::<Vec<_>>();
    solutions.extend(refined);

    Ok(solutions)
}

/// `term` with every solved variable replaced by its solution, simultaneously. Parallel substitution of `solutions` into `term`, as one identity-memoized, free-vars-pruned traversal: a subtree mentioning no solved name is returned by reference, and a shared input node is rewritten once rather than once per occurrence, so sharing and warm memo cells survive the arm.
///
/// `Scope::close` followed by `open` computes the same term, but `close`'s capture rebuilds every node — unpruned and unshared — so each arm's specialization expanded shared subtrees into trees and re-copied nested bodies once per enclosing arm. Inserting a value verbatim under any binder depth is sound only while the value carries no loose index to shift; kernel solution values — case values and inverted index targets, complete terms both — never do, and the assert is what keeps that a checked contract.
pub(super) fn substitute(term: &Term, solutions: &[(Free, Term)]) -> Term {
    if solutions.is_empty() {
        return term.clone();
    }

    for (_, value) in solutions {
        assert!(value.closed(), "substitution value carries a loose index");
    }

    let solutions = solutions.to_vec();
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term| {
            if let Subterm::Var(var) = &**term
                && let Some(name) = var.as_free()
                && let Some((_, value)) = solutions.iter().find(|(solved, _)| solved == name)
            {
                return Some(value.clone());
            }

            match solutions
                .iter()
                .any(|(solved, _)| term.mentions_free(solved))
            {
                true => None,
                false => Some(term.clone()),
            }
        }),
    );

    term.traverse(&mut visit)
}

/// Re-assume, at its specialized type, every local whose type mentions a solved variable. The shadow is what a lookup finds — locals resolve innermost-first — and the enclosing `mark`/`retract` bracket retracts it with the arm.
///
/// A solved variable's own entry is left alone: its occurrences in the arm were substituted away, so nothing looks it up at its stale type.
pub(super) fn shadow(kernel: &mut Kernel, solutions: &[(Free, Term)]) {
    if solutions.is_empty() {
        return;
    }

    let locals = kernel
        .local_names()
        .into_iter()
        .zip(kernel.local_types())
        .collect::<Vec<_>>();

    for (name, type_) in locals {
        if solutions.iter().any(|(solved, _)| *solved == name) {
            continue;
        }

        let mentioned = type_.free_vars();
        if solutions
            .iter()
            .any(|(solved, _)| mentioned.contains(solved))
        {
            kernel.assume(&name, &substitute(&type_, solutions));
        }
    }
}

/// Open a constructor signature's payload binders into scope, hand the binder names, the occurrences, and the constructed terminal to `body`.
fn open_payload<T, B: Bound>(
    kernel: &mut Kernel,
    signature: Telescope<B>,
    body: impl FnOnce(&mut Kernel, &[Free], &[Term], &B) -> Result<T, KernelError>,
) -> Result<T, KernelError> {
    let mut signature = signature;
    let mut binders = Vec::new();
    let mut payload = Vec::new();

    let constructed = loop {
        match signature {
            Telescope::Cons(field, rest) => {
                let binder = kernel.fresh(rest.first_hint());
                kernel.assume(&binder, &field);
                let occurrence = Term::free_var(&binder);
                signature = rest.open(&[&occurrence]);
                binders.push(binder);
                payload.push(occurrence);
            }
            Telescope::Done(constructed) => break *constructed,
        }
    };

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

    // The motive's sort is taken from `check_motive`, which derived it by typing the motive under its real binders. This used to re-ask `Sort::of` under binders assumed at `Type` — a second reading of a question already answered, and the reading that could be lied to: a motive stating `Prop` over arms inhabiting `Type` was classified `Prop` here and skipped the whole guard.
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
                .ok_or_else(|| KernelError::Undeclared(family.name.clone()))?;

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
                false => Err(KernelError::LargeElimination(family.name.clone())),
            }
        }
        _ => Err(KernelError::LargeElimination(family.name.clone())),
    }
}
