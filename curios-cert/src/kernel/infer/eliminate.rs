//! Verifying an elimination: that each arm inhabits the motive at its own
//! case, and that a proposition is not eliminated into a relevant result.
//!
//! An elimination is the only term form whose *type* says nothing about whether
//! it is sound. `infer` reads the result off the motive, and the motive is
//! whatever the term claims — so the whole content of the rule is here.
//!
//! # The arm rule
//!
//! For each constructor, the arm body must inhabit the motive **at that
//! constructor's own index targets**, not at the scrutinee's. `Vec/nil` targets
//! index `0` and `Vec/cons` targets `succ(n)`, so the two arms are checked
//! against two different instances of the motive, and that is exactly what
//! makes a dependent elimination worth having. Checking every arm at the
//! scrutinee's indices would be both wrong and useless.
//!
//! # Context specialization
//!
//! Opening the motive at the case's targets teaches the *goal* the case's
//! equations, and nothing else: the ambient locals, the body's occurrences of
//! outer variables, and the scrutinee variable itself would all stay at their
//! unrefined types. The other half of the rule is [`specialize`]: the arm is
//! checked in a context specialized by the most-general solution of
//! `actual indices ~ case targets` (plus `scrutinee ~ constructed value` when
//! the scrutinee is a variable). Definitional K — `Eq : Prop` plus proof
//! irrelevance, recorded permanent in `DESIGN.md` — is the license for solving
//! those equations by first-order unification and substituting.
//!
//! Both directions run the *shared* unifier from [`crate::invert_indices`].
//! Pinning an arm binder to the rigid actual it must equal is the call as the
//! elaborator makes it; refining an outer variable to the target it must equal
//! is the same call with its sides swapped, and the swap lands the guards
//! exactly right — the occurs check refuses the parameter cycle (`b := b + 1`
//! through a family parameter), and the top guard leaves the variable-variable
//! case to the first direction. The elaborator reaches the same specialization
//! through its refinement store (`refine_head`); the kernel holds no store, so
//! it substitutes into the arm and shadows the affected locals instead, which
//! the existing `mark`/`retract` bracket scopes exactly to the arm.
//!
//! # The large-elimination guard
//!
//! Proof irrelevance says any two inhabitants of a proposition are
//! interchangeable. If a program could eliminate a proposition into a relevant
//! result, it could extract *which* proof it received, and the two facts
//! together are inconsistent. So the elimination is allowed only when the
//! proposition carries no information to extract: when it has no constructors
//! at all, or exactly one whose every payload component is already determined.
//!
//! "Already determined" is the load-bearing phrase, and [`forced`] is where it
//! is decided. A component is determined when the constructor's index targets
//! *pin* it — when matching the target against a value recovers the component.
//! Occurring in a target is not the same thing: `mk(a : Nat) : (blur(a))`
//! mentions `a` in its index, but `blur` is an arbitrary function and knowing
//! `blur(a)` recovers nothing. Reading occurrence as determination is precisely
//! how a proposition with a real payload gets eliminated into a relevant type,
//! and from there `False` follows.

#[cfg(test)]
mod tests;

use {
    super::{check, infer},
    crate::{
        Invert, Kernel, KernelError, invert_indices, invert_indices_outer, kernel::Sort,
        pinned_by_targets,
    },
    curios_core::{
        Atom, Bound, Free, InductArm, InductDecl, InductType, Many, Reducer, Scope, Subterm,
        Telescope, Term, Variant, Visit,
    },
};

/// Check every arm of an elimination of `scrutinee_type` under `motive`.
pub(super) fn check_induct_arms(
    kernel: &mut Kernel,
    declaration: &InductDecl,
    family: &InductType,
    motive: &Scope<Many>,
    cases: &[(Atom, InductArm)],
    default: Option<&Term>,
    scrutinee: &Term,
) -> Result<(), KernelError> {
    // A match with no arms and no catch-all is a vacuous elimination: the
    // coverage loop below must then prove *every* constructor impossible at
    // the scrutinee's indices, so the eliminated instance is uninhabited and
    // discharging it into a relevant result leaks nothing. The guard exists
    // for eliminations that can run; this one cannot.
    if !(cases.is_empty() && default.is_none()) {
        guard_large_elimination(kernel, declaration, family, motive)?;
    }

    for (tag, arm) in cases {
        check_arm(kernel, declaration, family, motive, scrutinee, tag, arm)?;
    }

    // A catch-all binds nothing and stands for the scrutinee itself, so it is
    // checked at the scrutinee's own indices rather than at any case's.
    if let Some(default) = default {
        let mut arguments = family.indices.clone();
        arguments.push(scrutinee_of(family));
        let refs = arguments.iter().collect::<Vec<_>>();

        check(kernel, default, &motive.open(&refs))?;

        return Ok(());
    }

    // Coverage: an absent arm must justify its absence. With no catch-all,
    // every constructor with no arm must be *impossible* at the scrutinee's
    // indices — its targets must clash with the actuals, decided by the same
    // shared unifier that specializes the present arms. A case the unifier
    // merely cannot decide is a refusal, not a pass: undecided is not absent.
    for (tag, _) in &declaration.constructors {
        if cases.iter().any(|(present, _)| present == tag) {
            continue;
        }

        let signature = declaration
            .instantiate(tag, &family.params)
            .ok_or_else(|| KernelError::Undeclared(family.name.clone()))?;

        let mark = kernel.mark();
        let outcome = open_payload(
            kernel,
            signature,
            |kernel, binders, _payload, constructed| {
                let Subterm::InductType(constructed) = &**constructed else {
                    return Err(KernelError::Unclassified(constructed.clone()));
                };

                invert_indices(kernel, &family.indices, &constructed.indices, binders)
            },
        );
        kernel.retract(mark);

        if !matches!(outcome?, Invert::Impossible) {
            return Err(KernelError::MissingArm {
                family: family.name.clone(),
                tag: tag.clone(),
            });
        }
    }

    Ok(())
}

/// One arm: open the constructor's payload under fresh binders at its declared
/// field types, specialize the context by this case's forced equations, then
/// require the body to inhabit the motive at this constructor's index targets
/// and at the value it constructs.
fn check_arm(
    kernel: &mut Kernel,
    declaration: &InductDecl,
    family: &InductType,
    motive: &Scope<Many>,
    scrutinee: &Term,
    tag: &Atom,
    arm: &InductArm,
) -> Result<(), KernelError> {
    let signature = declaration
        .instantiate(tag, &family.params)
        .ok_or_else(|| KernelError::Undeclared(family.name.clone()))?;

    if signature.len() != arm.arity() {
        return Err(KernelError::Arity {
            expected: signature.len(),
            actual: arm.arity(),
        });
    }

    let mark = kernel.mark();
    let outcome = open_payload(
        kernel,
        signature,
        |kernel, binders, payload, constructed| {
            // The constructed type's indices are this case's targets, with the
            // payload binders substituted in — which is what makes `Vec/nil`'s arm
            // check at length `0` and `Vec/cons`'s at `succ(n)`.
            let Subterm::InductType(constructed) = &**constructed else {
                return Err(KernelError::Unclassified(constructed.clone()));
            };

            // The forced equations of this case, as one substitution. An
            // unreachable arm (a definite clash) is checked as written, exactly as
            // the elaborator checks one.
            let mut solutions = specialize(kernel, family, &constructed.indices, binders)?;

            // The value this arm's scrutinee is: the constructor at its payload.
            let value: Term = Subterm::Variant(Variant {
                name: family.name.clone(),
                universes: family.universes.clone(),
                params: family.params.clone(),
                tag: tag.clone(),
                payload: payload.to_vec(),
            })
            .into();

            // A variable scrutinee is this case's value within the arm — the
            // zero-index instance of the same equations. Any other scrutinee
            // stands as a case equation the reducer consults.
            if let Subterm::Var(var) = &**scrutinee
                && var.as_bound().is_none()
                && kernel.local_type(var.unwrap()).is_some()
            {
                let solved = substitute(&value, &solutions);
                solutions.push((var.unwrap().clone(), solved));
            } else {
                let stuck = kernel
                    .reduce(scrutinee.clone())
                    .unwrap_or_else(|_| scrutinee.clone());
                kernel.refine(stuck, substitute(&value, &solutions));
            }

            let refs = payload.iter().collect::<Vec<_>>();
            let body = substitute(&arm.open(&refs), &solutions);

            let mut arguments = constructed.indices.clone();
            arguments.push(value);
            let refs = arguments.iter().collect::<Vec<_>>();
            let expected = substitute(&motive.open(&refs), &solutions);

            shadow(kernel, &solutions);

            check(kernel, &body, &expected)
        },
    );
    kernel.retract(mark);

    outcome
}

/// The most-general solution of `actual indices ~ case targets`, both
/// directions, as one idempotent substitution. Empty when the equations force
/// nothing — including when they *clash*, which makes the arm unreachable and
/// therefore checked as written.
///
/// Direction one pins a payload binder to the rigid actual it must equal.
/// Direction two refines an outer variable to the target it must equal, and is
/// the same shared unifier with its sides swapped: solving flexible variables
/// on what is now the target side, with the occurs check refusing a solution
/// that mentions any other refinable variable — which is exactly the parameter
/// cycle (`b := b + 1` through a family parameter) that must not substitute.
/// Running the directions in sequence, with the first solution applied to the
/// targets before the second runs, keeps one equation from being solved twice
/// in opposite orientations.
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

    // The outer variables the actual indices mention: local assumptions, never
    // top-level names, which have fixed meanings no case can refine.
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

    // Triangular composition: a pinned binder's value may mention an outer
    // variable the second direction refined. The reverse cannot happen — the
    // second direction ran on targets the first was already applied to — so one
    // pass makes the union idempotent.
    let mut solutions = pinned
        .into_iter()
        .map(|(name, value)| (name, substitute(&value, &refined)))
        .collect::<Vec<_>>();
    solutions.extend(refined);

    Ok(solutions)
}

/// `term` with every solved variable replaced by its solution, simultaneously.
/// Parallel substitution of `solutions` into `term`, as one identity-memoized,
/// free-vars-pruned traversal: a subtree mentioning no solved name is returned
/// by reference, and a shared input node is rewritten once rather than once
/// per occurrence, so sharing and warm memo cells survive the arm.
///
/// `Scope::close` followed by `open` computes the same term, but `close`'s
/// capture rebuilds every node — unpruned and unshared — so each arm's
/// specialization expanded shared subtrees into trees and re-copied nested
/// bodies once per enclosing arm. Inserting a value verbatim under any binder
/// depth is sound only while the value carries no loose index to shift; kernel
/// solution values — case values and inverted index targets, complete terms
/// both — never do, and the assert is what keeps that a checked contract.
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

/// Re-assume, at its specialized type, every local whose type mentions a solved
/// variable. The shadow is what a lookup finds — locals resolve innermost-first
/// — and the enclosing `mark`/`retract` bracket retracts it with the arm.
///
/// A solved variable's own entry is left alone: its occurrences in the arm were
/// substituted away, so nothing looks it up at its stale type.
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

/// Open a constructor signature's payload binders into scope, hand the binder
/// names, the occurrences, and the constructed terminal to `body`.
fn open_payload<T>(
    kernel: &mut Kernel,
    signature: Telescope<Term>,
    body: impl FnOnce(&mut Kernel, &[Free], &[Term], &Term) -> Result<T, KernelError>,
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

/// The scrutinee a catch-all arm stands for: the family applied to its own
/// parameters and indices, as a neutral.
fn scrutinee_of(family: &InductType) -> Term {
    Subterm::InductType(family.clone()).into()
}

/// Refuse eliminating a proposition into a relevant result unless the
/// proposition is empty or a singleton.
///
/// The guard fires only when both halves hold: the scrutinee's family is
/// `Prop`-sorted, and the motive lands in `Type`. A proposition eliminated into
/// another proposition is always fine — irrelevance makes the result
/// indistinguishable either way.
fn guard_large_elimination(
    kernel: &mut Kernel,
    declaration: &InductDecl,
    family: &InductType,
    motive: &Scope<Many>,
) -> Result<(), KernelError> {
    let scrutinee_type: Term = Subterm::InductType(family.clone()).into();
    if !Sort::of(kernel, &scrutinee_type)?.is_prop() {
        return Ok(());
    }

    // The motive under fresh binders: its *result* is what decides relevance,
    // and it is the same sort at every case.
    let mark = kernel.mark();
    let binders = (0..motive.arity())
        .map(|_| {
            let binder = kernel.fresh(None);
            kernel.assume(&binder, &Term::type_ground());
            Term::free_var(&binder)
        })
        .collect::<Vec<_>>();
    let refs = binders.iter().collect::<Vec<_>>();
    let result = motive.open(&refs);
    let relevant = Sort::of(kernel, &result).map(|sort| !sort.is_prop());
    kernel.retract(mark);

    if !relevant? {
        return Ok(());
    }

    match declaration.constructors.as_slice() {
        // Empty: there is nothing to have received, so nothing to extract.
        [] => Ok(()),
        // Singleton: allowed exactly when every payload component is already
        // determined, so knowing the value tells a program nothing it did not
        // already know.
        [(tag, _)] => {
            let signature = declaration
                .instantiate(tag, &family.params)
                .ok_or_else(|| KernelError::Undeclared(family.name.clone()))?;

            let mark = kernel.mark();
            let outcome = open_payload(
                kernel,
                signature,
                |kernel, _binders, payload, constructed| {
                    let Subterm::InductType(constructed) = &**constructed else {
                        return Ok(false);
                    };

                    let determined = pinned_by_targets(&constructed.indices);

                    for component in payload {
                        let Subterm::Var(var) = &**component else {
                            continue;
                        };
                        let name = var.unwrap();

                        if determined.contains(name) {
                            continue;
                        }
                        // A component that is itself a proof or a type carries no
                        // information a relevant result could depend on: erasure
                        // deletes it either way.
                        let type_ = infer(kernel, component)?;
                        if !carries_information(kernel, &type_)? {
                            continue;
                        }

                        return Ok(false);
                    }

                    Ok(true)
                },
            );
            kernel.retract(mark);

            match outcome? {
                true => Ok(()),
                false => Err(KernelError::LargeElimination(family.name.clone())),
            }
        }
        _ => Err(KernelError::LargeElimination(family.name.clone())),
    }
}

/// Whether a value of this type can be distinguished from another of the same
/// type — the question "does eliminating it leak anything".
///
/// A proof does not: irrelevance makes any two interchangeable. A type does
/// not either: erasure deletes it. Anything else does.
fn carries_information(kernel: &mut Kernel, type_: &Term) -> Result<bool, KernelError> {
    if Sort::of(kernel, type_)?.is_prop() {
        return Ok(false);
    }

    Ok(!matches!(
        &*kernel.reduce_forced(type_.clone())?,
        Subterm::Type(_) | Subterm::Prop
    ))
}
