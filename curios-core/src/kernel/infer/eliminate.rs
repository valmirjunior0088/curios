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
        Atom, Free, InductArm, InductDecl, InductType, Kernel, KernelError, Reducer, Scope,
        Subterm, Telescope, Term, Variant, kernel::sort::sort_of,
    },
};

/// Check every arm of an elimination of `scrutinee_type` under `motive`.
pub(super) fn check_induct_arms(
    kernel: &mut Kernel,
    declaration: &InductDecl,
    family: &InductType,
    motive: &Scope<crate::Many>,
    cases: &[(Atom, InductArm)],
    default: Option<&Term>,
) -> Result<(), KernelError> {
    guard_large_elimination(kernel, declaration, family, motive)?;

    for (tag, arm) in cases {
        check_arm(kernel, declaration, family, motive, tag, arm)?;
    }

    // A catch-all binds nothing and stands for the scrutinee itself, so it is
    // checked at the scrutinee's own indices rather than at any case's.
    if let Some(default) = default {
        let mut arguments = family.indices.clone();
        arguments.push(scrutinee_of(family));
        let refs = arguments.iter().collect::<Vec<_>>();

        check(kernel, default, &motive.open(&refs))?;
    }

    Ok(())
}

/// One arm: open the constructor's payload under fresh binders at its declared
/// field types, then require the body to inhabit the motive at this
/// constructor's index targets and at the value it constructs.
fn check_arm(
    kernel: &mut Kernel,
    declaration: &InductDecl,
    family: &InductType,
    motive: &Scope<crate::Many>,
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
    let outcome = open_payload(kernel, signature, |kernel, payload, constructed| {
        let refs = payload.iter().collect::<Vec<_>>();
        let body = arm.open(&refs);

        // The constructed type's indices are this case's targets, with the
        // payload binders substituted in — which is what makes `Vec/nil`'s arm
        // check at length `0` and `Vec/cons`'s at `succ(n)`.
        let Subterm::InductType(constructed) = &**constructed else {
            return Err(KernelError::Unclassified(constructed.clone()));
        };

        let mut arguments = constructed.indices.clone();
        arguments.push(
            Subterm::Variant(Variant {
                name: family.name.clone(),
                universes: family.universes.clone(),
                params: family.params.clone(),
                tag: tag.clone(),
                payload: payload.to_vec(),
            })
            .into(),
        );
        let refs = arguments.iter().collect::<Vec<_>>();

        check(kernel, &body, &motive.open(&refs))
    });
    kernel.retract(mark);

    outcome
}

/// Open a constructor signature's payload binders into scope, hand the
/// occurrences and the constructed terminal to `body`.
fn open_payload<T>(
    kernel: &mut Kernel,
    signature: Telescope<Term>,
    body: impl FnOnce(&mut Kernel, &[Term], &Term) -> Result<T, KernelError>,
) -> Result<T, KernelError> {
    let mut signature = signature;
    let mut payload = Vec::new();

    let constructed = loop {
        match signature {
            Telescope::Cons(field, rest) => {
                let binder = kernel.fresh(rest.first_hint());
                kernel.assume(&binder, &field);
                let occurrence = Term::free_var(&binder);
                signature = rest.open(&[&occurrence]);
                payload.push(occurrence);
            }
            Telescope::Done(constructed) => break *constructed,
        }
    };

    body(kernel, &payload, &constructed)
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
    motive: &Scope<crate::Many>,
) -> Result<(), KernelError> {
    let scrutinee_type: Term = Subterm::InductType(family.clone()).into();
    if !sort_of(kernel, &scrutinee_type)?.is_prop() {
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
    let relevant = sort_of(kernel, &result).map(|sort| !sort.is_prop());
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
            let outcome = open_payload(kernel, signature, |kernel, payload, constructed| {
                let Subterm::InductType(constructed) = &**constructed else {
                    return Ok(false);
                };

                let determined = forced(&constructed.indices);

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
            });
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
    if sort_of(kernel, type_)?.is_prop() {
        return Ok(false);
    }

    Ok(!matches!(
        &*kernel.reduce_forced(type_.clone())?,
        Subterm::Type(_) | Subterm::Prop
    ))
}

/// The payload binders an index target set *determines*.
///
/// A binder is determined when matching a value against the target recovers it.
/// That holds when the target is the binder itself, and — recursively — when it
/// is a constructor application and the binder sits in one of its arguments,
/// because constructors are injective and their tags are disjoint.
///
/// It does **not** hold for a binder under anything else. `blur(a)` is an
/// arbitrary function of `a`: knowing its value recovers nothing, since `blur`
/// need not be injective and in the case that motivated this rule is the
/// constant zero. Reading occurrence as determination — asking only whether `a`
/// appears anywhere in the target — is the mistake, and it is the difference
/// between a singleton and a proposition with a payload a program can read.
///
/// Total by construction: a target this cannot decompose contributes nothing,
/// so a shape nobody anticipated yields *fewer* determined binders and a
/// stricter guard, never a looser one.
fn forced(indices: &[Term]) -> Vec<Free> {
    fn walk(target: &Term, determined: &mut Vec<Free>) {
        match &**target {
            Subterm::Var(var) => determined.push(var.unwrap().clone()),
            // Constructors are injective and their tags disjoint, so matching
            // recovers every argument. Parameters are not walked: they are the
            // family's, fixed before this constructor was reached.
            Subterm::Variant(Variant { payload, .. }) => {
                for component in payload {
                    walk(component, determined);
                }
            }
            // Anything else — an application, a projection, a primitive, a
            // stuck match — determines nothing.
            _ => {}
        }
    }

    let mut determined = Vec::new();
    for target in indices {
        walk(target, &mut determined);
    }

    determined
}
