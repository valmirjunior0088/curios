//! Checking a whole program: definitions in order, each brought into scope for
//! the ones that follow.
//!
//! A module is a sequence of top-level items, and a kernel run over one is just
//! that sequence walked in order. Each item's type is checked to be a type, its
//! body checked against that type, and only then is the name defined — so an
//! item can never depend on itself except through `rec`, and a later item sees
//! exactly the earlier ones.
//!
//! The order is load-bearing and it is the module's, not a convenience. A
//! definition placed after its use would go unnoticed by a checker that seeded
//! every name up front; here it is an [`Unbound`](crate::KernelError::Unbound).
//!
//! # This crate does not know what a module is
//!
//! `Module` is `curios-elab`'s type, and `curios-core` does not depend on it.
//! So what lives here is the *rule* for an item — check, then define — and the
//! caller walks its own representation. That keeps the kernel free of the
//! elaborator's export metadata, islands, roots, and totality flags, none of
//! which bear on whether a term is well-typed.

use {
    super::{Kernel, KernelError, infer::check, sort::sort_of},
    crate::{Free, RecGroup, Term, UniverseContext},
};

/// Check `name : type_ = body`, then bring it into scope.
///
/// The budget is restored first: each item gets the whole of it, so one
/// expensive definition cannot starve the next.
///
/// A universe-polymorphic definition is checked *generically*, at its own
/// parameters rather than at any instance. That is the right reading — a scheme
/// is valid exactly when its body checks with its parameters held abstract —
/// and it is also the only one available, since the kernel sees no use sites.
pub fn check_definition(
    kernel: &mut Kernel,
    name: &Free,
    type_: &Term,
    body: &Term,
    universes: &UniverseContext,
) -> Result<(), KernelError> {
    kernel.restore_budget();

    sort_of(kernel, type_)?;
    check(kernel, body, type_)?;

    kernel.define(name, type_, body, universes);

    Ok(())
}

/// Check a top-level recursive group, then bring every member into scope under
/// the name it is exported as.
///
/// Each member is assumed at its declared type while every body is checked, so
/// a member may call itself and its siblings. `names` parallels the group's
/// members positionally; an export is defined as the folded selection of the
/// member it names, which is what a later item's occurrence of it reduces
/// through.
///
/// Totality is not decided here. `rec` is general recursion by design, and the
/// obligation that keeps it sound is positional and whole-module — see
/// "Totality of the erased program" in `documentation/DESIGN.md`.
pub fn check_rec_group(
    kernel: &mut Kernel,
    names: &[Free],
    group: &RecGroup,
    universes: &UniverseContext,
) -> Result<(), KernelError> {
    kernel.restore_budget();

    if names.len() != group.length() {
        return Err(KernelError::Arity {
            expected: group.length(),
            actual: names.len(),
        });
    }

    for index in 0..group.length() {
        let type_ = group.member_type(index);

        sort_of(kernel, &type_)?;
        check(kernel, &group.member_body(index), &type_)?;
    }

    for (index, name) in names.iter().enumerate() {
        kernel.define(
            name,
            &group.member_type(index),
            &Term::rec_member(group.clone(), index),
            universes,
        );
    }

    Ok(())
}

/// Check a term that closes the program — an entrypoint body, with no name to
/// export. `expected` is its declared type when it has one.
pub fn check_entrypoint(
    kernel: &mut Kernel,
    body: &Term,
    expected: Option<&Term>,
) -> Result<(), KernelError> {
    kernel.restore_budget();

    match expected {
        Some(type_) => {
            sort_of(kernel, type_)?;
            check(kernel, body, type_)
        }
        None => super::infer::infer(kernel, body).map(|_| ()),
    }
}
