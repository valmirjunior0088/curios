//! Checking a whole program: definitions in order, each brought into scope for the ones that follow.
//!
//! A module is a sequence of top-level items, and a kernel run over one is just that sequence walked in order. Each item's type is checked to be a type, its body checked against that type, and only then is the name defined — so an item can never depend on itself except through `rec`, and a later item sees exactly the earlier ones.
//!
//! The order is load-bearing and it is the module's, not a convenience. A definition placed after its use would go unnoticed by a checker that seeded every name up front; here it is an [`Unbound`](crate::KernelError::Unbound).
//!
//! # This crate does not know what a module is
//!
//! `Module` is `curios-elab`'s type, and `curios-core` does not depend on it. So what lives here is the *rule* for an item — check, then define — and the caller walks its own representation. That keeps the kernel free of the elaborator's export metadata, islands, roots, and totality flags, none of which bear on whether a term is well-typed.

#[cfg(test)]
mod tests;

use {
    super::{Kernel, KernelError, Sort, carries_information, infer::check},
    crate::{group_totality, yields_a_sort},
    curios_core::{
        Bound, Free, InductDecl, RecGroup, Reducer, StructDecl, Subterm, Telescope, Term, Totality,
        UniverseContext,
    },
};

/// Check `name : type_ = body`, then bring it into scope.
///
/// The budget is restored first: each item gets the whole of it, so one expensive definition cannot starve the next.
///
/// A universe-polymorphic definition is checked *generically*, at its own parameters rather than at any instance. That is the right reading — a scheme is valid exactly when its body checks with its parameters held abstract — and it is also the only one available, since the kernel sees no use sites.
pub fn check_definition(
    kernel: &mut Kernel,
    name: &Free,
    type_: &Term,
    body: &Term,
    universes: &UniverseContext,
) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(universes);

    Sort::of(kernel, type_)?;
    check(kernel, body, type_)?;

    kernel.define(name, type_, body, universes);

    Ok(())
}

/// Check a top-level recursive group, then bring every member into scope under the name it is exported as.
///
/// Each member is assumed at its declared type while every body is checked, so a member may call itself and its siblings. `names` parallels the group's members positionally; an export is defined as the folded selection of the member it names, which is what a later item's occurrence of it reduces through.
///
/// Totality is not decided here. `rec` is general recursion by design, and the obligation that keeps it sound is positional and whole-module — see "Totality of the erased program" in `documentation/DESIGN.md`.
pub fn check_rec_group(
    kernel: &mut Kernel,
    names: &[Free],
    group: &RecGroup,
    universes: &UniverseContext,
) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(universes);

    if names.len() != group.length() {
        return Err(KernelError::Arity {
            expected: group.length(),
            actual: names.len(),
        });
    }

    let mut erased_member: Option<Term> = None;
    for index in 0..group.length() {
        let type_ = group.member_type(index);

        let sort = Sort::of(kernel, &type_)?;
        check(kernel, &group.member_body(index), &type_)?;

        // A proof-typed or type-yielding member is deleted by erasure, so its recursion must descend: assuming it at its declared type otherwise certifies `rec f : False = f`.
        if erased_member.is_none() && (sort.is_prop() || yields_a_sort(&type_)) {
            erased_member = Some(type_);
        }
    }

    if let Some(type_) = erased_member
        && group_totality(kernel, group) != Totality::Total
    {
        return Err(KernelError::NotDescending {
            type_: Box::new(type_),
        });
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

/// Check an `induct` declaration's registry entry: the size condition, under the declaration's own universe hypotheses.
///
/// Payload well-sortedness and registry-versus-binding agreement fall out of the ordinary item walk, because a declaration lowers to a `rec` group of real definitions. What does not fall out is the *constructor size condition* — each `Type`-sorted domain of a constructor must sit at or below the family's declared level, with one extra rung of slack for the uniform parameters — because the item walk computes each signature's sort and compares it to nothing. This is the clause that keeps an inductive from containing the universe it lives in, which is the paradox the hierarchy exists to exclude.
///
/// Call after *both* registries are seeded: a signature may name any declaration, its own family included.
pub fn check_induct_decl(kernel: &mut Kernel, declaration: &InductDecl) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(&declaration.universe_context);

    for constructor in declaration.signatures() {
        check_sizing(
            kernel,
            &constructor.telescope,
            declaration.params.len(),
            &declaration.result_sort,
        )?;
    }

    Ok(())
}

/// [`check_induct_decl`] for a `struct`: one field telescope instead of one telescope per constructor, under the same rule.
pub fn check_struct_decl(kernel: &mut Kernel, declaration: &StructDecl) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(&declaration.universe_context);

    check_non_informative(kernel, declaration)?;

    check_sizing(
        kernel,
        &declaration.fields,
        declaration.params.len(),
        &declaration.result_sort,
    )
}

/// A `Prop`-sorted structure's fields must all be non-informative, which means every one of them is a proof.
///
/// Proof irrelevance makes any two inhabitants of a proposition definitionally equal, and a structure's payload is read back by *projection*, which is not an elimination and so meets no large-elimination guard. A `Prop` structure carrying a `Nat` therefore hands the same field two convertible inhabitants with different values, and `Eq` plus congruence turns that into `False`. A field carrying a *type* does the same thing one level up — the two convertible inhabitants hand the projection two different types — so being erased buys it no exemption; see [`carries_information`].
///
/// Parameters are skipped: they are the family's arguments rather than stored payload, so a proposition may be indexed by data without carrying any. Inductives are deliberately not subject to this — `induct Box : Prop | mk(n : Nat)` is a legal declaration whose *elimination* the singleton rung guards instead.
fn check_non_informative(kernel: &mut Kernel, declaration: &StructDecl) -> Result<(), KernelError> {
    if !matches!(
        &*kernel.reduce_forced(declaration.result_sort.clone())?,
        Subterm::Prop
    ) {
        return Ok(());
    }

    let mark = kernel.mark();
    let outcome = (|| {
        let mut telescope = declaration.fields.clone();
        let mut position = 0;

        while let Telescope::Cons(type_, rest) = telescope {
            if position >= declaration.params.len() && carries_information(kernel, &type_)? {
                return Err(KernelError::Informative {
                    field: Box::new(type_),
                });
            }

            let binder = kernel.fresh(rest.first_hint());
            kernel.assume(&binder, &type_);
            telescope = rest.open(&[&Term::free_var(&binder)]);
            position += 1;
        }

        Ok(())
    })();
    kernel.retract(mark);

    outcome
}

/// Walk one declaration telescope, requiring each `Type`-sorted domain to sit at or below the declared result level — one rung higher for the leading `uniform` binders, which are the declaration's parameters.
///
/// A `Prop`-sorted result imposes no condition: `Prop` is impredicative, and what keeps *that* sound is the large-elimination guard, not sizing. A `Prop`-sorted domain imposes none either — `Prop` sits below every level.
fn check_sizing<B: Bound + Clone>(
    kernel: &mut Kernel,
    telescope: &Telescope<B>,
    uniform: usize,
    result_sort: &Term,
) -> Result<(), KernelError> {
    let result = kernel.reduce_forced(result_sort.clone())?;
    let Subterm::Type(bound) = &*result else {
        return Ok(());
    };
    let raised = bound.checked_add(1)?;

    let mark = kernel.mark();
    let outcome = (|| {
        let mut position = 0;
        let mut telescope = telescope.clone();
        while let Telescope::Cons(domain, rest) = telescope {
            if let Sort::Type(level) = Sort::of(kernel, &domain)? {
                let upper = if position < uniform { &raised } else { bound };

                if !kernel.level_leq(&level, upper) {
                    return Err(KernelError::Oversized {
                        domain: level,
                        bound: upper.clone(),
                    });
                }
            }

            let binder = kernel.fresh(rest.first_hint());
            kernel.assume(&binder, &domain);
            telescope = rest.open(&[&Term::free_var(&binder)]);
            position += 1;
        }

        Ok(())
    })();
    kernel.retract(mark);

    outcome
}

/// Check a term that closes the program — an entrypoint body, with no name to export. `expected` is its declared type when it has one.
pub fn check_entrypoint(
    kernel: &mut Kernel,
    body: &Term,
    expected: Option<&Term>,
) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(&UniverseContext::empty());

    match expected {
        Some(type_) => {
            Sort::of(kernel, type_)?;
            check(kernel, body, type_)
        }
        None => super::infer::infer(kernel, body).map(|_| ()),
    }
}
