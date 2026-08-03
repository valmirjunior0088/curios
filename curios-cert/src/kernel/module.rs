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
    super::{
        Kernel, KernelError, Sort, carries_information,
        convert::convert,
        infer::{check, infer_type},
    },
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
pub(crate) fn check_definition(
    kernel: &mut Kernel,
    name: &Free,
    type_: &Term,
    body: &Term,
    universes: &UniverseContext,
) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(universes);

    infer_type(kernel, type_)?;
    check(kernel, body, type_)?;

    kernel.define(name, type_, body, universes);

    Ok(())
}

/// Check every member of `group` against its declared type, then run `within` with the members in scope.
///
/// Each member is assumed under a *fresh binder identity* rather than opened over the group's own folded spelling. A member occurrence inside a body is then an ordinary local, gated by the scope stack like every other binder — which is what stops a body from naming a group nothing ever checked, and what stops checking a body from re-entering the check it is already inside. `within` runs within that scope, because a `rec` tail sees the members too; the names retract on every path out, so what the caller returns must not mention them.
///
/// Totality is not decided for the group as a whole. `rec` is general recursion by design, and the obligation that keeps it sound is positional and whole-module — see "Totality of the erased program" in `documentation/DESIGN.md`. What *is* decided here is the local gate: a member that erasure deletes must descend, or assuming it at its declared type certifies `rec f : False = f`.
pub(crate) fn check_group<R>(
    kernel: &mut Kernel,
    group: &RecGroup,
    within: impl FnOnce(&mut Kernel, &[Free]) -> Result<R, KernelError>,
) -> Result<R, KernelError> {
    kernel.scoped(|kernel| {
        let mut names = Vec::with_capacity(group.length());
        let mut erased_member: Option<Term> = None;

        // Every member is assumed before any body is checked, because a member may call any other.
        for index in 0..group.length() {
            let type_ = group.member_type(index);
            let sort = infer_type(kernel, &type_)?;

            if erased_member.is_none() && (sort.is_prop() || yields_a_sort(&type_)) {
                erased_member = Some(type_.clone());
            }

            let name = kernel.fresh(Some("rec"));
            kernel.assume(&name, &type_);
            names.push(name);
        }

        let members = names.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = members.iter().collect::<Vec<_>>();

        for (index, member) in group.iter().enumerate() {
            check(kernel, &member.body.open(&refs), &group.member_type(index))?;
        }

        if let Some(type_) = erased_member
            && group_totality(kernel, group) != Totality::Total
        {
            return Err(KernelError::NotDescending {
                type_: Box::new(type_),
            });
        }

        within(kernel, &names)
    })
}

/// Check a top-level recursive group, then bring every member into scope under the name it is exported as.
///
/// Each member is assumed at its declared type while every body is checked, so a member may call itself and its siblings. `names` parallels the group's members positionally; an export is defined as the folded selection of the member it names, which is what a later item's occurrence of it reduces through.
///
/// Totality is not decided here. `rec` is general recursion by design, and the obligation that keeps it sound is positional and whole-module — see "Totality of the erased program" in `documentation/DESIGN.md`.
pub(crate) fn check_rec_group(
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

    check_group(kernel, group, |_, _| Ok(()))?;

    for (index, name) in names.iter().enumerate() {
        kernel.define(
            name,
            &group.member_type(index),
            &Term::rec_proj(group.clone(), index),
            universes,
        );
    }

    Ok(())
}

/// What a well-formed `induct` registry entry asserts — the whole statement, under the declaration's own universe hypotheses.
///
/// A registry entry is not an ordinary term. The typing rules consult it as an *oracle*: `Sort::of` reads `result_sort` and that decides irrelevance, `infer` on a `Variant` reads a constructor's signature, and the arm rule and index inversion read its targets. So the entry is a premise of judgments rather than an object of one, and what it asserts has to be written down somewhere. This is that place.
///
/// # The clauses
///
/// 1. Its universe context is closed and satisfiable — `universe_verdict`, run before anything is assumed.
/// 2. It carries no elaboration residue: no `Metavar` node, no level holding an unsolved universe metavariable — `induct_residue`.
/// 3. Its `result_sort` is a *literal* sort — `check_declared_sort`. See the note below on why literal, and not merely something that reduces to one.
/// 4. Every domain of its arity is a type — `check_arity`, which reaches the parameters as well as the indices, and so covers a family with no constructors at all.
/// 5. No two constructors share a tag — `check_distinct_tags`. Every lookup resolves a tag by first match, so this is what makes `constructors` a set of constructors rather than a list in which some are unreachable.
/// 6. Every constructor domain is well-sorted and sits at or below the family's declared level, with one rung of slack for the parameters — `check_signature`. This is the clause that keeps an inductive from containing the universe it lives in.
/// 7. Every constructor's parameter prefix agrees with the arity's, domain by domain — `check_constructed`. See the note below on why that prefix exists at all.
/// 8. Every constructor states as many index targets as the family declares indices — `check_constructed`.
/// 9. Every index target inhabits the index telescope at the constructor's own parameters — `check_constructed`. This is the clause nothing asked for a long time: a target is read by inversion and by the arm rule, and both reached it without any judgment having typed it.
/// 10. The declaration is strictly positive modulo polarity. That one is *deliberately* not here: the occurrence relation closes transitively across declarations, so it is decided over the whole set at once by [`positivity_vectors`](crate::positivity_vectors) rather than per entry.
///
/// # What is unspellable rather than checked
///
/// Two agreements used to need clauses and no longer exist to be violated. The indices are the terminal of the parameter telescope, so "the index telescope leads with the parameters" cannot fail. And a constructor's signature terminates in its index targets alone, so a terminal naming another family, or standing at parameters other than the declaration's own, cannot be written — which also retired three `unreachable!`s that asserted exactly that.
///
/// # The one repetition kept, and why
///
/// A constructor telescope still repeats the parameters as its prefix. That is not an oversight: a [`Telescope`] is *closed*, and a constructor's payload types mention the parameters, so the parameters must be bound inside it for the signature to be a self-contained value at all. Removing it needs either nesting the constructors under the arity — which buries tags, declaration order and plicities under binders they do not depend on — or context-relative terms, which is `curios-core`'s whole binder discipline rather than this type. So the repetition is the price of closedness, it is kept deliberately, and clause 7 is what makes it an enforced agreement rather than an assumed one.
///
/// # Not established here
///
/// One thing, found by reading every consumer of a registry entry against the clauses above rather than by probing, and recorded as a gap rather than assumed.
///
/// That `plicities` has one entry per telescope binder. Its only consumer is `curios-elab`'s `payload_plicities`, which slices at the parameter count, so a short vector is a panic in the elaborator rather than a judgment this kernel makes — fail-closed, and outside what a clause here would establish.
///
/// The other two are gone. That `result_sort` is a literal sort is clause 3, and that constructor tags are distinct is clause 5; each was probed, and the second was found admitting a term.
///
/// Call after *both* registries are seeded: a signature may name any declaration, its own family included.
pub(crate) fn check_induct_decl(
    kernel: &mut Kernel,
    declaration: &InductDecl,
) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(&declaration.universe_context);

    check_declared_sort(&declaration.result_sort)?;
    check_distinct_tags(declaration)?;
    check_arity(kernel, declaration)?;

    for constructor in declaration.signatures() {
        check_signature(
            kernel,
            &constructor.telescope,
            declaration.param_count(),
            &declaration.result_sort,
            |kernel, entries, targets| check_constructed(kernel, declaration, entries, targets),
        )?;
    }

    Ok(())
}

/// The declared result sort is a *literal* sort, not a term that reduces to one.
///
/// Every other consumer of this field reduces before reading it — `Sort::of` through `as_sort`, `check_signature` and `check_non_informative` through `Reducer::reduce_forced` — and exactly one does not: the `Prop`-valued index guard in [`invert_indices`](crate::invert_indices) matches `Subterm::Prop` on the nose. So a `result_sort` that unfolds to `Prop` made the family a proposition to every reader but that one, which is the reader whose silence is unsound: inversion went on to tell a proposition's constructors apart, and the arm it excused as impossible was reachable. Requiring the field to be literal is what makes that syntactic match correct rather than lucky; teaching each reader to reduce would instead leave the next syntactic reader to rediscover the same hole.
///
/// Nothing legitimate is refused: the surface grammar admits only the keywords `Type` and `Prop` after a declaration's `:`, so an entry the elaborator builds satisfies this by construction.
fn check_declared_sort(result_sort: &Term) -> Result<(), KernelError> {
    match &**result_sort {
        Subterm::Prop | Subterm::Type(_) => Ok(()),
        _ => Err(KernelError::NotASort(result_sort.clone())),
    }
}

/// No two constructors share a tag.
///
/// A tag is the elimination key and the runtime index, and every lookup resolves one by *first match* — [`InductDecl::constructor`], and `constructor_index` with it. So a repeat does not add a constructor, it hides one, and the rules that walk `constructors` entry by entry answer about the first one once per entry. Coverage is where that showed: asked whether each constructor is impossible at the scrutinee's indices, it resolved both entries to the first and reported a family empty at an index the declaration's own second entry constructs at — certifying a refutation of a constructor the same declaration states.
///
/// Nothing exploited it, because construction resolves by first match too, so the shadowed entry has no inhabitant to hand that refutation. This clause is what replaces that accident: the elimination rule now ranges over constructors that are distinct, rather than being sound because an unrelated rule happens to be lossy in the same direction.
fn check_distinct_tags(declaration: &InductDecl) -> Result<(), KernelError> {
    // Scanned rather than collected into a set, for the reason [`InductDecl::constructor`] gives for scanning: constructor counts are small enough that the set costs more than it saves.
    for (position, tag) in declaration.constructor_order().enumerate() {
        if declaration
            .constructor_order()
            .take(position)
            .any(|earlier| earlier == tag)
        {
            return Err(KernelError::RepeatedTag(tag.clone()));
        }
    }

    Ok(())
}

/// Every domain of the declaration's arity is a type: its parameters, and the index telescope they terminate in.
///
/// One walk, because the indices are scoped under the parameters by construction. It reaches the parameter domains as well, which matters for a family with no constructors — nothing else would present them for sorting.
fn check_arity(kernel: &mut Kernel, declaration: &InductDecl) -> Result<(), KernelError> {
    walk_arity(
        kernel,
        &declaration.arity,
        |kernel, domain| infer_type(kernel, domain).map(|_| ()),
        |kernel, domain| infer_type(kernel, domain).map(|_| ()),
    )
}

/// Walk a declaration's arity: its parameter binders, then the telescope they terminate in — an `induct`'s indices, a `struct`'s fields — handing each domain to the clause that owns it.
///
/// One walk rather than two, because the two halves *are* one telescope: the terminal is scoped under the parameters by construction, so an index or field domain sees every parameter without the caller reopening anything. Each domain is assumed before the walk descends past it, which is what lets a later one mention an earlier one; the whole walk runs inside [`Kernel::scoped`], so none of those binders outlives the clause that needed them.
fn walk_arity<B: Bound>(
    kernel: &mut Kernel,
    arity: &Telescope<Telescope<B>>,
    mut parameter: impl FnMut(&mut Kernel, &Term) -> Result<(), KernelError>,
    mut terminal: impl FnMut(&mut Kernel, &Term) -> Result<(), KernelError>,
) -> Result<(), KernelError> {
    kernel.scoped(|kernel| {
        let mut arity = arity.clone();

        let mut inner = loop {
            match arity {
                Telescope::Cons(domain, rest) => {
                    parameter(kernel, &domain)?;

                    let binder = kernel.fresh(rest.first_hint());
                    kernel.assume(&binder, &domain);
                    arity = rest.open(&[&Term::free_var(&binder)]);
                }
                Telescope::Done(inner) => break *inner,
            }
        };

        while let Telescope::Cons(domain, rest) = inner {
            terminal(kernel, &domain)?;

            let binder = kernel.fresh(rest.first_hint());
            kernel.assume(&binder, &domain);
            inner = rest.open(&[&Term::free_var(&binder)]);
        }

        Ok(())
    })
}

/// A constructor's index targets must inhabit the family's index telescope, and its parameter prefix must be the family's own parameters.
///
/// The terminal carries the targets and nothing else — the family and its parameters are fixed by the declaration, so a terminal naming another family or standing at other parameters is unspellable rather than refused. What remains is two typing clauses: the targets are checked here, and until they were, index inversion and the arm rule both read them without any judgment having seen them.
fn check_constructed(
    kernel: &mut Kernel,
    declaration: &InductDecl,
    entries: &[(Free, Term)],
    targets: &[Term],
) -> Result<(), KernelError> {
    // The prefix must be the declaration's parameters, not merely as many binders. `instantiate` substitutes the family's actual parameters into these slots, so a constructor declaring one at a different domain types its payload against a family this declaration does not describe — and closed telescopes are why the prefix is repeated here at all, which makes the agreement something to enforce rather than something to assume.
    let mut arity = declaration.arity.clone();
    let mut declared = Vec::with_capacity(declaration.param_count());

    for (binder, domain) in entries.iter().take(declaration.param_count()) {
        let Telescope::Cons(expected, rest) = arity else {
            return Err(KernelError::Arity {
                expected: declaration.param_count(),
                actual: entries.len(),
            });
        };
        if !convert(kernel, &Term::type_ground(), domain, &expected)? {
            return Err(KernelError::Mismatch {
                inferred: Box::new(domain.clone()),
                expected: Box::new(expected),
            });
        }

        let occurrence = Term::free_var(binder);
        arity = rest.open(&[&occurrence]);
        declared.push(occurrence);
    }

    let expected = declaration.index_count();
    if targets.len() != expected {
        return Err(KernelError::Arity {
            expected,
            actual: targets.len(),
        });
    }

    let mut telescope = declaration.indices_at(&declared);
    for target in targets {
        let Telescope::Cons(domain, rest) = telescope else {
            break;
        };

        check(kernel, target, &domain)?;
        telescope = rest.open(&[target]);
    }

    Ok(())
}

/// What a well-formed `struct` registry entry asserts: its universe context (clause 1), its absence of residue (clause 2), that its `result_sort` is a literal sort (clause 3), the size condition over its field telescope (clause 6), and that a `Prop`-sorted structure carries only proofs — see `check_non_informative`. Positivity is decided over the whole set as it is for an `induct`.
///
/// A `struct` has no constructors and no index targets, so clauses 5 through 9 have nothing to range over — and clause 7 could not be violated even if it did, since a structure's fields are the terminal of its own `arity` rather than a separately-stored telescope repeating the parameters: the same nesting, and the same reason, as [`InductDecl::arity`].
pub(crate) fn check_struct_decl(
    kernel: &mut Kernel,
    declaration: &StructDecl,
) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(&declaration.universe_context);

    check_declared_sort(&declaration.result_sort)?;
    check_non_informative(kernel, declaration)?;

    // The parameters, then the fields they terminate in — the second walk runs inside the first's scope, so a field domain sees every parameter. Parameters get the extra rung of slack; fields do not.
    check_signature(
        kernel,
        &declaration.arity,
        declaration.param_count(),
        &declaration.result_sort,
        |kernel, _, fields| {
            check_signature(
                kernel,
                fields,
                0,
                &declaration.result_sort,
                |_, _, _| Ok(()),
            )
        },
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

    // The parameters are skipped rather than counted past: they are the arity's own binders, and the fields it terminates in are exactly the stored payload this rule is about.
    walk_arity(
        kernel,
        &declaration.arity,
        |_, _| Ok(()),
        |kernel, type_| match carries_information(kernel, type_)? {
            true => Err(KernelError::Informative {
                field: Box::new(type_.clone()),
            }),
            false => Ok(()),
        },
    )
}

/// Walk one declaration telescope, requiring each `Type`-sorted domain to sit at or below the declared result level — one rung higher for the leading `uniform` binders, which are the declaration's parameters.
///
/// A `Prop`-sorted result imposes no condition: `Prop` is impredicative, and what keeps *that* sound is the large-elimination guard, not sizing. A `Prop`-sorted domain imposes none either — `Prop` sits below every level.
fn check_signature<B: Bound + Clone>(
    kernel: &mut Kernel,
    telescope: &Telescope<B>,
    uniform: usize,
    result_sort: &Term,
    terminal: impl FnOnce(&mut Kernel, &[(Free, Term)], &B) -> Result<(), KernelError>,
) -> Result<(), KernelError> {
    kernel.scoped(|kernel| {
        let result = kernel.reduce_forced(result_sort.clone())?;
        // A `Prop`-sorted result imposes no size condition, but the walk still runs: the terminal clause below is owed whatever the result sort is.
        let sized = match &*result {
            Subterm::Type(bound) => Some((bound.clone(), bound.checked_add(1)?)),
            _ => None,
        };

        let mut entries: Vec<(Free, Term)> = Vec::new();
        let mut telescope = telescope.clone();

        while let Telescope::Cons(domain, rest) = telescope {
            if let Some((bound, raised)) = &sized
                && let Sort::Type(level) = infer_type(kernel, &domain)?
            {
                let upper = if entries.len() < uniform {
                    raised
                } else {
                    bound
                };

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
            entries.push((binder, domain));
        }

        let Telescope::Done(body) = telescope else {
            unreachable!("the loop exits only at the terminal")
        };

        terminal(kernel, &entries, &body)
    })
}

/// Check a term that closes the program — an entrypoint body, with no name to export. `expected` is its declared type when it has one.
pub(crate) fn check_entrypoint(
    kernel: &mut Kernel,
    body: &Term,
    expected: Option<&Term>,
) -> Result<(), KernelError> {
    kernel.restore_budget();
    kernel.assume_universes(&UniverseContext::empty());

    match expected {
        Some(type_) => {
            infer_type(kernel, type_)?;
            check(kernel, body, type_)
        }
        None => super::infer::infer(kernel, body).map(|_| ()),
    }
}
