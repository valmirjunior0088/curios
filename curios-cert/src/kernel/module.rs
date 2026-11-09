//! Checking a whole program: definitions in order, each brought into scope for the ones that follow.
//!
//! A module is a sequence of top-level items, and a kernel run over one is just that sequence walked in order. Each item's type is checked to be a type, its body checked against that type, and only then is the name defined — so an item can never depend on itself except through `rec`, and a later item sees exactly the earlier ones.
//!
//! The order is load-bearing and it is the module's, not a convenience. A definition placed after its use would go unnoticed by a checker that seeded every name up front; here it is an [`Unbound`](crate::Error::Unbound).
//!
//! # This module does not know what a module is
//!
//! `Module` is `curios-core`'s, and nothing here names it: what lives here is the *rule* for an item — check, then define — while `recheck.rs` walks the representation and hands each item's parts down. That keeps the rule free of the elaborator's export metadata, islands, and roots, none of which bear on whether a term is well-typed. The totality an item carries does bear on it, but not here — `obligation.rs` is where it is read.

#[cfg(test)]
mod definition_tests;
#[cfg(test)]
mod induct_tests;
#[cfg(test)]
mod struct_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod walk_tests;

use {
    super::{Counted, Error, Kernel, Sort, carries_information, check, convert, infer_type},
    crate::bears_proof,
    curios_analysis::{blocker, known_to_terminate, yields_a_sort_where},
    curios_core::{
        Bound, DefinitionKind, Free, Global, InductDecl, Many, Quality, RecGroup, Reducer, Scope,
        StructDecl, Subterm, Telescope, Term, Totality, UniverseContext, instantiate_qualities,
    },
};

/// Judge a declaration at every assignment of the sorts `universes` leaves to its caller, the budget restored and the declaration's constraints assumed for each.
///
/// **A declaration generic in a sort is judged at each sort a use can state, and never at the parameter.** Every rule that reads a sort reads which of `Prop` and `Type` it is — irrelevance, the elimination guard, eta, the obligation an erased position carries — and each has its own safe answer for a sort that states neither. So `judge` is handed the assignment and reads the declaration's terms at it ([`at_sorts`]): what it judges holds no parameter, a store keyed by a term holds each term at one sort, and a use at any instance stands at an assignment that was judged. That is two to the count judgments, which [`UniverseContext::QUALITY_LIMIT`] bounds.
///
/// A refusal names the assignment it was reached at. A declaration that leaves no sort open is judged once, as it stands, and its refusal is its own.
fn at_every_sort(
    kernel: &mut Kernel,
    universes: &UniverseContext,
    mut judge: impl FnMut(&mut Kernel, &[Quality]) -> Result<(), Error>,
) -> Result<(), Error> {
    if universes.quality_count > UniverseContext::QUALITY_LIMIT {
        return Err(Error::TooManySorts {
            stated: universes.quality_count,
        });
    }

    for sorts in Quality::assignments(universes.quality_count) {
        kernel.restore_budget();
        kernel.assume_universes(universes);

        judge(kernel, &sorts).map_err(|refusal| match sorts.is_empty() {
            true => refusal,
            false => Error::AtSorts {
                sorts: sorts.clone(),
                refusal: Box::new(refusal),
            },
        })?;
    }

    Ok(())
}

/// `value` at one assignment of the sorts its declaration leaves open: as it stands where the declaration leaves none.
fn at_sorts<B: Bound>(value: &B, sorts: &[Quality]) -> B {
    match sorts.is_empty() {
        true => value.clone(),
        false => instantiate_qualities(value, sorts),
    }
}

/// Check `name : type_ = body`, then bring it into scope.
///
/// The budget is restored first: each item gets the whole of it, so one expensive definition cannot starve the next.
///
/// A definition polymorphic in a level is checked *generically*, at its own parameters rather than at any instance. That is the right reading — a scheme is valid exactly when its body checks with its parameters held abstract — and it is also the only one available, since the kernel sees no use sites. One that leaves a sort to its caller is checked at every assignment of them ([`at_every_sort`]), and stored as it is stated: an occurrence states the sorts with the levels, and reads the type and the body at them.
///
/// What this check states of the definition, by its own reading, and defines it by ([`Stated`]): whether it is a proof a declaration wrote — one introduced as [`DefinitionKind::Authored`] whose type is a proposition at every assignment of its sorts, which the sort this check types the statement at answers with nothing run — and the sort its value inhabits, where its type is written as a sort, its value is no proof, and every assignment reaches one sort.
pub fn check_definition(
    kernel: &mut Kernel,
    name: &Global,
    kind: &DefinitionKind,
    type_: &Term,
    body: &Term,
    universes: &UniverseContext,
) -> Result<Stated, Error> {
    let stated = stating(kernel, kind, type_, body, true, universes)?;
    define_as_stated(
        kernel,
        name,
        type_,
        body,
        stated.proof,
        stated.value_sort.as_ref(),
        universes,
    )?;

    Ok(stated)
}

/// [`check_definition`]'s statements of a definition whose verdict a record vouches for, the walk judging nothing of it: the same questions, its body not checked, and the name defined by them.
pub fn state_definition(
    kernel: &mut Kernel,
    name: &Global,
    kind: &DefinitionKind,
    type_: &Term,
    body: &Term,
    universes: &UniverseContext,
) -> Result<Stated, Error> {
    let stated = stating(kernel, kind, type_, body, false, universes)?;
    define_as_stated(
        kernel,
        name,
        type_,
        body,
        stated.proof,
        stated.value_sort.as_ref(),
        universes,
    )?;

    Ok(stated)
}

/// What a definition of `kind` at `type_` and of `body` is stated, at every assignment of its sorts, its body checked against its type there where `checking`.
fn stating(
    kernel: &mut Kernel,
    kind: &DefinitionKind,
    type_: &Term,
    body: &Term,
    checking: bool,
    universes: &UniverseContext,
) -> Result<Stated, Error> {
    let mut proof = matches!(kind, DefinitionKind::Authored);
    let sorted = matches!(&**type_, Subterm::Sort(_));
    let mut reached = Vec::new();
    at_every_sort(kernel, universes, |kernel, sorts| {
        let (type_, body) = (at_sorts(type_, sorts), at_sorts(body, sorts));
        proof &= infer_type(kernel, &type_)?.is_prop();
        if checking {
            check(kernel, &body, &type_)?;
        }
        if sorted {
            reached.push(Sort::of(kernel, &body).ok().map(Sort::term));
        }

        Ok(())
    })?;
    let value_sort = match proof {
        true => None,
        false => agreed(reached),
    };

    Ok(Stated { proof, value_sort })
}

/// What the kernel states of a definition it judged beside its terms ([`check_definition`]): whether it is a proof, whose body no judgment after its own check unfolds, and the sort its value inhabits, which a judgment asking a binder of the name its sort reads unreduced.
pub struct Stated {
    pub proof: bool,
    pub value_sort: Option<Term>,
}

/// The one sort every assignment reached, where each reached one and they are one: a sort stated generically holds at every assignment only where none of them moves it.
fn agreed(reached: Vec<Option<Term>>) -> Option<Term> {
    let mut reached = reached.into_iter();
    let first = reached.next()??;

    reached
        .all(|sort| sort.as_ref() == Some(&first))
        .then_some(first)
}

/// Bring `name : type_` into scope as its definition states it: unfolding to `body`, or to nothing where it is marked a proof, whose body its own check read and no judgment after it does, with the sort its value inhabits where it states one. A refused definition stands the same way, so what a name is to the rest of a walk is what it is to a walk over a unit that mounts this one.
pub fn define_as_stated(
    kernel: &mut Kernel,
    name: &Global,
    type_: &Term,
    body: &Term,
    proof: bool,
    value_sort: Option<&Term>,
    universes: &UniverseContext,
) -> Result<(), Error> {
    match proof {
        true => kernel.declare(name, type_, universes),
        false => kernel.define_sorted(name, type_, body, value_sort, universes),
    }
}

/// Check every member of `group` against its declared type, then run `within` with the members in scope.
///
/// Each member is assumed under a *fresh binder identity* rather than opened over the group's own folded spelling. A member occurrence inside a body is then an ordinary local, gated by the scope stack like every other binder — which is what stops a body from naming a group nothing ever checked, and what stops checking a body from re-entering the check it is already inside. `within` runs within that scope, because a `rec` tail sees the members too; the names retract on every path out, so what the caller returns must not mention them.
///
/// Member *signatures* are checked at that opaque spelling too, and bodies at the folded one. See the note in the body for what each phase needs and what goes wrong when either takes the other's.
///
/// The group's calls are the ones this check types in its bodies, recorded and graded as they are typed and closed to a verdict once every body is checked (see `kernel::calls`) — which is the verdict obligations (T) and (V) read for the group afterwards. It is not a demand: `rec` is general recursion by design, and the obligation that keeps it sound is positional and whole-module — see `documentation/design/soundness/totality-of-the-erased-program.md`. What *is* decided here is the local gate: a member that erasure deletes must descend, or assuming it at its declared type certifies `rec f : False = f`.
pub fn check_group<R>(
    kernel: &mut Kernel,
    group: &RecGroup,
    tail: Option<&Scope<Many>>,
    within: impl FnOnce(&mut Kernel, &[Free]) -> Result<R, Error>,
) -> Result<R, Error> {
    // A group carries no universe context of its own, so there is nothing to decide here and nothing to assume: its members' level and quality parameters are the enclosing declaration's, whose context `certify_definitions` decided closed and satisfiable before this walk assumed it, and whose sorts `at_every_sort` judges this group at every assignment of.
    // Every scope a `rec` states binds one binder per member: a member's two, which `member_type` and `member_body` open over the group, and the tail, which `infer`'s `Rec` arm opens over the locals the members are assumed under. Each count is a fact about a sibling field — the members the group holds — that nothing in the representation relates it to, and no typing rule reads one, so this is the boundary that checks them all and a term a caller built otherwise is refused here rather than asserted on where a scope is opened. A `rec` item passes no tail: its members are the module's own declarations, and what follows them is the rest of the module.
    let members = group.length();
    let stated = group
        .iter()
        .flat_map(|member| [member.type_.arity(), member.body.arity()])
        .chain(tail.map(Scope::arity));
    for binders in stated {
        if binders != members {
            return Err(Error::Arity {
                counted: Counted::GroupBinders,
                expected: members,
                actual: binders,
            });
        }
    }

    kernel.scoped(|kernel| {
        // Signatures first, then bodies, and neither reads a member before its turn has found it known to terminate.
        //
        // A member's *type* may name the group: in `rec T(n : Nat) -> Type … and val : T(2)`, a sibling computes `val`'s type. [`RecGroup::member_type`] hands that type back opened over the group's own folded spelling, so inferring its sort unfolds `T`, walks back into the group, and re-enters this very check without bound — aborting the process rather than refusing a term. Opened over the assumptions minted here instead, `T` is opaque and `T(2)` is a stuck neutral whose type is `Type`, which is all a sort check needs. Each member is assumed at its type so opened, and its body is checked at it.
        //
        // A body needs a sibling's value: `val = 3` type-checks only because `T(2)` *reduces* to `Nat`. So once every signature is checked, each member takes its turn in written order (`known_to_terminate`): it is known to terminate where the calls among the members known so far and its own descend, and from then on its local unfolds in reduction to its selection out of the known members (`Calls::member_value`), whose bodies name a member withheld by its local, standing for nothing. Every turn is taken before any body is typed, so a body reads a sibling written after it, as the elaborator's reading parked on the sibling's slot comes to; a member unfolds before its own body is typed, and the group's acceptance waits on typing it. The locals keep their names, so a call is recorded and kept from the memo as it was, and only a reduct spells the known members.
        //
        // Agda type-checks a mutual block reading its siblings' definitions before its termination check runs, and may loop there; the turns are a termination check taken first. Coq forbids the question outright — its `Fix` rule checks each `A_i` in a context without the fixpoint names — which is a language restriction rather than a rule this kernel could adopt while `rec` admits an inductive-recursive group.
        let names = (0..group.length())
            .map(|_| kernel.fresh(None))
            .collect::<Vec<_>>();

        let members = names.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = members.iter().collect::<Vec<_>>();
        let types = group
            .iter()
            .map(|member| member.type_.open(&refs))
            .collect::<Vec<_>>();
        let bodies = group
            .iter()
            .map(|member| member.body.open(&refs))
            .collect::<Vec<_>>();
        // Where the positions the signatures and bodies record begin, each holding a member as the local it is assumed at.
        let since = kernel.positions_recorded();

        // Assumed before any signature is checked, because one member's type may name any other. A type not yet known to be a type is assumed only for the span of this scope: an ill-sorted one refuses the group below, and `scoped` retracts every assumption on the way out.
        for (name, type_) in names.iter().zip(&types) {
            kernel.assume(name, type_);
        }

        // Whether each member's signature or body encloses a group of its own that does not descend — what obligations (T) and (V) read the member by, since a group is typed here with its enclosing binders opened and the member's term holds it closed.
        let mut enclosing = vec![false; group.length()];

        let mut proofs = Vec::with_capacity(group.length());
        for (index, type_) in types.iter().enumerate() {
            let before = kernel.partial_groups();
            let sort = infer_type(kernel, type_)?;
            enclosing[index] |= kernel.partial_groups() > before;
            proofs.push(sort.is_prop());
        }

        // The bodies are where the group's calls are: each one the walk types is recorded, graded under the arms around it, against the parameters the body's leading lambdas bind (see `kernel::calls`). The turns find calls by the elaborator's walk instead, the one call discovery both checkers take a turn by, so each reads the same members by value; the group's verdict is still the one its typed calls close to.
        kernel.open_group(group, &names);
        let mut known = vec![false; group.length()];
        for index in 0..group.length() {
            let entering = (0..group.length())
                .filter(|member| known[*member] || *member == index)
                .map(|member| (names[member], types[member].clone(), bodies[member].clone()))
                .collect();
            known[index] = known_to_terminate(kernel, entering)?;
        }
        let known_group = RecGroup::closing(
            (0..group.length())
                .filter(|member| known[*member])
                .map(|member| (names[member], types[member].clone(), bodies[member].clone()))
                .collect(),
        );
        let mut selections = known_group.members().into_iter();
        kernel.read_known(
            known
                .iter()
                .map(|known| match known {
                    true => selections.next(),
                    false => None,
                })
                .collect(),
        );

        // A group owes descent where a member is a proof or its type yields a sort, read as the bodies read it. A type stuck on a member withheld may be a sort once that member's value is, which nothing will read, so it owes descent as well.
        let withheld = names
            .iter()
            .zip(&known)
            .filter(|(_, known)| !**known)
            .map(|(name, _)| *name)
            .collect::<Vec<_>>();
        let names_withheld = |reduced: Option<&Term>| {
            reduced.is_none_or(|reduced| {
                blocker(reduced)
                    .is_some_and(|leaf| withheld.iter().any(|name| leaf.mentions_free(name)))
            })
        };
        let mut erased_member: Option<Term> = None;
        for (index, type_) in types.iter().enumerate() {
            if proofs[index] || yields_a_sort_where(kernel, type_, &names_withheld)? {
                // Named as the rest of the kernel spells the member.
                erased_member = Some(group.member_type(index));
                break;
            }
        }

        // Such a group is accepted only where every member descends, which its typed calls decide. With a member withheld it may not, and a body may be refused for reading around that member, so each body's refusal is held back until the calls have closed to a verdict that names the group where it does not descend; a withheld member unfolds nowhere, so no body runs the budget out on it.
        let held_back = erased_member.is_some() && !withheld.is_empty();
        let mut refused = None;
        for (index, (body, type_)) in bodies.iter().zip(&types).enumerate() {
            let before = kernel.partial_groups();
            kernel.begin_member(index);
            match held_back {
                false => check(kernel, body, type_)?,
                true => {
                    if let Err(error) = kernel.tried(|kernel| check(kernel, body, type_)) {
                        refused.get_or_insert(error);
                    }
                }
            }
            enclosing[index] |= kernel.partial_groups() > before;
        }
        let totality = kernel.close_group(group, enclosing);
        // A member stands in its group's own signatures and bodies as a local, which holds no group, so a position there reaching one is not seen to enclose the group by what it holds: every one that names a member of a group that does not descend encloses it, under the item that holds the group, as a position outside the group holding it closed does.
        if totality != Totality::Total {
            kernel.enclose_partial_naming(since, &names);
        }

        if let Some(type_) = erased_member
            && totality != Totality::Total
        {
            return Err(Error::NotDescending {
                type_: Box::new(type_),
            });
        }
        if let Some(error) = refused {
            return Err(error);
        }

        within(kernel, &names)
    })
}

/// Check a top-level recursive group, then bring every member into scope under the name it is exported as.
///
/// Each member is assumed at its declared type while every body is checked, so a member may call itself and its siblings. `members` parallels the group's members positionally, each the name it is exported as with the kind it was introduced as; an export is defined as the folded selection of the member it names, which is what a later item's occurrence of it reduces through, and one this check states a proof — introduced as [`DefinitionKind::Authored`], at a type that is a proposition at every assignment of its sorts — reduces through nothing. What it states of each member is handed back, in member order.
///
/// Totality is not decided here. `rec` is general recursion by design, and the obligation that keeps it sound is positional and whole-module — see `documentation/design/soundness/totality-of-the-erased-program.md`.
///
/// A group that leaves a sort to its caller is judged at every assignment of them ([`at_every_sort`]), the group read at each, and defined as it is stated. What the obligations read of it under its own spelling is what every assignment says: it descends where it does at each, and a member encloses a group that does not where it does at any.
pub fn check_rec_group(
    kernel: &mut Kernel,
    members: &[(Global, &DefinitionKind)],
    group: &RecGroup,
    universes: &UniverseContext,
) -> Result<Vec<bool>, Error> {
    if members.len() != group.length() {
        return Err(Error::Arity {
            counted: Counted::GroupMembers,
            expected: group.length(),
            actual: members.len(),
        });
    }

    let mut totality = Totality::Total;
    let mut enclosing = vec![false; group.length()];
    let mut proofs = members
        .iter()
        .map(|(_, kind)| matches!(kind, DefinitionKind::Authored))
        .collect::<Vec<_>>();
    at_every_sort(kernel, universes, |kernel, sorts| {
        let at;
        let group = match sorts.is_empty() {
            true => group,
            false => {
                // A member's quality parameters are the declaration's, so an assignment reaches them as it reaches every other term of the item.
                at = group.map_members(|body| instantiate_qualities(body, sorts));
                &at
            }
        };

        check_group(kernel, group, None, |_, _| Ok(()))?;

        for (index, proof) in proofs.iter_mut().enumerate() {
            *proof = *proof && bears_proof(kernel, &group.member_type(index)).unwrap_or(false);
            enclosing[index] |= kernel.member_encloses_partial(group, index);
        }
        if kernel.group_verdict(group) != Some(Totality::Total) {
            totality = Totality::Partial;
        }

        Ok(())
    })?;
    if universes.quality_count != 0 {
        kernel.file_group(group, totality, enclosing);
    }

    for (index, ((name, _), proof)) in members.iter().zip(&proofs).enumerate() {
        define_as_stated(
            kernel,
            name,
            &group.member_type(index),
            &Term::rec_proj(group.clone(), index),
            *proof,
            None,
            universes,
        )?;
    }

    Ok(proofs)
}

/// [`check_rec_group`]'s statements of a group whose verdict a record vouches for, the walk judging nothing of it: which members are proofs, at every assignment of its sorts, and each export defined by them.
pub fn state_rec_group(
    kernel: &mut Kernel,
    members: &[(Global, &DefinitionKind)],
    group: &RecGroup,
    universes: &UniverseContext,
) -> Result<Vec<bool>, Error> {
    let mut proofs = members
        .iter()
        .map(|(_, kind)| matches!(kind, DefinitionKind::Authored))
        .collect::<Vec<_>>();
    at_every_sort(kernel, universes, |kernel, sorts| {
        let at = group.map_members(|body| at_sorts(body, sorts));
        for (index, proof) in proofs.iter_mut().enumerate() {
            *proof = *proof && bears_proof(kernel, &at.member_type(index)).unwrap_or(false);
        }

        Ok(())
    })?;
    for (index, ((name, _), proof)) in members.iter().zip(&proofs).enumerate() {
        define_as_stated(
            kernel,
            name,
            &group.member_type(index),
            &Term::rec_proj(group.clone(), index),
            *proof,
            None,
            universes,
        )?;
    }

    Ok(proofs)
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
/// 9. Every index target inhabits the index telescope at the constructor's own parameters — `check_constructed`. A target is read by inversion and by the arm rule, so this is the judgment that types it.
/// 10. The declaration is strictly positive modulo polarity. That one is *deliberately* not here: the occurrence relation closes transitively across declarations, so it is decided over the whole set at once by [`positivity_vectors`](curios_analysis::positivity_vectors) rather than per entry.
/// 11. Its variance vector calls no universe level irrelevant that a constructor payload, index target or index type mentions, except through a position itself irrelevant. Not here either, and for clause 10's reason: a level's variance composes through the families a telescope reaches, so a declaration's vectors are computed together by [`variance_vectors`](curios_analysis::variance_vectors) in `recheck`, which files them, compares two instances by them from then on, and refuses a carried vector that claims an irrelevance past them.
///
/// # What is unspellable rather than checked
///
/// Two agreements need no clause because they cannot be violated. The indices are the terminal of the parameter telescope, so "the index telescope leads with the parameters" cannot fail. And a constructor's signature terminates in its index targets alone, so a terminal naming another family, or standing at parameters other than the declaration's own, cannot be written. And a constructor's marks are its signature's entries' own, so a signature a mark short of its binders cannot be written. A variance vector's length needs no clause either: a position it does not reach reads invariant, which is equality, and an entry past the declaration's parameter count has no recomputed counterpart, so the reconciliation denies it if it claims irrelevance and nothing reads it if it does not.
///
/// # The one repetition kept, and why
///
/// A constructor telescope still repeats the parameters as its prefix. That is not an oversight: a [`Telescope`] is *closed*, and a constructor's payload types mention the parameters, so the parameters must be bound inside it for the signature to be a self-contained value at all. Removing it needs either nesting the constructors under the arity — which buries tags, declaration order and plicities under binders they do not depend on — or context-relative terms, which is `curios-core`'s whole binder discipline rather than this type. So the repetition is the price of closedness, it is kept deliberately, and clause 7 is what makes it an enforced agreement rather than an assumed one.
///
/// # A declaration that leaves a sort to its caller
///
/// Clause 3 is read off the entry as it is stated, so the family's own sort is one sort whatever a use states: the readers that take it literally read the entry, and an entry at an assignment would hand them a literal the entry does not hold. Every clause that types a telescope is judged at every assignment of the declaration's open sorts ([`at_every_sort`]), the entry read at each ([`InductDecl::at_qualities`](curios_core::InductDecl::at_qualities)).
///
/// Call after *both* registries are seeded: a signature may name any declaration, its own family included.
pub fn check_induct_decl(kernel: &mut Kernel, declaration: &InductDecl) -> Result<(), Error> {
    curios_profile::profile!("check_induct_decl");
    check_case_counts(declaration)?;
    check_declared_sort(&declaration.result_sort)?;
    check_distinct_tags(declaration)?;

    at_every_sort(kernel, &declaration.universe_context, |kernel, sorts| {
        let at;
        let declaration = match sorts.is_empty() {
            true => declaration,
            false => {
                at = declaration.at_qualities(sorts);
                &at
            }
        };

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
    })
}

/// The declared result sort is a *literal* sort, not a term that reduces to one.
///
/// Every other consumer of this field reduces before reading it — `Sort::of` through `as_sort`, `check_signature` and `check_non_informative` through `Reducer::reduce_forced` — and exactly one does not: the `Prop`-valued index guard in [`invert_indices`](curios_analysis::invert_indices) matches `Subterm::Sort(curios_core::Sort { quality: Quality::Prop, level: _ })` on the nose. So a `result_sort` that unfolds to `Prop` would make the family a proposition to every reader but that one, which is the reader whose silence is unsound: inversion would tell a proposition's constructors apart, and the arm it excused as impossible would be reachable. Requiring the field to be literal is what makes that syntactic match correct rather than lucky; teaching each reader to reduce would instead leave the next syntactic reader to rediscover the same hole.
///
/// Nothing legitimate is refused: the surface grammar admits only the keywords `Type` and `Prop` after a declaration's `:`, so an entry the elaborator builds satisfies this by construction.
fn check_declared_sort(result_sort: &Term) -> Result<(), Error> {
    match &**result_sort {
        Subterm::Sort(curios_core::Sort {
            quality: Quality::Prop | Quality::Type,
            level: _,
        }) => Ok(()),
        Subterm::Sort(curios_core::Sort {
            quality: Quality::Param(_) | Quality::Meta(_),
            level: _,
        }) => Err(Error::OpenSort(result_sort.clone())),
        _ => Err(Error::NotASort(result_sort.clone())),
    }
}

/// No two constructors share a tag.
///
/// A tag is the elimination key and the runtime index, and every lookup resolves one by *first match* — [`InductDecl::constructor`], and `constructor_index` with it. So a repeat does not add a constructor, it hides one, and the rules that walk `constructors` entry by entry answer about the first one once per entry. Coverage shows the cost: asked whether each constructor is impossible at the scrutinee's indices, it would resolve both entries to the first and could report a family empty at an index the declaration's own second entry constructs at — certifying a refutation of a constructor the same declaration states.
///
/// Construction resolves by first match too, so the shadowed entry would have no inhabitant to hand that refutation, but that is an accident. With this clause the elimination rule ranges over constructors that are distinct, rather than being sound because an unrelated rule happens to be lossy in the same direction.
fn check_distinct_tags(declaration: &InductDecl) -> Result<(), Error> {
    // Scanned rather than collected into a set, for the reason [`InductDecl::constructor`] gives for scanning: constructor counts are small enough that the set costs more than it saves.
    for (position, tag) in declaration.constructor_order().enumerate() {
        if declaration
            .constructor_order()
            .take(position)
            .any(|earlier| earlier == tag)
        {
            return Err(Error::RepeatedTag(tag.clone()));
        }
    }

    Ok(())
}

/// Every domain of the declaration's arity is a type: its parameters, and the index telescope they terminate in.
///
/// One walk, because the indices are scoped under the parameters by construction. It reaches the parameter domains as well, which matters for a family with no constructors — nothing else would present them for sorting.
fn check_arity(kernel: &mut Kernel, declaration: &InductDecl) -> Result<(), Error> {
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
    mut parameter: impl FnMut(&mut Kernel, &Term) -> Result<(), Error>,
    mut terminal: impl FnMut(&mut Kernel, &Term) -> Result<(), Error>,
) -> Result<(), Error> {
    kernel.scoped(|kernel| {
        let mut cursor = arity.cursor();
        while let Some((_, domain)) = cursor.entry() {
            parameter(kernel, &domain)?;
            kernel.advance_assumed(&mut cursor, &domain);
        }

        let inner = cursor.body().expect("a cursor past every entry");
        let mut cursor = inner.cursor();
        while let Some((_, domain)) = cursor.entry() {
            terminal(kernel, &domain)?;
            kernel.advance_assumed(&mut cursor, &domain);
        }

        Ok(())
    })
}

/// Each case of `declaration` states at least its family's parameters and as many index targets as its family has indices, read off its telescope as it stands: every reader of a case opens its parameter prefix at the family's parameters, asserting the prefix is there, so a walk that judges anything of a module counts its cases first (`recheck`).
pub fn check_case_counts(declaration: &InductDecl) -> Result<(), Error> {
    let (parameters, indices) = (declaration.param_count(), declaration.index_count());
    for constructor in declaration.signatures() {
        let stated = constructor.telescope.len();
        if stated < parameters {
            return Err(Error::Arity {
                counted: Counted::Parameters,
                expected: parameters,
                actual: stated,
            });
        }
        let targets = constructor.telescope.terminal().len();
        if targets != indices {
            return Err(Error::Arity {
                counted: Counted::Indices,
                expected: indices,
                actual: targets,
            });
        }
    }

    Ok(())
}

/// A constructor's index targets must inhabit the family's index telescope, and its parameter prefix must be the family's own parameters.
///
/// The terminal carries the targets and nothing else — the family and its parameters are fixed by the declaration, so a terminal naming another family or standing at other parameters is unspellable rather than refused. What remains is two typing clauses: the targets are checked here, because index inversion and the arm rule both read them.
fn check_constructed(
    kernel: &mut Kernel,
    declaration: &InductDecl,
    entries: &[(Free, Term)],
    targets: &[Term],
) -> Result<(), Error> {
    // The prefix must be the declaration's parameters, not merely as many binders. `instantiate` substitutes the family's actual parameters into these slots, so a constructor declaring one at a different domain types its payload against a family this declaration does not describe — and closed telescopes are why the prefix is repeated here at all, which makes the agreement something to enforce rather than something to assume.
    //
    // The counts are [`check_case_counts`]'s, so `take` finds the whole prefix and `indices_at` as many targets as the family has indices.

    let mut cursor = declaration.arity.cursor();

    for (binder, domain) in entries.iter().take(declaration.param_count()) {
        let (_, expected) = cursor
            .entry()
            .expect("the arity holds exactly `param_count` binders");
        if !convert(kernel, &Term::type_ground(), domain, &expected)? {
            return Err(Error::Mismatch {
                inferred: Box::new(domain.clone()),
                expected: Box::new(expected),
            });
        }

        cursor.advance(Term::free_var(binder));
    }
    let declared = cursor.into_args();

    let telescope = declaration.indices_at(&declared);
    let mut cursor = telescope.cursor();
    for target in targets {
        // Loud rather than a `break`, because the quiet exit fails open: every target past it would stand unchecked, and inversion and the arm rule read them.
        let (_, domain) = cursor
            .entry()
            .expect("the index telescope holds exactly `index_count` binders");

        check(kernel, target, &domain)?;
        cursor.advance(target.clone());
    }

    Ok(())
}

/// What a well-formed `struct` registry entry asserts: its universe context (clause 1), its absence of residue (clause 2), that its `result_sort` is a literal sort (clause 3), the size condition over its field telescope (clause 6), and that a `Prop`-sorted structure carries only proofs — see `check_non_informative`. Positivity and variance are decided over the whole set as they are for an `induct`, a field read as a payload is.
///
/// A `struct` has no constructors and no index targets, so clauses 5 through 9 have nothing to range over — and clause 7 could not be violated even if it did, since a structure's fields are the terminal of its own `arity` rather than a separately-stored telescope repeating the parameters: the same nesting, and the same reason, as [`InductDecl::arity`].
///
/// One that leaves a sort to its caller is read as [`check_induct_decl`] reads an `induct`'s: its own sort off the entry as stated, and its telescopes at every assignment.
pub fn check_struct_decl(kernel: &mut Kernel, declaration: &StructDecl) -> Result<(), Error> {
    curios_profile::profile!("check_struct_decl");
    check_declared_sort(&declaration.result_sort)?;

    at_every_sort(kernel, &declaration.universe_context, |kernel, sorts| {
        let at;
        let declaration = match sorts.is_empty() {
            true => declaration,
            false => {
                at = declaration.at_qualities(sorts);
                &at
            }
        };

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
    })
}

/// A `Prop`-sorted structure's fields must all be non-informative, which means every one of them is a proof.
///
/// Proof irrelevance makes any two inhabitants of a proposition definitionally equal, and a structure's payload is read back by *projection*, which is not an elimination and so meets no large-elimination guard. A `Prop` structure carrying a `Nat` therefore hands the same field two convertible inhabitants with different values, and `Eq` plus congruence turns that into `False`. A field carrying a *type* does the same thing one level up — the two convertible inhabitants hand the projection two different types — so being erased buys it no exemption; see [`carries_information`].
///
/// Parameters are skipped: they are the family's arguments rather than stored payload, so a proposition may be indexed by data without carrying any. Inductives are deliberately not subject to this — `induct Box : Prop | mk(n : Nat)` is a legal declaration whose *elimination* the singleton rung guards instead.
fn check_non_informative(kernel: &mut Kernel, declaration: &StructDecl) -> Result<(), Error> {
    if !matches!(
        &*kernel.reduce_forced(declaration.result_sort.clone())?,
        Subterm::Sort(curios_core::Sort {
            quality: Quality::Prop,
            level: _
        })
    ) {
        return Ok(());
    }

    // The parameters are skipped rather than counted past: they are the arity's own binders, and the fields it terminates in are exactly the stored payload this rule is about.
    walk_arity(
        kernel,
        &declaration.arity,
        |_, _| Ok(()),
        |kernel, type_| match carries_information(kernel, type_)? {
            true => Err(Error::Informative {
                field: Box::new(type_.clone()),
            }),
            false => Ok(()),
        },
    )
}

/// Walk one declaration telescope, typing each domain and requiring each to sit at or below the declared result level — one rung higher for the leading `uniform` binders, which are the declaration's parameters.
///
/// A `Prop`-sorted result bounds its telescope as a `Type`-sorted one does, and a `Prop`-sorted domain counts at its own level, whichever sort the result is. The size order reads an application of a payload as a child of its constructor, which holds only of a payload that cannot be applied at the family holding it: a payload quantifying over the sort its family sits in can, and hands back the value it was given.
///
/// **Typing the domain is a separate clause from sizing it.** The size test reads a domain only where the result reduces to a sort, and nothing else covers the domains: `check_arity` reaches an `induct`'s parameters and indices but not its constructor telescopes, a `struct` has no `check_arity`, and `check_non_informative` only *classifies* each field with `Sort::of`, so a field type that lies about its own sort would stand unexamined; see `recheck::proposition_tests::a_proposition_may_not_carry_a_computed_relevant_field`.
fn check_signature<B: Bound + Clone>(
    kernel: &mut Kernel,
    telescope: &Telescope<B>,
    uniform: usize,
    result_sort: &Term,
    terminal: impl FnOnce(&mut Kernel, &[(Free, Term)], &B) -> Result<(), Error>,
) -> Result<(), Error> {
    kernel.scoped(|kernel| {
        let result = kernel.reduce_forced(result_sort.clone())?;
        // A result that is no sort imposes no size condition, but the walk still runs: the terminal clause below is owed whatever the result is.
        let sized = match &*result {
            Subterm::Sort(curios_core::Sort {
                quality: _,
                level: bound,
            }) => Some((bound.clone(), bound.checked_add(1)?)),
            _ => None,
        };

        let mut entries: Vec<(Free, Term)> = Vec::new();
        let mut cursor = telescope.cursor();

        while let Some((_, domain)) = cursor.entry() {
            let sort = infer_type(kernel, &domain)?;

            if let Some((bound, raised)) = &sized {
                let (Sort::Type(level) | Sort::Prop(level)) = sort;
                let upper = if entries.len() < uniform {
                    raised
                } else {
                    bound
                };

                if !kernel.level_leq(&level, upper) {
                    return Err(Error::Oversized {
                        domain: level,
                        bound: upper.clone(),
                    });
                }
            }

            let binder = kernel.advance_assumed(&mut cursor, &domain);
            entries.push((binder, domain));
        }

        let body = cursor.body().expect("the loop exits only at the terminal");

        terminal(kernel, &entries, &body)
    })
}

/// Check a term that closes the program — an entrypoint body, with no name to export — against the type its entry states: the program contract a compile supplies, or whatever the builder of the entry stated instead, so the kernel holds a program to its contract itself rather than taking elaboration's word that it was.
pub fn check_entrypoint(kernel: &mut Kernel, body: &Term, type_: &Term) -> Result<(), Error> {
    curios_profile::profile!("check_entrypoint");
    kernel.restore_budget();
    kernel.assume_universes(&UniverseContext::empty());

    infer_type(kernel, type_)?;
    check(kernel, body, type_)
}
