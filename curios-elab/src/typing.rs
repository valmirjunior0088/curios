#[cfg(test)]
mod tests;

use {
    super::{
        AwaitId, Binders, Choice, Context, Error, FrozenFrame, Mode, Opened, Outcome, ParkedChoice,
        ParkedProblem, ParkedWork, Problem, Sort, Watched, answer_witness, callee, convert,
        diagnose_embedding, diagnose_shape, elaborate, elaborate_func_settle, embeds, is_monad,
        monad_shape, normalize, numeral_default, probe_open_sort, problem_outcome, reduce_forced,
        refold_recs, refused_scrutinee, retry_discharge, retry_match, retry_projection,
        retry_witness, settle_qualities, shallow_scrutinee, subsume_outcome, synth_neutral,
        zonk_solved_term_metas,
    },
    curios_analysis::{
        RESOLVED_SPELLING_LAYERS, Unfolding, another_case, records_case_equation,
        scrutinee_solution,
    },
    curios_core::{
        Advance, Apply, Bound, Extent, Field, Free, Func, FuncType, Global, ImplicitOrigin,
        Intrinsic, Many, Metavar, MetavarId, MetavarOrigin, Produced, Proj, Quality, ReduceError,
        Scope, Subterm, Telescope, Term, Transient, Universes, Visit,
    },
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
        slice,
    },
};

/// Synthesis is just `elaborate` in `Infer` mode, projecting out the type.
pub(crate) fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Infer).map(|(_, type_)| type_)
}

/// Checking counterpart to `infer`: `elaborate` in `Check` mode, returning the *elaborated* term. Drives `term` against a known `ty` — the rebuilt, de-Bruijn-correct subterm whose lambda domains are solved and whose binders are re-closed. Elaboration is authoritative: this output, not the original lowered term, is what flows on to `zonk`/`erase`.
pub(crate) fn check(context: &mut Context, term: &Term, ty: Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Check(ty)).map(|(term, _)| term)
}

pub(crate) fn reduce_with(context: &mut Context, term: &Term) -> Result<Term, Error> {
    reduce_forced(context, term.clone()).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::reduce_exhausted(term.clone(), refusal)
        })
    })
}

/// `super::convert` with its `ReduceError` mapped to `Error`, at an explicit type so proof-irrelevance and eta fire at the terms' real sort. The inverter uses it to compare a binder's competing forcings at the binder's own type.
pub(crate) fn convert_at(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    convert(context, type_, this, that).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::convert_exhausted(this.clone(), that.clone(), refusal)
        })
    })
}

/// The sort term (`Type`/`Prop`) `type_` inhabits — what a type-former reports as its type-of-a-type, so a proposition checks against `Prop`. Wraps `Sort::of`, mapping an exhausted budget to an error like the helpers above.
pub(crate) fn sort_term(context: &mut Context, type_: &Term) -> Result<Term, Error> {
    Sort::of(context, type_).map(Sort::term).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::reduce_exhausted(type_.clone(), refusal)
        })
    })
}

/// Whether `type_` is a strict proposition (its sort is `Prop`). Wraps `Sort::of`, mapping an exhausted budget like the helpers above.
pub(crate) fn is_prop(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    is_prop_in(context, &mut Vec::new(), type_, "is_prop")
}

/// [`is_prop`] under the binders a surrounding walk has opened, asked by the reader named `reader` ([`Sort::is_prop_to`]).
///
/// The binders are threaded rather than assumed into the [`Context`], because `Context::assume` bumps the mutation stamp that validates the memoization caches. A seeding walk that assumed instead would invalidate them at every binder it descended through.
fn is_prop_in(
    context: &mut Context,
    opened: &mut Vec<(Free, Term)>,
    type_: &Term,
    reader: &'static str,
) -> Result<bool, Error> {
    Sort::of_in(context, opened, type_)
        .map(|sort| sort.is_prop_to(context, reader))
        .map_err(|error| {
            Error::from_reduce(error, |refusal| {
                Error::reduce_exhausted(type_.clone(), refusal)
            })
        })
}

/// The quality of the sort `type_` inhabits, as far as it is solved. Wraps `Sort::of`, mapping an exhausted budget like the helpers above.
pub(crate) fn quality_of(context: &mut Context, type_: &Term) -> Result<Quality, Error> {
    Sort::of(context, type_)
        .map(|sort| sort.quality)
        .map_err(|error| {
            Error::from_reduce(error, |refusal| {
                Error::reduce_exhausted(type_.clone(), refusal)
            })
        })
}

/// Hold `type_` to a proposition where a rule admits nothing else of it: answers whether it is one, having decided its sort where that was a quality nothing had solved. A type, and a sort its declaration leaves to a caller, is none. `site` is the term the rule is asked of, which a `Sort` this decides is reported with.
pub(crate) fn hold_prop(
    context: &mut Context,
    type_: &Term,
    site: Option<Extent>,
) -> Result<bool, Error> {
    let quality = quality_of(context, type_)?;
    probe_open_sort(context, "hold_prop", quality);

    Ok(match quality {
        Quality::Prop => true,
        quality @ Quality::Meta(_) => context.same_quality_at(quality, Quality::Prop, site),
        Quality::Type | Quality::Param(_) => false,
    })
}

/// What a term at `type_` is called where it owes what a proof owes, and `None` where it owes nothing: a proof where the type is a proposition, and a value at a sort its declaration leaves to a caller, which is a proof at the use that states `Prop`. What the termination of an erased position is taken by, where [`is_prop`] answers for what is known a proof.
pub(crate) fn proof_position(
    context: &mut Context,
    type_: &Term,
) -> Result<Option<&'static str>, Error> {
    let quality = quality_of(context, type_)?;
    probe_open_sort(context, "proof_position", quality);

    Ok(match quality {
        Quality::Prop => Some("a proof"),
        Quality::Param(_) => Some("a value at a sort left to a caller"),
        Quality::Type | Quality::Meta(_) => None,
    })
}

/// Whether `type_`, as written, is an application of a declaration that lands in `Prop` ([`Sort::declared_into_prop`]), under the binders a surrounding walk has opened: what erasure's classifier asks before it reduces a type to read it.
pub(crate) fn is_declared_prop_in(
    context: &mut Context,
    opened: &Opened,
    type_: &Term,
) -> Result<bool, Error> {
    Sort::declared_into_prop(context, &Binders::default(), opened, type_)
        .map(|level| level.is_some())
        .map_err(|error| {
            Error::from_reduce(error, |refusal| {
                Error::reduce_exhausted(type_.clone(), refusal)
            })
        })
}

/// Whether a value of `type_` carries nothing at run time, under the binders a surrounding walk has opened, each with its declared type: `type_` is a sort, so the value is a type; a proposition, so the value is a proof; or a function that ultimately answers one of those. The empty tuple is none of them, being what an effect answers, so `{}` and `X -> {}` are kept.
///
/// A domain that is a bound variable of `Prop` sort — `proof: P` under `P: Prop` — is a proof only through `opened`, which is threaded rather than assumed for the reason [`is_prop_in`] states.
///
/// **One reading's answer.** A domain that is a variable here is a proof where the variable is instantiated at a proposition, so a `false` says only that this reading keeps the value. A `true` holds at every instantiation: a sort, a proposition and a function into one stay what they are under any substitution. Erasure asks through its reader ([`crate::Retyping`]), of a declaration's signature for what the declaration stores and of a slot's type for what the slot holds.
pub(crate) fn is_erasable_in(
    context: &mut Context,
    opened: &mut Vec<(Free, Term)>,
    type_: &Term,
) -> Result<bool, Error> {
    // A proposition known by its head is erased with nothing run to read it.
    if is_declared_prop_in(context, opened, type_)? {
        return Ok(true);
    }

    match Term::unwrap_or_clone(reduce_with(context, type_)?) {
        Subterm::Sort(..) => Ok(true),
        Subterm::Instance(instance) => is_erasable_in(context, opened, &instance.head.to_term()),
        // A function erases iff what it ultimately returns does — a proof-/type-producing function is pure, content-free; an effectful `X -> {}` is not. Recurse past the parameters into the codomain, each opened binder joining `opened` so the codomain can read its sort.
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let mark = opened.len();
            let mut cursor = telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                opened.push((cursor.advance_fresh(|hint| context.fresh(hint)), domain));
            }
            let output = cursor.body().expect("a cursor past every entry");
            let result = is_erasable_in(context, opened, &output);
            opened.truncate(mark);
            result
        }
        _ => is_prop_in(context, opened, type_, "is_erasable_in"),
    }
}

/// Whether a value of `type_` is a proof, under the binders a surrounding walk has opened, each with its declared type: `type_` is a proposition, or a function that ultimately answers one. A sort is neither, so a type and a family are not.
///
/// It is what a published item's binders are asked, once, as the item is published ([`crate::state_proof_binders`]), and what the closed machine then leaves as written. The question is narrower than [`is_erasable_in`]'s on purpose. Erasure computes neither a proof nor a type, but a judgment reads a type: conversion compares the reduct it is handed, and two instances of a definition apart in a level are one type or two by whether they reach it as written or as what they unfold to. A proof's value is read by nothing.
///
/// A `true` holds at every instantiation, as [`is_erasable_in`]'s does, and a `false` says only that this reading computes the value.
pub(crate) fn is_proof_in(
    context: &mut Context,
    opened: &mut Vec<(Free, Term)>,
    type_: &Term,
) -> Result<bool, Error> {
    // A proposition known by its head is one with nothing run to read it.
    if is_declared_prop_in(context, opened, type_)? {
        return Ok(true);
    }

    match Term::unwrap_or_clone(reduce_with(context, type_)?) {
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let mark = opened.len();
            let mut cursor = telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                opened.push((cursor.advance_fresh(|hint| context.fresh(hint)), domain));
            }
            let output = cursor.body().expect("a cursor past every entry");
            let result = is_proof_in(context, opened, &output);
            opened.truncate(mark);
            result
        }
        _ => is_prop_in(context, opened, type_, "is_proof_in"),
    }
}

/// Elaborate `term` as a type/proposition without inventing an arbitrary expected universe upper bound.
pub(crate) fn check_is_sort(context: &mut Context, term: &Term) -> Result<(Term, Sort), Error> {
    let (rebuilt, inferred) = if matches!(&**term, Subterm::Metavar(_)) {
        let sort = context.fresh_sort("type left to inference");
        elaborate(context, term, Mode::Check(sort))?
    } else {
        elaborate(context, term, Mode::Infer)?
    };

    match &*reduce_with(context, &inferred)? {
        Subterm::Sort(quality, level) => Ok((
            rebuilt,
            Sort {
                quality: context.universes().quality(*quality),
                level: level.clone(),
            },
        )),
        // The reduct is not what is reported: it spells a recursive name as its group, which the display form gives back its name. Located here, at the term written where a type is wanted: the refusal is raised once its elaboration has returned, so no frame of that term is left to place it.
        _ => Err(Error::type_mismatch(
            resolved_for_display(context, &inferred),
            Term::type_ground(),
        )
        .at_opt(term.extent())),
    }
}

/// Best-effort display form for a mismatch report: substitute the solutions that have landed, so the message names the actual disagreement rather than the metavariables it arrived wrapped in, then deep-[`normalize`] the result so a stuck concept-method projection standing in an index position collapses to the value it denotes (`Vec(Nat, (sys/witness@0).0(0, 1))` → `Vec(Nat, 1)`) rather than surfacing compiler-internal witness machinery. Materialization is tolerant: a metavariable still open spells `?` while every solved one beside it shows its value — the strict `zonk` would refuse the whole term on the first open hole, so one unsolved `f` would render `?(?)` where `?(double(p))` is known. Universe levels come through verbatim and the report's spelling erases them. A normalization that exhausts its budget falls back to the merely-materialized form.
///
/// Normalization is the whole story only while the operand type is concrete. Under a `use Add(A)` parameter the projection is stuck on an abstract witness and no amount of reduction reaches the operator; the printer spells it, against the witness binders the report carries (axis (h)).
pub(crate) fn resolved_for_display(context: &mut Context, term: &Term) -> Term {
    let term = &numerals_at_their_default(context, term);
    // Refolded on both sides of normalization. Before: a committed solution spells a stuck recursive call as its canonical neutral, the `Rec` node itself, and `normalize` keeps a name only where the *written* head is one (`stalled_unfolding`) — so the node is first given back its name, which the stall rule then holds. After: whatever normalization exposed elsewhere.
    let zonked = refold_recs(
        context,
        &settle_qualities(context, &zonk_solved_term_metas(context, term)),
    );
    let resolved = normalize(context, zonked.clone()).unwrap_or(zonked);
    refold_recs(context, &resolved)
}

/// `term` with each metavariable parked numerals wait on shown as the default they would take together. A report can be made before their term completes — a turnaround that fails while a numeral still waits — and would show `Option(?)` for `Option/some(1)`, whose type every reader knows. A side that is such a metavariable itself is left as it stands: the report is then about what that metavariable may be, which the default does not answer.
fn numerals_at_their_default(context: &mut Context, term: &Term) -> Term {
    if !term.has_metavar() || matches!(&**term, Subterm::Metavar(_)) {
        return term.clone();
    }
    let waiting = context.waiting_numerals();
    if waiting.is_empty() {
        return term.clone();
    }

    let zonked = zonk_solved_term_metas(context, term);
    let defaults = waiting
        .iter()
        .map(|(id, literals)| (*id, numeral_default(context, literals)))
        .collect::<BTreeMap<_, _>>();
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term: &Term| match &**term {
            Subterm::Metavar(metavar) => defaults.get(&metavar.id).cloned(),
            _ => None,
        }),
    );

    zonked.traverse(&mut visit)
}

/// A `type_mismatch` error naming both sides in their best-effort display form (see [`resolved_for_display`]) — unless `term`, the node the conversion was about, is the `/std/Monad/bind` application a postfix `!` desugars to and the region it hoisted to has nothing to sequence in.
///
/// `bind` produces `M(B)`, so the generic report names a type the author never wrote, mentions neither the `!` nor the region that rejected it, and leaks the still-unsolved `B`. The specialized report names the monad the region would have to be and the type it actually has.
pub(crate) fn display_mismatch(
    context: &mut Context,
    term: &Term,
    this: &Term,
    that: &Term,
) -> Error {
    if let Some(report) = sort_decided_twice(context, this, that) {
        return report;
    }

    let this = resolved_for_display(context, this);
    let that = resolved_for_display(context, that);

    if is_sequencing(context, term) && wraps_a_result(&this) && !wraps_a_result(&that) {
        return Error::stranded_sequencing(required_region_type(context, &this), that);
    }
    if let Some(report) = unembedded_action(context, &this, &that) {
        return report;
    }
    if let Some(report) = implicit_at_other_sort(context, &this, &that) {
        return report;
    }

    Error::type_mismatch(this, that)
}

/// The specialized report for two sorts apart where one is a `Sort` the declaration wrote and an earlier term decided: the reader wrote neither sort, and what refuses this term is the other's decision. Read off the sides as conversion held them: the display form settles each quality to the sort it was solved to, and a written `Sort` is told from that sort nowhere after.
fn sort_decided_twice(context: &mut Context, this: &Term, that: &Term) -> Option<Error> {
    let inferred = reduce_forced(context, this.clone()).ok()?;
    let expected = reduce_forced(context, that.clone()).ok()?;
    let (Subterm::Sort(inferred, _), Subterm::Sort(expected, _)) = (&*inferred, &*expected) else {
        return None;
    };
    let universes = context.universes();

    // The side that is the written `Sort` states what the earlier term decided, and the other what this one needs of it.
    [(*inferred, *expected), (*expected, *inferred)]
        .into_iter()
        .find_map(|(needed, written)| {
            let other = universes.written_decision(written)?;
            match universes.quality(needed) {
                needed @ (Quality::Prop | Quality::Type)
                    if needed != universes.quality(written) =>
                {
                    Some(Error::sort_decided_twice(needed.is_prop(), other))
                }
                _ => None,
            }
        })
}

/// Where a term of the declaration decided the sort `type_` stands at, that sort being a `Sort` the declaration wrote ([`UniverseSolver::written_decision`](crate::UniverseSolver::written_decision)): the term, where its check was recorded. `None` where `type_` stands at no such sort. A refusal that follows from the decision asks before it names a sort its reader did not write.
///
/// A type at a written `Sort` is a variable or an application of one, so the sort is read off the neutral's own type, where the written quality still stands for itself.
pub(crate) fn decided_sort_of(context: &mut Context, type_: &Term) -> Option<Option<Extent>> {
    let reduced = reduce_forced(context, type_.clone()).ok()?;
    let sort = synth_neutral(context, &Binders::default(), &[], &reduced).ok()??;

    match &*reduce_forced(context, sort).ok()? {
        Subterm::Sort(quality, _) => context.universes().written_decision(*quality),
        _ => None,
    }
}

/// The specialized report for an argument handed where an implicit argument declared at the other sort is still open: `this` is the one candidate the argument offers the implicit `that` stands for, a proposition where the implicit is declared at `Type` and a type where it is declared at `Prop`, and neither sort admits what stands at the other, so the implicit stays unsolved and the generic report names it `?`.
///
/// Where `this` stands at a `Sort` the declaration wrote, an earlier term decided the sort it is at, and the report is that the two terms decide it apart.
fn implicit_at_other_sort(context: &mut Context, this: &Term, that: &Term) -> Option<Error> {
    let Subterm::Metavar(Metavar {
        id,
        origin: MetavarOrigin::Implicit(ImplicitOrigin { func, binder }),
        ..
    }) = &**that
    else {
        return None;
    };
    if context.metavar_solution(*id).is_some() {
        return None;
    }
    let declared = context.metavar_entry(*id)?.result.clone();
    let declared = reduce_forced(context, declared).ok()?;
    let Subterm::Sort(declared, _) = &*declared else {
        return None;
    };
    let declared = context.universes().quality(*declared);

    // A type still stuck on a metavariable is read as a type's where nothing states its sort, which says nothing of what it comes to.
    let given = reduce_forced(context, this.clone()).ok()?;
    if stuck_on_metavar(context, &given) {
        return None;
    }
    let proposition = match (quality_of(context, this).ok()?, declared) {
        (Quality::Prop, Quality::Type) => true,
        (Quality::Type, Quality::Prop) => false,
        _ => return None,
    };
    if let Some(other) = decided_sort_of(context, this) {
        return Some(Error::sort_decided_twice(!proposition, other));
    }

    Some(Error::implicit_at_other_sort(
        callee(context, func),
        binder.clone(),
        this.clone(),
        proposition,
    ))
}

/// The specialized report for a mismatch between two monad applications that differ in their head or a context argument: an action of one monad where another is expected. The `!` and tail oracles embed such an action through the declared `Lift` witness when its monad can be read from its head's declaration, so this is the case they could not read — a projection, a computed head — or a position they never look at, and the explicit `lift` spelling is the remedy the report names. `None` for every other mismatch, which keeps the generic report — and for one whose shapes cannot be read, exhaustion included, since the refusal this decorates is already the verdict.
fn unembedded_action(context: &mut Context, this: &Term, that: &Term) -> Option<Error> {
    let this_whnf = reduce_forced(context, this.clone()).ok()?;
    let that_whnf = reduce_forced(context, that.clone()).ok()?;
    let action = monad_shape(context, &this_whnf).ok().flatten()?;
    let expected = monad_shape(context, &that_whnf).ok().flatten()?;
    let differ = embeds(&expected, &action)
        && is_monad(context, &action.head)
        && is_monad(context, &expected.head);

    differ.then(|| Error::unembedded_action(this.clone(), that.clone()))
}

/// Whether `term` is a `/std/Monad/bind` application — what every `!` lowers to, and the only shape whose result type is an `M(B)` a stranded sequencing can clash on.
fn is_sequencing(context: &Context, term: &Term) -> bool {
    let Subterm::Apply(Apply { head, .. }) = &**term else {
        return false;
    };

    head.head_name() == Some(&Free::global(context.syntax().monad.bind.qualifier()))
}

/// Whether `type_` stands over a result: a former applied to at least one argument, which is the shape `M(B)` always has.
///
/// Read on the expected side, this is what separates a stranded `!` from an ordinary mismatch inside a real region — `Io(Str)` against `Io(Nat)` disagrees about the result, while `Io(_)` against `Nat` is a region that cannot sequence at all. `Io` is named among the intrinsics because it is the one monad whose former is an intrinsic node rather than an application or a parameterized nominal type.
fn wraps_a_result(type_: &Term) -> bool {
    match &**type_ {
        Subterm::Apply(_) | Subterm::Intrinsic(Intrinsic::IoType(_)) => true,
        Subterm::InductType(induct) => !induct.params.is_empty(),
        Subterm::StructType(struct_) => !struct_.params.is_empty(),
        _ => false,
    }
}

/// What a stranded-sequencing report names as the region's required type: the sequenced `M(B)` with every metavariable it still carries blanked to one binder rendered `_`. The report wants `M(B)` for its `M` alone — `B` is what the continuation would have produced, and a region that cannot sequence never determines it.
///
/// `None` when `M` is itself one of those holes: nothing pinned the monad, so the whole spelling would be placeholders and the report says "a monad" instead.
fn required_region_type(context: &mut Context, sequenced: &Term) -> Option<Term> {
    if let Subterm::Apply(Apply { head, .. }) = &**sequenced
        && matches!(&**head, Subterm::Metavar(_))
    {
        return None;
    }

    let hole = Term::free_var(&context.fresh(Some("_")));
    // Memoized on node identity: every metavariable blanks to the one hole, so a node's answer is the node's alone.
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term: &Term| {
            matches!(&**term, Subterm::Metavar(_)).then(|| hole.clone())
        }),
    );

    Some(sequenced.traverse(&mut visit))
}

/// Hold `term`, of type `inferred`, to the type `expected`: the subsumption relation `A ≤ B`, as `curios_cert`'s `subsumes` decides it — `Type u ≤ Type v` and `Prop u ≤ Prop v` under the level algebra, `Π(x:A).B ≤ Π(x:A').B'` when `A ≡ A'` and `B ≤ B'`, and conversion otherwise. See `documentation/design/theory/subsumption-is-a-relation-not-a-traversal-order.md`.
///
/// **The function-type case is what makes this a relation rather than a traversal artifact.** Checking a λ against a Π pushes the comparison to the leaves, where a head-only rule suffices; checking a *name* against one does not, and the Π is formed and compared whole. Domains stay invariant, as they do in the kernel: reading one covariantly would hand a function that takes only small arguments a large one.
///
/// It is asked of conversion at [`Relation::Below`](crate::Relation::Below), so the relation is the problem's and no property of a walk made here: a function type's domains are equations and its codomain keeps the relation, a pair that waits on a metavariable is parked as the subsumption it is, and its retry resumes as one.
pub(crate) fn expect(
    context: &mut Context,
    term: &Term,
    inferred: &Term,
    expected: &Term,
) -> Result<(), Error> {
    curios_profile::profile!("typing::expect");
    let outcome = subsume_outcome(context, inferred, expected, term.extent()).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::convert_exhausted(inferred.clone(), expected.clone(), refusal)
        })
    })?;

    match outcome {
        Outcome::Converts => context.retry_parked(),
        Outcome::Mismatch(declined) => {
            Err(display_mismatch(context, term, inferred, expected).declined(declined))
        }
        // Undecided: blocked on unsolved metavariables. Park the goals to be retried when a watched metavariable is solved and succeed provisionally — unless this is inside an oracle, where provisional success may not leak into the answer: the turnaround fails there, and the oracle records that it would have parked.
        Outcome::Blocked(goals) => {
            if !context.may_park() {
                return Err(display_mismatch(context, term, inferred, expected));
            }

            for goal in goals {
                context.park(ParkedWork::Conversion(goal), term.clone());
            }
            context.retry_parked()
        }
    }
}

/// Whether a *weak-head-reduced* term is stuck on an unsolved metavariable — headed by one, applied from one, eliminated on one, or projected from one.
///
/// The four shapes are one fact: reduction cannot proceed until some metavariable is solved, so nothing about the term is decided yet. Conversion already treats each of them as [`Outcome::Blocked`](crate::Outcome) rather than as a mismatch; this is the same reading for the checked-only introduction forms, which otherwise refuse a program one step before the turnaround that would have solved the blocker. `Count(?L)` is the case that motivates the `Match` arm: the payload type of a description-indexed constructor, stuck until the index is known.
pub(crate) fn stuck_on_metavar(context: &Context, reduced: &Term) -> bool {
    match &**reduced {
        Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
        Subterm::Match(matched) => stuck_on_metavar(context, &matched.head),
        Subterm::Proj(proj) => stuck_on_metavar(context, &proj.head),
        Subterm::Apply(apply) => {
            // A folded recursive call is the fourth shape, and it does not wear its blocker on its head: `force_rec` hands the call itself back when unfolding cannot choose an arm, so what reduction is waiting for sits in an *argument*. `Count(?L)` is that term — a description-indexed payload type before its index is known. An argument that is rigid instead blocks on nothing a solution could change, and rightly refuses.
            stuck_on_metavar(context, &apply.head)
                || (apply.head.as_rec_proj().is_some()
                    && apply.params().any(|param| stuck_on_metavar(context, param)))
        }
        _ => false,
    }
}

/// Whether every domain of `func` is written: a lambda whose type its annotations and body state, with nothing left for a term beside it to give.
fn annotated(context: &mut Context, func: &Func) -> bool {
    let mut cursor = func.telescope.cursor();
    while let Some((_, domain)) = cursor.entry() {
        if matches!(&*domain, Subterm::Metavar(metavar) if metavar.is_hole()) {
            return false;
        }
        cursor.advance_fresh(|hint| context.fresh(hint));
    }

    true
}

/// Whether `arg` is a form that cannot be elaborated yet because the type structure it needs is an unsolved metavariable: a tuple literal or a numeral whose whole expected type, or a lambda one of whose expected *domains*, reduces to one. (A lambda only needs its domains known: the body, which may project any parameter, can't be checked against an unknown domain; its codomain may stay a metavariable.) Synthesizable forms return `false`: they have a turnaround of their own and must run eagerly so their solutions feed the result unification. A list literal is one of them: it is a list whatever it holds, and each element waits by itself.
pub(crate) fn blocked_on_metavar(
    context: &mut Context,
    arg: &Term,
    ty: &Term,
    result_metavars: &BTreeSet<MetavarId>,
    expected_ground: bool,
) -> Result<bool, Error> {
    let is_lambda = matches!(&**arg, Subterm::Func(_));
    let is_tuple = matches!(&**arg, Subterm::Tuple(_));
    // A numeral waits as they do, on its whole type and nothing else: it is realized at whatever that comes to.
    let is_numeral = matches!(&**arg, Subterm::Transient(Transient::NumLit(_)));
    if !is_lambda && !is_tuple && !is_numeral {
        return Ok(false);
    }
    let reduced = reduce_with(context, ty)?;
    if stuck_on_metavar(context, &reduced) {
        // A tuple, a numeral or a lambda whose whole expected type is stuck on an unsolved metavar — a bare one, or a reduction waiting on one.
        return Ok(true);
    }
    Ok(match &*reduced {
        Subterm::FuncType(FuncType { telescope, .. }) if is_lambda => {
            // A lambda one of whose expected *domains* is stuck: its body may need that domain's structure (to project the parameter), so postpone it until a sibling argument (e.g. `p : Parse(Bytes, A)`, or the fold's initial accumulator) pins the domain. Every binder the lambda itself has is looked at, not the first alone — `(t, (acc, i)) => …` projects its second parameter — each opened under a fresh variable so a later domain's dependency on an earlier binder is a stuck reduction rather than a bound index.
            let arity = match &**arg {
                Subterm::Func(func) => func.telescope.len(),
                _ => 0,
            };
            let mut walk = telescope.cursor();
            let mut domain_blocked = false;
            for _ in 0..arity {
                let Some((_, domain)) = walk.entry() else {
                    break;
                };
                let reduced_domain = reduce_with(context, &domain)?;
                if stuck_on_metavar(context, &reduced_domain) {
                    domain_blocked = true;
                    break;
                }
                walk.advance_fresh(|hint| context.fresh(hint));
            }
            // ...or a lambda whose *codomain* still carries an unsolved metavar that the result type will pin: postpone until `expect(output, expected)` solves it, so the body is checked against the refined codomain. This is the `let !`-continuation case — `(x) => …` checked against `?dom => Parse(Bytes, ?B)`, where `?dom` is already pinned by the bind's action but `?B` (the bind's own result type) is solved only by the turnaround. Gating on `result_metavars` keeps it to metavars `expect` will address; gating on `expected_ground` ensures that turnaround actually grounds `?B` (vs. a flex-flex alias that the eager body must ground instead).
            domain_blocked
                || (expected_ground
                    && reduced.metavars().iter().any(|id| {
                        result_metavars.contains(id) && context.metavar_solution(*id).is_none()
                    }))
        }
        _ => false,
    })
}

/// Whether metavar `id` is solved *all the way down*: solved, and every metavar in its solution is itself transitively ground. `metavar_solution` only sees one level, and a solution can still embed unsolved metavars, so `expected_ground` needs this transitive view to be sure the turnaround will actually pin a result metavar rather than alias it flex-flex. Terminates: the occurs check forbids cyclic solutions.
pub(crate) fn transitively_ground(context: &Context, id: MetavarId) -> bool {
    match context.metavar_solution(id) {
        None => false,
        Some(solution) => solution
            .metavars()
            .iter()
            .all(|&inner| transitively_ground(context, inner)),
    }
}

impl Context<'_> {
    /// Retry parked constraints woken by freshly landed solutions, to fixpoint. A woken goal re-runs under its frozen frame: converts and is dropped, mismatches and errors at its origin, or re-parks still blocked. Each round consumes wake signals and ids solve exactly once, so this terminates.
    pub(crate) fn retry_parked(&mut self) -> Result<(), Error> {
        // Never retry inside an oracle: re-validation reads every failure but exhaustion as a rejected candidate, so a woken goal's mismatch would vanish along with the goal itself — a silently dropped obligation. The wake signals stay queued; the next unsuppressed turnaround retries them.
        if self.parking_suppressed() {
            return Ok(());
        }
        curios_profile::profile!("typing::retry_parked");

        loop {
            let woken = self.wake_parked();
            if woken.is_empty() {
                return Ok(());
            }

            for parked in woken {
                retry_one(self, parked)?;
            }
        }
    }

    /// Complete the awaiting term `term`, which has read its parts and met its expectation: retry what a solution woke, take one choice among the problems it awaits, and start again, until none is left to take. Each choice is taken only here, where nothing the term reads can inform it any more, so what the term is admitted at does not follow the order of its parts. Inside an oracle nothing parks, so nothing waits to be taken.
    pub(crate) fn complete(&mut self, term: Option<AwaitId>) -> Result<(), Error> {
        if self.parked_len() == 0 || self.parking_suppressed() {
            return Ok(());
        }

        let mut taken = BTreeSet::new();
        loop {
            self.retry_parked()?;
            if !self.take_choice(term, &mut taken)? {
                return Ok(());
            }
        }
    }

    /// Take one choice among the problems `term` awaits, the lowest-ranked there is ([`Choice`]): a `use` slot's witness, then a literal's shape, then the numerals' default, then a lambda, at the function type its expectation has come to or, where it has come to none, at the type its annotations state. `taken` holds what was taken already — a metavariable shaped or defaulted, a lambda's placeholder — so a choice conversion could not commit is not offered again. Answers whether one was taken.
    fn take_choice(
        &mut self,
        term: Option<AwaitId>,
        taken: &mut BTreeSet<MetavarId>,
    ) -> Result<bool, Error> {
        // A `use` slot first: a dictionary decides what a method's type reduces to, and so what a literal beside it is read at.
        if self.answer_witnesses(term)? {
            return Ok(true);
        }

        let (choices, links) = self.choices_awaited_by(term);
        if choices.is_empty() {
            return Ok(false);
        }

        // Each check with what its type has come to, and the bare metavariable where that is one.
        let mut waiting = Vec::with_capacity(choices.len());
        for choice in choices {
            let reduced = self.with_retry_frame(&choice.frame, |context| {
                reduce_with(context, &choice.expected)
            })?;
            let bare = match &*reduced {
                Subterm::Metavar(Metavar { id, .. }) if self.metavar_solution(*id).is_none() => {
                    Some(*id)
                }
                _ => None,
            };
            waiting.push((bare, reduced, choice));
        }
        let open = |choice: Choice, taken: &BTreeSet<MetavarId>| {
            waiting.iter().position(|(bare, _, waiting)| {
                waiting.choice == choice && bare.is_some_and(|id| !taken.contains(&id))
            })
        };

        if let Some(at) = open(Choice::Shape, taken) {
            let (bare, _, choice) = &waiting[at];
            let id = bare.expect("an open choice waits on a bare metavariable");
            taken.insert(id);
            self.shape(id, choice)?;
            return Ok(true);
        }

        if let Some(at) = open(Choice::Numeral, taken) {
            let first = waiting[at]
                .0
                .expect("an open choice waits on a bare metavariable");
            taken.insert(first);

            // The metavariables held to the first by the conversions parked between them: numerals wait on one type where it is one metavariable, or two a parked conversion holds together.
            let mut held = BTreeSet::from([first]);
            loop {
                let before = held.len();
                for link in &links {
                    if link.iter().any(|id| held.contains(id)) {
                        held.extend(link.iter().copied());
                    }
                }
                if held.len() == before {
                    break;
                }
            }
            let literals = waiting
                .iter()
                .filter(|(bare, _, waiting)| {
                    waiting.choice == Choice::Numeral && bare.is_some_and(|id| held.contains(&id))
                })
                .filter_map(|(_, _, waiting)| match &*waiting.written {
                    Subterm::Transient(Transient::NumLit(literal)) => Some(literal),
                    _ => None,
                });
            let default = numeral_default(self, literals);
            let choice = &waiting[at].2;
            self.with_retry_frame(&choice.frame, |context| {
                expect(context, &choice.written, &default, &choice.expected)
            })?;
            return Ok(true);
        }

        // A lambda whose expectation has become a function type and that still waits, a domain of it being open: nothing its term reads can give that domain any more, so the body is what grounds it.
        let lambda = waiting.iter().find(|(_, reduced, waiting)| {
            waiting.choice == Choice::Lambda
                && matches!(&**reduced, Subterm::FuncType(_))
                && !taken.contains(&waiting.placeholder)
        });
        if let Some((_, _, choice)) = lambda {
            taken.insert(choice.placeholder);
            if self.take_checking(choice.placeholder).is_some() {
                let checked = self.with_retry_frame(&choice.frame, |context| {
                    check(context, &choice.written, choice.expected.clone())
                })?;
                self.fill_placeholder(choice.placeholder, checked);
            }
            return Ok(true);
        }

        // A lambda on a bare metavariable: nothing its term reads gives it a function type any more, so it is checked at the one its annotations state. One whose every domain is annotated is taken ahead of one with a domain left open, which the first may give.
        let mut bare = Vec::new();
        for (at, (open, _, waiting)) in waiting.iter().enumerate() {
            if let Subterm::Func(func) = &*waiting.written
                && waiting.choice == Choice::Lambda
                && open.is_some()
                && !taken.contains(&waiting.placeholder)
            {
                bare.push((!annotated(self, func), at));
            }
        }
        if let Some(&(_, at)) = bare.iter().min() {
            let choice = &waiting[at].2;
            taken.insert(choice.placeholder);
            self.settle(choice)?;
            return Ok(true);
        }

        Ok(false)
    }

    /// Check the lambda of `choice`, parked on a bare metavariable, at the type its annotations state, and hold that type to the metavariable: a domain left open stands as a metavariable named after its binder, and the body states the codomain ([`elaborate_func_settle`]). Only an expectation can supply a hidden binder, and this is where none is coming: a function type with hidden binders handed to the lambda afterwards is refused, as it is for a lambda bound with no call around it.
    fn settle(&mut self, choice: &ParkedChoice) -> Result<(), Error> {
        let Subterm::Func(func) = &*choice.written else {
            unreachable!("a lambda's choice is a lambda's");
        };
        // Out of the store while it is checked, as a literal is while it is shaped.
        if self.take_checking(choice.placeholder).is_none() {
            return Ok(());
        }
        let (rebuilt, stated) = self.with_retry_frame(&choice.frame, |context| {
            elaborate_func_settle(context, func, &choice.written)
        })?;
        self.fill_placeholder(choice.placeholder, rebuilt);

        self.with_retry_frame(&choice.frame, |context| {
            expect(context, &choice.written, &stated, &choice.expected)
        })
    }

    /// Give the bare metavariable `id` the shape of the tuple literal that waits on it: a non-dependent product of one open type a field, labelled as the literal is.
    ///
    /// The literal is checked at the product first, and each field that says what it is of itself is read in its turn ([`Context::structure`]), and only then is the product held to `id`: a product is a proposition where every field is one, which the fields alone say, so held to `id` unread it would be sized as a type's whatever it came to hold. Every other literal that waits on `id` is checked at the product by the wake, each filling what the other left open.
    ///
    /// A part is a type left to inference, at a sort of its own: a proposition's where `id`'s own sort is one, every field of a record of propositions being one, and its quality open otherwise. Its level is bounded by the field it is given and by the product it stands in, as a field's is where a product is synthesized from its fields.
    fn shape(&mut self, id: MetavarId, choice: &ParkedChoice) -> Result<(), Error> {
        let Subterm::Tuple(tuple) = &*choice.written else {
            unreachable!("a shape is a tuple literal's");
        };
        // Out of the store while it is checked, so a wake its own fields set off cannot run it a second time.
        if self.take_checking(choice.placeholder).is_none() {
            return Ok(());
        }
        let sort = self
            .metavar_entry(id)
            .map(|entry| entry.result.clone())
            .expect("a metavariable waited on has a birth record");

        let (product, parts, checked, declared) =
            self.with_retry_frame(&choice.frame, |context| {
                let declared = match &*reduce_with(context, &sort)? {
                    Subterm::Sort(quality, _) => Some(context.universes().quality(*quality)),
                    _ => None,
                };
                let mut fields = Vec::with_capacity(tuple.fields.len());
                let mut parts = Vec::with_capacity(tuple.fields.len());
                for position in 0..tuple.fields.len() {
                    let part = match declared {
                        Some(Quality::Prop) => context.fresh_prop_sort("literal field"),
                        _ => context.fresh_sort("literal field"),
                    };
                    let (part, hole) = context.fresh_hole(part, choice.written.extent());
                    parts.push(part);
                    // `names` is empty for a positional literal, not a row of `None`s, so the label is read by position.
                    let name = tuple.names.get(position).and_then(|name| name.as_deref());
                    fields.push((context.fresh(name), hole));
                }
                let product = Term::tuple_type(fields);
                let checked = check(context, &choice.written, product.clone())?;

                Ok::<_, Error>((product, parts, checked, declared))
            })?;
        self.fill_placeholder(choice.placeholder, checked);

        for part in parts {
            self.structure(part)?;
        }

        self.with_retry_frame(&choice.frame, |context| {
            expect(context, &choice.written, &product, &choice.expected)
        })?;
        // Where the metavariable was declared a type, the product is read again once the item has settled its fields ([`Context::hold_shapes`]). Where its sort was left to inference there is nothing declared to hold it to.
        if declared == Some(Quality::Type) {
            self.note_shaped(choice.clone());
        }

        Ok(())
    }

    /// Read the form parked on the bare metavariable `id`, a part of a shape, where it says what it is of itself, with nothing beside it to wait for: a literal takes its shape, and a lambda whose every domain is annotated the type its annotations and body state. Any other form is left parked, a lambda with a domain left open among them, which a literal beside it may give.
    fn structure(&mut self, id: MetavarId) -> Result<(), Error> {
        let Some(inner) = self.choice_waiting_on(id) else {
            return Ok(());
        };

        match (&*inner.written, inner.choice) {
            (_, Choice::Shape) => self.shape(id, &inner),
            (Subterm::Func(func), Choice::Lambda) if annotated(self, func) => self.settle(&inner),
            _ => Ok(()),
        }
    }

    /// Hold each product a literal shaped, at a metavariable declared a `Type`, to that sort, now that the item has settled what the product's fields are. A product is sized where it is held to its metavariable, and a field still open there — a lambda, typed after it — is read as a type's whatever it comes to hold, so a record of propositions where a `Type` is declared would be the kernel's to refuse. It is refused here, as it is where every field is read before the product is held.
    fn hold_shapes(&mut self) -> Result<(), Error> {
        for shaped in self.take_shaped() {
            let Subterm::Metavar(metavar) = &*shaped.expected else {
                continue;
            };
            let Some(declared) = self
                .metavar_entry(metavar.id)
                .filter(|entry| entry.solution.is_some())
                .map(|entry| entry.result.clone())
            else {
                continue;
            };
            let refused = self.with_retry_frame(&shaped.frame, |context| {
                let product = zonk_solved_term_metas(context, &shaped.expected);
                if quality_of(context, &product)? != Quality::Prop {
                    return Ok(None);
                }

                Ok::<_, Error>(Some(match &metavar.origin {
                    MetavarOrigin::Implicit(ImplicitOrigin { func, binder }) => {
                        let given = resolved_for_display(context, &product);
                        Error::implicit_at_other_sort(
                            callee(context, func),
                            binder.clone(),
                            given,
                            true,
                        )
                    }
                    _ => {
                        let sort = sort_term(context, &product)?;
                        display_mismatch(context, &shaped.written, &sort, &declared)
                    }
                }))
            })?;
            if let Some(error) = refused {
                return Err(error.at_opt(shaped.written.extent()));
            }
        }

        Ok(())
    }

    /// Retry every currently parked problem once without requiring the store to become empty, and answer each `use` slot no term awaits any more. Declaration finalization uses this to settle work that is already decidable before closing universe metas, while witness goals that genuinely depend on a later declaration remain parked. The slots are answered here and not left to the drain: a witness resolved once its declaration's levels have closed closes the levels it mints by itself, apart from the declaration's own.
    pub(crate) fn sweep_parked(&mut self) -> Result<(), Error> {
        let parked = self.take_parked();
        for goal in parked {
            retry_one(self, goal)?;
        }
        self.retry_parked()?;
        while self.answer_witnesses(None)? {
            self.retry_parked()?;
        }

        Ok(())
    }

    /// Answer each `use` slot among those `term` awaits whose goal resolution can decide ([`answer_witness`]), in one pass and before anything they woke is retried: two slots a parked conversion holds together are each resolution's to name, at an instance of its own, where a retry between them would hand the second the first's answer, and which of the two resolution named would follow the order they were parked in. Every goal is asked again each time, since one whose key was flex may have come to a head with a choice taken since. Answers whether a slot was solved.
    fn answer_witnesses(&mut self, term: Option<AwaitId>) -> Result<bool, Error> {
        let mut answered = false;
        for slot in self.witnesses_awaited_by(term) {
            let Some(ParkedProblem {
                work:
                    ParkedWork::Witness {
                        slot,
                        goal,
                        provenance,
                    },
                origin,
                frame,
                ..
            }) = self.take_witness(slot)
            else {
                continue;
            };
            answer_witness(self, slot, goal, provenance, origin, frame)?;
            answered |= self.metavar_solution(slot).is_some();
        }

        Ok(answered)
    }

    /// Drain the parked store ([`Context::drain_store`]), then hold each product a literal shaped to the sort its metavariable was declared at ([`Context::hold_shapes`]). Run after each top-level item and after the entrypoint body, so an unresolvable constraint is attributed to the definition that produced it and frozen frames do not pile up.
    pub(crate) fn drain_parked(&mut self) -> Result<(), Error> {
        self.drain_store()?;
        self.hold_shapes()
    }

    /// Retry everything parked to a fixpoint, taking what no term awaited as a term's completion takes its own, then report any survivor as a mismatch at its origin.
    fn drain_store(&mut self) -> Result<(), Error> {
        curios_profile::profile!("typing::drain_parked");
        let mut taken = BTreeSet::new();
        loop {
            self.retry_parked()?;

            let remaining = self.take_parked();
            if remaining.is_empty() {
                return Ok(());
            }

            // Final sweep: attempt everything once more — a goal parked after the last solution landed has never been retried.
            let before = remaining.len();
            for parked in remaining {
                retry_one(self, parked)?;
            }

            // No progress in a full sweep: the rest can never resolve. A conversion held up by written goals alone is not an error but the goals' own report — the batch at module end names each `?`, and this obligation goes with it as what the hole must make true; the program never compiles with a goal in it, so the dropped constraint is never unchecked. Anything else reports the first survivor at its origin.
            if self.parked_len() >= before && !self.has_newly_solved() {
                // What no term awaits is the item's to answer, by the ranks a term completes by ([`Context::complete`]), before anything is reported.
                if self.take_choice(None, &mut taken)? {
                    continue;
                }

                for parked in self.take_parked() {
                    let ParkedProblem {
                        work,
                        origin: parked_origin,
                        frame,
                        watching,
                    } = parked;
                    if let ParkedWork::Conversion(goal) = &work
                        && let Some(goals) = blocked_on_written_goals(self, &watching, goal)
                    {
                        let this = resolved_for_display(self, &goal.this);
                        let that = resolved_for_display(self, &goal.that);
                        self.note_goal_obligation(goals, this, that);
                        continue;
                    }
                    return Err(match work {
                        ParkedWork::Conversion(goal) => {
                            // A conversion stuck between two witness holes reads as a bare metavariable mismatch; name the unresolved witness it actually is instead.
                            match self
                                .witness_hole(&goal.this)
                                .or_else(|| self.witness_hole(&goal.that))
                            {
                                Some((origin, witness_goal)) => {
                                    let embedding = diagnose_embedding(
                                        self,
                                        &witness_goal,
                                        parked_origin.extent().as_ref(),
                                    );
                                    let shape = diagnose_shape(self, &witness_goal);
                                    Error::no_witness(
                                        resolved_for_display(self, &witness_goal),
                                        callee(self, &origin.func),
                                        origin.binder,
                                        embedding,
                                        shape,
                                    )
                                    .at_opt(parked_origin.extent())
                                }
                                // A watched blocker that is itself blocked names a goal that merely waited. Follow the chain to what nothing was ever going to solve and report *that* instead.
                                None if let Some(error) =
                                    root_blocker_error(self, &watching, parked_origin.extent()) =>
                                {
                                    error
                                }
                                // A survivor of the drain is postponed, not rigidly mismatched — the retry's own Mismatch arm reports rigid disagreements. Say so, naming the blockers it watched: a rigid mismatch means the program is wrong, a postponement means inference never gained the structure to decide.
                                None => {
                                    let blockers =
                                        watched_blockers(self, &watching, &goal.this, &goal.that);
                                    Error::postponed_conversion(
                                        resolved_for_display(self, &goal.this),
                                        resolved_for_display(self, &goal.that),
                                        blockers,
                                        frame.carries_refinements(),
                                    )
                                    .at_opt(parked_origin.extent())
                                }
                            }
                        }
                        ParkedWork::Checking { term, expected, .. } => match &*term {
                            // A parked bang is a region that never named its monad; report it as that, not as a generic stalled check.
                            Subterm::Transient(Transient::Bang(_)) => {
                                Error::bang_region_undetermined().at_opt(parked_origin.extent())
                            }
                            _ => Error::postponed_check(resolved_for_display(self, &expected))
                                .at_opt(parked_origin.extent()),
                        },
                        ParkedWork::Witness {
                            goal, provenance, ..
                        } => {
                            let embedding =
                                diagnose_embedding(self, &goal, parked_origin.extent().as_ref());
                            let shape = diagnose_shape(self, &goal);
                            Error::no_witness(
                                resolved_for_display(self, &goal),
                                callee(self, &provenance.func),
                                provenance.binder,
                                embedding,
                                shape,
                            )
                            .at_opt(parked_origin.extent())
                        }
                        // A head whose type never reached a tuple or a struct is what the projection refused before it could wait.
                        ParkedWork::Projection(projection) => {
                            Error::not_a_tuple(resolved_for_display(self, &projection.head_type))
                                .at_opt(parked_origin.extent())
                        }
                        // A scrutinee whose type never reached a carrier is what the match refused before it could wait.
                        ParkedWork::Match(parked) => refused_scrutinee(
                            &parked.term,
                            resolved_for_display(self, &parked.scrutinee_type),
                        )
                        .at_opt(parked_origin.extent()),
                        // A bound that never came to truth is its hole's to report: zonk names the binder and the bound, which is the report such a hole gets whether or not it was ever parked.
                        ParkedWork::Discharge { .. } => continue,
                    });
                }
                return Ok(());
            }
        }
    }
}

/// The written goals a surviving conversion is held up by, when they are all that holds it up: every still-unsolved watched metavariable occurs in the goal with `Goal` provenance. `None` when any blocker is anything else — an uninferred implicit, a witness, a hole no occurrence names — or when nothing is watched at all, since a conversion nothing could wake is not the goals' to carry.
fn blocked_on_written_goals(
    context: &Context,
    watching: &BTreeSet<MetavarId>,
    goal: &Problem,
) -> Option<BTreeSet<MetavarId>> {
    let origins = metavar_origins(&[&goal.this, &goal.that]);
    let unsolved: BTreeSet<MetavarId> = watching
        .iter()
        .copied()
        .filter(|id| context.metavar_solution(*id).is_none())
        .collect();
    if unsolved.is_empty() {
        return None;
    }
    unsolved
        .iter()
        .all(|id| matches!(origins.get(id), Some((MetavarOrigin::Goal, _))))
        .then_some(unsolved)
}

/// Render a surviving conversion goal's still-unsolved watched metavariables, each by what it *is* — the implicit or witness argument it fills, the written goal it stands for — never by its id: an id is elaboration state the reader cannot decode, and two blockers rendering alike decides nothing, for the reason the printer's anonymous-metavariable axis gives. Insertion provenance rides the *occurrence*, not the birth entry, so both sides are scanned for the watched ids; a watched metavariable no occurrence names (solved away from the spelling, or watched through a spine) is an inferred hole.
fn watched_blockers(
    context: &Context,
    watching: &BTreeSet<MetavarId>,
    this: &Term,
    that: &Term,
) -> Vec<Watched> {
    let origins = metavar_origins(&[this, that]);
    watching
        .iter()
        .filter(|id| context.metavar_solution(**id).is_none())
        .map(|id| match origins.get(id) {
            Some((MetavarOrigin::Implicit(origin), _)) => Watched::Slot {
                callee: callee(context, &origin.func),
                noun: "implicit argument",
                binder: origin.binder.clone(),
            },
            Some((MetavarOrigin::Witness(origin), _)) => Watched::Slot {
                callee: callee(context, &origin.func),
                noun: "witness argument",
                binder: origin.binder.clone(),
            },
            // Named by the extent the goal was born with, never by an occurrence's: substituted into a declaration's type, the goal rides the extent of the binder it replaced, which would name the declaration rather than the `?`. Where that is in a file is the report's to say.
            Some((MetavarOrigin::Goal, _)) => Watched::Goal(context.goal_extent(*id).copied()),
            Some((MetavarOrigin::Domain(binder), _)) => {
                Watched::Described(format!("the type of parameter '{binder}'"))
            }
            Some((MetavarOrigin::Hole, _)) | None => {
                Watched::Described("an inferred hole".to_string())
            }
        })
        .collect()
}

/// A metavariable's insertion provenance and the extent of the occurrence it was read off — what a report names a blocker by.
pub(crate) type Provenance = (MetavarOrigin, Option<Extent>);

/// Every metavariable occurrence across `terms` that carries an origin, by id, with the extent of its first occurrence.
///
/// Memoized on node identity: the hook only records an id's first occurrence, which a memoized walk still reaches first, and `convert`'s solver asks this of a candidate — a reduct, whose tree can be exponential in its depth — whenever it postpones one.
pub(crate) fn metavar_origins(terms: &[&Term]) -> BTreeMap<MetavarId, Provenance> {
    let origins: Rc<RefCell<BTreeMap<MetavarId, Provenance>>> =
        Rc::new(RefCell::new(BTreeMap::new()));
    for term in terms {
        let sink = Rc::clone(&origins);
        let mut visit = Visit::rewriting_shared(
            |_, _| None,
            Box::new(move |_, term: &Term| {
                if let Subterm::Metavar(Metavar { id, origin, .. }) = &**term {
                    sink.borrow_mut()
                        .entry(*id)
                        .or_insert_with(|| (origin.clone(), term.extent()));
                }
                None
            }),
        );
        let _: Term = term.traverse(&mut visit);
    }
    Rc::try_unwrap(origins)
        .map(RefCell::into_inner)
        .unwrap_or_else(|shared| shared.borrow().clone())
}

/// The spines `id` occurs at in `term`, one per occurrence: what a restriction spells the embedding metavariable's entries through ([`Context::metavar_restriction`]).
pub(crate) fn metavar_spines(term: &Term, id: MetavarId) -> Vec<Rc<Vec<Term>>> {
    let spines: Rc<RefCell<Vec<Rc<Vec<Term>>>>> = Rc::new(RefCell::new(Vec::new()));
    let sink = Rc::clone(&spines);
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term: &Term| {
            if let Subterm::Metavar(Metavar {
                id: occurring,
                spine,
                ..
            }) = &**term
                && *occurring == id
            {
                sink.borrow_mut().push(Rc::clone(spine));
            }
            None
        }),
    );
    let _: Term = term.traverse(&mut visit);
    Rc::try_unwrap(spines)
        .map(RefCell::into_inner)
        .unwrap_or_else(|shared| shared.borrow().clone())
}

fn retry_one(context: &mut Context, parked: ParkedProblem) -> Result<(), Error> {
    let ParkedProblem {
        work,
        origin,
        frame,
        watching,
    } = parked;

    context.retried_under(&watching, |context| {
        retry_work(context, work, origin, frame)
    })
}

/// Retry one problem's work under its frozen frame ([`retry_one`]).
fn retry_work(
    context: &mut Context,
    work: ParkedWork,
    origin: Term,
    frame: FrozenFrame,
) -> Result<(), Error> {
    let goal = match work {
        ParkedWork::Conversion(goal) => goal,
        ParkedWork::Checking {
            term,
            expected,
            placeholder,
        } => return retry_checking(context, term, expected, placeholder, origin, frame),
        ParkedWork::Witness {
            slot,
            goal,
            provenance,
        } => {
            retry_witness(context, slot, goal, provenance, origin, frame);
            return Ok(());
        }
        ParkedWork::Discharge {
            slot,
            bound,
            provenance,
        } => return retry_discharge(context, slot, bound, provenance, origin, frame),
        ParkedWork::Projection(projection) => {
            return retry_projection(context, projection, origin, frame);
        }
        ParkedWork::Match(parked) => return retry_match(context, parked, origin, frame),
    };

    enum Retry {
        Converts,
        Mismatch(Error),
        Blocked(Vec<Problem>),
    }

    let outcome = context.with_retry_frame(&frame, |context| {
        Ok(
            match problem_outcome(context, goal.clone(), origin.extent())? {
                Outcome::Converts => Retry::Converts,
                // Built here, inside the restored frame, rather than at the report below: `display_mismatch` reads the sides through whatever solutions have landed, so they name the actual disagreement rather than the metavariables it arrived wrapped in (see `resolved_for_display`). A stranded `!` reaches its report through this arm — the region's own type only settles after the sequencing has parked — so the origin is what decides which message it gets.
                // What was compared is read off the type the problem is posed at: at a sort the two are types, one inferred and one expected, and anywhere else they are terms a type states at one position. A residual of the carriers' algebra is posed at `Type` for want of the type it stands at, and is told from two types by what it is: a value an intrinsic builds is no type.
                Outcome::Mismatch(declined) => Retry::Mismatch(
                    match &*goal.type_ {
                        Subterm::Sort(..)
                            if !(intrinsic_value(context, &goal.this)
                                || intrinsic_value(context, &goal.that)) =>
                        {
                            display_mismatch(context, &origin, &goal.this, &goal.that)
                        }
                        _ => Error::terms_differ(
                            resolved_for_display(context, &goal.this),
                            resolved_for_display(context, &goal.that),
                        ),
                    }
                    .declined(declined),
                ),
                Outcome::Blocked(goals) => Retry::Blocked(goals),
            },
        )
    });

    let outcome = outcome.map_err(|error: ReduceError| {
        Error::from_reduce(error, |refusal| {
            Error::convert_exhausted(goal.this.clone(), goal.that.clone(), refusal)
        })
    })?;

    match outcome {
        Retry::Converts => Ok(()),
        // Locate the mismatch at the construct that parked it, as every sibling arm of the drain does. A parked goal outlives the frame it came from, so without this the only surviving clue is the module.
        Retry::Mismatch(error) => Err(error.at_opt(origin.extent())),
        Retry::Blocked(goals) => {
            for goal in goals {
                context.repark(ParkedWork::Conversion(goal), origin.clone(), frame.clone());
            }
            Ok(())
        }
    }
}

/// Whether `term` is a value an intrinsic builds, a literal or an operation's result, and so no type.
fn intrinsic_value(context: &Context, term: &Term) -> bool {
    matches!(
        &**term,
        Subterm::Intrinsic(intrinsic)
            if !matches!(intrinsic.signature(&context.syntax()).produced, Produced::Sort)
    )
}

/// Retry a parked *checking problem*: under the frozen frame, the expected type either still reduces to a bare metavariable (re-park) or has gained structure — run the check for real and fill the placeholder with the rebuilt term. Errors propagate carrying the term's own extents.
///
/// The term is elaborated here for the first time, and no second: a term's lowering-minted holes are birthed where it is first elaborated, and an occurrence rebuilt by another run would drop their spines. So whatever runs a parked check holds it out of the store, and nothing but its check fills its placeholder ([`Context::fill_placeholder`]).
fn retry_checking(
    context: &mut Context,
    term: Term,
    expected: Term,
    placeholder: MetavarId,
    origin: Term,
    frame: FrozenFrame,
) -> Result<(), Error> {
    let rebuilt = context.with_retry_frame(&frame, |context| {
        // Re-park while the term is still blocked on its expected type's structure — the same predicate that parked it, with the result-directed refinements off: at retry there is no output turnaround left to wait for, so a codomain-only block checks eagerly.
        if blocked_on_metavar(context, &term, &expected, &BTreeSet::new(), false)? {
            return Ok(None);
        }

        elaborate(context, &term, Mode::Check(expected.clone())).map(|(rebuilt, _)| Some(rebuilt))
    })?;

    match rebuilt {
        Some(rebuilt) => context.fill_placeholder(placeholder, rebuilt),
        None => context.repark(
            ParkedWork::Checking {
                term,
                expected,
                placeholder,
            },
            origin,
            frame,
        ),
    }

    Ok(())
}

/// Register a counterfactual match-arm refinement of a scrutinee, so a context hypothesis whose type mentions it reduces at the arm's value. The frame holding the refinement is scoped to the arm, so the (counterfactual) assumption does not leak. Three keyings, by head shape:
///
/// - a `Var` reduces to the value (`refine`), and a local `let`'s definition is refined to the same value in turn: the kernel reads a scrutinee by value, so the one it matches on is the definition, and a name refined alone disagrees with the definition it unfolds to — the solver reifies a local definition into the solution it stores, and that spelling saw nothing;
/// - a projection — a `Bool`/`Nat` match on a tuple field — refines that projection (`refine_projection`);
/// - any other head — a stuck application like `classify(c)` / `Nat/in_range(...)` — is recorded as written in the term-keyed scrutinee store (`refine_scrutinee`), where an occurrence spelled otherwise is met at the entry's reduced spelling.
///
/// A scrutinee's *indices* are not refined here: an arm solves their equations with `curios_analysis::solve_indices`, the kernel's own function, and records the variables it solves. Keying a non-variable index — a constructor, a stuck application — as an equation on the index itself would record the reverse of what the inverter pins, or a fact the kernel does not have.
///
/// Nothing is recorded under a spelling [`records_case_equation`] refuses — one that mentions no local, which is where the kernel records none either — judged on the scrutinee as the kernel spells it, its local definitions substituted and the variables the arms around it refined spelled as their values ([`Context::kernel_spelling`]), and again on each spelling the term-keyed store would hold: an equation one checker records and the other does not reads a dead arm two ways.
///
/// All three rest on one premise — the arm is reached only when the scrutinee *equals* the case's value — and every scrutinee has it, because a term of non-`Io` type denotes one value.
///
/// That is a typing fact, not an analysis, because every host operation returns `Io`. `Cell/get(c)` denotes differently before and after a `Cell/set`, and an equation for it would read one term as `true` in one arm and `false` in a nested one — an equation between two `Bool` literals, and from there a closed inhabitant of `False` — but it has type `Io(T)`, which is opaque and has no cases, so it cannot be a scrutinee at all. And no inhabitant of `(Bool) -> Bool` performs an effect, so `f(true)` fixes a value for every possible caller binding, a binder `f` included.
pub(crate) fn refine_head(context: &mut Context, head: &Term, value: &Term) -> Result<(), Error> {
    if !records_case_equation(&context.kernel_spelling(head)) {
        return Ok(());
    }

    match &**head {
        Subterm::Var(var) => {
            let name = var.unwrap();
            context.refine(name, value);
            if !context.is_top_level(name)
                && let Some(definition) = context.definition_body(name).cloned()
            {
                refine_head(context, &definition, value)?;
            }
        }
        Subterm::Proj(Proj {
            head,
            field: Field::Tuple(index) | Field::Struct(_, index),
        }) => {
            context.refine_projection(head.clone(), *index, value.clone());
        }
        _ => {
            let mut spellings = scrutinee_spellings(context, head)?
                .into_iter()
                .map(|(key, original)| (key, original, false))
                .collect::<Vec<_>>();

            // The kernel reads a term by value where reduction is entered, so an occurrence reached by unfolding a definition spells the values the guard's local definitions stand for, where one written in the guard's own scope spells their names. The kernel's spelling is recorded beside the written one, as an alias: the exact lookup meets it, and settlement never reduces it.
            let unfolded = Unfolding::everything(&*context).term(head);
            if unfolded != *head {
                for (key, original) in scrutinee_spellings(context, &unfolded)? {
                    if !spellings.iter().any(|(written, _, _)| *written == key) {
                        spellings.push((key, original, true));
                    }
                }
            }

            spellings.retain(|(_, original, _)| {
                records_case_equation(&context.kernel_spelling(original))
            });
            context.refine_scrutinee_spellings(spellings, value);
        }
    }

    Ok(())
}

/// The case a guard always is, where that is a case other than `value`, the arm's: the arm is then never taken, and an error its body raises is reported as one in a dead arm — which is where a proof resting on the guard fails, since no equation is recorded under a spelling that mentions no local ([`records_case_equation`]). `None` where the guard is the arm's own case, or no literal or constructor at all — a guard its locals decide stays stuck.
pub(crate) fn unreachable_arm(context: &mut Context, head: &Term, value: &Term) -> Option<Term> {
    let spelled = Unfolding::everything(&*context).term(head);
    let case = reduce_with(context, &spelled).ok()?;
    another_case(context, &case, value, &[])
        .ok()?
        .then_some(case)
}

/// The guard of an arm around this one that the arm at `value` contradicts, with the case that guard always is there: where `head` is a variable, an equation in force — or one the arm already withholds — whose scrutinee names no local once the variable is spelled as `value`, and which computes to another case than the one its arm assumed. The arm is then never taken, and a proof resting on that guard fails in it, since the equation answers nothing there ([`Context::kernel_spelling`], and the withholding `Context::refine` does).
pub(crate) fn contradicted_guard(
    context: &mut Context,
    head: &Term,
    value: &Term,
) -> Option<(Term, Term)> {
    let solution = scrutinee_solution(&*context, head, value)?;
    let equations = context
        .scrutinee_equations()
        .into_iter()
        .map(|(_, entry)| entry)
        .chain(context.withheld_equations())
        .filter(|entry| !entry.alias)
        .collect::<Vec<_>>();

    for entry in equations {
        let spelled = context
            .kernel_spelling(&entry.original)
            .substitute(slice::from_ref(&solution));
        if records_case_equation(&spelled) {
            continue;
        }
        let Ok(case) = reduce_with(context, &spelled) else {
            continue;
        };
        if another_case(context, &case, &entry.value, &[]).is_ok_and(|another| another) {
            return Some((entry.original, case));
        }
    }

    None
}

/// The spellings a scrutinee that is neither a variable nor a projection is met by, each with the term it was registered from: as written, and resolved through a concept dispatch where it has one.
fn scrutinee_spellings(context: &mut Context, head: &Term) -> Result<Vec<(Term, Term)>, Error> {
    // Registered on the *cheap* key: the scrutinee as written, with metas and universes normalized. Reducing it here would make a guard cost its operand's evaluation before any use of the fact — see `shallow_scrutinee`. An occurrence this spelling does not meet is met at the stuck form reduction leaves it as, against the entry's reduced spelling (`reduce`'s `refined_reduct`), which is settled when a probe first needs it rather than at every site that records one.
    let canonical = shallow_scrutinee(context, head);

    // A concept-dispatched scrutinee (`a <= hi`) elaborates to the method projected out of the witness — `(?w).1(a, hi)` — which is not the shape the reducer probes: by then it has become the intrinsic normal form `NatLe(a, hi)`. Registering only the verbatim key leaves the arm unrefined, silently, while the equivalent `Nat/le(a, hi)` spelling refines. Register the probed form alongside it so both spellings agree.
    let resolved = match canonical.head_key().is_none() {
        true => spine_whnf(context, head)?
            .map(|spined| {
                let key = shallow_scrutinee(context, &spined);
                (key, spined)
            })
            .filter(|(resolved, _)| resolved.head_key().is_some() && *resolved != canonical),
        false => None,
    };

    // The operands disagree the same way and are deliberately *not* re-registered here. Their spellings differ by a fold and by an unfolded local as well as by a wrapper, so bringing them together is reduction — and reduction at registration is what would make a guard cost its subject's evaluation before any use of the fact, which is the whole reason `shallow_scrutinee` records the written form. `reduce`'s `settle` does it at the first probe that needs it instead, under a ceiling and once per entry — from the *unerased* spelling stored beside the key, since the erased one cannot unfold the polymorphic heads reduction must see through.
    let mut spellings = vec![(canonical, head.clone())];
    spellings.extend(resolved);
    Ok(spellings)
}

/// A scrutinee's applied spine taken to weak-head normal form one application layer at a time, or `None` when it is not an application spine or nothing moved.
///
/// Bounded rather than reduced: it opens layers and stops, so it never forces an argument. That is what makes it usable at *registration*, where reducing would cost the guard its subject's evaluation.
fn spine_whnf(context: &mut Context, term: &Term) -> Result<Option<Term>, Error> {
    let mut current = term.clone();

    // Bounded: each step consumes one application layer of an elaborated dispatch, and a runaway is a bug rather than something to spin on.
    for step in 0..RESOLVED_SPELLING_LAYERS {
        let Some(opened) = open_layer(context, &current)? else {
            return Ok((step > 0).then_some(current));
        };
        current = opened;
    }

    Ok(None)
}

/// One application layer of `term` opened: its head reduced to a function and the body instantiated at the arguments as written, or `None` where `term` is no application or its head reduces to no function of its arity. No argument is reduced.
///
/// A layer opens only at the arity it saturates, as the kernel's `resolved_spelling` does: a layer whose arguments do not match the lambda its head reduces to is left where it stands. An elaborated application is saturated against its head's type, so no program is known to reach the check; without it, a term built wrong would reach `Telescope::open`'s assertion as a panic.
pub(crate) fn open_layer(context: &mut Context, term: &Term) -> Result<Option<Term>, Error> {
    let Subterm::Apply(apply) = &**term else {
        return Ok(None);
    };
    let Subterm::Func(Func { telescope, .. }) = &*reduce_with(context, &apply.head)? else {
        return Ok(None);
    };
    let args = apply.params().collect::<Vec<_>>();
    if telescope.len() != args.len() {
        return Ok(None);
    }
    Ok(Some(telescope.open(&args)))
}

/// What an eliminator's motive must abstract: the scrutinee's indices, then the scrutinee itself. Parameters are never abstracted — they are uniform across constructors and fixed by the scrutinee's type.
pub(crate) enum MotiveShape<'a> {
    /// An intrinsic carrier (`Bool`, `Nat`, `List`, `Bin`): no indices, so the motive binds only the scrutinee, at the carrier's own type.
    Intrinsic(&'a Term),
    /// A nominal inductive: `indices` is the declaration's index telescope already instantiated at the scrutinee's actual parameters (`InductDecl::indices_at`), and the scrutinee binder takes `I(p̄, ī)` at those index binders.
    Induct {
        name: &'a Global,
        universes: &'a Universes,
        params: &'a [Term],
        indices: Telescope<()>,
    },
}

impl MotiveShape<'_> {
    /// The motive's binder count: one per index, then the scrutinee.
    pub(crate) fn arity(&self) -> usize {
        match self {
            MotiveShape::Intrinsic(_) => 1,
            MotiveShape::Induct { indices, .. } => indices.len() + 1,
        }
    }

    /// The eliminated family's name, for diagnostics.
    fn name(&self) -> Option<&Global> {
        match self {
            MotiveShape::Intrinsic(_) => None,
            MotiveShape::Induct { name, .. } => Some(name),
        }
    }

    /// Assume this shape's binders in `context` under `labels`, each index type opened with the preceding index binders and the scrutinee typed at those binders. Returns the scrutinee's assumed type for the caller's use.
    fn assume(&self, context: &mut Context, binders: &[Free]) -> Term {
        let (scrutinee_binder, index_binders) = binders
            .split_last()
            .expect("a motive binds at least the scrutinee");

        let scrutinee_type = match self {
            MotiveShape::Intrinsic(head_type) => (*head_type).clone(),
            MotiveShape::Induct {
                name,
                universes,
                params,
                indices,
            } => {
                let mut cursor = indices.cursor();
                for binder in index_binders {
                    let (_, ty) = cursor
                        .entry()
                        .expect("index label count equals the index telescope's");
                    context.assume(binder, &ty);
                    cursor.advance(Term::free_var(binder));
                }
                Term::induct_type_at(
                    *(*name),
                    (*universes).clone(),
                    params.to_vec(),
                    cursor.into_args(),
                )
            }
        };

        context.assume(scrutinee_binder, &scrutinee_type);
        scrutinee_type
    }

    /// This shape's motive type, `(ī' : Ī(p̄)) -> I(p̄, ī') -> Type`, which a *written* motive is checked against. The codomain is a sort left to inference ([`Context::fresh_sort`]), its quality still to be solved: a motive into `Prop` and one into `Type` each decide it at the checking boundary (`expect`).
    fn motive_type(&self, context: &mut Context) -> Term {
        let mut binders = Vec::with_capacity(self.arity());

        let scrutinee_type = match self {
            MotiveShape::Intrinsic(head_type) => (*head_type).clone(),
            MotiveShape::Induct {
                name,
                universes,
                params,
                indices,
            } => {
                let mut cursor = indices.cursor();
                while let Some((_, ty)) = cursor.entry() {
                    binders.push((cursor.advance_fresh(|hint| context.fresh(hint)), ty));
                }
                let index_vars = cursor.into_args();
                Term::induct_type_at(*(*name), (*universes).clone(), params.to_vec(), index_vars)
            }
        };

        binders.push((context.fresh(None), scrutinee_type));
        let sort = context.fresh_sort("motive result");
        Term::func_type(binders, sort)
    }
}

/// Check that `motive` is a well-formed type family for an eliminator of the given [`MotiveShape`], returning it closed at that shape's arity.
///
/// Two inputs reach here. A motive already closed at the eliminator's arity — synthesized, or a rebuilt match coming back through re-elaboration — is opened under its assumed binders and its body checked against `Type`, then re-closed so the motive carries its solved form. A *written* motive arrives from `into_core` un-scoped, in an arity-0 scope (`Term::match_motive_written`): it is an ordinary term, checked against the shape's motive type and then decomposed into the same canonical scope.
///
/// The elided hole — the bare metavariable `into_core` mints for an absent motive — is neither: it is wrapped at the eliminator's arity and left for synthesis or for `solve` to fill in.
pub(crate) fn check_motive(
    context: &mut Context,
    shape: &MotiveShape<'_>,
    motive: &Scope<Many>,
) -> Result<Scope<Many>, Error> {
    let arity = shape.arity();

    match motive.arity() {
        0 => match &**motive.body() {
            Subterm::Metavar(_) => {
                let elided = Scope::constant(Many(arity), motive.body().clone());
                check_closed_motive(context, shape, &elided)
            }
            _ => check_written_motive(context, shape, motive.body()),
        },
        written if written == arity => check_closed_motive(context, shape, motive),
        written => Err(Error::motive_binder_count(
            shape.name().copied(),
            arity,
            written,
        )),
    }
}

/// The already-closed path: assume the shape's binders, check the opened body against `Type`, and re-close.
fn check_closed_motive(
    context: &mut Context,
    shape: &MotiveShape<'_>,
    motive: &Scope<Many>,
) -> Result<Scope<Many>, Error> {
    let hints = motive
        .hint_iter()
        .map(|label| label.map(str::to_string))
        .collect::<Vec<_>>();
    let labels = hints
        .iter()
        .map(|hint| context.fresh(hint.as_deref()))
        .collect::<Vec<_>>();

    context.with_frame(|context| {
        shape.assume(context, &labels);

        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = vars.iter().collect::<Vec<_>>();
        let sort = context.fresh_sort("eliminator motive result");
        let body = check(context, &motive.open(&refs), sort)?;

        let binders = labels.iter().collect::<Vec<_>>();
        Ok(Scope::close(Many(labels.len()), &binders, body))
    })
}

/// The written path: an ordinary term checked against the shape's motive type, then decomposed into the canonical scope.
///
/// A written motive is nearly always a lambda, and its binder count is checked up front so a miscount reports as itself rather than as a confusing domain mismatch. Decomposition beta-opens that lambda at the scope's own binders, leaving no redex behind. Anything else — a motive naming a top-level family — is eta-expanded instead.
fn check_written_motive(
    context: &mut Context,
    shape: &MotiveShape<'_>,
    written: &Term,
) -> Result<Scope<Many>, Error> {
    let arity = shape.arity();

    if let Subterm::Func(Func { telescope, .. }) = &**written
        && telescope.len() != arity
    {
        return Err(Error::motive_binder_count(
            shape.name().copied(),
            arity,
            telescope.len(),
        ));
    }

    let motive_type = shape.motive_type(context);
    let checked = check(context, written, motive_type)?;

    // The written binder names become the scope's labels, so the motive prints back the way it was spelled.
    let hints = match &*checked {
        Subterm::Func(Func { telescope, .. }) if telescope.len() == arity => telescope
            .labels()
            .into_iter()
            .map(|label| (!label.is_empty()).then(|| label.to_string()))
            .collect::<Vec<_>>(),
        _ => vec![None; arity],
    };
    let labels = hints
        .iter()
        .map(|hint| context.fresh(hint.as_deref()))
        .collect::<Vec<_>>();

    context.with_frame(|context| {
        shape.assume(context, &labels);

        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = vars.iter().collect::<Vec<_>>();
        let body = match &*checked {
            Subterm::Func(Func { telescope, .. }) if telescope.len() == arity => {
                telescope.open(&refs)
            }
            _ => Term::apply(checked.clone(), vars.clone()),
        };

        let binders = labels.iter().collect::<Vec<_>>();
        Ok(Scope::close(Many(arity), &binders, body))
    })
}

/// The error for the *root* of a blocking chain, when a surviving conversion goal waited on a metavariable that was itself blocked.
///
/// `Convert::solve`'s embedded-metavariable guard postpones a candidate carrying an unsolved metavariable of a wider context, and records the edge (`Context::note_solve_blockers`). Walking those edges reaches the metavariable nothing was ever going to solve — characteristically an undischarged decided bound riding inside a helper whose body the candidate unfolded — and reporting it is the difference between naming the proposition the author must discharge, at the call that demands it, and naming an implicit they never wrote, at a line that is not where the fault is.
///
/// Only a *terminal* implicit argument is reported: one whose own blockers have all been solved, so it is the end of the chain rather than another waiter, and one whose provenance yields the same message a lone undischarged bound already gets. Everything else falls through to the postponement report, which is the honest answer when no better one exists.
fn root_blocker_error(
    context: &mut Context,
    watching: &BTreeSet<MetavarId>,
    fallback: Option<Extent>,
) -> Option<Error> {
    let mut seen: BTreeSet<MetavarId> = BTreeSet::new();
    let mut frontier: Vec<MetavarId> = watching.iter().copied().collect();
    let mut root: Option<(MetavarId, ImplicitOrigin, Option<Extent>)> = None;

    while let Some(id) = frontier.pop() {
        if !seen.insert(id) {
            continue;
        }

        for (blocker, origin, extent) in context.solve_blockers(id) {
            if context.metavar_solution(*blocker).is_some() {
                continue;
            }

            let terminal = context
                .solve_blockers(*blocker)
                .iter()
                .all(|(next, _, _)| context.metavar_solution(*next).is_some());

            if terminal && let MetavarOrigin::Implicit(implicit) = origin {
                root = Some((*blocker, implicit.clone(), *extent));
            }

            frontier.push(*blocker);
        }
    }

    let (id, origin, extent) = root?;
    let (bound, proposition, reduct, refusal) = context.metavar_entry(id).map(|entry| {
        (
            entry.result.clone(),
            entry.proposition,
            entry.reduct.clone(),
            entry.refusal.as_deref().cloned(),
        )
    })?;

    // Only a *proposition* is a bound. An implicit whose type is a sort is a type argument — `@A: Type` — and there is nothing it could have discharged, so "nothing discharged Type" would name an obligation the author never had while burying the postponement that actually explains the goal. Report the root only where the root reads as one.
    if !proposition {
        return None;
    }

    Some(
        Error::uninferred_implicit(
            callee(context, &origin.func),
            origin.binder,
            resolved_for_display(context, &bound),
            true,
            reduct,
            refusal,
        )
        // The blocker's own occurrence rides inside a candidate the reducer built, which need not have kept an extent; the waiting goal's origin is then the nearest honest place to point.
        .at_opt(extent.or(fallback)),
    )
}
