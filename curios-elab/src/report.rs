//! A report is built where it is reported: an error holds what reading the context gives of the terms it shows, and what reducing them gives — their normal forms, the specialized report the error stands for — is built where the refusal stands.
//!
//! **What a raise holds is what reading the context gives.** Substituting the solutions that have landed, giving a waiting numeral its default, settling qualities and giving recursive names back each read the context, and a raise does them ([`pending`]), with the frames it was raised on held (`Context::hold_frames`). Normalizing a side and asking which specialized report a mismatch stands for reduce, which spends a declaration's budget, mints its identities and warms its caches: they are done where a report is made, standing on the frames the raise held ([`Error::shown`]). An error raised under a bracket that rolls back, and thrown away there, costs its declaration nothing past reading.
//!
//! **The two sides of the line are two types.** A display is built in a [`Reporting`], which only this module makes, at the two places a report is made: where a refusal stands, before what its attempt wrote is undone ([`Error::shown`]), and where the drain keeps a conversion for the report a written goal gets ([`obligation_shown`]). A raise holds a `Context` and no `Reporting`, so it holds what it shows and builds nothing.
//!
//! Why, and what the peers do, is the crate README's decision, *A refusal's report is built where the refusal stands*.

#[cfg(test)]
mod tests;

use {
    super::{
        Beside, Context, Error, HeldFrames, Rejected, callee, decided_sort_of, embeds, is_monad,
        monad_shape, normalize, numeral_default, quality_of, reduce_forced, refold_recs,
        settle_qualities, stuck_on_metavar, watched, zonk_solved_term_metas,
    },
    curios_core::{
        Apply, Bound, Free, ImplicitOrigin, Intrinsic, Metavar, MetavarOrigin, Quality, Subterm,
        Term, Visit,
    },
    std::{
        collections::BTreeMap,
        fmt,
        ops::{Deref, DerefMut},
    },
};

/// The context a report is built in: the elaboration's own, entered only where a report is made, so a display spends, mints and caches where a refusal stands.
pub(crate) struct Reporting<'r, 's>(&'r mut Context<'s>);

impl<'s> Deref for Reporting<'_, 's> {
    type Target = Context<'s>;

    fn deref(&self) -> &Context<'s> {
        self.0
    }
}

impl<'s> DerefMut for Reporting<'_, 's> {
    fn deref_mut(&mut self) -> &mut Context<'s> {
        self.0
    }
}

/// How a pending report is built, out of what its raise held.
type Build = Box<dyn for<'r, 's> FnOnce(&mut Reporting<'r, 's>) -> Error>;

/// A report its raise did not build: what it shows, held as reading the context gave it, the frames it was raised on, and how it is built, standing on those frames, where it is reported (`Error::shown`).
pub struct Pending {
    frames: HeldFrames,
    build: Build,
}

impl fmt::Debug for Pending {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Pending").finish_non_exhaustive()
    }
}

/// A report showing `terms`, each held as reading the context gives it ([`held`]) and handed to `build` where the report is made, standing on the frames the context stands on now.
pub(crate) fn pending<const N: usize>(
    context: &mut Context,
    terms: [&Term; N],
    build: impl for<'r, 's> FnOnce(&mut Reporting<'r, 's>, [Term; N]) -> Error + 'static,
) -> Error {
    let terms = terms.map(|term| held(context, term));
    let frames = context.hold_frames();

    Error::Pending(Box::new(Pending {
        frames,
        build: Box::new(move |reporting| build(reporting, terms)),
    }))
}

/// A mismatch between `this` and `that`, the sides of a conversion about `term`, with what the conversion said beside it: the report [`display_mismatch`] chooses, built where it is reported.
pub(crate) fn mismatch(
    context: &mut Context,
    term: &Term,
    this: &Term,
    that: &Term,
    beside: Option<Beside>,
) -> Error {
    let term = term.clone();
    // A written `Sort` is told from the sort it was solved to only before qualities settle, so the sides are held unsettled as well.
    let sides = [
        zonk_solved_term_metas(context, this),
        zonk_solved_term_metas(context, that),
    ];
    let beside = held_beside(context, beside);

    pending(context, [this, that], move |reporting, [this, that]| {
        let report = display_mismatch(reporting, &term, &sides, &this, &that);
        said_beside(reporting, report, beside)
    })
}

/// Two terms a type states at one position, which differ, with what the conversion that compared them said beside it ([`Error::terms_differ`]).
pub(crate) fn terms_differ(
    context: &mut Context,
    written: &Term,
    expected: &Term,
    beside: Option<Beside>,
) -> Error {
    let beside = held_beside(context, beside);

    pending(
        context,
        [written, expected],
        move |reporting, [written, expected]| {
            let report = Error::terms_differ(
                resolved_for_display(reporting, &written),
                resolved_for_display(reporting, &expected),
            );
            said_beside(reporting, report, beside)
        },
    )
}

/// `beside` with a rejected candidate held as reading the context gives it, so its report shows the solutions that had landed where the mismatch was raised.
fn held_beside(context: &Context, beside: Option<Beside>) -> Option<Beside> {
    match beside {
        Some(Beside::Rejected(rejected)) => {
            let Rejected {
                metavar,
                candidate,
                error,
            } = *rejected;
            Some(Beside::Rejected(Box::new(Rejected {
                metavar,
                candidate: held(context, &candidate),
                error,
            })))
        }
        beside => beside,
    }
}

impl Error {
    /// This error with every report it holds built, each standing on the frames it was raised on: what a refusal reports, made where the refusal stands and before what its attempt wrote is undone.
    pub(crate) fn shown(self, context: &mut Context) -> Self {
        match self {
            Self::Pending(pending) => {
                let Pending { frames, build } = *pending;
                context
                    .standing_on(&frames, |context| build(&mut Reporting(context)))
                    .shown(context)
            }
            Self::Located { extent, error } => Self::Located {
                extent,
                error: Box::new(error.shown(context)),
            },
            Self::InDeclaration { names, error } => Self::InDeclaration {
                names,
                error: Box::new(error.shown(context)),
            },
            Self::InUnreachableArm { dead, error } => Self::InUnreachableArm {
                dead,
                error: Box::new(error.shown(context)),
            },
            Self::InScope { witnesses, error } => Self::InScope {
                witnesses,
                error: Box::new(error.shown(context)),
            },
            Self::RejectedCandidate {
                watched,
                candidate,
                error,
            } => Self::RejectedCandidate {
                watched,
                candidate,
                error: Box::new(error.shown(context)),
            },
            Self::Batch(errors) => Self::Batch(
                errors
                    .into_iter()
                    .map(|error| error.shown(context))
                    .collect(),
            ),
            error => error,
        }
    }
}

/// The two sides of a conversion the drain keeps for the report a written goal gets, in display form: a goal is always reported, so the drain is where its report is made.
pub(crate) fn obligation_shown(context: &mut Context, this: &Term, that: &Term) -> (Term, Term) {
    let mut reporting = Reporting(context);

    (
        resolved_for_display(&mut reporting, this),
        resolved_for_display(&mut reporting, that),
    )
}

/// `term` as reading the context gives it for a report: each metavariable parked numerals wait on shown at the default they would take, the solutions that have landed substituted, qualities settled and recursive names given back. Tolerant: a metavariable still open spells `?` while every solved one beside it shows its value — the strict `zonk` would refuse the whole term on the first open hole, so one unsolved `f` would render `?(?)` where `?(double(p))` is known. Universe levels come through verbatim and the report's spelling erases them.
///
/// A committed solution spells a stuck recursive call as its canonical neutral, the `Rec` node itself, and normalization keeps a name only where the *written* head is one (`stalled_unfolding`), so the node is given back its name here, which the stall rule then holds where the report normalizes.
fn held(context: &Context, term: &Term) -> Term {
    let term = &numerals_at_their_default(context, term);

    refold_recs(
        context,
        &settle_qualities(context, &zonk_solved_term_metas(context, term)),
    )
}

/// Best-effort display form for a report: `term` as reading the context gives it ([`held`]), deep-[`normalize`]d so a stuck concept-method projection standing in an index position collapses to the value it denotes (`Vec(Nat, (sys/witness@0).0(0, 1))` → `Vec(Nat, 1)`) rather than surfacing compiler-internal witness machinery, and its recursive names given back again for whatever normalization exposed. A normalization that exhausts its budget falls back to the held form.
///
/// Normalization is the whole story only while the operand type is concrete. Under a `use Add(A)` parameter the projection is stuck on an abstract witness and no amount of reduction reaches the operator; the printer spells it, against the witness binders the report carries (axis (h)).
pub(crate) fn resolved_for_display(reporting: &mut Reporting, term: &Term) -> Term {
    let held = held(reporting, term);
    let resolved = normalize(reporting, held.clone()).unwrap_or(held);

    refold_recs(reporting, &resolved)
}

/// `term` with each metavariable parked numerals wait on shown as the default they would take together. A report can be made before their term completes — a turnaround that fails while a numeral still waits — and would show `Option(?)` for `Option/some(1)`, whose type every reader knows. A side that is such a metavariable itself is left as it stands: the report is then about what that metavariable may be, which the default does not answer.
fn numerals_at_their_default(context: &Context, term: &Term) -> Term {
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

/// A `type_mismatch` error naming both sides in their best-effort display form (see [`resolved_for_display`]) — unless `term`, the node the conversion was about, is the `/std/Monad/bind` application a postfix `!` desugars to and the region it hoisted to has nothing to sequence in, or the sides are the sorts a specialized report names. `sides` are the two held unsettled, which [`sort_decided_twice`] reads.
///
/// `bind` produces `M(B)`, so the generic report names a type the author never wrote, mentions neither the `!` nor the region that rejected it, and leaks the still-unsolved `B`. The specialized report names the monad the region would have to be and the type it actually has.
fn display_mismatch(
    reporting: &mut Reporting,
    term: &Term,
    [this_side, that_side]: &[Term; 2],
    this: &Term,
    that: &Term,
) -> Error {
    if let Some(report) = sort_decided_twice(reporting, this_side, that_side) {
        return report;
    }

    let this = resolved_for_display(reporting, this);
    let that = resolved_for_display(reporting, that);

    if is_sequencing(reporting, term) && wraps_a_result(&this) && !wraps_a_result(&that) {
        return Error::stranded_sequencing(required_region_type(reporting, &this), that);
    }
    if let Some(report) = unembedded_action(reporting, &this, &that) {
        return report;
    }
    if let Some(report) = implicit_at_other_sort(reporting, &this, &that) {
        return report;
    }

    Error::type_mismatch(this, that)
}

/// `report`, a mismatch's, with what conversion said beside the mismatch. The plain report of two sides notes what was left uncompared, and gives way to a rejected candidate's, its two sides not having been found apart. Any other report says what it says already: one that names the cause, an implicit handed a term of another sort among them, is not replaced by the check that refused the same thing.
fn said_beside(reporting: &mut Reporting, report: Error, beside: Option<Beside>) -> Error {
    match (report, beside) {
        (Error::TypeMismatch { .. }, Some(Beside::Rejected(rejected))) => {
            rejected_candidate(reporting, *rejected)
        }
        (report, beside) => report.declined(beside.and_then(Beside::declined)),
    }
}

/// The report of a candidate conversion found for a metavariable and saw refused where the metavariable was born ([`Rejected`]): the metavariable by what it is, the candidate as a report shows a term, and the check's refusal. It stands in place of the mismatch of the pair conversion was comparing, which was not found apart.
fn rejected_candidate(reporting: &mut Reporting, rejected: Rejected) -> Error {
    let Rejected {
        metavar,
        candidate,
        error,
    } = rejected;

    Error::rejected_candidate(
        watched(reporting, metavar.id, Some(&metavar.origin)),
        resolved_for_display(reporting, &candidate),
        error,
    )
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
/// Read on the expected side, this is what separates a stranded `!` from an ordinary mismatch inside a real region — `Io(Str)` against `Io(Nat)` disagrees about the result, while `Io(?)` against `Nat` is a region that cannot sequence at all. `Io` is named among the intrinsics because it is the one monad whose former is an intrinsic node rather than an application or a parameterized nominal type.
fn wraps_a_result(type_: &Term) -> bool {
    match &**type_ {
        Subterm::Apply(_) | Subterm::Intrinsic(Intrinsic::IoType(_)) => true,
        Subterm::InductType(induct) => !induct.params.is_empty(),
        Subterm::StructType(struct_) => !struct_.params.is_empty(),
        _ => false,
    }
}

/// What a stranded-sequencing report names as the region's required type: the sequenced `M(B)` with every metavariable it still carries blanked to one goal nothing solves, rendered `?`. The report wants `M(B)` for its `M` alone — `B` is what the continuation would have produced, and a region that cannot sequence never determines it.
///
/// `None` when `M` is itself one of those holes: nothing pinned the monad, so the whole spelling would be placeholders and the report says "a monad" instead.
fn required_region_type(context: &mut Context, sequenced: &Term) -> Option<Term> {
    if let Subterm::Apply(Apply { head, .. }) = &**sequenced
        && matches!(&**head, Subterm::Metavar(_))
    {
        return None;
    }

    let hole = Term::hole(context.mint_metavar());
    // Memoized on node identity: every metavariable blanks to the one hole, so a node's answer is the node's alone.
    let mut visit = Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, term: &Term| {
            matches!(&**term, Subterm::Metavar(_)).then(|| hole.clone())
        }),
    );

    Some(sequenced.traverse(&mut visit))
}
