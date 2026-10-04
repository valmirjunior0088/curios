use {
    super::{Misaligned, align, flexible, trivially_inhabited},
    crate::{
        ArgumentSite, Context, Entailed, Error, FrozenFrame, Mode, ParkedWork, SettleTier,
        attempt_witness_goal, blocked_on_metavar, callee, check, check_is_sort, elaborate, entail,
        exhausted_bound, expect, is_prop, reduce_with, settle_against, sort_term,
        transitively_ground,
    },
    curios_core::{
        Advance, Apply, CalleeId, Cursor, Free, FuncType, ImplicitOrigin, InstanceHead, Intrinsic,
        MetavarId, One, Probe, Scope, Spelling, Subterm, Telescope, Term, WitnessOrigin,
    },
    curios_utilities::Plicity,
    std::collections::BTreeSet,
};

pub(super) fn elaborate_func_type(
    context: &mut Context,
    ft: &FuncType,
) -> Result<(Term, Term), Error> {
    // One walk, each domain opened once at the binders before it, rather than a recursion reopening the rest per binder.
    let mut domains = Vec::new();
    let output = context.with_frame(|context| {
        let mut cursor = ft.telescope.cursor();
        while let Some((_, ty)) = cursor.entry() {
            let domain = check_is_sort(context, &ty)?.0;
            // A definition sugar's parameter is one written binder in its type and in its lambda, so a proof written under it here credits it as one written in the body does.
            let written = cursor.written();
            let name = cursor.advance_fresh(|hint| context.fresh_for(hint, written));
            // Assume the *rebuilt* domain: insertion saturates applications during elaboration, and a lowered (under-applied) type leaking into later reduction would open a telescope at the wrong arity. A `use` binder additionally joins the witness scope: the rest of the type may itself need resolution through it.
            match ft.plicities().get(domains.len()) {
                Some(Plicity::Witness) => {
                    check_witness_domain(context, &domain)?;
                    context.assume_witness(&name, &domain);
                }
                _ => context.assume(&name, &domain),
            }
            domains.push((name, domain));
        }

        let output = cursor.body().expect("a cursor past every entry");
        let output = check_is_sort(context, &output).map(|(term, _)| term)?;

        // An `@` member is filled by unification, which needs a later type to mention it, and resolution answers only `use` slots. One at a concept's type that nothing later mentions could only ever be written out, so every call leaving it out would fail far from here, as an implicit that was not inferred: refused where it is declared, beside the dual [`check_witness_domain`] refuses. One a later type does mention — `@from: Key(K)` beside `map: Map(K, use from, V)` — is determined, and stays.
        for (index, (name, domain)) in domains.iter().enumerate() {
            let mentioned = || {
                domains[index + 1..]
                    .iter()
                    .map(|(_, later)| later)
                    .chain([&output])
                    .any(|later| later.free_vars().contains(name))
            };
            if ft.plicities().get(index) != Some(&Plicity::Implicit) || mentioned() {
                continue;
            }
            let reduced = reduce_with(context, domain)?;
            if matches!(&*reduced, Subterm::StructType(struct_type) if context.concept(&struct_type.name).is_some())
            {
                return Err(Error::implicit_concept_member_unfillable(domain.clone())
                    .at_opt(domain.span()));
            }
        }

        Ok::<_, Error>(output)
    })?;

    let rebuilt = Term::func_type_marked(
        ft.plicities()
            .iter()
            .zip(domains)
            .map(|(&plicity, (label, domain))| (plicity, label, domain)),
        output,
    );

    let sort = sort_term(context, &rebuilt)?;
    Ok((rebuilt, sort))
}

/// Refuse a written `use` binder whose type is not a concept application.
///
/// Resolution answers a `use` slot with a concept's witness and nothing else, so a binder at any other type could only ever be filled by writing its argument out, and every call that omitted it failed at resolution — far from the declaration, naming whatever the type happened to reduce to. A type still headed by an unsolved metavariable is let through, since it may yet be solved to a concept application.
pub(super) fn check_witness_domain(context: &mut Context, domain: &Term) -> Result<(), Error> {
    let reduced = reduce_with(context, domain)?;
    let admitted = match &*reduced {
        Subterm::StructType(struct_type) => context.concept(&struct_type.name).is_some(),
        _ => flexible(context, &reduced),
    };
    if admitted {
        return Ok(());
    }

    // Only a hint: a sort that cannot be computed must not hide the refusal it decorates.
    let proposition = is_prop(context, domain).unwrap_or(false);
    Err(Error::use_parameter_not_a_concept(domain.clone(), proposition).at_opt(domain.span()))
}

/// A one-based position as an English ordinal, for naming which slot of a call a goal belongs to.
pub(crate) fn ordinal(index: usize) -> String {
    let position = index + 1;
    let suffix = match (position % 10, position % 100) {
        (_, 11..=13) => "th",
        (1, _) => "st",
        (2, _) => "nd",
        (3, _) => "rd",
        _ => "th",
    };
    format!("{position}{suffix}")
}

/// How a witness goal names the slot it fills: its position among the head's `use` slots.
///
/// A position rather than a name, because a `use` parameter *has* no name — `let`, `rec` and `satisfy` sugar all declare one anonymously, so a name would read `_` for every premise of user-written code. Only the generated method wrappers write `use w`, and `w` is an implementation detail appearing in no program.
pub(crate) fn premise_label(index: usize) -> String {
    format!("its {} 'use' premise", ordinal(index))
}

/// Where each slot of a call sits among the slots of its own kind — the position a report names it by, whether it was written or filled: "its 2nd 'use' premise". Every slot a walk visits passes through [`SlotPositions::next`] in telescope order.
#[derive(Default)]
pub(crate) struct SlotPositions {
    explicit: usize,
    implicit: usize,
    witness: usize,
}

impl SlotPositions {
    /// The 0-based position of the next slot of `plicity` among the slots of its kind, advancing past it.
    pub(crate) fn next(&mut self, plicity: Plicity) -> usize {
        let counter = match plicity {
            Plicity::Explicit => &mut self.explicit,
            Plicity::Implicit => &mut self.implicit,
            Plicity::Witness => &mut self.witness,
        };
        let position = *counter;
        *counter += 1;
        position
    }
}

/// Fill an omitted non-explicit slot: an implicit binder gets a fresh metavariable; a witness binder gets a fresh metavariable *plus* a resolution goal, attempted eagerly (solved now, parked on a flex key, or deferred on a missing table entry). `origin` is the application node — the span anchor for the goal. `position` is the slot's place among the head's slots of its kind, which names a witness goal's premise.
pub(super) fn insert_auto_argument(
    context: &mut Context,
    plicity: Plicity,
    type_: &Term,
    label: Option<&str>,
    func: &CalleeId,
    origin: &Term,
    position: usize,
) -> Result<Term, Error> {
    let binder = binder_name(label);

    match plicity {
        // An obligation already decided in the goal's favour is filled here, because *here* is where the facts that decide it are in scope: a scrutinee refinement lives only inside its arm, so an index guarded by `i < len(b)` has its bound established in the arm the call sits in, and the inhabitant written here sits inside that arm. One that follows from the facts in scope is proved here for the same reason ([`crate::entail`]). A bound not yet decided because its subject still waits on a metavariable — one a later argument or the expectation pins — is parked instead, and filled once the subject is known ([`attempt_discharge`]); one waiting only on a recursive member's slot is asked of that procedure first ([`waits_past_its_group`]).
        Plicity::Implicit => {
            let provenance = ImplicitOrigin {
                func: func.clone(),
                binder,
            };
            let (reduced, inhabitant) = trivially_inhabited(context, type_)
                .map_err(|error| bound_exhausted(context, error, type_, &provenance))?;
            if let Some(inhabitant) = inhabitant {
                return Ok(inhabitant);
            }

            // Whether the slot is a bound or a value is decided here, where the sort can still be asked, and kept on the birth record for the report an unsolved one becomes — with what the bound reduced to, when that is an inductive type the report can name.
            let proposition = is_prop(context, type_).probed()?.unwrap_or(false);
            let waiting = proposition && waits_on_metavariable(context, &reduced);
            let mut refusal = None;
            if proposition && !waits_past_its_group(context, &reduced) {
                match entail(context, type_, &reduced)
                    .map_err(|error| bound_exhausted(context, error, type_, &provenance))?
                {
                    Entailed::Proved(proof) => return Ok(proof),
                    Entailed::Refused(refused) => refusal = Some(refused),
                }
            }
            let reduct =
                (proposition && matches!(&*reduced, Subterm::InductType(_))).then_some(reduced);
            let (slot, hole) = context.fresh_metavar(
                type_.clone(),
                origin.span(),
                provenance.clone(),
                proposition,
                reduct,
            );
            if let Some(refusal) = refusal {
                context.note_refusal(slot, refusal);
            }
            if waiting && !context.parking_suppressed() {
                context.park(
                    ParkedWork::Discharge {
                        slot,
                        bound: type_.clone(),
                        provenance,
                    },
                    origin.clone(),
                );
            }
            Ok(hole)
        }
        Plicity::Witness => {
            let provenance = WitnessOrigin {
                func: func.clone(),
                binder: premise_label(position),
            };
            let (id, metavar) =
                context.fresh_witness_metavar(type_.clone(), origin.span(), provenance.clone());
            attempt_witness_goal(context, id, type_, provenance, origin)?;
            Ok(metavar)
        }
        Plicity::Explicit => unreachable!("explicit slots are never auto-filled"),
    }
}

/// Try a bound standing as the hole `slot` once more: fill the hole if the bound has come to truth, record what it reduced to if it came to anything else, and answer whether it still waits on a metavariable.
///
/// The fill is a metavariable solution, so the bound is reduced as re-validation judges one: under the refinements the slot was born under and no others ([`Context::with_refinements`]). A bound true under the guard of the arm the call sits in is filled on retry as it would have been at insertion, and the fill cannot leave that arm: a solution for a metavariable born outside it that mentions the unfilled slot is not contained in that metavariable's birth context and waits, and once the slot is filled it is re-validated without the guard. A bound true only under a guard the slot was not born under stays unfilled and is reported. A bound that follows from the facts in scope under those refinements is proved there as well ([`crate::entail`]).
pub(crate) fn attempt_discharge(
    context: &mut Context,
    slot: MetavarId,
    bound: &Term,
    provenance: &ImplicitOrigin,
) -> Result<bool, Error> {
    if context.metavar_solution(slot).is_some() {
        return Ok(false);
    }
    let birth = context
        .metavar_entry(slot)
        .map(|entry| entry.refinements.clone())
        .expect("a bound's slot has a birth record");
    let (reduced, inhabitant) = context
        .with_refinements(&birth, |context| trivially_inhabited(context, bound))
        .map_err(|error| bound_exhausted(context, error, bound, provenance))?;
    if let Some(inhabitant) = inhabitant {
        context.solve_metavar(slot, inhabitant);
        return Ok(false);
    }
    if waits_past_its_group(context, &reduced) {
        return Ok(true);
    }
    match context
        .with_refinements(&birth, |context| entail(context, bound, &reduced))
        .map_err(|error| bound_exhausted(context, error, bound, provenance))?
    {
        Entailed::Proved(proof) => {
            context.solve_metavar(slot, proof);
            return Ok(false);
        }
        Entailed::Refused(refusal) => context.note_refusal(slot, refusal),
    }
    // Waiting on the group's own members alone: reduction may yet decide the bound once they are defined.
    if waits_on_metavariable(context, &reduced) {
        return Ok(true);
    }
    if matches!(&*reduced, Subterm::InductType(_)) {
        context.note_reduct(slot, reduced);
    }
    Ok(false)
}

/// Retry a parked discharge under the frame it was parked in ([`attempt_discharge`]), parking it again while it still waits.
pub(crate) fn retry_discharge(
    context: &mut Context,
    slot: MetavarId,
    bound: Term,
    provenance: ImplicitOrigin,
    origin: Term,
    frame: FrozenFrame,
) -> Result<(), Error> {
    let waiting = context
        .with_retry_frame(&frame, |context| {
            attempt_discharge(context, slot, &bound, &provenance)
        })
        .map_err(|error| error.at_opt(origin.span()))?;
    if waiting {
        context.repark(
            ParkedWork::Discharge {
                slot,
                bound,
                provenance,
            },
            origin,
            frame,
        );
    }
    Ok(())
}

/// Whether a bound's reduct still waits on an unsolved metavariable, so that a later solution may yet bring it to truth. A reduct that waits on none has said all it will.
fn waits_on_metavariable(context: &Context, reduct: &Term) -> bool {
    reduct
        .metavars()
        .iter()
        .any(|id| context.metavar_solution(*id).is_none())
}

/// Whether a bound's reduct waits on an unsolved metavariable other than the slot of a `rec` group's member — what reduction turns a recursive reference into while the group is checked. A bound waiting on such slots alone is still asked of the procedure that proves a bound from the facts in scope ([`crate::entail`]): the slot's solution is the body the bound sits in, so it waits on the bound itself, and a proof from the facts is written over the spellings in scope, which name the member rather than its slot.
fn waits_past_its_group(context: &Context, reduct: &Term) -> bool {
    reduct
        .metavars()
        .iter()
        .any(|id| context.metavar_solution(*id).is_none() && !context.is_rec_slot(*id))
}

/// An exhausted discharge of a bound, re-reported by the partial definition it names when it names one ([`exhausted_bound`]).
fn bound_exhausted(
    context: &Context,
    error: Error,
    bound: &Term,
    provenance: &ImplicitOrigin,
) -> Error {
    let site =
        callee(context, &provenance.func).slot("bound", &provenance.binder, &Spelling::default());
    exhausted_bound(context, error, bound, site)
}

/// A binder's user-facing name: its minting hint, or `_` where it has none.
///
/// The head's function type is the *rebuilt* one, whose binders were re-closed under freshly minted identities; a report should still name the binder as written, and the hint is what the mint carried forward.
pub(super) fn binder_name(hint: Option<&str>) -> String {
    hint.unwrap_or("_").to_string()
}

pub(super) fn elaborate_apply(
    context: &mut Context,
    apply: &Apply,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    curios_profile::profile!("apply::elaborate_apply");
    let Apply { head, arguments } = apply;

    // Insertion provenance: name the applied function in the uninferred-implicit report.
    //
    // Through the spine, not just at its top: a curried call — `Fmt/print(fmt)(a)(b)`, and every partial application — heads the outer apply with another *apply*, so reading only the outermost node would report `<function>` for exactly the calls a reader most needs named. The innermost reference is the one the program wrote.
    fn innermost_reference(term: &Term) -> Option<Free> {
        match &**term {
            Subterm::Var(var) => Some(*var.unwrap()),
            Subterm::Apply(apply) => innermost_reference(&apply.head),
            Subterm::Instance(instance) => match &instance.head {
                InstanceHead::Var(var) => Some(*var.unwrap()),
                InstanceHead::RecProj(..) => None,
            },
            _ => None,
        }
    }
    let func_label = innermost_reference(head).map_or(CalleeId::Anonymous, CalleeId::Function);

    let (head, written_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &written_type)?;

    // One call fills exactly one parameter list: the head's own. A function returning a function is called once per list — `f(a)(b)` — and a list of hidden parameters alone is no exception, so `Eq()(x, y)` is how an all-implicit list is passed on to the one after it. See documentation/design/theory/a-call-fills-one-parameter-group.md.
    let ft = match &*head_type {
        Subterm::FuncType(ft) => ft.clone(),
        other => return Err(Error::not_a_function(written_type.clone(), other.clone())),
    };
    let mut positions = SlotPositions::default();

    // One walk matches the written arguments to the head's slots ([`align`]): the plain ones are the explicit slots, in order, and between two of them the hidden ones written are the first of their run, in order.
    let written = arguments
        .iter()
        .map(|argument| argument.plicity)
        .collect::<Vec<_>>();
    let fills = align(ft.plicities(), &written).map_err(|misaligned| match misaligned {
        Misaligned::Plain => {
            let explicit = |marks: &[Plicity]| {
                marks
                    .iter()
                    .filter(|mark| matches!(mark, Plicity::Explicit))
                    .count()
            };
            Error::wrong_number_of_arguments(explicit(ft.plicities()), explicit(&written))
        }
        Misaligned::Mark { member, slot } => Error::hidden_member_out_of_order(
            written[member],
            ft.plicities()[slot],
            ft.telescope.labels()[slot],
        )
        .at_opt(arguments[member].term.span()),
        Misaligned::Surplus { member } => {
            Error::hidden_member_without_slot(written[member]).at_opt(arguments[member].term.span())
        }
    })?;

    // Whether the expected type is fully ground. The codomain postponement is only a win when `expect(output, expected)` actually *grounds* the result metavar; if `expected` itself carries an unsolved metavar, that turnaround is flex-flex and the metavar must instead be grounded by the continuation's body — so postponing it would strand the metavar (flex-flex-under-constructor) rather than refine it. When expected is not ground the argument checks eagerly.
    let expected_ground = match &mode {
        Mode::Check(expected) => expected
            .metavars()
            .iter()
            .all(|&id| transitively_ground(context, id)),
        Mode::Infer => false,
    };

    // The single walk. Every slot settles in telescope order, and the dependent substitution only ever receives elaborated terms or compiler-born metavariables — the invariant is the code path, not a guard. A written argument is checked at its domain, opened through the elaborated prefix; a checked-only intro form whose structure is still blocked (see `blocked_on_metavar`) becomes a parked checking problem whose placeholder stands in the telescope, retried by the wake machinery the moment a solution lands, so a sibling's turnaround retries the parked check before any later slot opens through it. The park is minted in both modes: an inferred apply has no turnaround, but the force tier below settles what the walk leaves blocked, and a park `check` made on its own would sit in the store beyond that tier's reach. A missing hidden slot is inserted at that same true domain. Under suppressed parking the blocked case checks eagerly instead: re-validation re-elaborates rebuilt nodes whose types are already solved, so the branch is dead over the corpus and merely safe.
    let original = ft.telescope.clone();
    let mut elaborated: Vec<Term> = Vec::with_capacity(ft.plicities().len());
    // The pendings this apply minted: (slot, placeholder, written term), consulted by the fallback pin below.
    let mut pendings: Vec<(usize, MetavarId, Term)> = Vec::new();
    let mut cursor = original.cursor();
    for (index, plicity) in ft.plicities().iter().enumerate() {
        let (hint, ty) = cursor.entry().expect("plicities parallel the telescope");
        let position = positions.next(*plicity);
        // A hidden argument written `_` holds its slot's place and says nothing, so the slot is filled as one left out is.
        let written = fills[index]
            .map(|member| arguments[member].term.clone())
            .filter(|written| *plicity == Plicity::Explicit || !is_placeholder(context, written));
        let arg = match written {
            Some(written) => {
                let blocked = !context.parking_suppressed()
                    && matches!(
                        &*written,
                        Subterm::Func(_)
                            | Subterm::Tuple(_)
                            | Subterm::Intrinsic(Intrinsic::List { .. })
                    )
                    && {
                        let result_metavars = result_metavars_from(context, &opened_link(&cursor));
                        blocked_on_metavar(
                            context,
                            &written,
                            &ty,
                            &result_metavars,
                            expected_ground,
                        )?
                    };
                if blocked {
                    let (placeholder, stand_in) =
                        context.fresh_placeholder(ty.clone(), written.span());
                    context.park(
                        ParkedWork::Checking {
                            term: written.clone(),
                            expected: ty.clone(),
                            placeholder,
                        },
                        written.clone(),
                    );
                    pendings.push((elaborated.len(), placeholder, written));
                    stand_in
                } else {
                    check(context, &written, ty.clone()).map_err(|error| {
                        error.at_argument(argument_site(
                            &func_label,
                            *plicity,
                            position,
                            &opened_link(&cursor),
                            &ft.plicities()[index + 1..],
                        ))
                    })?
                }
            }
            None => {
                insert_auto_argument(context, *plicity, &ty, hint, &func_label, term, position)?
            }
        };
        cursor.advance(arg.clone());
        elaborated.push(arg);
    }
    let output = cursor.body().expect("plicities parallel the telescope");

    if let Mode::Check(expected) = &mode {
        // The output carries any pending's placeholder, which *blocks* rather than manufacturing a raw substitution's false mismatches — so this turnaround runs unbracketed, a mismatch propagates as genuine, and its pins wake parked checks through the ordinary retry machinery with every discharged obligation's solutions kept.
        expect(context, term, &output, expected)?;
    }

    if !pendings.is_empty() {
        // The force tier: a pending still undischarged after the turnaround above is checked now, under whatever it pinned. A lambda whose domain only its own body can ground is grounded by that body here; the placeholder takes the checked term, so no pending outlives its apply undischarged. The parked copy of the obligation reconciles against this solution when its retry fires. A bracketed best-effort expect over the *raw* spellings before this tier would add nothing: measured across the corpus, it discharges no pending the wake machinery and this tier do not.
        //
        // The tier runs in both modes. In checking mode the turnaround above has consulted the expectation; in inference mode there is no expectation to consult, and without the tier a `let z = id((1, true))` would leave its tuple parked until the item's drain, where `z.0` would already have met a bare metavariable. Either way no later slot opens through this one, so the tier's premise holds: nothing is left to give the expectation structure, and an inferred call commits to its argument's product exactly as a bare literal does.
        for (slot, placeholder, written) in &pendings {
            if context.metavar_solution(*placeholder).is_none() {
                let slot_ty = original
                    .clone()
                    .nth(*slot, |k| elaborated[k].clone())
                    .expect("pending slot is within the telescope");
                // A form that can be synthesized takes its product *here* rather than at the item's drain, so the rest of the item sees a real type — a projection off the result would otherwise check against a metavariable that only settles after every expression around it.
                let checked = match settle_against(context, written, &slot_ty, SettleTier::Force)? {
                    Some(settled) => settled,
                    None => check(context, written, slot_ty)?,
                };
                context.solve_metavar(*placeholder, checked.clone());
                elaborated[*slot] = checked;
            }
        }

        // The authoritative turnaround, through fully settled arguments.
        if let Mode::Check(expected) = &mode {
            expect(context, term, &output, expected)?;
        }
    }

    // The rebuilt application is fully saturated; each argument's mark is its binder's plicity (inserted metavariables recorded like any other argument), so re-elaborating the rebuilt node is stable: every slot is then written, in telescope order, and nothing is minted twice.
    Ok((
        Term::apply_marked(head, ft.plicities().iter().copied().zip(elaborated)),
        output,
    ))
}

/// Whether a written hidden member is the placeholder `_`: a silent hole nothing has birthed, which is what lowering writes for it. A birthed one is a term the compiler supplies — the monad a bind is built at, a parked argument's stand-in — and is an argument like any other; a written `?` is a goal.
pub(super) fn is_placeholder(context: &Context, term: &Term) -> bool {
    matches!(&**term, Subterm::Metavar(metavar) if metavar.is_hole() && context.metavar_entry(metavar.id).is_none())
}

/// The link at the cursor's entry, opened at every argument before it: what [`argument_site`] and `result_metavars_from` read, since a later domain's shape can depend on an earlier argument. It costs the remainder's size, so only the paths that read it — a failed check, a literal that might park — ask for it.
fn opened_link(cursor: &Cursor<'_, Term>) -> Scope<One, Telescope<Term>> {
    match cursor.rest() {
        Telescope::Cons(_, link) => link,
        Telescope::Done(_) => unreachable!("an entry stands at the cursor"),
    }
}

/// Where the argument just checked sits: the parameter it filled, the mark it was written with and its position among the arguments written with that mark, and — for a plain argument — the next explicit parameter of function type, if any, the slot a lambda handed in here was likely meant for.
///
/// A `use` slot's binder goes unnamed, since no program names it — the method wrappers' `w` is the only name one ever carries. A hidden argument is pointed at no plain parameter: the hint is for swapped plain arguments, and an author who wrote `@` or `use` chose a hidden slot on purpose.
fn argument_site(
    function: &CalleeId,
    plicity: Plicity,
    position: usize,
    rest: &Scope<One, Telescope<Term>>,
    later_plicities: &[Plicity],
) -> ArgumentSite {
    let mut function_typed = None;
    if plicity == Plicity::Explicit {
        let mut cursor = rest.body();
        let mut explicit = position + 1;
        for later in later_plicities {
            let Telescope::Cons(ty, next) = cursor else {
                break;
            };
            if *later == Plicity::Explicit {
                if matches!(&**ty, Subterm::FuncType(_)) {
                    function_typed = Some((next.first_hint().map(str::to_string), explicit));
                    break;
                }
                explicit += 1;
            }
            cursor = next.body();
        }
    }
    let parameter = match plicity {
        Plicity::Explicit | Plicity::Implicit => rest.first_hint().map(str::to_string),
        Plicity::Witness => None,
    };
    ArgumentSite {
        function: function.clone(),
        parameter,
        plicity,
        position,
        function_typed,
    }
}

/// The metavariables the result `expect` can pin, as seen from one slot: the suffix telescope's terminal with this and every later binder opened as a fresh variable. Only prefix-born metavariables can occur in a slot's own domain — domains open over the prefix — so fresh-var opening of the unvisited suffix is decision-equivalent to a full-argument pre-read of the output, computed only for postponement candidates instead of once per application.
fn result_metavars_from(
    context: &mut Context,
    rest: &Scope<One, Telescope<Term>>,
) -> BTreeSet<MetavarId> {
    let suffix = rest.open(&[&Term::free_var(&context.fresh(rest.first_hint()))]);
    let mut cursor = suffix.cursor();
    while !cursor.is_done() {
        cursor.advance_fresh(|hint| context.fresh(hint));
    }
    cursor.body().expect("a cursor past every entry").metavars()
}
