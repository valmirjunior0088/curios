use {
    super::*,
    crate::{ArgumentSite, FrozenFrame, SettleTier, callee, exhausted_bound},
    curios_core::{Advance, CalleeId, Cursor, Spelling},
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
            let domain = crate::check_is_sort(context, &ty)?.0;
            let name = cursor.advance_fresh(|hint| context.fresh(hint));
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
        crate::check_is_sort(context, &output).map(|(term, _)| term)
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
    let proposition = crate::is_prop(context, domain).unwrap_or(false);
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
/// A position rather than a name, because a `use` parameter *has* no name — `let`, `rec` and `satisfy` sugar all declare one anonymously, so every premise of user-written code reported as `_`. Only the generated method wrappers write `use w`, and `w` is an implementation detail appearing in no program.
pub(crate) fn premise_label(index: usize) -> String {
    format!("its {} 'use' premise", ordinal(index))
}

/// Where each slot of a call sits among the slots of its own kind — the position a report names it by, whether it was written or filled.
///
/// Written `@` and `use` arguments fill the slots of their kind in order, so a slot's position among its kind is also the position of the argument written for it, and one count serves both the argument a refusal names and the premise a witness goal names. Every slot a walk visits passes through [`SlotPositions::next`] in telescope order; a walk that saturates several telescopes of one call carries one value across them, as it carries the written queues.
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
        // An obligation already decided in the goal's favour is filled here, because *here* is where the facts that decide it are in scope: a scrutinee refinement lives only inside its arm, so an index guarded by `i < len(b)` has its bound established at the call and nowhere afterwards, and the inhabitant written here sits inside that arm. A bound not yet decided because its subject still waits on a metavariable — one a later argument or the expectation pins — is parked instead, and filled once the subject is known ([`attempt_discharge`]).
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
            let proposition = crate::is_prop(context, type_).unwrap_or(false);
            let waiting = proposition && waits_on_metavariable(context, &reduced);
            let reduct =
                (proposition && matches!(&*reduced, Subterm::InductType(_))).then_some(reduced);
            let (slot, hole) = context.fresh_metavar(
                type_.clone(),
                origin.span(),
                provenance.clone(),
                proposition,
                reduct,
            );
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
/// The fill is a metavariable solution, spliced wherever the hole travelled — past the arm it was minted in, when unification carried it out — so the bound is reduced with every live refinement withheld. A bound true only inside an arm stays unfilled and is reported; the eager attempt at insertion, which writes its inhabitant into the arm itself, is the one that may use them.
pub(crate) fn attempt_discharge(
    context: &mut Context,
    slot: MetavarId,
    bound: &Term,
    provenance: &ImplicitOrigin,
) -> Result<bool, Error> {
    if context.metavar_solution(slot).is_some() {
        return Ok(false);
    }
    let (reduced, inhabitant) = context
        .with_suppressed_refinements(|context| trivially_inhabited(context, bound))
        .map_err(|error| bound_exhausted(context, error, bound, provenance))?;
    if let Some(inhabitant) = inhabitant {
        context.solve_metavar(slot, inhabitant);
        return Ok(false);
    }
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
        .with_frame(|context| {
            context.restore_frame(&frame);
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
/// The head's function type is the *rebuilt* one, whose binders were re-closed under freshly minted identities; a report should still name the binder as written, and the hint is what the mint carried forward. This used to cut the written name back out of a minted spelling.
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
    // Through the spine, not just at its top: a curried call — `Fmt/print(fmt)(a)(b)`, and every partial application — heads the outer apply with another *apply*, so reading only the outermost node reported `<function>` for exactly the calls a reader most needs named. The innermost reference is the one the program wrote.
    fn innermost_reference(term: &Term) -> Option<Free> {
        match &**term {
            Subterm::Var(var) => Some(var.unwrap().clone()),
            Subterm::Apply(apply) => innermost_reference(&apply.head),
            Subterm::Instance(instance) => match &instance.head {
                InstanceHead::Var(var) => Some(var.unwrap().clone()),
                InstanceHead::RecProj(..) => None,
            },
            _ => None,
        }
    }
    let func_label = innermost_reference(head).map_or(CalleeId::Anonymous, CalleeId::Function);

    let (mut head, written_type) = elaborate(context, head, Mode::Infer)?;
    let mut head_type = reduce_with(context, &written_type)?;

    // The three call-site queues: plain arguments fill explicit binders in telescope order, `@`-arguments fill implicit binders, `use`-arguments fill witness binders — each matched independently, so the relative position of a marked argument among the plain ones carries no meaning.
    let mut plain: VecDeque<Term> = VecDeque::new();
    let mut marked: VecDeque<Term> = VecDeque::new();
    let mut used: VecDeque<Term> = VecDeque::new();
    for argument in arguments {
        match argument.plicity {
            Plicity::Explicit => plain.push_back(argument.term.clone()),
            Plicity::Implicit => marked.push_back(argument.term.clone()),
            Plicity::Witness => used.push_back(argument.term.clone()),
        }
    }

    // All-auto telescopes (the curried `bind` shape, e.g. `(@A, @B) -> (M A, A -> M B) -> M B`, or a method wrapper's `(@A, use w) -> …`): when the head telescope has zero explicit slots but plain arguments were given, saturate it — marked queues first, fresh metavariables (and witness goals) for the rest — reduce the output, and re-target the plain arguments at the next telescope. This fires *only* with zero explicit slots, so application stays arity-strict everywhere else (this is deliberately not general partial application).
    let mut positions = SlotPositions::default();
    let ft = loop {
        let ft = match &*head_type {
            Subterm::FuncType(ft) => ft.clone(),
            other => return Err(Error::not_a_function(written_type.clone(), other.clone())),
        };

        let all_auto = !ft.plicities().is_empty()
            && ft
                .plicities()
                .iter()
                .all(|p| !matches!(p, Plicity::Explicit));
        if !all_auto || plain.is_empty() {
            break ft;
        }

        let mut args = Vec::with_capacity(ft.plicities().len());
        let mut cursor = ft.telescope.cursor();
        for (index, plicity) in ft.plicities().iter().enumerate() {
            let (hint, ty) = cursor.entry().expect("plicities parallel the telescope");
            let position = positions.next(*plicity);
            let queue = match plicity {
                Plicity::Implicit => &mut marked,
                Plicity::Witness => &mut used,
                Plicity::Explicit => unreachable!("all-auto telescope"),
            };
            let arg = match queue.pop_front() {
                Some(arg) => check(context, &arg, ty.clone()).map_err(|error| {
                    error.at_argument(argument_site(
                        &func_label,
                        *plicity,
                        position,
                        &opened_link(&cursor),
                        &ft.plicities()[index + 1..],
                    ))
                })?,
                None => {
                    insert_auto_argument(context, *plicity, &ty, hint, &func_label, term, position)?
                }
            };
            cursor.advance(arg.clone());
            args.push((*plicity, arg));
        }
        let output = cursor.body().expect("plicities parallel the telescope");

        head = Term::apply_marked(head, args);
        head_type = reduce_with(context, &output)?;
    };

    // Arity is checked per queue: plain arguments must exactly cover the explicit slots; `@`- and `use`-arguments may undershoot their slots (the remainder is inserted/resolved) but never overshoot them.
    let explicit_slots = ft
        .plicities()
        .iter()
        .filter(|p| matches!(p, Plicity::Explicit))
        .count();
    let implicit_slots = ft
        .plicities()
        .iter()
        .filter(|p| matches!(p, Plicity::Implicit))
        .count();
    let witness_slots = ft
        .plicities()
        .iter()
        .filter(|p| matches!(p, Plicity::Witness))
        .count();

    if plain.len() != explicit_slots {
        return Err(Error::wrong_number_of_arguments(
            explicit_slots,
            plain.len(),
        ));
    }
    if marked.len() > implicit_slots {
        return Err(Error::too_many_implicits(implicit_slots, marked.len()));
    }
    if used.len() > witness_slots {
        return Err(Error::too_many_witness_args(witness_slots, used.len()));
    }

    // Whether the expected type is fully ground. The codomain postponement is only a win when `expect(output, expected)` actually *grounds* the result metavar; if `expected` itself carries an unsolved metavar, that turnaround is flex-flex and the metavar must instead be grounded by the continuation's body — so postponing it would strand the metavar (flex-flex-under-constructor) rather than refine it. When expected is not ground the argument checks eagerly.
    let expected_ground = match &mode {
        Mode::Check(expected) => expected
            .metavars()
            .iter()
            .all(|&id| transitively_ground(context, id)),
        Mode::Infer => false,
    };

    // The single walk. Every slot settles in telescope order, and the dependent substitution only ever receives elaborated terms or compiler-born metavariables — the invariant is the code path, not a guard. A written argument is checked at its domain, opened through the elaborated prefix; a checked-only intro form whose structure is still blocked (see `blocked_on_metavar`) becomes a parked checking problem whose placeholder stands in the telescope, retried by the wake machinery the moment a solution lands — which subsumes the retired clear loop, since a sibling's turnaround retries the parked check before any later slot opens through it. The park is minted in both modes: an inferred apply has no turnaround, but the force tier below settles what the walk leaves blocked, and a park `check` made on its own would sit in the store beyond that tier's reach. A missing hidden slot is inserted at that same true domain. Under suppressed parking the blocked case checks eagerly instead: re-validation re-elaborates rebuilt nodes whose types are already solved, so the branch is dead over the corpus (fact F1) and merely safe.
    let original = ft.telescope.clone();
    let mut elaborated: Vec<Term> = Vec::with_capacity(ft.plicities().len());
    // The pendings this apply minted: (slot, placeholder, written term), consulted by the fallback pin below.
    let mut pendings: Vec<(usize, MetavarId, Term)> = Vec::new();
    let mut cursor = original.cursor();
    for (index, plicity) in ft.plicities().iter().enumerate() {
        let (hint, ty) = cursor.entry().expect("plicities parallel the telescope");
        let position = positions.next(*plicity);
        let written = match plicity {
            Plicity::Explicit => Some(plain.pop_front().expect("arity checked above")),
            Plicity::Implicit => marked.pop_front(),
            Plicity::Witness => used.pop_front(),
        };
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
        // The output carries any pending's placeholder, which *blocks* rather than manufacturing the raw-substitution false mismatches the retired design had to bracket against — so this turnaround runs unbracketed, a mismatch propagates as genuine, and its pins wake parked checks through the ordinary retry machinery with every discharged obligation's solutions kept.
        expect(context, term, &output, expected)?;
    }

    if !pendings.is_empty() {
        // The force tier — the retired settle, kept: a pending still undischarged after the turnaround above is checked now, under whatever it pinned. A lambda whose domain only its own body can ground is grounded by that body here, exactly as the settle once grounded it; the placeholder takes the checked term, so no pending outlives its apply undischarged. The parked copy of the obligation reconciles against this solution when its retry fires. The retired design also ran a bracketed best-effort expect over the *raw* spellings before settling — measured across the corpus, that pin never discharged a pending the wake machinery and this tier did not, so it is gone rather than kept.
        //
        // The tier runs in both modes. In checking mode the turnaround above has consulted the expectation; in inference mode there is no expectation to consult, and a `let z = id((1, true))` left its tuple parked until the item's drain, where `z.0` had already met a bare metavariable. Either way no later slot opens through this one, so the tier's premise holds: nothing is left to give the expectation structure, and an inferred call commits to its argument's product exactly as a bare literal does.
        for (slot, placeholder, written) in &pendings {
            if context.metavar_solution(*placeholder).is_none() {
                let slot_ty = original
                    .clone()
                    .nth(*slot, |k| elaborated[k].clone())
                    .expect("pending slot is within the telescope");
                // A form that can be synthesized takes its product *here* rather than at the item's drain, so the rest of the item sees a real type — a projection off the result would otherwise check against a metavariable that only settles after every expression around it.
                let checked =
                    match crate::settle_against(context, written, &slot_ty, SettleTier::Force)? {
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

    // The rebuilt application is fully saturated; each argument's mark is its binder's plicity (inserted metavariables recorded like any other argument), so re-elaborating the rebuilt node is stable: both queues then match their slots exactly and nothing is minted twice.
    Ok((
        Term::apply_marked(head, ft.plicities().iter().copied().zip(elaborated)),
        output,
    ))
}

/// Where the argument just checked sits: the parameter it filled, the mark it was written with and its position among the arguments written with that mark, and — for a plain argument — the next explicit parameter of function type, if any, the slot a lambda handed in here was likely meant for.
///
/// A `use` slot's binder goes unnamed, since no program names it — the method wrappers' `w` is the only name one ever carries. A hidden argument is pointed at no plain parameter: the hint is for swapped plain arguments, and an author who wrote `@` or `use` chose a hidden slot on purpose.
/// The link at the cursor's entry, opened at every argument before it: what [`argument_site`] and `result_metavars_from` read, since a later domain's shape can depend on an earlier argument. It costs the remainder's size, so only the paths that read it — a failed check, a literal that might park — ask for it.
fn opened_link(cursor: &Cursor<'_, Term>) -> Scope<One, Telescope<Term>> {
    match cursor.rest() {
        Telescope::Cons(_, link) => link,
        Telescope::Done(_) => unreachable!("an entry stands at the cursor"),
    }
}

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
