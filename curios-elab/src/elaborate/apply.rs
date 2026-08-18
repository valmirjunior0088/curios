use super::*;

pub(super) fn elaborate_func_type(
    context: &mut Context,
    ft: &FuncType,
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<Term>,
        plicities: &[Plicity],
        domains: &mut Vec<(Free, Term)>,
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(output) => crate::check_is_sort(context, &output).map(|(term, _)| term),
            Telescope::Cons(ty, rest) => {
                let domain = crate::check_is_sort(context, &ty)?.0;
                let name = context.fresh(rest.first_hint());
                let x = Term::free_var(&name);
                // Assume the *rebuilt* domain: insertion saturates applications during elaboration, and a lowered (under-applied) type leaking into later reduction would open a telescope at the wrong arity. A `use` binder additionally joins the witness scope: the rest of the type may itself need resolution through it.
                match plicities.get(domains.len()) {
                    Some(Plicity::Witness) => context.assume_witness(&name, &domain),
                    _ => context.assume(&name, &domain),
                }
                domains.push((name, domain));
                walk(context, rest.open(&[&x]), plicities, domains)
            }
        }
    }

    let mut domains = Vec::new();
    let output = context
        .with_frame(|context| walk(context, ft.telescope.clone(), &ft.plicities, &mut domains))?;

    let rebuilt = Term::func_type_marked(
        ft.plicities
            .iter()
            .zip(domains)
            .map(|(&plicity, (label, domain))| (plicity, label, domain)),
        output,
    );

    let sort = sort_term(context, &rebuilt)?;
    Ok((rebuilt, sort))
}

/// Fill an omitted non-explicit slot: an implicit binder gets a fresh metavariable; a witness binder gets a fresh metavariable *plus* a resolution goal, attempted eagerly (solved now, parked on a flex key, or deferred on a missing table entry). `origin` is the application node — the span anchor for the goal.
pub(super) fn insert_auto_argument(
    context: &mut Context,
    plicity: Plicity,
    type_: &Term,
    label: Option<&str>,
    func: &str,
    origin: &Term,
) -> Result<Term, Error> {
    let binder = binder_name(label);

    match plicity {
        // An obligation already decided in the goal's favour is filled here rather than deferred, because *here* is where the facts that decide it are in scope. A scrutinee refinement lives only inside its arm, so an index guarded by `i < len(b)` has its bound established at the call and nowhere afterwards — a hole minted now and swept at the item boundary would be reduced with the refinement already out of scope, and would report as uninferred against a caller who did establish it.
        Plicity::Implicit => Ok(trivially_inhabited(context, type_)?.unwrap_or_else(|| {
            context.fresh_metavar(
                type_.clone(),
                origin.span(),
                ImplicitOrigin {
                    func: func.to_string(),
                    binder: binder.clone(),
                },
            )
        })),
        Plicity::Witness => {
            let provenance = WitnessOrigin {
                func: func.to_string(),
                binder,
            };
            let (id, metavar) =
                context.fresh_witness_metavar(type_.clone(), origin.span(), provenance.clone());
            attempt_witness_goal(context, id, type_, provenance, origin)?;
            Ok(metavar)
        }
        Plicity::Explicit => unreachable!("explicit slots are never auto-filled"),
    }
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
    let Apply {
        head,
        params,
        plicities,
    } = apply;

    // Insertion provenance: name the applied function in the uninferred-implicit report. Heads are references in practice; anything else gets a placeholder (the span still locates the call).
    let func_label = match &**head {
        Subterm::Var(var) => var.unwrap().to_string(),
        _ => "<function>".to_string(),
    };

    let (mut head, head_type) = elaborate(context, head, Mode::Infer)?;
    let mut head_type = reduce_with(context, &head_type)?;

    // The three call-site queues: plain arguments fill explicit binders in telescope order, `@`-arguments fill implicit binders, `use`-arguments fill witness binders — each matched independently, so the relative position of a marked argument among the plain ones carries no meaning.
    let mut plain: VecDeque<Term> = VecDeque::new();
    let mut marked: VecDeque<Term> = VecDeque::new();
    let mut used: VecDeque<Term> = VecDeque::new();
    for (plicity, param) in plicities.iter().zip(params) {
        match plicity {
            Plicity::Explicit => plain.push_back(param.clone()),
            Plicity::Implicit => marked.push_back(param.clone()),
            Plicity::Witness => used.push_back(param.clone()),
        }
    }

    // All-auto telescopes (the curried `bind` shape, e.g. `(@A, @B) -> (M A, A -> M B) -> M B`, or a method wrapper's `(@A, use w) -> …`): when the head telescope has zero explicit slots but plain arguments were given, saturate it — marked queues first, fresh metavariables (and witness goals) for the rest — reduce the output, and re-target the plain arguments at the next telescope. This fires *only* with zero explicit slots, so application stays arity-strict everywhere else (this is deliberately not general partial application).
    let ft = loop {
        let ft = match &*head_type {
            Subterm::FuncType(ft) => ft.clone(),
            other => return Err(Error::not_a_function(other.clone())),
        };

        let all_auto = !ft.plicities.is_empty()
            && ft.plicities.iter().all(|p| !matches!(p, Plicity::Explicit));
        if !all_auto || plain.is_empty() {
            break ft;
        }

        let mut args = Vec::with_capacity(ft.plicities.len());
        let mut tele = ft.telescope.clone();
        for plicity in &ft.plicities {
            let Telescope::Cons(ty, rest) = tele else {
                unreachable!("plicities parallel the telescope");
            };
            let queue = match plicity {
                Plicity::Implicit => &mut marked,
                Plicity::Witness => &mut used,
                Plicity::Explicit => unreachable!("all-auto telescope"),
            };
            let arg = match queue.pop_front() {
                Some(arg) => check(context, &arg, ty.clone())?,
                None => insert_auto_argument(
                    context,
                    *plicity,
                    &ty,
                    rest.first_hint(),
                    &func_label,
                    term,
                )?,
            };
            tele = rest.open(&[&arg]);
            args.push((*plicity, arg));
        }
        let Telescope::Done(output) = tele else {
            unreachable!("plicities parallel the telescope");
        };

        head = Term::apply_marked(head, args);
        head_type = reduce_with(context, &output)?;
    };

    // Arity is checked per queue: plain arguments must exactly cover the explicit slots; `@`- and `use`-arguments may undershoot their slots (the remainder is inserted/resolved) but never overshoot them.
    let explicit_slots = ft
        .plicities
        .iter()
        .filter(|p| matches!(p, Plicity::Explicit))
        .count();
    let implicit_slots = ft
        .plicities
        .iter()
        .filter(|p| matches!(p, Plicity::Implicit))
        .count();
    let witness_slots = ft
        .plicities
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

    let checking = matches!(mode, Mode::Check(_));

    // Whether the expected type is fully ground. The codomain postponement is only a win when `expect(output, expected)` actually *grounds* the result metavar; if `expected` itself carries an unsolved metavar, that turnaround is flex-flex and the metavar must instead be grounded by the continuation's body — so postponing it would strand the metavar (flex-flex-under-constructor) rather than refine it. When expected is not ground the argument checks eagerly.
    let expected_ground = match &mode {
        Mode::Check(expected) => expected
            .metavars()
            .iter()
            .all(|&id| transitively_ground(context, id)),
        Mode::Infer => false,
    };

    // The single walk. Every slot settles in telescope order, and the dependent substitution only ever receives elaborated terms or compiler-born metavariables — the invariant is the code path, not a guard. A written argument is checked at its domain, opened through the elaborated prefix; a checked-only intro form whose structure is still blocked (see `blocked_on_metavar`) becomes a parked checking problem whose placeholder stands in the telescope, retried by the wake machinery the moment a solution lands — which subsumes the retired clear loop, since a sibling's turnaround retries the parked check before any later slot opens through it. A missing hidden slot is inserted at that same true domain. Under suppressed parking the blocked case checks eagerly instead: re-validation re-elaborates rebuilt nodes whose types are already solved, so the branch is dead over the corpus (fact F1) and merely safe.
    let original = ft.telescope.clone();
    let mut elaborated: Vec<Term> = Vec::with_capacity(ft.plicities.len());
    // The pendings this apply minted: (slot, placeholder, written term), consulted by the fallback pin below.
    let mut pendings: Vec<(usize, MetaId, Term)> = Vec::new();
    let mut tele = original.clone();
    for plicity in &ft.plicities {
        let Telescope::Cons(ty, rest) = tele else {
            unreachable!("plicities parallel the telescope");
        };
        let written = match plicity {
            Plicity::Explicit => Some(plain.pop_front().expect("arity checked above")),
            Plicity::Implicit => marked.pop_front(),
            Plicity::Witness => used.pop_front(),
        };
        let arg = match written {
            Some(written) => {
                let blocked = checking
                    && !context.parking_suppressed()
                    && matches!(
                        &*written,
                        Subterm::Func(_)
                            | Subterm::Tuple(_)
                            | Subterm::Intrinsic(Intrinsic::List { .. })
                    )
                    && {
                        let result_metavars = result_metavars_from(context, &rest);
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
                    check(context, &written, ty.clone())?
                }
            }
            None => {
                insert_auto_argument(context, *plicity, &ty, rest.first_hint(), &func_label, term)?
            }
        };
        tele = rest.open(&[&arg]);
        elaborated.push(arg);
    }
    let Telescope::Done(output) = tele else {
        unreachable!("plicities parallel the telescope");
    };
    let output = *output;

    if let Mode::Check(expected) = &mode {
        // The output carries any pending's placeholder, which *blocks* rather than manufacturing the raw-substitution false mismatches the retired design had to bracket against — so this turnaround runs unbracketed, a mismatch propagates as genuine, and its pins wake parked checks through the ordinary retry machinery with every discharged obligation's solutions kept.
        expect(context, term, &output, expected)?;

        if !pendings.is_empty() {
            // The force tier — the retired settle, kept: a pending still undischarged after the turnaround above is checked now, under whatever it pinned. A lambda whose domain only its own body can ground is grounded by that body here, exactly as the settle once grounded it; the placeholder takes the checked term, so no pending outlives its apply undischarged. The parked copy of the obligation reconciles against this solution when its retry fires. The retired design also ran a bracketed best-effort expect over the *raw* spellings before settling — measured across the corpus, that pin never discharged a pending the wake machinery and this tier did not, so it is gone rather than kept.
            for (slot, placeholder, written) in &pendings {
                if context.metavar_solution(*placeholder).is_none() {
                    let slot_ty = original
                        .clone()
                        .nth(*slot, |k| elaborated[k].clone())
                        .expect("pending slot is within the telescope");
                    let checked = check(context, written, slot_ty)?;
                    context.solve_metavar(*placeholder, checked.clone());
                    elaborated[*slot] = checked;
                }
            }

            // The authoritative turnaround, through fully settled arguments.
            expect(context, term, &output, expected)?;
        }
    }

    // The rebuilt application is fully saturated; each argument's mark is its binder's plicity (inserted metavariables recorded like any other argument), so re-elaborating the rebuilt node is stable: both queues then match their slots exactly and nothing is minted twice.
    Ok((
        Term::apply_marked(head, ft.plicities.iter().copied().zip(elaborated)),
        output,
    ))
}

/// The metavariables the result `expect` can pin, as seen from one slot: the suffix telescope's terminal with this and every later binder opened as a fresh variable. Only prefix-born metavariables can occur in a slot's own domain — domains open over the prefix — so fresh-var opening of the unvisited suffix is decision-equivalent to a full-argument pre-read of the output, computed only for postponement candidates instead of once per application.
fn result_metavars_from(
    context: &mut Context,
    rest: &Scope<One, Telescope<Term>>,
) -> BTreeSet<MetaId> {
    let mut tele = rest
        .clone()
        .open(&[&Term::free_var(&context.fresh(rest.first_hint()))]);
    loop {
        match tele {
            Telescope::Done(body) => return body.metavars(),
            Telescope::Cons(_, next) => {
                tele = next
                    .clone()
                    .open(&[&Term::free_var(&context.fresh(next.first_hint()))]);
            }
        }
    }
}
