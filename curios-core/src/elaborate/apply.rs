use super::*;

pub(super) fn elaborate_func_type(
    context: &mut Context,
    ft: &FuncType,
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<Term>,
        plicities: &[Plicity],
        domains: &mut Vec<(String, Term)>,
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(output) => check(context, &output, Term::type_()),
            Telescope::Cons(ty, rest) => {
                let domain = check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::free_var(&name);
                // Assume the *rebuilt* domain: insertion saturates applications
                // during elaboration, and a lowered (under-applied) type leaking
                // into later reduction would open a telescope at the wrong arity.
                // A `use` binder additionally joins the witness scope: the rest
                // of the type may itself need resolution through it.
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

    Ok((rebuilt, Term::type_()))
}

/// Fill an omitted non-explicit slot: an implicit binder gets a fresh
/// metavariable; a witness binder gets a fresh metavariable *plus* a
/// resolution goal, attempted eagerly (solved now, parked on a flex key, or
/// deferred on a missing table entry). `origin` is the application node — the
/// span anchor for the goal.
pub(super) fn insert_auto_argument(
    context: &mut Context,
    plicity: Plicity,
    type_: &Term,
    label: Option<&str>,
    func: &str,
    origin: &Term,
) -> Result<Term, Error> {
    let binder = binder_name(label.unwrap_or("_"));

    match plicity {
        Plicity::Implicit => Ok(context.fresh_metavar(
            type_.clone(),
            origin.span(),
            ImplicitOrigin {
                func: func.to_string(),
                binder,
            },
        )),
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

/// A binder's user-facing name. The head's function type is the *rebuilt* one,
/// whose binders were re-closed under `fresh`-minted labels (`T#1`); reports
/// should name the binder as written, and `#` cannot occur in an identifier.
pub(super) fn binder_name(label: &str) -> String {
    match label.split_once('#') {
        Some(("", _)) => "_".to_string(),
        Some((name, _)) => name.to_string(),
        None => label.to_string(),
    }
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

    // Insertion provenance: name the applied function in the uninferred-
    // implicit report. Heads are references in practice; anything else gets a
    // placeholder (the span still locates the call).
    let func_label = match &**head {
        Subterm::Var(var) => var.unwrap().to_string(),
        _ => "<function>".to_string(),
    };

    let (mut head, head_type) = elaborate(context, head, Mode::Infer)?;
    let mut head_type = reduce_with(context, &head_type)?;

    // The three call-site queues: plain arguments fill explicit binders in
    // telescope order, `@`-arguments fill implicit binders, `use`-arguments
    // fill witness binders — each matched independently, so the relative
    // position of a marked argument among the plain ones carries no meaning.
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

    // All-auto telescopes (the curried `bind` shape, e.g.
    // `(@A, @B) -> (M A, A -> M B) -> M B`, or a method wrapper's
    // `(@A, use w) -> …`): when the head telescope has zero explicit slots but
    // plain arguments were given, saturate it — marked queues first, fresh
    // metavariables (and witness goals) for the rest — reduce the output, and
    // re-target the plain arguments at the next telescope. This fires *only*
    // with zero explicit slots, so application stays arity-strict everywhere
    // else (this is deliberately not general partial application).
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
                    rest.first_label(),
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

    // Arity is checked per queue: plain arguments must exactly cover the
    // explicit slots; `@`- and `use`-arguments may undershoot their slots (the
    // remainder is inserted/resolved) but never overshoot them.
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

    // Materialize the saturated argument vector, threading the dependent
    // substitution so each inserted metavariable is born at its binder's
    // *instantiated* type. The walk below re-checks the inserted metavariables
    // idempotently (`elaborate_metavar` re-checks the recorded type).
    let mut full_args = Vec::with_capacity(ft.plicities.len());
    {
        let mut tele = ft.telescope.clone();
        for plicity in &ft.plicities {
            let Telescope::Cons(ty, rest) = tele else {
                unreachable!("plicities parallel the telescope");
            };
            let arg = match plicity {
                Plicity::Explicit => plain.pop_front().expect("arity checked above"),
                Plicity::Implicit => match marked.pop_front() {
                    Some(arg) => arg,
                    None => insert_auto_argument(
                        context,
                        *plicity,
                        &ty,
                        rest.first_label(),
                        &func_label,
                        term,
                    )?,
                },
                Plicity::Witness => match used.pop_front() {
                    Some(arg) => arg,
                    None => insert_auto_argument(
                        context,
                        *plicity,
                        &ty,
                        rest.first_label(),
                        &func_label,
                        term,
                    )?,
                },
            };
            tele = rest.open(&[&arg]);
            full_args.push(arg);
        }
    }
    let params = &full_args;

    // Result-directed argument order (§6). An introduction form (tuple,
    // lambda) is checked-only: it can't be elaborated against a parameter type that
    // reduces to a bare, unsolved metavar — there is no structure to drive it. In
    // `Check` mode we postpone exactly those arguments, unify the application's
    // result type against `expected` (which pins the metavars — both those a sibling
    // argument would witness and phantom ones the expected type alone carries), then
    // re-check the postponed arguments against their now-refined types. Synthesizable
    // arguments (`Var`/`Apply`/`Proj`/literals) are never postponed: they run first
    // and feed that very unification, so this only reorders the checked-only forms
    // and is otherwise byte-for-byte the previous left-to-right walk. If the result
    // unification fails to pin a postponed argument's type, the re-check fails with
    // the same error as before — no new acceptance, graceful degradation.
    let checking = matches!(mode, Mode::Check(_));

    // The metavars the result type carries — exactly the ones `expect(output, expected)`
    // can pin. A continuation lambda whose codomain still mentions one of these is
    // postponed (see `blocked_on_metavar`) so its body is checked only after that
    // unification refines the codomain. Opening over the raw args is pure substitution
    // (no birth/solve), so this is just an early read of the result type.
    let arg_refs = params.iter().collect::<Vec<&Term>>();
    let result_metavars = ft.telescope.clone().open(&arg_refs).metavars();

    // Whether the expected type is fully ground. The codomain postponement is only a
    // win when `expect(output, expected)` actually *grounds* the result metavar; if
    // `expected` itself carries an unsolved metavar, that turnaround is flex-flex and
    // the metavar must instead be grounded by the continuation's body — so postponing
    // it would strand the metavar (flex-flex-under-constructor) rather than refine it.
    // When expected is not ground we fall back to the eager (current) behavior.
    let expected_ground = match &mode {
        Mode::Check(expected) => expected
            .metavars()
            .iter()
            .all(|&id| transitively_ground(context, id)),
        Mode::Infer => false,
    };

    // The telescope is opened with the *rebuilt* argument at every eager
    // slot, so later entry types and the output carry rebuilt spellings only
    // — a lowered copy spliced into the output would smuggle a birthed hole's
    // bare node past its rebuild (and a lowered term toward the reducer).
    // A postponed intro form stays lowered for now; its holes are unbirthed,
    // and its rebuilt form lands after the output `expect` pins its metas.

    // Walk the telescope, checking each argument against its (dependent) domain
    // and opening the rest with the elaborated form. A checked-only intro form
    // blocked on a metavar is postponed — its slot keeps the raw term for now —
    // but the moment a *later* synthesizable argument grounds the metavar it was
    // waiting on (e.g. `subst`'s `p : Eq(x, y)` grounds the motive's domain), it
    // is re-checked and the remaining telescope re-opened through its elaborated
    // form. Otherwise a sibling whose type mentions it (`subst`'s `v : P x`) or
    // the result (`P y`) would reduce through a raw term whose un-inserted
    // implicits (like `Eq`'s `@A`) panic the reducer. Arguments still genuinely
    // blocked at the end (a continuation awaiting a codomain metavar) are settled
    // after the result `expect`, as before.
    let original = ft.telescope.clone();
    let mut elaborated: Vec<Term> = Vec::with_capacity(params.len());
    let mut postponed: Vec<usize> = Vec::new();
    let mut tele = original.clone();
    let mut index = 0;
    let output = loop {
        let (ty, rest) = match tele {
            Telescope::Done(body) => break *body,
            Telescope::Cons(ty, rest) => (ty, rest),
        };
        let term = if checking
            && blocked_on_metavar(
                context,
                &params[index],
                &ty,
                &result_metavars,
                expected_ground,
            )? {
            postponed.push(index);
            params[index].clone()
        } else {
            check(context, &params[index], ty.clone())?
        };
        elaborated.push(term);

        // Re-check any postponed argument whose block this slot just cleared.
        let mut resolved = false;
        let mut cursor = 0;
        while cursor < postponed.len() {
            let slot = postponed[cursor];
            let slot_ty = original
                .clone()
                .nth(slot, |k| elaborated[k].clone())
                .expect("postponed slot is within the telescope");
            if blocked_on_metavar(
                context,
                &params[slot],
                &slot_ty,
                &result_metavars,
                expected_ground,
            )? {
                cursor += 1;
            } else {
                elaborated[slot] = check(context, &params[slot], slot_ty)?;
                postponed.remove(cursor);
                resolved = true;
            }
        }

        // Re-open from the top through the (possibly updated) prefix so later
        // slot types carry the elaborated forms; otherwise just advance.
        tele = match resolved {
            false => rest.open(&[&elaborated[index]]),
            true => original.clone().open_params(&elaborated),
        };
        index += 1;
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &output, expected)?;
        for &slot in &postponed {
            let slot_ty = original
                .clone()
                .nth(slot, |k| elaborated[k].clone())
                .expect("postponed slot is within the telescope");
            elaborated[slot] = check(context, &params[slot], slot_ty)?;
        }
    }

    // The rebuilt application is fully saturated; each argument's mark is its
    // binder's plicity (inserted metavariables recorded like any other
    // argument), so re-elaborating the rebuilt node is stable: both queues
    // then match their slots exactly and nothing is minted twice.
    Ok((
        Term::apply_marked(head, ft.plicities.iter().copied().zip(elaborated)),
        output,
    ))
}

/// Whether `arg` is a checked-only introduction form (tuple, lambda, list
/// literal) that cannot be elaborated yet because the type structure it needs is
/// an unsolved metavar — a tuple or list literal whose whole expected type, or a
/// lambda whose expected *domain*, reduces to one. (A lambda only needs its domain
/// known: the body, which may project the parameter, can't be checked against an
/// unknown domain; its codomain may stay a metavar. A list literal borrows its
/// element type from `expected`, so it needs the expected head — `Lst _` — to be
/// known.) Synthesizable forms return `false`: they have a turnaround of their own
/// and must run eagerly so their solutions feed the result unification.
pub(super) fn blocked_on_metavar(
    context: &mut Context,
    arg: &Term,
    ty: &Term,
    result_metavars: &BTreeSet<MetavarId>,
    expected_ground: bool,
) -> Result<bool, Error> {
    let is_lambda = matches!(&**arg, Subterm::Func(_));
    let is_list = matches!(&**arg, Subterm::Prim(Prim::Lst(_)));
    let is_tuple = matches!(&**arg, Subterm::Tuple(_));
    if !is_lambda && !is_list && !is_tuple {
        return Ok(false);
    }
    let reduced = reduce_with(context, ty)?;
    Ok(match &*reduced {
        // A tuple/list/lambda whose whole expected type is an unsolved metavar.
        Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
        Subterm::FuncType(FuncType { telescope, .. }) if is_lambda => match telescope {
            Telescope::Cons(domain, _) => {
                // A lambda whose expected *domain* is an unsolved metavar: its body may
                // need the domain's structure (to project the parameter), so postpone it
                // until a sibling argument (e.g. `p : Parse(A)`) pins the domain.
                let domain_blocked = match &*reduce_with(context, domain)? {
                    Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
                    _ => false,
                };
                // ...or a lambda whose *codomain* still carries an unsolved metavar that
                // the result type will pin: postpone until `expect(output, expected)`
                // solves it, so the body is checked against the refined codomain. This is
                // the `let !`-continuation case — `(x) => …` checked against
                // `?dom => Parse(?B)`, where `?dom` is already pinned by the bind's action
                // but `?B` (the bind's own result type) is solved only by the turnaround.
                // Gating on `result_metavars` keeps it to metavars `expect` will address;
                // gating on `expected_ground` ensures that turnaround actually grounds
                // `?B` (vs. a flex-flex alias that the eager body must ground instead).
                domain_blocked
                    || (expected_ground
                        && reduced.metavars().iter().any(|id| {
                            result_metavars.contains(id) && context.metavar_solution(*id).is_none()
                        }))
            }
            Telescope::Done(_) => false,
        },
        _ => false,
    })
}

/// Whether metavar `id` is solved *all the way down*: solved, and every metavar in its
/// solution is itself transitively ground. `metavar_solution` only sees one level, and
/// a solution can still embed unsolved metavars, so `expected_ground` needs this
/// transitive view to be sure the turnaround will actually pin a result metavar rather
/// than alias it flex-flex. Terminates: the occurs check forbids cyclic solutions.
pub(super) fn transitively_ground(context: &Context, id: MetavarId) -> bool {
    match context.metavar_solution(id) {
        None => false,
        Some(solution) => solution
            .metavars()
            .iter()
            .all(|&inner| transitively_ground(context, inner)),
    }
}
