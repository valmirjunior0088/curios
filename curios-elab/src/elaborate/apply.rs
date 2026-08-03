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
                        Subterm::Func(_) | Subterm::Tuple(_) | Subterm::Prim(Prim::Lst(..))
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
            // PHASE3-RAWBETA fallback, temporary and measured: a pending the wake machinery could not discharge may wait on a metavariable only the raw spelling can pin — beta-reducing the unelaborated body through the result type is the documented cycle-breaker a placeholder cannot replicate. The pin runs under *suppressed parking* inside a solution bracket: suppressed, it can neither park new work nor retry parked work, so nothing is consumed inside a bracket that may roll back — a retry fired mid-bracket would elaborate a pending whose solutions the rollback then unwinds while its lowering-minted holes stay birthed, and the force tier's second elaboration would drop their spines. Deleted (or promoted to a documented rule) once the corpus measurement decides.
            let undischarged: Vec<(usize, Term)> = pendings
                .iter()
                .filter(|(_, id, _)| context.metavar_solution(*id).is_none())
                .map(|(slot, _, raw)| (*slot, raw.clone()))
                .collect();
            if !undischarged.is_empty() {
                let mut raw_opened = elaborated.clone();
                for (slot, raw) in &undischarged {
                    raw_opened[*slot] = raw.clone();
                }
                if let Telescope::Done(raw_output) = original.clone().open_params(&raw_opened) {
                    let mark = context.solution_mark();
                    let pinned = context.with_suppressed_parking(|context| {
                        expect(context, term, &raw_output, expected)
                    });
                    if pinned.is_err() {
                        context.rollback_solutions(mark);
                    }
                    context.retry_parked()?;

                    // TEMP PHASE3 measurement: log every firing and how many pendings it discharged.
                    let discharged = undischarged
                        .iter()
                        .filter(|(slot, _)| {
                            pendings.iter().any(|(s, id, _)| {
                                s == slot && context.metavar_solution(*id).is_some()
                            })
                        })
                        .count();
                    {
                        use std::io::Write;
                        if let Ok(mut f) = std::fs::OpenOptions::new()
                            .create(true)
                            .append(true)
                            .open("/tmp/claude-1000/-var-home-valmirpretto-Workspace-curios/5dd7d59b-fd3f-419b-813e-b6157632180a/scratchpad/rawbeta.log")
                        {
                            let _ = writeln!(
                                f,
                                "fired undischarged={} discharged={}",
                                undischarged.len(),
                                discharged
                            );
                        }
                    }
                }
            }

            // The force tier — the retired settle, kept: a pending still undischarged after every pin is checked now, under whatever the turnarounds landed. A lambda whose domain only its own body can ground is grounded by that body here, exactly as the settle once grounded it; the placeholder takes the checked term, so no pending outlives its apply undischarged — which is also what keeps the fallback bracket above safe, since no foreign obligation can be consumed inside it. The parked copy of the obligation reconciles against this solution when its retry fires.
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

// === Iterative fast path for ground, all-explicit applications ==============
//
// `elaborate` (the driver in `elaborate.rs`) defunctionalizes the recursive cycle `elaborate → elaborate_apply → check(rest) → elaborate` onto an explicit frame stack — the reducer's `PendingMatch` move, one level up — so a right-nested constructor spine (a string literal's `Utf8` derivation, one `more(c, st, t, rest)` link per byte) elaborates at native depth bounded by the written program's binder nesting rather than by the spine's length. This module holds the frame and the walk logic; the driver holds the loop.
//
// The fast path is a hand-specialized copy of `elaborate_apply` restricted to the class that provably neither inserts an implicit nor postpones an intro form: `term` is ground and every argument is explicit, and (confirmed after the head elaborates) the head telescope is all-explicit at the call's arity. Under those gates the all-auto saturation loop, argument insertion, and the result-directed postponement machinery are all inert, so the walk reduces to "check each argument against its dependent domain, opening the rest over the checked form" — and only the potentially-deep argument's `check` is turned into a descent. Every other application (and every position the gate declines) falls through to the unchanged native `elaborate_apply`, so the output stays byte-for-byte identical to the recursive elaborator.

/// The cheap, O(1) gate the driver applies before any head elaboration: `term` is ground (no metavariable, no elaborator-minted free name — the bits `Term` caches per node) and every call-site argument is explicit. Passing it makes the term a *candidate*; `start_fast_apply` confirms the head telescope conforms before committing to the specialized walk.
pub(super) fn fast_apply_eligible(term: &Term, apply: &Apply) -> bool {
    !term.has_metavar()
        && !term.has_local_free()
        && apply
            .plicities
            .iter()
            .all(|p| matches!(p, Plicity::Explicit))
}

/// One suspended fast-path application on the driver's frame stack — the iterative analogue of `reduce`'s `PendingMatch`. Captures the telescope walk of one application whose current argument (`params[index]`) is being elaborated by descent; a finished child resolves against this frame instead of returning through a native frame. Every field is owned and `Clone`, none borrows the context across the child's elaboration.
pub(super) struct ElabFrame {
    /// The already-elaborated head, spelled into the rebuilt application.
    head: Term,
    /// The head telescope's plicities (all `Explicit` under the gate) — the marks for the rebuilt application, paired with `elaborated`.
    plicities: Vec<Plicity>,
    /// The original lowered arguments, in call order (all explicit, so no insertion reorders them; `full_args` in `elaborate_apply` is exactly this vector for the fast-path class).
    params: Vec<Term>,
    /// The remaining head telescope, already opened over the checked prefix. Bounded by the head's declared arity, never by data length.
    tele: Telescope<Term>,
    /// The checked arguments accumulated so far, in slot order.
    elaborated: Vec<Term>,
    /// The slot currently being elaborated (index into `params`).
    index: usize,
    /// The application's own mode; drives the final `Check`-mode `expect`.
    mode: Mode,
    /// The application node — its span anchors restamping and error location, and rebuilds the cache key at record-at-pop.
    term: Term,
    /// `Some(stamp)` when this node was a cacheable probe miss: record the finalized (un-restamped) result under `stamp` at pop. `None` when the node is uncacheable (a non-ground expected type).
    record: Option<ElaborationStamp>,
}

/// Outcome of advancing a fast-path walk one step ([`ElabFrame::advance`]/[`ElabFrame::resume`]).
pub(super) enum Walk {
    /// The walk reached a fast-path-eligible argument: the driver pushes `frame` and descends into `arg` in `Check(ty)` mode. Boxed — an `ElabFrame` dwarfs the other variant.
    Suspend {
        frame: Box<ElabFrame>,
        arg: Term,
        ty: Term,
    },
    /// The walk finished: `result` is the un-restamped `(rebuilt, type)` for `term`. Record-at-pop has already fired for a cacheable frame.
    Done { term: Term, result: (Term, Term) },
}

impl ElabFrame {
    /// Begin a fast-path walk for a gated application. Elaborates the head (native, shallow), confirms the head telescope is all-explicit at the call's arity — delegating to the unchanged native `elaborate_apply` when it is not — then starts the walk. `record` is the probe outcome: `Some(stamp)` records the result at finalize, `None` leaves it uncached. Returns a [`Walk`]: `Suspend` means the first argument needs descent, `Done` means the walk completed without one (e.g. a nullary constructor).
    pub(super) fn start(
        context: &mut Context,
        apply: &Apply,
        term: &Term,
        mode: Mode,
        record: Option<ElaborationStamp>,
    ) -> Result<Walk, Error> {
        let (head, head_type) = elaborate(context, &apply.head, Mode::Infer)?;
        let head_type = reduce_with(context, &head_type)?;
        let ft = match &*head_type {
            Subterm::FuncType(ft) => ft.clone(),
            other => return Err(Error::not_a_function(other.clone())),
        };

        // Confirm the specialized class: the head telescope is all-explicit and its arity matches the call. A head type that still carries an implicit slot, or a genuine arity error, is the general elaborator's business — hand the whole node to native `elaborate_apply` (which re-elaborates the head through the cache: cheap and idempotent) and still record the result for a cacheable miss, exactly as the recursive form would.
        let conforms = ft.plicities.len() == apply.params.len()
            && ft.plicities.iter().all(|p| matches!(p, Plicity::Explicit));
        if !conforms {
            let expected = mode.expected();
            let result = elaborate_apply(context, apply, term, mode)?;
            if let Some(stamp) = record {
                context.record_elaborated(term, expected.as_ref(), stamp, &result);
            }
            return Ok(Walk::Done {
                term: term.clone(),
                result,
            });
        }

        ElabFrame {
            head,
            plicities: ft.plicities.clone(),
            elaborated: Vec::with_capacity(apply.params.len()),
            params: apply.params.clone(),
            tele: ft.telescope,
            index: 0,
            mode,
            term: term.clone(),
            record,
        }
        .advance(context)
    }

    /// Advance this walk to its next descent or its finish. Checks each shallow argument natively (bounded by that argument's written depth), suspending the moment it reaches a fast-path-eligible argument, and finalizes — running the `Check`-mode `expect` and recording a cacheable result — when the telescope is exhausted. Mirrors `elaborate_apply`'s main telescope walk with the deep argument's `check` factored out into `Walk::Suspend`.
    fn advance(mut self, context: &mut Context) -> Result<Walk, Error> {
        loop {
            // The telescope is the head's declared arity (four for `more`), never the spine length, so cloning it per step is bounded work.
            match self.tele.clone() {
                Telescope::Done(body) => {
                    let output = *body;
                    if let Mode::Check(expected) = &self.mode {
                        expect(context, &self.term, &output, expected)?;
                    }
                    let rebuilt = Term::apply_marked(
                        self.head.clone(),
                        self.plicities
                            .iter()
                            .copied()
                            .zip(self.elaborated.iter().cloned()),
                    );
                    let result = (rebuilt, output);
                    if let Some(stamp) = self.record {
                        let expected = self.mode.expected();
                        context.record_elaborated(&self.term, expected.as_ref(), stamp, &result);
                    }
                    return Ok(Walk::Done {
                        term: self.term,
                        result,
                    });
                }
                Telescope::Cons(ty, rest) => {
                    let arg = self.params[self.index].clone();
                    if let Subterm::Apply(inner) = &*arg
                        && fast_apply_eligible(&arg, inner)
                    {
                        // Suspend at this slot; `self.tele`/`self.index` still point at it, so `resume` re-opens `rest` over the finished child.
                        return Ok(Walk::Suspend {
                            frame: Box::new(self),
                            arg,
                            ty,
                        });
                    }
                    let checked = check(context, &arg, ty.clone())?;
                    self.tele = rest.open(&[&checked]);
                    self.elaborated.push(checked);
                    self.index += 1;
                }
            }
        }
    }

    /// Resume this suspended frame with the finished (already span-stamped) argument: slot it in, open the telescope over it, and advance to the next descent or finish. The frame was suspended at a `Cons`, so the re-destructure is total.
    pub(super) fn resume(mut self, context: &mut Context, child: Term) -> Result<Walk, Error> {
        let Telescope::Cons(_, rest) = self.tele.clone() else {
            unreachable!("a suspended frame is always positioned at a telescope Cons");
        };
        self.tele = rest.open(&[&child]);
        self.elaborated.push(child);
        self.index += 1;
        self.advance(context)
    }

    /// The span this frame's node contributes to an error draining through it.
    pub(super) fn span(&self) -> Option<Span> {
        self.term.span()
    }
}
