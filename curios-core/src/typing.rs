use super::{
    Context, Error, Field, Many, MetavarId, Mode, Outcome, Prim, PrimHead, Proj, Scope, Subterm,
    Term, elaborate,
};

/// Synthesis is just `elaborate` in `Infer` mode, projecting out the type. Kept
/// as a thin shim so the many existing call sites (this module, `erase*.rs`,
/// tests) read unchanged while erase is migrated to downstream lowering (§6).
pub fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Infer).map(|(_, type_)| type_)
}

/// Checking counterpart to `infer`: `elaborate` in `Check` mode, returning the
/// *elaborated* term. Drives `term` against a known `ty` — the rebuilt,
/// de-Bruijn-correct subterm whose lambda domains are solved and whose binders
/// are re-closed (§9). Elaboration is authoritative: this output, not the
/// original lowered term, is what flows on to `zonk`/`erase`.
pub fn check(context: &mut Context, term: &Term, ty: Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Check(ty)).map(|(term, _)| term)
}

pub fn reduce_with(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce(context, term.clone())
        .map_err(|error| error.into_error(|| Error::reduce_preempted(term.clone())))
}

/// [`super::convert`] with its `ReduceError` mapped to `Error`, at an explicit
/// type so proof-irrelevance and eta fire at the terms' real sort. The inverter
/// uses it to compare a binder's competing forcings at the binder's own type.
pub fn convert_at(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    super::convert(context, type_, this, that)
        .map_err(|error| error.into_error(|| Error::convert_preempted(this.clone(), that.clone())))
}

/// `convert_at` at `Type`: comparing two types, the elaboration turnaround's
/// common case.
pub fn convert_with(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    convert_at(context, &Term::type_(), this, that)
}

/// The sort term (`Type`/`Prop`) `type_` inhabits — what a type-former reports
/// as its type-of-a-type, so a proposition checks against `Prop`. Wraps
/// [`super::Sort::of`], mapping preemption to an error like the helpers above.
pub fn sort_term(context: &mut Context, type_: &Term) -> Result<Term, Error> {
    super::Sort::of(context, type_)
        .map(super::Sort::term)
        .map_err(|error| error.into_error(|| Error::reduce_preempted(type_.clone())))
}

/// Whether `type_` is a strict proposition (its sort is `Prop`). Wraps
/// [`super::Sort::of`], mapping preemption like the helpers above.
pub fn is_prop(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    super::Sort::of(context, type_)
        .map(|sort| matches!(sort, super::Sort::Prop))
        .map_err(|error| error.into_error(|| Error::reduce_preempted(type_.clone())))
}

/// Best-effort display form for a mismatch report: substitute the solutions
/// that have landed, so the message names the actual disagreement rather than
/// the metavariables it lstived wrapped in, then deep-[`normalize`](super::normalize)
/// the result so a stuck concept-method projection standing in an index
/// position collapses to the value it denotes (`Vec(Nat, (sys/witness#0).0(0, 1))`
/// → `Vec(Nat, 1)`) rather than surfacing compiler-internal witness machinery.
/// An unsolved metavariable makes `zonk` fail, in which case the raw spelling
/// is kept; a normalization that preempts falls back to the merely-zonked form.
fn resolved_for_display(context: &mut Context, term: &Term) -> Term {
    let Ok(zonked) = super::zonk(context, term) else {
        return term.clone();
    };
    super::normalize(context, zonked.clone()).unwrap_or(zonked)
}

/// A `type_mismatch` error naming both sides in their best-effort display
/// form (see [`resolved_for_display`]).
fn display_mismatch(context: &mut Context, this: &Term, that: &Term) -> Error {
    Error::type_mismatch(
        resolved_for_display(context, this),
        resolved_for_display(context, that),
    )
}

pub fn expect(
    context: &mut Context,
    term: &Term,
    inferred: &Term,
    expected: &Term,
) -> Result<(), Error> {
    // Cumulativity: `Prop ⊑ Type`. A proposition is a type, so a term whose type
    // is `Prop` is accepted where a `Type` is expected — the sole universe
    // subsumption (the reverse does not hold). The direct case at the checking
    // boundary; everything else goes through ordinary conversion below.
    if matches!(&*reduce_with(context, expected)?, Subterm::Type)
        && matches!(&*reduce_with(context, inferred)?, Subterm::Prop)
    {
        return retry_parked(context);
    }

    let outcome =
        super::convert_outcome(context, &Term::type_(), inferred, expected).map_err(|error| {
            error.into_error(|| Error::convert_preempted(inferred.clone(), expected.clone()))
        })?;

    match outcome {
        Outcome::Converts => retry_parked(context),
        Outcome::Mismatch => Err(display_mismatch(context, inferred, expected)),
        // Undecided: blocked on unsolved metavariables. Park the goals to be
        // retried when a watched metavariable is solved (§8) and succeed
        // provisionally — unless conversion is currently a yes/no oracle, in
        // which case undecided must stay a mismatch.
        Outcome::Blocked(goals) => {
            if context.parking_suppressed() {
                return Err(display_mismatch(context, inferred, expected));
            }

            for goal in goals {
                context.park(super::ParkedWork::Conversion(goal), term.clone());
            }
            retry_parked(context)
        }
    }
}

/// Retry parked constraints woken by freshly landed solutions, to fixpoint
/// (§8). A woken goal re-runs under its frozen frame: converts and is dropped,
/// mismatches and errors at its origin, or re-parks still blocked. Each round
/// consumes wake signals and ids solve exactly once, so this terminates.
pub fn retry_parked(context: &mut Context) -> Result<(), Error> {
    // Never retry inside an oracle: re-validation swallows errors
    // (`Err(_) => false`), so a woken goal's mismatch would vanish along with
    // the goal itself — a silently dropped obligation. The wake signals stay
    // queued; the next unsuppressed turnaround retries them.
    if context.parking_suppressed() {
        return Ok(());
    }

    loop {
        let woken = context.wake_parked();
        if woken.is_empty() {
            return Ok(());
        }

        for parked in woken {
            retry_one(context, parked)?;
        }
    }
}

fn retry_one(context: &mut Context, parked: super::ParkedGoal) -> Result<(), Error> {
    let super::ParkedGoal {
        work,
        origin,
        frame,
        ..
    } = parked;

    let goal = match work {
        super::ParkedWork::Conversion(goal) => goal,
        super::ParkedWork::Checking {
            term,
            expected,
            placeholder,
        } => return retry_checking(context, term, expected, placeholder, origin, frame),
        super::ParkedWork::Witness {
            slot,
            goal,
            provenance,
        } => return super::retry_witness(context, slot, goal, provenance, origin, frame),
    };

    enum Retry {
        Converts,
        Mismatch(Term, Term),
        Blocked(Vec<super::Goal>),
    }

    let outcome = context.with_frame(|context| {
        context.restore_frame(&frame);

        Ok(
            match super::convert_outcome(context, &goal.type_, &goal.this, &goal.that)? {
                Outcome::Converts => Retry::Converts,
                // Report through whatever solutions have landed: the normalized
                // sides name the actual disagreement, not the metavariables it
                // lstived wrapped in — and, deep-normalized, no stuck operator
                // witness machinery in an index (see `resolved_for_display`).
                Outcome::Mismatch => Retry::Mismatch(
                    super::normalize(context, goal.this.clone())?,
                    super::normalize(context, goal.that.clone())?,
                ),
                Outcome::Blocked(goals) => Retry::Blocked(goals),
            },
        )
    });

    let outcome = outcome.map_err(|error: super::ReduceError| {
        error.into_error(|| Error::convert_preempted(goal.this.clone(), goal.that.clone()))
    })?;

    match outcome {
        Retry::Converts => Ok(()),
        Retry::Mismatch(this, that) => Err(Error::type_mismatch(this, that)),
        Retry::Blocked(goals) => {
            for goal in goals {
                context.repark(
                    super::ParkedWork::Conversion(goal),
                    origin.clone(),
                    frame.clone(),
                );
            }
            Ok(())
        }
    }
}

/// Retry a parked *checking problem*: under the frozen frame, the expected
/// type either still reduces to a bare metavariable (re-park) or has gained
/// structure — run the check for real and solve the placeholder with the
/// rebuilt term. Errors propagate carrying the term's own spans.
fn retry_checking(
    context: &mut Context,
    term: Term,
    expected: Term,
    placeholder: MetavarId,
    origin: Term,
    frame: super::FrozenFrame,
) -> Result<(), Error> {
    let rebuilt = context.with_frame(|context| {
        context.restore_frame(&frame);

        if matches!(&*reduce_with(context, &expected)?, Subterm::Metavar(_)) {
            return Ok(None);
        }

        elaborate(context, &term, Mode::Check(expected.clone())).map(|(rebuilt, _)| Some(rebuilt))
    })?;

    match rebuilt {
        Some(rebuilt) => {
            context.solve_metavar(placeholder, rebuilt);
            Ok(())
        }
        None => {
            context.repark(
                super::ParkedWork::Checking {
                    term,
                    expected,
                    placeholder,
                },
                origin,
                frame,
            );
            Ok(())
        }
    }
}

/// Drain the parked store: retry everything to a fixpoint, then report any
/// survivor as a mismatch at its origin. Run after each top-level item and
/// after the entrypoint body, so an unresolvable constraint is attributed to
/// the definition that produced it and frozen frames do not pile up.
pub fn drain_parked(context: &mut Context) -> Result<(), Error> {
    loop {
        retry_parked(context)?;

        let remaining = context.take_parked();
        if remaining.is_empty() {
            return Ok(());
        }

        // Final sweep: attempt everything once more — a goal parked after the
        // last solution landed has never been retried.
        let before = remaining.len();
        for parked in remaining {
            retry_one(context, parked)?;
        }

        // No progress in a full sweep: the rest can never resolve. Report the
        // first at its origin.
        if context.parked_len() >= before && !context.has_newly_solved() {
            if let Some(parked) = context.take_parked().into_iter().next() {
                return Err(match parked.work {
                    super::ParkedWork::Conversion(goal) => {
                        display_mismatch(context, &goal.this, &goal.that)
                    }
                    super::ParkedWork::Checking { .. } => Error::CannotInfer,
                    super::ParkedWork::Witness {
                        goal, provenance, ..
                    } => Error::no_witness(
                        resolved_for_display(context, &goal),
                        provenance.func,
                        provenance.binder,
                    )
                    .at_opt(parked.origin.span()),
                });
            }
            return Ok(());
        }
    }
}

/// Register a counterfactual match-arm refinement of a scrutinee (or a learned
/// scrutinee index), so a context hypothesis whose type mentions it reduces at
/// the arm's value. The frame holding the refinement is scoped to the arm, so
/// the (counterfactual) assumption does not leak. Three keyings, by head shape:
///
/// - a `Var` reduces to the value (`refine`);
/// - a projection — a `Bln`/`Nat` match on a tuple field — refines that
///   projection (`refine_projection`);
/// - any other head — a stuck application like `classify(c)` / `Nat/in_range(...)`
///   — is canonicalized (head verbatim, arguments in WHNF) and recorded in the
///   term-keyed scrutinee store (`refine_scrutinee`), so an occurrence spelled
///   with differently-reduced arguments still matches.
///
/// Also drives Rung-B index *learning* (`refine_head(actual, target)`), where
/// `actual` is a scrutinee index: a `Var` index is the live case, and a
/// *constructor* index records an entry the reducer never fires (it has no
/// applied-head symbol to probe) — the inverter pins the arm binders the other
/// way. A stuck-*application* index would be the genuinely cyclic case, but no
/// inductive in the library is indexed by one.
pub fn refine_head(context: &mut Context, head: &Term, value: &Term) -> Result<(), Error> {
    match &**head {
        Subterm::Var(var) => {
            context.refine(var.unwrap(), value);
        }
        Subterm::Proj(Proj {
            head,
            field: Field::Index(index),
        }) => {
            context.refine_projection(head.clone(), *index, value.clone());
        }
        _ => {
            let canonical = super::canonical_scrutinee(context, head)
                .map_err(|error| error.into_error(|| Error::reduce_preempted(head.clone())))?;
            context.refine_scrutinee(canonical, value.clone());
        }
    }

    Ok(())
}

/// Check that `motive` is a well-formed type family over a scrutinee of `head_type`,
/// returning the rebuilt motive. Shared by every match form whose motive binds
/// only the scrutinee (arity 1) — an annotated inductive-match motive goes through
/// `check_inductive_motive` instead. Runs the motive through `elaborate` in
/// `Check(Type)` mode — erase is downstream lowering now and no longer
/// type-checks (§6) — and re-closes the elaborated body so the motive carries
/// its solved/re-closed form (§9).
pub fn check_motive(
    context: &mut Context,
    head_type: &Term,
    motive: &Scope<Many>,
) -> Result<Scope<Many>, Error> {
    let label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&label, head_type);
        let body = elaborate(
            context,
            &motive.open(&[&Term::free_var(&label)]),
            Mode::Check(Term::type_()),
        )?
        .0;
        Ok(Scope::close(Many(1), &[label.as_str()], body))
    })
}

/// Accept `head_type` (already reduced) when it is `expected`'s type-former,
/// else the matching `not_*_type` error. The shared core of `expect_prim_head`
/// and `elaborate`'s `elaborate_prim_head` — one source of truth for the
/// `PrimHead` → type-former / error mapping.
pub fn check_prim_head(expected: PrimHead, head_type: Term) -> Result<Term, Error> {
    let matches = match expected {
        PrimHead::Nat => matches!(&*head_type, Subterm::Prim(Prim::NatType)),
        PrimHead::Bln => matches!(&*head_type, Subterm::Prim(Prim::BlnType)),
        PrimHead::Bin => matches!(&*head_type, Subterm::Prim(Prim::BinType)),
    };

    match matches {
        true => Ok(head_type),
        false => Err(match expected {
            PrimHead::Nat => Error::not_nat_type(head_type),
            PrimHead::Bln => Error::not_bln_type(head_type),
            PrimHead::Bin => Error::not_bin_type(head_type),
        }),
    }
}

/// Infer the scrutinee's type, reduce it, and require it to be the given prim
/// type. Returns the reduced head type — used by `erase` to erase the head.
pub fn expect_prim_head(
    context: &mut Context,
    head: &Term,
    expected: PrimHead,
) -> Result<Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    check_prim_head(expected, head_type)
}
