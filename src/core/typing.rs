use super::{
    Context, Error, Field, Many, Mode, Prim, PrimHead, Proj, Scope, Subterm, Term, Var, elaborate,
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

pub fn convert_with(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    super::convert(context, &Term::type_(), this, that)
        .map_err(|error| error.into_error(|| Error::convert_preempted(this.clone(), that.clone())))
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
/// the metavariables it arrived wrapped in. An unsolved metavariable makes
/// `zonk` fail, in which case the raw spelling is kept.
fn resolved_for_display(context: &Context, term: &Term) -> Term {
    super::zonk(context, term).unwrap_or_else(|_| term.clone())
}

/// A `type_mismatch` error naming both sides in their best-effort display
/// form (see [`resolved_for_display`]).
fn display_mismatch(context: &Context, this: &Term, that: &Term) -> Error {
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
        super::Outcome::Converts => retry_parked(context),
        super::Outcome::Mismatch => Err(display_mismatch(context, inferred, expected)),
        // Undecided: blocked on unsolved metavariables. Park the goals to be
        // retried when a watched metavariable is solved (§8) and succeed
        // provisionally — unless conversion is currently a yes/no oracle, in
        // which case undecided must stay a mismatch.
        super::Outcome::Blocked(goals) => {
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
                super::Outcome::Converts => Retry::Converts,
                // Report through whatever solutions have landed: the reduced
                // sides name the actual disagreement, not the metavariables it
                // arrived wrapped in.
                super::Outcome::Mismatch => Retry::Mismatch(
                    super::reduce(context, goal.this.clone())?,
                    super::reduce(context, goal.that.clone())?,
                ),
                super::Outcome::Blocked(goals) => Retry::Blocked(goals),
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
    placeholder: usize,
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
                });
            }
            return Ok(());
        }
    }
}

/// Register a counterfactual match-arm refinement of the scrutinee: a `Var`
/// scrutinee reduces to the arm's value inside the arm (`refine`), and a
/// projection scrutinee (e.g. a `Bln` or `Nat` match on a tuple field) refines that
/// projection (`refine_projection`). The frame containing the refinement is
/// scoped to the arm, so the (possibly counterfactual) assumption does not
/// leak. Any other scrutinee shape records nothing — there is no stable key
/// to refine, and the arm then types against the unrefined head.
pub fn refine_head(context: &mut Context, head: &Term, value: &Term) {
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
        _ => {}
    }
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
            &motive.open(&[&Term::var(Var::free(&label))]),
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
