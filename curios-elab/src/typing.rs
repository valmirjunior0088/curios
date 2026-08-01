#[cfg(test)]
mod tests;

use super::{Context, Error, Mode, Outcome, Sort, elaborate};
use curios_core::{
    Apply, Field, Free, Func, Global, Level, Many, MetaId, Prim, PrimHead, Proj, ReduceError,
    Scope, Subterm, Telescope, Term, UniverseConstraintKind, UniverseConstraintOrigin,
    UniverseRole,
};

/// Synthesis is just `elaborate` in `Infer` mode, projecting out the type. Kept as a thin shim so the many existing call sites (this module, `erase*.rs`, tests) read unchanged while erase is migrated to downstream lowering.
pub(crate) fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Infer).map(|(_, type_)| type_)
}

/// Checking counterpart to `infer`: `elaborate` in `Check` mode, returning the *elaborated* term. Drives `term` against a known `ty` — the rebuilt, de-Bruijn-correct subterm whose lambda domains are solved and whose binders are re-closed. Elaboration is authoritative: this output, not the original lowered term, is what flows on to `zonk`/`erase`.
pub(crate) fn check(context: &mut Context, term: &Term, ty: Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Check(ty)).map(|(term, _)| term)
}

pub(crate) fn reduce_with(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce_forced(context, term.clone())
        .map_err(|error| Error::from_reduce(error, || Error::reduce_exhausted(term.clone())))
}

/// `super::convert` with its `ReduceError` mapped to `Error`, at an explicit type so proof-irrelevance and eta fire at the terms' real sort. The inverter uses it to compare a binder's competing forcings at the binder's own type.
pub(crate) fn convert_at(
    context: &mut Context,
    type_: &Term,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    super::convert(context, type_, this, that).map_err(|error| {
        Error::from_reduce(error, || {
            Error::convert_exhausted(this.clone(), that.clone())
        })
    })
}

/// The sort term (`Type`/`Prop`) `type_` inhabits — what a type-former reports as its type-of-a-type, so a proposition checks against `Prop`. Wraps `Sort::of`, mapping an exhausted budget to an error like the helpers above.
pub(crate) fn sort_term(context: &mut Context, type_: &Term) -> Result<Term, Error> {
    Sort::of(context, type_)
        .map(Sort::term)
        .map_err(|error| Error::from_reduce(error, || Error::reduce_exhausted(type_.clone())))
}

/// Whether `type_` is a strict proposition (its sort is `Prop`). Wraps `Sort::of`, mapping an exhausted budget like the helpers above.
pub(crate) fn is_prop(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    is_prop_in(context, &mut Vec::new(), type_)
}

/// [`is_prop`] under the binders a surrounding walk has opened.
///
/// The binders are threaded rather than assumed into the [`Context`], because `Context::assume` bumps the mutation stamp that validates the memoization caches — see `convert::Opened`. A seeding walk that assumed instead would invalidate them at every binder it descended through.
pub(crate) fn is_prop_in(
    context: &mut Context,
    opened: &mut Vec<(Free, Term)>,
    type_: &Term,
) -> Result<bool, Error> {
    Sort::of_in(context, opened, type_)
        .map(|sort| matches!(sort, Sort::Prop))
        .map_err(|error| Error::from_reduce(error, || Error::reduce_exhausted(type_.clone())))
}

/// Elaborate `term` as a type/proposition without inventing an arbitrary expected universe upper bound.
pub(crate) fn check_is_sort(context: &mut Context, term: &Term) -> Result<(Term, Sort), Error> {
    let (rebuilt, inferred) = if matches!(&**term, Subterm::Metavar(_)) {
        let classifier = context.fresh_classifier_type("written type hole");
        elaborate(context, term, Mode::Check(classifier))?
    } else {
        elaborate(context, term, Mode::Infer)?
    };

    match &*reduce_with(context, &inferred)? {
        Subterm::Prop => Ok((rebuilt, Sort::Prop)),
        Subterm::Type(level) => Ok((rebuilt, Sort::Type(level.clone()))),
        other => Err(Error::type_mismatch(other.clone(), Term::type_ground())),
    }
}

/// Best-effort display form for a mismatch report: substitute the solutions that have landed, so the message names the actual disagreement rather than the metavariables it arrived wrapped in, then deep-[`normalize`](super::normalize) the result so a stuck concept-method projection standing in an index position collapses to the value it denotes (`Vec(Nat, (sys/witness@0).0(0, 1))` → `Vec(Nat, 1)`) rather than surfacing compiler-internal witness machinery. An unsolved metavariable makes `zonk` fail, in which case the raw spelling is kept; a normalization that exhausts its budget falls back to the merely-zonked form.
fn resolved_for_display(context: &mut Context, term: &Term) -> Term {
    let Ok(zonked) = super::zonk(context, term) else {
        return term.clone();
    };
    super::normalize(context, zonked.clone()).unwrap_or(zonked)
}

/// A `type_mismatch` error naming both sides in their best-effort display form (see [`resolved_for_display`]).
fn display_mismatch(context: &mut Context, this: &Term, that: &Term) -> Error {
    Error::type_mismatch(
        resolved_for_display(context, this),
        resolved_for_display(context, that),
    )
}

pub(crate) fn expect(
    context: &mut Context,
    term: &Term,
    inferred: &Term,
    expected: &Term,
) -> Result<(), Error> {
    let inferred_sort = reduce_with(context, inferred)?;
    let expected_sort = reduce_with(context, expected)?;
    if let Subterm::Type(upper) = &*expected_sort {
        let lower = match &*inferred_sort {
            Subterm::Prop => Some(Level::zero()),
            Subterm::Type(lower) => Some(lower.clone()),
            _ => None,
        };
        if let Some(lower) = lower {
            context
                .universes_mut()
                .add_leq(
                    lower,
                    upper.clone(),
                    UniverseConstraintOrigin {
                        span: term.span(),
                        kind: UniverseConstraintKind::Cumulativity,
                        declaration: None,
                        binder: None,
                    },
                )
                .map_err(Error::from)?;
            return context.retry_parked();
        }
    }

    let outcome = super::convert_outcome(context, &Term::type_ground(), inferred, expected)
        .map_err(|error| {
            Error::from_reduce(error, || {
                Error::convert_exhausted(inferred.clone(), expected.clone())
            })
        })?;

    match outcome {
        Outcome::Converts => context.retry_parked(),
        Outcome::Mismatch => Err(display_mismatch(context, inferred, expected)),
        // Undecided: blocked on unsolved metavariables. Park the goals to be retried when a watched metavariable is solved and succeed provisionally — unless conversion is currently a yes/no oracle, in which case undecided must stay a mismatch.
        Outcome::Blocked(goals) => {
            if context.parking_suppressed() {
                return Err(display_mismatch(context, inferred, expected));
            }

            for goal in goals {
                context.park(super::ParkedWork::Conversion(goal), term.clone());
            }
            context.retry_parked()
        }
    }
}

impl Context {
    /// Retry parked constraints woken by freshly landed solutions, to fixpoint. A woken goal re-runs under its frozen frame: converts and is dropped, mismatches and errors at its origin, or re-parks still blocked. Each round consumes wake signals and ids solve exactly once, so this terminates.
    pub(crate) fn retry_parked(&mut self) -> Result<(), Error> {
        // Never retry inside an oracle: re-validation swallows errors (`Err(_) => false`), so a woken goal's mismatch would vanish along with the goal itself — a silently dropped obligation. The wake signals stay queued; the next unsuppressed turnaround retries them.
        if self.parking_suppressed() {
            return Ok(());
        }

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

    /// Retry every currently parked problem once without requiring the store to become empty. Declaration finalization uses this to settle work that is already decidable before closing universe metas, while witness goals that genuinely depend on a later declaration remain parked.
    pub(crate) fn sweep_parked(&mut self) -> Result<(), Error> {
        let parked = self.take_parked();
        for goal in parked {
            retry_one(self, goal)?;
        }
        self.retry_parked()
    }

    /// Drain the parked store: retry everything to a fixpoint, then report any survivor as a mismatch at its origin. Run after each top-level item and after the entrypoint body, so an unresolvable constraint is attributed to the definition that produced it and frozen frames do not pile up.
    pub(crate) fn drain_parked(&mut self) -> Result<(), Error> {
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

            // No progress in a full sweep: the rest can never resolve. Report the first at its origin.
            if self.parked_len() >= before && !self.has_newly_solved() {
                if let Some(parked) = self.take_parked().into_iter().next() {
                    return Err(match parked.work {
                        super::ParkedWork::Conversion(goal) => {
                            // A conversion stuck between two witness holes reads as a bare metavariable mismatch; name the unresolved witness it actually is instead.
                            match self
                                .witness_hole(&goal.this)
                                .or_else(|| self.witness_hole(&goal.that))
                            {
                                Some((origin, witness_goal)) => Error::no_witness(
                                    resolved_for_display(self, &witness_goal),
                                    origin.func,
                                    origin.binder,
                                )
                                .at_opt(parked.origin.span()),
                                None => display_mismatch(self, &goal.this, &goal.that)
                                    .at_opt(parked.origin.span()),
                            }
                        }
                        super::ParkedWork::Checking { .. } => Error::CannotInfer,
                        super::ParkedWork::Witness {
                            goal, provenance, ..
                        } => Error::no_witness(
                            resolved_for_display(self, &goal),
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
                // Report through whatever solutions have landed: the normalized sides name the actual disagreement, not the metavariables it arrived wrapped in — and, deep-normalized, no stuck operator witness machinery in an index (see `resolved_for_display`). Best-effort, like every other display path: a term that cannot be normalized (an effectful primitive reached at the type level, a reduction out of budget) is reported as it stands. Propagating that failure would replace the mismatch the user needs to see with an artifact of rendering it.
                Outcome::Mismatch => Retry::Mismatch(
                    resolved_for_display(context, &goal.this),
                    resolved_for_display(context, &goal.that),
                ),
                Outcome::Blocked(goals) => Retry::Blocked(goals),
            },
        )
    });

    let outcome = outcome.map_err(|error: ReduceError| {
        Error::from_reduce(error, || {
            Error::convert_exhausted(goal.this.clone(), goal.that.clone())
        })
    })?;

    match outcome {
        Retry::Converts => Ok(()),
        // Locate the mismatch at the construct that parked it, as every sibling arm of the drain does. A parked goal outlives the frame it came from, so without this the only surviving clue is the module.
        Retry::Mismatch(this, that) => Err(Error::type_mismatch(this, that).at_opt(origin.span())),
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

/// Retry a parked *checking problem*: under the frozen frame, the expected type either still reduces to a bare metavariable (re-park) or has gained structure — run the check for real and solve the placeholder with the rebuilt term. Errors propagate carrying the term's own spans.
fn retry_checking(
    context: &mut Context,
    term: Term,
    expected: Term,
    placeholder: MetaId,
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

/// Register a counterfactual match-arm refinement of a scrutinee (or a learned scrutinee index), so a context hypothesis whose type mentions it reduces at the arm's value. The frame holding the refinement is scoped to the arm, so the (counterfactual) assumption does not leak. Three keyings, by head shape:
///
/// - a `Var` reduces to the value (`refine`);
/// - a projection — a `Bool`/`Nat` match on a tuple field — refines that projection (`refine_projection`);
/// - any other head — a stuck application like `classify(c)` / `Nat/in_range(...)` — is canonicalized (head verbatim, arguments in WHNF) and recorded in the term-keyed scrutinee store (`refine_scrutinee`), so an occurrence spelled with differently-reduced arguments still matches.
///
/// Also drives Rung-B index *learning* (`refine_head(actual, target)`), where `actual` is a scrutinee index: a `Var` index is the live case, and a *constructor* index records an entry the reducer never fires (it has no applied-head symbol to probe) — the inverter pins the arm binders the other way. A stuck-*application* index would be the genuinely cyclic case, but no inductive in the library is indexed by one.
pub(crate) fn refine_head(context: &mut Context, head: &Term, value: &Term) -> Result<(), Error> {
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
            let canonical = super::canonical_scrutinee(context, head).map_err(|error| {
                Error::from_reduce(error, || Error::reduce_exhausted(head.clone()))
            })?;

            // A concept-dispatched scrutinee (`a <= hi`) elaborates to the method projected out of the witness — `(?w).1(a, hi)` — which is not the shape the reducer probes: by then it has become the primitive normal form `NatLte(a, hi)`. Registering only the verbatim key leaves the arm unrefined, silently, while the equivalent `Nat/lte(a, hi)` spelling refines. Register the probed form alongside it so both spellings agree.
            if canonical.head_key().is_none()
                && let Some(spined) = spine_whnf(context, head)?
            {
                // Propagated, not swallowed: `canonical_scrutinee` is best-effort about arguments and returns only `Exhausted`, so discarding its error would discard a genuine exhaustion.
                let resolved = super::canonical_scrutinee(context, &spined).map_err(|error| {
                    Error::from_reduce(error, || Error::reduce_exhausted(head.clone()))
                })?;

                if resolved.head_key().is_some() && resolved != canonical {
                    context.refine_scrutinee(resolved, value.clone());
                }
            }

            context.refine_scrutinee(canonical, value.clone());
        }
    }

    Ok(())
}

/// Weak-head normal form of the *spine only*: reduce the function position and beta-open it over the arguments, repeating, but never reduce an argument and never evaluate the primitive node this lands on.
///
/// This is what turns a concept-dispatched comparison `(?w).1(a, hi)` into the `NatLte(a, hi)` the reducer actually probes, while leaving `a` and `hi` exactly as written. Reducing the whole application would do the same, but it forces the arguments — and an argument may legitimately contain an effect at the value level, which must keep being an error at the type level rather than being evaluated here.
///
/// `None` when the head does not resolve to a function, which is every non-dispatch scrutinee: those keep their verbatim key.
fn spine_whnf(context: &mut Context, term: &Term) -> Result<Option<Term>, Error> {
    let mut current = term.clone();

    // Bounded: each step consumes one application layer of an elaborated dispatch, and a runaway is a bug rather than something to spin on.
    for step in 0..16 {
        let Subterm::Apply(Apply { head, params, .. }) = &*current else {
            return Ok((step > 0).then_some(current));
        };

        let Subterm::Func(Func { telescope, .. }) = &*reduce_with(context, head)? else {
            return Ok((step > 0).then_some(current));
        };

        let args = params.iter().collect::<Vec<_>>();
        current = telescope.open(&args);
    }

    Ok(None)
}

/// What an eliminator's motive must abstract: the scrutinee's indices, then the scrutinee itself. Parameters are never abstracted — they are uniform across constructors and fixed by the scrutinee's type.
pub(crate) enum MotiveShape<'a> {
    /// A primitive carrier (`Bool`, `Nat`, `Lst`, `Bin`): no indices, so the motive binds only the scrutinee, at the carrier's own type.
    Prim(&'a Term),
    /// A nominal inductive: `indices` is the declaration's index telescope already instantiated at the scrutinee's actual parameters (`InductDecl::indices_at`), and the scrutinee binder takes `I(p̄, ī)` at those index binders.
    Induct {
        name: &'a Global,
        universes: &'a [Level],
        params: &'a [Term],
        indices: Telescope<()>,
    },
}

impl MotiveShape<'_> {
    /// The motive's binder count: one per index, then the scrutinee.
    pub(crate) fn arity(&self) -> usize {
        match self {
            MotiveShape::Prim(_) => 1,
            MotiveShape::Induct { indices, .. } => indices.len() + 1,
        }
    }

    /// The eliminated family's name, for diagnostics.
    fn name(&self) -> Option<&Global> {
        match self {
            MotiveShape::Prim(_) => None,
            MotiveShape::Induct { name, .. } => Some(name),
        }
    }

    /// Assume this shape's binders in `context` under `labels`, each index type opened with the preceding index binders and the scrutinee typed at those binders. Returns the scrutinee's assumed type for the caller's use.
    fn assume(&self, context: &mut Context, binders: &[Free]) -> Term {
        let (scrutinee_binder, index_binders) = binders
            .split_last()
            .expect("a motive binds at least the scrutinee");

        let scrutinee_type = match self {
            MotiveShape::Prim(head_type) => (*head_type).clone(),
            MotiveShape::Induct {
                name,
                universes,
                params,
                indices,
            } => {
                let mut telescope = indices.clone();
                let mut index_vars = Vec::with_capacity(index_binders.len());
                for binder in index_binders {
                    let var = Term::free_var(binder);
                    telescope = match telescope {
                        Telescope::Cons(ty, rest) => {
                            context.assume(binder, &ty);
                            rest.open(&[&var])
                        }
                        Telescope::Done(_) => {
                            unreachable!("index label count equals the index telescope's")
                        }
                    };
                    index_vars.push(var);
                }
                Term::induct_type_at(
                    (*name).clone(),
                    universes.to_vec(),
                    params.to_vec(),
                    index_vars,
                )
            }
        };

        context.assume(scrutinee_binder, &scrutinee_type);
        scrutinee_type
    }

    /// This shape's motive type, `(ī' : Ī(p̄)) -> I(p̄, ī') -> Type`, which a *written* motive is checked against. The codomain is `Type` rather than a sort variable because `Prop ⊑ Type` cumulativity already admits a proposition-valued motive at the checking boundary (`expect`).
    fn motive_type(&self, context: &mut Context) -> Term {
        let mut binders = Vec::with_capacity(self.arity());

        let scrutinee_type = match self {
            MotiveShape::Prim(head_type) => (*head_type).clone(),
            MotiveShape::Induct {
                name,
                universes,
                params,
                indices,
            } => {
                let mut telescope = indices.clone();
                let mut index_vars = Vec::new();
                while let Telescope::Cons(ty, rest) = telescope {
                    let label = context.fresh(rest.first_hint());
                    let var = Term::free_var(&label);
                    telescope = rest.open(&[&var]);
                    binders.push((label, ty));
                    index_vars.push(var);
                }
                Term::induct_type_at(
                    (*name).clone(),
                    universes.to_vec(),
                    params.to_vec(),
                    index_vars,
                )
            }
        };

        binders.push((context.fresh(None), scrutinee_type));
        let level = context.fresh_universe(
            UniverseRole::Flexible,
            Some(UniverseConstraintOrigin::new(
                UniverseConstraintKind::Other("motive result".into()),
            )),
        );
        Term::func_type(binders, Term::type_at(level))
    }
}

/// Check that `motive` is a well-formed type family for an eliminator of the given [`MotiveShape`], returning it closed at that shape's arity.
///
/// Two inputs reach here. A motive already closed at the eliminator's arity — synthesized, or a rebuilt match coming back through re-elaboration — is opened under its assumed binders and its body checked against `Type`, then re-closed so the motive carries its solved form. A *written* motive arrives from `into_core` un-scoped, in an arity-0 scope ([`Term::match_motive_written`]): it is an ordinary term, checked against the shape's motive type and then decomposed into the same canonical scope.
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
            shape.name().map(Global::symbol),
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
        let classifier = context.fresh_classifier_type("eliminator motive result");
        let body = check(context, &motive.open(&refs), classifier)?;

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
            shape.name().map(Global::symbol),
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

/// Accept `head_type` (already reduced) when it is `expected`'s type-former, else the matching `not_*_type` error. The shared core of `expect_prim_head` and `elaborate`'s `elaborate_prim_head` — one source of truth for the `PrimHead` → type-former / error mapping.
pub(crate) fn check_prim_head(expected: PrimHead, head_type: Term) -> Result<Term, Error> {
    let matches = match expected {
        PrimHead::Nat => matches!(&*head_type, Subterm::Prim(Prim::NatType)),
        PrimHead::Bool => matches!(&*head_type, Subterm::Prim(Prim::BoolType)),
        PrimHead::Bin(grain) => {
            matches!(&*head_type, Subterm::Prim(Prim::BinType(actual)) if *actual == grain)
        }
    };

    match matches {
        true => Ok(head_type),
        false => Err(match expected {
            PrimHead::Nat => Error::not_nat_type(head_type),
            PrimHead::Bool => Error::not_bool_type(head_type),
            PrimHead::Bin(_) => Error::not_bin_type(head_type),
        }),
    }
}

/// Infer the scrutinee's type, reduce it, and require it to be the given prim type. Returns the reduced head type — used by `erase` to erase the head.
pub(crate) fn expect_prim_head(
    context: &mut Context,
    head: &Term,
    expected: PrimHead,
) -> Result<Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    check_prim_head(expected, head_type)
}
