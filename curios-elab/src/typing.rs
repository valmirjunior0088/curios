#[cfg(test)]
mod tests;

use super::{Context, Error, Mode, Outcome, ParkedWork, Sort, elaborate};
use curios_core::{
    Apply, Bound, Field, Free, Func, FuncType, Global, Intrinsic, IntrinsicHead, Level, Many,
    MetaId, Metavar, MetavarOrigin, Proj, ReduceError, Scope, Subterm, Telescope, Term, Transient,
    UniverseConstraintKind, UniverseConstraintOrigin, UniverseRole, Visit,
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
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
///
/// Normalization is the whole denoising story only while the operand type is concrete. Under a `use Add(A)` parameter the projection is stuck on an abstract witness and no amount of reduction reaches the operator, so the structural fold the goal reports use runs afterwards over the live local scope — the same three witness forms, the same infix spelling.
fn resolved_for_display(context: &mut Context, term: &Term) -> Term {
    let Ok(zonked) = super::zonk(context, term) else {
        return term.clone();
    };
    let resolved = super::normalize(context, zonked.clone()).unwrap_or(zonked);
    let operators = super::operator_table(context);
    let binders = Rc::new(
        context
            .locals()
            .iter()
            .map(|(name, type_)| (name.clone(), type_.clone()))
            .collect(),
    );
    super::denoise_for_display(&operators, &binders, &resolved)
}

/// A `type_mismatch` error naming both sides in their best-effort display form (see [`resolved_for_display`]) — unless `term`, the node the conversion was about, is the `/syn/Monad/bind` application a postfix `!` desugars to and the region it hoisted to has nothing to sequence in.
///
/// `bind` produces `M(B)`, so the generic report names a type the author never wrote, mentions neither the `!` nor the region that rejected it, and leaks the still-unsolved `B`. The specialized report names the monad the region would have to be and the type it actually has.
fn display_mismatch(context: &mut Context, term: &Term, this: &Term, that: &Term) -> Error {
    let this = resolved_for_display(context, this);
    let that = resolved_for_display(context, that);

    if is_sequencing(context, term) && wraps_a_result(&this) && !wraps_a_result(&that) {
        return Error::stranded_sequencing(required_region_type(context, &this), that);
    }

    Error::type_mismatch(this, that)
}

/// Whether `term` is a `/syn/Monad/bind` application — what every `!` lowers to, and the only shape whose result type is an `M(B)` a stranded sequencing can clash on.
fn is_sequencing(context: &Context, term: &Term) -> bool {
    let Subterm::Apply(Apply { head, .. }) = &**term else {
        return false;
    };

    head.head_name() == Some(&Free::global(context.syntax().monad.bind.qualifier()))
}

/// Whether `type_` stands over a result: a former applied to at least one argument, which is the shape `M(B)` always has.
///
/// Read on the expected side, this is what separates a stranded `!` from an ordinary mismatch inside a real region — `Io Str` against `Io Nat` disagrees about the result, while `Io _` against `Nat` is a region that cannot sequence at all. `Io` is named among the intrinsics because it is the one monad whose former is an intrinsic node rather than an application or a parameterized nominal type.
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
    let mut visit = Visit::rewriting(
        |_, _| None,
        Box::new(move |_, term: &Term| {
            matches!(&**term, Subterm::Metavar(_)).then(|| hole.clone())
        }),
    );

    Some(sequenced.traverse(&mut visit))
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
        Outcome::Mismatch => Err(display_mismatch(context, term, inferred, expected)),
        // Undecided: blocked on unsolved metavariables. Park the goals to be retried when a watched metavariable is solved and succeed provisionally — unless conversion is currently a yes/no oracle, in which case undecided must stay a mismatch.
        Outcome::Blocked(goals) => {
            if context.parking_suppressed() {
                return Err(display_mismatch(context, term, inferred, expected));
            }

            for goal in goals {
                context.park(ParkedWork::Conversion(goal), term.clone());
            }
            context.retry_parked()
        }
    }
}

/// Whether `arg` is a checked-only introduction form (tuple, lambda, list literal) that cannot be elaborated yet because the type structure it needs is an unsolved metavar — a tuple or list literal whose whole expected type, or a lambda whose expected *domain*, reduces to one. (A lambda only needs its domain known: the body, which may project the parameter, can't be checked against an unknown domain; its codomain may stay a metavar. A list literal borrows its element type from `expected`, so it needs the expected head — `List _` — to be known.) Synthesizable forms return `false`: they have a turnaround of their own and must run eagerly so their solutions feed the result unification.
pub(crate) fn blocked_on_metavar(
    context: &mut Context,
    arg: &Term,
    ty: &Term,
    result_metavars: &BTreeSet<MetaId>,
    expected_ground: bool,
) -> Result<bool, Error> {
    let is_lambda = matches!(&**arg, Subterm::Func(_));
    let is_list = matches!(&**arg, Subterm::Intrinsic(Intrinsic::List { .. }));
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
                // A lambda whose expected *domain* is an unsolved metavar: its body may need the domain's structure (to project the parameter), so postpone it until a sibling argument (e.g. `p : Parse(A)`) pins the domain.
                let domain_blocked = match &*reduce_with(context, domain)? {
                    Subterm::Metavar(Metavar { id, .. }) => context.metavar_solution(*id).is_none(),
                    _ => false,
                };
                // ...or a lambda whose *codomain* still carries an unsolved metavar that the result type will pin: postpone until `expect(output, expected)` solves it, so the body is checked against the refined codomain. This is the `let !`-continuation case — `(x) => …` checked against `?dom => Parse(?B)`, where `?dom` is already pinned by the bind's action but `?B` (the bind's own result type) is solved only by the turnaround. Gating on `result_metavars` keeps it to metavars `expect` will address; gating on `expected_ground` ensures that turnaround actually grounds `?B` (vs. a flex-flex alias that the eager body must ground instead).
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

/// Whether metavar `id` is solved *all the way down*: solved, and every metavar in its solution is itself transitively ground. `metavar_solution` only sees one level, and a solution can still embed unsolved metavars, so `expected_ground` needs this transitive view to be sure the turnaround will actually pin a result metavar rather than alias it flex-flex. Terminates: the occurs check forbids cyclic solutions.
pub(crate) fn transitively_ground(context: &Context, id: MetaId) -> bool {
    match context.metavar_solution(id) {
        None => false,
        Some(solution) => solution
            .metavars()
            .iter()
            .all(|&inner| transitively_ground(context, inner)),
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
                    let super::ParkedGoal {
                        work,
                        origin: parked_origin,
                        frame,
                        watching,
                    } = parked;
                    return Err(match work {
                        ParkedWork::Conversion(goal) => {
                            // A conversion stuck between two witness holes reads as a bare metavariable mismatch; name the unresolved witness it actually is instead.
                            match self
                                .witness_hole(&goal.this)
                                .or_else(|| self.witness_hole(&goal.that))
                            {
                                Some((origin, witness_goal)) => {
                                    let embedding = super::diagnose_embedding(self, &witness_goal);
                                    Error::no_witness(
                                        resolved_for_display(self, &witness_goal),
                                        origin.func,
                                        origin.binder,
                                        embedding,
                                    )
                                    .at_opt(parked_origin.span())
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
                                    .at_opt(parked_origin.span())
                                }
                            }
                        }
                        ParkedWork::Checking { term, expected, .. } => match &*term {
                            // A parked bang is a region that never named its monad; report it as that, not as a generic stalled check.
                            Subterm::Transient(Transient::Bang(_)) => {
                                Error::bang_region_undetermined().at_opt(parked_origin.span())
                            }
                            _ => Error::postponed_check(resolved_for_display(self, &expected))
                                .at_opt(parked_origin.span()),
                        },
                        ParkedWork::Witness {
                            goal, provenance, ..
                        } => {
                            let embedding = super::diagnose_embedding(self, &goal);
                            Error::no_witness(
                                resolved_for_display(self, &goal),
                                provenance.func,
                                provenance.binder,
                                embedding,
                            )
                            .at_opt(parked_origin.span())
                        }
                    });
                }
                return Ok(());
            }
        }
    }
}

/// Render a surviving conversion goal's still-unsolved watched metavariables. Insertion provenance rides the *occurrence*, not the birth entry, so both sides are scanned for the watched ids; a watched metavariable no occurrence names (solved away from the spelling, or watched through a spine) still renders as its bare id.
fn watched_blockers(
    context: &Context,
    watching: &BTreeSet<MetaId>,
    this: &Term,
    that: &Term,
) -> Vec<String> {
    let origins: Rc<RefCell<BTreeMap<MetaId, MetavarOrigin>>> =
        Rc::new(RefCell::new(BTreeMap::new()));
    for side in [this, that] {
        let sink = Rc::clone(&origins);
        let mut visit = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, term: &Term| {
                if let Subterm::Metavar(Metavar {
                    id,
                    origin: Some(origin),
                    ..
                }) = &**term
                {
                    sink.borrow_mut()
                        .entry(*id)
                        .or_insert_with(|| origin.clone());
                }
                None
            }),
        );
        let _: Term = side.traverse(&mut visit);
    }

    let origins = origins.borrow();
    watching
        .iter()
        .filter(|id| context.metavar_solution(**id).is_none())
        .map(|id| match origins.get(id) {
            Some(MetavarOrigin::Implicit(origin)) => {
                format!(
                    "the implicit argument '{}' of '{}'",
                    origin.binder, origin.func
                )
            }
            _ => format!("?{}", id.0),
        })
        .collect()
}

fn retry_one(context: &mut Context, parked: super::ParkedGoal) -> Result<(), Error> {
    let super::ParkedGoal {
        work,
        origin,
        frame,
        ..
    } = parked;

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
        } => return super::retry_witness(context, slot, goal, provenance, origin, frame),
    };

    enum Retry {
        Converts,
        Mismatch(Error),
        Blocked(Vec<super::Goal>),
    }

    let outcome = context.with_frame(|context| {
        context.restore_frame(&frame);

        Ok(
            match super::convert_outcome(context, &goal.type_, &goal.this, &goal.that)? {
                Outcome::Converts => Retry::Converts,
                // Built here, inside the restored frame, rather than at the report below: `display_mismatch` reads the sides through whatever solutions have landed, so they name the actual disagreement rather than the metavariables it arrived wrapped in (see `resolved_for_display`). A stranded `!` reaches its report through this arm — the region's own type only settles after the sequencing has parked — so the origin is what decides which message it gets.
                Outcome::Mismatch => {
                    Retry::Mismatch(display_mismatch(context, &origin, &goal.this, &goal.that))
                }
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
        Retry::Mismatch(error) => Err(error.at_opt(origin.span())),
        Retry::Blocked(goals) => {
            for goal in goals {
                context.repark(ParkedWork::Conversion(goal), origin.clone(), frame.clone());
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
    // Discharged already — by the force tier at its own apply (the same obligation's check, run in the richer live context), or by a unification commitment downstream checking will judge. Re-running the check here would re-elaborate a term whose lowering-minted holes are already birthed, and the rebuilt occurrence would drop their spines.
    if context.metavar_solution(placeholder).is_some() {
        return Ok(());
    }

    let rebuilt = context.with_frame(|context| {
        context.restore_frame(&frame);

        // Re-park while the term is still blocked on its expected type's structure — the same predicate that parked it, with the result-directed refinements off: at retry there is no output turnaround left to wait for, so a codomain-only block checks eagerly, matching the retired settle's behavior.
        if blocked_on_metavar(context, &term, &expected, &BTreeSet::new(), false)? {
            return Ok(None);
        }

        elaborate(context, &term, Mode::Check(expected.clone())).map(|(rebuilt, _)| Some(rebuilt))
    })?;

    match rebuilt {
        Some(rebuilt) => match context.metavar_solution(placeholder).cloned() {
            None => {
                context.solve_metavar(placeholder, rebuilt);
                Ok(())
            }
            // Unification reached the placeholder first. Reconcile rather than overwrite: `Solutions::solve` replaces silently, and goals already discharged against the earlier solution would be standing on a value that just changed under them. The checked term must convert with what the tree committed to, at the type both inhabit; a disagreement is an honest mismatch at the argument, and an undecidable comparison parks like any conversion.
            Some(existing) => {
                let outcome = super::convert_outcome(context, &expected, &rebuilt, &existing)
                    .map_err(|error| {
                        Error::from_reduce(error, || {
                            Error::convert_exhausted(rebuilt.clone(), existing.clone())
                        })
                    })?;
                match outcome {
                    Outcome::Converts => Ok(()),
                    Outcome::Mismatch => {
                        Err(display_mismatch(context, &origin, &rebuilt, &existing)
                            .at_opt(origin.span()))
                    }
                    Outcome::Blocked(goals) => {
                        for goal in goals {
                            context.park(ParkedWork::Conversion(goal), origin.clone());
                        }
                        Ok(())
                    }
                }
            }
        },
        None => {
            context.repark(
                ParkedWork::Checking {
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
///
/// All three rest on one premise — the arm is reached only when the scrutinee *equals* the case's value — and every scrutinee has it, because a term of non-`Io` type denotes one value.
///
/// That is a typing fact now, not an analysis. This used to be guarded by a walk over the scrutinee and everything it reaches, asking whether its spelling fixes a value: `Cell/get(c)` denotes differently before and after a `Cell/set`, and registering an equation for it read one term as `true` in one arm and `false` in a nested one — an equation between two `Bool` literals, and from there a closed inhabitant of `False`. The walk could never close the case it documented, either: `f(b)` for a *binder* `f` had to be assumed effectful, because the function space admitted `Cell/get` and no property of `(Bool) -> Bool` distinguished an effectful inhabitant from a pure one.
///
/// Retyping every host operation to return `Io` made both questions vacuous. `Cell/get(c)` has type `Io(T)`, which is opaque and has no cases, so it cannot be a scrutinee at all; and no inhabitant of `(Bool) -> Bool` performs an effect, so `f(true)` fixes a value for every possible caller binding. The refinement the walk had to withhold from a pure opaque head is therefore restored — this is a change that *admits* more, not merely one that deletes an analysis.
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
            // Registered on the *cheap* key: the scrutinee as written, with metas and universes normalized. Canonicalizing here is what used to make a guard cost its operand's evaluation before any use of the fact — see `shallow_scrutinee`. The reducer's probe escalates on a miss, so a spelling this does not collapse is still found, and found by reducing at the site that needs it rather than at every site that records one.
            let canonical = super::shallow_scrutinee(context, head);

            // A concept-dispatched scrutinee (`a <= hi`) elaborates to the method projected out of the witness — `(?w).1(a, hi)` — which is not the shape the reducer probes: by then it has become the intrinsic normal form `NatLe(a, hi)`. Registering only the verbatim key leaves the arm unrefined, silently, while the equivalent `Nat/le(a, hi)` spelling refines. Register the probed form alongside it so both spellings agree.
            let resolved = match canonical.head_key().is_none() {
                true => spine_whnf(context, head)?
                    .map(|spined| super::shallow_scrutinee(context, &spined))
                    .filter(|resolved| resolved.head_key().is_some() && *resolved != canonical),
                false => None,
            };

            // The operands disagree the same way and are deliberately *not* re-registered here. Their spellings differ by a fold and by an unfolded local as well as by a wrapper, so bringing them together is reduction — and reduction at registration is what would make a guard cost its subject's evaluation before any use of the fact, which is the whole reason `shallow_scrutinee` records the written form. `reduce::canonical_key` does it at the probe instead, under a ceiling and once per key.
            if let Some(resolved) = resolved {
                context.refine_scrutinee(resolved, value.clone());
            }

            context.refine_scrutinee(canonical, value.clone());
        }
    }

    Ok(())
}

/// A scrutinee's applied spine taken to weak-head normal form one application layer at a time, or `None` when it is not an application spine or nothing moved.
///
/// Bounded rather than reduced: it opens layers and stops, so it never forces an argument. That is what makes it usable at *registration*, where reducing would cost the guard its subject's evaluation.
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
    /// An intrinsic carrier (`Bool`, `Nat`, `List`, `Bin`): no indices, so the motive binds only the scrutinee, at the carrier's own type.
    Intrinsic(&'a Term),
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
            MotiveShape::Intrinsic(head_type) => (*head_type).clone(),
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

/// Accept `head_type` (already reduced) when it is `expected`'s type-former, else the matching `not_*_type` error. The shared core of `expect_intrinsic_head` and `elaborate`'s `elaborate_intrinsic_head` — one source of truth for the `IntrinsicHead` → type-former / error mapping.
pub(crate) fn check_intrinsic_head(
    expected: IntrinsicHead,
    head_type: Term,
) -> Result<Term, Error> {
    let matches = match expected {
        IntrinsicHead::Nat => matches!(&*head_type, Subterm::Intrinsic(Intrinsic::NatType)),
        IntrinsicHead::Bool => matches!(&*head_type, Subterm::Intrinsic(Intrinsic::BoolType)),
        IntrinsicHead::Bin(grain) => {
            matches!(&*head_type, Subterm::Intrinsic(Intrinsic::BinType(actual)) if *actual == grain)
        }
    };

    match matches {
        true => Ok(head_type),
        false => Err(match expected {
            IntrinsicHead::Nat => Error::not_nat_type(head_type),
            IntrinsicHead::Bool => Error::not_bool_type(head_type),
            IntrinsicHead::Bin(grain) => Error::not_bin_type(grain, head_type),
        }),
    }
}

/// Infer the scrutinee's type, reduce it, and require it to be the given intrinsic type. Returns the reduced head type — used by `erase` to erase the head.
pub(crate) fn expect_intrinsic_head(
    context: &mut Context,
    head: &Term,
    expected: IntrinsicHead,
) -> Result<Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    check_intrinsic_head(expected, head_type)
}
