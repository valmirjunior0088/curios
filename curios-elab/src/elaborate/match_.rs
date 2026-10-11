use {
    super::{Context, Error, Misaligned, Mode, align, check, elaborate, expect, stands_at},
    crate::{
        BinderSite, FrozenFrame, MotiveShape, ParkedMatch, ParkedWork, check_motive,
        contradicted_guard, decided_sort_of, hold_prop, is_prop, probe_open_sort, quality_of,
        reduce_with, refine_head, resolved_for_display, same_uncommitted, stuck_on_metavar,
        unreachable_arm,
    },
    curios_analysis::{Invert, retyped, scrutinee_solution, solve_indices, wholly_pinned},
    curios_core::{
        Advance, Arity, Atom, Carrier, Cases, Free, Global, InductArm, InductDecl, InductType,
        Intrinsic, IntrinsicHead, Many, Match, MatchResult, MetavarOrigin, Nat, Quality, Scope,
        Subterm, Telescope, Term, Three, Two,
    },
    curios_num::{Binary, Grain, Natural},
    curios_utilities::Plicity,
};

/// A match's scrutinee, elaborated once by [`elaborate_match`] before any arm is: rebuilt, beside its type reduced — what every eliminator reads its carrier off.
struct Scrutinee {
    term: Term,
    type_: Term,
}

impl Scrutinee {
    /// The rebuilt scrutinee and its type, required to be the given intrinsic type; any other is refused as `term`'s eliminator refuses it.
    fn of_intrinsic(
        self,
        context: &mut Context,
        term: &Term,
        expected: IntrinsicHead,
    ) -> Result<(Term, Term), Error> {
        let Self { term: head, type_ } = self;
        let carrier = match expected {
            IntrinsicHead::Nat => matches!(&*type_, Subterm::Intrinsic(Intrinsic::NatType)),
            IntrinsicHead::Bool => matches!(&*type_, Subterm::Intrinsic(Intrinsic::BoolType)),
            IntrinsicHead::Bin(grain) => {
                matches!(&*type_, Subterm::Intrinsic(Intrinsic::BinType(actual)) if *actual == grain)
            }
        };

        match carrier {
            true => Ok((head, type_)),
            false => Err(refused_scrutinee(
                term,
                resolved_for_display(context, &type_),
            )),
        }
    }
}

/// When a match is elaborated in checking mode, solve its motive against the expected type *before* the arms are checked. An omitted motive is a constant scope wrapping a fresh metavar (`text::into_core::match_compile`'s `motive_scope`), so `motive.open` is that bare metavar and this pins it to `expected` up front — checking-only arms (tuples, constructors) then see a concrete target instead of an unsolved hole, and a result mentioning an enclosing type variable is taken straight from `expected` rather than inverted out of an arm. For an explicit motive it is the same consistency check that the `Check` turnaround would otherwise run post-hoc on the match's type (`elaborate_subterm`), only earlier.
fn seed_motive(
    context: &mut Context,
    term: &Term,
    motive: &Scope<Many>,
    head: &Term,
    mode: &Mode,
) -> Result<(), Error> {
    if let Mode::Check(expected) = mode {
        expect(context, term, &motive.open(&[head]), expected)?;
    }

    Ok(())
}

/// Resolve the (arity-one) motive of an intrinsic eliminator that keeps a family: a fold whose arm reads its hypothesis, or any intrinsic elimination whose motive is written or inferred — an elided one checked against an expected type is otherwise ambient (`resolve_intrinsic_result`). An elided motive checked against an expected type and matched on a *bare variable* scrutinee is synthesised dependent — abstracting that variable out of the expected type — so each arm checks against the goal specialised at its constructor (`0` / `pred + 1`, `x[]` / `head :: tail`, `false` / `true`, ...) rather than the unspecialised expected a constant motive would leave.
///
/// This complements `solve`'s occurrence abstraction (`convert.rs`), which already derives the dependent motive for a *compound* scrutinee: there the scrutinee is a clean abstraction subject in the motive metavar's spine, whereas a bare variable coincides with its own context binder — a duplicated, non-invertible spine entry that `solve` must leave alone. So anything but an elided-checking-mode-bare-variable match keeps the metavar path verbatim, letting `solve` (or the constant motive) do its job.
fn resolve_intrinsic_motive(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    mode: &Mode,
) -> Result<Scope<Many>, Error> {
    let shape = MotiveShape::Intrinsic(head_type);

    if let (Mode::Check(expected), Subterm::Var(var)) = (mode, &**head)
        && is_elided_motive(motive)
        && let Some(binder) = var.as_free()
    {
        let synthesized = Scope::close(Many(1), &[binder], expected.clone());
        return check_motive(context, &shape, &synthesized);
    }

    check_motive(context, &shape, motive)
}

/// The result of a fold, by the rule every reader of one states — erasure's split-or-fold reading and the kernel's arm rule too: the induction hypothesis is assumed exactly when the arm reads it, and only a family types it, at the motive opened at the tail. An arm that may read it (`may_read_hypothesis`) keeps the family, seeded against the expected type. An arm that cannot is a case split, resolved as a `Bool` elimination is: ambient where the motive is elided and checked, since nothing needs the type a hypothesis would have had.
///
/// The rebuilt motive is what everything below opens: insertion saturates applications during elaboration, and a lowered (under-applied) motive body reaching the reducer would open a telescope at the wrong arity.
fn resolve_fold_result(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    (term, mode): (&Term, &Mode),
    reads_hypothesis: bool,
) -> Result<MatchResult, Error> {
    if !reads_hypothesis {
        let result = resolve_intrinsic_result(context, head_type, head, motive, mode)?;
        if let Some(motive) = result.family() {
            seed_motive(context, term, motive, head, mode)?;
        }
        return Ok(result);
    }

    let motive = resolve_intrinsic_motive(context, head_type, head, motive, mode)?;
    seed_motive(context, term, &motive, head, mode)?;

    Ok(MatchResult::Family(motive))
}

/// Whether a fold's written cons arm may read its induction hypothesis, the binder at `index`: it names the binder, or it holds a written goal, which stands for a term not yet written and may name anything in scope. Nothing else reaches the hypothesis without naming it — a metavariable is solved from types, and no type in scope mentions the hypothesis unless the arm does — so an arm that does neither elaborates to one that reads none, which is the occurrence the kernel and erasure read off the elaborated arm.
fn may_read_hypothesis<N: Arity>(arm: &Scope<N>, index: usize) -> bool {
    fn holds_goal(term: &Term) -> bool {
        matches!(&**term, Subterm::Metavar(metavar) if metavar.origin == MetavarOrigin::Goal)
            || term.any_child_term(&mut |child| holds_goal(child))
    }

    arm.uses(index) || holds_goal(arm.body())
}

/// One arm of a free-monoid elimination, at its case value. A case split's has the scrutinee refined to the case, the locals its solution re-types re-assumed, and the body checked against the result there.
///
/// An arm of a fold — an elimination whose cons arm reads its hypothesis — learns nothing of the scrutinee: the fold at the tail is the same arms over the tail, so reduction takes each at every tail of the value, where the scrutinee is no case of it. It is checked at the motive opened at its case value and under nothing else, the kernel's rule (`check_free_monoid`), and a hypothesis about the scrutinee rides in the motive.
fn check_fold_arm(
    context: &mut Context,
    head: &Term,
    result: &MatchResult,
    value: &Term,
    body: &Term,
    reads_hypothesis: bool,
) -> Result<Term, Error> {
    if reads_hypothesis {
        return check(context, body, result.at(head, &[], &[], value));
    }

    refine_head(context, head, value)
        .and_then(|()| {
            retype_locals(context, head, value, Vec::new());
            check(context, body, result.at(head, &[], &[], value))
        })
        .map_err(|error| from_arm(context, head, value, error))
}

/// `error`, raised in the arm at `value`, reported as one in a dead arm where the guard is always another case — see [`unreachable_arm`] — or where the arm's case contradicts the guard of an arm around it — see [`contradicted_guard`].
fn from_arm(context: &mut Context, head: &Term, value: &Term, error: Error) -> Error {
    if let Some(case) = unreachable_arm(context, head, value) {
        return error.in_unreachable_arm(head.clone(), case);
    }
    match contradicted_guard(context, head, value) {
        Some((guard, case)) => error.under_contradicted_guard(guard, case),
        None => error,
    }
}

fn elaborate_nat_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = scrutinee.of_intrinsic(context, term, IntrinsicHead::Nat)?;

    let reads_hypothesis = may_read_hypothesis(succ_case, 1);
    let result = resolve_fold_result(
        context,
        &Subterm::Intrinsic(Intrinsic::NatType).into(),
        &head_elaborated,
        motive,
        (term, &mode),
        reads_hypothesis,
    )?;

    // A case split's arms have the scrutinee refined to their case (as `Bool`/`Switch` do), so a hypothesis whose type mentions it reduces at the arm's value with no hand-written convoy. A fold's arms learn nothing of it (`check_fold_arm`).
    let zero_value: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))).into();
    let zero_elaborated = context.with_frame(|context| {
        check_fold_arm(
            context,
            &head_elaborated,
            &result,
            &zero_value,
            zero_case,
            reads_hypothesis,
        )
    })?;

    let pred_label = context.fresh_for(succ_case.hint(0), succ_case.written(0));
    let ih_label = context.fresh_for(succ_case.hint(1), succ_case.written(1));

    let succ_body = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Intrinsic(Intrinsic::NatType).into());
        if let Some(motive) = result.family().filter(|_| reads_hypothesis) {
            context.assume(&ih_label, &motive.open(&[&Term::free_var(&pred_label)]));
        }

        let succ_value: Term = Subterm::Intrinsic(Intrinsic::nat_add(
            Term::free_var(&pred_label),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        ))
        .into();

        let body = succ_case.open(&[&Term::free_var(&pred_label), &Term::free_var(&ih_label)]);
        check_fold_arm(
            context,
            &head_elaborated,
            &result,
            &succ_value,
            &body,
            reads_hypothesis,
        )
    })?;

    let succ_elaborated = Scope::close(Two, &[&pred_label, &ih_label], succ_body);

    let result_type = result.of(&head_elaborated, &[]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result,
        // `Nat` is the free monoid on one payload-less generator: its cons arm binds just (predecessor, ih), so the carrier is `Nat` and the head is absent.
        cases: Cases::FreeMonoid {
            carrier: Carrier::Nat {
                empty_case: zero_elaborated,
                cons_case: succ_elaborated,
            },
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

fn elaborate_list_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `List` carries an element type the eliminator must read off the scrutinee (unlike `Nat`, whose carrier is parameterless) — its type must be `List(elem)`.
    let Scrutinee {
        term: head_elaborated,
        type_: head_type,
    } = scrutinee;
    let elem = match &*head_type {
        Subterm::Intrinsic(Intrinsic::ListType(elem)) => elem.clone(),
        _ => {
            return Err(refused_scrutinee(
                term,
                resolved_for_display(context, &head_type),
            ));
        }
    };

    let reads_hypothesis = may_read_hypothesis(cons_case, 2);
    let result = resolve_fold_result(
        context,
        &head_type,
        &head_elaborated,
        motive,
        (term, &mode),
        reads_hypothesis,
    )?;

    // A case split's arms have the scrutinee refined to their case (as `Bool`/`Switch` do), so a hypothesis whose type mentions it reduces at the arm's value with no hand-written convoy. A fold's arms learn nothing of it (`check_fold_arm`).
    let empty_value: Term = Subterm::Intrinsic(Intrinsic::List {
        element: elem.clone(),
        items: vec![],
    })
    .into();
    let empty_elaborated = context.with_frame(|context| {
        check_fold_arm(
            context,
            &head_elaborated,
            &result,
            &empty_value,
            empty_case,
            reads_hypothesis,
        )
    })?;

    let head_label = context.fresh_for(cons_case.hint(0), cons_case.written(0));
    let tail_label = context.fresh_for(cons_case.hint(1), cons_case.written(1));
    let ih_label = context.fresh_for(cons_case.hint(2), cons_case.written(2));

    let cons_body = context.with_frame(|context| {
        context.assume(&head_label, &elem);
        context.assume(&tail_label, &head_type);
        if let Some(motive) = result.family().filter(|_| reads_hypothesis) {
            context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));
        }

        // The cons value `head :: tail`, encoded as the monoid operation on a singleton and the tail (no separate prepend intrinsic).
        let cons_value: Term = Subterm::Intrinsic(Intrinsic::ListConcat {
            element: elem.clone(),
            operands: vec![
                Subterm::Intrinsic(Intrinsic::List {
                    element: elem.clone(),
                    items: vec![Term::free_var(&head_label)],
                })
                .into(),
                Term::free_var(&tail_label),
            ],
        })
        .into();

        let body = cons_case.open(&[
            &Term::free_var(&head_label),
            &Term::free_var(&tail_label),
            &Term::free_var(&ih_label),
        ]);
        check_fold_arm(
            context,
            &head_elaborated,
            &result,
            &cons_value,
            &body,
            reads_hypothesis,
        )
    })?;

    let cons_elaborated = Scope::close(Three, &[&head_label, &tail_label, &ih_label], cons_body);

    let result_type = result.of(&head_elaborated, &[]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result,
        cases: Cases::FreeMonoid {
            carrier: Carrier::List {
                elem,
                empty_case: empty_elaborated,
                cons_case: cons_elaborated,
            },
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

fn elaborate_bin_match(
    context: &mut Context,
    grain: Grain,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    cases: (&Term, &Scope<Three>),
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (empty_case, cons_case) = cases;
    // `Bin` is a parameterless carrier (like `Nat`/`Bool`), so the scrutinee's type is just `Bin` — no element type to read off the head as `List` needs.
    let (head_elaborated, head_type) =
        scrutinee.of_intrinsic(context, term, IntrinsicHead::Bin(grain))?;

    let reads_hypothesis = may_read_hypothesis(cons_case, 2);
    let result = resolve_fold_result(
        context,
        &head_type,
        &head_elaborated,
        motive,
        (term, &mode),
        reads_hypothesis,
    )?;

    // A case split's arms have the scrutinee refined to their case (as `Bool`/`Switch` do), so a hypothesis whose type mentions it reduces at the arm's value with no hand-written convoy. A fold's arms learn nothing of it (`check_fold_arm`).
    let empty_value: Term = Subterm::Intrinsic(Intrinsic::Bin(grain, Binary::empty())).into();
    let empty_elaborated = context.with_frame(|context| {
        check_fold_arm(
            context,
            &head_elaborated,
            &result,
            &empty_value,
            empty_case,
            reads_hypothesis,
        )
    })?;

    let head_label = context.fresh_for(cons_case.hint(0), cons_case.written(0));
    let tail_label = context.fresh_for(cons_case.hint(1), cons_case.written(1));
    let ih_label = context.fresh_for(cons_case.hint(2), cons_case.written(2));

    let cons_body = context.with_frame(|context| {
        let atom_type: Term = Subterm::Intrinsic(match grain {
            Grain::B => Intrinsic::BoolType,
            Grain::X => Intrinsic::ByteType,
        })
        .into();
        context.assume(&head_label, &atom_type);
        context.assume(&tail_label, &head_type);
        if let Some(motive) = result.family().filter(|_| reads_hypothesis) {
            context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));
        }

        // The cons value `head :: tail`, encoded as the monoid operation on the singleton `[head]` and the tail. A `Bits`/`Bytes` literal holds only concrete bytes, so the singleton of the symbolic byte `head` is `append(x[], head)` (an atom appended to the empty packed sequence), not a literal run.
        let singleton: Term = Subterm::Intrinsic(Intrinsic::BinAppend {
            grain,
            bin: Subterm::Intrinsic(Intrinsic::Bin(grain, Binary::empty())).into(),
            element: Term::free_var(&head_label),
        })
        .into();
        let cons_value: Term = Subterm::Intrinsic(Intrinsic::BinConcat {
            grain,
            operands: vec![singleton, Term::free_var(&tail_label)],
        })
        .into();

        let body = cons_case.open(&[
            &Term::free_var(&head_label),
            &Term::free_var(&tail_label),
            &Term::free_var(&ih_label),
        ]);
        check_fold_arm(
            context,
            &head_elaborated,
            &result,
            &cons_value,
            &body,
            reads_hypothesis,
        )
    })?;

    let cons_elaborated = Scope::close(Three, &[&head_label, &tail_label, &ih_label], cons_body);

    let result_type = result.of(&head_elaborated, &[]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result,
        cases: Cases::FreeMonoid {
            carrier: Carrier::Bin {
                grain,
                empty_case: empty_elaborated,
                cons_case: cons_elaborated,
            },
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// The result of an elimination whose arms read no induction hypothesis — `Bool`, `Switch`, and a fold's case split (`resolve_fold_result`): an elided motive checked against an expected type takes that type as its ambient result, whatever the scrutinee, and anything else resolves as an intrinsic motive. A fold whose arm reads its hypothesis keeps the family — the hypothesis is typed at the motive opened at the tail.
///
/// An expression scrutinee takes the ambient form as a variable does: each arm checks against the goal with the scrutinee's syntactic occurrences standing for the case (`MatchResult::at`), and the arm's refinement reduces any occurrence the goal reaches only by unfolding. A family solved by `solve`'s occurrence abstraction instead sees the goal as it arrives reduced at its root, and an occurrence spelled through a `let` there escapes the abstraction while the application the refinement is keyed on has been unfolded away, which would leave an arm less than the unabstracted goal gives it.
fn resolve_intrinsic_result(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    mode: &Mode,
) -> Result<MatchResult, Error> {
    if let Mode::Check(expected) = mode
        && is_elided_motive(motive)
    {
        return Ok(MatchResult::Ambient(expected.clone()));
    }
    resolve_intrinsic_motive(context, head_type, head, motive, mode).map(MatchResult::Family)
}

fn elaborate_switch(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    cases: &[(Natural, Term)],
    default: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = scrutinee.of_intrinsic(context, term, IntrinsicHead::Nat)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`, or the ambient goal.
    let result = resolve_intrinsic_result(
        context,
        &Subterm::Intrinsic(Intrinsic::NatType).into(),
        &head_elaborated,
        motive,
        &mode,
    )?;
    if let Some(motive) = result.family() {
        seed_motive(context, term, motive, &head_elaborated, &mode)?;
    }

    // The arms keep the order they arrived in, which `Cases::Switch` states is strictly ascending — elaborating an arm rewrites its body, never its key.
    let mut cases_elaborated = Vec::with_capacity(cases.len());
    for (n, body) in cases {
        let literal: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(n.clone()))).into();
        let body = context
            .with_frame(|context| {
                refine_head(context, &head_elaborated, &literal)?;
                retype_locals(context, &head_elaborated, &literal, Vec::new());
                check(
                    context,
                    body,
                    result.at(&head_elaborated, &[], &[], &literal),
                )
            })
            .map_err(|error| from_arm(context, &head_elaborated, &literal, error))?;
        cases_elaborated.push((n.clone(), body));
    }

    let result_type = result.of(&head_elaborated, &[]);
    let default_elaborated = check(context, default, result_type.clone())?;

    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result,
        cases: Cases::Switch {
            cases: cases_elaborated,
            default: default_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// Whether a rebuilt match's `goal` is the type it is now checked at, as far as conversion says with nothing committed: a pair that would need a metavariable solved is left to the checked type.
fn kept_goal(context: &mut Context, goal: &Term, expected: &Term) -> Result<bool, Error> {
    same_uncommitted(context, goal, expected).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::convert_exhausted(goal.clone(), expected.clone(), refusal)
        })
    })
}

pub(crate) fn elaborate_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // A rebuilt match coming back through re-elaboration at its ambient goal: the goal *is* the expected type it was elaborated against, so it re-elaborates as the elided motive it came from, checked against that goal. Checked at another spelling of that type — the one an arm around it specializes now, where its first elaboration met the scrutinee behind a metavariable not yet substituted — it keeps the goal it holds, so elaborating what elaboration built gives it back: conversion compares two goals with no arm's equation, and a rebuilt copy of a recursive member that differed from the member in a goal's spelling would be another term to it.
    let mode = match (&m.result, mode) {
        (MatchResult::Ambient(goal), Mode::Infer) => Mode::Check(goal.clone()),
        (MatchResult::Ambient(goal), Mode::Check(expected))
            if *goal != expected && kept_goal(context, goal, &expected)? =>
        {
            Mode::Check(goal.clone())
        }
        (_, mode) => mode,
    };

    let (head, head_type) = elaborate(context, &m.head, Mode::Infer)?;
    let scrutinee = Scrutinee {
        term: head,
        type_: reduce_with(context, &head_type)?,
    };

    // No carrier *yet*: a scrutinee type stuck on an unsolved metavariable has decided nothing, and what decides it — tuple arms of an unannotated match the drain settles, a projection waiting on them — may still be coming. The match waits for it rather than refusing its scrutinee one step early.
    if stuck_on_metavar(context, &scrutinee.type_) && context.may_park() {
        return Ok(park_match(context, term, mode, scrutinee));
    }

    eliminate(context, m, term, mode, scrutinee)
}

/// Park a match whose scrutinee's type is stuck ([`ParkedMatch`]): a placeholder stands for it at the expected type, or at a fresh one when it infers its own, until its scrutinee's type is known.
fn park_match(
    context: &mut Context,
    term: &Term,
    mode: Mode,
    scrutinee: Scrutinee,
) -> (Term, Term) {
    let result = match &mode {
        Mode::Check(expected) => expected.clone(),
        Mode::Infer => {
            let sort = context.fresh_sort("parked match type");
            context.fresh_hole_metavar(sort, term.extent())
        }
    };
    let (placeholder, stand_in) = context.fresh_placeholder(result.clone(), term.extent());
    let Scrutinee {
        term: scrutinee,
        type_: scrutinee_type,
    } = scrutinee;
    context.park(
        ParkedWork::Match(ParkedMatch {
            term: term.clone(),
            scrutinee,
            scrutinee_type,
            mode,
            result: result.clone(),
            placeholder,
        }),
        term.clone(),
    );
    (stand_in, result)
}

/// Retry a parked match: under the frozen frame, its scrutinee's type is either still stuck (re-park) or has reached a carrier, and then the match is elaborated over its elaborated scrutinee in the mode it was parked in — an inferred type met with the stand-in type — and the placeholder solved to it.
pub(crate) fn retry_match(
    context: &mut Context,
    parked: ParkedMatch,
    origin: Term,
    frame: FrozenFrame,
) -> Result<(), Error> {
    let rebuilt = context.with_retry_frame(&frame, |context| -> Result<Option<Term>, Error> {
        let type_ = reduce_with(context, &parked.scrutinee_type)?;
        if stuck_on_metavar(context, &type_) {
            return Ok(None);
        }
        let Subterm::Match(m) = &*parked.term else {
            unreachable!("a parked match is a match");
        };
        let scrutinee = Scrutinee {
            term: parked.scrutinee.clone(),
            type_,
        };
        let (rebuilt, type_) = eliminate(context, m, &parked.term, parked.mode.clone(), scrutinee)
            .map_err(|error| error.at_opt(origin.extent()))?;
        if matches!(parked.mode, Mode::Infer) {
            expect(context, &parked.term, &type_, &parked.result)?;
        }
        Ok(Some(rebuilt))
    })?;

    match rebuilt {
        Some(rebuilt) => context.fill_placeholder(parked.placeholder, rebuilt),
        None => context.repark(ParkedWork::Match(parked), origin, frame),
    }

    Ok(())
}

/// The refusal a match's eliminator gives a scrutinee of `type_`, which is the type in its display form: an eliminator holds the reduct, which spells a recursive name as its group.
pub(crate) fn refused_scrutinee(term: &Term, type_: Term) -> Error {
    let Subterm::Match(Match { cases, .. }) = &**term else {
        unreachable!("an eliminator's term is a match");
    };
    match cases {
        Cases::Bool { .. } => Error::not_bool_type(type_),
        Cases::Switch { .. }
        | Cases::FreeMonoid {
            carrier: Carrier::Nat { .. },
        } => Error::not_nat_type(type_),
        Cases::FreeMonoid {
            carrier: Carrier::List { .. },
        } => Error::not_list_type(type_),
        Cases::FreeMonoid {
            carrier: Carrier::Bin { grain, .. },
        } => Error::not_bin_type(*grain, type_),
        Cases::Induct { cases, default, .. } => {
            Error::not_a_induct_type(type_, cases.is_empty() && default.is_none())
        }
    }
}

/// Elaborate a match over its elaborated scrutinee, by the eliminator its cases name.
fn eliminate(
    context: &mut Context,
    m: &Match,
    term: &Term,
    mode: Mode,
    scrutinee: Scrutinee,
) -> Result<(Term, Term), Error> {
    let Match { result, cases, .. } = m;
    let elided;
    let motive = match result {
        MatchResult::Family(motive) => motive,
        MatchResult::Ambient(_) => {
            elided = Term::match_motive_written(Term::hole(context.mint_metavar()));
            &elided
        }
    };

    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => elaborate_bool_match(
            context, scrutinee, motive, false_case, true_case, term, mode,
        ),
        Cases::Switch { cases, default } => {
            elaborate_switch(context, scrutinee, motive, cases, default, term, mode)
        }
        // The instance a match states is this elaboration's to write, off the type its scrutinee is elaborated at: one read back is written again.
        Cases::Induct { cases, default, .. } => elaborate_induct_match(
            context,
            InductMatchInput {
                scrutinee,
                motive,
                cases,
                default: default.as_ref(),
                term,
            },
            mode,
        ),
        Cases::FreeMonoid {
            carrier:
                Carrier::Nat {
                    empty_case,
                    cons_case,
                },
        } => elaborate_nat_match(
            context, scrutinee, motive, empty_case, cons_case, term, mode,
        ),
        Cases::FreeMonoid {
            carrier:
                Carrier::List {
                    empty_case,
                    cons_case,
                    ..
                },
        } => elaborate_list_match(
            context, scrutinee, motive, empty_case, cons_case, term, mode,
        ),
        Cases::FreeMonoid {
            carrier:
                Carrier::Bin {
                    grain,
                    empty_case,
                    cons_case,
                },
        } => elaborate_bin_match(
            context,
            *grain,
            scrutinee,
            motive,
            (empty_case, cons_case),
            term,
            mode,
        ),
    }
}

struct InductMatchInput<'a> {
    scrutinee: Scrutinee,
    motive: &'a Scope<Many>,
    cases: &'a [(Atom, InductArm)],
    default: Option<&'a Term>,
    term: &'a Term,
}

fn elaborate_bool_match(
    context: &mut Context,
    scrutinee: Scrutinee,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = scrutinee.of_intrinsic(context, term, IntrinsicHead::Bool)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`, or the ambient goal.
    let result = resolve_intrinsic_result(
        context,
        &Subterm::Intrinsic(Intrinsic::BoolType).into(),
        &head_elaborated,
        motive,
        &mode,
    )?;
    if let Some(motive) = result.family() {
        seed_motive(context, term, motive, &head_elaborated, &mode)?;
    }

    let false_literal: Term = Subterm::Intrinsic(Intrinsic::Bool(false)).into();
    let false_elaborated = context
        .with_frame(|context| {
            refine_head(context, &head_elaborated, &false_literal)?;
            retype_locals(context, &head_elaborated, &false_literal, Vec::new());
            check(
                context,
                false_case,
                result.at(&head_elaborated, &[], &[], &false_literal),
            )
        })
        .map_err(|error| from_arm(context, &head_elaborated, &false_literal, error))?;

    let true_literal: Term = Subterm::Intrinsic(Intrinsic::Bool(true)).into();
    let true_elaborated = context
        .with_frame(|context| {
            refine_head(context, &head_elaborated, &true_literal)?;
            retype_locals(context, &head_elaborated, &true_literal, Vec::new());
            check(
                context,
                true_case,
                result.at(&head_elaborated, &[], &[], &true_literal),
            )
        })
        .map_err(|error| from_arm(context, &head_elaborated, &true_literal, error))?;

    let result_type = result.of(&head_elaborated, &[]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result,
        cases: Cases::Bool {
            false_case: false_elaborated,
            true_case: true_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// Whether a proposition may be matched into data: it declares one constructor, and that constructor's index targets pin every payload binder (matching a value against a target recovers the binder, as `Eq`'s `refl(z) : (z, z)` recovers `z`), decided by the walk both checkers share. Occurrence is not determination: `blur(a)` mentions `a` and pins nothing, since `blur` need not be injective. A payload that is a proof is not excused, since the arm would be handed the proof the match was given; and a second constructor the scrutinee's indices rule out is one the kernel counts all the same.
fn singleton_eliminable(context: &mut Context, induct_decl: &InductDecl, params: &[Term]) -> bool {
    let [(tag, _)] = induct_decl.constructors.as_slice() else {
        return false;
    };
    let Some(payload) = induct_decl.instantiate(tag, params) else {
        return false;
    };

    // Open the payload telescope under fresh binders; the terminal's indices are the constructor's targets.
    let mut binders: Vec<Free> = Vec::new();
    let mut cursor = payload.cursor();
    while cursor.entry().is_some() {
        binders.push(cursor.advance_fresh(|hint| context.fresh(hint)));
    }
    let terminal = cursor.body().expect("a cursor past every entry");

    wholly_pinned(&binders, &terminal)
}

/// The intrinsic eliminator's typing rule. Arm binders are typed directly from the constructor's registry telescope instantiated at the scrutinee type's parameters — no projections from a stuck payload — and each arm's written binders are aligned to that telescope's marks, a hidden payload left out bound all the same.
///
/// The motive binds the scrutinee's indices and then the scrutinee, whatever its body does with them: each arm checks against the motive at *that case's* target indices, the whole match types at the scrutinee's *actual* indices, and a catch-all default — which binds nothing and refines no index — checks at the actual indices too.
fn elaborate_induct_match(
    context: &mut Context,
    input: InductMatchInput<'_>,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let InductMatchInput {
        scrutinee,
        motive,
        cases,
        default,
        term,
    } = input;

    // A match with no arms is a vacuous elimination — either of an empty inductive (`False`) or of one whose every constructor inversion-clashes at the scrutinee's indices. Such a match compiles to unreachable code that never inspects the scrutinee. A proof/type scrutinee carries no runtime content (sort-driven erasure), so an empty match discharging an erased witness of falsity into a relevant result is sound without a usage discipline.
    let Scrutinee {
        term: head_elaborated,
        type_: head_type,
    } = scrutinee;

    let (name, universes, params, indices) = match &*head_type {
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            indices,
        }) => (*name, universes.clone(), params.clone(), indices.clone()),
        _ => {
            return Err(refused_scrutinee(
                term,
                resolved_for_display(context, &head_type),
            ));
        }
    };

    // Reducing the scrutinee type to weak-head normal form leaves its index *arguments* untouched, so an index that is an outer-arm key (`s` refined to `Scan/bad()` by an enclosing match) still reads as the bare variable. Reduce each index in the current (refined) context so inversion sees the forced value and pins arm binders against it, rather than refusing it as a key-shaped index, which refinement handles.
    let actual_indices = indices
        .iter()
        .map(|index| reduce_with(context, index))
        .collect::<Result<Vec<_>, _>>()?;

    let Some(induct_decl) = context.induct_decl(&name).cloned() else {
        return Err(Error::unknown_declaration(name.symbol()));
    };
    let induct_decl = context.instantiate_induct_decl_at(&induct_decl, &universes)?;

    // Opacity covers every eliminator, including defaults and vacuous or inversion-discharged matches. Check before motives, coverage, or index refinement can reveal representation facts.
    if !induct_decl.rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(&induct_decl.module))
    {
        return Err(Error::private_representation(name.symbol()));
    }

    // The motive abstracts the index telescope instantiated at the scrutinee's actual parameters, then the scrutinee.
    let shape = MotiveShape::Induct {
        name: &name,
        universes: &universes,
        params: &params,
        indices: induct_decl.indices_at(&params),
    };

    // An elided motive checked against an expected type takes that type as its *ambient* result: each arm is checked against the expected type with the scrutinee and its variable indices standing for the arm's case, and the match's result is the expected type itself. That is what a hand-written convoy arranges, with no convoy: the ambient form needs no family to close, so a hypothesis whose type mentions the scrutinee rides along unchanged — see `MatchResult::Ambient`. An expression scrutinee takes it too, its syntactic occurrences standing for the case and the arm's refinement reducing the rest, for the reason `resolve_intrinsic_result` gives. A written motive, and inference mode, are a family, checked or solved.
    let result = match &mode {
        Mode::Check(expected) if is_elided_motive(motive) => MatchResult::Ambient(expected.clone()),
        _ => MatchResult::Family(check_motive(context, &shape, motive)?),
    };

    // The match's own type: the result at the scrutinee's actual indices and the scrutinee itself — opened from the *rebuilt* motive, as in `elaborate_nat_match`, or the ambient goal as written.
    let result_type = result.of(&head_elaborated, &actual_indices);

    // The seed (`seed_motive`'s job, generalized over the pattern binders): in checking mode, pin the motive — a bare metavar when elided — to the expected type before the arms are checked. An ambient result is that type already, and the expectation is met by construction.
    if let Mode::Check(expected) = &mode {
        expect(context, term, &result_type, expected)?;
    }

    // Large-elimination guard: a strict proposition may not be eliminated into a relevant (data) result — observing which inhabitant it was would break proof irrelevance. Permitted only by *empty* elimination (no arm, every constructor ruled out) or by the elimination of a family with one constructor whose every payload its indices pin.
    // Whether the scrutinee's family is a proposition whose one constructor has every payload pinned: the only kind a match may take into data once it has an arm, and the kind whose arms are written over what their equations state below.
    let proposition = is_prop(context, &head_type)?;
    let pinned = proposition && singleton_eliminable(context, &induct_decl, &params);
    // A match with no arm is vacuous: the coverage check below proves every constructor ruled out at the scrutinee's indices, so nothing is eliminated. Any other is judged by the family's declared constructors, as the kernel judges it, so a second constructor the indices rule out here counts all the same. A catch-all default stands in for more than one constructor — a relevant result branching on an erased scrutinee's tag is exactly what this guard forbids — so it is never permitted here. (A prop-into-prop defaulted match never reaches this branch.)
    let permitted = default.is_none() && (cases.is_empty() || pinned);
    // Every other match on a proposition answers a proposition and nothing else. A result known for data is refused here, before an arm is read. One whose sort nothing has decided waits for the arms, which decide it where they are typed, and is held to `Prop` after them ([`hold_prop`]): so a match its arms make data is refused as this elimination, and not as a mismatch in an arm.
    let undecided = match proposition && !permitted {
        false => false,
        true => {
            let quality = quality_of(context, &result_type)?;
            probe_open_sort(context, "elaborate_induct_match", quality);
            match quality {
                Quality::Prop => false,
                Quality::Meta(_) => true,
                Quality::Type | Quality::Param(_) => {
                    return Err(eliminated_into_data(context, &name, &result_type));
                }
            }
        }
    };

    // Every written arm must name a constructor; coverage is decided per constructor below — a missing arm is legal iff inversion proves it impossible.
    if let Some(tag) = cases
        .iter()
        .map(|(tag, _)| tag)
        .find(|tag| !induct_decl.declares(tag))
    {
        // Each constructor as a pattern writes it, one placeholder per plain payload — a hidden one is left out — so the report shows what the arm could have named.
        let constructors = induct_decl
            .constructor_order()
            .map(|constructor| {
                let payload = induct_decl
                    .payload_plicities(constructor)
                    .unwrap_or_default()
                    .iter()
                    .filter(|plicity| **plicity == Plicity::Explicit)
                    .map(|_| "_")
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{constructor}({payload})")
            })
            .collect();
        return Err(Error::unknown_match_constructor(
            name.symbol(),
            tag.to_string(),
            constructors,
        ));
    }

    // Built by walking the *declaration* order, not the written order, so the elaborated arm sequence is canonical: two matches differing only in how their arms were written produce the same term.
    let mut cases_elaborated = Vec::new();
    for tag in induct_decl.constructor_order() {
        let Some((_, scope)) = cases.iter().find(|(candidate, _)| candidate == tag) else {
            // A catch-all default covers every un-enumerated constructor, so a missing arm needs neither the unindexed-completeness check nor index inversion — the default is checked once, below.
            if default.is_some() {
                continue;
            }

            // An unindexed inductive has nothing to invert: every arm is reachable and a missing one is plainly missing.
            if actual_indices.is_empty() {
                return Err(Error::match_case_missing(name.symbol(), tag.to_string()));
            }

            // Inversion — checker-verified omission: a missing arm is accepted iff first-order unification of the scrutinee's actual indices against this case's targets finds a *definite* clash, in either direction — the answer a written arm is checked under, so an arm may be left out exactly where the kernel would type none of it. The arm is then pruned (erase fills its slot with an unreachable body); anything short of definite keeps the arm mandatory.
            let telescope = induct_decl
                .instantiate(tag, &params)
                .expect("constructor instantiates at its inductive's parameters");

            let labels = (0..telescope.len())
                .map(|_| context.fresh(None))
                .collect::<Vec<_>>();

            // The case is opened as a written arm's is, its binders assumed, because inversion reconciles a binder forced twice *at its type*: `refl(@z) : (z, z)` against `Eq()(false, true)` is decided by what `false` and `true` are at `Bool`, and a binder that is only a name has no type to be asked at.
            let inversion = context.with_frame(|context| {
                let ix_c = assume_payload(context, telescope, &labels);
                solve_indices(context, &actual_indices, &ix_c, &labels)
            })?;

            match inversion {
                Invert::Impossible => continue,
                Invert::Solved(_) => {
                    return Err(Error::missing_arm_not_impossible(tag.clone()));
                }
            }
        };

        let telescope = induct_decl
            .instantiate(tag, &params)
            .expect("constructor instantiates at its inductive's parameters");

        // An arm's written binders meet the payload's slots as a lambda's meet a function type's ([`align`]): the plain binders are the plain payloads, in order, and between two of them the hidden binders written are the first of their run. A hidden payload no binder was written for is bound all the same, hintless, so nothing in the arm names it and it is in scope as its mark makes it.
        let arity = telescope.len();
        let payload_plicities = telescope.marks();
        let payload_proofs = telescope.proofs();
        let written = scope.plicities();
        let plain = |marks: &[Plicity]| {
            marks
                .iter()
                .filter(|mark| **mark == Plicity::Explicit)
                .count()
        };
        let fills = align(&payload_plicities, written).map_err(|misaligned| match misaligned {
            // Plain payloads against plain binders: a hidden payload is bound rather than written, so a total would name a count the arm may not write.
            Misaligned::Missing | Misaligned::Extra { .. } => {
                Error::ctor_arity_mismatch(tag.clone(), plain(&payload_plicities), plain(written))
            }
            Misaligned::Mark { member, slot } => Error::hidden_member_out_of_order(
                written[member],
                payload_plicities[slot],
                telescope.labels()[slot],
            ),
            // A marked binder with no hidden payload left in its run: at the plain payload the plain binders before it have reached, which no mark binds, or past the last.
            Misaligned::Surplus { member } => {
                match stands_at(&payload_plicities, written, member) {
                    Some(slot) => Error::BinderPlicityMismatch {
                        site: BinderSite::Payload {
                            constructor: tag.to_string(),
                        },
                        position: slot + 1,
                        binder: String::new(),
                        expected: payload_plicities[slot],
                        written: written[member],
                    },
                    None => Error::hidden_member_without_slot(written[member]),
                }
            }
        })?;

        // Open the telescope with fresh names, one per payload: a written binder's under its own label, each binder assumed at its declared (dependent) type.
        let labels = fills
            .iter()
            .map(|fill| match fill {
                Some(member) => {
                    context.fresh_for(scope.body.hint(*member), scope.body.written(*member))
                }
                None => context.fresh(None),
            })
            .collect::<Vec<_>>();
        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

        // Refinement propagates `head := ctor_val` to other occurrences of the scrutinee in the arm body; the binder types themselves came from the telescope below. Built at the scrutinee's own universe levels, because this value outlives the refinement: the motive is opened on it, so it is what a metavariable in an arm's expected type is solved to — the `@z` of an `Eq/refl()` against `Eq()(len(xs), len(xs))` — and a level-less occurrence of a polymorphic family zonks into the definition, where the arity check (or, for a prelude family it cannot see, the kernel) refuses it.
        let ctor_val = Term::variant_at(
            name,
            universes.clone(),
            params.clone(),
            tag.clone(),
            vars.clone(),
        );

        // Why the arm is never taken, where its case's indices clash with the scrutinee's.
        let mut ruled_out = None;
        let (body_elaborated, held) = context
            .with_frame(|context| {
                let ix_c = assume_payload(context, telescope, &labels);
                refine_head(context, &head_elaborated, &ctor_val)?;

                // The index equations: the most-general solution of `actual indices ~ case targets`, both directions — arm binders pinned to the actuals they must equal, outer variables refined to the targets they must equal — by the kernel's own function, and recorded as the same frame-scoped refinements. A definite clash means the arm is unreachable; it was written, so it is simply checked as is, with nothing solved, and an error it raises is reported as one in an arm that may be left out. Refinements never justify the typing (the motive application does); they are convertibility aids, so the body's occurrences of a solved variable reduce at the arm's indices.
                let solutions = match solve_indices(context, &actual_indices, &ix_c, &labels)? {
                    Invert::Solved(solutions) => solutions,
                    Invert::Impossible => {
                        let case = Term::induct_type_at(
                            name,
                            universes.clone(),
                            params.clone(),
                            ix_c.clone(),
                        );
                        let scrutinee = Term::induct_type_at(
                            name,
                            universes.clone(),
                            params.clone(),
                            actual_indices.clone(),
                        );
                        ruled_out = Some(context.rule_out_arm(case, scrutinee));

                        Vec::new()
                    }
                };
                for (name, solution) in &solutions {
                    context.refine(name, solution);
                }

                // The result at this case: a family's index binders take the case's target indices and its scrutinee binder the constructed value; an ambient goal has the case's targets and value substituted for its variable indices and scrutinee.
                let expected = result.at(&head_elaborated, &actual_indices, &ix_c, &ctor_val);
                // What the equations hold each payload binder to, kept for the arm that is rebuilt below.
                let held = solutions
                    .iter()
                    .filter(|(name, _)| labels.contains(name))
                    .cloned()
                    .collect::<Vec<_>>();
                retype_locals(context, &head_elaborated, &ctor_val, solutions);

                // The body is closed over the binders the arm wrote, which the alignment matched to their payloads in order.
                let var_refs = vars
                    .iter()
                    .zip(&fills)
                    .filter(|(_, fill)| fill.is_some())
                    .map(|(var, _)| var)
                    .collect::<Vec<_>>();
                check(context, &scope.open(&var_refs), expected).map(|body| (body, held))
            })
            .map_err(|error| match &ruled_out {
                Some(dead) => error.in_dead_arm(dead),
                None => from_arm(context, &head_elaborated, &ctor_val, error),
            })?;

        // A match on a proposition whose one constructor is wholly pinned writes a payload binder as what the arm's equations hold it to, which is the index that pins it wherever the scrutinee's indices unify with the constructor's. An arm written so names no payload, and on a closed term the match is its body with the proof unreduced. A binder the equations leave open stays named: the arm is typed at it, and reduction binds it to the index when the match fires.
        let body_elaborated = match pinned {
            true => body_elaborated.substitute(&held),
            false => body_elaborated,
        };

        let label_strs = labels.iter().collect::<Vec<_>>();
        // Rebuild the arm over every payload under the constructor's canonical marks, so a re-elaborated arm writes each binder and re-checks identically (idempotence). Each binder states what its payload's does of whether it binds a proof, which the constructor's declaration said when it was published.
        cases_elaborated.push((
            tag.clone(),
            InductArm::new(
                Scope::close(Many(arity), &label_strs, body_elaborated)
                    .with_proofs(&payload_proofs),
                payload_plicities,
            ),
        ));
    }

    // The catch-all binds nothing and refines no index, so it is checked at the unrefined head — the result at the actual scrutinee, exactly as `elaborate_switch` checks its default.
    let default_elaborated = default
        .map(|d| context.with_frame(|context| check(context, d, result_type.clone())))
        .transpose()?;

    if undecided && !hold_prop(context, &result_type, term.extent())? {
        return Err(eliminated_into_data(context, &name, &result_type));
    }

    // A match with one arm over a proposition whose indices pin its payload states the instance it eliminates: reduction fires it on that instance and reads no binder's type, so what it is moves with the term and with nothing else.
    let stated = (pinned && default_elaborated.is_none() && cases_elaborated.len() == 1)
        .then(|| head_type.clone());
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result,
        cases: Cases::Induct {
            cases: cases_elaborated,
            default: default_elaborated,
            stated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// The refusal of a match on the proposition `name` into `result_type`, which is no proposition: this elimination's. Where the result stands at a `Sort` the declaration wrote, it is a type's because an earlier term decided so, the reader having written neither sort, and the refusal is that the two terms decide it apart.
fn eliminated_into_data(context: &mut Context, name: &Global, result_type: &Term) -> Error {
    match decided_sort_of(context, result_type) {
        Some(other) => Error::sort_decided_twice(true, other),
        None => Error::large_elim_of_prop(name.symbol()),
    }
}

/// Open a case's instantiated telescope under `labels`, each entered at its declared (dependent) type under its payload's mark, and answer the index targets the signature terminates in, stated over those binders. The caller holds the frame the assumptions live in.
///
/// A written arm and an omitted one open their case the same way, so inversion is put the same question about both — which is also the question the kernel puts, its callers reaching the unifier through a payload open that assumes every binder.
fn assume_payload(
    context: &mut Context,
    telescope: Telescope<Vec<Term>>,
    labels: &[Free],
) -> Vec<Term> {
    let mut cursor = telescope.cursor();
    for label in labels {
        let (_, type_) = cursor
            .entry()
            .expect("a case's binders parallel its telescope");
        let mark = cursor
            .mark()
            .expect("a case's binders parallel its telescope");
        context.enter(label, &type_, mark);
        cursor.advance(Term::free_var(label));
    }

    match cursor.body() {
        Some(targets) => targets,
        None => unreachable!("a case's binders parallel its telescope"),
    }
}

/// Re-assume every local a case's solution re-types, at its specialized type: `curios_analysis::retyped` over `solutions` joined by the scrutinee's own (`curios_analysis::scrutinee_solution`), which is the list and the rule the kernel's arm applies. The arm's refinements make such a type *reduce* at the case, which is enough for the body's own conversions, but not for a metavariable born in the arm: it keeps the types of its birth context and checks its solution against them, retried outside the frame — `z : Sizes(s)` used as `(z).0` under `s := node(a, b)` has to be a tuple there, which the re-typed entry states outright.
fn retype_locals(
    context: &mut Context,
    head: &Term,
    value: &Term,
    mut solutions: Vec<(Free, Term)>,
) {
    let value = value.substitute(&solutions);
    if let Some(solution) = scrutinee_solution(&*context, head, &value) {
        solutions.push(solution);
    }

    let locals = context.locals().to_vec();
    for (name, type_) in retyped(&*context, &locals, &solutions) {
        context.assume(&name, &type_);
    }
}

/// Whether a motive scope is the lowering's elided form — a bare metavariable body (`match s | ..` or the explicit hole `match s : _ | ..`), as opposed to a user-written constant or scrutinee-binding motive. Only the elided form takes the ambient result; everything else is taken verbatim.
fn is_elided_motive(motive: &Scope<Many>) -> bool {
    // A silent hole only: a written `?` motive is a user-written motive the author is asking about, checked against the eliminator's motive type like any other and reported by zonk (`MetavarOrigin` states the rule).
    matches!(&**motive.body(), Subterm::Metavar(metavar) if metavar.is_hole())
}
