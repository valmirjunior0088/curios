use {
    super::{Context, Error, Mode, check, elaborate, expect},
    crate::{
        BinderSite, FrozenFrame, MotiveShape, ParkedMatch, ParkedWork, check_intrinsic_head,
        check_motive, fill_placeholder, is_prop, reduce_with, refine_head, stuck_on_metavar,
        unreachable_arm,
    },
    curios_analysis::{
        Invert, invert_indices, pinned_by_targets, retyped, scrutinee_solution, solve_indices,
    },
    curios_core::{
        Advance, Arity, Atom, Carrier, Cases, Free, InductArm, InductDecl, InductType, Intrinsic,
        IntrinsicHead, Many, Match, MatchResult, MetavarOrigin, Nat, Scope, Subterm, Telescope,
        Term, Three, Two,
    },
    curios_num::{Binary, Grain, Natural},
    curios_utilities::Plicity,
    std::collections::BTreeSet,
};

/// A match's scrutinee, elaborated once by [`elaborate_match`] before any arm is: rebuilt, beside its type reduced — what every eliminator reads its carrier off.
struct Scrutinee {
    term: Term,
    type_: Term,
}

impl Scrutinee {
    /// The rebuilt scrutinee and its type, required to be the given intrinsic type. The authoritative analogue of `expect_intrinsic_head` (kept for `erase`).
    fn of_intrinsic(self, expected: IntrinsicHead) -> Result<(Term, Term), Error> {
        let Self { term, type_ } = self;
        check_intrinsic_head(expected, type_).map(|type_| (term, type_))
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

/// Refuse a fold whose arm reads its hypothesis, under a motive that reaches its scrutinee other than through the binder it declares.
///
/// The induction hypothesis is assumed at the motive opened at the tail *inside* the cons arm, where `refine_head` has the scrutinee reducing to the cons value. A captured occurrence reduces with it, so the hypothesis would be typed at the arm's own goal: `match n : (_) => Eq()(n, 0) | 0 => refl | k + 1; ih => ih end` then proves `Eq()(n, 0)` for every `n`. The kernel refuses the same shape by the same test (`check_free_monoid`); this is the elaborator's copy, so the refusal is reported where the motive was written. A local defined in the frame — a `let` alias of the scrutinee, or a binder an enclosing arm refined — is read through its definition, since the reducer will read it the same way.
fn refuse_captured_scrutinee(
    context: &Context,
    motive: &Scope<Many>,
    head: &Term,
) -> Result<(), Error> {
    // A metavariable's spine names every local in scope and is not an occurrence of any of them; a solution that does capture the scrutinee is met by the kernel's copy of this test, which runs post-zonk. `Term::mentions_term` would read the spine, so the walk is spelled here.
    fn occurs(term: &Term, head: &Term) -> bool {
        if term == head {
            return true;
        }
        if matches!(&**term, Subterm::Metavar(_)) {
            return false;
        }
        term.any_child_term(&mut |child| occurs(child, head))
    }

    // The free variables outside any metavariable spine, for the same reason: a spine names the whole context, and reading a scrutinee an *enclosing* arm refined through its reduct is a real capture only where the motive names that scrutinee.
    fn frees(term: &Term, out: &mut BTreeSet<Free>) {
        match &**term {
            Subterm::Var(var) => {
                if let Some(name) = var.as_free() {
                    out.insert(*name);
                }
            }
            Subterm::Metavar(_) => {}
            _ => {
                term.any_child_term(&mut |child| {
                    frees(child, out);
                    false
                });
            }
        }
    }

    fn captures(context: &Context, term: &Term, head: &Term, seen: &mut BTreeSet<Free>) -> bool {
        if occurs(term, head) {
            return true;
        }
        let mut names = BTreeSet::new();
        frees(term, &mut names);
        names.iter().any(|name| {
            seen.insert(*name)
                && context
                    .var_reduct(name)
                    .is_some_and(|reduct| captures(context, reduct, head, seen))
        })
    }

    match captures(context, motive.body(), head, &mut BTreeSet::new()) {
        true => Err(Error::fold_motive_captures_scrutinee(head.clone())),
        false => Ok(()),
    }
}

/// The result of a fold, by the rule every reader of one states — erasure's split-or-fold reading and the kernel's arm rule too: the induction hypothesis is assumed exactly when the arm reads it, and only a family types it, at the motive opened at the tail. An arm that may read it (`may_read_hypothesis`) keeps the family, which is refused if it captures the scrutinee and seeded against the expected type. An arm that cannot is a case split, resolved as a `Bool` elimination is: ambient where the motive is elided and checked, since nothing needs the type a hypothesis would have had.
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
    refuse_captured_scrutinee(context, &motive, head)?;
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

/// One arm of a fold, at its case value: the scrutinee refined to it, the locals its solution re-types re-assumed, and the body checked against the result there.
fn check_fold_arm(
    context: &mut Context,
    head: &Term,
    result: &MatchResult,
    value: &Term,
    body: &Term,
) -> Result<Term, Error> {
    refine_head(context, head, value)
        .and_then(|()| {
            retype_locals(context, head, value, Vec::new());
            check(context, body, result.at(head, &[], &[], value))
        })
        .map_err(|error| from_arm(context, head, value, error))
}

/// `error`, raised in the arm at `value`, reported as one in a dead arm where the guard is always another case — see [`unreachable_arm`].
fn from_arm(context: &mut Context, head: &Term, value: &Term, error: Error) -> Error {
    match unreachable_arm(context, head, value) {
        Some(case) => error.in_unreachable_arm(head.clone(), case),
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
    let (head_elaborated, _) = scrutinee.of_intrinsic(IntrinsicHead::Nat)?;

    let reads_hypothesis = may_read_hypothesis(succ_case, 1);
    let result = resolve_fold_result(
        context,
        &Subterm::Intrinsic(Intrinsic::NatType).into(),
        &head_elaborated,
        motive,
        (term, &mode),
        reads_hypothesis,
    )?;

    // Refine the scrutinee to its constructor in each arm (as `Bool`/`Switch` already do): a context hypothesis whose type mentions the scrutinee then reduces at the arm's value, so a dependent match needs no hand-written convoy to carry it across the eliminator.
    let zero_value: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))).into();
    let zero_elaborated = context.with_frame(|context| {
        check_fold_arm(context, &head_elaborated, &result, &zero_value, zero_case)
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
        check_fold_arm(context, &head_elaborated, &result, &succ_value, &body)
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
        _ => return Err(Error::not_list_type(head_type)),
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

    // Refine the scrutinee to its value in each arm (as `Nat`/`Bool`/`Switch` already do), so a hypothesis whose type mentions the scrutinee reduces at the arm's value without a hand-written convoy.
    let empty_value: Term = Subterm::Intrinsic(Intrinsic::List {
        element: elem.clone(),
        items: vec![],
    })
    .into();
    let empty_elaborated = context.with_frame(|context| {
        check_fold_arm(context, &head_elaborated, &result, &empty_value, empty_case)
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
        check_fold_arm(context, &head_elaborated, &result, &cons_value, &body)
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
    let (head_elaborated, head_type) = scrutinee.of_intrinsic(IntrinsicHead::Bin(grain))?;

    let reads_hypothesis = may_read_hypothesis(cons_case, 2);
    let result = resolve_fold_result(
        context,
        &head_type,
        &head_elaborated,
        motive,
        (term, &mode),
        reads_hypothesis,
    )?;

    // Refine the scrutinee to its value in each arm (as `Nat`/`Bool`/`Switch` already do): a context hypothesis whose type mentions the scrutinee then reduces at the arm's value, so a dependent match needs no hand-written convoy to carry it across the eliminator.
    let empty_value: Term = Subterm::Intrinsic(Intrinsic::Bin(grain, Binary::empty())).into();
    let empty_elaborated = context.with_frame(|context| {
        check_fold_arm(context, &head_elaborated, &result, &empty_value, empty_case)
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
        check_fold_arm(context, &head_elaborated, &result, &cons_value, &body)
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
    let (head_elaborated, _) = scrutinee.of_intrinsic(IntrinsicHead::Nat)?;

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

pub(crate) fn elaborate_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // A rebuilt match coming back through re-elaboration at its ambient goal: the goal *is* the expected type it was elaborated against, so it re-elaborates as the elided motive it came from, checked against that goal.
    let mode = match (&m.result, mode) {
        (MatchResult::Ambient(goal), Mode::Infer) => Mode::Check(goal.clone()),
        (_, mode) => mode,
    };

    let (head, head_type) = elaborate(context, &m.head, Mode::Infer)?;
    let scrutinee = Scrutinee {
        term: head,
        type_: reduce_with(context, &head_type)?,
    };

    // No carrier *yet*: a scrutinee type stuck on an unsolved metavariable has decided nothing, and what decides it — tuple arms of an unannotated match the drain settles, a projection waiting on them — may still be coming. The match waits for it rather than refusing its scrutinee one step early.
    if !context.parking_suppressed() && stuck_on_metavar(context, &scrutinee.type_) {
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
            let classifier = context.fresh_classifier_type("parked match type");
            context.fresh_hole_metavar(classifier, term.span())
        }
    };
    let (placeholder, stand_in) = context.fresh_placeholder(result.clone(), term.span());
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
            .map_err(|error| error.at_opt(origin.span()))?;
        if matches!(parked.mode, Mode::Infer) {
            expect(context, &parked.term, &type_, &parked.result)?;
        }
        Ok(Some(rebuilt))
    })?;

    match rebuilt {
        Some(rebuilt) => fill_placeholder(
            context,
            parked.placeholder,
            &parked.result,
            rebuilt,
            &origin,
        ),
        None => {
            context.repark(ParkedWork::Match(parked), origin, frame);
            Ok(())
        }
    }
}

/// The refusal a match's eliminator gives a scrutinee of `type_`, for a parked match whose scrutinee's type never reached a carrier.
pub(crate) fn refused_scrutinee(term: &Term, type_: Term) -> Error {
    let Subterm::Match(Match { cases, .. }) = &**term else {
        unreachable!("a parked match is a match");
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
        Cases::Induct { cases, default } => {
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
        Cases::Induct { cases, default } => elaborate_induct_match(
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
    let (head_elaborated, _) = scrutinee.of_intrinsic(IntrinsicHead::Bool)?;

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

/// Whether a single-constructor proposition admits large elimination: every payload binder must be non-informative — a proposition itself, or *pinned* by the constructor's index targets (matching a value against a target recovers the binder, as `Eq`'s `refl(z) : (z, z)` recovers `z`), decided by the walk both checkers share. Occurrence is not determination: `blur(a)` mentions `a` and pins nothing, since `blur` need not be injective.
fn singleton_eliminable(
    context: &mut Context,
    induct_decl: &InductDecl,
    tag: &Atom,
    params: &[Term],
) -> Result<bool, Error> {
    let Some(payload) = induct_decl.instantiate(tag, params) else {
        return Ok(false);
    };

    // Open the payload telescope under fresh binders, collecting each binder's (name, type) and the terminal whose indices are the constructor's targets.
    let mut binders: Vec<(Free, Term)> = Vec::new();
    let mut cursor = payload.cursor();
    while let Some((_, ty)) = cursor.entry() {
        binders.push((cursor.advance_fresh(|hint| context.fresh(hint)), ty));
    }
    let terminal = cursor.body().expect("a cursor past every entry");

    // A binder is forced iff the index targets the signature terminates in pin it.
    let forced = pinned_by_targets(&terminal);
    for (name, ty) in &binders {
        if !forced.contains(name) && !is_prop(context, ty)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// The intrinsic eliminator's typing rule. Arm binders are typed directly from the constructor's registry telescope instantiated at the scrutinee type's parameters — no projections from a stuck payload — and each arm's binder count is statically checked against that telescope.
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
        other => {
            return Err(Error::not_a_induct_type(
                other.clone(),
                cases.is_empty() && default.is_none(),
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

    // Large-elimination guard: a strict proposition may not be eliminated into a relevant (data) result — observing which inhabitant it was would break proof irrelevance. Permitted only by *empty* elimination (no constructors) or *singleton* elimination (one constructor whose payload is entirely non-informative: each binder a proposition, or forced by the indices).
    if is_prop(context, &head_type)? && !is_prop(context, &result_type)? {
        // Count the *covered* constructors (the written arms), not the inductive's global ones: index inversion may leave only one applicable — an inductive order at `(a + 1, b + 1)` admits only its successor constructor — making an otherwise multi-constructor proposition singleton in this context. (An under-covered match is still caught by the coverage check below.) A catch-all default stands in for more than one constructor — a relevant result branching on an erased scrutinee's tag is exactly what this guard forbids — so it is never permitted here. (A prop-into-prop defaulted match never reaches this branch.)
        let permitted = default.is_none()
            && match cases.len() {
                0 => true,
                1 => {
                    let (tag, _) = cases.first().expect("one covered constructor");
                    singleton_eliminable(context, &induct_decl, tag, &params)?
                }
                _ => false,
            };
        if !permitted {
            return Err(Error::large_elim_of_prop(name.symbol()));
        }
    }

    // Every written arm must name a constructor; coverage is decided per constructor below — a missing arm is legal iff inversion proves it impossible.
    if let Some(tag) = cases
        .iter()
        .map(|(tag, _)| tag)
        .find(|tag| !induct_decl.declares(tag))
    {
        // Each constructor as a pattern writes it, one placeholder per payload under the mark its position takes, so the report shows what the arm could have named.
        let constructors = induct_decl
            .constructor_order()
            .map(|constructor| {
                let payload = induct_decl
                    .payload_plicities(constructor)
                    .unwrap_or_default()
                    .iter()
                    .map(|plicity| match plicity {
                        Plicity::Implicit => "@_",
                        Plicity::Explicit | Plicity::Witness => "_",
                    })
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

            // Inversion — checker-verified omission: a missing arm is accepted iff first-order inversion of the scrutinee's actual indices against this case's targets finds a *definite* clash. The arm is then pruned (erase fills its slot with an unreachable body); anything short of definite keeps the arm mandatory.
            let telescope = induct_decl
                .instantiate(tag, &params)
                .expect("constructor instantiates at its inductive's parameters");

            let labels = (0..telescope.len())
                .map(|_| context.fresh(None))
                .collect::<Vec<_>>();

            // The case is opened as a written arm's is, its binders assumed, because inversion reconciles a binder forced twice *at its type*: `refl(@z) : (z, z)` against `Eq()(false, true)` is decided by what `false` and `true` are at `Bool`, and a binder that is only a name has no type to be asked at.
            let inversion = context.with_frame(|context| {
                let ix_c = assume_payload(context, telescope, &labels);
                invert_indices(context, &actual_indices, &ix_c, &labels)
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

        // Static arity check: the arm's binder count must equal the constructor's payload arity.
        let arity = telescope.len();
        if scope.arity() != arity {
            return Err(Error::ctor_arity_mismatch(
                tag.clone(),
                arity,
                scope.arity(),
            ));
        }

        // Check each written pattern plicity against the constructor's canonical payload plicity: a payload slot the declaration marked `@` must be matched with `@`, an unmarked payload with a plain binder. (Alignment is exact here — pattern insertion is deferred, so arity already matched above.)
        let payload_plicities = induct_decl
            .payload_plicities(tag)
            .expect("constructor payload plicities parallel its telescope");
        for (position, (written, canonical)) in
            scope.plicities().iter().zip(payload_plicities).enumerate()
        {
            if written != canonical {
                return Err(Error::BinderPlicityMismatch {
                    site: BinderSite::Payload {
                        constructor: tag.to_string(),
                    },
                    position: position + 1,
                    binder: String::new(),
                    expected: *canonical,
                    written: *written,
                });
            }
        }

        // Open the telescope with fresh names paralleling the arm's binder labels; each binder is assumed at its declared (dependent) type.
        let labels = (0..scope.arity())
            .map(|index| context.fresh_for(scope.body.hint(index), scope.body.written(index)))
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

        let body_elaborated = context
            .with_frame(|context| {
                let ix_c = assume_payload(context, telescope, &labels);
                refine_head(context, &head_elaborated, &ctor_val)?;

                // The index equations: the most-general solution of `actual indices ~ case targets`, both directions — arm binders pinned to the actuals they must equal, outer variables refined to the targets they must equal — by the kernel's own function, and recorded as the same frame-scoped refinements. A definite clash means the arm is unreachable; it was written, so it is simply checked as is, with nothing solved. Refinements never justify the typing (the motive application does); they are convertibility aids, so the body's occurrences of a solved variable reduce at the arm's indices.
                let solutions = match solve_indices(context, &actual_indices, &ix_c, &labels)? {
                    Invert::Solved(solutions) => solutions,
                    Invert::Impossible => Vec::new(),
                };
                for (name, solution) in &solutions {
                    context.refine(name, solution);
                }

                // The result at this case: a family's index binders take the case's target indices and its scrutinee binder the constructed value; an ambient goal has the case's targets and value substituted for its variable indices and scrutinee.
                let expected = result.at(&head_elaborated, &actual_indices, &ix_c, &ctor_val);
                retype_locals(context, &head_elaborated, &ctor_val, solutions);

                let var_refs = vars.iter().collect::<Vec<_>>();
                check(context, &scope.open(&var_refs), expected)
            })
            .map_err(|error| from_arm(context, &head_elaborated, &ctor_val, error))?;

        let label_strs = labels.iter().collect::<Vec<_>>();
        // Rebuild the arm with the constructor's canonical payload plicities, so a re-elaborated arm re-checks identically (idempotence).
        cases_elaborated.push((
            tag.clone(),
            InductArm::new(
                Scope::close(Many(arity), &label_strs, body_elaborated),
                payload_plicities.to_vec(),
            ),
        ));
    }

    // The catch-all binds nothing and refines no index, so it is checked at the unrefined head — the result at the actual scrutinee, exactly as `elaborate_switch` checks its default.
    let default_elaborated = default
        .map(|d| context.with_frame(|context| check(context, d, result_type.clone())))
        .transpose()?;

    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result,
        cases: Cases::Induct {
            cases: cases_elaborated,
            default: default_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// Open a case's instantiated telescope under `labels`, each assumed at its declared (dependent) type, and answer the index targets the signature terminates in, stated over those binders. The caller holds the frame the assumptions live in.
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
        context.assume(label, &type_);
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
