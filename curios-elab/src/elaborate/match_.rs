use {
    super::{Context, Error, Mode, check, elaborate, expect},
    crate::{MotiveShape, check_intrinsic_head, check_motive, is_prop, reduce_with, refine_head},
    curios_analysis::{Invert, invert_indices, pinned_by_targets},
    curios_core::{
        Atom, Carrier, Cases, Free, InductArm, InductDecl, InductType, Intrinsic, IntrinsicHead,
        Many, Match, MatchResult, Nat, Scope, Subterm, Telescope, Term, Three, Two,
        case_substitution,
    },
    curios_num::Natural,
    curios_utilities::{Grain, PackedBin},
    std::collections::BTreeSet,
};

/// Infer and rebuild a match scrutinee, requiring its reduced type to be the given intrinsic type. The authoritative analogue of `expect_intrinsic_head` (kept for `erase`): it returns the rebuilt head alongside its reduced type.
fn elaborate_intrinsic_head(
    context: &mut Context,
    head: &Term,
    expected: IntrinsicHead,
) -> Result<(Term, Term), Error> {
    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    check_intrinsic_head(expected, head_type).map(|head_type| (head, head_type))
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

/// Resolve the (arity-one) motive of an intrinsic eliminator. An elided motive checked against an expected type and matched on a *bare variable* scrutinee is synthesised dependent — abstracting that variable out of the expected type — so each arm checks against the goal specialised at its constructor (`0` / `pred + 1`, `x[]` / `head :: tail`, `false` / `true`, ...) rather than the unspecialised expected a constant motive would leave.
///
/// This complements `solve`'s occurrence abstraction (`convert.rs`), which already derives the dependent motive for a *compound* scrutinee: there the scrutinee is a clean abstraction subject in the motive metavar's spine, whereas a bare variable coincides with its own context binder — a duplicated, non-invertible spine entry that `solve` must leave alone. So anything but an elided-checking-mode-bare-variable match keeps the metavar path verbatim, letting `solve` (or the constant motive) do its job exactly as before.
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

/// Refuse a fold whose motive reaches its scrutinee other than through the binder it declares.
///
/// The induction hypothesis is assumed at the motive opened at the tail *inside* the cons arm, where `refine_head` has the scrutinee reducing to the cons value. A captured occurrence reduces with it, so the hypothesis would be typed at the arm's own goal: `match n : (_) => Eq(n, 0) | 0 => refl | k + 1; ih => ih end` then proves `Eq(n, 0)` for every `n`. The kernel refuses the same shape by the same test (`check_free_monoid`); this is the elaborator's copy, so the refusal is reported where the motive was written. A local defined in the frame — a `let` alias of the scrutinee, or a binder an enclosing arm refined — is read through its definition, since the reducer will read it the same way.
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
                    out.insert(name.clone());
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
            seen.insert(name.clone())
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

fn elaborate_nat_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_intrinsic_head(context, head, IntrinsicHead::Nat)?;

    // Everything below opens the *rebuilt* motive: insertion saturates applications during elaboration, and a lowered (under-applied) motive body reaching the reducer would open a telescope at the wrong arity.
    let motive = resolve_intrinsic_motive(
        context,
        &Subterm::Intrinsic(Intrinsic::NatType).into(),
        &head_elaborated,
        motive,
        &mode,
    )?;
    refuse_captured_scrutinee(context, &motive, &head_elaborated)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    // Refine the scrutinee to its constructor in each arm (as `Bool`/`Switch` already do): a context hypothesis whose type mentions the scrutinee then reduces at the arm's value, so a dependent match needs no hand-written convoy to carry it across the eliminator.
    let zero_value: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))).into();
    let zero_elaborated = context.with_frame(|context| {
        refine_head(context, &head_elaborated, &zero_value)?;
        check(context, zero_case, motive.open(&[&zero_value]))
    })?;

    let pred_label = context.fresh(succ_case.first_hint());
    let ih_label = context.fresh(succ_case.second_hint());

    let succ_body = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Intrinsic(Intrinsic::NatType).into());
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&pred_label)]));

        let succ_value: Term = Subterm::Intrinsic(Intrinsic::nat_add(
            Term::free_var(&pred_label),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        ))
        .into();
        refine_head(context, &head_elaborated, &succ_value)?;

        check(
            context,
            &succ_case.open(&[&Term::free_var(&pred_label), &Term::free_var(&ih_label)]),
            motive.open(&[&succ_value]),
        )
    })?;

    let succ_elaborated = Scope::close(Two, &[&pred_label, &ih_label], succ_body);

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result: MatchResult::Family(motive),
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
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `List` carries an element type the eliminator must read off the scrutinee (unlike `Nat`, whose carrier is parameterless) — infer the head, then demand its type is `List(elem)`.
    let (head_elaborated, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;
    let elem = match &*head_type {
        Subterm::Intrinsic(Intrinsic::ListType(elem)) => elem.clone(),
        _ => return Err(Error::not_list_type(head_type)),
    };

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = resolve_intrinsic_motive(context, &head_type, &head_elaborated, motive, &mode)?;
    refuse_captured_scrutinee(context, &motive, &head_elaborated)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    // Refine the scrutinee to its value in each arm (as `Nat`/`Bool`/`Switch` already do), so a hypothesis whose type mentions the scrutinee reduces at the arm's value without a hand-written convoy.
    let empty_value: Term = Subterm::Intrinsic(Intrinsic::List {
        element: elem.clone(),
        items: vec![],
    })
    .into();
    let empty_elaborated = context.with_frame(|context| {
        refine_head(context, &head_elaborated, &empty_value)?;
        check(context, empty_case, motive.open(&[&empty_value]))
    })?;

    let head_label = context.fresh(cons_case.first_hint());
    let tail_label = context.fresh(cons_case.second_hint());
    let ih_label = context.fresh(cons_case.third_hint());

    let cons_body = context.with_frame(|context| {
        context.assume(&head_label, &elem);
        context.assume(&tail_label, &head_type);
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));

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
        refine_head(context, &head_elaborated, &cons_value)?;

        check(
            context,
            &cons_case.open(&[
                &Term::free_var(&head_label),
                &Term::free_var(&tail_label),
                &Term::free_var(&ih_label),
            ]),
            motive.open(&[&cons_value]),
        )
    })?;

    let cons_elaborated = Scope::close(Three, &[&head_label, &tail_label, &ih_label], cons_body);

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result: MatchResult::Family(motive),
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
    head: &Term,
    motive: &Scope<Many>,
    cases: (&Term, &Scope<Three>),
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (empty_case, cons_case) = cases;
    // `Bin` is a parameterless carrier (like `Nat`/`Bool`), so the scrutinee's type is just `Bin` — no element type to read off the head as `List` needs.
    let (head_elaborated, head_type) =
        elaborate_intrinsic_head(context, head, IntrinsicHead::Bin(grain))?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = resolve_intrinsic_motive(context, &head_type, &head_elaborated, motive, &mode)?;
    refuse_captured_scrutinee(context, &motive, &head_elaborated)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    // Refine the scrutinee to its value in each arm (as `Nat`/`Bool`/`Switch` already do): a context hypothesis whose type mentions the scrutinee then reduces at the arm's value, so a dependent match needs no hand-written convoy to carry it across the eliminator.
    let empty_value: Term = Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty())).into();
    let empty_elaborated = context.with_frame(|context| {
        refine_head(context, &head_elaborated, &empty_value)?;
        check(context, empty_case, motive.open(&[&empty_value]))
    })?;

    let head_label = context.fresh(cons_case.first_hint());
    let tail_label = context.fresh(cons_case.second_hint());
    let ih_label = context.fresh(cons_case.third_hint());

    let cons_body = context.with_frame(|context| {
        let atom_type: Term = Subterm::Intrinsic(match grain {
            Grain::B => Intrinsic::BoolType,
            Grain::X => Intrinsic::ByteType,
        })
        .into();
        context.assume(&head_label, &atom_type);
        context.assume(&tail_label, &head_type);
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));

        // The cons value `head :: tail`, encoded as the monoid operation on the singleton `[head]` and the tail. A `Bits`/`Bytes` literal holds only concrete bytes, so the singleton of the symbolic byte `head` is `append(x[], head)` (an atom appended to the empty packed sequence), not a literal run.
        let singleton: Term = Subterm::Intrinsic(Intrinsic::BinAppend {
            grain,
            bin: Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty())).into(),
            element: Term::free_var(&head_label),
        })
        .into();
        let cons_value: Term = Subterm::Intrinsic(Intrinsic::BinConcat {
            grain,
            operands: vec![singleton, Term::free_var(&tail_label)],
        })
        .into();
        refine_head(context, &head_elaborated, &cons_value)?;

        check(
            context,
            &cons_case.open(&[
                &Term::free_var(&head_label),
                &Term::free_var(&tail_label),
                &Term::free_var(&ih_label),
            ]),
            motive.open(&[&cons_value]),
        )
    })?;

    let cons_elaborated = Scope::close(Three, &[&head_label, &tail_label, &ih_label], cons_body);

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        result: MatchResult::Family(motive),
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

/// The result of a `Bool` or `Switch` elimination, which binds no induction hypothesis: an elided motive over a variable scrutinee checked against an expected type takes that type as its ambient result, and anything else resolves as an intrinsic motive. A fold keeps the family whatever its shape — its hypothesis is typed at the motive opened at the tail.
fn resolve_intrinsic_result(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    mode: &Mode,
) -> Result<MatchResult, Error> {
    if let Mode::Check(expected) = mode
        && is_elided_motive(motive)
        && is_variable(head)
    {
        return Ok(MatchResult::Ambient(expected.clone()));
    }
    resolve_intrinsic_motive(context, head_type, head, motive, mode).map(MatchResult::Family)
}

fn elaborate_switch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &[(Natural, Term)],
    default: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_intrinsic_head(context, head, IntrinsicHead::Nat)?;

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
        let body = context.with_frame(|context| {
            let literal: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(n.clone()))).into();
            refine_head(context, &head_elaborated, &literal)?;
            if result.ambient().is_some() {
                shadow_specialized(context, &head_elaborated, &[], &[], &literal);
            }
            check(
                context,
                body,
                result.at(&head_elaborated, &[], &[], &literal),
            )
        })?;
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
    let Match {
        head,
        result,
        cases,
    } = m;
    // A rebuilt match coming back through re-elaboration at its ambient goal: the goal *is* the expected type it was elaborated against, so it re-elaborates as the elided motive it came from, checked against that goal.
    let mut mode = mode;
    let elided;
    let motive = match result {
        MatchResult::Family(motive) => motive,
        MatchResult::Ambient(goal) => {
            if matches!(mode, Mode::Infer) {
                mode = Mode::Check(goal.clone());
            }
            elided = Term::match_motive_written(Term::hole(context.mint_metavar()));
            &elided
        }
    };

    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => elaborate_bool_match(context, head, motive, false_case, true_case, term, mode),
        Cases::Switch { cases, default } => {
            elaborate_switch(context, head, motive, cases, default, term, mode)
        }
        Cases::Induct { cases, default } => elaborate_induct_match(
            context,
            InductMatchInput {
                head,
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
        } => elaborate_nat_match(context, head, motive, empty_case, cons_case, term, mode),
        Cases::FreeMonoid {
            carrier:
                Carrier::List {
                    empty_case,
                    cons_case,
                    ..
                },
        } => elaborate_list_match(context, head, motive, empty_case, cons_case, term, mode),
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
            head,
            motive,
            (empty_case, cons_case),
            term,
            mode,
        ),
    }
}

struct InductMatchInput<'a> {
    head: &'a Term,
    motive: &'a Scope<Many>,
    cases: &'a [(Atom, InductArm)],
    default: Option<&'a Term>,
    term: &'a Term,
}

fn elaborate_bool_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_intrinsic_head(context, head, IntrinsicHead::Bool)?;

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

    let false_elaborated = context.with_frame(|context| {
        let literal: Term = Subterm::Intrinsic(Intrinsic::Bool(false)).into();
        refine_head(context, &head_elaborated, &literal)?;
        if result.ambient().is_some() {
            shadow_specialized(context, &head_elaborated, &[], &[], &literal);
        }
        check(
            context,
            false_case,
            result.at(&head_elaborated, &[], &[], &literal),
        )
    })?;

    let true_elaborated = context.with_frame(|context| {
        let literal: Term = Subterm::Intrinsic(Intrinsic::Bool(true)).into();
        refine_head(context, &head_elaborated, &literal)?;
        if result.ambient().is_some() {
            shadow_specialized(context, &head_elaborated, &[], &[], &literal);
        }
        check(
            context,
            true_case,
            result.at(&head_elaborated, &[], &[], &literal),
        )
    })?;

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
    let mut telescope = payload;
    let terminal = loop {
        match telescope {
            Telescope::Cons(ty, rest) => {
                let name = context.fresh(rest.first_hint());
                telescope = rest.open(&[&Term::free_var(&name)]);
                binders.push((name, ty));
            }
            Telescope::Done(terminal) => break *terminal,
        }
    };

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
        head,
        motive,
        cases,
        default,
        term,
    } = input;

    // A match with no arms is a vacuous elimination — either of an empty inductive (`False`) or of one whose every constructor inversion-clashes at the scrutinee's indices. Such a match compiles to unreachable code that never inspects the scrutinee. A proof/type scrutinee carries no runtime content (sort-driven erasure), so an empty match discharging an erased witness of falsity into a relevant result is sound without a usage discipline.
    let (head_elaborated, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    let (name, universes, params, indices) = match &*head_type {
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            indices,
        }) => (
            name.clone(),
            universes.clone(),
            params.clone(),
            indices.clone(),
        ),
        other => return Err(Error::not_a_induct_type(other.clone())),
    };

    // Reducing the scrutinee type to weak-head normal form leaves its index *arguments* untouched, so an index that is an outer-arm key (`s` refined to `Scan/bad()` by an enclosing match) still reads as the bare variable. Reduce each index in the current (refined) context so inversion sees the forced value and pins arm binders against it, rather than refusing it as Rung B's key-shaped territory.
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

    // An elided motive over a variable scrutinee, checked against an expected type, takes that type as its *ambient* result: each arm is checked against the expected type with the scrutinee and its variable indices standing for the arm's case, and the match's result is the expected type itself. That is what a hand-written convoy used to arrange, and what the elaborator used to synthesize one for; the ambient form needs no family to close, so a hypothesis whose type mentions the scrutinee rides along unchanged — see `MatchResult::Ambient`. Anything else — a written motive, inference mode, an expression scrutinee, whose occurrences in the goal only `solve`'s occurrence abstraction can find — is a family, checked or solved as before.
    let result = match &mode {
        Mode::Check(expected) if is_elided_motive(motive) && is_variable(&head_elaborated) => {
            MatchResult::Ambient(expected.clone())
        }
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

    // Every written arm must name a constructor; coverage is decided per constructor below — a missing arm is legal iff inversion proves it impossible (Rung C).
    if let Some(tag) = cases
        .iter()
        .map(|(tag, _)| tag)
        .find(|tag| !induct_decl.declares(tag))
    {
        return Err(Error::unknown_match_constructor(
            name.symbol(),
            tag.to_string(),
        ));
    }

    // Built by walking the *declaration* order, not the written order, so the elaborated arm sequence is canonical: two matches differing only in how their arms were written produce the same term.
    let mut cases_elaborated = Vec::new();
    for tag in induct_decl.constructor_order() {
        let Some((_, scope)) = cases.iter().find(|(candidate, _)| candidate == tag) else {
            // A catch-all default covers every un-enumerated constructor, so a missing arm needs neither the unindexed-completeness check nor Rung-C inversion — the default is checked once, below.
            if default.is_some() {
                continue;
            }

            // An unindexed inductive has nothing to invert: every arm is reachable and a missing one is plainly missing.
            if actual_indices.is_empty() {
                return Err(Error::match_case_missing(name.symbol(), tag.to_string()));
            }

            // Rung C — checker-verified omission: a missing arm is accepted iff first-order inversion of the scrutinee's actual indices against this case's targets finds a *definite* clash. The arm is then pruned (erase fills its slot with an unreachable body); anything short of definite keeps the arm mandatory.
            let telescope = induct_decl
                .instantiate(tag, &params)
                .expect("constructor instantiates at its inductive's parameters");

            let labels = (0..telescope.len())
                .map(|_| context.fresh(None))
                .collect::<Vec<_>>();

            // The case is opened as a written arm's is, its binders assumed, because inversion reconciles a binder forced twice *at its type*: `refl(@z) : (z, z)` against `Eq(false, true)` is decided by what `false` and `true` are at `Bool`, and a binder that is only a name has no type to be asked at.
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
                    position: position + 1,
                    expected: *canonical,
                    written: *written,
                });
            }
        }

        // Open the telescope with fresh names paralleling the arm's binder labels; each binder is assumed at its declared (dependent) type.
        let hints = scope
            .hint_iter()
            .map(|l| l.map(str::to_string))
            .collect::<Vec<_>>();
        let labels = hints
            .iter()
            .map(|hint| context.fresh(hint.as_deref()))
            .collect::<Vec<_>>();
        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

        let body_elaborated = context.with_frame(|context| {
            let ix_c = assume_payload(context, telescope, &labels);

            // Refinement propagates `head := ctor_val` to other occurrences of the scrutinee in the arm body; the binder types themselves came from the telescope above. Built at the scrutinee's own universe levels, because this value outlives the refinement: the motive is opened on it, so it is what a metavariable in an arm's expected type is solved to — the `@z` of an `Eq/refl()` against `Eq(len(xs), len(xs))` — and a level-less occurrence of a polymorphic family zonks into the definition, where the arity check (or, for a prelude family it cannot see, the kernel) refuses it.
            let ctor_val = Term::variant_at(
                name.clone(),
                universes.clone(),
                params.clone(),
                tag.clone(),
                vars.clone(),
            );
            refine_head(context, &head_elaborated, &ctor_val)?;

            // Rung B — definitional learning: a scrutinee index that is a `Var` reduces, inside this arm, to the case's target index — the same counterfactual, frame-scoped move as `head := ctor_val`. A constructor index records an inert entry (`refine_head`); the inverter below pins the arm binders the other way. Refinements never justify the typing (the motive application does); they are convertibility aids, so context hypotheses mentioning the key reduce at the arm's index.
            for (actual, target) in actual_indices.iter().zip(&ix_c) {
                refine_head(context, actual, target)?;
            }

            // Rung C — inversion, arm side: a scrutinee index in constructor form pins arm binders to forced values (`m + 1 ~ n + 1` pins `m := n`), registered as the same frame-scoped reducts. A definite clash here means the arm is unreachable; it was written, so it is simply checked as is.
            if let Invert::Solved(solutions) =
                invert_indices(context, &actual_indices, &ix_c, &labels)?
            {
                for (label, solution) in solutions {
                    context.refine(&label, &solution);
                }
            }

            // The result at this case: a family's index binders take the case's target indices and its scrutinee binder the constructed value; an ambient goal has the case's targets and value substituted for its variable indices and scrutinee.
            let expected = result.at(&head_elaborated, &actual_indices, &ix_c, &ctor_val);
            if result.ambient().is_some() {
                shadow_specialized(context, &head_elaborated, &actual_indices, &ix_c, &ctor_val);
            }

            let var_refs = vars.iter().collect::<Vec<_>>();
            check(context, &scope.open(&var_refs), expected)
        })?;

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

/// Re-assume, at its specialized type, every local whose type mentions a variable the case substitutes for — the elaborator's copy of the kernel's `shadow`. The arm's refinements already make such a type *reduce* at the case, which is enough for the arm body's own conversions, but not for a metavariable solution parked and retried outside the frame: `z : Sizes(s)` used as `(z).0` under `s := node(a, b)` has to be a tuple where the solution is checked, which the shadow states outright. The substituted variables' own entries are left alone, exactly as the kernel leaves them.
/// Open a case's instantiated telescope under `labels`, each assumed at its declared (dependent) type, and answer the index targets the signature terminates in, stated over those binders. The caller holds the frame the assumptions live in.
///
/// A written arm and an omitted one open their case the same way, so inversion is put the same question about both — which is also the question the kernel puts, its callers reaching the unifier through a payload open that assumes every binder.
fn assume_payload(
    context: &mut Context,
    mut telescope: Telescope<Vec<Term>>,
    labels: &[Free],
) -> Vec<Term> {
    for label in labels {
        match telescope {
            Telescope::Cons(type_, rest) => {
                context.assume(label, &type_);
                telescope = rest.open(&[&Term::free_var(label)]);
            }
            Telescope::Done(_) => unreachable!("a case's binders parallel its telescope"),
        }
    }

    match telescope {
        Telescope::Done(targets) => *targets,
        Telescope::Cons(..) => unreachable!("a case's binders parallel its telescope"),
    }
}

fn shadow_specialized(
    context: &mut Context,
    head: &Term,
    actual_indices: &[Term],
    case_indices: &[Term],
    case_value: &Term,
) {
    let substitution = case_substitution(head, actual_indices, case_indices, case_value);
    if substitution.is_empty() {
        return;
    }
    let binders = substitution
        .iter()
        .map(|(name, _)| *name)
        .collect::<Vec<_>>();
    let values = substitution
        .iter()
        .map(|(_, value)| *value)
        .collect::<Vec<_>>();
    let locals = context.locals().to_vec();
    for (name, type_) in locals {
        if binders.contains(&&name) || !binders.iter().any(|binder| type_.mentions_free(binder)) {
            continue;
        }
        let specialized = Scope::close(Many(binders.len()), &binders, type_).open(&values);
        context.assume(&name, &specialized);
    }
}

/// Whether a scrutinee is a variable with a binder — the one shape a case can be substituted for, and so the precondition of an ambient result.
fn is_variable(head: &Term) -> bool {
    matches!(&**head, Subterm::Var(var) if var.as_free().is_some())
}

/// Whether a motive scope is the lowering's elided form — a bare metavariable body (`match s | ..` or the explicit hole `match s : _ | ..`), as opposed to a user-written constant or scrutinee-binding motive. Only the elided form takes the ambient result; everything else is taken verbatim.
fn is_elided_motive(motive: &Scope<Many>) -> bool {
    // A silent hole only: a written `?` motive is a user-written motive the author is asking about, checked against the eliminator's motive type like any other and reported by zonk (`MetavarOrigin` states the rule).
    matches!(&**motive.body(), Subterm::Metavar(metavar) if metavar.is_hole())
}
