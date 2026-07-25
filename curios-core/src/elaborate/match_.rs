use {
    super::{Context, Error, InductType, Mode, check, elaborate, expect},
    crate::{
        Atom, Carrier, Cases, InductArm, InductDecl, Invert, Many, Match, MotiveShape, Nat, Prim,
        PrimHead, Scope, Subterm, Telescope, Term, Three, Two, case_target_indices, check_motive,
        check_prim_head, invert_indices, is_prop, reduce_with, refine_head,
    },
    curios_base::{Grain, PackedBin},
    std::collections::{BTreeMap, BTreeSet},
};

/// Infer and rebuild a match scrutinee, requiring its reduced type to be the
/// given primitive type. The authoritative analogue of `expect_prim_head` (kept
/// for `erase`): it returns the rebuilt head alongside its reduced type.
fn elaborate_prim_head(
    context: &mut Context,
    head: &Term,
    expected: PrimHead,
) -> Result<(Term, Term), Error> {
    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    check_prim_head(expected, head_type).map(|head_type| (head, head_type))
}

/// When a match is elaborated in checking mode, solve its motive against the
/// expected type *before* the arms are checked. An omitted motive is a constant
/// scope wrapping a fresh metavar (`text::into_core::match_compile`'s `motive_parts`), so `motive.open`
/// is that bare metavar and this pins it to `expected` up front — checking-only
/// arms (tuples, constructors) then see a concrete target instead of an
/// unsolved hole, and a result mentioning an enclosing type variable is taken
/// straight from `expected` rather than inverted out of an arm. For an explicit
/// motive it is the same consistency check that the `Check` turnaround would
/// otherwise run post-hoc on the match's type (`elaborate_subterm`), only earlier.
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

/// Resolve the (arity-one) motive of a primitive eliminator. An elided motive
/// checked against an expected type and matched on a *bare variable* scrutinee
/// is synthesised dependent — abstracting that variable out of the expected
/// type — so each arm checks against the goal specialised at its constructor
/// (`0` / `pred + 1`, `\\` / `head :: tail`, `false` / `true`, ...) rather than
/// the unspecialised expected a constant motive would leave.
///
/// This complements `solve`'s occurrence abstraction (`convert.rs`), which
/// already derives the dependent motive for a *compound* scrutinee: there the
/// scrutinee is a clean abstraction subject in the motive metavar's spine,
/// whereas a bare variable coincides with its own context binder — a duplicated,
/// non-invertible spine entry that `solve` must leave alone. So anything but an
/// elided-checking-mode-bare-variable match keeps the metavar path verbatim,
/// letting `solve` (or the constant motive) do its job exactly as before.
fn resolve_prim_motive(
    context: &mut Context,
    head_type: &Term,
    head: &Term,
    motive: &Scope<Many>,
    mode: &Mode,
) -> Result<Scope<Many>, Error> {
    let shape = MotiveShape::Prim(head_type);

    if let (Mode::Check(expected), Subterm::Var(var)) = (mode, &**head)
        && is_elided_motive(motive)
        && let Some(label) = var.as_free()
    {
        let synthesized = Scope::close(Many(1), &[label], expected.clone());
        return check_motive(context, &shape, &synthesized);
    }

    check_motive(context, &shape, motive)
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
    let (head_elaborated, _) = elaborate_prim_head(context, head, PrimHead::Nat)?;

    // Everything below opens the *rebuilt* motive: insertion saturates
    // applications during elaboration, and a lowered (under-applied) motive
    // body reaching the reducer would open a telescope at the wrong arity.
    let motive = resolve_prim_motive(
        context,
        &Subterm::Prim(Prim::NatType).into(),
        &head_elaborated,
        motive,
        &mode,
    )?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    // Refine the scrutinee to its constructor in each arm (as `Bool`/`Switch`
    // already do): a context hypothesis whose type mentions the scrutinee then
    // reduces at the arm's value, so a dependent match needs no hand-written
    // convoy to carry it across the eliminator.
    let zero_value: Term = Subterm::Prim(Prim::Nat(Nat::new(0usize))).into();
    let zero_elaborated = context.with_frame(|context| {
        refine_head(context, &head_elaborated, &zero_value)?;
        check(context, zero_case, motive.open(&[&zero_value]))
    })?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let succ_body = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&pred_label)]));

        let succ_value: Term = Subterm::Prim(Prim::nat_add(
            Term::free_var(&pred_label),
            Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        ))
        .into();
        refine_head(context, &head_elaborated, &succ_value)?;

        check(
            context,
            &succ_case.open(&[&Term::free_var(&pred_label), &Term::free_var(&ih_label)]),
            motive.open(&[&succ_value]),
        )
    })?;

    let succ_elaborated = Scope::close(Two, &[pred_label.as_str(), ih_label.as_str()], succ_body);

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        // `Nat` is the free monoid on one payload-less generator: its cons arm binds
        // just (predecessor, ih), so the carrier is `Nat` and the head is absent.
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

fn elaborate_lst_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Lst` carries an element type the eliminator must read off the scrutinee
    // (unlike `Nat`, whose carrier is parameterless) — infer the head, then
    // demand its type is `Lst(elem)`.
    let (head_elaborated, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;
    let elem = match &*head_type {
        Subterm::Prim(Prim::LstType(elem)) => elem.clone(),
        _ => return Err(Error::not_lst_type(head_type)),
    };

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = resolve_prim_motive(context, &head_type, &head_elaborated, motive, &mode)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    // Refine the scrutinee to its value in each arm (as `Nat`/`Bool`/`Switch`
    // already do), so a hypothesis whose type mentions the scrutinee reduces at
    // the arm's value without a hand-written convoy.
    let empty_value: Term = Subterm::Prim(Prim::Lst(vec![])).into();
    let empty_elaborated = context.with_frame(|context| {
        refine_head(context, &head_elaborated, &empty_value)?;
        check(context, empty_case, motive.open(&[&empty_value]))
    })?;

    let head_label = context.fresh(cons_case.first_label());
    let tail_label = context.fresh(cons_case.second_label());
    let ih_label = context.fresh(cons_case.third_label());

    let cons_body = context.with_frame(|context| {
        context.assume(&head_label, &elem);
        context.assume(&tail_label, &head_type);
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));

        // The cons value `head :: tail`, encoded as the monoid operation on a
        // singleton and the tail (no separate prepend primitive).
        let cons_value: Term = Subterm::Prim(Prim::LstConcat(
            elem.clone(),
            vec![
                Subterm::Prim(Prim::Lst(vec![Term::free_var(&head_label)])).into(),
                Term::free_var(&tail_label),
            ],
        ))
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

    let cons_elaborated = Scope::close(
        Three,
        &[head_label.as_str(), tail_label.as_str(), ih_label.as_str()],
        cons_body,
    );

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        cases: Cases::FreeMonoid {
            carrier: Carrier::Lst {
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
    // `Bin` is a parameterless carrier (like `Nat`/`Bool`), so the scrutinee's type
    // is just `Bin` — no element type to read off the head as `Lst` needs.
    let (head_elaborated, head_type) = elaborate_prim_head(context, head, PrimHead::Bin(grain))?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = resolve_prim_motive(context, &head_type, &head_elaborated, motive, &mode)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    // Refine the scrutinee to its value in each arm (as `Nat`/`Bool`/`Switch`
    // already do): a context hypothesis whose type mentions the scrutinee then
    // reduces at the arm's value, so a dependent match needs no hand-written
    // convoy to carry it across the eliminator.
    let empty_value: Term = Subterm::Prim(Prim::Bin(grain, PackedBin::empty())).into();
    let empty_elaborated = context.with_frame(|context| {
        refine_head(context, &head_elaborated, &empty_value)?;
        check(context, empty_case, motive.open(&[&empty_value]))
    })?;

    let head_label = context.fresh(cons_case.first_label());
    let tail_label = context.fresh(cons_case.second_label());
    let ih_label = context.fresh(cons_case.third_label());

    let cons_body = context.with_frame(|context| {
        let atom_type: Term = Subterm::Prim(match grain {
            Grain::B => Prim::BoolType,
            Grain::X => Prim::ByteType,
        })
        .into();
        context.assume(&head_label, &atom_type);
        context.assume(&tail_label, &head_type);
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));

        // The cons value `head :: tail`, encoded as the monoid operation on the
        // singleton `[head]` and the tail. A `Bits`/`Bytes` literal holds only concrete
        // bytes, so the singleton of the symbolic byte `head` is `append(\\, head)`
        // (an atom appended to the empty packed sequence), not a literal run.
        let singleton: Term = Subterm::Prim(Prim::BinAppend(
            grain,
            Subterm::Prim(Prim::Bin(grain, PackedBin::empty())).into(),
            Term::free_var(&head_label),
        ))
        .into();
        let cons_value: Term = Subterm::Prim(Prim::BinConcat(
            grain,
            vec![singleton, Term::free_var(&tail_label)],
        ))
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

    let cons_elaborated = Scope::close(
        Three,
        &[head_label.as_str(), tail_label.as_str(), ih_label.as_str()],
        cons_body,
    );

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
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

fn elaborate_switch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, PrimHead::Nat)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = resolve_prim_motive(
        context,
        &Subterm::Prim(Prim::NatType).into(),
        &head_elaborated,
        motive,
        &mode,
    )?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let mut cases_elaborated = BTreeMap::new();
    for (n, body) in cases {
        let body = context.with_frame(|context| {
            refine_head(
                context,
                &head_elaborated,
                &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
            )?;
            check(
                context,
                body,
                motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]),
            )
        })?;
        cases_elaborated.insert(*n, body);
    }

    let default_elaborated = check(context, default, motive.open(&[&head_elaborated]))?;

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
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
        motive,
        cases,
    } = m;

    match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => elaborate_bln_match(context, head, motive, false_case, true_case, term, mode),
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
                Carrier::Lst {
                    empty_case,
                    cons_case,
                    ..
                },
        } => elaborate_lst_match(context, head, motive, empty_case, cons_case, term, mode),
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
    cases: &'a BTreeMap<Atom, InductArm>,
    default: Option<&'a Term>,
    term: &'a Term,
}

fn elaborate_bln_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, PrimHead::Bool)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = resolve_prim_motive(
        context,
        &Subterm::Prim(Prim::BoolType).into(),
        &head_elaborated,
        motive,
        &mode,
    )?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let false_elaborated = context.with_frame(|context| {
        refine_head(
            context,
            &head_elaborated,
            &Subterm::Prim(Prim::Bool(false)).into(),
        )?;
        check(
            context,
            false_case,
            motive.open(&[&Subterm::Prim(Prim::Bool(false)).into()]),
        )
    })?;

    let true_elaborated = context.with_frame(|context| {
        refine_head(
            context,
            &head_elaborated,
            &Subterm::Prim(Prim::Bool(true)).into(),
        )?;
        check(
            context,
            true_case,
            motive.open(&[&Subterm::Prim(Prim::Bool(true)).into()]),
        )
    })?;

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        cases: Cases::Bool {
            false_case: false_elaborated,
            true_case: true_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// Whether a single-constructor proposition admits large elimination: every
/// payload binder must be non-informative — a proposition itself, or *forced* by
/// appearing in the constructor's index targets (recovered from the scrutinee's
/// type, as `Eq`'s `refl(z) : (z, z)` recovers `z` from its indices).
fn singleton_eliminable(
    context: &mut Context,
    induct_decl: &InductDecl,
    tag: &Atom,
    params: &[Term],
) -> Result<bool, Error> {
    let Some(payload) = induct_decl.instantiate(tag, params) else {
        return Ok(false);
    };

    // Open the payload telescope under fresh binders, collecting each binder's
    // (name, type) and the terminal whose indices are the constructor's targets.
    let mut binders: Vec<(String, Term)> = Vec::new();
    let mut telescope = payload;
    let terminal = loop {
        match telescope {
            Telescope::Cons(ty, rest) => {
                let name = context.fresh(rest.first_label());
                telescope = rest.open(&[&Term::free_var(&name)]);
                binders.push((name, ty));
            }
            Telescope::Done(terminal) => break *terminal,
        }
    };

    // A binder is forced iff it occurs in the terminal's index expressions.
    let forced = terminal.free_vars();
    for (name, ty) in &binders {
        if !forced.contains(name) && !is_prop(context, ty)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// The primitive eliminator's typing rule. Arm binders are
/// typed directly from the constructor's registry telescope instantiated at
/// the scrutinee type's parameters — no projections from a stuck payload —
/// and each arm's binder count is statically checked against that telescope.
///
/// The motive binds the scrutinee's indices and then the scrutinee, whatever
/// its body does with them: each arm checks against the motive at *that case's*
/// target indices, the whole match types at the scrutinee's *actual* indices,
/// and a catch-all default — which binds nothing and refines no index — checks
/// at the actual indices too.
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

    // A match with no arms is a vacuous elimination — either of an empty inductive
    // (`False`) or of one whose every constructor inversion-clashes at the
    // scrutinee's indices. Such a match compiles to unreachable code that never
    // inspects the scrutinee. A proof/type scrutinee carries no runtime content
    // (sort-driven erasure), so an empty match discharging an erased witness of
    // falsity into a relevant result is sound without a usage discipline.
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

    // Reducing the scrutinee type to weak-head normal form leaves its index
    // *arguments* untouched, so an index that is an outer-arm key (`s` refined
    // to `Scan/bad()` by an enclosing match) still reads as the bare variable.
    // Reduce each index in the current (refined) context so inversion sees the
    // forced value and pins arm binders against it, rather than refusing it as
    // Rung B's key-shaped territory.
    let actual_indices = indices
        .iter()
        .map(|index| reduce_with(context, index))
        .collect::<Result<Vec<_>, _>>()?;

    let Some(induct_decl) = context.induct_decl(&name).cloned() else {
        return Err(Error::unbound_variable(Term::free_var(&name)));
    };
    let induct_decl = context.instantiate_induct_decl_at(&induct_decl, &universes)?;

    // Opacity covers every eliminator, including defaults and vacuous or
    // inversion-discharged matches. Check before motives, coverage, or index
    // refinement can reveal representation facts.
    if !induct_decl.rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(&induct_decl.module))
    {
        return Err(Error::private_representation(name));
    }

    // The motive abstracts the index telescope instantiated at the scrutinee's
    // actual parameters, then the scrutinee.
    let shape = MotiveShape::Induct {
        name: &name,
        universes: &universes,
        params: &params,
        indices: induct_decl.indices.clone().open_params(&params),
    };

    // An elided motive (the lowering's bare metavar) checked against an
    // expected type is filled in by dependent-motive synthesis: the arms are
    // then checked against the expected type specialised at each
    // constructor, exactly as a hand-written convoy motive would, and the
    // match's result is a concrete type rather than a metavar that would
    // stall the large-elimination guard. Outside checking mode there is no
    // expected type to abstract, so the metavar stays and is solved by
    // unifying the arms (`check_motive`).
    //
    // A defaulted match (`| _ =>` catch-all / bind-arm fallthrough) keeps the
    // plain path: its default stands in for constructors whose target indices
    // are unknown here, so there is nothing for per-constructor specialisation
    // to buy, and unifying the arms is what the surface form asks for.
    let (motive_elaborated, generalized) = match &mode {
        Mode::Check(expected) if is_elided_motive(motive) && default.is_none() => {
            synthesize_induct_motive(
                context,
                &shape,
                &induct_decl,
                &actual_indices,
                &head_elaborated,
                expected,
            )?
        }
        _ => (check_motive(context, &shape, motive)?, vec![]),
    };

    // The match's own type: the motive at the scrutinee's actual indices and
    // the scrutinee itself. Opened from the *rebuilt* motive, as in
    // `elaborate_nat_match`. When hypotheses were generalized this is a Π over
    // them (`elim_type`); the eliminator is then *applied* to the originals
    // below, recovering the original goal.
    let result_refs = actual_indices
        .iter()
        .chain([&head_elaborated])
        .collect::<Vec<_>>();
    let elim_type = motive_elaborated.open(&result_refs);

    // The type this whole expression has. With no generalization it is the
    // eliminator's own type; with generalization the eliminator is a function we
    // apply to the original hypotheses, so the expression's type is the original
    // expected goal (generalization only happens in checking mode).
    let result_type = match &mode {
        Mode::Check(expected) if !generalized.is_empty() => expected.clone(),
        _ => elim_type.clone(),
    };

    // The seed (`seed_motive`'s job, generalized over the pattern binders):
    // in checking mode, pin the motive — a bare metavar when elided — to the
    // expected type before the arms are checked. Skipped under generalization:
    // `result_type` is already the expected goal verbatim, and `elim_type` (a Π)
    // is deliberately *not* the expected type.
    if let Mode::Check(expected) = &mode
        && generalized.is_empty()
    {
        expect(context, term, &result_type, expected)?;
    }

    // Large-elimination guard: a strict proposition may not be eliminated into a
    // relevant (data) result — observing which inhabitant it was would break
    // proof irrelevance. Permitted only by *empty* elimination (no constructors)
    // or *singleton* elimination (one constructor whose payload is entirely
    // non-informative: each binder a proposition, or forced by the indices).
    if is_prop(context, &head_type)? && !is_prop(context, &result_type)? {
        // Count the *covered* constructors (the written arms), not the
        // inductive's global ones: index inversion may leave only one applicable
        // — a `cont`-scan `Utf8` admits only `more` — making an otherwise
        // multi-constructor proposition singleton in this context. (An
        // under-covered match is still caught by the coverage check below.)
        // A catch-all default stands in for more than one constructor — a
        // relevant result branching on an erased scrutinee's tag is exactly
        // what this guard forbids — so it is never permitted here. (A
        // prop-into-prop defaulted match never reaches this branch.)
        let permitted = default.is_none()
            && match cases.len() {
                0 => true,
                1 => {
                    let tag = cases.keys().next().expect("one covered constructor");
                    singleton_eliminable(context, &induct_decl, tag, &params)?
                }
                _ => false,
            };
        if !permitted {
            return Err(Error::large_elim_of_prop(name.clone()));
        }
    }

    // Every written arm must name a constructor; coverage is decided per
    // constructor below — a missing arm is legal iff inversion proves it
    // impossible (Rung C).
    if let Some(tag) = cases
        .keys()
        .find(|tag| !induct_decl.constructors.contains_key(*tag))
    {
        return Err(Error::unknown_match_constructor(
            name.clone(),
            tag.to_string(),
        ));
    }

    let mut cases_elaborated = BTreeMap::new();
    for tag in induct_decl.constructors.keys() {
        let Some(scope) = cases.get(tag) else {
            // A catch-all default covers every un-enumerated constructor, so a
            // missing arm needs neither the unindexed-completeness check nor
            // Rung-C inversion — the default is checked once, below.
            if default.is_some() {
                continue;
            }

            // An unindexed inductive has nothing to invert: every arm is
            // reachable and a missing one is plainly missing.
            if actual_indices.is_empty() {
                return Err(Error::match_case_missing(term.clone(), tag.clone()));
            }

            // Rung C — checker-verified omission: a missing arm is accepted
            // iff first-order inversion of the scrutinee's actual indices
            // against this case's targets finds a *definite* clash. The arm
            // is then pruned (erase fills its slot with an unreachable
            // body); anything short of definite keeps the arm mandatory.
            let telescope = induct_decl
                .instantiate(tag, &params)
                .expect("constructor instantiates at its inductive's parameters");

            let labels = (0..telescope.len())
                .map(|_| context.fresh(None))
                .collect::<Vec<_>>();
            let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();
            let ix_c = case_target_indices(telescope, &vars);

            match invert_indices(context, &actual_indices, &ix_c, &labels)? {
                Invert::Impossible => continue,
                Invert::Solved(_) => {
                    return Err(Error::missing_arm_not_impossible(tag.clone()));
                }
            }
        };

        let telescope = induct_decl
            .instantiate(tag, &params)
            .expect("constructor instantiates at its inductive's parameters");

        // Static arity check: the arm's binder count must equal the
        // constructor's payload arity.
        let arity = telescope.len();
        if scope.arity() != arity {
            return Err(Error::ctor_arity_mismatch(
                tag.clone(),
                arity,
                scope.arity(),
            ));
        }

        // Check each written pattern plicity against the constructor's canonical
        // payload plicity: a payload slot the declaration marked `@` must be
        // matched with `@`, an unmarked payload with a plain binder. (Alignment
        // is exact here — pattern insertion is deferred, so arity already
        // matched above.)
        let payload_plicities = induct_decl
            .payload_plicities(tag)
            .expect("constructor payload plicities parallel its telescope");
        for (position, (written, canonical)) in
            scope.plicities.iter().zip(payload_plicities).enumerate()
        {
            if written != canonical {
                return Err(Error::BinderPlicityMismatch {
                    position: position + 1,
                    expected: *canonical,
                    written: *written,
                });
            }
        }

        // Open the telescope with fresh names paralleling the arm's binder
        // labels; each binder is assumed at its declared (dependent) type.
        let hints = scope
            .label_iter()
            .map(|l| l.map(str::to_string))
            .collect::<Vec<_>>();
        let labels = hints
            .iter()
            .map(|hint| context.fresh(hint.as_deref()))
            .collect::<Vec<_>>();
        let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

        let body_elaborated = context.with_frame(|context| {
            let mut telescope = telescope;
            for (label, var) in labels.iter().zip(&vars) {
                match telescope {
                    Telescope::Cons(ty, rest) => {
                        context.assume(label, &ty);
                        telescope = rest.open(&[var]);
                    }
                    Telescope::Done(_) => unreachable!("arity checked above"),
                }
            }

            // This case's target indices: the terminal of its (instantiated,
            // opened) signature states them over the payload binders.
            let ix_c = match &telescope {
                Telescope::Done(terminal) => match &***terminal {
                    Subterm::InductType(InductType { indices, .. }) => indices.clone(),
                    _ => unreachable!("constructor terminal is its inductive type"),
                },
                Telescope::Cons(..) => unreachable!("arity checked above"),
            };

            // Refinement propagates `head := ctor_val` to other occurrences of
            // the scrutinee in the arm body; the binder types themselves came
            // from the telescope above.
            let ctor_val = Term::variant(name.clone(), params.clone(), tag.clone(), vars.clone());
            refine_head(context, &head_elaborated, &ctor_val)?;

            // Rung B — definitional learning: a scrutinee index that is a
            // `Var` reduces, inside this arm, to the case's target index — the
            // same counterfactual, frame-scoped move as `head := ctor_val`. A
            // constructor index records an inert entry (`refine_head`); the
            // inverter below pins the arm binders the other way. Refinements
            // never justify the typing (the motive application does); they are
            // convertibility aids, so context hypotheses mentioning the key
            // reduce at the arm's index.
            for (actual, target) in actual_indices.iter().zip(&ix_c) {
                refine_head(context, actual, target)?;
            }

            // Rung C — inversion, arm side: a scrutinee index in constructor
            // form pins arm binders to forced values (`m + 1 ~ n + 1` pins
            // `m := n`), registered as the same frame-scoped reducts. A
            // definite clash here means the arm is unreachable; it was
            // written, so it is simply checked as is.
            if let Invert::Solved(solutions) =
                invert_indices(context, &actual_indices, &ix_c, &labels)?
            {
                for (label, solution) in solutions {
                    context.refine(&label, &solution);
                }
            }

            // The motive at this case: the index binders take the case's target
            // indices, the scrutinee binder the constructed value.
            let arm_refs = ix_c.iter().chain([&ctor_val]).collect::<Vec<_>>();
            let expected = motive_elaborated.open(&arm_refs);

            let var_refs = vars.iter().collect::<Vec<_>>();
            check_generalized_arm(context, &scope.open(&var_refs), expected, &generalized)
        })?;

        let label_strs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        // Rebuild the arm with the constructor's canonical payload plicities, so
        // a re-elaborated arm re-checks identically (idempotence).
        cases_elaborated.insert(
            tag.clone(),
            InductArm {
                body: Scope::close(Many(arity), &label_strs, body_elaborated),
                plicities: payload_plicities.to_vec(),
            },
        );
    }

    // The catch-all binds nothing and refines no index, so it is checked at the
    // unrefined head — the motive at the actual scrutinee (`elim_type`), exactly
    // as `elaborate_switch` checks its default. Under generalization `elim_type`
    // is the Π over the hypotheses, so it routes through `check_generalized_arm`
    // (with no arm binders) just as the constructor arms do.
    let default_elaborated = default
        .map(|d| {
            context.with_frame(|context| {
                check_generalized_arm(context, d, elim_type.clone(), &generalized)
            })
        })
        .transpose()?;

    let rebuilt_match: Term = Subterm::Match(Match {
        head: head_elaborated,
        motive: motive_elaborated,
        cases: Cases::Induct {
            cases: cases_elaborated,
            default: default_elaborated,
        },
    })
    .into();

    // Under generalization the eliminator is a function over the generalized
    // telescope; apply it to the original hypotheses (in binding order) to
    // recover a term of the original goal — the `go(nz)` of a hand-written
    // convoy, synthesized.
    let rebuilt = if generalized.is_empty() {
        rebuilt_match
    } else {
        Term::apply(
            rebuilt_match,
            generalized.iter().map(|(name, _)| Term::free_var(name)),
        )
    };

    Ok((rebuilt, result_type))
}

/// Whether a motive scope is the lowering's elided form — a bare metavariable
/// body (`match s | ..` or the explicit hole `match s : _ | ..`), as opposed to
/// a user-written constant or scrutinee-binding motive. Synthesis only steps in
/// for the elided form; everything else is taken verbatim.
fn is_elided_motive(motive: &Scope<Many>) -> bool {
    matches!(&**motive.body(), Subterm::Metavar(_))
}

/// The label by which a flat motive slot (or the scrutinee) abstracts the
/// expected type: a slot whose actual value is a not-yet-used free variable
/// abstracts by that variable's name, so the eliminator specialises the goal at
/// each constructor when it reopens the motive; a forced constructor, a compound
/// term, or a repeated variable takes a fresh label and binds vacuously (the
/// inversion already pins it).
fn abstraction_label(context: &mut Context, value: &Term, used: &mut BTreeSet<String>) -> String {
    if let Subterm::Var(var) = &**value
        && let Some(label) = var.as_free()
        && used.insert(label.to_string())
    {
        return label.to_string();
    }
    context.fresh(None)
}

/// The product of motive synthesis: the rebuilt motive scope and the hypotheses
/// generalized into it (in binding order, for the caller to re-apply the
/// eliminator to).
type SynthesizedMotive = (Scope<Many>, Vec<(String, Term)>);

/// Derive, for an inductive match that elided its motive, the dependent motive
/// the convoy pattern would otherwise be written by hand. The expected type is
/// abstracted over the scrutinee's index *variables* and the scrutinee itself
/// (forced or compound positions bind vacuously); the resulting scope is
/// validated by the same [`check_motive`] a written motive goes through, so its
/// per-arm specialisation behaves identically. Any context hypothesis whose type
/// mentions an abstracted index and which occurs in the goal is generalized into
/// the motive as a Π-telescope (the convoy, automated).
fn synthesize_induct_motive(
    context: &mut Context,
    shape: &MotiveShape<'_>,
    induct_decl: &InductDecl,
    actual_indices: &[Term],
    head: &Term,
    expected: &Term,
) -> Result<SynthesizedMotive, Error> {
    let n_indices = induct_decl.indices.len() - induct_decl.params.len();

    // One abstraction label per index, then the scrutinee — the binder order
    // every motive has. The labels that come back as a variable's own name
    // (`used` accumulates them) are the *roots*: the binders that genuinely
    // specialise the goal, and the ones a context hypothesis can depend on.
    //
    // Parameters are deliberately *not* abstracted. They are uniform across
    // constructors and fixed by the scrutinee's type, so abstracting one binds
    // it to the identical term in the match's own type and in every arm — a
    // vacuous binder that specialises nothing. Leaving them out keeps them out
    // of `used`, hence out of `infected`, so a hypothesis whose type merely
    // mentions a parameter is not generalized into the convoy below.
    let mut used = BTreeSet::new();
    let mut labels = Vec::with_capacity(n_indices + 1);
    for value in actual_indices {
        labels.push(abstraction_label(context, value, &mut used));
    }
    labels.push(abstraction_label(context, head, &mut used));

    // Dependent generalization — automate the convoy. A context hypothesis whose
    // type mentions a root (an abstracted index/parameter/scrutinee variable)
    // cannot stay free in the abstracted goal: once the root becomes a fresh
    // motive binder, the hypothesis's stated type no longer matches the position
    // it occupies (the `Lt(0, len b)` vs `Lt(0, len b2)` mismatch). Such
    // hypotheses ride into the motive as a Π-telescope ahead of the goal —
    // exactly the binders a hand-written convoy introduces — and the caller
    // re-applies the eliminator to the originals.
    //
    // Two conditions, both necessary. *Infected*: the hypothesis's type
    // transitively mentions a root (a forward sweep — a hypothesis's type can
    // only name earlier binders). *Reachable*: the hypothesis actually occurs in
    // the goal, or in the type of an already-generalized hypothesis (a backward
    // sweep). Generalizing an infected-but-unreachable hypothesis — e.g. an
    // induction hypothesis whose type mentions the index but which the goal never
    // names — is sound yet needless, and it would restructure a match that
    // converges fine on its own (via arm-local refinement), so we leave it alone.
    let mut infected = used.clone();
    for (label, type_) in context.locals() {
        if !infected.contains(label) && type_.free_vars().iter().any(|v| infected.contains(v)) {
            infected.insert(label.clone());
        }
    }

    let mut needed = expected.free_vars();
    let mut generalized: Vec<(String, Term)> = Vec::new();
    for (label, type_) in context.locals().iter().rev() {
        if used.contains(label) {
            continue;
        }
        if needed.contains(label) && infected.contains(label) {
            needed.extend(type_.free_vars());
            generalized.push((label.clone(), type_.clone()));
        }
    }
    generalized.reverse();

    // The goal the motive abstracts: the expected type, prefixed by the
    // generalized telescope. `func_type` captures each hypothesis name in the
    // later hypothesis types and in the goal, leaving the roots free for the
    // motive's own `close` below.
    let goal = if generalized.is_empty() {
        expected.clone()
    } else {
        Term::func_type(
            generalized.iter().map(|(n, t)| (n.clone(), t.clone())),
            expected.clone(),
        )
    };

    let label_refs = labels.iter().map(String::as_str).collect::<Vec<_>>();
    let motive = Scope::close(Many(labels.len()), &label_refs, goal);

    Ok((check_motive(context, shape, &motive)?, generalized))
}

/// Check an arm body against its (possibly generalized) per-case goal. Without
/// generalization this is a plain `check`. With it, the goal is a Π over the
/// generalized hypotheses (`(h : T_specialised) -> G`); peel that telescope,
/// re-assuming each hypothesis under its *case-specialised* type and its
/// original name — shadowing the ambient binder — so the body (which names the
/// hypotheses) and the codomain `G` (which also mentions them) line up at the
/// refined types, then wrap the checked body back into the matching lambda. This
/// is the convoy's `(h) => …` arm, synthesized; the eliminator's branch must be
/// a function because its motive is a Π.
fn check_generalized_arm(
    context: &mut Context,
    body: &Term,
    expected: Term,
    generalized: &[(String, Term)],
) -> Result<Term, Error> {
    if generalized.is_empty() {
        return check(context, body, expected);
    }

    let ft = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::FuncType(ft) => ft,
        _ => unreachable!("a generalized arm goal is the Π synthesize built"),
    };

    // Walk the Π at the originals' names, re-assuming each hypothesis under its
    // *case-specialised* type — shadowing the ambient binder — and collecting the
    // lambda's domains as we go. `walk` returns the codomain `G`, which (like the
    // body) names the hypotheses, so both line up at the refined types.
    let names = generalized
        .iter()
        .map(|(name, _)| Term::free_var(name))
        .collect::<Vec<_>>();

    let mut domains = Vec::with_capacity(generalized.len());
    let plicities = ft.plicities.clone();
    let codomain = ft.telescope.walk(&names, |i, name, type_| {
        let name = name.head_label().expect("walked at a fresh free variable");
        context.assume(name, type_);
        domains.push((plicities[i], name.to_string(), type_.clone()));
        Ok::<(), Error>(())
    })?;

    let inner = check(context, body, codomain)?;
    Ok(Term::func_marked(domains, inner))
}
