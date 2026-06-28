use {
    super::{
        Atom, Carrier, Cases, Context, Error, Inductive, InductiveType, Invert, Many, Match, Mode,
        MotivePattern, MotiveSlot, Nat, Prim, PrimHead, Scope, Subterm, Telescope, Term, Three,
        Two, case_target_indices, check, check_motive, check_prim_head, convert_with, elaborate,
        expect, invert_indices, is_prop, reduce_with, refine_head,
    },
    std::collections::BTreeMap,
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
/// scope wrapping a fresh metavar (`text::to_core::elaborate`), so `motive.open`
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
    let motive = check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let zero_elaborated = check(
        context,
        zero_case,
        motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let succ_body = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&pred_label)]));

        check(
            context,
            &succ_case.open(&[&Term::free_var(&pred_label), &Term::free_var(&ih_label)]),
            motive.open(&[&Subterm::Prim(Prim::nat_add(
                Term::free_var(&pred_label),
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into()]),
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

fn elaborate_arr_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Arr` carries an element type the eliminator must read off the scrutinee
    // (unlike `Nat`, whose carrier is parameterless) — infer the head, then
    // demand its type is `Arr(elem)`.
    let (head_elaborated, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;
    let elem = match &*head_type {
        Subterm::Prim(Prim::ArrType(elem)) => elem.clone(),
        _ => return Err(Error::not_arr_type(head_type)),
    };

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = check_motive(context, &head_type, motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let empty_value: Term = Subterm::Prim(Prim::Arr(vec![])).into();
    let empty_elaborated = check(context, empty_case, motive.open(&[&empty_value]))?;

    let head_label = context.fresh(cons_case.first_label());
    let tail_label = context.fresh(cons_case.second_label());
    let ih_label = context.fresh(cons_case.third_label());

    let cons_body = context.with_frame(|context| {
        context.assume(&head_label, &elem);
        context.assume(&tail_label, &head_type);
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));

        // The cons value `head :: tail`, encoded as the monoid operation on a
        // singleton and the tail (no separate prepend primitive).
        let cons_value: Term = Subterm::Prim(Prim::ArrConcat(
            elem.clone(),
            vec![
                Subterm::Prim(Prim::Arr(vec![Term::free_var(&head_label)])).into(),
                Term::free_var(&tail_label),
            ],
        ))
        .into();

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
            carrier: Carrier::Arr {
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
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `Bin` is a parameterless carrier (like `Nat`/`Bln`), so the scrutinee's type
    // is just `Bin` — no element type to read off the head as `Arr` needs.
    let (head_elaborated, head_type) = elaborate_prim_head(context, head, PrimHead::Bin)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = check_motive(context, &head_type, motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let empty_value: Term = Subterm::Prim(Prim::Bin(vec![])).into();
    let empty_elaborated = check(context, empty_case, motive.open(&[&empty_value]))?;

    let head_label = context.fresh(cons_case.first_label());
    let tail_label = context.fresh(cons_case.second_label());
    let ih_label = context.fresh(cons_case.third_label());

    let cons_body = context.with_frame(|context| {
        // A `Bin`'s generator is a single byte, typed as `Nat`.
        context.assume(&head_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(&tail_label, &head_type);
        context.assume(&ih_label, &motive.open(&[&Term::free_var(&tail_label)]));

        // The cons value `head :: tail`, encoded as the monoid operation on the
        // singleton `[head]` and the tail. A `Bin` literal holds only concrete
        // bytes, so the singleton of the symbolic byte `head` is `append(\\, head)`
        // (a byte appended to the empty bytestring), not a `Bin` literal.
        let singleton: Term = Subterm::Prim(Prim::BinAppend(
            Subterm::Prim(Prim::Bin(vec![])).into(),
            Term::free_var(&head_label),
        ))
        .into();
        let cons_value: Term = Subterm::Prim(Prim::BinConcat(vec![
            singleton,
            Term::free_var(&tail_label),
        ]))
        .into();

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
    let motive = check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let mut cases_elaborated = BTreeMap::new();
    for (n, body) in cases {
        let body = context.with_frame(|context| {
            refine_head(
                context,
                head,
                &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
            );
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

pub fn elaborate_match(
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
        Cases::Bln {
            false_case,
            true_case,
        } => elaborate_bln_match(context, head, motive, false_case, true_case, term, mode),
        Cases::Switch { cases, default } => {
            elaborate_switch(context, head, motive, cases, default, term, mode)
        }
        Cases::Inductive { cases, pattern } => {
            elaborate_inductive_match(context, head, motive, cases, pattern.as_ref(), term, mode)
        }
        Cases::FreeMonoid {
            carrier:
                Carrier::Nat {
                    empty_case,
                    cons_case,
                },
        } => elaborate_nat_match(context, head, motive, empty_case, cons_case, term, mode),
        Cases::FreeMonoid {
            carrier:
                Carrier::Arr {
                    empty_case,
                    cons_case,
                    ..
                },
        } => elaborate_arr_match(context, head, motive, empty_case, cons_case, term, mode),
        Cases::FreeMonoid {
            carrier:
                Carrier::Bin {
                    empty_case,
                    cons_case,
                },
        } => elaborate_bin_match(context, head, motive, empty_case, cons_case, term, mode),
    }
}

#[allow(clippy::too_many_arguments)]
fn elaborate_bln_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, PrimHead::Bln)?;

    // The *rebuilt* motive throughout, as in `elaborate_nat_match`.
    let motive = check_motive(context, &Subterm::Prim(Prim::BlnType).into(), motive)?;

    seed_motive(context, term, &motive, &head_elaborated, &mode)?;

    let false_elaborated = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into());
        check(
            context,
            false_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    let true_elaborated = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into());
        check(
            context,
            true_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    let result_type = motive.open(&[&head_elaborated]);
    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive,
        cases: Cases::Bln {
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
    inductive: &Inductive,
    tag: &Atom,
    params: &[Term],
) -> Result<bool, Error> {
    let Some(payload) = inductive.instantiate(tag, params) else {
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
/// With a plain motive (no pattern) the discipline is constant/scrutinee-only:
/// arms check against `motive(variant)`, the match has type `motive(head)`,
/// and any indices ride along inertly. The annotated type-pattern motive
/// (Rung A of the indexed-inductive ladder) additionally binds the scrutinee's
/// indices: each arm checks against the motive at *that case's* target
/// indices, and the whole match types at the scrutinee's *actual* indices.
fn elaborate_inductive_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<Atom, Scope<Many>>,
    pattern: Option<&MotivePattern>,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // A match with no arms is a vacuous elimination — either of an empty inductive
    // (`False`) or of one whose every constructor inversion-clashes at the
    // scrutinee's indices. Such a match compiles to unreachable code that never
    // inspects the scrutinee. A proof/type scrutinee carries no runtime content
    // (sort-driven erasure), so an empty match discharging an erased witness of
    // falsity into a relevant result is sound without a usage discipline.
    let (head_elaborated, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    let (name, params, actual_indices) = match &*head_type {
        Subterm::InductiveType(InductiveType {
            name,
            params,
            indices,
        }) => (name.clone(), params.clone(), indices.clone()),
        other => return Err(Error::not_a_inductive_type(other.clone())),
    };

    let Some(inductive) = context.inductive(&name).cloned() else {
        return Err(Error::unbound_variable(Term::free_var(&name)));
    };

    let (motive_elaborated, pattern_elaborated, plan) = match pattern {
        None => (check_motive(context, &head_type, motive)?, None, vec![]),
        Some(pattern) => {
            let (motive_elaborated, pattern_elaborated, plan) =
                check_inductive_motive(context, &inductive, &name, &params, motive, pattern)?;
            (motive_elaborated, Some(pattern_elaborated), plan)
        }
    };

    // The match's own type: the motive at the scrutinee itself — and, for a
    // pattern motive, at the scrutinee's actual parameters and indices. Opened
    // from the *rebuilt* motive, as in `elaborate_nat_match`.
    let result_args = plan
        .iter()
        .map(|slot| match slot {
            SlotPlan::Param(i) => params[*i].clone(),
            SlotPlan::Index(j) => actual_indices[*j].clone(),
        })
        .collect::<Vec<_>>();
    let result_refs = result_args
        .iter()
        .chain([&head_elaborated])
        .collect::<Vec<_>>();
    let result_type = motive_elaborated.open(&result_refs);

    // The seed (`seed_motive`'s job, generalized over the pattern binders):
    // in checking mode, pin the motive — a bare metavar when elided — to the
    // expected type before the arms are checked.
    if let Mode::Check(expected) = &mode {
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
        let permitted = match cases.len() {
            0 => true,
            1 => {
                let tag = cases.keys().next().expect("one covered constructor");
                singleton_eliminable(context, &inductive, tag, &params)?
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
        .find(|tag| !inductive.constructors.contains_key(*tag))
    {
        return Err(Error::unknown_match_constructor(
            name.clone(),
            tag.to_string(),
        ));
    }

    let mut cases_elaborated = BTreeMap::new();
    for tag in inductive.constructors.keys() {
        let Some(scope) = cases.get(tag) else {
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
            let telescope = inductive
                .instantiate(tag, &params)
                .expect("constructor instantiates at its inductive's parameters");

            let labels = (0..telescope.len())
                .map(|_| context.fresh(None))
                .collect::<Vec<_>>();
            let vars = labels
                .iter()
                .map(|label| Term::free_var(label))
                .collect::<Vec<_>>();
            let ix_c = case_target_indices(telescope, &vars);

            match invert_indices(context, &actual_indices, &ix_c, &labels)? {
                Invert::Impossible => continue,
                Invert::Solved(_) => {
                    return Err(Error::missing_arm_not_impossible(tag.clone()));
                }
            }
        };

        let telescope = inductive
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
        let vars = labels
            .iter()
            .map(|label| Term::free_var(label))
            .collect::<Vec<_>>();

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
                    Subterm::InductiveType(InductiveType { indices, .. }) => indices.clone(),
                    _ => unreachable!("constructor terminal is its inductive type"),
                },
                Telescope::Cons(..) => unreachable!("arity checked above"),
            };

            // Refinement propagates `head := ctor_val` to other occurrences of
            // the scrutinee in the arm body; the binder types themselves came
            // from the telescope above.
            let ctor_val = Term::variant(name.clone(), params.clone(), tag.clone(), vars.clone());
            refine_head(context, head, &ctor_val);

            // Rung B — definitional learning: a scrutinee index that is a
            // stable key (`refine_head`'s Var/Proj restriction) reduces,
            // inside this arm, to the case's target index — the same
            // counterfactual, frame-scoped move as `head := ctor_val`.
            // Refinements never justify the typing (the motive application
            // does); they are convertibility aids, so context hypotheses
            // mentioning the key reduce at the arm's index.
            for (actual, target) in actual_indices.iter().zip(&ix_c) {
                refine_head(context, actual, target);
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

            // The motive at this case: pattern binders take the actual
            // parameters and the case's target indices; the scrutinee takes
            // the constructed value.
            let arm_args = plan
                .iter()
                .map(|slot| match slot {
                    SlotPlan::Param(i) => params[*i].clone(),
                    SlotPlan::Index(j) => ix_c[*j].clone(),
                })
                .collect::<Vec<_>>();
            let arm_refs = arm_args.iter().chain([&ctor_val]).collect::<Vec<_>>();
            let expected = motive_elaborated.open(&arm_refs);

            let var_refs = vars.iter().collect::<Vec<_>>();
            check(context, &scope.open(&var_refs), expected)
        })?;

        let label_strs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        cases_elaborated.insert(
            tag.clone(),
            Scope::close(Many(arity), &label_strs, body_elaborated),
        );
    }

    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive: motive_elaborated,
        cases: Cases::Inductive {
            cases: cases_elaborated,
            pattern: pattern_elaborated,
        },
    })
    .into();

    Ok((rebuilt, result_type))
}

/// How each binder of an annotated motive scope (scrutinee excluded) maps to
/// the inductive's flat argument list: a parameter position (opened with the
/// actual parameter everywhere) or an index position (opened with the case's
/// target index in arms, the scrutinee's actual index for the match itself).
enum SlotPlan {
    Param(usize),
    Index(usize),
}

/// Check the annotated type-pattern motive of an inductive match: validate the
/// pattern against the registry (parameter slots verbatim-or-binder, index
/// slots binder-only), then check the motive body as a type family over the
/// index binders and the scrutinee.
///
/// Index binders are assumed at the registry's index telescope instantiated
/// at the scrutinee's actual parameters, each later type opened with the
/// earlier binder; the scrutinee binder is assumed at
/// `InductiveType(name, params, index-vars)`. No unification anywhere — the
/// eliminator's discipline.
fn check_inductive_motive(
    context: &mut Context,
    inductive: &Inductive,
    name: &str,
    params: &[Term],
    motive: &Scope<Many>,
    pattern: &MotivePattern,
) -> Result<(Scope<Many>, MotivePattern, Vec<SlotPlan>), Error> {
    let n_params = inductive.params.len();
    let n_indices = inductive.indices.len() - n_params;

    if pattern.name != name {
        return Err(Error::motive_wrong_inductive(
            pattern.name.clone(),
            name.to_string(),
        ));
    }

    if pattern.slots.len() != n_params + n_indices {
        return Err(Error::motive_pattern_arity(
            n_params + n_indices,
            pattern.slots.len(),
        ));
    }

    // Each parameter's declared type at the actual parameters, for checking
    // verbatim slots.
    let mut param_types = Vec::with_capacity(n_params);
    inductive.params.clone().walk(params, |_, _, ty| {
        param_types.push(ty.clone());
        Ok(())
    })?;

    let mut plan = Vec::new();
    let mut slots_elaborated = Vec::new();

    for (position, slot) in pattern.slots.iter().enumerate() {
        match (slot, position < n_params) {
            // A parameter slot binder is legal — it binds the actual
            // parameter, which the scrutinee's type fixes anyway.
            (MotiveSlot::Binder, true) => {
                plan.push(SlotPlan::Param(position));
                slots_elaborated.push(MotiveSlot::Binder);
            }
            (MotiveSlot::Binder, false) => {
                plan.push(SlotPlan::Index(position - n_params));
                slots_elaborated.push(MotiveSlot::Binder);
            }
            // A verbatim parameter must be the actual parameter — written
            // out for the reader, checked for the checker.
            (MotiveSlot::Term(t), true) => {
                let elaborated = check(context, t, param_types[position].clone())?;
                if !convert_with(context, &elaborated, &params[position])? {
                    return Err(Error::motive_param_mismatch(
                        elaborated,
                        params[position].clone(),
                    ));
                }
                slots_elaborated.push(MotiveSlot::Term(elaborated));
            }
            // An index slot states nothing — it binds. Constraining an index
            // is the declaration's job (case targets), not the motive's.
            (MotiveSlot::Term(t), false) => {
                return Err(Error::motive_index_slot_not_binder(t.clone()));
            }
        }
    }

    // The lowering closes one motive binder per binder slot, then the
    // scrutinee; a mismatch is a lowering bug, not user error.
    assert_eq!(
        plan.len() + 1,
        motive.arity(),
        "motive scope arity matches its pattern's binder slots"
    );

    let hints = motive
        .label_iter()
        .map(|l| l.map(str::to_string))
        .collect::<Vec<_>>();
    let labels = hints
        .iter()
        .map(|hint| context.fresh(hint.as_deref()))
        .collect::<Vec<_>>();

    let motive_elaborated = context.with_frame(|context| {
        // Peel the parameter binders off the full index telescope, leaving
        // the index types at the actual parameters.
        let mut ix_telescope = inductive.indices.clone().open_params(params);

        let mut index_vars = Vec::with_capacity(n_indices);
        for (slot, label) in plan.iter().zip(&labels) {
            let var = Term::free_var(label);
            match slot {
                // A parameter binder is an *alias* of the actual parameter —
                // defined, not assumed, so the motive body's uses of it are
                // convertible with the index types (which are instantiated at
                // the actual parameters directly).
                SlotPlan::Param(i) => context.define_assuming(label, &param_types[*i], &params[*i]),
                SlotPlan::Index(_) => {
                    ix_telescope = match ix_telescope {
                        Telescope::Cons(ty, rest) => {
                            context.assume(label, &ty);
                            rest.open(&[&var])
                        }
                        Telescope::Done(_) => {
                            unreachable!("index slot count equals the index telescope's")
                        }
                    };
                    index_vars.push(var);
                }
            }
        }

        let scrutinee_label = labels.last().expect("motive binds at least the scrutinee");
        context.assume(
            scrutinee_label,
            &Term::inductive_type(name, params.to_vec(), index_vars),
        );

        let var_terms = labels
            .iter()
            .map(|label| Term::free_var(label))
            .collect::<Vec<_>>();
        let var_refs = var_terms.iter().collect::<Vec<_>>();
        let body = elaborate(context, &motive.open(&var_refs), Mode::Check(Term::type_()))?.0;

        let label_strs = labels.iter().map(String::as_str).collect::<Vec<_>>();
        Ok(Scope::close(Many(labels.len()), &label_strs, body))
    })?;

    Ok((
        motive_elaborated,
        MotivePattern {
            name: name.to_string(),
            slots: slots_elaborated,
        },
        plan,
    ))
}
