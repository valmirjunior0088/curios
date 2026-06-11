use {
    super::{
        Apply, Cases, Context, Field, Func, Let, Match, Metavar, Nat, Prim, Proj, ReduceError,
        Subterm,
        Term, Tuple, Var, reduce_prim,
    },
    num_bigint::BigUint,
    num_traits::ToPrimitive,
    std::time::Instant,
};

enum Reduce {
    Continue(Term),
    Break(Term),
}

fn reduce_apply(context: &mut Context, apply: Apply) -> Result<Reduce, ReduceError> {
    let Apply {
        head,
        params,
        plicities,
    } = apply;
    let param_refs = params.iter().collect::<Vec<_>>();
    match Term::unwrap_or_clone(reduce(context, head)?) {
        Subterm::Func(Func { telescope }) => Ok(Reduce::Continue(telescope.open(&param_refs))),
        head => Ok(Reduce::Break(Term::from(Subterm::Apply(Apply {
            head: head.into(),
            params,
            plicities,
        })))),
    }
}

fn reduce_proj(context: &mut Context, proj: Proj) -> Result<Reduce, ReduceError> {
    let Proj { head, field } = proj;
    // Label projections are resolved (and rebuilt positionally) by elaborate;
    // reduction only ever sees post-elaboration terms.
    let Field::Index(index) = field else {
        unreachable!("unresolved label projection reached reduction");
    };
    if let Some(v) = context.proj_reduct(&head, index) {
        return Ok(Reduce::Continue(v.clone()));
    }
    match Term::unwrap_or_clone(reduce(context, head)?) {
        Subterm::Tuple(Tuple { fields, .. }) => Ok(Reduce::Continue(
            fields
                .into_iter()
                .nth(index)
                .expect("Proj: index out of bounds"),
        )),
        // The untyped reducer's flat view of a constructor value, mirroring the
        // runtime layout `(tag, payload...)`: field i + 1 is the i-th payload
        // component. `reduce_union_match` relies on this to bind arms by
        // projection (call-by-name). Field 0 (the tag) is never projected at
        // the term level — dispatch inspects the `Variant` directly.
        Subterm::Variant(ctor) if (1..=ctor.payload.len()).contains(&index) => {
            Ok(Reduce::Continue(
                ctor.payload
                    .into_iter()
                    .nth(index - 1)
                    .expect("index bounded above"),
            ))
        }
        head => {
            let head: Term = head.into();
            match context.proj_reduct(&head, index) {
                Some(v) => Ok(Reduce::Continue(v.clone())),
                None => Ok(Reduce::Break(Term::proj(head, index))),
            }
        }
    }
}

fn reduce_func_eta(context: &mut Context, func: Func) -> Result<Reduce, ReduceError> {
    let n = func.telescope.len();
    let freshs = (0..n).map(|_| context.fresh(None)).collect::<Vec<_>>();
    let ys = freshs
        .iter()
        .map(|f| Term::var(Var::free(f)))
        .collect::<Vec<_>>();
    let y_refs = ys.iter().collect::<Vec<_>>();
    match Term::unwrap_or_clone(func.telescope.open(&y_refs)) {
            Subterm::Apply(Apply { head, params, .. })
                if params.len() == n
                    && params.iter().enumerate().all(|(i, p)| {
                        matches!(p.as_ref(), Subterm::Var(v) if v.unwrap() == freshs[i].as_str())
                    })
                    && freshs.iter().all(|f| !head.free_vars().contains(f)) =>
            {
                Ok(Reduce::Continue(head))
            }
            _ => Ok(Reduce::Break(Term::from(Subterm::Func(func)))),
        }
}

fn reduce_match(context: &mut Context, m: Match) -> Result<Reduce, ReduceError> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    match cases {
        Cases::Bln {
            false_case,
            true_case,
        } => match Term::unwrap_or_clone(reduce(context, head)?) {
            Subterm::Prim(Prim::Bln(false)) => Ok(Reduce::Continue(false_case)),
            Subterm::Prim(Prim::Bln(true)) => Ok(Reduce::Continue(true_case)),
            head => Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                head: head.into(),
                motive,
                cases: Cases::Bln {
                    false_case,
                    true_case,
                },
            })))),
        },

        Cases::Nat {
            zero_case,
            succ_case,
        } => match Term::unwrap_or_clone(reduce(context, head)?) {
            Subterm::Prim(Prim::Nat(Nat::Zero)) => Ok(Reduce::Continue(zero_case)),
            Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) => {
                let one = BigUint::from(1usize);
                let pred = if spine == one {
                    inner
                } else {
                    Term::prim(Prim::Nat(Nat::Succ(spine - one, inner)))
                };
                let ih: Term = Subterm::Match(Match {
                    head: pred.clone(),
                    motive: motive.clone(),
                    cases: Cases::Nat {
                        zero_case: zero_case.clone(),
                        succ_case: succ_case.clone(),
                    },
                })
                .into();
                Ok(Reduce::Continue(succ_case.open(&[&pred, &ih])))
            }
            head => Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                head: head.into(),
                motive,
                cases: Cases::Nat {
                    zero_case,
                    succ_case,
                },
            })))),
        },

        Cases::Switch { cases, default } => match Term::unwrap_or_clone(reduce(context, head)?) {
            Subterm::Prim(Prim::Nat(Nat::Zero)) => match cases.get(&0) {
                Some(body) => Ok(Reduce::Continue(body.clone())),
                None => Ok(Reduce::Continue(default.clone())),
            },
            Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner)))
                if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) =>
            {
                match spine.to_u32().and_then(|k| cases.get(&k)) {
                    Some(body) => Ok(Reduce::Continue(body.clone())),
                    None => Ok(Reduce::Continue(default.clone())),
                }
            }
            head => Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                head: head.into(),
                motive,
                cases: Cases::Switch { cases, default },
            })))),
        },

        // Dispatch on the reduced scrutinee — a `Variant` directly, or one
        // reached through a match-arm refinement (`refine_head` registers
        // `head := ctor_val`, which `reduce` follows). The selected arm's
        // binders are bound to *projections of the original head term*
        // (`head.(i + 1)`, the flat view in `reduce_proj`), not to the reduced
        // payload values: call-by-name. Substituting reduced payloads would
        // inline evaluated definition internals (including local-`let`
        // annotation holes that elaboration never births) into types that
        // flow on to `zonk`.
        Cases::Union { cases, pattern } => {
            let head_reduced = reduce(context, head.clone())?;

            if let Subterm::Variant(ctor) = &*head_reduced
                && let Some(scope) = cases.get(&ctor.tag)
            {
                let projections = (0..scope.arity())
                    .map(|i| Term::proj(head.clone(), i + 1))
                    .collect::<Vec<_>>();
                let projection_refs = projections.iter().collect::<Vec<_>>();

                return Ok(Reduce::Continue(scope.open(&projection_refs)));
            }

            Ok(Reduce::Break(Term::from(Subterm::Match(Match {
                head: head_reduced,
                motive,
                cases: Cases::Union { cases, pattern },
            }))))
        }
    }
}

fn reduce_let(let_: Let) -> Reduce {
    Reduce::Continue(let_.tail.open(&[&let_.body]))
}

fn reduce_var(context: &Context, var: Var) -> Reduce {
    match context.var_reduct(var.unwrap()) {
        Some(next) => Reduce::Continue(next.clone()),
        None => Reduce::Break(Term::var(var)),
    }
}

fn reduce_metavar(context: &Context, metavar: Metavar) -> Reduce {
    match context.metavar_solution(metavar.id) {
        Some(solution) => Reduce::Continue(solution.clone()),
        None => Reduce::Break(Term::from(Subterm::Metavar(metavar))),
    }
}

pub fn reduce(context: &mut Context, term: Term) -> Result<Term, ReduceError> {
    context.get_or_init_reduced(term, |context, term| {
        let mut term = term;

        loop {
            if Instant::now() > context.deadline() {
                break Err(ReduceError::Preempted);
            }

            let step = match Term::unwrap_or_clone(term) {
                Subterm::Prim(prim) => Reduce::Break(reduce_prim(context, &prim)?.into()),
                Subterm::Match(m) => reduce_match(context, m)?,
                Subterm::Apply(apply) => reduce_apply(context, apply)?,
                Subterm::Proj(proj) => reduce_proj(context, proj)?,
                Subterm::Func(func) => reduce_func_eta(context, func)?,
                Subterm::Let(let_) => reduce_let(let_),
                Subterm::Var(var) => reduce_var(context, var),
                Subterm::Metavar(metavar) => reduce_metavar(context, metavar),
                // `UnionType` and `Variant` are primitive normal forms, like
                // `Tuple`: their sub-terms are not reduced in WHNF.
                term => Reduce::Break(term.into()),
            };

            match step {
                Reduce::Continue(next) => term = next,
                Reduce::Break(result) => break Ok(result),
            }
        }
    })
}
