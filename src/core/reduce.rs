mod reduce_prim;
use reduce_prim::*;

use {
    super::{
        Apply, BlnMatch, Context, Func, Let, Match, Nat, NatMatch, One, Preempted, Prim, Proj,
        Scope, Subterm, Term, Tuple, Two, Var,
    },
    std::{collections::BTreeMap, time::Instant},
};

enum Reduce {
    Continue(Term),
    Break(Term),
}

fn reduce_apply(context: &mut Context, apply: Apply) -> Result<Reduce, Preempted> {
    let Apply { head, params } = apply;
    let param_refs = params.iter().collect::<Vec<_>>();
    match Term::unwrap_or_clone(reduce(context, head)?) {
        Subterm::Func(Func { body }) => Ok(Reduce::Continue(body.open(&param_refs))),
        head => Ok(Reduce::Break(Term::apply(head, params))),
    }
}

fn reduce_proj(context: &mut Context, proj: Proj) -> Result<Reduce, Preempted> {
    let Proj { head, index } = proj;
    if let Some(v) = context.projection(&head, index) {
        return Ok(Reduce::Continue(v.clone()));
    }
    match Term::unwrap_or_clone(reduce(context, head)?) {
        Subterm::Tuple(Tuple { fields }) => Ok(Reduce::Continue(
            fields
                .into_iter()
                .nth(index)
                .expect("Proj: index out of bounds"),
        )),
        head => {
            let head: Term = head.into();
            match context.projection(&head, index) {
                Some(v) => Ok(Reduce::Continue(v.clone())),
                None => Ok(Reduce::Break(Term::proj(head, index))),
            }
        }
    }
}

fn reduce_func_eta(context: &mut Context, func: Func) -> Result<Reduce, Preempted> {
    let n = func.body.arity();
    let freshs = (0..n).map(|_| context.fresh(None)).collect::<Vec<_>>();
    let ys = freshs
        .iter()
        .map(|f| Term::from(Var::free(f)))
        .collect::<Vec<_>>();
    let y_refs = ys.iter().collect::<Vec<_>>();
    match Term::unwrap_or_clone(func.body.open(&y_refs)) {
            Subterm::Apply(Apply { head, params })
                if params.len() == n
                    && params.iter().enumerate().all(|(i, p)| {
                        matches!(p.as_ref(), Subterm::Var(v) if v.unwrap() == freshs[i].as_str())
                    })
                    && freshs.iter().all(|f| !head.free_vars().contains(f)) =>
            {
                Ok(Reduce::Continue(head))
            }
            _ => Ok(Reduce::Break(Term::new(Subterm::Func(func)))),
        }
}

fn reduce_nat_induction(
    context: &mut Context,
    head: Subterm,
    motive: Scope<One>,
    zero_case: Term,
    succ_case: Scope<Two>,
) -> Result<Reduce, Preempted> {
    match Term::unwrap_or_clone(reduce(context, head.into())?) {
        Subterm::Prim(Prim::Nat(Nat::Zero)) => Ok(Reduce::Continue(zero_case)),
        Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) => {
            let pred = if spine == 1 {
                inner
            } else {
                Prim::Nat(Nat::Succ(spine - 1, inner)).into()
            };
            let ih: Term = Subterm::NatMatch(NatMatch::Induction {
                head: pred.clone(),
                motive: motive.clone(),
                zero_case: zero_case.clone(),
                succ_case: succ_case.clone(),
            })
            .into();
            Ok(Reduce::Continue(succ_case.open(&[&pred, &ih])))
        }
        head => Ok(Reduce::Break(Term::new(Subterm::NatMatch(
            NatMatch::Induction {
                head: head.into(),
                motive,
                zero_case,
                succ_case,
            },
        )))),
    }
}

fn reduce_bln_match(context: &mut Context, bm: BlnMatch) -> Result<Reduce, Preempted> {
    let BlnMatch {
        head,
        motive,
        false_case,
        true_case,
    } = bm;
    match Term::unwrap_or_clone(reduce(context, head)?) {
        Subterm::Prim(Prim::Bln(false)) => Ok(Reduce::Continue(false_case)),
        Subterm::Prim(Prim::Bln(true)) => Ok(Reduce::Continue(true_case)),
        head => Ok(Reduce::Break(Term::new(Subterm::BlnMatch(BlnMatch {
            head: head.into(),
            motive,
            false_case,
            true_case,
        })))),
    }
}

fn reduce_nat_dispatch(
    context: &mut Context,
    head: Subterm,
    motive: Scope<One>,
    cases: BTreeMap<u32, Term>,
    default: Term,
) -> Result<Reduce, Preempted> {
    match Term::unwrap_or_clone(reduce(context, head.into())?) {
        Subterm::Prim(Prim::Nat(Nat::Zero)) => match cases.get(&0) {
            Some(body) => Ok(Reduce::Continue(body.clone())),
            None => Ok(Reduce::Continue(default.clone())),
        },
        Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner)))
            if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) =>
        {
            match cases.get(&spine) {
                Some(body) => Ok(Reduce::Continue(body.clone())),
                None => Ok(Reduce::Continue(default.clone())),
            }
        }
        head => Ok(Reduce::Break(Term::new(Subterm::NatMatch(
            NatMatch::Dispatch {
                head: head.into(),
                motive,
                cases,
                default,
            },
        )))),
    }
}

fn reduce_nat_match(context: &mut Context, nm: NatMatch) -> Result<Reduce, Preempted> {
    match nm {
        NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        } => reduce_nat_induction(
            context,
            Term::unwrap_or_clone(head),
            motive,
            zero_case,
            succ_case,
        ),
        NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        } => reduce_nat_dispatch(context, Term::unwrap_or_clone(head), motive, cases, default),
    }
}

fn reduce_match(context: &mut Context, m: Match) -> Result<Reduce, Preempted> {
    let Match {
        head,
        motive,
        cases,
    } = m;
    let atom = match Term::unwrap_or_clone(reduce(context, head)?) {
        Subterm::Atom(atom) => atom,
        head => {
            return Ok(Reduce::Break(Term::new(Subterm::Match(Match {
                head: head.into(),
                motive,
                cases,
            }))));
        }
    };

    match cases.get(&atom) {
        Some(body) => Ok(Reduce::Continue(body.clone())),
        None => Ok(Reduce::Break(Term::new(Subterm::Match(Match {
            head: Term::atom(atom),
            motive,
            cases,
        })))),
    }
}

fn reduce_let(let_: Let) -> Reduce {
    Reduce::Continue(let_.tail.open(&[&let_.body]))
}

fn reduce_var(context: &Context, var: Var) -> Reduce {
    match context.definition(var.unwrap()) {
        Some(next) => Reduce::Continue(next.clone()),
        None => Reduce::Break(var.into()),
    }
}

pub fn reduce(context: &mut Context, term: Term) -> Result<Term, Preempted> {
    context.get_or_init_reduced(term, |context, term| {
        let mut term = term;

        loop {
            if Instant::now() > context.deadline() {
                break Err(Preempted);
            }

            let step = match Term::unwrap_or_clone(term) {
                Subterm::Prim(prim) => Reduce::Break(reduce_prim(context, &prim)?.into()),
                Subterm::BlnMatch(bm) => reduce_bln_match(context, bm)?,
                Subterm::NatMatch(nm) => reduce_nat_match(context, nm)?,
                Subterm::Apply(apply) => reduce_apply(context, apply)?,
                Subterm::Proj(proj) => reduce_proj(context, proj)?,
                Subterm::Func(func) => reduce_func_eta(context, func)?,
                Subterm::Match(m) => reduce_match(context, m)?,
                Subterm::Let(let_) => reduce_let(let_),
                Subterm::Var(var) => reduce_var(context, var),
                term => Reduce::Break(term.into()),
            };

            match step {
                Reduce::Continue(next) => term = next,
                Reduce::Break(result) => break Ok(result),
            }
        }
    })
}

#[cfg(test)]
mod tests;
