use {
    super::{
        Apply, AtomType, BlnMatch, Context, Error, FuncType, Let, Match, Nat, NatMatch, One, Prim,
        Proj, Rec, Scope, Subterm, Telescope, Term, TupleType, Two, Var, check_motive, erase,
        expect_prim_head, infer_prim, reduce_with, refine_head,
    },
    std::collections::BTreeMap,
};

fn infer_func_type(context: &mut Context, ft: &FuncType) -> Result<Term, Error> {
    fn walk(context: &mut Context, tele: Telescope<Term>) -> Result<(), Error> {
        match tele {
            Telescope::Done(body) => erase(context, &body, &Term::type_()).map(|_| ()),
            Telescope::Cons(ty, rest) => {
                erase(context, &ty, &Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &ty);
                walk(context, rest.open(&[&x]))
            }
        }
    }

    context.with_frame(|context| walk(context, ft.telescope.clone()))?;

    Ok(Term::type_())
}

fn infer_apply(context: &mut Context, apply: &Apply, term: &Term) -> Result<Term, Error> {
    let Apply { head, params } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let ft = match &*head_type {
        Subterm::FuncType(ft) => ft.clone(),
        other => return Err(Error::not_a_function(term.clone(), other.clone())),
    };

    if params.len() != ft.telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            term.clone(),
            ft.telescope.len(),
            params.len(),
        ));
    }

    ft.telescope
        .walk(params, |arg, ty| erase(context, arg, ty).map(|_| ()))
}

fn infer_tuple_type(context: &mut Context, tt: &TupleType) -> Result<Term, Error> {
    fn walk(context: &mut Context, tele: Telescope<()>) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                erase(context, &ty, &Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &ty);
                walk(context, rest.open(&[&x]))
            }
        }
    }

    context.with_frame(|context| walk(context, tt.telescope.clone()))?;

    Ok(Term::type_())
}

fn infer_nat_induction(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
) -> Result<Term, Error> {
    expect_prim_head(context, head, term, Prim::NatType)?;

    check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    erase(
        context,
        zero_case,
        &motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(
            &ih_label,
            &motive.open(&[&Term::var(Var::free(&pred_label))]),
        );

        erase(
            context,
            &succ_case.open(&[
                &Term::var(Var::free(&pred_label)),
                &Term::var(Var::free(&ih_label)),
            ]),
            &motive.open(&[&Subterm::Prim(Prim::nat_add(
                Term::var(Var::free(&pred_label)),
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into()]),
        )
        .map(|_| ())
    })?;

    Ok(motive.open(&[head]))
}

fn infer_nat_dispatch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
) -> Result<Term, Error> {
    expect_prim_head(context, head, term, Prim::NatType)?;

    check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    for (n, body) in cases {
        context.with_frame(|context| {
            refine_head(
                context,
                head,
                &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
            )?;
            erase(
                context,
                body,
                &motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]),
            )
            .map(|_| ())
        })?;
    }

    erase(context, default, &motive.open(&[head]))?;

    Ok(motive.open(&[head]))
}

fn infer_nat_match(context: &mut Context, nm: &NatMatch, term: &Term) -> Result<Term, Error> {
    match nm {
        NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        } => infer_nat_induction(context, head, motive, zero_case, succ_case, term),
        NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        } => infer_nat_dispatch(context, head, motive, cases, default, term),
    }
}

fn infer_bln_match(context: &mut Context, bm: &BlnMatch, term: &Term) -> Result<Term, Error> {
    let BlnMatch {
        head,
        motive,
        false_case,
        true_case,
    } = bm;

    expect_prim_head(context, head, term, Prim::BlnType)?;

    check_motive(context, &Subterm::Prim(Prim::BlnType).into(), motive)?;

    context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into())?;
        erase(
            context,
            false_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
        .map(|_| ())
    })?;

    context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into())?;
        erase(
            context,
            true_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
        .map(|_| ())
    })?;

    Ok(motive.open(&[head]))
}

fn infer_proj(context: &mut Context, proj: &Proj, term: &Term) -> Result<Term, Error> {
    let Proj { head, index } = proj;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let TupleType { telescope } = match &*head_type {
        Subterm::TupleType(tt) => tt.clone(),
        other => return Err(Error::not_a_tuple(term.clone(), other.clone())),
    };

    if *index >= telescope.len() {
        return Err(Error::tuple_index_out_of_bounds(
            term.clone(),
            *index,
            telescope.len(),
        ));
    }

    Ok(telescope
        .nth(*index, |j| Term::proj((**head).clone(), j))
        .expect("index in range"))
}

fn infer_match(context: &mut Context, m: &Match, term: &Term) -> Result<Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let atoms = match &*head_type {
        Subterm::AtomType(AtomType { atoms }) => atoms.clone(),
        other => return Err(Error::not_an_atom_type(term.clone(), other.clone())),
    };

    check_motive(context, &Term::atom_type(atoms.iter().cloned()), motive)?;

    if cases.len() != atoms.len() {
        return Err(Error::match_arity_mismatch(
            term.clone(),
            atoms.len(),
            cases.len(),
        ));
    }

    for atom in &atoms {
        let body = if let Some(body) = cases.get(atom) {
            body
        } else {
            return Err(Error::match_case_missing(term.clone(), atom.clone()));
        };

        let expected = motive.open(&[&Term::atom(atom.clone())]);

        context.with_frame(|context| {
            refine_head(context, head, &Term::atom(atom.clone()))?;
            erase(context, body, &expected)
        })?;
    }

    Ok(motive.open(&[head]))
}

fn infer_let(context: &mut Context, let_: &Let) -> Result<Term, Error> {
    let Let { type_, body, tail } = let_;

    erase(context, type_, &Term::type_())?;
    erase(context, body, type_)?;

    let label = context.fresh(tail.first_label());

    context.with_frame(|context| {
        context.define_assuming(&label, type_, body);

        let tail_type = infer(context, &tail.open(&[&Term::var(Var::free(label))]))?;

        reduce_with(context, &tail_type)
    })
}

fn infer_rec(context: &mut Context, rec: &Rec) -> Result<Term, Error> {
    let Rec { items, tail } = rec;

    let labels = tail
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();

    let label_terms = labels
        .iter()
        .map(Var::free)
        .map(Term::var)
        .collect::<Vec<_>>();

    let label_terms = label_terms.iter().collect::<Vec<_>>();

    let items = items
        .iter()
        .map(|(type_, body)| (type_.open(&label_terms), body.open(&label_terms)))
        .collect::<Vec<_>>();

    let tail = tail.open(&label_terms);

    context.with_frame(|context| {
        for (label, (type_, _)) in labels.iter().zip(items.iter()) {
            context.assume(label, type_);
        }

        for (type_, _) in &items {
            erase(context, type_, &Term::type_())?;
        }

        for (label, (_, body)) in labels.iter().zip(items.iter()) {
            context.define(label, body);
        }

        for (_, (type_, body)) in labels.iter().zip(items.iter()) {
            erase(context, body, type_)?;
        }

        let tail_type = infer(context, &tail)?;

        reduce_with(context, &tail_type)
    })
}

pub fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    let result = match &**term {
        Subterm::Type => Ok(Term::type_()),
        Subterm::Prim(prim) => infer_prim(context, prim),
        Subterm::BlnMatch(bm) => infer_bln_match(context, bm, term),
        Subterm::NatMatch(nm) => infer_nat_match(context, nm, term),
        Subterm::FuncType(ft) => infer_func_type(context, ft),
        Subterm::Apply(apply) => infer_apply(context, apply, term),
        Subterm::TupleType(tt) => infer_tuple_type(context, tt),
        Subterm::Proj(proj) => infer_proj(context, proj, term),
        Subterm::AtomType(_) => Ok(Term::type_()),
        Subterm::Match(m) => infer_match(context, m, term),
        Subterm::Let(let_) => infer_let(context, let_),
        Subterm::Rec(rec) => infer_rec(context, rec),
        Subterm::Var(var) => match context.assumption(var.unwrap()) {
            Some(type_) => Ok(type_.clone()),
            None => Err(Error::unbound_variable(Term::var(var.clone()))),
        },
        _ => Err(Error::cannot_infer(term.clone())),
    };

    match term.span() {
        Some(span) => result.map_err(|error| error.at(span)),
        None => result,
    }
}
