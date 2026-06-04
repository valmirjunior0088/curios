use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Error, Func, Let, Match, Nat, NatMatch, One,
        Prim, Proj, Rec, Scope, Subterm, Telescope, Term, Tuple, TupleType, Two, Var, erase_prim,
        expect, infer, reduce_with, refine_head,
    },
    crate::ersd,
    std::collections::BTreeMap,
};

fn erase_func(
    context: &mut Context,
    func: &Func,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Func { body } = func;

    let ft = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::FuncType(ft) => ft,
        _ => return Err(Error::not_a_function_type(term.clone(), expected.clone())),
    };

    let n = ft.telescope.len();
    let captures = body.free_vars().into_iter().collect::<Vec<_>>();

    let param_names = (0..n)
        .map(|i| context.fresh(body.label_iter().nth(i).flatten()))
        .collect::<Vec<_>>();
    let param_terms = param_names
        .iter()
        .map(|p| Term::var(Var::free(p)))
        .collect::<Vec<_>>();
    let param_refs = param_terms.iter().collect::<Vec<_>>();
    let body_opened = body.open(&param_refs);

    fn output_type(
        context: &mut Context,
        tele: Telescope<Term>,
        names: &[String],
        terms: &[Term],
    ) -> Term {
        match tele {
            Telescope::Done(body) => *body,
            Telescope::Cons(ty, rest) => {
                context.assume(&names[0], &ty);
                output_type(context, rest.open(&[&terms[0]]), &names[1..], &terms[1..])
            }
        }
    }

    let erased_body = context.with_frame(|context| {
        let output_type = output_type(context, ft.telescope, &param_names, &param_terms);
        erase(context, &body_opened, &output_type)
    })?;

    Ok(ersd::Term::Func(ersd::Func {
        captures,
        params: param_names,
        body: erased_body.into(),
    }))
}

fn erase_apply(
    context: &mut Context,
    apply: &Apply,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Apply { head, params } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let ft = match &*head_type {
        Subterm::FuncType(ft) => ft,
        _ => return Err(Error::not_a_function(term.clone(), head_type)),
    };

    if params.len() != ft.telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            term.clone(),
            ft.telescope.len(),
            params.len(),
        ));
    }

    fn walk(
        context: &mut Context,
        tele: Telescope<Term>,
        params: &[Term],
        erased: &mut Vec<ersd::Term>,
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(body) => Ok(*body),
            Telescope::Cons(ty, rest) => {
                let head = &params[0];
                erased.push(erase(context, head, &ty)?);
                walk(context, rest.open(&[head]), &params[1..], erased)
            }
        }
    }

    let mut erased_params = Vec::with_capacity(params.len());
    let result_type = walk(context, ft.telescope.clone(), params, &mut erased_params)?;
    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &result_type, expected)?;

    Ok(ersd::Term::Apply(ersd::Apply {
        head: erased_head.into(),
        params: erased_params.into_iter().map(|p| p.into()).collect(),
    }))
}

fn erase_tuple(context: &mut Context, tuple: &Tuple, expected: &Term) -> Result<ersd::Term, Error> {
    let Tuple { fields } = tuple;

    let type_telescope = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::TupleType(TupleType { telescope }) => telescope,
        _ => {
            return Err(Error::not_a_tuple_type(
                Term::from(Subterm::Tuple(tuple.clone())),
                expected.clone(),
            ));
        }
    };

    if fields.len() != type_telescope.len() {
        return Err(Error::tuple_arity_mismatch(
            Term::from(Subterm::Tuple(tuple.clone())),
            type_telescope.len(),
            fields.len(),
        ));
    }

    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &[Term],
        erased: &mut Vec<ersd::Subterm>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let head = &fields[0];
                erased.push(erase(context, head, &ty)?.into());
                walk(context, rest.open(&[head]), &fields[1..], erased)
            }
        }
    }

    let mut erased_fields = Vec::<ersd::Subterm>::new();
    walk(context, type_telescope, fields, &mut erased_fields)?;

    Ok(ersd::Term::Tuple(ersd::Tuple {
        fields: erased_fields,
    }))
}

fn erase_nat_induction(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(&*head_type, Subterm::Prim(Prim::NatType)) {
        return Err(Error::not_nat_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Subterm::Prim(Prim::NatType).into());

        erase(
            context,
            &motive.open(&[&Term::var(Var::free(head_label))]),
            &Term::type_(),
        )
    })?;

    let erased_zero_case = erase(
        context,
        zero_case,
        &motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let erased_succ_case = context.with_frame(|context| {
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
    })?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::Term::NatMatch(ersd::NatMatch::Induction {
        head: erased_head.into(),
        zero_case: erased_zero_case.into(),
        pred: pred_label,
        ih: ih_label,
        succ_case: erased_succ_case.into(),
    }))
}

fn erase_nat_dispatch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(&*head_type, Subterm::Prim(Prim::NatType)) {
        return Err(Error::not_nat_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Subterm::Prim(Prim::NatType).into());
        erase(
            context,
            &motive.open(&[&Term::var(Var::free(head_label))]),
            &Term::type_(),
        )
    })?;

    let erased_cases = cases
        .iter()
        .map(|(n, body)| {
            let case_expected = motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]);
            context.with_frame(|context| {
                refine_head(
                    context,
                    head,
                    &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
                )?;
                erase(context, body, &case_expected).map(|e| (*n, e.into()))
            })
        })
        .collect::<Result<BTreeMap<_, _>, Error>>()?;

    let erased_default = erase(context, default, &motive.open(&[head]))?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::Term::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head.into(),
        cases: erased_cases,
        default: erased_default.into(),
    }))
}

fn erase_nat_match(
    context: &mut Context,
    nm: &NatMatch,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match nm {
        NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        } => erase_nat_induction(context, head, motive, zero_case, succ_case, term, expected),
        NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        } => erase_nat_dispatch(context, head, motive, cases, default, term, expected),
    }
}

fn erase_bln_match(
    context: &mut Context,
    bm: &BlnMatch,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let BlnMatch {
        head,
        motive,
        false_case,
        true_case,
    } = bm;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(&*head_type, Subterm::Prim(Prim::BlnType)) {
        return Err(Error::not_bln_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Subterm::Prim(Prim::BlnType).into());
        erase(
            context,
            &motive.open(&[&Term::var(Var::free(head_label))]),
            &Term::type_(),
        )
    })?;

    let erased_false = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into())?;
        erase(
            context,
            false_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    let erased_true = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into())?;
        erase(
            context,
            true_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::Term::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head.into(),
        cases: BTreeMap::from([(0, erased_false.into())]),
        default: erased_true.into(),
    }))
}

fn erase_proj(
    context: &mut Context,
    proj: &Proj,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Proj { head, index } = proj;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let TupleType { telescope } = match Term::unwrap_or_clone(head_type.clone()) {
        Subterm::TupleType(tt) => tt,
        other => return Err(Error::not_a_tuple(term.clone(), other)),
    };

    if *index >= telescope.len() {
        return Err(Error::tuple_index_out_of_bounds(
            term.clone(),
            *index,
            telescope.len(),
        ));
    }

    let field_type = telescope
        .nth(*index, |j| Term::proj((**head).clone(), j))
        .expect("index in range");

    expect(context, term, &field_type, expected)?;

    Ok(ersd::Term::Proj(ersd::Proj {
        head: erase(context, head, &head_type)?.into(),
        index: *index,
    }))
}

fn erase_atom(
    context: &mut Context,
    atom: &Atom,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let atoms = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::AtomType(AtomType { atoms }) => atoms,
        _ => {
            return Err(Error::type_mismatch(
                term.clone(),
                Term::atom_type([atom.clone()]),
                expected.clone(),
            ));
        }
    };

    let index = atoms
        .iter()
        .position(|candidate| candidate == atom)
        .ok_or_else(|| {
            Error::type_mismatch(
                term.clone(),
                Term::atom_type([atom.clone()]),
                expected.clone(),
            )
        })?;

    Ok(ersd::Term::Atom(ersd::Atom { index }))
}

fn erase_match(
    context: &mut Context,
    m: &Match,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let atoms = match Term::unwrap_or_clone(head_type.clone()) {
        Subterm::AtomType(AtomType { atoms }) => atoms,
        other => return Err(Error::not_an_atom_type(term.clone(), other)),
    };

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Term::atom_type(atoms.iter().cloned()));

        erase(
            context,
            &motive.open(&[&Term::var(Var::free(head_label))]),
            &Term::type_(),
        )
    })?;

    if cases.len() != atoms.len() {
        return Err(Error::match_arity_mismatch(
            term.clone(),
            atoms.len(),
            cases.len(),
        ));
    }

    let cases = atoms
        .iter()
        .map(|atom| {
            let body = if let Some(body) = cases.get(atom) {
                body
            } else {
                return Err(Error::match_case_missing(term.clone(), atom.clone()));
            };

            let expected = motive.open(&[&Term::atom(atom.clone())]);

            context.with_frame(|context| {
                refine_head(context, head, &Term::atom(atom.clone()))?;
                erase(context, body, &expected).map(Into::into)
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    expect(context, term, &motive.open(&[head]), expected)?;

    Ok(ersd::Term::Match(ersd::Match {
        head: erase(context, head, &head_type)?.into(),
        cases,
    }))
}

fn erase_let(context: &mut Context, let_: &Let, expected: &Term) -> Result<ersd::Term, Error> {
    let Let {
        type_: body_type,
        body,
        tail,
    } = let_;

    erase(context, body_type, &Term::type_())?;

    let name = context.fresh(tail.first_label());
    let erased_body = erase(context, body, body_type)?;
    let var_term = Term::var(Var::free(&name));
    let tail = tail.open(&[&var_term]);

    let tail = context.with_frame(|context| {
        context.define_assuming(&name, body_type, body);

        erase(context, &tail, expected)
    })?;

    Ok(ersd::Term::Let(ersd::Let {
        name,
        body: erased_body.into(),
        tail: tail.into(),
    }))
}

fn erase_rec(context: &mut Context, rec: &Rec, expected: &Term) -> Result<ersd::Term, Error> {
    let Rec { items, tail } = rec;

    let names = tail
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();

    let label_terms = names
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

    let erased = context.with_frame(|context| {
        for (name, (type_, _)) in names.iter().zip(items.iter()) {
            context.assume(name, type_);
        }

        for (type_, _) in &items {
            erase(context, type_, &Term::type_())?;
        }

        for (name, (_, body)) in names.iter().zip(items.iter()) {
            context.define(name, body);
        }

        let erased_items = items
            .iter()
            .map(|(type_, body)| erase(context, body, type_).map(Into::into))
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(ersd::Rec {
            names,
            items: erased_items,
            tail: erase(context, &tail, expected)?.into(),
        })
    })?;

    Ok(ersd::Term::Rec(erased))
}

pub fn erase(context: &mut Context, term: &Term, expected: &Term) -> Result<ersd::Term, Error> {
    let result = match &**term {
        Subterm::Prim(prim) => erase_prim(context, term, prim, expected),
        Subterm::BlnMatch(bm) => erase_bln_match(context, bm, term, expected),
        Subterm::NatMatch(nm) => erase_nat_match(context, nm, term, expected),
        Subterm::Type => {
            expect(context, term, &Term::type_(), expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::FuncType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::Func(func) => erase_func(context, func, term, expected),
        Subterm::Apply(apply) => erase_apply(context, apply, term, expected),
        Subterm::TupleType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::Tuple(tuple) => erase_tuple(context, tuple, expected),
        Subterm::Proj(proj) => erase_proj(context, proj, term, expected),
        Subterm::AtomType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Subterm::Atom(atom) => erase_atom(context, atom, term, expected),
        Subterm::Match(m) => erase_match(context, m, term, expected),
        Subterm::Let(let_) => erase_let(context, let_, expected),
        Subterm::Rec(lr) => erase_rec(context, lr, expected),
        Subterm::Var(var) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Name(ersd::Name::from(var.unwrap())))
        }
    };

    match term.span() {
        Some(span) => result.map_err(|error| error.at(span)),
        None => result,
    }
}
