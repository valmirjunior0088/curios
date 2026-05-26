use {
    super::{
        Apply, AtomType, BlnMatch, Context, Error, FuncType, Let, Match, Nat, NatMatch, One, Prim,
        Proj, Rec, Scope, Subterm, Term, TupleType, Two, Type, Var, erase, reduce_with,
        refine_head,
    },
    std::collections::BTreeMap,
};

fn infer_prim(context: &mut Context, prim: &Prim) -> Result<Term, Error> {
    match prim {
        Prim::BlnType => Ok(Type.into()),
        Prim::Bln(_) => Ok(Term::Prim(Prim::BlnType)),
        Prim::NatType => Ok(Type.into()),
        Prim::Nat(_) => Ok(Term::Prim(Prim::NatType)),
        Prim::NatEql(left, right)
        | Prim::NatNeq(left, right)
        | Prim::NatLt(left, right)
        | Prim::NatGt(left, right)
        | Prim::NatLte(left, right)
        | Prim::NatGte(left, right) => {
            erase(context, left, &Term::Prim(Prim::NatType))?;
            erase(context, right, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::BlnType))
        }
        Prim::NatAdd(left, right)
        | Prim::NatSub(left, right)
        | Prim::NatMul(left, right)
        | Prim::NatDiv(left, right)
        | Prim::NatRem(left, right) => {
            erase(context, left, &Term::Prim(Prim::NatType))?;
            erase(context, right, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::NatType))
        }
        Prim::IntType => Ok(Type.into()),
        Prim::Int(_) => Ok(Term::Prim(Prim::IntType)),
        Prim::IntEql(left, right)
        | Prim::IntNeq(left, right)
        | Prim::IntLt(left, right)
        | Prim::IntGt(left, right)
        | Prim::IntLte(left, right)
        | Prim::IntGte(left, right) => {
            erase(context, left, &Term::Prim(Prim::IntType))?;
            erase(context, right, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::BlnType))
        }
        Prim::IntAdd(left, right)
        | Prim::IntSub(left, right)
        | Prim::IntMul(left, right)
        | Prim::IntDiv(left, right)
        | Prim::IntRem(left, right) => {
            erase(context, left, &Term::Prim(Prim::IntType))?;
            erase(context, right, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::IntType))
        }
        Prim::FltType => Ok(Type.into()),
        Prim::Flt(_) => Ok(Term::Prim(Prim::FltType)),
        Prim::FltAdd(left, right)
        | Prim::FltSub(left, right)
        | Prim::FltMul(left, right)
        | Prim::FltDiv(left, right)
        | Prim::FltMin(left, right)
        | Prim::FltMax(left, right) => {
            erase(context, left, &Term::Prim(Prim::FltType))?;
            erase(context, right, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::FltType))
        }
        Prim::FltNeg(inner)
        | Prim::FltAbs(inner)
        | Prim::FltSqrt(inner)
        | Prim::FltFloor(inner)
        | Prim::FltCeil(inner)
        | Prim::FltTrunc(inner)
        | Prim::FltNearest(inner) => {
            erase(context, inner, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::FltType))
        }
        Prim::FltEql(left, right)
        | Prim::FltNeq(left, right)
        | Prim::FltLt(left, right)
        | Prim::FltGt(left, right)
        | Prim::FltLte(left, right)
        | Prim::FltGte(left, right) => {
            erase(context, left, &Term::Prim(Prim::FltType))?;
            erase(context, right, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::BlnType))
        }
        Prim::NatToStr(inner) => {
            erase(context, inner, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::BinType))
        }
        Prim::IntToStr(inner) => {
            erase(context, inner, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::BinType))
        }
        Prim::FltToStr(inner) => {
            erase(context, inner, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::BinType))
        }
        Prim::NatToInt(inner) => {
            erase(context, inner, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::IntType))
        }
        Prim::NatToFlt(inner) => {
            erase(context, inner, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::FltType))
        }
        Prim::IntToNat(inner) => {
            erase(context, inner, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::NatType))
        }
        Prim::IntToFlt(inner) => {
            erase(context, inner, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::FltType))
        }
        Prim::FltToNat(inner) => {
            erase(context, inner, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::NatType))
        }
        Prim::FltToInt(inner) => {
            erase(context, inner, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::IntType))
        }
        Prim::BinType => Ok(Type.into()),
        Prim::Bin(_) => Ok(Term::Prim(Prim::BinType)),
        Prim::BinLen(bin) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match bin_type {
                Term::Prim(Prim::BinType) => Ok(Term::Prim(Prim::NatType)),
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other,
                    Term::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinEql(left, right) => {
            erase(context, left, &Term::Prim(Prim::BinType))?;
            erase(context, right, &Term::Prim(Prim::BinType))?;

            Ok(Term::Prim(Prim::BlnType))
        }
        Prim::BinGet(bin, index) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match bin_type {
                Term::Prim(Prim::BinType) => {
                    erase(context, index, &Term::Prim(Prim::NatType))?;
                    Ok(Term::Prim(Prim::NatType))
                }
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other,
                    Term::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinSlice(bin, start, end) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match bin_type {
                Term::Prim(Prim::BinType) => {
                    erase(context, start, &Term::Prim(Prim::NatType))?;
                    erase(context, end, &Term::Prim(Prim::NatType))?;
                    Ok(Term::Prim(Prim::BinType))
                }
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other,
                    Term::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinAppend(bin, byte) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match bin_type {
                Term::Prim(Prim::BinType) => {
                    erase(context, byte, &Term::Prim(Prim::NatType))?;
                    Ok(Term::Prim(Prim::BinType))
                }
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other,
                    Term::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinConcat(operands) => {
            for operand in operands {
                erase(context, operand, &Term::Prim(Prim::BinType))?;
            }
            Ok(Term::Prim(Prim::BinType))
        }
        Prim::ArrType(elem) => {
            erase(context, elem, &Type.into())?;
            Ok(Type.into())
        }
        Prim::Arr(_) => Err(Error::cannot_infer_literal(Term::Prim(prim.clone()))),
        Prim::ArrLen(list) => {
            let list_type = infer(context, list)?;
            let list_type = reduce_with(context, &list_type)?;
            match list_type {
                Term::Prim(Prim::ArrType(_)) => Ok(Term::Prim(Prim::NatType)),
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other,
                    Term::Prim(Prim::NatType),
                )),
            }
        }
        Prim::ArrGet(list, index) => {
            let list_type = infer(context, list)?;
            let list_type = reduce_with(context, &list_type)?;
            match list_type {
                Term::Prim(Prim::ArrType(elem)) => {
                    erase(context, index, &Term::Prim(Prim::NatType))?;
                    Ok(*elem)
                }
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other,
                    Term::Prim(Prim::NatType),
                )),
            }
        }
        Prim::ArrSlice(list, start, end) => {
            let list_type = infer(context, list)?;
            let list_type = reduce_with(context, &list_type)?;
            match &list_type {
                Term::Prim(Prim::ArrType(_)) => {
                    erase(context, start, &Term::Prim(Prim::NatType))?;
                    erase(context, end, &Term::Prim(Prim::NatType))?;
                    Ok(list_type)
                }
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other.clone(),
                    Term::Prim(Prim::NatType),
                )),
            }
        }
        Prim::ArrAppend(list, elem) => {
            let list_type = infer(context, list)?;
            let list_type = reduce_with(context, &list_type)?;
            match &list_type {
                Term::Prim(Prim::ArrType(elem_type)) => {
                    let elem_type = *elem_type.clone();
                    erase(context, elem, &elem_type)?;
                    Ok(list_type)
                }
                other => Err(Error::type_mismatch(
                    Term::Prim(prim.clone()),
                    other.clone(),
                    Term::Prim(Prim::NatType),
                )),
            }
        }
        Prim::ArrConcat(_) => Err(Error::cannot_infer_literal(Term::Prim(prim.clone()))),
        Prim::SysPrint(inner) => {
            erase(context, inner, &Term::Prim(Prim::BinType))?;
            Ok(Term::TupleType(TupleType::unit()))
        }
        Prim::SysRead => Ok(Term::Prim(Prim::BinType)),
    }
}

fn infer_func_type(context: &mut Context, ft: &FuncType) -> Result<Term, Error> {
    let FuncType { params, output } = ft;
    let n = params.len();

    let labels = (0..n)
        .map(|i| {
            let hint = params
                .get(i + 1)
                .and_then(|s| s.label_iter().nth(i))
                .flatten();
            context.fresh(hint)
        })
        .collect::<Vec<_>>();
    let label_terms = labels
        .iter()
        .map(|l| Term::from(Var::free(l)))
        .collect::<Vec<Term>>();
    let label_refs = label_terms.iter().collect::<Vec<_>>();

    context.with_frame(|context| {
        for i in 0..n {
            let ty = params[i].open(&label_refs[..i]);
            erase(context, &ty, &Type.into())?;
            context.assume(&labels[i], &ty);
        }
        erase(context, &output.open(&label_refs), &Type.into()).map(|_| ())
    })?;

    Ok(Type.into())
}

fn infer_apply(context: &mut Context, apply: &Apply, term: &Term) -> Result<Term, Error> {
    let Apply { head, params } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let ft = match head_type {
        Term::FuncType(ft) => ft,
        other => return Err(Error::not_a_function(term.clone(), other)),
    };

    if params.len() != ft.params.len() {
        return Err(Error::wrong_number_of_arguments(
            term.clone(),
            ft.params.len(),
            params.len(),
        ));
    }

    let mut param_terms: Vec<Term> = Vec::with_capacity(params.len());

    for (i, param) in params.iter().enumerate() {
        let so_far = param_terms.iter().collect::<Vec<_>>();
        let expected_ty = ft.params[i].open(&so_far);
        erase(context, param, &expected_ty)?;
        param_terms.push(*param.clone());
    }

    let param_refs = param_terms.iter().collect::<Vec<_>>();
    Ok(ft.output.open(&param_refs))
}

fn infer_tuple_type(context: &mut Context, tt: &TupleType) -> Result<Term, Error> {
    let TupleType { fields } = tt;
    let n = fields.len();

    let labels = (0..n)
        .map(|i| {
            let hint = fields
                .get(i + 1)
                .and_then(|s| s.label_iter().nth(i))
                .flatten();
            context.fresh(hint)
        })
        .collect::<Vec<_>>();
    let label_terms = labels
        .iter()
        .map(|l| Term::from(Var::free(l)))
        .collect::<Vec<Term>>();
    let label_refs = label_terms.iter().collect::<Vec<_>>();

    context.with_frame(|context| {
        for i in 0..n {
            let ty = fields[i].open(&label_refs[..i]);
            erase(context, &ty, &Type.into())?;
            context.assume(&labels[i], &ty);
        }
        Ok(())
    })?;

    Ok(Type.into())
}

fn infer_nat_induction(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
) -> Result<Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(head_type, Term::Prim(Prim::NatType)) {
        return Err(Error::not_nat_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Term::Prim(Prim::NatType));

        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

    erase(
        context,
        zero_case,
        &motive.open(&[&Term::Prim(Prim::Nat(Nat::new(0)))]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    context.with_frame(|context| {
        context.assume(&pred_label, &Term::Prim(Prim::NatType));
        context.assume(&ih_label, &motive.open(&[&Var::free(&pred_label).into()]));

        erase(
            context,
            &succ_case.open(&[&Var::free(&pred_label).into(), &Var::free(&ih_label).into()]),
            &motive.open(&[&Term::Prim(Prim::nat_add(
                Var::free(&pred_label),
                Term::Prim(Prim::Nat(Nat::new(1))),
            ))]),
        )
        .map(|_| ())
    })?;

    Ok(motive.open(&[head]))
}

fn infer_nat_dispatch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    cases: &BTreeMap<u32, Subterm>,
    default: &Term,
    term: &Term,
) -> Result<Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(head_type, Term::Prim(Prim::NatType)) {
        return Err(Error::not_nat_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Term::Prim(Prim::NatType));
        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

    for (n, body) in cases {
        context.with_frame(|context| {
            refine_head(context, head, &Term::Prim(Prim::Nat(Nat::new(*n))))?;
            erase(
                context,
                body,
                &motive.open(&[&Term::Prim(Prim::Nat(Nat::new(*n)))]),
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

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    if !matches!(head_type, Term::Prim(Prim::BlnType)) {
        return Err(Error::not_bln_type(term.clone(), head_type));
    }

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Term::Prim(Prim::BlnType));
        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

    context.with_frame(|context| {
        refine_head(context, head.as_ref(), &Term::Prim(Prim::Bln(false)))?;
        erase(
            context,
            false_case,
            &motive.open(&[&Term::Prim(Prim::Bln(false))]),
        )
        .map(|_| ())
    })?;

    context.with_frame(|context| {
        refine_head(context, head.as_ref(), &Term::Prim(Prim::Bln(true)))?;
        erase(
            context,
            true_case,
            &motive.open(&[&Term::Prim(Prim::Bln(true))]),
        )
        .map(|_| ())
    })?;

    Ok(motive.open(&[head.as_ref()]))
}

fn infer_proj(context: &mut Context, proj: &Proj, term: &Term) -> Result<Term, Error> {
    let Proj { head, index } = proj;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let TupleType { fields } = match head_type {
        Term::TupleType(tt) => tt,
        other => return Err(Error::not_a_tuple(term.clone(), other)),
    };

    if *index >= fields.len() {
        return Err(Error::tuple_index_out_of_bounds(
            term.clone(),
            *index,
            fields.len(),
        ));
    }

    let prefix: Vec<Term> = (0..*index)
        .map(|j| Proj::new(*head.clone(), j).into())
        .collect();
    let prefix_refs: Vec<&Term> = prefix.iter().collect();

    Ok(fields[*index].open(&prefix_refs))
}

fn infer_match(context: &mut Context, m: &Match, term: &Term) -> Result<Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let atoms = match head_type {
        Term::AtomType(AtomType { atoms }) => atoms,
        other => return Err(Error::not_an_atom_type(term.clone(), other)),
    };

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &AtomType::new(atoms.iter().cloned()).into());

        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

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

        let expected = motive.open(&[&atom.clone().into()]);

        context.with_frame(|context| {
            refine_head(context, head.as_ref(), &atom.clone().into())?;
            erase(context, body, &expected)
        })?;
    }

    Ok(motive.open(&[head.as_ref()]))
}

fn infer_let(context: &mut Context, let_: &Let) -> Result<Term, Error> {
    let Let { type_, body, tail } = let_;

    erase(context, type_, &Type.into())?;
    erase(context, body, type_)?;

    let label = context.fresh(tail.first_label());

    context.with_frame(|context| {
        context.define_assuming(&label, type_, body);

        let tail_type = infer(context, &tail.open(&[&Var::free(label).into()]))?;

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
        .map(Into::into)
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
            erase(context, type_, &Type.into())?;
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
    match term {
        Term::Type => Ok(Type.into()),
        Term::Prim(prim) => infer_prim(context, prim),
        Term::BlnMatch(bm) => infer_bln_match(context, bm, term),
        Term::NatMatch(nm) => infer_nat_match(context, nm, term),
        Term::FuncType(ft) => infer_func_type(context, ft),
        Term::Apply(apply) => infer_apply(context, apply, term),
        Term::TupleType(tt) => infer_tuple_type(context, tt),
        Term::Proj(proj) => infer_proj(context, proj, term),
        Term::AtomType(_) => Ok(Type.into()),
        Term::Match(m) => infer_match(context, m, term),
        Term::Let(let_) => infer_let(context, let_),
        Term::Rec(rec) => infer_rec(context, rec),
        Term::Var(var) => match context.assumption(var.unwrap()) {
            Some(type_) => Ok(type_.clone()),
            None => Err(Error::unbound_variable(var.clone())),
        },
        Term::Spanned(span, inner) => infer(context, inner).map_err(|error| error.at(*span)),
        _ => Err(Error::cannot_infer(term.clone())),
    }
}
