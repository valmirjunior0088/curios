use {
    super::{
        Apply, AtomType, Context, Error, Func, FuncType, Let, Match, NatFold, NatMatch, Preempted,
        Prim, Proj, Rec, Seal, Sealed, Term, Tuple, TupleType, Type, Unseal, Var,
    },
    crate::ersd,
};

fn reduce(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce(context, term).map_err(|Preempted| Error::reduce_preempted(term.clone()))
}

fn convert(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    super::convert(context, &Type.into(), this, that)
        .map_err(|Preempted| Error::convert_preempted(this.clone(), that.clone()))
}

fn expect(
    context: &mut Context,
    term: &Term,
    inferred: &Term,
    expected: &Term,
) -> Result<(), Error> {
    match convert(context, inferred, expected)? {
        true => Ok(()),
        false => Err(Error::type_mismatch(term.clone(), expected.clone())),
    }
}

fn infer_prim(context: &mut Context, prim: &Prim) -> Result<Term, Error> {
    match prim {
        Prim::NatType => Ok(Type.into()),
        Prim::Nat(_) => Ok(Term::Prim(Prim::NatType)),
        Prim::NatEql(left, right)
        | Prim::NatNeq(left, right)
        | Prim::NatAdd(left, right)
        | Prim::NatSub(left, right)
        | Prim::NatMul(left, right)
        | Prim::NatLt(left, right)
        | Prim::NatDiv(left, right)
        | Prim::NatRem(left, right)
        | Prim::NatGt(left, right)
        | Prim::NatLte(left, right)
        | Prim::NatGte(left, right) => {
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

            Ok(Term::Prim(Prim::NatType))
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

            Ok(Term::Prim(Prim::NatType))
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
            let bin_type = reduce(context, &bin_type)?;
            match bin_type {
                Term::Prim(Prim::BinType) => Ok(Term::Prim(Prim::NatType)),
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
            }
        }
        Prim::BinEql(left, right) => {
            erase(context, left, &Term::Prim(Prim::BinType))?;
            erase(context, right, &Term::Prim(Prim::BinType))?;

            Ok(Term::Prim(Prim::NatType))
        }
        Prim::BinGet(bin, index) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce(context, &bin_type)?;
            match bin_type {
                Term::Prim(Prim::BinType) => {
                    erase(context, index, &Term::Prim(Prim::NatType))?;
                    Ok(Term::Prim(Prim::NatType))
                }
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
            }
        }
        Prim::BinSlice(bin, start, end) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce(context, &bin_type)?;
            match &bin_type {
                Term::Prim(Prim::BinType) => {
                    erase(context, start, &Term::Prim(Prim::NatType))?;
                    erase(context, end, &Term::Prim(Prim::NatType))?;
                    Ok(bin_type)
                }
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
            }
        }
        Prim::BinAppend(bin, byte) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce(context, &bin_type)?;
            match bin_type {
                Term::Prim(Prim::BinType) => {
                    erase(context, byte, &Term::Prim(Prim::NatType))?;
                    Ok(Term::Prim(Prim::BinType))
                }
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
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
        Prim::Arr(_) => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
        Prim::ArrLen(list) => {
            let list_type = infer(context, list)?;
            let list_type = reduce(context, &list_type)?;
            match list_type {
                Term::Prim(Prim::ArrType(_)) => Ok(Term::Prim(Prim::NatType)),
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
            }
        }
        Prim::ArrGet(list, index) => {
            let list_type = infer(context, list)?;
            let list_type = reduce(context, &list_type)?;
            match list_type {
                Term::Prim(Prim::ArrType(elem)) => {
                    erase(context, index, &Term::Prim(Prim::NatType))?;
                    Ok(*elem)
                }
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
            }
        }
        Prim::ArrSlice(list, start, end) => {
            let list_type = infer(context, list)?;
            let list_type = reduce(context, &list_type)?;
            match &list_type {
                Term::Prim(Prim::ArrType(_)) => {
                    erase(context, start, &Term::Prim(Prim::NatType))?;
                    erase(context, end, &Term::Prim(Prim::NatType))?;
                    Ok(list_type)
                }
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
            }
        }
        Prim::ArrAppend(list, elem) => {
            let list_type = infer(context, list)?;
            let list_type = reduce(context, &list_type)?;
            match &list_type {
                Term::Prim(Prim::ArrType(elem_type)) => {
                    let elem_type = *elem_type.clone();
                    erase(context, elem, &elem_type)?;
                    Ok(list_type)
                }
                _ => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
            }
        }
        Prim::ArrConcat(_) => Err(Error::cannot_infer(Term::Prim(prim.clone()))),
        Prim::SysPrint(inner) => {
            erase(context, inner, &Term::Prim(Prim::BinType))?;
            Ok(Term::AtomType(AtomType::new(["unit"])))
        }
    }
}

fn infer_func_type(context: &mut Context, ft: &FuncType) -> Result<Term, Error> {
    let FuncType { input, output } = ft;

    erase(context, input, &Type.into())?;

    let label = context.fresh();

    context.with_frame(|context| {
        context.assume(&label, input);

        erase(
            context,
            &output.open(&[&Var::free(label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

    Ok(Type.into())
}

fn infer_apply(context: &mut Context, apply: &Apply, term: &Term) -> Result<Term, Error> {
    let Apply { head, param } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    let (input, output) = if let Term::FuncType(FuncType { input, output }) = head_type {
        (input, output)
    } else {
        return Err(Error::cannot_infer(term.clone()));
    };

    erase(context, param, &input)?;

    Ok(output.open(&[param.as_ref()]))
}

fn infer_tuple_type(context: &mut Context, tt: &TupleType) -> Result<Term, Error> {
    let TupleType { fields } = tt;
    let n = fields.len();

    let labels = (0..n).map(|_| context.fresh()).collect::<Vec<_>>();
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

fn infer_nat_fold(context: &mut Context, nat_fold: &NatFold, term: &Term) -> Result<Term, Error> {
    let NatFold {
        head,
        motive,
        zero_case,
        succ_case,
    } = nat_fold;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    if !matches!(head_type, Term::Prim(Prim::NatType)) {
        return Err(Error::cannot_infer(term.clone()));
    }

    let head_label = context.fresh();

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
        &motive.open(&[&Term::Prim(Prim::Nat(0))]),
    )?;

    let pred_label = context.fresh();
    let ih_label = context.fresh();

    context.with_frame(|context| {
        context.assume(&pred_label, &Term::Prim(Prim::NatType));
        context.assume(&ih_label, &motive.open(&[&Var::free(&pred_label).into()]));

        erase(
            context,
            &succ_case.open(&[&Var::free(&pred_label).into(), &Var::free(&ih_label).into()]),
            &motive.open(&[&Term::Prim(Prim::nat_add(
                Var::free(&pred_label),
                Prim::Nat(1),
            ))]),
        )
        .map(|_| ())
    })?;

    Ok(motive.open(&[head.as_ref()]))
}

fn infer_nat_match(context: &mut Context, nm: &NatMatch, term: &Term) -> Result<Term, Error> {
    let NatMatch {
        head,
        motive,
        cases,
        default,
    } = nm;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    if !matches!(head_type, Term::Prim(Prim::NatType)) {
        return Err(Error::cannot_infer(term.clone()));
    }

    let head_label = context.fresh();

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
        erase(context, body, &motive.open(&[&Term::Prim(Prim::Nat(*n))]))?;
    }

    erase(context, default, &motive.open(&[head.as_ref()]))?;

    Ok(motive.open(&[head.as_ref()]))
}

fn infer_proj(context: &mut Context, proj: &Proj, term: &Term) -> Result<Term, Error> {
    let Proj { head, index } = proj;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    let TupleType { fields } = if let Term::TupleType(tt) = head_type {
        tt
    } else {
        return Err(Error::cannot_infer(term.clone()));
    };

    if *index >= fields.len() {
        return Err(Error::cannot_infer(term.clone()));
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
    let head_type = reduce(context, &head_type)?;

    let atoms = if let Term::AtomType(AtomType { atoms }) = head_type {
        atoms
    } else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let head_label = context.fresh();

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
        return Err(Error::cannot_infer(term.clone()));
    }

    let canonical = reduce(context, head.as_ref())?;

    for atom in &atoms {
        let body = if let Some(body) = cases.get(atom) {
            body
        } else {
            return Err(Error::cannot_infer(term.clone()));
        };

        let expected = motive.open(&[&atom.clone().into()]);

        context.with_frame(|context| {
            match &canonical {
                Term::Var(var) => {
                    context.define(var.unwrap(), &atom.clone().into());
                }
                Term::Proj(Proj { head: base, index }) => {
                    context.define_proj((**base).clone(), *index, &atom.clone().into());
                }
                _ => {}
            }

            erase(context, body, &expected)
        })?;
    }

    Ok(motive.open(&[head.as_ref()]))
}

fn infer_let(context: &mut Context, let_: &Let) -> Result<Term, Error> {
    let Let { type_, body, tail } = let_;

    erase(context, type_, &Type.into())?;
    erase(context, body, type_)?;

    let label = context.fresh();

    context.with_frame(|context| {
        context.define_assuming(&label, type_, body);

        infer(context, &tail.open(&[&Var::free(label).into()]))
    })
}

fn infer_rec(context: &mut Context, rec: &Rec) -> Result<Term, Error> {
    let Rec { items, tail } = rec;

    let labels = (0..items.len())
        .map(|_| context.fresh())
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

        infer(context, &tail)
    })
}

pub fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    match term {
        Term::Type => Ok(Type.into()),
        Term::Prim(prim) => infer_prim(context, prim),
        Term::NatFold(nat_fold) => infer_nat_fold(context, nat_fold, term),
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
            None => Err(Error::cannot_infer(var.clone())),
        },
        _ => Err(Error::cannot_infer(term.clone())),
    }
}

fn erase_prim(
    context: &mut Context,
    term: &Term,
    prim: &Prim,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match prim {
        Prim::NatType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Nat(value) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::Nat(value).into())
        }
        Prim::NatEql(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatEql(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatAdd(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatAdd(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatSub(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatSub(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatMul(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatMul(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatNeq(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatNeq(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatDiv(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatDiv(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatRem(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatRem(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatLt(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatLt(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatGt(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatGt(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatLte(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatLte(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::NatGte(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::NatGte(
                erase(context, left, &Term::Prim(Prim::NatType))?.into(),
                erase(context, right, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::IntType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Int(value) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::Int(value).into())
        }
        Prim::IntEql(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::IntEql(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntNeq(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::IntNeq(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntAdd(left, right) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::IntAdd(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntSub(left, right) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::IntSub(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntMul(left, right) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::IntMul(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntDiv(left, right) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::IntDiv(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntRem(left, right) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::IntRem(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntLt(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::IntLt(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntGt(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::IntGt(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntLte(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::IntLte(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntGte(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::IntGte(
                erase(context, left, &Term::Prim(Prim::IntType))?.into(),
                erase(context, right, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::FltType => {
            expect(context, term, &Type.into(), expected)?;

            Ok(ersd::Term::Erased)
        }
        &Prim::Flt(flt) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::Flt(flt.to_f32()).into())
        }
        Prim::FltAdd(left, right) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltAdd(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltSub(left, right) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltSub(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltMul(left, right) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltMul(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltNeg(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::FltNeg(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltAbs(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::FltAbs(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltSqrt(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::FltSqrt(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltFloor(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::FltFloor(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltCeil(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::FltCeil(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltTrunc(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::FltTrunc(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltNearest(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::FltNearest(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltDiv(left, right) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltDiv(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltMin(left, right) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltMin(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltMax(left, right) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltMax(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltEql(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::FltEql(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltNeq(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::FltNeq(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltLt(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::FltLt(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltGt(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::FltGt(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltLte(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::FltLte(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltGte(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::FltGte(
                erase(context, left, &Term::Prim(Prim::FltType))?.into(),
                erase(context, right, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::NatToStr(inner) => {
            expect(context, term, &Term::Prim(Prim::BinType), expected)?;

            Ok(
                ersd::Prim::NatToStr(erase(context, inner, &Term::Prim(Prim::NatType))?.into())
                    .into(),
            )
        }
        Prim::IntToStr(inner) => {
            expect(context, term, &Term::Prim(Prim::BinType), expected)?;

            Ok(
                ersd::Prim::IntToStr(erase(context, inner, &Term::Prim(Prim::IntType))?.into())
                    .into(),
            )
        }
        Prim::FltToStr(inner) => {
            expect(context, term, &Term::Prim(Prim::BinType), expected)?;

            Ok(
                ersd::Prim::FltToStr(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::NatToInt(inner) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(
                ersd::Prim::NatToInt(erase(context, inner, &Term::Prim(Prim::NatType))?.into())
                    .into(),
            )
        }
        Prim::IntToNat(inner) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(
                ersd::Prim::IntToNat(erase(context, inner, &Term::Prim(Prim::IntType))?.into())
                    .into(),
            )
        }
        Prim::IntToFlt(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::IntToFlt(erase(context, inner, &Term::Prim(Prim::IntType))?.into())
                    .into(),
            )
        }
        Prim::NatToFlt(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(
                ersd::Prim::NatToFlt(erase(context, inner, &Term::Prim(Prim::NatType))?.into())
                    .into(),
            )
        }
        Prim::FltToInt(inner) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(
                ersd::Prim::FltToInt(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::FltToNat(inner) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(
                ersd::Prim::FltToNat(erase(context, inner, &Term::Prim(Prim::FltType))?.into())
                    .into(),
            )
        }
        Prim::BinType => {
            expect(context, term, &Type.into(), expected)?;
            Ok(ersd::Term::Erased)
        }
        Prim::Bin(bytes) => {
            expect(context, term, &Term::Prim(Prim::BinType), expected)?;
            Ok(ersd::Prim::Bin(bytes.clone()).into())
        }
        Prim::BinLen(bin) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce(context, &bin_type)?;
            match &bin_type_reduced {
                Term::Prim(Prim::BinType) => {}
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            }
            Ok(ersd::Prim::BinLen(erase(context, bin, &bin_type)?.into()).into())
        }
        Prim::BinEql(left, right) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::BinEql(
                erase(context, left, &Term::Prim(Prim::BinType))?.into(),
                erase(context, right, &Term::Prim(Prim::BinType))?.into(),
            )
            .into())
        }
        Prim::BinGet(bin, index) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce(context, &bin_type)?;
            match &bin_type_reduced {
                Term::Prim(Prim::BinType) => {}
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            }
            Ok(ersd::Prim::BinGet(
                erase(context, bin, &bin_type)?.into(),
                erase(context, index, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::BinSlice(bin, start, end) => {
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce(context, &bin_type)?;
            match &bin_type_reduced {
                Term::Prim(Prim::BinType) => {}
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            }
            expect(context, term, &bin_type_reduced, expected)?;
            Ok(ersd::Prim::BinSlice(
                erase(context, bin, &bin_type)?.into(),
                erase(context, start, &Term::Prim(Prim::NatType))?.into(),
                erase(context, end, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::BinAppend(bin, byte) => {
            let bin_type = infer(context, bin)?;
            let bin_type_reduced = reduce(context, &bin_type)?;
            match &bin_type_reduced {
                Term::Prim(Prim::BinType) => {}
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            }
            expect(context, term, &bin_type_reduced, expected)?;
            Ok(ersd::Prim::BinAppend(
                erase(context, bin, &bin_type)?.into(),
                erase(context, byte, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::BinConcat(operands) => {
            expect(context, term, &Term::Prim(Prim::BinType), expected)?;
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &Term::Prim(Prim::BinType)).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::BinConcat(erased).into())
        }
        Prim::ArrType(elem) => {
            expect(context, term, &Type.into(), expected)?;
            erase(context, elem, &Type.into())?;
            Ok(ersd::Term::Erased)
        }
        Prim::Arr(elems) => {
            let expected_reduced = reduce(context, expected)?;
            let Term::Prim(Prim::ArrType(elem_type)) = expected_reduced else {
                return Err(Error::type_mismatch(term.clone(), expected.clone()));
            };
            let erased_elems = elems
                .iter()
                .map(|e| erase(context, e, &elem_type).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::Arr(erased_elems).into())
        }
        Prim::ArrLen(list) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce(context, &list_type)?;
            match &list_type_reduced {
                Term::Prim(Prim::ArrType(_)) => {}
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            }
            Ok(ersd::Prim::ArrLen(erase(context, list, &list_type)?.into()).into())
        }
        Prim::ArrGet(list, index) => {
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce(context, &list_type)?;
            let elem_type = match list_type_reduced {
                Term::Prim(Prim::ArrType(elem)) => *elem,
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            };
            expect(context, term, &elem_type, expected)?;
            Ok(ersd::Prim::ArrGet(
                erase(context, list, &list_type)?.into(),
                erase(context, index, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::ArrSlice(list, start, end) => {
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce(context, &list_type)?;
            match &list_type_reduced {
                Term::Prim(Prim::ArrType(_)) => {}
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            }
            expect(context, term, &list_type_reduced, expected)?;
            Ok(ersd::Prim::ArrSlice(
                erase(context, list, &list_type)?.into(),
                erase(context, start, &Term::Prim(Prim::NatType))?.into(),
                erase(context, end, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::ArrAppend(list, elem) => {
            let list_type = infer(context, list)?;
            let list_type_reduced = reduce(context, &list_type)?;
            let elem_type = match list_type_reduced {
                Term::Prim(Prim::ArrType(e)) => *e,
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            };
            expect(
                context,
                term,
                &Term::Prim(Prim::arr_type(elem_type.clone())),
                expected,
            )?;
            Ok(ersd::Prim::ArrAppend(
                erase(context, list, &list_type)?.into(),
                erase(context, elem, &elem_type)?.into(),
            )
            .into())
        }
        Prim::ArrConcat(operands) => {
            let expected_reduced = reduce(context, expected)?;
            match &expected_reduced {
                Term::Prim(Prim::ArrType(_)) => {}
                _ => return Err(Error::type_mismatch(term.clone(), expected.clone())),
            }
            let erased = operands
                .iter()
                .map(|e| erase(context, e, &expected_reduced).map(|t| t.into()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ersd::Prim::ArrConcat(erased).into())
        }
        Prim::SysPrint(inner) => {
            expect(context, term, &Term::AtomType(AtomType::new(["unit"])), expected)?;
            Ok(
                ersd::Prim::SysPrint(erase(context, inner, &Term::Prim(Prim::BinType))?.into())
                    .into(),
            )
        }
    }
}

fn erase_func(
    context: &mut Context,
    func: &Func,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Func { body } = func;

    let Term::FuncType(FuncType { input, output }) = reduce(context, expected)? else {
        return Err(Error::type_mismatch(term.clone(), expected.clone()));
    };

    let captures = body.free_vars().into_iter().collect::<Vec<_>>();
    let param = context.fresh();
    let param_term = Var::free(&param).into();
    let body = body.open(&[&param_term]);

    let body = context.with_frame(|context| {
        context.assume(&param, &input);

        erase(context, &body, &output.open(&[&param_term]))
    })?;

    Ok(ersd::Func {
        captures,
        param,
        body: body.into(),
    }
    .into())
}

fn erase_apply(
    context: &mut Context,
    apply: &Apply,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Apply { head, param } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    let Term::FuncType(FuncType { input, output }) = &head_type else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let erased = ersd::Apply {
        head: erase(context, head, &head_type)?.into(),
        param: erase(context, param, input)?.into(),
    };

    expect(context, term, &output.open(&[param.as_ref()]), expected)?;

    Ok(erased.into())
}

fn erase_tuple(context: &mut Context, tuple: &Tuple, expected: &Term) -> Result<ersd::Term, Error> {
    let Tuple { fields } = tuple;

    let type_fields = if let Term::TupleType(TupleType { fields: tf }) = reduce(context, expected)?
    {
        tf
    } else {
        return Err(Error::type_mismatch(tuple.clone(), expected.clone()));
    };

    if fields.len() != type_fields.len() {
        return Err(Error::type_mismatch(tuple.clone(), expected.clone()));
    }

    let mut checked_terms = Vec::<&Term>::new();
    let mut erased_fields = Vec::<ersd::Subterm>::new();

    for (i, field) in fields.iter().enumerate() {
        let field_type = type_fields[i].open(&checked_terms);
        erased_fields.push(erase(context, field, &field_type)?.into());
        checked_terms.push(field.as_ref());
    }

    Ok(ersd::Tuple {
        fields: erased_fields,
    }
    .into())
}

fn erase_nat_fold(
    context: &mut Context,
    nat_fold: &NatFold,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let NatFold {
        head,
        motive,
        zero_case,
        succ_case,
    } = nat_fold;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    if !matches!(head_type, Term::Prim(Prim::NatType)) {
        return Err(Error::cannot_infer(term.clone()));
    }

    let head_label = context.fresh();

    context.with_frame(|context| {
        context.assume(&head_label, &Term::Prim(Prim::NatType));

        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
    })?;

    let erased_zero_case = erase(
        context,
        zero_case,
        &motive.open(&[&Term::Prim(Prim::Nat(0))]),
    )?;

    let pred_label = context.fresh();
    let ih_label = context.fresh();

    let erased_succ_case = context.with_frame(|context| {
        context.assume(&pred_label, &Term::Prim(Prim::NatType));
        context.assume(&ih_label, &motive.open(&[&Var::free(&pred_label).into()]));

        erase(
            context,
            &succ_case.open(&[&Var::free(&pred_label).into(), &Var::free(&ih_label).into()]),
            &motive.open(&[&Term::Prim(Prim::nat_add(
                Var::free(&pred_label),
                Prim::Nat(1),
            ))]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

    Ok(ersd::NatFold {
        head: erased_head.into(),
        zero_case: erased_zero_case.into(),
        pred: pred_label,
        ih: ih_label,
        succ_case: erased_succ_case.into(),
    }
    .into())
}

fn erase_nat_match(
    context: &mut Context,
    nm: &NatMatch,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let NatMatch {
        head,
        motive,
        cases,
        default,
    } = nm;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    if !matches!(head_type, Term::Prim(Prim::NatType)) {
        return Err(Error::cannot_infer(term.clone()));
    }

    let head_label = context.fresh();

    context.with_frame(|context| {
        context.assume(&head_label, &Term::Prim(Prim::NatType));
        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
    })?;

    let erased_cases = cases
        .iter()
        .map(|(n, body)| {
            let case_expected = motive.open(&[&Term::Prim(Prim::Nat(*n))]);
            erase(context, body, &case_expected).map(|e| (*n, e.into()))
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let erased_default = erase(context, default, &motive.open(&[head.as_ref()]))?;

    let erased_head = erase(context, head, &head_type)?;

    expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

    Ok(ersd::NatMatch {
        head: erased_head.into(),
        cases: erased_cases,
        default: erased_default.into(),
    }
    .into())
}

fn erase_proj(
    context: &mut Context,
    proj: &Proj,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Proj { head, index } = proj;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    let TupleType { fields } = if let Term::TupleType(tt) = &head_type {
        tt.clone()
    } else {
        return Err(Error::cannot_infer(term.clone()));
    };

    if *index >= fields.len() {
        return Err(Error::cannot_infer(term.clone()));
    }

    let prefix: Vec<Term> = (0..*index)
        .map(|j| Proj::new(*head.clone(), j).into())
        .collect();
    let prefix_refs: Vec<&Term> = prefix.iter().collect();
    let field_type = fields[*index].open(&prefix_refs);

    expect(context, term, &field_type, expected)?;

    Ok(ersd::Proj {
        head: erase(context, head, &head_type)?.into(),
        index: *index,
    }
    .into())
}

fn erase_atom(
    context: &mut Context,
    atom: &super::Atom,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Term::AtomType(AtomType { atoms }) = reduce(context, expected)? else {
        return Err(Error::type_mismatch(term.clone(), expected.clone()));
    };

    if atoms.len() == 1 {
        atoms
            .iter()
            .position(|candidate| candidate == atom)
            .ok_or_else(|| Error::type_mismatch(term.clone(), expected.clone()))?;

        return Ok(ersd::Prim::Unit.into());
    }

    let index = atoms
        .iter()
        .position(|candidate| candidate == atom)
        .ok_or_else(|| Error::type_mismatch(term.clone(), expected.clone()))?;

    Ok(ersd::Atom { index }.into())
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
    let head_type = reduce(context, &head_type)?;

    let atoms = if let Term::AtomType(AtomType { atoms }) = &head_type {
        atoms.clone()
    } else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let head_label = context.fresh();

    context.with_frame(|context| {
        context.assume(&head_label, &AtomType::new(atoms.iter().cloned()).into());

        erase(
            context,
            &motive.open(&[&Var::free(head_label).into()]),
            &Type.into(),
        )
    })?;

    if cases.len() != atoms.len() {
        return Err(Error::cannot_infer(term.clone()));
    }

    let canonical = reduce(context, head.as_ref())?;

    if atoms.len() == 1 {
        let atom = atoms.iter().next().unwrap();

        let body = cases.get(atom).ok_or_else(|| Error::cannot_infer(term.clone()))?;
        let expected_body = motive.open(&[&atom.clone().into()]);

        erase(context, head, &head_type)?;
        expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

        return context.with_frame(|context| {
            match &canonical {
                Term::Var(var) => {
                    context.define(var.unwrap(), &atom.clone().into());
                }
                Term::Proj(Proj { head: base, index }) => {
                    context.define_proj((**base).clone(), *index, &atom.clone().into());
                }
                _ => {}
            }

            erase(context, body, &expected_body)
        });
    }

    let cases = atoms
        .iter()
        .map(|atom| {
            let body = if let Some(body) = cases.get(atom) {
                body
            } else {
                return Err(Error::cannot_infer(term.clone()));
            };

            let expected = motive.open(&[&atom.clone().into()]);

            context.with_frame(|context| {
                match &canonical {
                    Term::Var(var) => {
                        context.define(var.unwrap(), &atom.clone().into());
                    }
                    Term::Proj(Proj { head: base, index }) => {
                        context.define_proj((**base).clone(), *index, &atom.clone().into());
                    }
                    _ => {}
                }

                erase(context, body, &expected).map(Into::into)
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

    Ok(ersd::Match {
        head: erase(context, head, &head_type)?.into(),
        cases,
    }
    .into())
}

fn erase_sealed(
    context: &mut Context,
    sealed: &Sealed,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Sealed { witness, tail } = sealed;
    erase(context, witness, &Type.into())?;
    let label = context.fresh();
    let tail = tail.open(&[&Var::free(&label).into()]);
    context.with_frame(|context| {
        context.seal(&label, witness);
        erase(context, &tail, expected)
    })
}

fn erase_seal(
    context: &mut Context,
    seal: &Seal,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Seal { witness, value } = seal;
    let witness_reduced = reduce(context, witness)?;
    let Term::Var(var) = &witness_reduced else {
        return Err(Error::cannot_infer(term.clone()));
    };
    let repr = context
        .witness(var.unwrap())
        .ok_or_else(|| Error::cannot_infer(term.clone()))?
        .clone();
    expect(context, term, &witness_reduced, expected)?;
    erase(context, value, &repr)
}

fn erase_unseal(
    context: &mut Context,
    unseal: &Unseal,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Unseal { witness, value } = unseal;
    let witness_reduced = reduce(context, witness)?;
    let Term::Var(var) = &witness_reduced else {
        return Err(Error::cannot_infer(term.clone()));
    };
    let repr = context
        .witness(var.unwrap())
        .ok_or_else(|| Error::cannot_infer(term.clone()))?
        .clone();
    expect(context, term, &repr, expected)?;
    erase(context, value, &witness_reduced)
}

fn erase_let(context: &mut Context, let_: &Let, expected: &Term) -> Result<ersd::Term, Error> {
    let Let {
        type_: body_type,
        body,
        tail,
    } = let_;

    erase(context, body_type, &Type.into())?;

    let name = context.fresh();
    let erased_body = erase(context, body, body_type)?;
    let var_term = Var::free(&name).into();
    let tail = tail.open(&[&var_term]);

    let tail = context.with_frame(|context| {
        context.define_assuming(&name, body_type, body);

        erase(context, &tail, expected)
    })?;

    Ok(ersd::Let {
        name,
        body: erased_body.into(),
        tail: tail.into(),
    }
    .into())
}

fn erase_rec(context: &mut Context, rec: &Rec, expected: &Term) -> Result<ersd::Term, Error> {
    let Rec { items, tail } = rec;

    let names = (0..items.len())
        .map(|_| context.fresh())
        .collect::<Vec<_>>();

    let label_terms = names
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

    let erased = context.with_frame(|context| {
        for (name, (type_, _)) in names.iter().zip(items.iter()) {
            context.assume(name, type_);
        }

        for (type_, _) in &items {
            erase(context, type_, &Type.into())?;
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

    Ok(erased.into())
}

pub fn erase(context: &mut Context, term: &Term, expected: &Term) -> Result<ersd::Term, Error> {
    match term {
        Term::Prim(prim) => erase_prim(context, term, prim, expected),
        Term::NatFold(nat_fold) => erase_nat_fold(context, nat_fold, term, expected),
        Term::NatMatch(nm) => erase_nat_match(context, nm, term, expected),
        Term::Type => {
            expect(context, term, &Type.into(), expected)?;
            Ok(ersd::Term::Erased)
        }
        Term::FuncType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Term::Func(func) => erase_func(context, func, term, expected),
        Term::Apply(apply) => erase_apply(context, apply, term, expected),
        Term::TupleType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Term::Tuple(tuple) => erase_tuple(context, tuple, expected),
        Term::Proj(proj) => erase_proj(context, proj, term, expected),
        Term::AtomType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Term::Atom(atom) => erase_atom(context, atom, term, expected),
        Term::Match(m) => erase_match(context, m, term, expected),
        Term::Let(let_) => erase_let(context, let_, expected),
        Term::Rec(lr) => erase_rec(context, lr, expected),
        Term::Sealed(sealed) => erase_sealed(context, sealed, expected),
        Term::Seal(seal) => erase_seal(context, seal, term, expected),
        Term::Unseal(unseal) => erase_unseal(context, unseal, term, expected),
        Term::Var(var) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Name::from(var.unwrap()).into())
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::{
            core::{
                Atom, AtomType, Flt, Func, FuncType, Match, NatFold, NatMatch, Prim, Rec, Seal,
                Sealed, Term, Tuple, TupleType, Type, Unseal, Var,
            },
            ersd, text,
        },
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_secs(1))
    }

    #[test]
    fn erase_dependent_tuple_type_over_atom_match_and_tuple_value() {
        let mut context = context();

        let tuple_type = Term::from(TupleType::new([
            ("x", Term::from(AtomType::new(["left", "right"]))),
            (
                "y",
                Term::from(Match::new(
                    Var::free("x"),
                    "m",
                    Type,
                    vec![
                        ("left", AtomType::new(["hot"])),
                        ("right", AtomType::new(["cold"])),
                    ],
                )),
            ),
        ]));

        erase(&mut context, &tuple_type, &Type.into()).unwrap();

        let tuple = Term::from(Tuple::new([Atom::from("left"), Atom::from("hot")]));

        erase(&mut context, &tuple, &tuple_type).unwrap();

        let tuple = Term::from(Tuple::new([Atom::from("right"), Atom::from("cold")]));

        erase(&mut context, &tuple, &tuple_type).unwrap();
    }

    #[test]
    fn erase_dependent_tuple_type_rejects_wrong_branch_atom() {
        let mut context = context();

        let tuple_type = Term::from(TupleType::new([
            ("x", Term::from(AtomType::new(["left", "right"]))),
            (
                "y",
                Term::from(Match::new(
                    Var::free("x"),
                    "m",
                    Type,
                    vec![
                        ("left", AtomType::new(["hot"])),
                        ("right", AtomType::new(["cold"])),
                    ],
                )),
            ),
        ]));

        let tuple = Term::from(Tuple::new([Atom::from("left"), Atom::from("cold")]));

        assert!(matches!(
            erase(&mut context, &tuple, &tuple_type),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_match_singleton_lowers_to_body() {
        let type_ = text::to_core(&"'[yes, no]".parse().unwrap(), &text::PanicLoader);

        let term = text::to_core(
            &r#"
                let x : '[unit] = 'unit;
                match x : _ => '[yes, no];
                | 'unit => 'yes;
            "#
            .parse()
            .unwrap(),
            &text::PanicLoader,
        );

        let erased = erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();

        let ersd::Term::Let(ersd::Let { body, tail, .. }) = erased else {
            panic!("expected let");
        };

        assert!(matches!(*body, ersd::Term::Prim(ersd::Prim::Unit)));

        // singleton match lowers directly to the branch body, not ersd::Match
        assert!(!matches!(*tail, ersd::Term::Match(_)));
        assert!(matches!(*tail, ersd::Term::Atom(ersd::Atom { index: 1 })));
    }

    #[test]
    fn erase_rec_single_identity_function() {
        let mut context = context();

        let func_type = Term::from(FuncType::new(
            "x",
            AtomType::new(["a"]),
            AtomType::new(["a"]),
        ));

        let term = Term::from(Rec::new(
            vec![("f", func_type.clone(), Func::new("x", Var::free("x")))],
            Var::free("f"),
        ));

        erase(&mut context, &term, &func_type).unwrap();
    }

    #[test]
    fn erase_preempts_on_cyclic_expected_type() {
        let mut context = context();

        context.define("loop", &Var::free("loop").into());

        assert!(matches!(
            erase(&mut context, &Type.into(), &Var::free("loop").into()),
            Err(Error::ConvertPreempted { .. })
        ));
    }

    #[test]
    fn erase_accepts_term_level_loop_with_stable_type() {
        let mut context = context();

        let type_ = Term::from(AtomType::new(["a"]));

        let term = Term::from(Rec::new(
            vec![("loop", type_.clone(), Var::free("loop"))],
            Var::free("loop"),
        ));

        erase(&mut context, &term, &type_).unwrap();
    }

    #[test]
    fn erase_prim_ops_typecheck() {
        let mut context = context();

        erase(
            &mut context,
            &Term::Prim(Prim::int_eql(
                Term::Prim(Prim::Int(1)),
                Term::Prim(Prim::Int(1)),
            )),
            &Term::Prim(Prim::NatType),
        )
        .unwrap();

        erase(
            &mut context,
            &Term::Prim(Prim::flt_add(
                Term::Prim(Prim::Flt(Flt::from_f32(1.5))),
                Term::Prim(Prim::Flt(Flt::from_f32(2.0))),
            )),
            &Term::Prim(Prim::FltType),
        )
        .unwrap();
    }

    #[test]
    fn erase_func_captures_free_variables_before_opening_body() {
        let atom_type = Term::from(AtomType::new(["a"]));
        let tuple_type = Term::from(TupleType::new([
            ("z", atom_type.clone()),
            ("w", atom_type.clone()),
        ]));
        let type_ = Term::from(FuncType::new("x", atom_type.clone(), tuple_type));
        let term = Term::from(Func::new(
            "x",
            Tuple::new([Term::from(Var::free("x")), Term::from(Var::free("y"))]),
        ));

        let mut context = Context::new(Duration::from_secs(1));
        context.assume("y", &atom_type);

        let erased = erase(&mut context, &term, &type_).unwrap();

        let ersd::Term::Func(ersd::Func { captures, .. }) = erased else {
            panic!("expected erased func");
        };

        assert_eq!(captures.len(), 1);
        assert!(captures.contains(&"y".to_string()));
    }

    #[test]
    fn erase_rejects_wrong_prim_operand_types() {
        assert!(matches!(
            erase(
                &mut Context::new(Duration::from_secs(1)),
                &Term::Prim(Prim::int_add(
                    Term::Prim(Prim::Int(1)),
                    Term::Prim(Prim::Flt(Flt::from_f32(2.0)))
                )),
                &Term::Prim(Prim::IntType),
            ),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_match_and_atom_stress_test() {
        let type_ = text::to_core(&"'[zeta, alpha, mu]".parse().unwrap(), &text::PanicLoader);

        let term = text::to_core(
            &r#"
                let outer : '[zeta, alpha, mu] = 'mu;
                let alpha_case : '[zeta, alpha, mu] = 'alpha;
                let mu_case : '[zeta, alpha, mu] = 'mu;
                let zeta_case : '[zeta, alpha, mu] = 'zeta;
                match outer : subject => '[zeta, alpha, mu];
                | 'zeta =>
                    match alpha_case : nested => '[zeta, alpha, mu];
                    | 'zeta => 'alpha;
                    | 'alpha => 'mu;
                    | 'mu => 'zeta;;
                | 'alpha =>
                    match zeta_case : nested => '[zeta, alpha, mu];
                    | 'zeta => 'mu;
                    | 'alpha => 'zeta;
                    | 'mu => 'alpha;;
                | 'mu =>
                    match mu_case : nested => '[zeta, alpha, mu];
                    | 'zeta => 'zeta;
                    | 'alpha => 'alpha;
                    | 'mu => 'mu;;
            "#
            .parse()
            .unwrap(),
            &text::PanicLoader,
        );

        let erased = erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();

        let ersd::Term::Let(ersd::Let {
            name: outer_name,
            body: outer_body,
            tail,
        }) = erased
        else {
            panic!("expected outer let");
        };

        assert_eq!(outer_name, "0");
        assert!(matches!(
            *outer_body,
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        let ersd::Term::Let(ersd::Let {
            name: alpha_name,
            body: alpha_body,
            tail,
        }) = *tail
        else {
            panic!("expected alpha_case let");
        };

        assert_eq!(alpha_name, "1");
        assert!(matches!(
            *alpha_body,
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));

        let ersd::Term::Let(ersd::Let {
            name: mu_name,
            body: mu_body,
            tail,
        }) = *tail
        else {
            panic!("expected mu_case let");
        };

        assert_eq!(mu_name, "2");
        assert!(matches!(
            *mu_body,
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        let ersd::Term::Let(ersd::Let {
            name: zeta_name,
            body: zeta_body,
            tail,
        }) = *tail
        else {
            panic!("expected zeta_case let");
        };

        assert_eq!(zeta_name, "3");
        assert!(matches!(
            *zeta_body,
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));

        let ersd::Term::Match(ersd::Match { head, cases }) = *tail else {
            panic!("expected outer erased case");
        };

        assert!(matches!(
            *head,
            ersd::Term::Name(name) if name.as_str() == "0"
        ));

        assert_eq!(cases.len(), 3);

        let ersd::Term::Match(ersd::Match {
            head: alpha_head,
            cases: alpha_cases,
        }) = &*cases[0]
        else {
            panic!("expected nested case for 'alpha case");
        };

        assert!(matches!(
            &**alpha_head,
            ersd::Term::Name(name) if name.as_str() == "3"
        ));

        assert_eq!(alpha_cases.len(), 3);
        assert!(matches!(
            *alpha_cases[0],
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));
        assert!(matches!(
            *alpha_cases[1],
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));
        assert!(matches!(
            *alpha_cases[2],
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        let ersd::Term::Match(ersd::Match {
            head: mu_head,
            cases: mu_cases,
        }) = &*cases[1]
        else {
            panic!("expected nested case for 'mu case");
        };

        assert!(matches!(
            &**mu_head,
            ersd::Term::Name(name) if name.as_str() == "2"
        ));

        assert_eq!(mu_cases.len(), 3);

        assert!(matches!(
            *mu_cases[0],
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));

        assert!(matches!(
            *mu_cases[1],
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        assert!(matches!(
            *mu_cases[2],
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));

        let ersd::Term::Match(ersd::Match {
            head: zeta_head,
            cases: zeta_cases,
        }) = &*cases[2]
        else {
            panic!("expected nested case for 'zeta case");
        };

        assert!(matches!(
            &**zeta_head,
            ersd::Term::Name(name) if name.as_str() == "1"
        ));

        assert_eq!(zeta_cases.len(), 3);

        assert!(matches!(
            *zeta_cases[0],
            ersd::Term::Atom(ersd::Atom { index: 1 })
        ));

        assert!(matches!(
            *zeta_cases[1],
            ersd::Term::Atom(ersd::Atom { index: 2 })
        ));

        assert!(matches!(
            *zeta_cases[2],
            ersd::Term::Atom(ersd::Atom { index: 0 })
        ));
    }

    #[test]
    fn erase_arr_nat_type_literal_len_and_get() {
        let mut context = context();

        let arr_nat = Term::Prim(Prim::arr_type(Term::Prim(Prim::NatType)));
        erase(&mut context, &arr_nat, &Type.into()).unwrap();

        let literal = Term::Prim(Prim::from(vec![
            Term::Prim(Prim::Nat(1)),
            Term::Prim(Prim::Nat(2)),
        ]));
        erase(&mut context, &literal, &arr_nat).unwrap();

        context.assume("xs", &arr_nat);
        let len = Term::Prim(Prim::arr_len(Var::free("xs")));
        assert_eq!(
            infer(&mut context, &len).unwrap(),
            Term::Prim(Prim::NatType)
        );

        let get = Term::Prim(Prim::arr_get(Var::free("xs"), Term::Prim(Prim::Nat(0))));
        assert_eq!(
            infer(&mut context, &get).unwrap(),
            Term::Prim(Prim::NatType)
        );
    }

    #[test]
    fn erase_bin_type_literal_len_and_get() {
        let mut context = context();

        let bin_type = Term::Prim(Prim::BinType);
        erase(&mut context, &bin_type, &Type.into()).unwrap();

        let literal = Term::Prim(Prim::Bin(vec![1, 2, 3]));
        assert_eq!(infer(&mut context, &literal).unwrap(), bin_type);
        erase(&mut context, &literal, &bin_type).unwrap();

        context.assume("b", &bin_type);
        let len = Term::Prim(Prim::bin_len(Var::free("b")));
        assert_eq!(
            infer(&mut context, &len).unwrap(),
            Term::Prim(Prim::NatType)
        );

        let get = Term::Prim(Prim::bin_get(Var::free("b"), Term::Prim(Prim::Nat(0))));
        assert_eq!(
            infer(&mut context, &get).unwrap(),
            Term::Prim(Prim::NatType)
        );
    }

    #[test]
    fn erase_bin_append() {
        let mut context = context();

        let bin_type = Term::Prim(Prim::BinType);
        context.assume("b", &bin_type);
        context.assume("n", &Term::Prim(Prim::NatType));

        let append = Term::Prim(Prim::bin_append(Var::free("b"), Var::free("n")));
        assert_eq!(infer(&mut context, &append).unwrap(), bin_type);
        erase(&mut context, &append, &bin_type).unwrap();
    }

    #[test]
    fn erase_bin_eql() {
        let mut context = context();

        let bin_type = Term::Prim(Prim::BinType);
        let nat_type = Term::Prim(Prim::NatType);
        context.assume("a", &bin_type);
        context.assume("b", &bin_type);

        let eql = Term::Prim(Prim::bin_eql(Var::free("a"), Var::free("b")));
        assert_eq!(infer(&mut context, &eql).unwrap(), nat_type);
        erase(&mut context, &eql, &nat_type).unwrap();
    }

    #[test]
    fn erase_nat_fold_nat_to_bool_atom() {
        let mut context = context();

        let bool_type = Term::from(AtomType::new(["false", "true"]));

        let nat_fold_zero = Term::from(NatFold::new(
            Prim::Nat(0),
            "m",
            AtomType::new(["false", "true"]),
            Atom::from("false"),
            "pred",
            "ih",
            Atom::from("true"),
        ));

        let nat_fold_one = Term::from(NatFold::new(
            Prim::Nat(1),
            "m",
            AtomType::new(["false", "true"]),
            Atom::from("false"),
            "pred",
            "ih",
            Atom::from("true"),
        ));

        erase(&mut context, &nat_fold_zero, &bool_type).unwrap();
        erase(&mut context, &nat_fold_one, &bool_type).unwrap();
    }

    #[test]
    fn erase_nat_fold_rejects_non_nat_head() {
        let mut context = context();

        let bool_type = Term::from(AtomType::new(["false", "true"]));

        let nat_fold = Term::from(NatFold::new(
            Prim::Int(1),
            "m",
            AtomType::new(["false", "true"]),
            Atom::from("false"),
            "pred",
            "ih",
            Atom::from("true"),
        ));

        assert!(matches!(
            erase(&mut context, &nat_fold, &bool_type),
            Err(Error::CannotInfer { .. })
        ));
    }

    #[test]
    fn erase_nat_match_dispatches_to_named_case() {
        let mut context = context();

        let bool_type = Term::from(AtomType::new(["false", "true"]));

        let nat_match = Term::from(NatMatch::new(
            Prim::Nat(5),
            "m",
            AtomType::new(["false", "true"]),
            [(5u32, Term::from(Atom::from("true")))],
            Atom::from("false"),
        ));

        erase(&mut context, &nat_match, &bool_type).unwrap();
    }

    #[test]
    fn erase_nat_match_rejects_non_nat_head() {
        let mut context = context();

        let bool_type = Term::from(AtomType::new(["false", "true"]));

        let nat_match = Term::from(NatMatch::new(
            Prim::Int(0),
            "m",
            AtomType::new(["false", "true"]),
            [(0u32, Term::from(Atom::from("true")))],
            Atom::from("false"),
        ));

        assert!(matches!(
            erase(&mut context, &nat_match, &bool_type),
            Err(Error::CannotInfer { .. })
        ));
    }

    #[test]
    fn erase_lst_append() {
        let mut context = context();

        let arr_nat = Term::Prim(Prim::arr_type(Term::Prim(Prim::NatType)));
        context.assume("xs", &arr_nat);
        context.assume("n", &Term::Prim(Prim::NatType));

        let append = Term::Prim(Prim::arr_append(Var::free("xs"), Var::free("n")));
        assert_eq!(infer(&mut context, &append).unwrap(), arr_nat);
        erase(&mut context, &append, &arr_nat).unwrap();
    }

    #[test]
    fn erase_three_field_tuple_type_and_value() {
        let mut context = context();

        let tuple_type = Term::from(TupleType::new([
            ("x", Term::from(AtomType::new(["a"]))),
            ("y", Term::from(AtomType::new(["b"]))),
            ("z", Term::from(AtomType::new(["c"]))),
        ]));

        erase(&mut context, &tuple_type, &Type.into()).unwrap();

        let tuple = Term::from(Tuple::new([
            Term::from(Atom::from("a")),
            Term::from(Atom::from("b")),
            Term::from(Atom::from("c")),
        ]));

        erase(&mut context, &tuple, &tuple_type).unwrap();
    }

    #[test]
    fn erase_bin_concat() {
        let mut context = context();

        let bin_type = Term::Prim(Prim::BinType);
        let concat = Term::Prim(Prim::bin_concat([
            Term::Prim(Prim::Bin(vec![1, 2])),
            Term::Prim(Prim::Bin(vec![3, 4])),
        ]));

        erase(&mut context, &concat, &bin_type).unwrap();
    }

    #[test]
    fn erase_bin_concat_rejects_wrong_expected_type() {
        let mut context = context();

        let concat = Term::Prim(Prim::bin_concat([
            Term::Prim(Prim::Bin(vec![1])),
            Term::Prim(Prim::Bin(vec![2])),
        ]));

        assert!(matches!(
            erase(&mut context, &concat, &Term::Prim(Prim::NatType)),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_arr_concat() {
        let mut context = context();

        let arr_nat = Term::Prim(Prim::arr_type(Term::Prim(Prim::NatType)));
        context.assume("xs", &arr_nat);
        context.assume("ys", &arr_nat);

        let concat = Term::Prim(Prim::arr_concat([Var::free("xs"), Var::free("ys")]));

        erase(&mut context, &concat, &arr_nat).unwrap();
    }

    #[test]
    fn erase_arr_concat_rejects_wrong_expected_type() {
        let mut context = context();

        let arr_nat = Term::Prim(Prim::arr_type(Term::Prim(Prim::NatType)));
        context.assume("xs", &arr_nat);
        context.assume("ys", &arr_nat);

        let concat = Term::Prim(Prim::arr_concat([Var::free("xs"), Var::free("ys")]));

        assert!(matches!(
            erase(&mut context, &concat, &Term::Prim(Prim::NatType)),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn seal_and_unseal_non_recursive() {
        let mut context = context();

        let term = Term::from(Sealed::new(
            "x",
            Term::Prim(Prim::NatType),
            Unseal::new(
                Var::free("x"),
                Seal::new(Var::free("x"), Term::Prim(Prim::Nat(42))),
            ),
        ));

        erase(&mut context, &term, &Term::Prim(Prim::NatType)).unwrap();
    }

    #[test]
    fn unseal_recovers_repr() {
        let mut context = context();

        let term = Term::from(Sealed::new(
            "x",
            Term::Prim(Prim::NatType),
            Unseal::new(
                Var::free("x"),
                Seal::new(Var::free("x"), Term::Prim(Prim::Nat(42))),
            ),
        ));

        let result = erase(&mut context, &term, &Term::Prim(Prim::NatType)).unwrap();

        assert!(matches!(result, ersd::Term::Prim(ersd::Prim::Nat(42))));
    }

    #[test]
    fn seal_rejected_for_wrong_opaque_type() {
        let mut context = context();

        context.seal("x", &Term::from(AtomType::new(["a"])));

        let seal = Term::from(Seal::new(Var::free("x"), Term::Prim(Prim::Nat(42))));

        assert!(matches!(
            erase(&mut context, &seal, &Var::free("x").into()),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn seal_erases_to_inner_value() {
        let mut context = context();

        context.seal("x", &Term::Prim(Prim::NatType));

        let seal = Term::from(Seal::new(Var::free("x"), Term::Prim(Prim::Nat(7))));

        let result = erase(&mut context, &seal, &Var::free("x").into()).unwrap();

        assert!(matches!(result, ersd::Term::Prim(ersd::Prim::Nat(7))));
    }
}
