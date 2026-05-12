use {
    super::{
        Apply, AtomType, Context, Error, Func, FuncType, Let, LetRec, Match, Name, Pair, PairType,
        Preempted, Prim, Split, Term, Type,
    },
    crate::ersd,
};

fn reduce(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce(context, term).map_err(|Preempted| Error::reduce_preempted(term.clone()))
}

fn convert(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    super::convert(context, this, that)
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
        Prim::NatValue(_) => Ok(Term::Prim(Prim::NatType)),
        Prim::NatEql(left, right)
        | Prim::NatNeq(left, right)
        | Prim::NatAdd(left, right)
        | Prim::NatSub(left, right)
        | Prim::NatMul(left, right)
        | Prim::NatDiv(left, right)
        | Prim::NatRem(left, right)
        | Prim::NatLt(left, right)
        | Prim::NatGt(left, right)
        | Prim::NatLte(left, right)
        | Prim::NatGte(left, right) => {
            erase(context, left, &Term::Prim(Prim::NatType))?;
            erase(context, right, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::NatType))
        }
        Prim::IntType => Ok(Type.into()),
        Prim::IntValue(_) => Ok(Term::Prim(Prim::IntType)),
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
        Prim::IntNeg(inner) => {
            erase(context, inner, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::IntType))
        }
        Prim::FltType => Ok(Type.into()),
        Prim::FltValue(_) => Ok(Term::Prim(Prim::FltType)),
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
        Prim::NatToInt(inner) => {
            erase(context, inner, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::IntType))
        }
        Prim::IntToNat(inner) => {
            erase(context, inner, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::NatType))
        }
        Prim::IntToFlt(inner) => {
            erase(context, inner, &Term::Prim(Prim::IntType))?;

            Ok(Term::Prim(Prim::FltType))
        }
        Prim::NatToFlt(inner) => {
            erase(context, inner, &Term::Prim(Prim::NatType))?;

            Ok(Term::Prim(Prim::FltType))
        }
        Prim::FltToInt(inner) => {
            erase(context, inner, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::IntType))
        }
        Prim::FltToNat(inner) => {
            erase(context, inner, &Term::Prim(Prim::FltType))?;

            Ok(Term::Prim(Prim::NatType))
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
            &output.open(&[&Name::label(label).into()]),
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

fn infer_pair_type(context: &mut Context, pt: &PairType) -> Result<Term, Error> {
    let PairType { input, output } = pt;

    erase(context, input, &Type.into())?;

    let label = context.fresh();

    context.with_frame(|context| {
        context.assume(&label, input);

        erase(
            context,
            &output.open(&[&Name::label(label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

    Ok(Type.into())
}

fn infer_split(context: &mut Context, split: &Split, term: &Term) -> Result<Term, Error> {
    let Split { head, motive, tail } = split;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    let (input, output) = if let Term::PairType(PairType { input, output }) = head_type {
        (input, output)
    } else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let head_label = context.fresh();

    context.with_frame(|context| {
        context.assume(
            &head_label,
            &PairType {
                input: input.clone(),
                output: output.clone(),
            }
            .into(),
        );

        erase(
            context,
            &motive.open(&[&Name::label(head_label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

    let fst_label = context.fresh();
    let snd_label = context.fresh();

    let type_ = motive.open(&[head.as_ref()]);

    context.with_frame(|context| {
        context.assume(&fst_label, &input);

        context.assume(&snd_label, &output.open(&[&Name::label(&fst_label).into()]));

        erase(
            context,
            &tail.open(&[
                &Name::label(&fst_label).into(),
                &Name::label(&snd_label).into(),
            ]),
            &motive.open(&[&Pair::new(Name::label(fst_label), Name::label(snd_label)).into()]),
        )
        .map(|_| ())
    })?;

    Ok(type_)
}

fn infer_match(context: &mut Context, match_: &Match, term: &Term) -> Result<Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = match_;

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
            &motive.open(&[&Name::label(head_label).into()]),
            &Type.into(),
        )
        .map(|_| ())
    })?;

    if cases.len() != atoms.len() {
        return Err(Error::cannot_infer(term.clone()));
    }

    for atom in &atoms {
        let body = if let Some(body) = cases.get(atom) {
            body
        } else {
            return Err(Error::cannot_infer(term.clone()));
        };

        erase(context, body, &motive.open(&[&atom.clone().into()]))?;
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

        infer(context, &tail.open(&[&Name::label(label).into()]))
    })
}

fn infer_letrec(context: &mut Context, letrec: &LetRec) -> Result<Term, Error> {
    let LetRec { items, tail } = letrec;

    let labels = (0..items.len())
        .map(|_| context.fresh())
        .collect::<Vec<_>>();

    let label_terms = labels
        .iter()
        .map(Name::label)
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

        for (_, (type_, body)) in labels.iter().zip(items.iter()) {
            erase(context, body, type_)?;
        }

        for (label, (_, body)) in labels.iter().zip(items.iter()) {
            context.define(label, body);
        }

        infer(context, &tail)
    })
}

pub fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    match term {
        Term::Type => Ok(Type.into()),
        Term::Prim(prim) => infer_prim(context, prim),
        Term::FuncType(ft) => infer_func_type(context, ft),
        Term::Apply(apply) => infer_apply(context, apply, term),
        Term::PairType(pt) => infer_pair_type(context, pt),
        Term::Split(split) => infer_split(context, split, term),
        Term::AtomType(_) => Ok(Type.into()),
        Term::Match(match_) => infer_match(context, match_, term),
        Term::Let(let_) => infer_let(context, let_),
        Term::LetRec(letrec) => infer_letrec(context, letrec),
        Term::Name(name) => match context.assumption(name.unwrap()) {
            Some(type_) => Ok(type_.clone()),
            None => Err(Error::cannot_infer(name.clone())),
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
        &Prim::NatValue(value) => {
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
        &Prim::IntValue(value) => {
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
        Prim::IntNeg(inner) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::IntNeg(
                erase(context, inner, &Term::Prim(Prim::IntType))?.into(),
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
        &Prim::FltValue(bits) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::Flt(f32::from_bits(bits)).into())
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

            Ok(ersd::Prim::FltNeg(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltAbs(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltAbs(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltSqrt(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltSqrt(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltFloor(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltFloor(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltCeil(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltCeil(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltTrunc(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltTrunc(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltNearest(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::FltNearest(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
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
        Prim::NatToInt(inner) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::NatToInt(
                erase(context, inner, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::IntToNat(inner) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::IntToNat(
                erase(context, inner, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::IntToFlt(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::IntToFlt(
                erase(context, inner, &Term::Prim(Prim::IntType))?.into(),
            )
            .into())
        }
        Prim::NatToFlt(inner) => {
            expect(context, term, &Term::Prim(Prim::FltType), expected)?;

            Ok(ersd::Prim::NatToFlt(
                erase(context, inner, &Term::Prim(Prim::NatType))?.into(),
            )
            .into())
        }
        Prim::FltToInt(inner) => {
            expect(context, term, &Term::Prim(Prim::IntType), expected)?;

            Ok(ersd::Prim::FltToInt(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
        }
        Prim::FltToNat(inner) => {
            expect(context, term, &Term::Prim(Prim::NatType), expected)?;

            Ok(ersd::Prim::FltToNat(
                erase(context, inner, &Term::Prim(Prim::FltType))?.into(),
            )
            .into())
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

    let captures = body.collect().into_iter().collect::<Vec<_>>();
    let param = context.fresh();
    let param_term = Name::label(&param).into();
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

fn erase_pair(context: &mut Context, pair: &Pair, expected: &Term) -> Result<ersd::Term, Error> {
    let Pair { fst, snd } = pair;

    let Term::PairType(PairType { input, output }) = reduce(context, expected)? else {
        return Err(Error::type_mismatch(pair.clone(), expected.clone()));
    };

    Ok(ersd::Pair {
        fst: erase(context, fst, &input)?.into(),
        snd: erase(context, snd, &output.open(&[fst.as_ref()]))?.into(),
    }
    .into())
}

fn erase_split(
    context: &mut Context,
    split: &Split,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Split { head, motive, tail } = split;

    let head_type = infer(context, head)?;
    let head_type = reduce(context, &head_type)?;

    let (input, output) = if let Term::PairType(PairType { input, output }) = &head_type {
        (input.clone(), output.clone())
    } else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let head_label = context.fresh();

    context.with_frame(|context| {
        context.assume(
            &head_label,
            &PairType {
                input: input.clone(),
                output: output.clone(),
            }
            .into(),
        );

        erase(
            context,
            &motive.open(&[&Name::label(head_label).into()]),
            &Type.into(),
        )
    })?;

    let fst = context.fresh();
    let snd = context.fresh();
    let fst_term = Term::from(Name::label(&fst));
    let snd_term = Term::from(Name::label(&snd));
    let tail = tail.open(&[&fst_term, &snd_term]);
    let tail_type = motive.open(&[&Pair::new(Name::label(&fst), Name::label(&snd)).into()]);

    let erased = context.with_frame(|context| {
        context.assume(&fst, &input);
        context.assume(&snd, &output.open(&[&fst_term]));

        Ok::<_, Error>(ersd::Split {
            head: erase(context, head, &head_type)?.into(),
            fst,
            snd,
            tail: erase(context, &tail, &tail_type)?.into(),
        })
    })?;

    expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

    Ok(erased.into())
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

    let index = atoms
        .iter()
        .position(|candidate| candidate == atom)
        .ok_or_else(|| Error::type_mismatch(term.clone(), expected.clone()))?;

    Ok(ersd::Atom { index }.into())
}

fn erase_match(
    context: &mut Context,
    match_: &Match,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = match_;

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
            &motive.open(&[&Name::label(head_label).into()]),
            &Type.into(),
        )
    })?;

    if cases.len() != atoms.len() {
        return Err(Error::cannot_infer(term.clone()));
    }

    let cases = atoms
        .iter()
        .map(|atom| {
            let body = if let Some(body) = cases.get(atom) {
                body
            } else {
                return Err(Error::cannot_infer(term.clone()));
            };

            erase(context, body, &motive.open(&[&atom.clone().into()])).map(Into::into)
        })
        .collect::<Result<Vec<_>, Error>>()?;

    expect(context, term, &motive.open(&[head.as_ref()]), expected)?;

    Ok(ersd::Match {
        head: erase(context, head, &head_type)?.into(),
        cases,
    }
    .into())
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
    let name_term = Name::label(&name).into();
    let tail = tail.open(&[&name_term]);

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

fn erase_letrec(
    context: &mut Context,
    letrec: &LetRec,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let LetRec { items, tail } = letrec;

    let names = (0..items.len())
        .map(|_| context.fresh())
        .collect::<Vec<_>>();

    let label_terms = names
        .iter()
        .map(Name::label)
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

        let erased_items = items
            .iter()
            .map(|(type_, body)| erase(context, body, type_).map(Into::into))
            .collect::<Result<Vec<_>, Error>>()?;

        for (name, (_, body)) in names.iter().zip(items.iter()) {
            context.define(name, body);
        }

        Ok(ersd::LetRec {
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
        Term::PairType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Term::Pair(pair) => erase_pair(context, pair, expected),
        Term::Split(split) => erase_split(context, split, term, expected),
        Term::AtomType(_) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Term::Erased)
        }
        Term::Atom(atom) => erase_atom(context, atom, term, expected),
        Term::Match(match_) => erase_match(context, match_, term, expected),
        Term::Let(let_) => erase_let(context, let_, expected),
        Term::LetRec(lr) => erase_letrec(context, lr, expected),
        Term::Name(name) => {
            let t = infer(context, term)?;
            expect(context, term, &t, expected)?;
            Ok(ersd::Name::from(name.unwrap()).into())
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::{
            core::{Atom, AtomType, Func, FuncType, LetRec, Match, Pair, PairType, Term, Type},
            ersd,
        },
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_secs(1))
    }

    #[test]
    fn erase_dependent_pair_type_over_atom_match_and_pair_value() {
        let mut context = context();

        let pair_type = Term::from(PairType::new(
            "x",
            AtomType::new(["left", "right"]),
            Match::new(
                Name::label("x"),
                "m",
                Type,
                vec![
                    ("left", AtomType::new(["hot"])),
                    ("right", AtomType::new(["cold"])),
                ],
            ),
        ));

        assert!(erase(&mut context, &pair_type, &Type.into()).is_ok());

        let pair = Term::from(Pair::new(Atom::from("left"), Atom::from("hot")));

        assert!(erase(&mut context, &pair, &pair_type).is_ok());

        let pair = Term::from(Pair::new(Atom::from("right"), Atom::from("cold")));

        assert!(erase(&mut context, &pair, &pair_type).is_ok());
    }

    #[test]
    fn erase_dependent_pair_type_rejects_wrong_branch_atom() {
        let mut context = context();

        let pair_type = Term::from(PairType::new(
            "x",
            AtomType::new(["left", "right"]),
            Match::new(
                Name::label("x"),
                "m",
                Type,
                vec![
                    ("left", AtomType::new(["hot"])),
                    ("right", AtomType::new(["cold"])),
                ],
            ),
        ));

        let pair = Term::from(Pair::new(Atom::from("left"), Atom::from("cold")));

        assert!(matches!(
            erase(&mut context, &pair, &pair_type),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_letrec_single_identity_function() {
        let mut context = context();

        let func_type = Term::from(FuncType::new(
            "x",
            AtomType::new(["a"]),
            AtomType::new(["a"]),
        ));

        let term = Term::from(LetRec::new(
            vec![("f", func_type.clone(), Func::new("x", Name::label("x")))],
            Name::label("f"),
        ));

        assert!(erase(&mut context, &term, &func_type).is_ok());
    }

    #[test]
    fn erase_preempts_on_cyclic_expected_type() {
        let mut context = context();

        context.define("loop", &Name::label("loop").into());

        assert!(matches!(
            erase(&mut context, &Type.into(), &Name::label("loop").into()),
            Err(Error::ConvertPreempted { .. })
        ));
    }

    #[test]
    fn erase_accepts_term_level_loop_with_stable_type() {
        let mut context = context();

        let type_ = Term::from(AtomType::new(["a"]));

        let term = Term::from(LetRec::new(
            vec![("loop", type_.clone(), Name::label("loop"))],
            Name::label("loop"),
        ));

        assert!(erase(&mut context, &term, &type_).is_ok());
    }

    #[test]
    fn erase_prim_ops_typecheck() {
        let mut context = context();

        assert!(
            erase(
                &mut context,
                &Term::Prim(Prim::int_eql(
                    Term::Prim(Prim::IntValue(1)),
                    Term::Prim(Prim::IntValue(1))
                )),
                &Term::Prim(Prim::NatType),
            )
            .is_ok()
        );

        assert!(
            erase(
                &mut context,
                &Term::Prim(Prim::flt_add(
                    Term::Prim(Prim::FltValue(1.5_f32.to_bits())),
                    Term::Prim(Prim::FltValue(2.0_f32.to_bits()))
                )),
                &Term::Prim(Prim::FltType),
            )
            .is_ok()
        );
    }

    #[test]
    fn erase_func_captures_free_variables_before_opening_body() {
        let atom_type = Term::from(AtomType::new(["a"]));
        let pair_type = Term::from(PairType::new("z", atom_type.clone(), atom_type.clone()));
        let type_ = Term::from(FuncType::new("x", atom_type.clone(), pair_type));
        let term = Term::from(Func::new(
            "x",
            Pair::new(Name::label("x"), Name::label("y")),
        ));

        let mut context = Context::new(Duration::from_secs(1));
        context.assume("y", &atom_type);

        erase(&mut context, &term, &type_).unwrap();

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
                    Term::Prim(Prim::IntValue(1)),
                    Term::Prim(Prim::FltValue(2.0_f32.to_bits()))
                )),
                &Term::Prim(Prim::IntType),
            ),
            Err(Error::TypeMismatch { .. })
        ));
    }

    #[test]
    fn erase_match_and_atom_stress_test() {
        let type_ = "'[zeta, alpha, mu]".parse().unwrap();

        let term = r#"
                let outer : '[zeta, alpha, mu] = 'mu;
                let alpha_case : '[zeta, alpha, mu] = 'alpha;
                let mu_case : '[zeta, alpha, mu] = 'mu;
                let zeta_case : '[zeta, alpha, mu] = 'zeta;
                match outer with subject => '[zeta, alpha, mu];
                case 'zeta =>
                    match alpha_case with nested => '[zeta, alpha, mu];
                    case 'zeta => 'alpha;
                    case 'alpha => 'mu;
                    case 'mu => 'zeta;;
                case 'alpha =>
                    match zeta_case with nested => '[zeta, alpha, mu];
                    case 'zeta => 'mu;
                    case 'alpha => 'zeta;
                    case 'mu => 'alpha;;
                case 'mu =>
                    match mu_case with nested => '[zeta, alpha, mu];
                    case 'zeta => 'zeta;
                    case 'alpha => 'alpha;
                    case 'mu => 'mu;;
            "#
        .parse()
        .unwrap();

        erase(&mut Context::new(Duration::from_secs(1)), &term, &type_).unwrap();
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
            panic!("expected outer erased match");
        };

        assert!(matches!(
            *head,
            ersd::Term::Name(ersd::Name { string }) if string == "0"
        ));

        assert_eq!(cases.len(), 3);

        let ersd::Term::Match(ersd::Match {
            head: alpha_head,
            cases: alpha_cases,
        }) = &*cases[0]
        else {
            panic!("expected nested match for 'alpha case");
        };

        assert!(matches!(
            &**alpha_head,
            ersd::Term::Name(ersd::Name { string }) if string == "3"
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
            panic!("expected nested match for 'mu case");
        };

        assert!(matches!(
            &**mu_head,
            ersd::Term::Name(ersd::Name { string }) if string == "2"
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
            panic!("expected nested match for 'zeta case");
        };

        assert!(matches!(
            &**zeta_head,
            ersd::Term::Name(ersd::Name { string }) if string == "1"
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
}
