use {
    super::{
        Apply, AtomType, BlnMatch, Context, Error, FuncType, Let, Match, Nat, NatMatch, One, Prim,
        Proj, Rec, Scope, Subterm, Telescope, Term, TupleType, Two, Var, erase, reduce_with,
        refine_head,
    },
    std::collections::BTreeMap,
};

fn infer_prim(context: &mut Context, prim: &Prim) -> Result<Term, Error> {
    match prim {
        Prim::BlnType => Ok(Term::type_()),
        Prim::Bln(_) => Ok(Subterm::Prim(Prim::BlnType).into()),
        Prim::NatType => Ok(Term::type_()),
        Prim::Nat(_) => Ok(Subterm::Prim(Prim::NatType).into()),
        Prim::NatEql(left, right)
        | Prim::NatNeq(left, right)
        | Prim::NatLt(left, right)
        | Prim::NatGt(left, right)
        | Prim::NatLte(left, right)
        | Prim::NatGte(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::NatType).into())?;
            erase(context, right, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::NatAdd(left, right)
        | Prim::NatSub(left, right)
        | Prim::NatMul(left, right)
        | Prim::NatDiv(left, right)
        | Prim::NatRem(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::NatType).into())?;
            erase(context, right, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::IntType => Ok(Term::type_()),
        Prim::Int(_) => Ok(Subterm::Prim(Prim::IntType).into()),
        Prim::IntEql(left, right)
        | Prim::IntNeq(left, right)
        | Prim::IntLt(left, right)
        | Prim::IntGt(left, right)
        | Prim::IntLte(left, right)
        | Prim::IntGte(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::IntType).into())?;
            erase(context, right, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::IntAdd(left, right)
        | Prim::IntSub(left, right)
        | Prim::IntMul(left, right)
        | Prim::IntDiv(left, right)
        | Prim::IntRem(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::IntType).into())?;
            erase(context, right, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::IntType).into())
        }
        Prim::FltType => Ok(Term::type_()),
        Prim::Flt(_) => Ok(Subterm::Prim(Prim::FltType).into()),
        Prim::FltAdd(left, right)
        | Prim::FltSub(left, right)
        | Prim::FltMul(left, right)
        | Prim::FltDiv(left, right)
        | Prim::FltMin(left, right)
        | Prim::FltMax(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::FltType).into())?;
            erase(context, right, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltNeg(inner)
        | Prim::FltAbs(inner)
        | Prim::FltSqrt(inner)
        | Prim::FltFloor(inner)
        | Prim::FltCeil(inner)
        | Prim::FltTrunc(inner)
        | Prim::FltNearest(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltEql(left, right)
        | Prim::FltNeq(left, right)
        | Prim::FltLt(left, right)
        | Prim::FltGt(left, right)
        | Prim::FltLte(left, right)
        | Prim::FltGte(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::FltType).into())?;
            erase(context, right, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::NatToStr(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::IntToStr(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::FltToStr(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::NatToInt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::IntType).into())
        }
        Prim::NatToFlt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::NatType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::IntToNat(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::IntToFlt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::IntType).into())?;

            Ok(Subterm::Prim(Prim::FltType).into())
        }
        Prim::FltToNat(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::FltToInt(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::FltType).into())?;

            Ok(Subterm::Prim(Prim::IntType).into())
        }
        Prim::BinType => Ok(Term::type_()),
        Prim::Bin(_) => Ok(Subterm::Prim(Prim::BinType).into()),
        Prim::BinLen(bin) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => Ok(Subterm::Prim(Prim::NatType).into()),
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinEql(left, right) => {
            erase(context, left, &Subterm::Prim(Prim::BinType).into())?;
            erase(context, right, &Subterm::Prim(Prim::BinType).into())?;

            Ok(Subterm::Prim(Prim::BlnType).into())
        }
        Prim::BinGet(bin, index) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => {
                    erase(context, index, &Subterm::Prim(Prim::NatType).into())?;
                    Ok(Subterm::Prim(Prim::NatType).into())
                }
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinSlice(bin, start, end) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => {
                    erase(context, start, &Subterm::Prim(Prim::NatType).into())?;
                    erase(context, end, &Subterm::Prim(Prim::NatType).into())?;
                    Ok(Subterm::Prim(Prim::BinType).into())
                }
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinAppend(bin, byte) => {
            let bin_type = infer(context, bin)?;
            let bin_type = reduce_with(context, &bin_type)?;
            match &*bin_type {
                Subterm::Prim(Prim::BinType) => {
                    erase(context, byte, &Subterm::Prim(Prim::NatType).into())?;
                    Ok(Subterm::Prim(Prim::BinType).into())
                }
                other => Err(Error::type_mismatch(
                    Subterm::Prim(prim.clone()),
                    other.clone(),
                    Subterm::Prim(Prim::BinType),
                )),
            }
        }
        Prim::BinConcat(operands) => {
            for operand in operands {
                erase(context, operand, &Subterm::Prim(Prim::BinType).into())?;
            }
            Ok(Subterm::Prim(Prim::BinType).into())
        }
        Prim::ArrType(elem) => {
            erase(context, elem, &Term::type_())?;
            Ok(Term::type_())
        }
        Prim::Arr(_) => Err(Error::cannot_infer_literal(Subterm::Prim(prim.clone()))),
        Prim::ArrLen(type_, list) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            Ok(Subterm::Prim(Prim::NatType).into())
        }
        Prim::ArrGet(type_, list, index) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            erase(context, index, &Subterm::Prim(Prim::NatType).into())?;
            Ok(type_.clone())
        }
        Prim::ArrSlice(type_, list, start, end) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            erase(context, start, &Subterm::Prim(Prim::NatType).into())?;
            erase(context, end, &Subterm::Prim(Prim::NatType).into())?;
            Ok(expected_list_type)
        }
        Prim::ArrAppend(type_, list, elem) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            erase(context, list, &expected_list_type)?;
            erase(context, elem, type_)?;
            Ok(expected_list_type)
        }
        Prim::ArrConcat(type_, operands) => {
            erase(context, type_, &Term::type_())?;
            let expected_list_type = Subterm::Prim(Prim::ArrType(type_.clone())).into();
            for operand in operands {
                erase(context, operand, &expected_list_type)?;
            }
            Ok(expected_list_type)
        }
        Prim::IoPrint(inner) => {
            erase(context, inner, &Subterm::Prim(Prim::BinType).into())?;
            Ok(Term::tuple_type_unit())
        }
        Prim::IoRead => Ok(Subterm::Prim(Prim::BinType).into()),
    }
}

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

    fn walk(context: &mut Context, tele: Telescope<Term>, params: &[Term]) -> Result<Term, Error> {
        match tele {
            Telescope::Done(body) => Ok(*body),
            Telescope::Cons(ty, rest) => {
                let head = &params[0];
                erase(context, head, &ty)?;
                walk(context, rest.open(&[head]), &params[1..])
            }
        }
    }

    walk(context, ft.telescope, params)
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
        .map(|_| ())
    })?;

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
        .map(|_| ())
    })?;

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
        .map(|_| ())
    })?;

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

    let head_label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&head_label, &Term::atom_type(atoms.iter().cloned()));

        erase(
            context,
            &motive.open(&[&Term::var(Var::free(head_label))]),
            &Term::type_(),
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
