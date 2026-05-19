use crate::{core, text};

pub fn elaborate(term: &text::Term) -> core::Term {
    match term {
        text::Term::Type => core::Term::Type,

        text::Term::Prim(p) => core::Term::Prim(elaborate_prim(p)),

        text::Term::Var(v) => core::Var::free(v.label.clone()).into(),

        text::Term::Atom(a) => core::Term::Atom(elaborate_atom(a)),

        text::Term::AtomType(at) => {
            core::AtomType::new(at.atoms.iter().map(elaborate_atom)).into()
        }

        text::Term::FuncType(ft) => {
            let label = ft.label.as_deref().unwrap_or("");
            core::FuncType::new(label, elaborate(&ft.input), elaborate(&ft.output)).into()
        }

        text::Term::Func(f) => core::Func::new(f.label.clone(), elaborate(&f.body)).into(),

        text::Term::Apply(ap) => {
            core::Apply::new(elaborate(&ap.head), elaborate(&ap.param)).into()
        }

        text::Term::TupleType(tt) => {
            let fields = tt.fields.iter().map(|(label, t)| {
                let label = label.as_deref().unwrap_or("").to_string();
                (label, elaborate(t))
            });
            core::TupleType::new(fields).into()
        }

        text::Term::Tuple(t) => core::Tuple::new(t.fields.iter().map(|f| elaborate(f))).into(),

        text::Term::NatMatch(nm) => core::NatMatch::new(
            elaborate(&nm.head),
            nm.motive_label.clone(),
            elaborate(&nm.motive),
            elaborate(&nm.zero_case),
            nm.pred_label.clone(),
            nm.ih_label.clone(),
            elaborate(&nm.succ_case),
        )
        .into(),

        text::Term::Split(sp) => core::Split::new(
            elaborate(&sp.head),
            sp.motive_label.clone(),
            elaborate(&sp.motive),
            sp.field_labels.iter().cloned(),
            elaborate(&sp.tail),
        )
        .into(),

        text::Term::Match(m) => {
            let cases = m
                .cases
                .iter()
                .map(|(a, b)| (elaborate_atom(a), elaborate(b)));
            core::Match::new(
                elaborate(&m.head),
                m.motive_label.clone(),
                elaborate(&m.motive),
                cases,
            )
            .into()
        }

        text::Term::Let(l) => core::Let::new(
            l.label.clone(),
            elaborate(&l.type_),
            elaborate(&l.body),
            elaborate(&l.tail),
        )
        .into(),

        text::Term::Rec(r) => {
            let items = r
                .items
                .iter()
                .map(|it| (it.label.clone(), elaborate(&it.type_), elaborate(&it.value)));
            core::Rec::new(items, elaborate(&r.tail)).into()
        }
    }
}

fn elaborate_atom(a: &text::Atom) -> core::Atom {
    core::Atom::from(a.string.clone())
}

fn elaborate_prim(p: &text::Prim) -> core::Prim {
    let e = |t: &text::Subterm| elaborate(t);
    match p {
        text::Prim::NatType => core::Prim::NatType,
        text::Prim::Nat(n) => core::Prim::Nat(*n),
        text::Prim::NatEql(l, r) => core::Prim::nat_eql(e(l), e(r)),
        text::Prim::NatNeq(l, r) => core::Prim::nat_neq(e(l), e(r)),
        text::Prim::NatAdd(l, r) => core::Prim::nat_add(e(l), e(r)),
        text::Prim::NatSub(l, r) => core::Prim::nat_sub(e(l), e(r)),
        text::Prim::NatMul(l, r) => core::Prim::nat_mul(e(l), e(r)),
        text::Prim::NatDiv(l, r) => core::Prim::nat_div(e(l), e(r)),
        text::Prim::NatRem(l, r) => core::Prim::nat_rem(e(l), e(r)),
        text::Prim::NatLt(l, r) => core::Prim::nat_lt(e(l), e(r)),
        text::Prim::NatGt(l, r) => core::Prim::nat_gt(e(l), e(r)),
        text::Prim::NatLte(l, r) => core::Prim::nat_lte(e(l), e(r)),
        text::Prim::NatGte(l, r) => core::Prim::nat_gte(e(l), e(r)),
        text::Prim::IntType => core::Prim::IntType,
        text::Prim::Int(n) => core::Prim::Int(*n),
        text::Prim::IntEql(l, r) => core::Prim::int_eql(e(l), e(r)),
        text::Prim::IntAdd(l, r) => core::Prim::int_add(e(l), e(r)),
        text::Prim::IntSub(l, r) => core::Prim::int_sub(e(l), e(r)),
        text::Prim::IntMul(l, r) => core::Prim::int_mul(e(l), e(r)),
        text::Prim::IntNeq(l, r) => core::Prim::int_neq(e(l), e(r)),
        text::Prim::IntDiv(l, r) => core::Prim::int_div(e(l), e(r)),
        text::Prim::IntRem(l, r) => core::Prim::int_rem(e(l), e(r)),
        text::Prim::IntLt(l, r) => core::Prim::int_lt(e(l), e(r)),
        text::Prim::IntGt(l, r) => core::Prim::int_gt(e(l), e(r)),
        text::Prim::IntLte(l, r) => core::Prim::int_lte(e(l), e(r)),
        text::Prim::IntGte(l, r) => core::Prim::int_gte(e(l), e(r)),
        text::Prim::FltType => core::Prim::FltType,
        text::Prim::Flt(n) => core::Prim::Flt(*n),
        text::Prim::FltAdd(l, r) => core::Prim::flt_add(e(l), e(r)),
        text::Prim::FltSub(l, r) => core::Prim::flt_sub(e(l), e(r)),
        text::Prim::FltMul(l, r) => core::Prim::flt_mul(e(l), e(r)),
        text::Prim::FltNeg(t) => core::Prim::flt_neg(e(t)),
        text::Prim::FltAbs(t) => core::Prim::flt_abs(e(t)),
        text::Prim::FltSqrt(t) => core::Prim::flt_sqrt(e(t)),
        text::Prim::FltFloor(t) => core::Prim::flt_floor(e(t)),
        text::Prim::FltCeil(t) => core::Prim::flt_ceil(e(t)),
        text::Prim::FltTrunc(t) => core::Prim::flt_trunc(e(t)),
        text::Prim::FltNearest(t) => core::Prim::flt_nearest(e(t)),
        text::Prim::FltDiv(l, r) => core::Prim::flt_div(e(l), e(r)),
        text::Prim::FltMin(l, r) => core::Prim::flt_min(e(l), e(r)),
        text::Prim::FltMax(l, r) => core::Prim::flt_max(e(l), e(r)),
        text::Prim::FltEql(l, r) => core::Prim::flt_eql(e(l), e(r)),
        text::Prim::FltNeq(l, r) => core::Prim::flt_neq(e(l), e(r)),
        text::Prim::FltLt(l, r) => core::Prim::flt_lt(e(l), e(r)),
        text::Prim::FltGt(l, r) => core::Prim::flt_gt(e(l), e(r)),
        text::Prim::FltLte(l, r) => core::Prim::flt_lte(e(l), e(r)),
        text::Prim::FltGte(l, r) => core::Prim::flt_gte(e(l), e(r)),
        text::Prim::NatToInt(t) => core::Prim::nat_to_int(e(t)),
        text::Prim::IntToNat(t) => core::Prim::int_to_nat(e(t)),
        text::Prim::IntToFlt(t) => core::Prim::int_to_flt(e(t)),
        text::Prim::NatToFlt(t) => core::Prim::nat_to_flt(e(t)),
        text::Prim::FltToInt(t) => core::Prim::flt_to_int(e(t)),
        text::Prim::FltToNat(t) => core::Prim::flt_to_nat(e(t)),
        text::Prim::BinType => core::Prim::BinType,
        text::Prim::Bin(bytes) => core::Prim::Bin(bytes.clone()),
        text::Prim::BinLen(t) => core::Prim::bin_len(e(t)),
        text::Prim::BinGet(b, i) => core::Prim::bin_get(e(b), e(i)),
        text::Prim::BinSlice(b, s, end) => core::Prim::bin_slice(e(b), e(s), e(end)),
        text::Prim::BinAppend(b, byte) => core::Prim::bin_append(e(b), e(byte)),
        text::Prim::BinConcat(vs) => core::Prim::bin_concat(vs.iter().map(e)),
        text::Prim::ArrType(t) => core::Prim::arr_type(e(t)),
        text::Prim::Arr(vs) => core::Prim::arr_concat(vs.iter().map(e)),
        text::Prim::ArrLen(t) => core::Prim::arr_len(e(t)),
        text::Prim::ArrGet(l, i) => core::Prim::arr_get(e(l), e(i)),
        text::Prim::ArrSlice(l, s, end) => core::Prim::arr_slice(e(l), e(s), e(end)),
        text::Prim::ArrAppend(l, elem) => core::Prim::arr_append(e(l), e(elem)),
        text::Prim::ArrConcat(vs) => core::Prim::arr_concat(vs.iter().map(e)),
    }
}
