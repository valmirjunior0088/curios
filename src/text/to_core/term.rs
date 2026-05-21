use {super::{resolve_name, ModuleInfo}, crate::{core, text::{Bin, Name, Nat, Prim, Term}}, std::collections::HashMap};

pub fn elaborate_term(
    term: &Term,
    scope: &HashMap<String, Name>,
    table: &HashMap<Name, ModuleInfo>,
) -> core::Term {
    match term {
        Term::Type => core::Term::Type,

        Term::Prim(prim) => core::Term::Prim(elaborate_prim(prim, scope, table)),

        Term::Name(name) => core::Var::free(if name.path.len() == 1 {
            name.path[0].clone()
        } else {
            resolve_name(name, scope, table).path.join("/")
        })
        .into(),

        Term::Atom(atom) => core::Term::Atom(core::Atom::from(atom.string.clone())),

        Term::AtomType(at) => core::AtomType::new(
            at.atoms
                .iter()
                .map(|atom| core::Atom::from(atom.string.clone())),
        )
        .into(),

        Term::FuncType(ft) => core::FuncType::new(
            ft.label.clone().unwrap_or_default(),
            elaborate_term(&ft.input, scope, table),
            elaborate_term(&ft.output, scope, table),
        )
        .into(),

        Term::Func(func) => {
            core::Func::new(func.label.clone(), elaborate_term(&func.body, scope, table)).into()
        }

        Term::Apply(ap) => core::Apply::new(
            elaborate_term(&ap.head, scope, table),
            elaborate_term(&ap.param, scope, table),
        )
        .into(),

        Term::TupleType(tt) => {
            let fields = tt.fields.iter().map(|(label, type_)| {
                let label = label.clone().unwrap_or_default();
                (label, elaborate_term(type_, scope, table))
            });
            core::TupleType::new(fields).into()
        }

        Term::Tuple(tuple) => core::Tuple::new(
            tuple
                .fields
                .iter()
                .map(|field| elaborate_term(field, scope, table)),
        )
        .into(),

        Term::NatFold(nat_fold) => core::NatFold::new(
            elaborate_term(&nat_fold.head, scope, table),
            nat_fold.motive_label.clone(),
            elaborate_term(&nat_fold.motive, scope, table),
            elaborate_term(&nat_fold.zero_case, scope, table),
            nat_fold.pred_label.clone(),
            nat_fold.ih_label.clone(),
            elaborate_term(&nat_fold.succ_case, scope, table),
        )
        .into(),

        Term::NatMatch(nm) => core::NatMatch::new(
            elaborate_term(&nm.head, scope, table),
            nm.motive_label.clone(),
            elaborate_term(&nm.motive, scope, table),
            nm.cases
                .iter()
                .map(|(&nat, body)| (nat, elaborate_term(body, scope, table))),
            elaborate_term(&nm.default, scope, table),
        )
        .into(),

        Term::Split(split) => core::Split::new(
            elaborate_term(&split.head, scope, table),
            split.motive_label.clone(),
            elaborate_term(&split.motive, scope, table),
            split.field_labels.iter().cloned(),
            elaborate_term(&split.tail, scope, table),
        )
        .into(),

        Term::Match(match_) => core::Match::new(
            elaborate_term(&match_.head, scope, table),
            match_.motive_label.clone(),
            elaborate_term(&match_.motive, scope, table),
            match_.cases.iter().map(|(atom, body)| {
                (
                    core::Atom::from(atom.string.clone()),
                    elaborate_term(body, scope, table),
                )
            }),
        )
        .into(),

        Term::Let(let_) => core::Let::new(
            let_.label.clone(),
            elaborate_term(&let_.type_, scope, table),
            elaborate_term(&let_.body, scope, table),
            elaborate_term(&let_.tail, scope, table),
        )
        .into(),

        Term::Rec(rec) => core::Rec::new(
            rec.items.iter().map(|it| {
                (
                    it.label.clone(),
                    elaborate_term(&it.type_, scope, table),
                    elaborate_term(&it.value, scope, table),
                )
            }),
            elaborate_term(&rec.tail, scope, table),
        )
        .into(),
    }
}

pub fn elaborate_prim(
    prim: &Prim,
    scope: &HashMap<String, Name>,
    table: &HashMap<Name, ModuleInfo>,
) -> core::Prim {
    match prim {
        Prim::NatType => core::Prim::NatType,
        Prim::Nat(Nat::Number(number)) => core::Prim::Nat(*number),
        Prim::Nat(Nat::Char(character)) => core::Prim::Nat(*character as u32),
        Prim::NatEql(left, right) => core::Prim::nat_eql(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatNeq(left, right) => core::Prim::nat_neq(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatAdd(left, right) => core::Prim::nat_add(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatSub(left, right) => core::Prim::nat_sub(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatMul(left, right) => core::Prim::nat_mul(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatLt(left, right) => core::Prim::nat_lt(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatDiv(left, right) => core::Prim::nat_div(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatRem(left, right) => core::Prim::nat_rem(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatGt(left, right) => core::Prim::nat_gt(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatLte(left, right) => core::Prim::nat_lte(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::NatGte(left, right) => core::Prim::nat_gte(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntType => core::Prim::IntType,
        Prim::Int(value) => core::Prim::Int(*value),
        Prim::IntEql(left, right) => core::Prim::int_eql(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntNeq(left, right) => core::Prim::int_neq(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntAdd(left, right) => core::Prim::int_add(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntSub(left, right) => core::Prim::int_sub(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntMul(left, right) => core::Prim::int_mul(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntDiv(left, right) => core::Prim::int_div(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntRem(left, right) => core::Prim::int_rem(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntLt(left, right) => core::Prim::int_lt(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntGt(left, right) => core::Prim::int_gt(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntLte(left, right) => core::Prim::int_lte(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::IntGte(left, right) => core::Prim::int_gte(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltType => core::Prim::FltType,
        Prim::Flt(flt) => core::Prim::Flt(core::Flt::from_f32(*flt)),
        Prim::FltAdd(left, right) => core::Prim::flt_add(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltSub(left, right) => core::Prim::flt_sub(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltMul(left, right) => core::Prim::flt_mul(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltDiv(left, right) => core::Prim::flt_div(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltEql(left, right) => core::Prim::flt_eql(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltNeq(left, right) => core::Prim::flt_neq(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltLt(left, right) => core::Prim::flt_lt(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltGt(left, right) => core::Prim::flt_gt(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltLte(left, right) => core::Prim::flt_lte(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltGte(left, right) => core::Prim::flt_gte(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltMin(left, right) => core::Prim::flt_min(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltMax(left, right) => core::Prim::flt_max(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::FltNeg(inner) => core::Prim::flt_neg(elaborate_term(inner, scope, table)),
        Prim::FltAbs(inner) => core::Prim::flt_abs(elaborate_term(inner, scope, table)),
        Prim::FltSqrt(inner) => core::Prim::flt_sqrt(elaborate_term(inner, scope, table)),
        Prim::FltFloor(inner) => core::Prim::flt_floor(elaborate_term(inner, scope, table)),
        Prim::FltCeil(inner) => core::Prim::flt_ceil(elaborate_term(inner, scope, table)),
        Prim::FltTrunc(inner) => core::Prim::flt_trunc(elaborate_term(inner, scope, table)),
        Prim::FltNearest(inner) => core::Prim::flt_nearest(elaborate_term(inner, scope, table)),
        Prim::NatToInt(inner) => core::Prim::nat_to_int(elaborate_term(inner, scope, table)),
        Prim::NatToFlt(inner) => core::Prim::nat_to_flt(elaborate_term(inner, scope, table)),
        Prim::IntToNat(inner) => core::Prim::int_to_nat(elaborate_term(inner, scope, table)),
        Prim::IntToFlt(inner) => core::Prim::int_to_flt(elaborate_term(inner, scope, table)),
        Prim::FltToNat(inner) => core::Prim::flt_to_nat(elaborate_term(inner, scope, table)),
        Prim::FltToInt(inner) => core::Prim::flt_to_int(elaborate_term(inner, scope, table)),
        Prim::BinType => core::Prim::BinType,
        Prim::Bin(Bin::Bytes(bytes)) => core::Prim::Bin(bytes.clone()),
        Prim::Bin(Bin::String(string)) => core::Prim::Bin(string.as_bytes().to_vec()),
        Prim::BinLen(inner) => core::Prim::bin_len(elaborate_term(inner, scope, table)),
        Prim::BinEql(left, right) => core::Prim::bin_eql(
            elaborate_term(left, scope, table),
            elaborate_term(right, scope, table),
        ),
        Prim::BinGet(bin, index) => core::Prim::bin_get(
            elaborate_term(bin, scope, table),
            elaborate_term(index, scope, table),
        ),
        Prim::BinSlice(bin, start, end) => core::Prim::bin_slice(
            elaborate_term(bin, scope, table),
            elaborate_term(start, scope, table),
            elaborate_term(end, scope, table),
        ),
        Prim::BinAppend(bin, byte) => core::Prim::bin_append(
            elaborate_term(bin, scope, table),
            elaborate_term(byte, scope, table),
        ),
        Prim::BinConcat(operands) =>
            core::Prim::bin_concat(operands.iter().map(|operand| elaborate_term(operand, scope, table))),
        Prim::ArrType(inner) => core::Prim::arr_type(elaborate_term(inner, scope, table)),
        Prim::Arr(elems) => core::Prim::Arr(
            elems
                .iter()
                .map(|elem| elaborate_term(elem, scope, table).into())
                .collect(),
        ),
        Prim::ArrLen(inner) => core::Prim::arr_len(elaborate_term(inner, scope, table)),
        Prim::ArrGet(list, index) => core::Prim::arr_get(
            elaborate_term(list, scope, table),
            elaborate_term(index, scope, table),
        ),
        Prim::ArrSlice(list, start, end) => core::Prim::arr_slice(
            elaborate_term(list, scope, table),
            elaborate_term(start, scope, table),
            elaborate_term(end, scope, table),
        ),
        Prim::ArrAppend(list, elem) => core::Prim::arr_append(
            elaborate_term(list, scope, table),
            elaborate_term(elem, scope, table),
        ),
        Prim::ArrConcat(operands) =>
            core::Prim::arr_concat(operands.iter().map(|operand| elaborate_term(operand, scope, table))),
    }
}
