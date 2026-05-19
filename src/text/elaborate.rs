use {super::*, crate::core};

pub fn elaborate(term: &Term) -> core::Term {
    match term {
        Term::Type => core::Term::Type,

        Term::Prim(p) => core::Term::Prim(elaborate_prim(p)),

        Term::Var(v) => core::Var::free(v.label.clone()).into(),

        Term::Atom(a) => core::Term::Atom(elaborate_atom(a)),

        Term::AtomType(at) => core::AtomType::new(at.atoms.iter().map(elaborate_atom)).into(),

        Term::FuncType(ft) => core::FuncType::new(
            ft.label.clone().unwrap_or_default(),
            elaborate(&ft.input),
            elaborate(&ft.output),
        )
        .into(),

        Term::Func(f) => core::Func::new(f.label.clone(), elaborate(&f.body)).into(),

        Term::Apply(ap) => core::Apply::new(elaborate(&ap.head), elaborate(&ap.param)).into(),

        Term::TupleType(tt) => {
            let fields = tt.fields.iter().map(|(label, t)| {
                let label = label.clone().unwrap_or_default();
                (label, elaborate(t))
            });
            core::TupleType::new(fields).into()
        }

        Term::Tuple(t) => core::Tuple::new(t.fields.iter().map(|f| elaborate(f))).into(),

        Term::NatFold(nm) => core::NatFold::new(
            elaborate(&nm.head),
            nm.motive_label.clone(),
            elaborate(&nm.motive),
            elaborate(&nm.zero_case),
            nm.pred_label.clone(),
            nm.ih_label.clone(),
            elaborate(&nm.succ_case),
        )
        .into(),

        Term::NatMatch(nm) => core::NatMatch::new(
            elaborate(&nm.head),
            nm.motive_label.clone(),
            elaborate(&nm.motive),
            nm.cases.iter().map(|(&n, body)| (n, elaborate(body))),
            elaborate(&nm.default),
        )
        .into(),

        Term::Split(sp) => core::Split::new(
            elaborate(&sp.head),
            sp.motive_label.clone(),
            elaborate(&sp.motive),
            sp.field_labels.iter().cloned(),
            elaborate(&sp.tail),
        )
        .into(),

        Term::Match(m) => {
            let cases = m.cases.iter().map(|(a, b)| {
                (
                    core::Atom {
                        string: a.string.clone(),
                    },
                    elaborate(b),
                )
            });
            core::Match::new(
                elaborate(&m.head),
                m.motive_label.clone(),
                elaborate(&m.motive),
                cases,
            )
            .into()
        }

        Term::Let(l) => core::Let::new(
            l.label.clone(),
            elaborate(&l.type_),
            elaborate(&l.body),
            elaborate(&l.tail),
        )
        .into(),

        Term::Rec(r) => {
            let items = r
                .items
                .iter()
                .map(|it| (it.label.clone(), elaborate(&it.type_), elaborate(&it.value)));
            core::Rec::new(items, elaborate(&r.tail)).into()
        }
    }
}

fn elaborate_atom(a: &Atom) -> core::Atom {
    core::Atom::from(a.string.clone())
}

fn elaborate_prim(p: &Prim) -> core::Prim {
    match p {
        Prim::NatType => core::Prim::NatType,
        Prim::Nat(nat) => core::Prim::Nat(match nat {
            Nat::Number(n) => *n,
            Nat::Char(c) => *c as u32,
        }),
        Prim::NatEql(l, r) => core::Prim::nat_eql(elaborate(l), elaborate(r)),
        Prim::NatNeq(l, r) => core::Prim::nat_neq(elaborate(l), elaborate(r)),
        Prim::NatAdd(l, r) => core::Prim::nat_add(elaborate(l), elaborate(r)),
        Prim::NatSub(l, r) => core::Prim::nat_sub(elaborate(l), elaborate(r)),
        Prim::NatMul(l, r) => core::Prim::nat_mul(elaborate(l), elaborate(r)),
        Prim::NatDiv(l, r) => core::Prim::nat_div(elaborate(l), elaborate(r)),
        Prim::NatRem(l, r) => core::Prim::nat_rem(elaborate(l), elaborate(r)),
        Prim::NatLt(l, r) => core::Prim::nat_lt(elaborate(l), elaborate(r)),
        Prim::NatGt(l, r) => core::Prim::nat_gt(elaborate(l), elaborate(r)),
        Prim::NatLte(l, r) => core::Prim::nat_lte(elaborate(l), elaborate(r)),
        Prim::NatGte(l, r) => core::Prim::nat_gte(elaborate(l), elaborate(r)),
        Prim::IntType => core::Prim::IntType,
        Prim::Int(n) => core::Prim::Int(*n),
        Prim::IntEql(l, r) => core::Prim::int_eql(elaborate(l), elaborate(r)),
        Prim::IntAdd(l, r) => core::Prim::int_add(elaborate(l), elaborate(r)),
        Prim::IntSub(l, r) => core::Prim::int_sub(elaborate(l), elaborate(r)),
        Prim::IntMul(l, r) => core::Prim::int_mul(elaborate(l), elaborate(r)),
        Prim::IntNeq(l, r) => core::Prim::int_neq(elaborate(l), elaborate(r)),
        Prim::IntDiv(l, r) => core::Prim::int_div(elaborate(l), elaborate(r)),
        Prim::IntRem(l, r) => core::Prim::int_rem(elaborate(l), elaborate(r)),
        Prim::IntLt(l, r) => core::Prim::int_lt(elaborate(l), elaborate(r)),
        Prim::IntGt(l, r) => core::Prim::int_gt(elaborate(l), elaborate(r)),
        Prim::IntLte(l, r) => core::Prim::int_lte(elaborate(l), elaborate(r)),
        Prim::IntGte(l, r) => core::Prim::int_gte(elaborate(l), elaborate(r)),
        Prim::FltType => core::Prim::FltType,
        Prim::Flt(n) => core::Prim::Flt(core::Flt::from_f32(*n)),
        Prim::FltAdd(l, r) => core::Prim::flt_add(elaborate(l), elaborate(r)),
        Prim::FltSub(l, r) => core::Prim::flt_sub(elaborate(l), elaborate(r)),
        Prim::FltMul(l, r) => core::Prim::flt_mul(elaborate(l), elaborate(r)),
        Prim::FltNeg(t) => core::Prim::flt_neg(elaborate(t)),
        Prim::FltAbs(t) => core::Prim::flt_abs(elaborate(t)),
        Prim::FltSqrt(t) => core::Prim::flt_sqrt(elaborate(t)),
        Prim::FltFloor(t) => core::Prim::flt_floor(elaborate(t)),
        Prim::FltCeil(t) => core::Prim::flt_ceil(elaborate(t)),
        Prim::FltTrunc(t) => core::Prim::flt_trunc(elaborate(t)),
        Prim::FltNearest(t) => core::Prim::flt_nearest(elaborate(t)),
        Prim::FltDiv(l, r) => core::Prim::flt_div(elaborate(l), elaborate(r)),
        Prim::FltMin(l, r) => core::Prim::flt_min(elaborate(l), elaborate(r)),
        Prim::FltMax(l, r) => core::Prim::flt_max(elaborate(l), elaborate(r)),
        Prim::FltEql(l, r) => core::Prim::flt_eql(elaborate(l), elaborate(r)),
        Prim::FltNeq(l, r) => core::Prim::flt_neq(elaborate(l), elaborate(r)),
        Prim::FltLt(l, r) => core::Prim::flt_lt(elaborate(l), elaborate(r)),
        Prim::FltGt(l, r) => core::Prim::flt_gt(elaborate(l), elaborate(r)),
        Prim::FltLte(l, r) => core::Prim::flt_lte(elaborate(l), elaborate(r)),
        Prim::FltGte(l, r) => core::Prim::flt_gte(elaborate(l), elaborate(r)),
        Prim::NatToInt(t) => core::Prim::nat_to_int(elaborate(t)),
        Prim::IntToNat(t) => core::Prim::int_to_nat(elaborate(t)),
        Prim::IntToFlt(t) => core::Prim::int_to_flt(elaborate(t)),
        Prim::NatToFlt(t) => core::Prim::nat_to_flt(elaborate(t)),
        Prim::FltToInt(t) => core::Prim::flt_to_int(elaborate(t)),
        Prim::FltToNat(t) => core::Prim::flt_to_nat(elaborate(t)),
        Prim::BinType => core::Prim::BinType,
        Prim::Bin(bin) => core::Prim::Bin(match bin {
            Bin::Bytes(bytes) => bytes.clone(),
            Bin::String(s) => s.as_bytes().to_vec(),
        }),
        Prim::BinLen(t) => core::Prim::bin_len(elaborate(t)),
        Prim::BinEql(left, right) => core::Prim::bin_eql(elaborate(left), elaborate(right)),
        Prim::BinGet(b, i) => core::Prim::bin_get(elaborate(b), elaborate(i)),
        Prim::BinSlice(b, s, end) => {
            core::Prim::bin_slice(elaborate(b), elaborate(s), elaborate(end))
        }
        Prim::BinAppend(b, byte) => core::Prim::bin_append(elaborate(b), elaborate(byte)),
        Prim::BinConcat(vs) => core::Prim::bin_concat(vs.iter().map(|t| elaborate(t))),
        Prim::ArrType(t) => core::Prim::arr_type(elaborate(t)),
        Prim::Arr(vs) => core::Prim::Arr(vs.iter().map(|t| elaborate(t).into()).collect()),
        Prim::ArrLen(t) => core::Prim::arr_len(elaborate(t)),
        Prim::ArrGet(l, i) => core::Prim::arr_get(elaborate(l), elaborate(i)),
        Prim::ArrSlice(l, s, end) => {
            core::Prim::arr_slice(elaborate(l), elaborate(s), elaborate(end))
        }
        Prim::ArrAppend(l, elem) => core::Prim::arr_append(elaborate(l), elaborate(elem)),
        Prim::ArrConcat(vs) => core::Prim::arr_concat(vs.iter().map(|t| elaborate(t))),
    }
}

#[cfg(test)]
mod tests {
    use crate::{core, text};

    fn parse(input: &str) -> core::Term {
        super::elaborate(&input.parse::<text::Term>().unwrap())
    }

    #[test]
    fn elaborate_rec_func_and_apply() {
        assert_eq!(
            parse("rec id : (x : Type) -> Type = x => x; id a"),
            core::Rec::new(
                vec![(
                    "id",
                    core::FuncType::new("x", core::Type, core::Type),
                    core::Func::new("x", core::Var::free("x"))
                )],
                core::Apply::many(core::Var::free("id"), [core::Var::free("a")]),
            )
            .into(),
        );
    }

    #[test]
    fn elaborate_let_tuple_and_atoms() {
        assert_eq!(
            parse("let x : '[hot, cold] = 'hot; (x, 'cold)"),
            core::Let::new(
                "x",
                core::AtomType::new(["hot", "cold"]),
                core::Atom::from("hot"),
                core::Tuple::new([
                    core::Term::from(core::Var::free("x")),
                    core::Term::from(core::Atom::from("cold")),
                ]),
            )
            .into(),
        );
    }

    #[test]
    fn elaborate_split_with_motive() {
        assert_eq!(
            parse("split ('left, 'right) : p => Type; | (x, y) => p"),
            core::Split::new(
                core::Tuple::new([core::Atom::from("left"), core::Atom::from("right")]),
                "p",
                core::Type,
                ["x", "y"],
                core::Var::free("p"),
            )
            .into(),
        );
    }

    #[test]
    fn elaborate_match_single_branch() {
        assert_eq!(
            parse("match 'foo : k => '[foo]; | 'foo => 'foo;"),
            core::Match::new(
                core::Atom::from("foo"),
                "k",
                core::AtomType::new(["foo"]),
                [(core::Atom::from("foo"), core::Atom::from("foo"))],
            )
            .into(),
        );
    }

    #[test]
    fn elaborate_prim() {
        assert_eq!(
            parse("Int.add 1i 2i"),
            core::Term::Prim(core::Prim::int_add(
                core::Term::Prim(core::Prim::Int(1)),
                core::Term::Prim(core::Prim::Int(2))
            ))
        );
        assert_eq!(
            parse("Nat.add 1n 2n"),
            core::Term::Prim(core::Prim::nat_add(
                core::Term::Prim(core::Prim::Nat(1)),
                core::Term::Prim(core::Prim::Nat(2))
            ))
        );
        assert_eq!(
            parse("Flt.mul 1.5 2.0"),
            core::Term::Prim(core::Prim::flt_mul(
                core::Term::Prim(core::Prim::Flt(core::Flt::from_f32(1.5))),
                core::Term::Prim(core::Prim::Flt(core::Flt::from_f32(2.0)))
            ))
        );
    }

    #[test]
    fn print_parse_roundtrip_closed_terms() {
        let terms = [
            core::FuncType::new("x", core::Type, core::Type).into(),
            core::Func::new("x", core::Var::free("x")).into(),
            core::Apply::many(
                core::Var::free("f"),
                [core::Tuple::new([
                    core::Atom::from("a"),
                    core::Atom::from("b"),
                ])],
            ),
            core::TupleType::new([("x", core::Type), ("y", core::Type), ("z", core::Type)]).into(),
            core::Tuple::new([
                core::Atom::from("a"),
                core::Atom::from("b"),
                core::Atom::from("c"),
            ])
            .into(),
            core::Let::new(
                "x",
                core::AtomType::new(["a", "b"]),
                core::Atom::from("a"),
                core::Match::new(
                    core::Var::free("x"),
                    "m",
                    core::Type,
                    [("a", core::Type), ("b", core::Type)],
                ),
            )
            .into(),
            core::Rec::new(
                vec![(
                    "id",
                    core::FuncType::new("x", core::Type, core::Type),
                    core::Func::new("x", core::Var::free("x")),
                )],
                core::Apply::many(core::Var::free("id"), [core::Type]),
            )
            .into(),
        ]
        .into_iter()
        .collect::<Vec<_>>();

        for term in terms {
            assert_eq!(parse(&term.to_string()), term);
        }
    }
}
