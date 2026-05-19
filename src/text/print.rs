use {
    super::{
        Apply, Atom, AtomType, Func, FuncType, Let, Match, NatMatch, Prim, Rec, Split, Term, Tuple,
        TupleType, Var,
    },
    crate::printer::{Printer, flat, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn print_atom(atom: Atom) -> Printer<'static> {
    flat([pure("'"), pure(atom.string)])
}

fn print_flt(bits: u32) -> Printer<'static> {
    let value = f32::from_bits(bits);
    let mut string = value.to_string();

    if let Some(index) = string.find(['e', 'E']) {
        if !string[..index].contains('.') {
            string.insert_str(index, ".0");
        }
    } else if !string.contains('.') {
        string.push_str(".0");
    }

    pure(string)
}

fn print_prim(prim: Prim) -> Printer<'static> {
    match prim {
        Prim::NatType => pure("Nat"),
        Prim::Nat(value) => pure(format!("{value}n")),
        Prim::NatEql(l, r) => flat([pure("Nat.eql "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatNeq(l, r) => flat([pure("Nat.neq "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatAdd(l, r) => flat([pure("Nat.add "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatSub(l, r) => flat([pure("Nat.sub "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatMul(l, r) => flat([pure("Nat.mul "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatDiv(l, r) => flat([pure("Nat.div "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatRem(l, r) => flat([pure("Nat.rem "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatLt(l, r) => flat([pure("Nat.lt "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatGt(l, r) => flat([pure("Nat.gt "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatLte(l, r) => flat([pure("Nat.lte "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::NatGte(l, r) => flat([pure("Nat.gte "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntType => pure("Int"),
        Prim::Int(value) => pure(format!("{value}i")),
        Prim::IntEql(l, r) => flat([pure("Int.eql "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntNeq(l, r) => flat([pure("Int.neq "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntAdd(l, r) => flat([pure("Int.add "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntSub(l, r) => flat([pure("Int.sub "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntMul(l, r) => flat([pure("Int.mul "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntDiv(l, r) => flat([pure("Int.div "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntRem(l, r) => flat([pure("Int.rem "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntLt(l, r) => flat([pure("Int.lt "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntGt(l, r) => flat([pure("Int.gt "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntLte(l, r) => flat([pure("Int.lte "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::IntGte(l, r) => flat([pure("Int.gte "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltType => pure("Flt"),
        Prim::Flt(bits) => print_flt(bits),
        Prim::FltAdd(l, r) => flat([pure("Flt.add "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltSub(l, r) => flat([pure("Flt.sub "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltMul(l, r) => flat([pure("Flt.mul "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltDiv(l, r) => flat([pure("Flt.div "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltMin(l, r) => flat([pure("Flt.min "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltMax(l, r) => flat([pure("Flt.max "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltEql(l, r) => flat([pure("Flt.eql "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltNeq(l, r) => flat([pure("Flt.neq "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltLt(l, r) => flat([pure("Flt.lt "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltGt(l, r) => flat([pure("Flt.gt "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltLte(l, r) => flat([pure("Flt.lte "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltGte(l, r) => flat([pure("Flt.gte "), print_term(*l), pure(" "), print_term(*r)]),
        Prim::FltNeg(t) => flat([pure("Flt.neg "), print_term(*t)]),
        Prim::FltAbs(t) => flat([pure("Flt.abs "), print_term(*t)]),
        Prim::FltSqrt(t) => flat([pure("Flt.sqrt "), print_term(*t)]),
        Prim::FltFloor(t) => flat([pure("Flt.floor "), print_term(*t)]),
        Prim::FltCeil(t) => flat([pure("Flt.ceil "), print_term(*t)]),
        Prim::FltTrunc(t) => flat([pure("Flt.trunc "), print_term(*t)]),
        Prim::FltNearest(t) => flat([pure("Flt.nearest "), print_term(*t)]),
        Prim::NatToInt(t) => flat([pure("Nat.to_int "), print_term(*t)]),
        Prim::IntToNat(t) => flat([pure("Int.to_nat "), print_term(*t)]),
        Prim::IntToFlt(t) => flat([pure("Int.to_flt "), print_term(*t)]),
        Prim::NatToFlt(t) => flat([pure("Nat.to_flt "), print_term(*t)]),
        Prim::FltToInt(t) => flat([pure("Flt.to_int "), print_term(*t)]),
        Prim::FltToNat(t) => flat([pure("Flt.to_nat "), print_term(*t)]),
        Prim::BinType => pure("Bin"),
        Prim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        Prim::BinLen(t) => flat([pure("Bin.len "), print_term(*t)]),
        Prim::BinGet(bin, index) => flat([
            pure("Bin.get "),
            print_term(*bin),
            pure(" "),
            print_term(*index),
        ]),
        Prim::BinSlice(bin, start, end) => flat([
            pure("Bin.slice "),
            print_term(*bin),
            pure(" "),
            print_term(*start),
            pure(" "),
            print_term(*end),
        ]),
        Prim::BinAppend(bin, byte) => flat([
            pure("Bin.append "),
            print_term(*bin),
            pure(" "),
            print_term(*byte),
        ]),
        Prim::BinConcat(operands) => flat([
            pure("Bin.concat "),
            sep_flat(operands.into_iter().map(|t| print_term(*t)), || pure(", ")),
        ]),
        Prim::ArrType(elem) => flat([pure("Arr "), print_term(*elem)]),
        Prim::Arr(elems) => flat([
            pure("["),
            sep_flat(elems.into_iter().map(|t| print_term(*t)), || pure(", ")),
            pure("]"),
        ]),
        Prim::ArrLen(t) => flat([pure("Arr.len "), print_term(*t)]),
        Prim::ArrGet(list, index) => flat([
            pure("Arr.get "),
            print_term(*list),
            pure(" "),
            print_term(*index),
        ]),
        Prim::ArrSlice(list, start, end) => flat([
            pure("Arr.slice "),
            print_term(*list),
            pure(" "),
            print_term(*start),
            pure(" "),
            print_term(*end),
        ]),
        Prim::ArrAppend(list, elem) => flat([
            pure("Arr.append "),
            print_term(*list),
            pure(" "),
            print_term(*elem),
        ]),
        Prim::ArrConcat(operands) => flat([
            pure("Arr.concat "),
            sep_flat(operands.into_iter().map(|t| print_term(*t)), || pure(", ")),
        ]),
    }
}

fn print_term(term: Term) -> Printer<'static> {
    match term {
        Term::Type => pure("Type"),
        Term::Prim(prim) => print_prim(prim),
        Term::Var(Var { label }) => pure(label),
        Term::Atom(atom) => print_atom(atom),
        Term::AtomType(AtomType { atoms }) => flat([
            pure("'["),
            sep_flat(atoms.into_iter().map(|a| pure(a.string)), || pure(", ")),
            pure("]"),
        ]),
        Term::FuncType(FuncType {
            label,
            input,
            output,
        }) => match label {
            Some(label) => flat([
                pure("("),
                pure(label),
                pure(" : "),
                print_term(*input),
                pure(") -> "),
                print_term(*output),
            ]),
            None => flat([print_term(*input), pure(" -> "), print_term(*output)]),
        },
        Term::Func(Func { label, body }) => flat([pure(label), pure(" => "), print_term(*body)]),
        Term::Apply(Apply { head, param }) => {
            flat([print_term(*head), pure(" "), print_term(*param)])
        }
        Term::TupleType(TupleType { fields }) => {
            let items = fields.into_iter().map(|(label, t)| match label {
                Some(label) => flat([pure(label), pure(" : "), print_term(*t)]),
                None => print_term(*t),
            });
            flat([pure("{"), sep_flat(items, || pure(", ")), pure("}")])
        }
        Term::Tuple(Tuple { fields }) => flat([
            pure("("),
            sep_flat(fields.into_iter().map(|f| print_term(*f)), || pure(", ")),
            pure(")"),
        ]),
        Term::NatMatch(NatMatch {
            head,
            motive_label,
            motive,
            zero_case,
            pred_label,
            ih_label,
            succ_case,
        }) => flat([
            pure("Nat.match "),
            print_term(*head),
            pure(" : "),
            pure(motive_label),
            pure(" => "),
            print_term(*motive),
            pure(";\n| 0n => "),
            print_term(*zero_case),
            pure(";\n| "),
            pure(pred_label),
            pure(" "),
            pure(ih_label),
            pure(" => "),
            print_term(*succ_case),
            pure(";"),
        ]),
        Term::Split(Split {
            head,
            motive_label,
            motive,
            field_labels,
            tail,
        }) => flat([
            pure("split "),
            print_term(*head),
            pure(" : "),
            pure(motive_label),
            pure(" => "),
            print_term(*motive),
            pure(";\n| ("),
            sep_flat(field_labels.into_iter().map(pure), || pure(", ")),
            pure(") => "),
            print_term(*tail),
        ]),
        Term::Match(Match {
            head,
            motive_label,
            motive,
            cases,
        }) => {
            let case_printers = cases.into_iter().map(|(atom, body)| {
                flat([
                    pure("\n| "),
                    print_atom(atom),
                    pure(" => "),
                    print_term(*body),
                    pure(";"),
                ])
            });
            flat([
                pure("match "),
                print_term(*head),
                pure(" : "),
                pure(motive_label),
                pure(" => "),
                print_term(*motive),
                pure(";"),
                flat(case_printers.collect::<Vec<_>>()),
            ])
        }
        Term::Let(Let {
            label,
            type_,
            body,
            tail,
        }) => flat([
            pure("let "),
            pure(label),
            pure(" : "),
            print_term(*type_),
            pure(" = "),
            print_term(*body),
            pure(";\n"),
            print_term(*tail),
        ]),
        Term::Rec(Rec { items, tail }) => {
            let bindings = items.into_iter().map(|it| {
                flat([
                    pure(it.label),
                    pure(" : "),
                    print_term(*it.type_),
                    pure(" = "),
                    print_term(*it.value),
                ])
            });
            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                print_term(*tail),
            ])
        }
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.clone()), formatter, 2)
    }
}
