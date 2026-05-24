use {
    super::{Atom, Func, Let, Match, NatFold, Prim, Proj, Rec, Term, Tuple},
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn print_atom(atom: &Atom) -> Printer<'static> {
    pure(format!("@{}", atom.index))
}

fn print_flt(value: f32) -> Printer<'static> {
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

fn print_prim<'a>(prim: &'a Prim) -> Printer<'a> {
    match prim {
        Prim::Nat(value) => pure(format!("{value}n")),
        Prim::NatEql(l, r) => flat([pure("Nat.eql "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatNeq(l, r) => flat([pure("Nat.neq "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatAdd(l, r) => flat([pure("Nat.add "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatSub(l, r) => flat([pure("Nat.sub "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatMul(l, r) => flat([pure("Nat.mul "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatLt(l, r) => flat([pure("Nat.lt "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatDiv(l, r) => flat([pure("Nat.div "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatRem(l, r) => flat([pure("Nat.rem "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatGt(l, r) => flat([pure("Nat.gt "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatLte(l, r) => flat([pure("Nat.lte "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatGte(l, r) => flat([pure("Nat.gte "), print_term(l), pure(" "), print_term(r)]),
        Prim::NatToStr(t) => flat([pure("Nat.to_str "), print_term(t)]),
        Prim::Int(value) => pure(format!("{value}i")),
        Prim::IntEql(l, r) => flat([pure("Int.eql "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntNeq(l, r) => flat([pure("Int.neq "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntAdd(l, r) => flat([pure("Int.add "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntSub(l, r) => flat([pure("Int.sub "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntMul(l, r) => flat([pure("Int.mul "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntDiv(l, r) => flat([pure("Int.div "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntRem(l, r) => flat([pure("Int.rem "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntLt(l, r) => flat([pure("Int.lt "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntGt(l, r) => flat([pure("Int.gt "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntLte(l, r) => flat([pure("Int.lte "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntGte(l, r) => flat([pure("Int.gte "), print_term(l), pure(" "), print_term(r)]),
        Prim::IntToStr(t) => flat([pure("Int.to_str "), print_term(t)]),
        Prim::Flt(value) => print_flt(*value),
        Prim::FltAdd(l, r) => flat([pure("Flt.add "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltSub(l, r) => flat([pure("Flt.sub "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltMul(l, r) => flat([pure("Flt.mul "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltDiv(l, r) => flat([pure("Flt.div "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltEql(l, r) => flat([pure("Flt.eql "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltNeq(l, r) => flat([pure("Flt.neq "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltLt(l, r) => flat([pure("Flt.lt "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltGt(l, r) => flat([pure("Flt.gt "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltLte(l, r) => flat([pure("Flt.lte "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltGte(l, r) => flat([pure("Flt.gte "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltMin(l, r) => flat([pure("Flt.min "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltMax(l, r) => flat([pure("Flt.max "), print_term(l), pure(" "), print_term(r)]),
        Prim::FltNeg(t) => flat([pure("Flt.neg "), print_term(t)]),
        Prim::FltAbs(t) => flat([pure("Flt.abs "), print_term(t)]),
        Prim::FltSqrt(t) => flat([pure("Flt.sqrt "), print_term(t)]),
        Prim::FltFloor(t) => flat([pure("Flt.floor "), print_term(t)]),
        Prim::FltCeil(t) => flat([pure("Flt.ceil "), print_term(t)]),
        Prim::FltTrunc(t) => flat([pure("Flt.trunc "), print_term(t)]),
        Prim::FltNearest(t) => flat([pure("Flt.nearest "), print_term(t)]),
        Prim::FltToStr(t) => flat([pure("Flt.to_str "), print_term(t)]),
        Prim::NatToInt(t) => flat([pure("Nat.to_int "), print_term(t)]),
        Prim::NatToFlt(t) => flat([pure("Nat.to_flt "), print_term(t)]),
        Prim::IntToNat(t) => flat([pure("Int.to_nat "), print_term(t)]),
        Prim::IntToFlt(t) => flat([pure("Int.to_flt "), print_term(t)]),
        Prim::FltToNat(t) => flat([pure("Flt.to_nat "), print_term(t)]),
        Prim::FltToInt(t) => flat([pure("Flt.to_int "), print_term(t)]),
        Prim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        Prim::BinLen(t) => flat([pure("Bin.len "), print_term(t)]),
        Prim::BinEql(left, right) => flat([
            pure("Bin.eql "),
            print_term(left),
            pure(" "),
            print_term(right),
        ]),
        Prim::BinGet(bin, index) => flat([
            pure("Bin.get "),
            print_term(bin),
            pure(" "),
            print_term(index),
        ]),
        Prim::BinSlice(bin, start, end) => flat([
            pure("Bin.slice "),
            print_term(bin),
            pure(" "),
            print_term(start),
            pure(" "),
            print_term(end),
        ]),
        Prim::BinAppend(bin, byte) => flat([
            pure("Bin.append "),
            print_term(bin),
            pure(" "),
            print_term(byte),
        ]),
        Prim::BinConcat(operands) => flat([
            pure("Bin.concat "),
            sep_flat(operands.iter().map(|t| print_term(t)), || pure(", ")),
        ]),
        Prim::Arr(elems) => flat([
            pure("["),
            sep_flat(elems.iter().map(|t| print_term(t)), || pure(", ")),
            pure("]"),
        ]),
        Prim::ArrLen(t) => flat([pure("Arr.len "), print_term(t)]),
        Prim::ArrGet(list, index) => flat([
            pure("Arr.get "),
            print_term(list),
            pure(" "),
            print_term(index),
        ]),
        Prim::ArrSlice(list, start, end) => flat([
            pure("Arr.slice "),
            print_term(list),
            pure(" "),
            print_term(start),
            pure(" "),
            print_term(end),
        ]),
        Prim::ArrAppend(list, elem) => flat([
            pure("Arr.append "),
            print_term(list),
            pure(" "),
            print_term(elem),
        ]),
        Prim::ArrConcat(operands) => flat([
            pure("Arr.concat "),
            sep_flat(operands.iter().map(|t| print_term(t)), || pure(", ")),
        ]),
        Prim::Unit => pure("()"),
        Prim::SysPrint(t) => flat([pure("Sys.print "), print_term(t)]),
    }
}

fn print_term<'a>(term: &'a Term) -> Printer<'a> {
    match term {
        Term::Erased => pure("_"),
        Term::Prim(prim) => print_prim(prim),
        Term::NatFold(NatFold {
            head,
            zero_case,
            pred,
            ih,
            succ_case,
        }) => flat([
            pure("Nat.fold "),
            print_term(head),
            pure(";\n| 0n =>\n"),
            indent(flat([print_term(zero_case), pure(";")])),
            pure("\n| "),
            pure(format!("#{}", pred.as_str())),
            pure(" "),
            pure(format!("#{}", ih.as_str())),
            pure(" =>\n"),
            indent(flat([print_term(succ_case), pure(";")])),
        ]),
        Term::NatMatch(super::NatMatch {
            head,
            cases,
            default,
        }) => {
            let case_printers = cases.iter().map(|(val, body)| {
                flat([
                    pure(format!("\n| {val}n =>\n")),
                    indent(flat([print_term(body), pure(";")])),
                ])
            });
            flat([
                pure("Nat.match "),
                print_term(head),
                pure(";"),
                flat(case_printers.collect::<Vec<_>>()),
                pure("\n| _ =>\n"),
                indent(flat([print_term(default), pure(";")])),
            ])
        }
        Term::Func(Func {
            captures,
            param,
            body,
        }) => {
            if captures.is_empty() {
                flat([
                    pure(format!("#{}", param.as_str())),
                    pure(" =>\n"),
                    indent(print_term(body)),
                ])
            } else {
                flat([
                    pure("{"),
                    sep_flat(
                        captures.iter().map(|s| pure(format!("#{}", s.as_str()))),
                        || pure(", "),
                    ),
                    pure("} "),
                    pure(format!("#{}", param.as_str())),
                    pure(" =>\n"),
                    indent(print_term(body)),
                ])
            }
        }
        Term::Apply(super::Apply { head, param }) => {
            flat([print_term(head), pure(" "), print_term(param)])
        }
        Term::Tuple(Tuple { fields }) => flat([
            pure("("),
            sep_flat(fields.iter().map(|f| print_term(f)), || pure(", ")),
            pure(")"),
        ]),
        Term::Proj(Proj { head, index }) => {
            flat([pure("("), print_term(head), pure(format!(").{index}"))])
        }
        Term::Atom(atom) => print_atom(atom),
        Term::Match(Match { head, cases }) => {
            let cases = cases.iter().enumerate().map(|(i, body)| {
                flat([
                    pure(format!("\n| @{i} =>\n")),
                    indent(flat([print_term(body), pure(";")])),
                ])
            });
            flat([
                pure("match "),
                print_term(head),
                pure(";"),
                flat(cases.collect::<Vec<_>>()),
            ])
        }
        Term::Let(Let { name, body, tail }) => flat([
            pure("let "),
            pure(format!("#{}", name.as_str())),
            pure(" =\n"),
            indent(flat([print_term(body), pure(";")])),
            pure("\n"),
            print_term(tail),
        ]),
        Term::Rec(Rec { names, items, tail }) => {
            let bindings = names
                .iter()
                .zip(items.iter())
                .map(|(name, body)| {
                    flat([
                        pure(format!("#{}", name.as_str())),
                        pure(" =\n"),
                        indent(print_term(body)),
                    ])
                })
                .collect::<Vec<_>>();

            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                print_term(tail),
            ])
        }
        Term::Name(name) => pure(format!("#{}", name.as_str())),
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self), formatter, 2)
    }
}
