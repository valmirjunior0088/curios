use {
    super::{
        Apply, Atom, AtomType, Bin, DefFrom, DefInto, Entrypoint, Func, FuncType, Let, Match,
        Module, Nat, NatFold, NatMatch, Prim, Proj, Rec, Term, TopDef, TopItem, TopLet, TopMod,
        TopUse, Tuple, TupleType,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn print_atom(atom: Atom) -> Printer<'static> {
    flat([pure("'"), pure(atom.as_string())])
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

    if !string.starts_with('-') {
        string.insert(0, '+');
    }

    pure(string)
}

fn print_prim(prim: Prim) -> Printer<'static> {
    match prim {
        Prim::NatType => pure("Nat"),
        Prim::Nat(nat) => match nat {
            Nat::Number(number) => pure(format!("{number}")),
            Nat::Char(character) => {
                let escaped = match character {
                    '\'' => "\\'".to_string(),
                    '\\' => "\\\\".to_string(),
                    '\n' => "\\n".to_string(),
                    '\t' => "\\t".to_string(),
                    '\r' => "\\r".to_string(),
                    _ => character.to_string(),
                };
                pure(format!("'{escaped}'"))
            }
        },
        Prim::NatEql(left, right) => flat([
            pure("Nat.eql "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatNeq(left, right) => flat([
            pure("Nat.neq "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatAdd(left, right) => flat([
            pure("Nat.add "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatSub(left, right) => flat([
            pure("Nat.sub "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatMul(left, right) => flat([
            pure("Nat.mul "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatLt(left, right) => flat([
            pure("Nat.lt "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatDiv(left, right) => flat([
            pure("Nat.div "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatRem(left, right) => flat([
            pure("Nat.rem "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatGt(left, right) => flat([
            pure("Nat.gt "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatLte(left, right) => flat([
            pure("Nat.lte "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatGte(left, right) => flat([
            pure("Nat.gte "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::NatToStr(operand) => flat([pure("Nat.to_str "), print_term(*operand)]),
        Prim::IntType => pure("Int"),
        Prim::Int(value) => pure(format!("{value:+}")),
        Prim::IntEql(left, right) => flat([
            pure("Int.eql "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntNeq(left, right) => flat([
            pure("Int.neq "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntAdd(left, right) => flat([
            pure("Int.add "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntSub(left, right) => flat([
            pure("Int.sub "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntMul(left, right) => flat([
            pure("Int.mul "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntDiv(left, right) => flat([
            pure("Int.div "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntRem(left, right) => flat([
            pure("Int.rem "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntLt(left, right) => flat([
            pure("Int.lt "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntGt(left, right) => flat([
            pure("Int.gt "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntLte(left, right) => flat([
            pure("Int.lte "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntGte(left, right) => flat([
            pure("Int.gte "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::IntToStr(operand) => flat([pure("Int.to_str "), print_term(*operand)]),
        Prim::FltType => pure("Flt"),
        Prim::Flt(value) => print_flt(value),
        Prim::FltAdd(left, right) => flat([
            pure("Flt.add "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltSub(left, right) => flat([
            pure("Flt.sub "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltMul(left, right) => flat([
            pure("Flt.mul "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltDiv(left, right) => flat([
            pure("Flt.div "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltEql(left, right) => flat([
            pure("Flt.eql "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltNeq(left, right) => flat([
            pure("Flt.neq "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltLt(left, right) => flat([
            pure("Flt.lt "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltGt(left, right) => flat([
            pure("Flt.gt "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltLte(left, right) => flat([
            pure("Flt.lte "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltGte(left, right) => flat([
            pure("Flt.gte "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltMin(left, right) => flat([
            pure("Flt.min "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltMax(left, right) => flat([
            pure("Flt.max "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
        Prim::FltNeg(operand) => flat([pure("Flt.neg "), print_term(*operand)]),
        Prim::FltAbs(operand) => flat([pure("Flt.abs "), print_term(*operand)]),
        Prim::FltSqrt(operand) => flat([pure("Flt.sqrt "), print_term(*operand)]),
        Prim::FltFloor(operand) => flat([pure("Flt.floor "), print_term(*operand)]),
        Prim::FltCeil(operand) => flat([pure("Flt.ceil "), print_term(*operand)]),
        Prim::FltTrunc(operand) => flat([pure("Flt.trunc "), print_term(*operand)]),
        Prim::FltNearest(operand) => flat([pure("Flt.nearest "), print_term(*operand)]),
        Prim::FltToStr(operand) => flat([pure("Flt.to_str "), print_term(*operand)]),
        Prim::NatToInt(operand) => flat([pure("Nat.to_int "), print_term(*operand)]),
        Prim::NatToFlt(operand) => flat([pure("Nat.to_flt "), print_term(*operand)]),
        Prim::IntToNat(operand) => flat([pure("Int.to_nat "), print_term(*operand)]),
        Prim::IntToFlt(operand) => flat([pure("Int.to_flt "), print_term(*operand)]),
        Prim::FltToNat(operand) => flat([pure("Flt.to_nat "), print_term(*operand)]),
        Prim::FltToInt(operand) => flat([pure("Flt.to_int "), print_term(*operand)]),
        Prim::BinType => pure("Bin"),
        Prim::Bin(bin) => match bin {
            Bin::Bytes(bytes) => pure(
                bytes
                    .iter()
                    .map(|byte| format!("\\{:02x}", byte))
                    .collect::<String>(),
            ),
            Bin::String(content) => {
                let escaped = content
                    .chars()
                    .map(|character| match character {
                        '"' => "\\\"".to_string(),
                        '\\' => "\\\\".to_string(),
                        '\n' => "\\n".to_string(),
                        '\t' => "\\t".to_string(),
                        '\r' => "\\r".to_string(),
                        _ => character.to_string(),
                    })
                    .collect::<String>();
                pure(format!("\"{escaped}\""))
            }
        },
        Prim::BinLen(operand) => flat([pure("Bin.len "), print_term(*operand)]),
        Prim::BinEql(left, right) => flat([
            pure("Bin.eql "),
            print_term(*left),
            pure(" "),
            print_term(*right),
        ]),
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
            sep_flat(
                operands.into_iter().map(|operand| print_term(*operand)),
                || pure(", "),
            ),
        ]),
        Prim::ArrType(elem) => flat([pure("Arr "), print_term(*elem)]),
        Prim::Arr(elems) => flat([
            pure("["),
            sep_flat(
                elems.into_iter().map(|operand| print_term(*operand)),
                || pure(", "),
            ),
            pure("]"),
        ]),
        Prim::ArrLen(operand) => flat([pure("Arr.len "), print_term(*operand)]),
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
            sep_flat(
                operands.into_iter().map(|operand| print_term(*operand)),
                || pure(", "),
            ),
        ]),
        Prim::SysPrint(operand) => flat([pure("Sys.print "), print_term(*operand)]),
    }
}

fn print_term(term: Term) -> Printer<'static> {
    match term {
        Term::Type => pure("Type"),
        Term::Prim(prim) => print_prim(prim),
        Term::Name(name) => pure(name.join()),
        Term::Atom(atom) => print_atom(atom),
        Term::AtomType(AtomType { atoms }) => flat([
            pure("'["),
            sep_flat(atoms.into_iter().map(|atom| pure(atom.as_string())), || {
                pure(", ")
            }),
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
        Term::Func(Func { label, body }) => {
            flat([pure(label), pure(" =>\n"), indent(print_term(*body))])
        }
        Term::Apply(Apply { head, param }) => {
            flat([print_term(*head), pure(" "), print_term(*param)])
        }
        Term::TupleType(TupleType { fields }) => {
            let items = fields.into_iter().map(|(label, field_type)| match label {
                Some(label) => indent(flat([pure(label), pure(" : "), print_term(*field_type)])),
                None => indent(print_term(*field_type)),
            });
            flat([pure("{ "), sep_flat(items, || pure("\n, ")), pure("\n}")])
        }
        Term::Tuple(Tuple { fields }) => flat([
            pure("("),
            sep_flat(fields.into_iter().map(|field| print_term(*field)), || {
                pure(", ")
            }),
            pure(")"),
        ]),
        Term::NatFold(NatFold {
            head,
            motive_label,
            motive,
            zero_case,
            pred_label,
            ih_label,
            succ_case,
        }) => flat([
            pure("Nat.fold "),
            print_term(*head),
            pure(" : "),
            pure(motive_label),
            pure(" => "),
            print_term(*motive),
            pure(";\n| 0n =>\n"),
            indent(flat([print_term(*zero_case), pure(";")])),
            pure("\n| "),
            pure(pred_label),
            pure(" "),
            pure(ih_label),
            pure(" =>\n"),
            indent(flat([print_term(*succ_case), pure(";")])),
        ]),
        Term::NatMatch(NatMatch {
            head,
            motive_label,
            motive,
            cases,
            default,
        }) => {
            let case_printers = cases.into_iter().map(|(nat, body)| {
                flat([
                    pure(format!("\n| {nat}n =>\n")),
                    indent(flat([print_term(*body), pure(";")])),
                ])
            });
            flat([
                pure("Nat.match "),
                print_term(*head),
                pure(" : "),
                pure(motive_label),
                pure(" => "),
                print_term(*motive),
                pure(";"),
                flat(case_printers.collect::<Vec<_>>()),
                pure("\n| _ =>\n"),
                indent(flat([print_term(*default), pure(";")])),
            ])
        }
        Term::Proj(Proj { head, index }) => {
            flat([pure("("), print_term(*head), pure(format!(").{index}"))])
        }
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
                    pure(" =>\n"),
                    indent(flat([print_term(*body), pure(";")])),
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
        Term::DefFrom(DefFrom { label, body }) => {
            flat([pure(label), pure(".from "), print_term(*body)])
        }
        Term::DefInto(DefInto { label, body }) => {
            flat([pure(label), pure(".into "), print_term(*body)])
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
            pure(" =\n"),
            indent(flat([print_term(*body), pure(";")])),
            pure("\n"),
            print_term(*tail),
        ]),
        Term::Rec(Rec { items, tail }) => {
            let bindings = items.into_iter().map(|item| {
                flat([
                    pure(item.label),
                    pure(" : "),
                    print_term(*item.type_),
                    pure(" =\n"),
                    indent(print_term(*item.value)),
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

fn print_pub(is_pub: bool) -> Printer<'static> {
    if is_pub { pure("pub ") } else { pure("") }
}

fn print_top_use(item: TopUse) -> Printer<'static> {
    flat([
        print_pub(item.is_pub),
        pure("use "),
        if item.is_abs { pure("/") } else { pure("") },
        pure(item.name.join()),
        pure(";"),
    ])
}

fn print_top_let(item: TopLet) -> Printer<'static> {
    flat([
        print_pub(item.is_pub),
        pure("let "),
        pure(item.label),
        pure(" : "),
        print_term(*item.type_),
        pure(" =\n"),
        indent(flat([print_term(*item.body), pure(";")])),
    ])
}

fn print_top_rec(items: Vec<TopLet>) -> Printer<'static> {
    let mut iter = items.into_iter();
    let first = iter.next().unwrap();
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.is_pub),
        pure("rec "),
        pure(first.label),
        pure(" : "),
        print_term(*first.type_),
        pure(" =\n"),
        indent(print_term(*first.body)),
        flat(
            rest.into_iter()
                .map(|item| {
                    flat([
                        pure("\nand "),
                        print_pub(item.is_pub),
                        pure(item.label),
                        pure(" : "),
                        print_term(*item.type_),
                        pure(" =\n"),
                        indent(print_term(*item.body)),
                    ])
                })
                .collect::<Vec<_>>(),
        ),
        pure(";"),
    ])
}

fn print_top_mod(item: TopMod) -> Printer<'static> {
    match item.module {
        None => flat([
            print_pub(item.is_pub),
            pure("mod "),
            pure(item.label),
            pure(";"),
        ]),
        Some(module) => flat([
            print_pub(item.is_pub),
            pure("mod "),
            pure(item.label),
            pure("\n"),
            indent(print_module_items(module.items)),
            pure("\nend"),
        ]),
    }
}

fn print_top_def(item: TopDef) -> Printer<'static> {
    flat([
        print_pub(item.is_pub),
        pure("def "),
        pure(item.label),
        pure("("),
        print_term(*item.witness),
        pure(")\n"),
        indent(print_module_items(item.module.items)),
        pure("\nend"),
    ])
}

fn print_module_items(items: Vec<TopItem>) -> Printer<'static> {
    sep_flat(items.into_iter().map(print_top_item), || pure("\n"))
}

fn print_top_item(item: TopItem) -> Printer<'static> {
    match item {
        TopItem::Mod(m) => print_top_mod(m),
        TopItem::Use(u) => print_top_use(u),
        TopItem::Let(l) => print_top_let(l),
        TopItem::Rec(items) => print_top_rec(items),
        TopItem::Def(d) => print_top_def(d),
    }
}

impl Display for Module {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_module_items(self.clone().items), formatter, 2)
    }
}

impl Display for Entrypoint {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        let entrypoint = self.clone();
        let printer = if entrypoint.items.is_empty() {
            print_term(entrypoint.tail)
        } else {
            flat([
                print_module_items(entrypoint.items),
                pure("\n"),
                print_term(entrypoint.tail),
            ])
        };
        run_printer(printer, formatter, 2)
    }
}
