use {
    super::{
        Apply, Atom, AtomMatch, AtomType, BinLiteral, BlnMatch, Entrypoint, Func, FuncType,
        GroupItem, Let, LetSignature, Match, Module, Nat, NatLiteral, NatMatch, Prim, Proj, Rec,
        Subterm, Term, TopCase, TopItem, TopLet, TopMod, TopUnion, TopUse, Tuple, TupleType,
        UnionCase, UnionMatch, UseGroup,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    num_traits::One,
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

fn print_prim_call(name: &'static str, args: Vec<Term>) -> Printer<'static> {
    flat([
        pure(name),
        pure("("),
        sep_flat(args.into_iter().map(print_term), || pure(", ")),
        pure(")"),
    ])
}

fn print_prim(prim: Prim) -> Printer<'static> {
    match prim {
        Prim::BlnType => pure("Bln"),
        Prim::Bln(false) => pure("false"),
        Prim::Bln(true) => pure("true"),
        Prim::NatType => pure("Nat"),
        Prim::Nat(Nat::Zero) => pure("0"),
        Prim::Nat(Nat::Succ(nat, inner)) => {
            if matches!(inner.as_subterm(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                match nat {
                    NatLiteral::Number(n) => pure(format!("{n}")),
                    NatLiteral::Char(c) => {
                        let escaped = match c {
                            '\'' => "\\'".to_string(),
                            '\\' => "\\\\".to_string(),
                            '\n' => "\\n".to_string(),
                            '\t' => "\\t".to_string(),
                            '\r' => "\\r".to_string(),
                            _ => c.to_string(),
                        };
                        pure(format!("'{escaped}'"))
                    }
                }
            } else {
                match nat {
                    NatLiteral::Number(n) if n.is_one() => {
                        flat([pure("Nat.succ("), print_term(inner), pure(")")])
                    }
                    NatLiteral::Number(n) => flat([
                        pure(format!("Nat.succ({n}, ")),
                        print_term(inner),
                        pure(")"),
                    ]),
                    NatLiteral::Char(c) => flat([
                        pure(format!("Nat.succ({}, ", c as usize)),
                        print_term(inner),
                        pure(")"),
                    ]),
                }
            }
        }
        Prim::NatEql(left, right) => print_prim_call("Nat.eql", vec![left, right]),
        Prim::NatNeq(left, right) => print_prim_call("Nat.neq", vec![left, right]),
        Prim::NatAdd(left, right) => print_prim_call("Nat.add", vec![left, right]),
        Prim::NatSub(left, right) => print_prim_call("Nat.sub", vec![left, right]),
        Prim::NatMul(left, right) => print_prim_call("Nat.mul", vec![left, right]),
        Prim::NatLt(left, right) => print_prim_call("Nat.lt", vec![left, right]),
        Prim::NatDiv(left, right) => print_prim_call("Nat.div", vec![left, right]),
        Prim::NatRem(left, right) => print_prim_call("Nat.rem", vec![left, right]),
        Prim::NatGt(left, right) => print_prim_call("Nat.gt", vec![left, right]),
        Prim::NatLte(left, right) => print_prim_call("Nat.lte", vec![left, right]),
        Prim::NatGte(left, right) => print_prim_call("Nat.gte", vec![left, right]),
        Prim::NatToStr(operand) => print_prim_call("Nat.to_str", vec![operand]),
        Prim::IntType => pure("Int"),
        Prim::Int(value) => pure(format!("{value:+}")),
        Prim::IntEql(left, right) => print_prim_call("Int.eql", vec![left, right]),
        Prim::IntNeq(left, right) => print_prim_call("Int.neq", vec![left, right]),
        Prim::IntAdd(left, right) => print_prim_call("Int.add", vec![left, right]),
        Prim::IntSub(left, right) => print_prim_call("Int.sub", vec![left, right]),
        Prim::IntMul(left, right) => print_prim_call("Int.mul", vec![left, right]),
        Prim::IntDiv(left, right) => print_prim_call("Int.div", vec![left, right]),
        Prim::IntRem(left, right) => print_prim_call("Int.rem", vec![left, right]),
        Prim::IntLt(left, right) => print_prim_call("Int.lt", vec![left, right]),
        Prim::IntGt(left, right) => print_prim_call("Int.gt", vec![left, right]),
        Prim::IntLte(left, right) => print_prim_call("Int.lte", vec![left, right]),
        Prim::IntGte(left, right) => print_prim_call("Int.gte", vec![left, right]),
        Prim::IntToStr(operand) => print_prim_call("Int.to_str", vec![operand]),
        Prim::FltType => pure("Flt"),
        Prim::Flt(value) => print_flt(value),
        Prim::FltAdd(left, right) => print_prim_call("Flt.add", vec![left, right]),
        Prim::FltSub(left, right) => print_prim_call("Flt.sub", vec![left, right]),
        Prim::FltMul(left, right) => print_prim_call("Flt.mul", vec![left, right]),
        Prim::FltDiv(left, right) => print_prim_call("Flt.div", vec![left, right]),
        Prim::FltEql(left, right) => print_prim_call("Flt.eql", vec![left, right]),
        Prim::FltNeq(left, right) => print_prim_call("Flt.neq", vec![left, right]),
        Prim::FltLt(left, right) => print_prim_call("Flt.lt", vec![left, right]),
        Prim::FltGt(left, right) => print_prim_call("Flt.gt", vec![left, right]),
        Prim::FltLte(left, right) => print_prim_call("Flt.lte", vec![left, right]),
        Prim::FltGte(left, right) => print_prim_call("Flt.gte", vec![left, right]),
        Prim::FltMin(left, right) => print_prim_call("Flt.min", vec![left, right]),
        Prim::FltMax(left, right) => print_prim_call("Flt.max", vec![left, right]),
        Prim::FltNeg(operand) => print_prim_call("Flt.neg", vec![operand]),
        Prim::FltAbs(operand) => print_prim_call("Flt.abs", vec![operand]),
        Prim::FltSqrt(operand) => print_prim_call("Flt.sqrt", vec![operand]),
        Prim::FltFloor(operand) => print_prim_call("Flt.floor", vec![operand]),
        Prim::FltCeil(operand) => print_prim_call("Flt.ceil", vec![operand]),
        Prim::FltTrunc(operand) => print_prim_call("Flt.trunc", vec![operand]),
        Prim::FltNearest(operand) => print_prim_call("Flt.nearest", vec![operand]),
        Prim::FltToStr(operand) => print_prim_call("Flt.to_str", vec![operand]),
        Prim::NatToInt(operand) => print_prim_call("Nat.to_int", vec![operand]),
        Prim::NatToFlt(operand) => print_prim_call("Nat.to_flt", vec![operand]),
        Prim::IntToNat(operand) => print_prim_call("Int.to_nat", vec![operand]),
        Prim::IntToFlt(operand) => print_prim_call("Int.to_flt", vec![operand]),
        Prim::FltToNat(operand) => print_prim_call("Flt.to_nat", vec![operand]),
        Prim::FltToInt(operand) => print_prim_call("Flt.to_int", vec![operand]),
        Prim::BinType => pure("Bin"),
        Prim::Bin(bin) => match bin {
            BinLiteral::Bytes(bytes) => pure(
                bytes
                    .iter()
                    .map(|byte| format!("\\{:02x}", byte))
                    .collect::<String>(),
            ),
            BinLiteral::String(content) => {
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
        Prim::BinLen(operand) => print_prim_call("Bin.len", vec![operand]),
        Prim::BinEql(left, right) => print_prim_call("Bin.eql", vec![left, right]),
        Prim::BinGet(bin, index) => print_prim_call("Bin.get", vec![bin, index]),
        Prim::BinSlice(bin, start, end) => print_prim_call("Bin.slice", vec![bin, start, end]),
        Prim::BinAppend(bin, byte) => print_prim_call("Bin.append", vec![bin, byte]),
        Prim::BinConcat(left, right) => print_prim_call("Bin.concat", vec![left, right]),
        Prim::ArrType(elem) => print_prim_call("Arr", vec![elem]),
        Prim::Arr(elems) => flat([
            pure("["),
            sep_flat(elems.into_iter().map(|operand| print_term(operand)), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        Prim::ArrLen(ty, operand) => print_prim_call("Arr.len", vec![ty, operand]),
        Prim::ArrGet(ty, list, index) => print_prim_call("Arr.get", vec![ty, list, index]),
        Prim::ArrSlice(ty, list, start, end) => {
            print_prim_call("Arr.slice", vec![ty, list, start, end])
        }
        Prim::ArrAppend(ty, list, elem) => print_prim_call("Arr.append", vec![ty, list, elem]),
        Prim::ArrConcat(ty, left, right) => print_prim_call("Arr.concat", vec![ty, left, right]),
        Prim::IoPrint(operand) => print_prim_call("Io.print", vec![operand]),
        Prim::IoRead => pure("Io.read"),
    }
}

fn print_term(term: Term) -> Printer<'static> {
    match term.into_subterm() {
        Subterm::Type => pure("Type"),
        Subterm::Prim(prim) => print_prim(prim),
        Subterm::Name(name) => pure(name.join()),
        Subterm::Atom(atom) => print_atom(atom),
        Subterm::AtomType(AtomType { atoms }) => flat([
            pure("'["),
            sep_flat(atoms.into_iter().map(|atom| pure(atom.as_string())), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        Subterm::FuncType(FuncType { params, output }) => flat([
            pure("("),
            sep_flat(
                params.into_iter().map(|(label, ty)| match label {
                    Some(label) => flat([pure(label), pure(" : "), print_term(ty)]),
                    None => print_term(ty),
                }),
                || pure(", "),
            ),
            pure(") -> "),
            print_term(output),
        ]),
        Subterm::Func(Func { params, body }) => flat([
            match params.as_slice() {
                [single] => flat([pure(single.clone())]),
                _ => flat([
                    pure("("),
                    sep_flat(params.into_iter().map(pure), || pure(", ")),
                    pure(")"),
                ]),
            },
            pure(" =>\n"),
            indent(print_term(body)),
        ]),
        Subterm::Apply(Apply { head, params }) => flat([
            print_term(head),
            pure("("),
            sep_flat(params.into_iter().map(|p| print_term(p)), || pure(", ")),
            pure(")"),
        ]),
        Subterm::TupleType(TupleType { fields }) => {
            let items = fields.into_iter().map(|(label, field_type)| match label {
                Some(label) => indent(flat([pure(label), pure(" : "), print_term(field_type)])),
                None => indent(print_term(field_type)),
            });
            flat([pure("{ "), sep_flat(items, || pure("\n, ")), pure("\n}")])
        }
        Subterm::Tuple(Tuple { fields }) => {
            if fields.len() == 1 {
                flat([
                    pure("("),
                    print_term(fields.into_iter().next().unwrap()),
                    pure(",)"),
                ])
            } else {
                flat([
                    pure("("),
                    sep_flat(fields.into_iter().map(|field| print_term(field)), || {
                        pure(", ")
                    }),
                    pure(")"),
                ])
            }
        }
        Subterm::Proj(Proj { head, index }) => {
            flat([pure("("), print_term(head), pure(format!(").{index}"))])
        }
        Subterm::Match(match_) => match match_ {
            Match::Bln(BlnMatch {
                head,
                motive,
                false_case,
                true_case,
            }) => flat([
                pure("match "),
                print_term(head),
                pure(" : "),
                match motive.label {
                    Some(label) => flat([pure(label), pure(" => "), print_term(motive.body)]),
                    None => print_term(motive.body),
                },
                pure("\n| false =>\n"),
                indent(print_term(false_case)),
                pure("\n| true =>\n"),
                indent(print_term(true_case)),
                pure("\nend"),
            ]),
            Match::Nat(NatMatch::Induction {
                head,
                motive,
                zero_case,
                pred_label,
                ih_label,
                succ_case,
            }) => flat([
                pure("match "),
                print_term(head),
                pure(" : "),
                match motive.label {
                    Some(label) => flat([pure(label), pure(" => "), print_term(motive.body)]),
                    None => print_term(motive.body),
                },
                pure("\n| 0 =>\n"),
                indent(print_term(zero_case)),
                pure("\n| "),
                pure(pred_label),
                pure(" "),
                pure(ih_label),
                pure(" =>\n"),
                indent(print_term(succ_case)),
                pure("\nend"),
            ]),
            Match::Nat(NatMatch::Dispatch {
                head,
                motive,
                cases,
                default,
            }) => flat([
                pure("match "),
                print_term(head),
                pure(" : "),
                match motive.label {
                    Some(label) => flat([pure(label), pure(" => "), print_term(motive.body)]),
                    None => print_term(motive.body),
                },
                flat(
                    cases
                        .into_iter()
                        .map(|(nat, body)| {
                            flat([pure(format!("\n| {nat} =>\n")), indent(print_term(body))])
                        })
                        .collect::<Vec<_>>(),
                ),
                pure("\n| _ =>\n"),
                indent(print_term(default)),
                pure("\nend"),
            ]),
            Match::Atom(AtomMatch {
                head,
                motive,
                cases,
            }) => flat([
                pure("match "),
                print_term(head),
                pure(" : "),
                match motive.label {
                    Some(label) => flat([pure(label), pure(" => "), print_term(motive.body)]),
                    None => print_term(motive.body),
                },
                flat(
                    cases
                        .into_iter()
                        .map(|(atom, body)| {
                            flat([
                                pure("\n| "),
                                print_atom(atom),
                                pure(" =>\n"),
                                indent(print_term(body)),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                pure("\nend"),
            ]),
            Match::Union(UnionMatch {
                head,
                motive,
                cases,
            }) => flat([
                pure("match "),
                print_term(head),
                pure(" : "),
                match motive.label {
                    Some(label) => flat([pure(label), pure(" => "), print_term(motive.body)]),
                    None => print_term(motive.body),
                },
                flat(
                    cases
                        .into_iter()
                        .map(|(label, UnionCase { binders, body })| {
                            flat([
                                pure(format!("\n| {label}(")),
                                pure(binders.join(", ")),
                                pure(") =>\n"),
                                indent(print_term(body)),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                pure("\nend"),
            ]),
        },
        Subterm::Let(Let {
            label,
            signature,
            tail,
        }) => flat([
            pure("let "),
            pure(label),
            print_let_signature(signature),
            pure(";"),
            pure("\n"),
            print_term(tail),
        ]),
        Subterm::Rec(Rec { items, tail }) => {
            let bindings = items.into_iter().map(|item| {
                flat([pure(item.label), print_let_signature(item.signature)])
            });
            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                print_term(tail),
            ])
        }
    }
}

fn print_let_signature(signature: LetSignature) -> Printer<'static> {
    match signature {
        LetSignature::Name { type_, body } => flat([
            pure(" : "),
            print_term(type_),
            pure(" =\n"),
            indent(print_term(body)),
        ]),
        LetSignature::Func {
            params,
            output,
            body,
        } => flat([
            pure("("),
            sep_flat(
                params.into_iter().map(|(name, ty)| {
                    flat([pure(name), pure(" : "), print_term(ty)])
                }),
                || pure(", "),
            ),
            pure(") -> "),
            print_term(output),
            pure(" =\n"),
            indent(print_term(body)),
        ]),
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

fn print_group_item(item: &GroupItem) -> String {
    match item {
        GroupItem::Mod(s) => format!("mod {s}"),
        GroupItem::Let(s) => format!("let {s}"),
        GroupItem::Both(s) => s.clone(),
    }
}

fn print_top_use(item: TopUse) -> Printer<'static> {
    flat([
        print_pub(item.is_pub),
        pure("use "),
        pure(item.name.join()),
        match item.group {
            UseGroup::Named(items) => pure(format!(
                "/{{{}}}",
                items
                    .iter()
                    .map(print_group_item)
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            UseGroup::Glob => pure("/*"),
        },
        pure(";"),
    ])
}

fn print_top_let(item: TopLet) -> Printer<'static> {
    flat([
        print_pub(item.is_pub),
        pure("let "),
        pure(item.label),
        print_let_signature(item.signature),
        pure(";"),
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
        print_let_signature(first.signature),
        flat(
            rest.into_iter()
                .map(|item| {
                    flat([
                        pure("\nand "),
                        print_pub(item.is_pub),
                        pure(item.label),
                        print_let_signature(item.signature),
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

fn print_module_items(items: Vec<TopItem>) -> Printer<'static> {
    sep_flat(items.into_iter().map(print_top_item), || pure("\n"))
}

fn print_top_union_case(case: TopCase) -> Printer<'static> {
    flat([
        pure(format!("\n| {}(", case.label)),
        flat(
            case.payload_types
                .into_iter()
                .map(|t| print_term(t))
                .collect::<Vec<_>>(),
        ),
        pure(")"),
    ])
}

fn print_top_union(unions: Vec<TopUnion>) -> Printer<'static> {
    let mut iter = unions.into_iter();
    let first = iter.next().unwrap();
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.is_pub),
        pure("union "),
        pure(first.label),
        flat(
            first
                .cases
                .into_iter()
                .map(print_top_union_case)
                .collect::<Vec<_>>(),
        ),
        flat(
            rest.into_iter()
                .map(|u| {
                    flat([
                        pure("\n"),
                        print_pub(u.is_pub),
                        pure("and "),
                        pure(u.label),
                        flat(
                            u.cases
                                .into_iter()
                                .map(print_top_union_case)
                                .collect::<Vec<_>>(),
                        ),
                    ])
                })
                .collect::<Vec<_>>(),
        ),
        pure("\nend"),
    ])
}

fn print_top_item(item: TopItem) -> Printer<'static> {
    match item {
        TopItem::Mod(m) => print_top_mod(m),
        TopItem::Use(u) => print_top_use(u),
        TopItem::Let(l) => print_top_let(l),
        TopItem::Rec(items) => print_top_rec(items),
        TopItem::Union(unions) => print_top_union(unions),
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
