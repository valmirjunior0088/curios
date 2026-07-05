use {
    super::{
        Atom, CellPrim, Func, HostPrim, Item, Let, Match, Module, NatMatch, Prim, Proj, PurePrim,
        Rec, Subterm, Term, Tuple,
    },
    curios_base::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
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
        Prim::Pure(pure_prim) => print_pure_prim(pure_prim),
        Prim::Host(host_prim) => print_host_prim(host_prim),
        Prim::Cell(cell_prim) => print_cell_prim(cell_prim),
    }
}

fn print_binary<'a>(name: &'static str, left: &'a Term, right: &'a Term) -> Printer<'a> {
    flat([
        pure(name),
        pure(" "),
        print_term(left),
        pure(" "),
        print_term(right),
    ])
}

fn print_unary<'a>(name: &'static str, operand: &'a Term) -> Printer<'a> {
    flat([pure(name), pure(" "), print_term(operand)])
}

fn print_ternary<'a>(name: &'static str, a: &'a Term, b: &'a Term, c: &'a Term) -> Printer<'a> {
    flat([
        pure(name),
        pure(" "),
        print_term(a),
        pure(" "),
        print_term(b),
        pure(" "),
        print_term(c),
    ])
}

fn print_pure_prim<'a>(prim: &'a PurePrim) -> Printer<'a> {
    match prim {
        PurePrim::Nat(value) => pure(format!("{value}n")),
        PurePrim::NatEql(l, r) => print_binary("Nat.eql", l, r),
        PurePrim::NatNeq(l, r) => print_binary("Nat.neq", l, r),
        PurePrim::NatAdd(l, r) => print_binary("Nat.add", l, r),
        PurePrim::NatSub(l, r) => print_binary("Nat.sub", l, r),
        PurePrim::NatMul(l, r) => print_binary("Nat.mul", l, r),
        PurePrim::NatLt(l, r) => print_binary("Nat.lt", l, r),
        PurePrim::NatDiv(l, r) => print_binary("Nat.div", l, r),
        PurePrim::NatRem(l, r) => print_binary("Nat.rem", l, r),
        PurePrim::NatGt(l, r) => print_binary("Nat.gt", l, r),
        PurePrim::NatLte(l, r) => print_binary("Nat.lte", l, r),
        PurePrim::NatGte(l, r) => print_binary("Nat.gte", l, r),
        PurePrim::NatAnd(l, r) => print_binary("Nat.and", l, r),
        PurePrim::NatOr(l, r) => print_binary("Nat.or", l, r),
        PurePrim::NatXor(l, r) => print_binary("Nat.xor", l, r),
        PurePrim::NatShl(l, r) => print_binary("Nat.shl", l, r),
        PurePrim::NatShr(l, r) => print_binary("Nat.shr", l, r),
        PurePrim::Int(value) => pure(format!("{value}i")),
        PurePrim::IntEql(l, r) => print_binary("Int.eql", l, r),
        PurePrim::IntNeq(l, r) => print_binary("Int.neq", l, r),
        PurePrim::IntAdd(l, r) => print_binary("Int.add", l, r),
        PurePrim::IntSub(l, r) => print_binary("Int.sub", l, r),
        PurePrim::IntMul(l, r) => print_binary("Int.mul", l, r),
        PurePrim::IntDiv(l, r) => print_binary("Int.div", l, r),
        PurePrim::IntRem(l, r) => print_binary("Int.rem", l, r),
        PurePrim::IntLt(l, r) => print_binary("Int.lt", l, r),
        PurePrim::IntGt(l, r) => print_binary("Int.gt", l, r),
        PurePrim::IntLte(l, r) => print_binary("Int.lte", l, r),
        PurePrim::IntGte(l, r) => print_binary("Int.gte", l, r),
        PurePrim::IntAnd(l, r) => print_binary("Int.and", l, r),
        PurePrim::IntOr(l, r) => print_binary("Int.or", l, r),
        PurePrim::IntXor(l, r) => print_binary("Int.xor", l, r),
        PurePrim::IntShl(l, r) => print_binary("Int.shl", l, r),
        PurePrim::IntShr(l, r) => print_binary("Int.shr", l, r),
        PurePrim::Flt(value) => print_flt(*value),
        PurePrim::FltAdd(l, r) => print_binary("Flt.add", l, r),
        PurePrim::FltSub(l, r) => print_binary("Flt.sub", l, r),
        PurePrim::FltMul(l, r) => print_binary("Flt.mul", l, r),
        PurePrim::FltDiv(l, r) => print_binary("Flt.div", l, r),
        PurePrim::FltRem(l, r) => print_binary("Flt.rem", l, r),
        PurePrim::FltEql(l, r) => print_binary("Flt.eql", l, r),
        PurePrim::FltNeq(l, r) => print_binary("Flt.neq", l, r),
        PurePrim::FltLt(l, r) => print_binary("Flt.lt", l, r),
        PurePrim::FltGt(l, r) => print_binary("Flt.gt", l, r),
        PurePrim::FltLte(l, r) => print_binary("Flt.lte", l, r),
        PurePrim::FltGte(l, r) => print_binary("Flt.gte", l, r),
        PurePrim::FltMin(l, r) => print_binary("Flt.min", l, r),
        PurePrim::FltMax(l, r) => print_binary("Flt.max", l, r),
        PurePrim::FltNeg(t) => print_unary("Flt.neg", t),
        PurePrim::FltAbs(t) => print_unary("Flt.abs", t),
        PurePrim::FltSqrt(t) => print_unary("Flt.sqrt", t),
        PurePrim::FltFloor(t) => print_unary("Flt.floor", t),
        PurePrim::FltCeil(t) => print_unary("Flt.ceil", t),
        PurePrim::FltTrunc(t) => print_unary("Flt.trunc", t),
        PurePrim::FltNearest(t) => print_unary("Flt.nearest", t),
        PurePrim::FltToLeBin(t) => print_unary("Flt.to_le_bin", t),
        PurePrim::NatToInt(t) => print_unary("Nat.to_int", t),
        PurePrim::NatToFlt(t) => print_unary("Nat.to_flt", t),
        PurePrim::IntToNat(t) => print_unary("Int.to_nat", t),
        PurePrim::IntToFlt(t) => print_unary("Int.to_flt", t),
        PurePrim::FltToNat(t) => print_unary("Flt.to_nat", t),
        PurePrim::FltToInt(t) => print_unary("Flt.to_int", t),
        PurePrim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        PurePrim::BinLen(t) => print_unary("Bin.len", t),
        PurePrim::BinEql(left, right) => print_binary("Bin.eql", left, right),
        PurePrim::BinGet(bin, index) => print_binary("Bin.get", bin, index),
        PurePrim::BinSlice(bin, start, end) => print_ternary("Bin.slice", bin, start, end),
        PurePrim::BinAppend(bin, byte) => print_binary("Bin.append", bin, byte),
        PurePrim::BinConcat(operands) => flat([
            pure("Bin.concat "),
            sep_flat(operands.iter().map(|t| print_term(t)), || pure(", ")),
        ]),
        PurePrim::Lst(elems) => flat([
            pure("["),
            sep_flat(elems.iter().map(|t| print_term(t)), || pure(", ")),
            pure("]"),
        ]),
        PurePrim::LstLen(t) => print_unary("Lst.len", t),
        PurePrim::LstGet(list, index) => print_binary("Lst.get", list, index),
        PurePrim::LstSlice(list, start, end) => print_ternary("Lst.slice", list, start, end),
        PurePrim::LstAppend(list, elem) => print_binary("Lst.append", list, elem),
        PurePrim::LstConcat(operands) => flat([
            pure("Lst.concat "),
            sep_flat(operands.iter().map(|t| print_term(t)), || pure(", ")),
        ]),
        PurePrim::LstMap(src, f) => print_binary("Lst.map", src, f),
        PurePrim::Io(token) => pure(format!("Io({token})")),
        PurePrim::IoEql(left, right) => print_binary("Io.eql", left, right),
    }
}

fn print_host_prim<'a>(prim: &'a HostPrim) -> Printer<'a> {
    match prim {
        HostPrim::Foreign(function, args) => flat(
            [pure(function.label.clone())]
                .into_iter()
                .chain(args.iter().flat_map(|arg| [pure(" "), print_term(arg)]))
                .collect::<Vec<_>>(),
        ),
        HostPrim::IoExit(code) => print_unary("Io.exit", code),
    }
}

fn print_cell_prim<'a>(prim: &'a CellPrim) -> Printer<'a> {
    match prim {
        CellPrim::New(init) => print_unary("Cell.new", init),
        CellPrim::Set(cell, value) => print_binary("Cell.set", cell, value),
        CellPrim::Get(cell) => print_unary("Cell.get", cell),
    }
}

fn print_term<'a>(term: &'a Term) -> Printer<'a> {
    match &**term {
        Subterm::Erased => pure("_"),
        Subterm::Unreachable => pure("unreachable"),
        Subterm::Prim(prim) => print_prim(prim),
        Subterm::NatMatch(NatMatch::Induction {
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
        Subterm::NatMatch(NatMatch::Dispatch {
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
        Subterm::Func(Func {
            captures,
            params,
            body,
            ..
        }) => {
            let params_str = params
                .iter()
                .map(|p| format!("#{}", p.as_str()))
                .collect::<Vec<_>>()
                .join(", ");
            if captures.is_empty() {
                flat([
                    pure(format!("({params_str})")),
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
                    pure(format!("({params_str})")),
                    pure(" =>\n"),
                    indent(print_term(body)),
                ])
            }
        }
        Subterm::Apply(super::Apply { head, params }) => flat([
            print_term(head),
            pure("("),
            sep_flat(params.iter().map(|p| print_term(p)), || pure(", ")),
            pure(")"),
        ]),
        Subterm::Tuple(Tuple { fields }) => flat([
            pure("("),
            sep_flat(fields.iter().map(|f| print_term(f)), || pure(", ")),
            pure(")"),
        ]),
        Subterm::Proj(Proj { head, index }) => {
            flat([pure("("), print_term(head), pure(format!(").{index}"))])
        }
        Subterm::Atom(atom) => print_atom(atom),
        Subterm::Match(Match { head, cases }) => {
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
        Subterm::Let(Let { name, body, tail }) => flat([
            pure("let "),
            pure(format!("#{}", name.as_str())),
            pure(" =\n"),
            indent(flat([print_term(body), pure(";")])),
            pure("\n"),
            print_term(tail),
        ]),
        Subterm::Rec(Rec { names, items, tail }) => {
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
        Subterm::Name(name) => pure(format!("#{}", name.as_str())),
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self), formatter, 2)
    }
}

impl Display for Module {
    // Iterates the flat items (each body printed via `Term`'s `Display`) — O(N),
    // so `--print ersd` cannot re-trigger the prelude-depth overflow.
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        for item in &self.items {
            match item {
                Item::Let { name, body } => {
                    writeln!(formatter, "let #{name} =\n{body};\n")?;
                }
                Item::Rec { names, items } => {
                    write!(formatter, "rec ")?;

                    for (index, (name, body)) in names.iter().zip(items).enumerate() {
                        if index > 0 {
                            write!(formatter, "and ")?;
                        }

                        write!(formatter, "#{name} =\n{body} ")?;
                    }

                    writeln!(formatter, ";")?;
                }
            }
        }

        write!(formatter, "{}", self.body)
    }
}
