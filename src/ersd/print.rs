use {
    super::{
        Atom, Func, HostPrim, Item, Let, Match, Module, NatMatch, Prim, Proj, PurePrim, Rec,
        Subterm, Term, Tuple,
    },
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
        Prim::Pure(pure_prim) => print_pure_prim(pure_prim),
        Prim::Host(host_prim) => print_host_prim(host_prim),
    }
}

fn print_pure_prim<'a>(prim: &'a PurePrim) -> Printer<'a> {
    match prim {
        PurePrim::Nat(value) => pure(format!("{value}n")),
        PurePrim::NatEql(l, r) => flat([pure("Nat.eql "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatNeq(l, r) => flat([pure("Nat.neq "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatAdd(l, r) => flat([pure("Nat.add "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatSub(l, r) => flat([pure("Nat.sub "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatMul(l, r) => flat([pure("Nat.mul "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatLt(l, r) => flat([pure("Nat.lt "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatDiv(l, r) => flat([pure("Nat.div "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatRem(l, r) => flat([pure("Nat.rem "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatGt(l, r) => flat([pure("Nat.gt "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatLte(l, r) => flat([pure("Nat.lte "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatGte(l, r) => flat([pure("Nat.gte "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatAnd(l, r) => flat([pure("Nat.and "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatOr(l, r) => flat([pure("Nat.or "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatXor(l, r) => flat([pure("Nat.xor "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatShl(l, r) => flat([pure("Nat.shl "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatShr(l, r) => flat([pure("Nat.shr "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::NatToStr(t) => flat([pure("Nat.to_str "), print_term(t)]),
        PurePrim::Int(value) => pure(format!("{value}i")),
        PurePrim::IntEql(l, r) => flat([pure("Int.eql "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntNeq(l, r) => flat([pure("Int.neq "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntAdd(l, r) => flat([pure("Int.add "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntSub(l, r) => flat([pure("Int.sub "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntMul(l, r) => flat([pure("Int.mul "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntDiv(l, r) => flat([pure("Int.div "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntRem(l, r) => flat([pure("Int.rem "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntLt(l, r) => flat([pure("Int.lt "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntGt(l, r) => flat([pure("Int.gt "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntLte(l, r) => flat([pure("Int.lte "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntGte(l, r) => flat([pure("Int.gte "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntAnd(l, r) => flat([pure("Int.and "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntOr(l, r) => flat([pure("Int.or "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntXor(l, r) => flat([pure("Int.xor "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntShl(l, r) => flat([pure("Int.shl "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntShr(l, r) => flat([pure("Int.shr "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::IntToStr(t) => flat([pure("Int.to_str "), print_term(t)]),
        PurePrim::Flt(value) => print_flt(*value),
        PurePrim::FltAdd(l, r) => flat([pure("Flt.add "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltSub(l, r) => flat([pure("Flt.sub "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltMul(l, r) => flat([pure("Flt.mul "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltDiv(l, r) => flat([pure("Flt.div "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltEql(l, r) => flat([pure("Flt.eql "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltNeq(l, r) => flat([pure("Flt.neq "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltLt(l, r) => flat([pure("Flt.lt "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltGt(l, r) => flat([pure("Flt.gt "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltLte(l, r) => flat([pure("Flt.lte "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltGte(l, r) => flat([pure("Flt.gte "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltMin(l, r) => flat([pure("Flt.min "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltMax(l, r) => flat([pure("Flt.max "), print_term(l), pure(" "), print_term(r)]),
        PurePrim::FltNeg(t) => flat([pure("Flt.neg "), print_term(t)]),
        PurePrim::FltAbs(t) => flat([pure("Flt.abs "), print_term(t)]),
        PurePrim::FltSqrt(t) => flat([pure("Flt.sqrt "), print_term(t)]),
        PurePrim::FltFloor(t) => flat([pure("Flt.floor "), print_term(t)]),
        PurePrim::FltCeil(t) => flat([pure("Flt.ceil "), print_term(t)]),
        PurePrim::FltTrunc(t) => flat([pure("Flt.trunc "), print_term(t)]),
        PurePrim::FltNearest(t) => flat([pure("Flt.nearest "), print_term(t)]),
        PurePrim::FltToStr(t) => flat([pure("Flt.to_str "), print_term(t)]),
        PurePrim::FltToLeBin(t) => flat([pure("Flt.to_le_bin "), print_term(t)]),
        PurePrim::NatToInt(t) => flat([pure("Nat.to_int "), print_term(t)]),
        PurePrim::NatToFlt(t) => flat([pure("Nat.to_flt "), print_term(t)]),
        PurePrim::IntToNat(t) => flat([pure("Int.to_nat "), print_term(t)]),
        PurePrim::IntToFlt(t) => flat([pure("Int.to_flt "), print_term(t)]),
        PurePrim::FltToNat(t) => flat([pure("Flt.to_nat "), print_term(t)]),
        PurePrim::FltToInt(t) => flat([pure("Flt.to_int "), print_term(t)]),
        PurePrim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        PurePrim::BinLen(t) => flat([pure("Bin.len "), print_term(t)]),
        PurePrim::BinEql(left, right) => flat([
            pure("Bin.eql "),
            print_term(left),
            pure(" "),
            print_term(right),
        ]),
        PurePrim::BinGet(bin, index) => flat([
            pure("Bin.get "),
            print_term(bin),
            pure(" "),
            print_term(index),
        ]),
        PurePrim::BinSlice(bin, start, end) => flat([
            pure("Bin.slice "),
            print_term(bin),
            pure(" "),
            print_term(start),
            pure(" "),
            print_term(end),
        ]),
        PurePrim::BinAppend(bin, byte) => flat([
            pure("Bin.append "),
            print_term(bin),
            pure(" "),
            print_term(byte),
        ]),
        PurePrim::BinConcat(operands) => flat([
            pure("Bin.concat "),
            sep_flat(operands.iter().map(|t| print_term(t)), || pure(", ")),
        ]),
        PurePrim::Arr(elems) => flat([
            pure("["),
            sep_flat(elems.iter().map(|t| print_term(t)), || pure(", ")),
            pure("]"),
        ]),
        PurePrim::ArrLen(t) => flat([pure("Arr.len "), print_term(t)]),
        PurePrim::ArrGet(list, index) => flat([
            pure("Arr.get "),
            print_term(list),
            pure(" "),
            print_term(index),
        ]),
        PurePrim::ArrSlice(list, start, end) => flat([
            pure("Arr.slice "),
            print_term(list),
            pure(" "),
            print_term(start),
            pure(" "),
            print_term(end),
        ]),
        PurePrim::ArrAppend(list, elem) => flat([
            pure("Arr.append "),
            print_term(list),
            pure(" "),
            print_term(elem),
        ]),
        PurePrim::ArrConcat(operands) => flat([
            pure("Arr.concat "),
            sep_flat(operands.iter().map(|t| print_term(t)), || pure(", ")),
        ]),
        PurePrim::Io(token) => pure(format!("Io({token})")),
        PurePrim::Unit => pure("()"),
    }
}

fn print_host_prim<'a>(prim: &'a HostPrim) -> Printer<'a> {
    match prim {
        HostPrim::IoRead(h, n) => flat([pure("Io.read "), print_term(h), pure(" "), print_term(n)]),
        HostPrim::IoWrite(h, b) => {
            flat([pure("Io.write "), print_term(h), pure(" "), print_term(b)])
        }
        HostPrim::IoOpen(p, m) => flat([pure("Io.open "), print_term(p), pure(" "), print_term(m)]),
        HostPrim::IoConnect(h, p, ct, rt, wt) => flat([
            pure("Io.connect "),
            print_term(h),
            pure(" "),
            print_term(p),
            pure(" "),
            print_term(ct),
            pure(" "),
            print_term(rt),
            pure(" "),
            print_term(wt),
        ]),
        HostPrim::IoListen(h, p) => {
            flat([pure("Io.listen "), print_term(h), pure(" "), print_term(p)])
        }
        HostPrim::IoAccept(h) => flat([pure("Io.accept "), print_term(h)]),
        HostPrim::IoClose(h) => flat([pure("Io.close "), print_term(h)]),
        HostPrim::IoClockWall => pure("Io.clock_wall"),
        HostPrim::IoClockMono => pure("Io.clock_mono"),
        HostPrim::IoRandom(n) => flat([pure("Io.random "), print_term(n)]),
        HostPrim::IoArgs => pure("Io.args"),
        HostPrim::IoEnv(name) => flat([pure("Io.env "), print_term(name)]),
        HostPrim::IoExit(code) => flat([pure("Io.exit "), print_term(code)]),
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
