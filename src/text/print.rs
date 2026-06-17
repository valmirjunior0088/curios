use {
    super::{
        Apply, BlnMatch, Entrypoint, Field, Func, FuncType, GroupItem, Let, LetBang, LetSignature,
        Match, Module, Motive, Nat, NatLiteral, NatMatch, Pattern, PatternLit, Plicity, Prim, Proj,
        Rec, StructLit, Subterm, Term, TopCase, TopItem, TopLet, TopMod, TopStruct, TopUnion,
        TopUse, Tuple, TupleType, UnionMatch, UseGroup,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    num_traits::One,
    std::fmt::{Display, Formatter, Result},
};

fn print_plicity(plicity: Plicity) -> Printer<'static> {
    match plicity {
        Plicity::Implicit => pure("@"),
        Plicity::Explicit => pure(""),
    }
}

fn print_pattern(pattern: Pattern) -> Printer<'static> {
    match pattern {
        Pattern::Bind(name) => pure(name),
        Pattern::Tuple(fields) => flat([
            pure("("),
            sep_flat(fields.into_iter().map(print_pattern), || pure(", ")),
            pure(")"),
        ]),
        // A pun field `(bar, Bind("bar"))` prints as the bare label; any other
        // binding prints as a rename `bar = <pattern>`.
        Pattern::Struct { head, fields } => flat([
            pure(head.join()),
            pure(" { "),
            sep_flat(
                fields.into_iter().map(|(label, pattern)| match pattern {
                    Pattern::Bind(ref name) if *name == label => pure(label),
                    pattern => flat([pure(label), pure(" = "), print_pattern(pattern)]),
                }),
                || pure(", "),
            ),
            pure(" }"),
        ]),
        Pattern::Variant { tag, args } => flat([
            pure(tag),
            pure("("),
            sep_flat(args.into_iter().map(print_pattern), || pure(", ")),
            pure(")"),
        ]),
        Pattern::Lit(PatternLit::Nat(n)) => pure(n.to_string()),
        Pattern::Lit(PatternLit::Bln(b)) => pure(b.to_string()),
    }
}

/// Prints a match's optional motive — the parenthesized ladder: ` : body`,
/// ` : (x) => body`, ` : (x : Vec(T, k)) => body` — or nothing at all when
/// the motive was omitted in the source.
fn print_motive(motive: Option<Motive>) -> Printer<'static> {
    match motive {
        Some(Motive::Constant(body)) => flat([pure(" : "), print_term(body)]),
        Some(Motive::Scrutinee { label, body }) => {
            flat([pure(" : ("), pure(label), pure(") => "), print_term(body)])
        }
        Some(Motive::Annotated {
            label,
            name,
            slots,
            body,
        }) => flat([
            pure(" : ("),
            pure(label),
            pure(" : "),
            pure(name.join()),
            if slots.is_empty() {
                pure("")
            } else {
                flat([
                    pure("("),
                    sep_flat(slots.into_iter().map(print_term), || pure(", ")),
                    pure(")"),
                ])
            },
            pure(") => "),
            print_term(body),
        ]),
        None => pure(""),
    }
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

fn print_named_field((name, field): (Option<String>, Term)) -> Printer<'static> {
    match name {
        Some(name) => flat([pure(name), pure(" = "), print_term(field)]),
        None => print_term(field),
    }
}

fn print_labeled((label, ty): (Option<String>, Term)) -> Printer<'static> {
    match label {
        Some(label) => flat([pure(label), pure(" : "), print_term(ty)]),
        None => print_term(ty),
    }
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
        Prim::BlnAnd(left, right) => print_prim_call("Bln.and", vec![left, right]),
        Prim::BlnOr(left, right) => print_prim_call("Bln.or", vec![left, right]),
        Prim::BlnXor(left, right) => print_prim_call("Bln.xor", vec![left, right]),
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
        Prim::NatAnd(left, right) => print_prim_call("Nat.and", vec![left, right]),
        Prim::NatOr(left, right) => print_prim_call("Nat.or", vec![left, right]),
        Prim::NatXor(left, right) => print_prim_call("Nat.xor", vec![left, right]),
        Prim::NatShl(left, right) => print_prim_call("Nat.shl", vec![left, right]),
        Prim::NatShr(left, right) => print_prim_call("Nat.shr", vec![left, right]),
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
        Prim::IntAnd(left, right) => print_prim_call("Int.and", vec![left, right]),
        Prim::IntOr(left, right) => print_prim_call("Int.or", vec![left, right]),
        Prim::IntXor(left, right) => print_prim_call("Int.xor", vec![left, right]),
        Prim::IntShl(left, right) => print_prim_call("Int.shl", vec![left, right]),
        Prim::IntShr(left, right) => print_prim_call("Int.shr", vec![left, right]),
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
        Prim::FltToLeBin(operand) => print_prim_call("Flt.to_le_bin", vec![operand]),
        Prim::NatToInt(operand) => print_prim_call("Nat.to_int", vec![operand]),
        Prim::NatToFlt(operand) => print_prim_call("Nat.to_flt", vec![operand]),
        Prim::IntToNat(operand) => print_prim_call("Int.to_nat", vec![operand]),
        Prim::IntToFlt(operand) => print_prim_call("Int.to_flt", vec![operand]),
        Prim::FltToNat(operand) => print_prim_call("Flt.to_nat", vec![operand]),
        Prim::FltToInt(operand) => print_prim_call("Flt.to_int", vec![operand]),
        Prim::BinType => pure("Bin"),
        Prim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|byte| format!("\\{:02x}", byte))
                .collect::<String>(),
        ),
        Prim::BinLen(operand) => print_prim_call("Bin.len", vec![operand]),
        Prim::BinEql(left, right) => print_prim_call("Bin.eql", vec![left, right]),
        Prim::BinGet(bin, index) => print_prim_call("Bin.get", vec![bin, index]),
        Prim::BinSlice(bin, start, end) => print_prim_call("Bin.slice", vec![bin, start, end]),
        Prim::BinAppend(bin, byte) => print_prim_call("Bin.append", vec![bin, byte]),
        Prim::BinConcat(left, right) => print_prim_call("Bin.concat", vec![left, right]),
        Prim::StrType => pure("Str"),
        Prim::Str(content) => pure(format!(
            "\"{}\"",
            content
                .chars()
                .map(|character| match character {
                    '"' => "\\\"".to_string(),
                    '\\' => "\\\\".to_string(),
                    '\n' => "\\n".to_string(),
                    '\t' => "\\t".to_string(),
                    '\r' => "\\r".to_string(),
                    _ => character.to_string(),
                })
                .collect::<String>()
        )),
        Prim::StrToBin(operand) => print_prim_call("Str.to_bin", vec![operand]),
        Prim::StrOfBin(operand) => print_prim_call("Str.of_bin", vec![operand]),
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
        Prim::IoType => pure("Io"),
        Prim::Io(token) => pure(format!("Io({token})")),
        Prim::IoRead(handle, count) => print_prim_call("Io.read", vec![handle, count]),
        Prim::IoWrite(handle, bytes) => print_prim_call("Io.write", vec![handle, bytes]),
        Prim::IoOpen(path, mode) => print_prim_call("Io.open", vec![path, mode]),
        Prim::IoResolve(host, port) => print_prim_call("Io.resolve", vec![host, port]),
        Prim::IoSocket(addr) => print_prim_call("Io.socket", vec![addr]),
        Prim::IoBind(handle, addr) => print_prim_call("Io.bind", vec![handle, addr]),
        Prim::IoConnect(handle, addr) => print_prim_call("Io.connect", vec![handle, addr]),
        Prim::IoListen(handle, backlog) => print_prim_call("Io.listen", vec![handle, backlog]),
        Prim::IoAccept(handle) => print_prim_call("Io.accept", vec![handle]),
        Prim::IoSetNonblocking(handle, on) => {
            print_prim_call("Io.set_nonblocking", vec![handle, on])
        }
        Prim::IoSetRecvTimeout(handle, ms) => {
            print_prim_call("Io.set_recv_timeout", vec![handle, ms])
        }
        Prim::IoSetSendTimeout(handle, ms) => {
            print_prim_call("Io.set_send_timeout", vec![handle, ms])
        }
        Prim::IoSetReuseaddr(handle, on) => {
            print_prim_call("Io.set_reuseaddr", vec![handle, on])
        }
        Prim::IoClose(handle) => print_prim_call("Io.close", vec![handle]),
        Prim::IoClockWall => pure("Io.clock_wall"),
        Prim::IoClockMono => pure("Io.clock_mono"),
        Prim::IoRandom(count) => print_prim_call("Io.random", vec![count]),
        Prim::IoArgs => pure("Io.args"),
        Prim::IoEnv(name) => print_prim_call("Io.env", vec![name]),
        Prim::IoExit(type_, code) => print_prim_call("Io.exit", vec![type_, code]),
    }
}

fn print_term(term: Term) -> Printer<'static> {
    match term.into_subterm() {
        Subterm::Type => pure("Type"),
        Subterm::Prim(prim) => print_prim(prim),
        Subterm::Name(name) => pure(name.join()),
        Subterm::Hole => pure("?"),
        Subterm::FuncType(FuncType { params, output }) => flat([
            pure("("),
            sep_flat(
                params.into_iter().map(|(plicity, label, ty)| {
                    flat([print_plicity(plicity), print_labeled((label, ty))])
                }),
                || pure(", "),
            ),
            pure(") -> "),
            print_term(output),
        ]),
        Subterm::Func(Func { params, body }) => flat([
            pure("("),
            sep_flat(
                params
                    .into_iter()
                    .map(|(pattern, annotation)| match annotation {
                        Some(ty) => flat([print_pattern(pattern), pure(" : "), print_term(ty)]),
                        None => print_pattern(pattern),
                    }),
                || pure(", "),
            ),
            pure(") =>\n"),
            indent(print_term(body)),
        ]),
        Subterm::Apply(Apply { head, params }) => flat([
            print_term(head),
            pure("("),
            sep_flat(
                params
                    .into_iter()
                    .map(|(plicity, p)| flat([print_plicity(plicity), print_term(p)])),
                || pure(", "),
            ),
            pure(")"),
        ]),
        Subterm::TupleType(TupleType { fields }) => {
            let items = fields.into_iter().map(|field| indent(print_labeled(field)));
            flat([pure("{ "), sep_flat(items, || pure("\n, ")), pure("\n}")])
        }
        Subterm::Tuple(Tuple { fields }) => {
            if fields.len() == 1 {
                let (name, field) = fields.into_iter().next().unwrap();
                let trailer = if name.is_some() { ")" } else { ",)" };
                flat([pure("("), print_named_field((name, field)), pure(trailer)])
            } else {
                flat([
                    pure("("),
                    sep_flat(fields.into_iter().map(print_named_field), || pure(", ")),
                    pure(")"),
                ])
            }
        }
        Subterm::Proj(Proj { head, field }) => {
            let field = match field {
                Field::Index(index) => format!(").{index}"),
                Field::Label(label) => format!(").{label}"),
            };
            flat([pure("("), print_term(head), pure(field)])
        }
        Subterm::StructLit(StructLit {
            head,
            params,
            fields,
        }) => {
            flat([
                pure(head.join()),
                if params.is_empty() {
                    pure("")
                } else {
                    flat([
                        pure("("),
                        sep_flat(params.into_iter().map(print_term), || pure(", ")),
                        pure(")"),
                    ])
                },
                pure(" { "),
                sep_flat(fields.into_iter().map(print_named_field), || pure(", ")),
                pure(" }"),
            ])
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
                print_motive(motive),
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
                print_motive(motive),
                pure("\n| 0 =>\n"),
                indent(print_term(zero_case)),
                pure("\n| "),
                pure(pred_label),
                pure(" + 1, "),
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
                print_motive(motive),
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
            Match::Union(UnionMatch { head, motive, rows }) => flat([
                pure("match "),
                print_term(head),
                print_motive(motive),
                flat(
                    rows.into_iter()
                        .map(|(pattern, body)| {
                            flat([
                                pure("\n| "),
                                print_pattern(pattern),
                                pure(" =>\n"),
                                indent(print_term(body)),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                pure("\nend"),
            ]),
        },
        Subterm::Let(Let {
            binder,
            signature,
            tail,
        }) => flat([
            pure("let "),
            print_pattern(binder),
            print_let_signature(signature),
            pure(";"),
            pure("\n"),
            print_term(tail),
        ]),
        Subterm::Rec(Rec { items, tail }) => {
            let bindings = items
                .into_iter()
                .map(|item| flat([pure(item.label), print_let_signature(item.signature)]));
            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                print_term(tail),
            ])
        }
        Subterm::LetBang(LetBang { bind, body }) => flat([
            pure("let ! = "),
            print_term(bind),
            pure(";\n"),
            print_term(body),
        ]),
        Subterm::Bang(term) => flat([pure("("), print_term(term), pure(")!")]),
    }
}

fn print_let_signature(signature: LetSignature) -> Printer<'static> {
    match signature {
        LetSignature::Name { type_, body } => flat([
            match type_ {
                Some(type_) => flat([pure(" : "), print_term(type_)]),
                None => pure(""),
            },
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
                params.into_iter().map(|(plicity, pattern, ty)| {
                    flat([
                        print_plicity(plicity),
                        print_pattern(pattern),
                        pure(" : "),
                        print_term(ty),
                    ])
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
    let payload = sep_flat(
        case.payload.into_iter().map(|(plicity, name, ty)| {
            flat([print_plicity(plicity), print_labeled((name, ty))])
        }),
        || pure(", "),
    );

    let target = match case.target {
        Some(exprs) => flat([
            pure(" : ("),
            sep_flat(exprs.into_iter().map(print_term), || pure(", ")),
            pure(")"),
        ]),
        None => pure(""),
    };

    flat([
        pure(format!("\n| {}(", case.label)),
        payload,
        pure(")"),
        target,
    ])
}

fn print_top_union_params(params: Vec<(Plicity, String, Term)>) -> Printer<'static> {
    if params.is_empty() {
        return pure("");
    }

    flat([
        pure("("),
        sep_flat(
            params.into_iter().map(|(plicity, name, ty)| {
                flat([
                    print_plicity(plicity),
                    pure(name),
                    pure(" : "),
                    print_term(ty),
                ])
            }),
            || pure(", "),
        ),
        pure(")"),
    ])
}

fn print_top_union_indices(indices: Vec<(Option<String>, Term)>) -> Printer<'static> {
    if indices.is_empty() {
        return pure("");
    }

    flat([
        pure(" : ("),
        sep_flat(indices.into_iter().map(print_labeled), || pure(", ")),
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
        print_top_union_params(first.params),
        print_top_union_indices(first.indices),
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
                        print_top_union_params(u.params),
                        print_top_union_indices(u.indices),
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

fn print_top_struct(item: TopStruct) -> Printer<'static> {
    flat([
        print_pub(item.is_pub),
        pure("struct "),
        pure(item.label),
        print_top_union_params(item.params),
        pure(" "),
        print_pub(item.rep_pub),
        pure("{ "),
        sep_flat(item.fields.into_iter().map(print_labeled), || pure(", ")),
        pure(" }"),
    ])
}

fn print_top_item(item: TopItem) -> Printer<'static> {
    match item {
        TopItem::Mod(m) => print_top_mod(m),
        TopItem::Use(u) => print_top_use(u),
        TopItem::Let(l) => print_top_let(l),
        TopItem::Rec(items) => print_top_rec(items),
        TopItem::Union(unions) => print_top_union(unions),
        TopItem::Struct(s) => print_top_struct(s),
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
        let printer = if entrypoint.module.items.is_empty() {
            print_term(entrypoint.tail)
        } else {
            flat([
                print_module_items(entrypoint.module.items),
                pure("\n"),
                print_term(entrypoint.tail),
            ])
        };
        run_printer(printer, formatter, 2)
    }
}
