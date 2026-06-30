use {
    super::{
        Apply, ArrMatch, BinMatch, BlnMatch, Entrypoint, Field, Func, FuncType, GroupItem,
        InductiveMatch, Infix, Let, LetBang, LetSignature, Match, Module, Motive, Nat, NatLiteral,
        NatMatch, NumLit, Pattern, Plicity, Prim, Proj, Radix, Rec, StructLit, Subterm, Syn, Term,
        TopCase, TopInductive, TopItem, TopLet, TopMod, TopStruct, TopUse, Tuple, TupleType,
        TupleTypeParam, UseGroup,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    num_bigint::BigUint,
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
    }
}

/// Prints one constructor arm pattern `tag(args…)` — the head of an inductive
/// match arm. `args` are irrefutable [`Pattern`]s.
fn print_constructor(tag: String, args: Vec<Pattern>) -> Printer<'static> {
    flat([
        pure(tag),
        pure("("),
        sep_flat(args.into_iter().map(print_pattern), || pure(", ")),
        pure(")"),
    ])
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

/// A Σ-type / struct field.
fn print_field(param: TupleTypeParam) -> Printer<'static> {
    let typed = print_term(param.type_);
    match param.label {
        Some(label) => flat([pure(label), pure(" : "), typed]),
        None => typed,
    }
}

fn format_radix(n: &BigUint, radix: Radix) -> String {
    match radix {
        Radix::Dec => format!("{n}"),
        Radix::Hex => format!("0x{n:X}"),
        Radix::Bin => format!("0b{n:b}"),
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
        Prim::BlnEql(left, right) => print_prim_call("Bln.eql", vec![left, right]),
        Prim::BlnNeq(left, right) => print_prim_call("Bln.neq", vec![left, right]),
        Prim::NatType => pure("Nat"),
        Prim::Nat(Nat::Zero) => pure("0"),
        Prim::Nat(Nat::Succ(nat, inner)) => {
            if matches!(inner.as_subterm(), Subterm::Prim(Prim::Nat(Nat::Zero))) {
                match nat {
                    NatLiteral::Number(n, radix) => pure(format_radix(&n, radix)),
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
                    NatLiteral::Number(n, _) if n.is_one() => {
                        flat([pure("Nat.succ("), print_term(inner), pure(")")])
                    }
                    NatLiteral::Number(n, radix) => flat([
                        pure(format!("Nat.succ({}, ", format_radix(&n, radix))),
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
        Prim::FltType => pure("Flt"),
        Prim::Flt(value) => print_flt(value),
        Prim::FltAdd(left, right) => print_prim_call("Flt.add", vec![left, right]),
        Prim::FltSub(left, right) => print_prim_call("Flt.sub", vec![left, right]),
        Prim::FltMul(left, right) => print_prim_call("Flt.mul", vec![left, right]),
        Prim::FltDiv(left, right) => print_prim_call("Flt.div", vec![left, right]),
        Prim::FltRem(left, right) => print_prim_call("Flt.rem", vec![left, right]),
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
        Prim::BinFlatten(operand) => print_prim_call("Bin.flatten", vec![operand]),
        Prim::ArrType(elem) => print_prim_call("Arr", vec![elem]),
        Prim::Arr(elems) => flat([
            pure("[|"),
            sep_flat(elems.into_iter().map(|operand| print_term(operand)), || {
                pure(", ")
            }),
            pure("|]"),
        ]),
        Prim::ArrLen(ty, operand) => print_prim_call("Arr.len", vec![ty, operand]),
        Prim::ArrGet(ty, list, index) => print_prim_call("Arr.get", vec![ty, list, index]),
        Prim::ArrSlice(ty, list, start, end) => {
            print_prim_call("Arr.slice", vec![ty, list, start, end])
        }
        Prim::ArrAppend(ty, list, elem) => print_prim_call("Arr.append", vec![ty, list, elem]),
        Prim::ArrConcat(ty, left, right) => print_prim_call("Arr.concat", vec![ty, left, right]),
        Prim::ArrFlatten(ty, operand) => print_prim_call("Arr.flatten", vec![ty, operand]),
        Prim::ArrMap(a, b, f, arr) => print_prim_call("Arr.map", vec![a, b, f, arr]),
        Prim::IoType => pure("Io"),
        Prim::Io(token) => pure(format!("Io({token})")),
        Prim::IoEql(left, right) => print_prim_call("Io.eql", vec![left, right]),
        Prim::IoRead(handle, count) => print_prim_call("Io.read", vec![handle, count]),
        Prim::IoWrite(handle, bytes) => print_prim_call("Io.write", vec![handle, bytes]),
        Prim::IoOpen(path, mode) => print_prim_call("Io.open", vec![path, mode]),
        Prim::IoLookup(host, port) => print_prim_call("Io.lookup", vec![host, port]),
        Prim::IoResolve(handle) => print_prim_call("Io.resolve", vec![handle]),
        Prim::IoSocket(addr) => print_prim_call("Io.socket", vec![addr]),
        Prim::IoBind(handle, addr) => print_prim_call("Io.bind", vec![handle, addr]),
        Prim::IoConnect(handle, addr) => print_prim_call("Io.connect", vec![handle, addr]),
        Prim::IoListen(handle, backlog) => print_prim_call("Io.listen", vec![handle, backlog]),
        Prim::IoAccept(handle) => print_prim_call("Io.accept", vec![handle]),
        Prim::IoStartTls(handle, sni) => print_prim_call("Io.start_tls", vec![handle, sni]),
        Prim::IoTlsServerConfig(cert, key) => {
            print_prim_call("Io.tls_server_config", vec![cert, key])
        }
        Prim::IoStartTlsServer(handle, cfg) => {
            print_prim_call("Io.start_tls_server", vec![handle, cfg])
        }
        Prim::IoSetNonblocking(handle, on) => {
            print_prim_call("Io.set_nonblocking", vec![handle, on])
        }
        Prim::IoSetRecvTimeout(handle, ms) => {
            print_prim_call("Io.set_recv_timeout", vec![handle, ms])
        }
        Prim::IoSetSendTimeout(handle, ms) => {
            print_prim_call("Io.set_send_timeout", vec![handle, ms])
        }
        Prim::IoSetReuseaddr(handle, on) => print_prim_call("Io.set_reuseaddr", vec![handle, on]),
        Prim::IoPoll(handles, events, timeout) => {
            print_prim_call("Io.poll", vec![handles, events, timeout])
        }
        Prim::IoClose(handle) => print_prim_call("Io.close", vec![handle]),
        Prim::IoClockWall => pure("Io.clock_wall"),
        Prim::IoClockMono => pure("Io.clock_mono"),
        Prim::IoRandom(count) => print_prim_call("Io.random", vec![count]),
        Prim::IoArgs => pure("Io.args"),
        Prim::IoEnv(name) => print_prim_call("Io.env", vec![name]),
        Prim::IoExit(type_, code) => print_prim_call("Io.exit", vec![type_, code]),
        Prim::CellType(elem) => print_prim_call("Cell", vec![elem]),
        Prim::Cell(type_, init) => print_prim_call("Cell.new", vec![type_, init]),
        Prim::CellSet(type_, cell, value) => print_prim_call("Cell.set", vec![type_, cell, value]),
        Prim::CellGet(type_, cell) => print_prim_call("Cell.get", vec![type_, cell]),
    }
}

fn print_term(term: Term) -> Printer<'static> {
    match term.into_subterm() {
        Subterm::Type => pure("Type"),
        Subterm::Prop => pure("Prop"),
        Subterm::Prim(prim) => print_prim(prim),
        Subterm::Name(name) => pure(name.join()),
        Subterm::Hole => pure("?"),
        Subterm::Syn(Syn::Str(content)) => pure(format!(
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
        Subterm::Syn(Syn::Lst(elems)) => flat([
            pure("["),
            sep_flat(elems.into_iter().map(|operand| print_term(operand)), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        Subterm::FuncType(FuncType { params, output }) => flat([
            pure("("),
            sep_flat(
                params.into_iter().map(|param| {
                    // Plicity prefixes the name (`@x` = implicit).
                    let typed = print_term(param.type_);
                    let body = match param.label {
                        Some(label) => flat([pure(label), pure(" : "), typed]),
                        None => typed,
                    };
                    flat([print_plicity(param.plicity), body])
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
            let items = fields.into_iter().map(|param| indent(print_field(param)));
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
        }) => flat([
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
        ]),
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
                pure(" + 1; "),
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
            Match::Inductive(InductiveMatch { head, motive, arms }) => flat([
                pure("match "),
                print_term(head),
                print_motive(motive),
                flat(
                    arms.into_iter()
                        .map(|arm| {
                            flat([
                                pure("\n| "),
                                print_constructor(arm.tag, arm.args),
                                pure(" =>\n"),
                                indent(print_term(arm.body)),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                pure("\nend"),
            ]),
            Match::Arr(ArrMatch {
                head,
                motive,
                empty_case,
                head_label,
                tail_label,
                ih_label,
                cons_case,
            }) => flat([
                pure("match "),
                print_term(head),
                print_motive(motive),
                pure("\n| [||] =>\n"),
                indent(print_term(empty_case)),
                pure("\n| "),
                pure(head_label),
                pure(", .."),
                pure(tail_label),
                pure("; "),
                pure(ih_label),
                pure(" =>\n"),
                indent(print_term(cons_case)),
                pure("\nend"),
            ]),
            Match::Bin(BinMatch {
                head,
                motive,
                empty_case,
                head_label,
                tail_label,
                ih_label,
                cons_case,
            }) => flat([
                pure("match "),
                print_term(head),
                print_motive(motive),
                pure("\n| \\\\ =>\n"),
                indent(print_term(empty_case)),
                pure("\n| "),
                pure(head_label),
                pure(", .."),
                pure(tail_label),
                pure("; "),
                pure(ih_label),
                pure(" =>\n"),
                indent(print_term(cons_case)),
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
        Subterm::Infix(Infix { op, left, right }) => flat([
            print_term(left),
            pure(format!(" {} ", op.symbol())),
            print_term(right),
        ]),
        Subterm::NumLit(NumLit {
            magnitude,
            radix,
            signed,
            negative,
        }) => {
            let sign = if negative {
                "-"
            } else if signed {
                "+"
            } else {
                ""
            };
            pure(format!("{sign}{}", format_radix(&magnitude, radix)))
        }
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
                params.into_iter().map(|param| {
                    // Plicity prefixes the name (`@x` = implicit).
                    flat([
                        print_plicity(param.plicity),
                        print_pattern(param.pattern),
                        pure(" : "),
                        print_term(param.type_),
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

fn print_top_inductive_case(case: TopCase) -> Printer<'static> {
    let payload = sep_flat(
        case.payload.into_iter().map(|param| {
            // Plicity prefixes the name (`@x` = implicit) — shared with
            // `print_field`.
            flat([
                print_plicity(param.plicity),
                print_field(TupleTypeParam {
                    label: param.label,
                    type_: param.type_,
                }),
            ])
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

fn print_top_inductive_params(params: Vec<(Plicity, String, Term)>) -> Printer<'static> {
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

fn print_top_inductive_indices(indices: Vec<(Option<String>, Term)>) -> Printer<'static> {
    if indices.is_empty() {
        return pure("");
    }

    flat([
        pure(" : ("),
        sep_flat(indices.into_iter().map(print_labeled), || pure(", ")),
        pure(")"),
    ])
}

fn print_top_inductive(group: Vec<TopInductive>) -> Printer<'static> {
    let mut iter = group.into_iter();
    let first = iter.next().unwrap();
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.is_pub),
        pure("induct "),
        pure(first.label),
        print_top_inductive_params(first.params),
        print_top_inductive_indices(first.indices),
        flat(
            first
                .cases
                .into_iter()
                .map(print_top_inductive_case)
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
                        print_top_inductive_params(u.params),
                        print_top_inductive_indices(u.indices),
                        flat(
                            u.cases
                                .into_iter()
                                .map(print_top_inductive_case)
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
        pure(if item.rep_pub { "record " } else { "struct " }),
        pure(item.label),
        print_top_inductive_params(item.params),
        pure(" "),
        pure("{ "),
        sep_flat(item.fields.into_iter().map(print_field), || pure(", ")),
        pure(" }"),
    ])
}

fn print_top_item(item: TopItem) -> Printer<'static> {
    match item {
        TopItem::Mod(m) => print_top_mod(m),
        TopItem::Use(u) => print_top_use(u),
        TopItem::Let(l) => print_top_let(l),
        TopItem::Rec(items) => print_top_rec(items),
        TopItem::Inductive(group) => print_top_inductive(group),
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
