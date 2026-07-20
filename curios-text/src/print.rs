use {
    super::{
        Apply, BinPattern, BinSegment, ConceptField, CondMatch, Field, Func, FuncSugarParam,
        FuncType, FuncTypeParam, GroupItem, Infix, LadderArm, LadderTest, Let, LetSignature,
        LstEntry, LstPattern, Match, MatchPattern, MatchPatternField, MatrixMatch, Motive, Nat,
        NatLiteral, NatPattern, NumLit, Pattern, PatternField, Prim, Proj, Radix, Rec, StructLit,
        StructLitEntry, Subterm, Syn, Term, TopCase, TopConcept, TopForeign, TopInduct, TopItem,
        TopLet, TopMod, TopStruct, TopUse, TopWitness, Tuple, TupleField, TupleType,
        TupleTypeParam, UseGroup, WitnessEntry,
    },
    curios_abi::{WireSignature, WireType},
    curios_base::{
        Grain, Plicity,
        printer::{Printer, flat, indent, pure, sep_flat},
    },
    num_bigint::BigUint,
    num_traits::One,
};

/// A `Bin` spread operand under the TIGHT grammar: a suffix chain —
/// projections, calls, `!` — bottoming out at a `Name` re-parses
/// unparenthesized, but only printed GLUED (`hdr.bytes`, `f(x)`, `read()!`):
/// `print_term`'s `(head).field` projection and `(term)!` bang forms would
/// end the literal at their `)`. Anything else is wrapped in parens, matching
/// the `\..(term)` operand form.
fn print_bin_spread_operand(term: Term) -> Printer<'static> {
    fn is_bare(term: &Term) -> bool {
        match term.as_subterm() {
            Subterm::Name(_) => true,
            Subterm::Proj(Proj { head, .. }) => is_bare(head),
            Subterm::Apply(Apply { head, .. }) => is_bare(head),
            Subterm::Bang(inner) => is_bare(inner),
            _ => false,
        }
    }

    fn print_bare(term: Term) -> Printer<'static> {
        match term.into_subterm() {
            Subterm::Name(name) => pure(name.join()),
            Subterm::Proj(Proj { head, field }) => flat([
                print_bare(head),
                pure(match field {
                    Field::Index(index) => format!(".{index}"),
                    Field::Label(label) => format!(".{label}"),
                }),
            ]),
            Subterm::Apply(Apply { head, params }) => flat([
                print_bare(head),
                pure("("),
                sep_flat(
                    params
                        .into_iter()
                        .map(|(plicity, param)| flat([print_plicity(plicity), print_term(param)])),
                    || pure(", "),
                ),
                pure(")"),
            ]),
            Subterm::Bang(inner) => flat([print_bare(inner), pure("!")]),
            _ => unreachable!("guarded by is_bare"),
        }
    }

    match is_bare(&term) {
        true => print_bare(term),
        false => flat([pure("("), print_term(term), pure(")")]),
    }
}

fn print_plicity(plicity: Plicity) -> Printer<'static> {
    match plicity {
        Plicity::Implicit => pure("@"),
        Plicity::Witness => pure("use "),
        Plicity::Explicit => pure(""),
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

/// One Π-binder, as in a function type: `@?label : type` (the label optional).
fn print_func_type_param(param: FuncTypeParam) -> Printer<'static> {
    let typed = print_term(param.type_);
    let body = match param.label {
        Some(label) => flat([pure(label), pure(" : "), typed]),
        None => typed,
    };
    flat([print_plicity(param.plicity), body])
}

/// One function-sugar binder (a `let`/`rec`/`satisfy` telescope parameter). A
/// `use` binder is anonymous — `use type`, no label; otherwise the plicity
/// prefixes the name (`@x` = implicit).
fn print_func_sugar_param(param: FuncSugarParam) -> Printer<'static> {
    if param.plicity == Plicity::Witness {
        flat([pure("use "), print_term(param.type_)])
    } else {
        flat([
            print_plicity(param.plicity),
            print_pattern(param.label),
            pure(" : "),
            print_term(param.type_),
        ])
    }
}

/// One lambda parameter: the binder name with its optional domain annotation.
fn print_func_param((name, annotation): (String, Option<Term>)) -> Printer<'static> {
    match annotation {
        Some(ty) => flat([pure(name), pure(" : "), print_term(ty)]),
        None => pure(name),
    }
}

/// A tuple-literal / struct-literal field: positional, `label = value`, or the
/// definition sugar `label(params) = value` re-sugared from the retained
/// parameter list.
fn print_tuple_field(field: TupleField) -> Printer<'static> {
    match (field.label, field.func_params) {
        (Some(label), Some(params)) => flat([
            pure(label),
            pure("("),
            sep_flat(params.into_iter().map(print_func_param), || pure(", ")),
            pure(") = "),
            print_term(field.value),
        ]),
        (Some(label), None) => flat([pure(label), pure(" = "), print_term(field.value)]),
        (None, _) => print_term(field.value),
    }
}

/// A struct-literal entry: a `..base` spread, a `use <term>` fill, or a
/// plain field.
fn print_struct_entry(entry: StructLitEntry) -> Printer<'static> {
    match entry {
        StructLitEntry::Field(field) => print_tuple_field(field),
        StructLitEntry::Use(term) => flat([pure("use "), print_term(term)]),
        StructLitEntry::Spread(term) => flat([pure(".."), print_term(term)]),
    }
}

/// A tuple-pattern / struct-pattern field: positional or `label = pattern` —
/// the literal mirror of `print_tuple_field`, with `Term` replaced by
/// `Pattern` (no definition-sugar form; a pattern field is never a function).
/// The optional `; ih` tail of an `Lst`/`Bin` fold's cons arm — `None` prints
/// nothing at all (a plain case-split), matching how it was written.
fn print_cons_ih(ih_label: Option<String>) -> Printer<'static> {
    match ih_label {
        Some(ih_label) => flat([pure("; "), pure(ih_label)]),
        None => pure(""),
    }
}

fn print_pattern_field(field: PatternField) -> Printer<'static> {
    match field.label {
        Some(label) => flat([pure(label), pure(" = "), print_pattern(field.value)]),
        None => print_pattern(field.value),
    }
}

/// A binder pattern: a plain name, a tuple pattern, or a struct pattern —
/// the literal mirror of the `Tuple`/`StructLit` term-printing arms below,
/// with `Term` replaced by `Pattern`.
fn print_pattern(pattern: Pattern) -> Printer<'static> {
    match pattern {
        Pattern::Binder(Some(name)) => pure(name),
        // Only a function-sugar `use` parameter (`Plicity::Witness`) has no
        // source binder at all — and that path never calls `print_pattern`
        // (see `print_func_sugar_param`), so this is unreachable.
        Pattern::Binder(None) => unreachable!("an anonymous binder has no pattern to print"),
        Pattern::Tuple(fields) => {
            if fields.len() == 1 {
                let field = fields.into_iter().next().unwrap();
                // A labeled one-element tuple pattern needs no trailing comma
                // — the `=` already disambiguates it from a grouped pattern.
                let trailer = if field.label.is_some() { ")" } else { ",)" };
                flat([pure("("), print_pattern_field(field), pure(trailer)])
            } else {
                flat([
                    pure("("),
                    sep_flat(fields.into_iter().map(print_pattern_field), || pure(", ")),
                    pure(")"),
                ])
            }
        }
        Pattern::Struct { head, fields } => flat([
            pure(head),
            pure(" { "),
            sep_flat(fields.into_iter().map(print_pattern_field), || pure(", ")),
            pure(" }"),
        ]),
    }
}

fn print_match_pattern_field(field: MatchPatternField) -> Printer<'static> {
    match field.label {
        Some(label) => flat([pure(label), pure(" = "), print_match_pattern(field.value)]),
        None => print_match_pattern(field.value),
    }
}

/// A match-arm pattern: a plain binder, an inductive constructor tag applied
/// to sub-patterns, a tuple pattern, or a struct pattern — the refutable
/// counterpart of `print_pattern` (see `MatchPattern`'s doc comment). `Ctor`
/// stays positional (constructors have no field labels); `Tuple`/`Struct`
/// mirror `print_pattern`'s own field-printing exactly.
fn print_match_pattern(pattern: MatchPattern) -> Printer<'static> {
    match pattern {
        MatchPattern::Binder(name) => pure(name),
        MatchPattern::Ctor { tag, args } => flat([
            pure(tag),
            pure("("),
            sep_flat(args.into_iter().map(print_match_pattern), || pure(", ")),
            pure(")"),
        ]),
        MatchPattern::Tuple(fields) => {
            if fields.len() == 1 {
                let field = fields.into_iter().next().unwrap();
                // A labeled one-element tuple pattern needs no trailing comma
                // — the `=` already disambiguates it from a grouped pattern.
                let trailer = if field.label.is_some() { ")" } else { ",)" };
                flat([pure("("), print_match_pattern_field(field), pure(trailer)])
            } else {
                flat([
                    pure("("),
                    sep_flat(fields.into_iter().map(print_match_pattern_field), || {
                        pure(", ")
                    }),
                    pure(")"),
                ])
            }
        }
        MatchPattern::Struct { head, fields } => flat([
            pure(head),
            pure(" { "),
            sep_flat(fields.into_iter().map(print_match_pattern_field), || {
                pure(", ")
            }),
            pure(" }"),
        ]),
        MatchPattern::Bln(false) => pure("false"),
        MatchPattern::Bln(true) => pure("true"),
        MatchPattern::Nat(NatPattern::Zero) => pure("0"),
        MatchPattern::Nat(NatPattern::Succ {
            pred_label,
            ih_label,
        }) => flat([pure(pred_label), pure(" + 1; "), pure(ih_label)]),
        MatchPattern::Nat(NatPattern::Lit(n)) => pure(n.to_string()),
        MatchPattern::Lst(LstPattern::Nil) => pure("[]"),
        MatchPattern::Lst(LstPattern::Cons {
            head_label,
            tail_label,
            ih_label,
        }) => flat([
            pure("["),
            pure(head_label),
            pure(", .."),
            pure(tail_label),
            pure("]"),
            print_cons_ih(ih_label),
        ]),
        MatchPattern::Bin(BinPattern::End(grain)) => pure(format!(
            "{}\\",
            match grain {
                Grain::B => "b",
                Grain::X => "x",
            }
        )),
        MatchPattern::Bin(BinPattern::Atom {
            grain,
            head_label,
            tail_label,
            ih_label,
        }) => flat([
            pure(match grain {
                Grain::B => "b\\",
                Grain::X => "x\\",
            }),
            pure(head_label),
            pure("\\.."),
            pure(tail_label),
            print_cons_ih(ih_label),
        ]),
    }
}

/// One lambda parameter: the binder pattern with its optional domain
/// annotation — the pattern-accepting counterpart of `print_func_param`,
/// forked for the same reason `parse_func_pattern_param` is (see `parse.rs`).
fn print_func_pattern_param((pattern, annotation): (Pattern, Option<Term>)) -> Printer<'static> {
    match annotation {
        Some(ty) => flat([print_pattern(pattern), pure(" : "), print_term(ty)]),
        None => print_pattern(pattern),
    }
}

fn print_labeled((label, ty): (Option<String>, Term)) -> Printer<'static> {
    match label {
        Some(label) => flat([pure(label), pure(" : "), print_term(ty)]),
        None => print_term(ty),
    }
}

/// A Σ-type / struct field: positional, `label : type`, or the signature sugar
/// `label(params) -> type` re-sugared from the retained parameter list.
fn print_field(param: TupleTypeParam) -> Printer<'static> {
    match (param.label, param.func_params) {
        (Some(label), Some(params)) => flat([
            pure(label),
            pure("("),
            sep_flat(params.into_iter().map(print_func_type_param), || pure(", ")),
            pure(") -> "),
            print_term(param.type_),
        ]),
        (Some(label), None) => flat([pure(label), pure(" : "), print_term(param.type_)]),
        (None, _) => print_term(param.type_),
    }
}

fn format_radix(n: &BigUint, radix: Radix) -> String {
    match radix {
        Radix::Dec => format!("{n}"),
        Radix::Hex => format!("0x{n:X}"),
        Radix::Bin => format!("0b{n:b}"),
    }
}

fn print_prim_call(name: impl Into<String> + 'static, args: Vec<Term>) -> Printer<'static> {
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
                let NatLiteral(n, radix) = nat;
                pure(format_radix(&n, radix))
            } else {
                match nat {
                    NatLiteral(n, _) if n.is_one() => {
                        flat([pure("Nat.succ("), print_term(inner), pure(")")])
                    }
                    NatLiteral(n, radix) => flat([
                        pure(format!("Nat.succ({}, ", format_radix(&n, radix))),
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
        Prim::Flt(value) => print_flt(value.to_f32()),
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
        Prim::FltToLeBytes(operand) => print_prim_call("Flt.to_le_bytes", vec![operand]),
        Prim::FltOfLeBytes(operand) => print_prim_call("Flt.of_le_bytes", vec![operand]),
        Prim::NatToInt(operand) => print_prim_call("Nat.to_int", vec![operand]),
        Prim::NatToFlt(operand) => print_prim_call("Nat.to_flt", vec![operand]),
        Prim::ByteType => pure("Byte"),
        Prim::Byte(value) => pure(format!("0x{value:02X}")),
        Prim::ByteToNat(operand) => print_prim_call("Byte.to_nat", vec![operand]),
        Prim::NatToByte(operand) => print_prim_call("Nat.to_byte", vec![operand]),
        Prim::ByteEql(left, right) => print_prim_call("Byte.eql", vec![left, right]),
        Prim::ByteLt(left, right) => print_prim_call("Byte.lt", vec![left, right]),
        Prim::ByteLte(left, right) => print_prim_call("Byte.lte", vec![left, right]),
        Prim::ByteGt(left, right) => print_prim_call("Byte.gt", vec![left, right]),
        Prim::ByteGte(left, right) => print_prim_call("Byte.gte", vec![left, right]),
        Prim::IntToNat(operand) => print_prim_call("Int.to_nat", vec![operand]),
        Prim::IntToFlt(operand) => print_prim_call("Int.to_flt", vec![operand]),
        Prim::FltToNat(operand) => print_prim_call("Flt.to_nat", vec![operand]),
        Prim::FltToInt(operand) => print_prim_call("Flt.to_int", vec![operand]),
        Prim::BinType(grain) => pure(match grain {
            Grain::B => "Bits",
            Grain::X => "Bytes",
        }),
        Prim::Bin(grain, segments) => flat([
            pure(format!("{grain:?}").to_lowercase()),
            match segments.is_empty() {
                true => pure("\\"),
                false => flat(segments.into_iter().map(move |segment| {
                    match segment {
                        BinSegment::Bytes(atoms) => pure(match grain {
                            Grain::B => atoms
                                .iter()
                                .map(|bit| format!("\\{bit}"))
                                .collect::<String>(),
                            Grain::X => atoms
                                .iter()
                                .map(|byte| format!("\\{byte:02x}"))
                                .collect::<String>(),
                        }),
                        BinSegment::Spread(operand) => {
                            flat([pure("\\.."), print_bin_spread_operand(operand)])
                        }
                    }
                })),
            },
        ]),
        Prim::BinLen(grain, operand) => print_prim_call(
            format!(
                "{}.len",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![operand],
        ),
        Prim::BinEql(grain, left, right) => print_prim_call(
            format!(
                "{}.eql",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![left, right],
        ),
        Prim::BinGet(grain, bin, index) => print_prim_call(
            format!(
                "{}.get",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![bin, index],
        ),
        Prim::BinSlice(grain, bin, start, end) => print_prim_call(
            format!(
                "{}.slice",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![bin, start, end],
        ),
        Prim::BinAppend(grain, bin, atom) => print_prim_call(
            format!(
                "{}.append",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![bin, atom],
        ),
        Prim::BinConcat(grain, left, right) => print_prim_call(
            format!(
                "{}.concat",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![left, right],
        ),
        Prim::LstType(elem) => print_prim_call("Lst", vec![elem]),
        Prim::Lst(entries) => flat([
            pure("["),
            sep_flat(
                entries.into_iter().map(|entry| match entry {
                    LstEntry::Elem(term) => print_term(term),
                    LstEntry::Spread(term) => flat([pure(".."), print_term(term)]),
                }),
                || pure(", "),
            ),
            pure("]"),
        ]),
        Prim::LstLen(ty, operand) => print_prim_call("Lst.len", vec![ty, operand]),
        Prim::LstGet(ty, list, index) => print_prim_call("Lst.get", vec![ty, list, index]),
        Prim::LstSlice(ty, list, start, end) => {
            print_prim_call("Lst.slice", vec![ty, list, start, end])
        }
        Prim::LstAppend(ty, list, elem) => print_prim_call("Lst.append", vec![ty, list, elem]),
        Prim::LstConcat(ty, left, right) => print_prim_call("Lst.concat", vec![ty, left, right]),
        Prim::LstMap(a, b, lst, f) => print_prim_call("Lst.map", vec![a, b, lst, f]),
        Prim::IoType => pure("Io"),
        Prim::Io(token) => pure(format!("Io({token})")),
        Prim::IoEql(left, right) => print_prim_call("Io.eql", vec![left, right]),
        Prim::Foreign(function, args) => print_prim_call(function.label.clone(), args),
        Prim::IoExit(type_, code) => print_prim_call("Io.exit", vec![type_, code]),
        Prim::CellType(elem) => print_prim_call("Cell", vec![elem]),
        Prim::Cell(type_, init) => print_prim_call("Cell.new", vec![type_, init]),
        Prim::CellSet(type_, cell, value) => print_prim_call("Cell.set", vec![type_, cell, value]),
        Prim::CellGet(type_, cell) => print_prim_call("Cell.get", vec![type_, cell]),
    }
}

pub(crate) fn print_term(term: Term) -> Printer<'static> {
    match term.into_subterm() {
        Subterm::Type => pure("Type"),
        Subterm::Prop => pure("Prop"),
        Subterm::Prim(prim) => print_prim(prim),
        Subterm::Name(name) => pure(name.join()),
        // Both spell `?`: the written/desugared distinction matters to zonk's
        // reporting, not to how the term reads.
        Subterm::Hole | Subterm::Goal => pure("?"),
        Subterm::Syn(Syn::Char(character)) => {
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
        Subterm::FuncType(FuncType { params, output }) => flat([
            pure("("),
            sep_flat(params.into_iter().map(print_func_type_param), || pure(", ")),
            pure(") -> "),
            print_term(output),
        ]),
        Subterm::Func(Func { params, body }) => flat([
            pure("("),
            sep_flat(params.into_iter().map(print_func_pattern_param), || {
                pure(", ")
            }),
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
                let field = fields.into_iter().next().unwrap();
                // A labeled one-element tuple needs no trailing comma — the
                // `=` already disambiguates it from a parenthesized term.
                let trailer = if field.label.is_some() { ")" } else { ",)" };
                flat([pure("("), print_tuple_field(field), pure(trailer)])
            } else {
                flat([
                    pure("("),
                    sep_flat(fields.into_iter().map(print_tuple_field), || pure(", ")),
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
            entries,
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
            sep_flat(entries.into_iter().map(print_struct_entry), || pure(", ")),
            pure(" }"),
        ]),
        Subterm::Match(match_) => match match_ {
            Match::Cond(CondMatch { arms, default }) => flat([
                pure("match"),
                flat(
                    arms.into_iter()
                        .map(|LadderArm { test, body }| {
                            let head = match test {
                                LadderTest::Cond(condition) => {
                                    flat([pure("\n| "), print_term(condition), pure(" =>\n")])
                                }
                                LadderTest::Bind { pattern, value } => flat([
                                    pure("\n| "),
                                    print_match_pattern(pattern),
                                    pure(" = "),
                                    print_term(value),
                                    pure(" =>\n"),
                                ]),
                            };
                            flat([head, indent(print_term(body))])
                        })
                        .collect::<Vec<_>>(),
                ),
                pure("\n| _ =>\n"),
                indent(print_term(default)),
                pure("\nend"),
            ]),
            Match::Matrix(MatrixMatch { head, motive, arms }) => flat([
                pure("match "),
                print_term(head),
                print_motive(motive),
                flat(
                    arms.into_iter()
                        .map(|arm| {
                            flat([
                                pure("\n| "),
                                print_match_pattern(arm.pattern),
                                pure(" =>\n"),
                                indent(print_term(arm.body)),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                pure("\nend"),
            ]),
        },
        Subterm::Let(Let { bindings, tail }) => flat(
            bindings
                .into_iter()
                .map(|binding| {
                    flat([
                        pure("let "),
                        print_pattern(binding.binder),
                        print_let_signature(binding.signature),
                        pure(";"),
                        pure("\n"),
                    ])
                })
                .chain([print_term(tail)]),
        ),
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
            sep_flat(params.into_iter().map(print_func_sugar_param), || {
                pure(", ")
            }),
            pure(") -> "),
            print_term(output),
            pure(" =\n"),
            indent(print_term(body)),
        ]),
    }
}

fn print_pub(vis_pub: bool) -> Printer<'static> {
    if vis_pub { pure("pub ") } else { pure("") }
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
        print_pub(item.vis_pub),
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
        print_pub(item.vis_pub),
        pure("let "),
        pure(item.label),
        print_let_signature(item.signature),
        pure(";"),
    ])
}

fn print_wire_type(type_: WireType) -> Printer<'static> {
    match type_ {
        WireType::Nat => pure("Nat"),
        WireType::Int => pure("Int"),
        WireType::Bln => pure("Bln"),
        WireType::Bin => pure("Bin"),
        WireType::Io => pure("Io"),
        WireType::Lst(element) => flat([pure("Lst("), print_wire_type(*element), pure(")")]),
    }
}

// `parse_wire_signature` only ever produces exactly one, unnamed (`_`)
// result — `foreign` has no surface syntax for `/sys/Io`'s named-record
// results — so the sole result is always present.
fn print_wire_signature(signature: WireSignature) -> Printer<'static> {
    let WireSignature { params, results } = signature;
    let output = results
        .into_iter()
        .next()
        .expect("foreign has one result")
        .1;

    if params.is_empty() {
        return print_wire_type(output);
    }

    flat([
        pure("("),
        sep_flat(
            params.into_iter().map(|(_, type_)| print_wire_type(type_)),
            || pure(", "),
        ),
        pure(") -> "),
        print_wire_type(output),
    ])
}

fn print_top_foreign(item: TopForeign) -> Printer<'static> {
    flat([
        print_pub(item.vis_pub),
        pure("foreign "),
        pure(item.label),
        pure(" : "),
        print_wire_signature(item.signature),
        pure(";"),
    ])
}

fn print_top_rec(items: Vec<TopLet>) -> Printer<'static> {
    let mut iter = items.into_iter();
    let first = iter.next().unwrap();
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.vis_pub),
        pure("rec "),
        pure(first.label),
        print_let_signature(first.signature),
        flat(
            rest.into_iter()
                .map(|item| {
                    flat([
                        pure("\nand "),
                        print_pub(item.vis_pub),
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
            print_pub(item.vis_pub),
            pure("mod "),
            pure(item.label),
            pure(";"),
        ]),
        Some(module) => flat([
            print_pub(item.vis_pub),
            pure("mod "),
            pure(item.label),
            pure("\n"),
            indent(print_module_items(module.items)),
            pure("\nend"),
        ]),
    }
}

pub(crate) fn print_module_items(items: Vec<TopItem>) -> Printer<'static> {
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
                    func_params: None,
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

/// The head's arity after the name: the (mandatory) result sort, preceded by an
/// index telescope when the inductive is indexed. `: Sort` for a plain type,
/// `: (indices) -> Sort` for an indexed one — the spellings `parse_inductive_arity`
/// accepts, so a printed declaration round-trips.
fn print_top_inductive_arity(
    indices: Vec<(Option<String>, Term)>,
    rep_pub: bool,
    result_sort: Term,
) -> Printer<'static> {
    if indices.is_empty() {
        return flat([pure(" : "), print_pub(rep_pub), print_term(result_sort)]);
    }

    flat([
        pure(" : ("),
        sep_flat(indices.into_iter().map(print_labeled), || pure(", ")),
        pure(") -> "),
        print_pub(rep_pub),
        print_term(result_sort),
    ])
}

fn print_top_induct(group: Vec<TopInduct>) -> Printer<'static> {
    let mut iter = group.into_iter();
    let first = iter.next().unwrap();
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.vis_pub),
        pure("induct "),
        pure(first.label),
        print_top_inductive_params(first.params),
        print_top_inductive_arity(first.indices, first.rep_pub, first.result_sort),
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
                        print_pub(u.vis_pub),
                        pure("and "),
                        pure(u.label),
                        print_top_inductive_params(u.params),
                        print_top_inductive_arity(u.indices, u.rep_pub, u.result_sort),
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
        print_pub(item.vis_pub),
        pure("struct "),
        pure(item.label),
        print_top_inductive_params(item.params),
        pure(" : "),
        print_pub(item.rep_pub),
        print_term(item.result_sort),
        pure(" "),
        pure("{ "),
        sep_flat(item.fields.into_iter().map(print_field), || pure(", ")),
        pure(" }"),
    ])
}

fn print_concept_field(field: ConceptField) -> Printer<'static> {
    // A superclass field is anonymous: `use <type>`, no label.
    if field.is_super {
        return flat([pure("use "), print_term(field.type_)]);
    }
    // The signature sugar `label(params) -> type` re-sugars from the retained
    // parameter list (never set on a super field).
    match field.func_params {
        Some(params) => flat([
            pure(field.label),
            pure("("),
            sep_flat(params.into_iter().map(print_func_type_param), || pure(", ")),
            pure(") -> "),
            print_term(field.type_),
        ]),
        None => flat([pure(field.label), pure(" : "), print_term(field.type_)]),
    }
}

fn print_top_concept(item: TopConcept) -> Printer<'static> {
    flat([
        print_pub(item.vis_pub),
        pure("concept "),
        pure(item.label),
        print_top_inductive_params(item.params),
        pure(" : "),
        print_term(item.result_sort),
        pure(" { "),
        sep_flat(item.fields.into_iter().map(print_concept_field), || {
            pure(", ")
        }),
        pure(" }"),
    ])
}

fn print_top_witness(item: TopWitness) -> Printer<'static> {
    let params = if item.params.is_empty() {
        pure("")
    } else {
        flat([
            pure(" ("),
            sep_flat(item.params.into_iter().map(print_func_sugar_param), || {
                pure(", ")
            }),
            pure(") =>"),
        ])
    };

    let app = if item.args.is_empty() {
        pure(item.concept.join())
    } else {
        flat([
            pure(item.concept.join()),
            pure("("),
            sep_flat(item.args.into_iter().map(print_term), || pure(", ")),
            pure(")"),
        ])
    };

    flat([
        pure("satisfy"),
        params,
        pure(" "),
        app,
        pure(" { "),
        sep_flat(item.entries.into_iter().map(print_witness_entry), || {
            pure(", ")
        }),
        pure(" }"),
    ])
}

/// A witness-body entry: a `use <term>` fill or an implementation field —
/// `label = value`, or the definition sugar `label(params) = value` re-sugared
/// from the retained parameter list.
fn print_witness_entry(entry: WitnessEntry) -> Printer<'static> {
    let field = match entry {
        WitnessEntry::Use(term) => return flat([pure("use "), print_term(term)]),
        WitnessEntry::Field(field) => field,
    };

    match field.func_params {
        Some(params) => flat([
            pure(field.label),
            pure("("),
            sep_flat(params.into_iter().map(print_func_param), || pure(", ")),
            pure(") = "),
            print_term(field.value),
        ]),
        None => flat([pure(field.label), pure(" = "), print_term(field.value)]),
    }
}

fn print_top_item(item: TopItem) -> Printer<'static> {
    match item {
        TopItem::Mod(m) => print_top_mod(m),
        TopItem::Use(u) => print_top_use(u),
        TopItem::Let(l) => print_top_let(l),
        TopItem::Rec(items) => print_top_rec(items),
        TopItem::Induct(group) => print_top_induct(group),
        TopItem::Struct(s) => print_top_struct(s),
        TopItem::Concept(c) => print_top_concept(c),
        TopItem::Witness(w) => print_top_witness(w),
        TopItem::Foreign(f) => print_top_foreign(f),
    }
}
