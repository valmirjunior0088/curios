#[cfg(test)]
mod tests;

use {
    super::{
        Apply, BinPattern, BinSegment, Choose, ChooseArm, ChooseTest, ConceptField, Field, Func,
        FuncParam, FuncSugarParam, FuncType, FuncTypeParam, GroupItem, Infix, Intrinsic, Let,
        LetBinding, LetSignature, ListEntry, ListPattern, Match, MatchPattern, MatchPatternField,
        Nat, NatLiteral, NatPattern, NumLit, Pattern, PatternField, Proj, Radix, Rec, StructLit,
        StructLitEntry, Subterm, Syn, Term, TopCase, TopConcept, TopForeign, TopInduct, TopItem,
        TopLet, TopMod, TopStruct, TopUse, TopWitness, Tuple, TupleField, TupleType,
        TupleTypeParam, UseGroup, WitnessEntry,
    },
    crate::{format::claim_comments_before, parse::op_precedence},
    curios_abi::{WireSignature, WireType},
    curios_base::{
        Grain, Plicity,
        printer::{
            Printer, fill, flat, group, hard_line, if_break, indent, line, line_suffix, pure,
            sep_flat, soft_line,
        },
    },
    num_bigint::BigUint,
    num_traits::One,
};

fn print_plicity(plicity: Plicity) -> Printer {
    match plicity {
        Plicity::Implicit => pure("@"),
        Plicity::Witness => pure("use "),
        Plicity::Explicit => pure(""),
    }
}

/// A bracketed **sequence**: flat when the group fits (`f(a, b)`), otherwise one element per line at the next indent with the closer *riding* the last one (`c)`).
///
/// The closer rides because the last element already ends the sequence; following it with `,` and a bracket on a line of its own spends two lines on punctuation. Calls, list and packed literals, tuple literals and types, and variant payloads are all this shape — what they delimit is a run of expressions, and none of them cares what kind the last one is. [`listed_block`] is the other shape, for the brace forms that read as records.
fn listed(open: impl Into<String>, items: Vec<Printer>, close: &'static str) -> Printer {
    let open = pure(open);
    if items.is_empty() {
        return flat([open, pure(close)]);
    }
    group(flat([
        open,
        indent(flat([
            soft_line(),
            sep_flat(items, || flat([pure(","), line()])),
        ])),
        pure(close),
    ]))
}

/// A bracketed **record**: padded flat (`{ a, b }`), and broken with a trailing comma and the closer on a line of its own.
///
/// The one shape that does *not* ride its closer, and the difference is about editing rather than looks. A record's fields are the thing that grows: with the brace riding, adding one is a two-line change, because the field above has to give the brace up and take a comma. A sequence's elements do not grow that way, so [`listed`] rides and this does not. Struct literals and patterns take this shape, matching the declaration bodies in [`listed_hard`] — a literal and the `struct` it inhabits close the same way.
fn listed_block(open: impl Into<String>, items: Vec<Printer>, close: &'static str) -> Printer {
    let open = pure(open);
    if items.is_empty() {
        return flat([open, pure(close)]);
    }
    group(flat([
        open,
        indent(flat([
            line(),
            sep_flat(items, || flat([pure(","), line()])),
            if_break("", ","),
        ])),
        line(),
        pure(close),
    ]))
}

/// The wrapping counterpart to [`listed`], for a bracketed run of short interchangeable atoms: flat when the whole run fits, otherwise broken open with the items *filled* across lines at the next indent.
///
/// [`listed`] is the right shape for a structure — when one part needs its own line they all take one. A run of names is not a structure, and giving an import that treatment spends a line per name. Items arrive as plain strings and their commas are attached here, so the fill inserts only the gap between them.
fn filled(open: &'static str, items: Vec<String>, close: &'static str) -> Printer {
    if items.is_empty() {
        return pure(format!("{open}{close}"));
    }
    let last = items.len() - 1;
    let entries = items
        .into_iter()
        .enumerate()
        .map(|(index, item)| match index == last {
            true => pure(item),
            false => pure(format!("{item},")),
        })
        .collect::<Vec<_>>();
    group(flat([
        pure(open),
        indent(flat([soft_line(), fill(entries)])),
        pure(close),
    ]))
}

/// The one shape every call takes: flat when it fits, otherwise the head on its own line and each argument on its own, with the closing bracket *riding* the last one.
///
/// One shape regardless of what the last argument is. A call whose trailing lambda hung on the head line read well in isolation, but it made the layout depend on the *kind* of the final argument, and a leading argument that then broke — a lambda too wide for the line, a `let` in a nested call — stranded the block after its own closing bracket. Reading a call should not require knowing which of two layouts it got.
///
/// It is `listed` minus two things: no trailing comma, and no break before the closer. The last argument already ends the call, and saying so with `,` and a lone `)` spends two lines on punctuation.
fn riding_call(head: Term, params: Vec<(Plicity, Term)>) -> Printer {
    let items = params
        .into_iter()
        .map(|(plicity, param)| flat([print_plicity(plicity), print_term(param)]))
        .collect::<Vec<_>>();
    flat([
        print_suffix_head(head),
        group(flat([
            pure("("),
            indent(flat([
                soft_line(),
                sep_flat(items, || flat([pure(","), line()])),
            ])),
            pure(")"),
        ])),
    ])
}

/// The always-broken brace block a `struct`, `concept`, or `satisfy` body is: one field per line at the next indent with a trailing comma, the closing brace at the opening's column, regardless of how little would fit flat. An empty body prints the bare delimiters.
fn listed_hard(open: &'static str, items: Vec<Printer>, close: &'static str) -> Printer {
    if items.is_empty() {
        return pure(format!("{open}{close}"));
    }
    flat([
        pure(open),
        indent(flat([
            hard_line(),
            sep_flat(items, || flat([pure(","), hard_line()])),
            pure(","),
        ])),
        hard_line(),
        pure(close),
    ])
}

/// A body that rides its introducer — ` => body`, ` = body` — inline when it fits, on the next line one level deeper when it does not.
fn attached_body(introducer: &'static str, body: Printer) -> Printer {
    group(flat([pure(introducer), indent(flat([line(), body]))]))
}

/// The most arms a ladder may keep on one line. Beyond this it is a dispatch table rather than a decision, and a table reads as rows.
const MAX_FLAT_ARMS: usize = 3;

/// Whether a term is itself an arm ladder — the arm body that forces its enclosing ladder to break.
///
/// Nested flat, two ladders put both their `end`s on one line and nothing says which closes which: `match b | true => match c | true => false | false => true end | false => c end`. Breaking the outer gives every `end` a column that names what it closes. This is about pairing delimiters by eye, not about density — which is why it applies at any width.
fn is_ladder(body: &Term) -> bool {
    matches!(body.as_subterm(), Subterm::Match(_) | Subterm::Choose(_))
}

/// Prints a match's optional motive — ` : ` and the written term (ordinarily a lambda, `(k, v) => P`) — or nothing at all when the motive was omitted in the source.
fn print_motive(motive: Option<Term>) -> Printer {
    match motive {
        Some(motive) => flat([pure(": "), print_term(motive)]),
        None => pure(""),
    }
}

fn print_flt(value: f32) -> Printer {
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
fn print_func_type_param(param: FuncTypeParam) -> Printer {
    let typed = print_term(param.type_);
    let body = match param.label {
        Some(label) => flat([pure(label), pure(": "), typed]),
        None => typed,
    };
    flat([print_plicity(param.plicity), body])
}

/// One function-sugar binder (a `let`/`rec`/`satisfy` telescope parameter). A `use` binder is anonymous — `use type`, no label; otherwise the plicity prefixes the name (`@x` = implicit).
fn print_func_sugar_param(param: FuncSugarParam) -> Printer {
    if param.plicity == Plicity::Witness {
        flat([pure("use "), print_term(param.type_)])
    } else {
        flat([
            print_plicity(param.plicity),
            print_pattern(param.label),
            pure(": "),
            print_term(param.type_),
        ])
    }
}

/// One lambda parameter: the binder name with its optional domain annotation.
fn print_func_param((plicity, name, annotation): (Plicity, String, Option<Term>)) -> Printer {
    let bound = match annotation {
        Some(ty) => flat([pure(name), pure(": "), print_term(ty)]),
        None => pure(name),
    };
    flat([print_plicity(plicity), bound])
}

/// A tuple-literal / struct-literal field: positional, `label = value`, or the definition sugar `label(params) = value` re-sugared from the retained parameter list.
fn print_tuple_field(field: TupleField) -> Printer {
    match (field.label, field.func_params) {
        (Some(label), Some(params)) => flat([
            pure(label),
            listed("(", params.into_iter().map(print_func_param).collect(), ")"),
            attached_body(" =", print_term(field.value)),
        ]),
        (Some(label), None) => flat([pure(label), attached_body(" =", print_term(field.value))]),
        (None, _) => print_term(field.value),
    }
}

/// A struct-literal entry: a `..base` spread, a `use <term>` fill, or a plain field.
fn print_struct_entry(entry: StructLitEntry) -> Printer {
    match entry {
        StructLitEntry::Field(field) => print_tuple_field(field),
        StructLitEntry::Use(term) => flat([pure("use "), print_term(term)]),
        StructLitEntry::Spread(term) => flat([pure(".."), print_term(term)]),
    }
}

/// The optional `; ih` tail of a `Nat` fold's succ arm or an `List`/`Bin` fold's cons arm — any irrefutable pattern, printed through `print_pattern`; `None` prints nothing at all (a plain case-split), matching how it was written.
fn print_cons_ih(ih: Option<Pattern>) -> Printer {
    match ih {
        Some(ih) => flat([pure("; "), print_pattern(ih)]),
        None => pure(""),
    }
}

fn print_pattern_field(field: PatternField) -> Printer {
    match field.label {
        Some(label) => flat([pure(label), pure(" = "), print_pattern(field.value)]),
        None => print_pattern(field.value),
    }
}

/// A binder pattern: a plain name, a tuple pattern, or a struct pattern — the literal mirror of the `Tuple`/`StructLit` term-printing arms below, with `Term` replaced by `Pattern`.
fn print_pattern(pattern: Pattern) -> Printer {
    match pattern {
        Pattern::Binder(Some(name)) => pure(name),
        // Only a function-sugar `use` parameter (`Plicity::Witness`) has no source binder at all — and that path never calls `print_pattern` (see `print_func_sugar_param`), so this is unreachable.
        Pattern::Binder(None) => unreachable!("an anonymous binder has no pattern to print"),
        Pattern::Tuple(fields) => {
            if fields.len() == 1 {
                let field = fields.into_iter().next().unwrap();
                // A labeled one-element tuple pattern needs no trailing comma — the `=` already disambiguates it from a grouped pattern.
                let trailer = if field.label.is_some() { ")" } else { ",)" };
                flat([pure("("), print_pattern_field(field), pure(trailer)])
            } else {
                listed(
                    "(",
                    fields.into_iter().map(print_pattern_field).collect(),
                    ")",
                )
            }
        }
        Pattern::Struct { head, fields } => flat([
            pure(head),
            pure(" "),
            listed_block(
                "{",
                fields.into_iter().map(print_pattern_field).collect(),
                "}",
            ),
        ]),
    }
}

fn print_match_pattern_field(field: MatchPatternField) -> Printer {
    match field.label {
        Some(label) => flat([pure(label), pure(" = "), print_match_pattern(field.value)]),
        None => print_match_pattern(field.value),
    }
}

/// A match-arm pattern: a plain binder, an inductive constructor tag applied to sub-patterns, a tuple pattern, or a struct pattern — the refutable counterpart of `print_pattern` (see `MatchPattern`'s doc comment). `Ctor` stays positional (constructors have no field labels); `Tuple`/`Struct` mirror `print_pattern`'s own field-printing exactly.
fn print_match_pattern(pattern: MatchPattern) -> Printer {
    match pattern {
        MatchPattern::Binder(name) => pure(name),
        MatchPattern::Variant { tag, args } => flat([
            pure(tag),
            listed(
                "(",
                args.into_iter()
                    .map(|(plicity, pattern)| {
                        flat([print_plicity(plicity), print_match_pattern(pattern)])
                    })
                    .collect(),
                ")",
            ),
        ]),
        MatchPattern::Tuple(fields) => {
            if fields.len() == 1 {
                let field = fields.into_iter().next().unwrap();
                // A labeled one-element tuple pattern needs no trailing comma — the `=` already disambiguates it from a grouped pattern.
                let trailer = if field.label.is_some() { ")" } else { ",)" };
                flat([pure("("), print_match_pattern_field(field), pure(trailer)])
            } else {
                listed(
                    "(",
                    fields.into_iter().map(print_match_pattern_field).collect(),
                    ")",
                )
            }
        }
        MatchPattern::Struct { head, fields } => flat([
            pure(head),
            pure(" "),
            listed_block(
                "{",
                fields.into_iter().map(print_match_pattern_field).collect(),
                "}",
            ),
        ]),
        MatchPattern::Bool(false) => pure("false"),
        MatchPattern::Bool(true) => pure("true"),
        MatchPattern::Nat(NatPattern::Zero) => pure("0"),
        MatchPattern::Nat(NatPattern::Succ { pred_label, ih }) => {
            flat([pure(pred_label), pure(" + 1"), print_cons_ih(ih)])
        }
        MatchPattern::Nat(NatPattern::Lit(n)) => pure(n.to_string()),
        MatchPattern::List(ListPattern::Nil) => pure("[]"),
        MatchPattern::List(ListPattern::Cons {
            head_label,
            tail_label,
            ih,
        }) => flat([
            pure("["),
            pure(head_label),
            pure(", .."),
            pure(tail_label),
            pure("]"),
            print_cons_ih(ih),
        ]),
        MatchPattern::Bin(BinPattern::End(grain)) => pure(match grain {
            Grain::B => "b[]",
            Grain::X => "x[]",
        }),
        MatchPattern::Bin(BinPattern::Atom {
            grain,
            head_label,
            tail_label,
            ih,
        }) => flat([
            pure(match grain {
                Grain::B => "b[",
                Grain::X => "x[",
            }),
            pure(head_label),
            pure(", .."),
            pure(tail_label),
            pure("]"),
            print_cons_ih(ih),
        ]),
    }
}

/// One lambda parameter: its plicity mark (`@`/`use`), the binder pattern, and its optional domain annotation — the pattern-accepting counterpart of `print_func_param`, forked for the same reason `parse_func_pattern_param` is (see `parse.rs`). A lambda's `use` binder is named (`use show`), so the mark precedes the pattern rather than an anonymous domain type.
fn print_func_pattern_param(param: FuncParam) -> Printer {
    let FuncParam {
        plicity,
        pattern,
        annotation,
    } = param;
    let bound = match annotation {
        Some(ty) => flat([print_pattern(pattern), pure(": "), print_term(ty)]),
        None => print_pattern(pattern),
    };
    flat([print_plicity(plicity), bound])
}

fn print_labeled((label, ty): (Option<String>, Term)) -> Printer {
    match label {
        Some(label) => flat([pure(label), pure(": "), print_term(ty)]),
        None => print_term(ty),
    }
}

/// A Σ-type / struct field: positional, `label : type`, or the signature sugar `label(params) -> type` re-sugared from the retained parameter list.
fn print_field(param: TupleTypeParam) -> Printer {
    match (param.label, param.func_params) {
        (Some(label), Some(params)) => flat([
            pure(label),
            listed(
                "(",
                params.into_iter().map(print_func_type_param).collect(),
                ")",
            ),
            pure(" -> "),
            print_term(param.type_),
        ]),
        (Some(label), None) => flat([pure(label), pure(": "), print_term(param.type_)]),
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

fn print_intrinsic_call(name: impl Into<String> + 'static, args: Vec<Term>) -> Printer {
    flat([
        pure(name),
        listed("(", args.into_iter().map(print_term).collect(), ")"),
    ])
}

fn print_intrinsic(intrinsic: Intrinsic) -> Printer {
    match intrinsic {
        Intrinsic::BoolType => pure("Bool"),
        Intrinsic::Bool(false) => pure("false"),
        Intrinsic::Bool(true) => pure("true"),
        Intrinsic::BoolAnd(left, right) => print_intrinsic_call("Bool.and", vec![left, right]),
        Intrinsic::BoolOr(left, right) => print_intrinsic_call("Bool.or", vec![left, right]),
        Intrinsic::BoolXor(left, right) => print_intrinsic_call("Bool.xor", vec![left, right]),
        Intrinsic::BoolEql(left, right) => print_intrinsic_call("Bool.eql", vec![left, right]),
        Intrinsic::BoolNeq(left, right) => print_intrinsic_call("Bool.neq", vec![left, right]),
        Intrinsic::NatType => pure("Nat"),
        Intrinsic::Nat(Nat::Zero) => pure("0"),
        Intrinsic::Nat(Nat::Succ(nat, inner)) => {
            if matches!(
                inner.as_subterm(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) {
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
        Intrinsic::NatEql(left, right) => print_intrinsic_call("Nat.eql", vec![left, right]),
        Intrinsic::NatNeq(left, right) => print_intrinsic_call("Nat.neq", vec![left, right]),
        Intrinsic::NatAdd(left, right) => print_intrinsic_call("Nat.add", vec![left, right]),
        Intrinsic::NatSub(left, right) => print_intrinsic_call("Nat.sub", vec![left, right]),
        Intrinsic::NatMul(left, right) => print_intrinsic_call("Nat.mul", vec![left, right]),
        Intrinsic::NatLt(left, right) => print_intrinsic_call("Nat.lt", vec![left, right]),
        Intrinsic::NatDiv(left, right) => print_intrinsic_call("Nat.div", vec![left, right]),
        Intrinsic::NatRem(left, right) => print_intrinsic_call("Nat.rem", vec![left, right]),
        Intrinsic::NatGt(left, right) => print_intrinsic_call("Nat.gt", vec![left, right]),
        Intrinsic::NatLte(left, right) => print_intrinsic_call("Nat.lte", vec![left, right]),
        Intrinsic::NatGte(left, right) => print_intrinsic_call("Nat.gte", vec![left, right]),
        Intrinsic::NatAnd(left, right) => print_intrinsic_call("Nat.and", vec![left, right]),
        Intrinsic::NatOr(left, right) => print_intrinsic_call("Nat.or", vec![left, right]),
        Intrinsic::NatXor(left, right) => print_intrinsic_call("Nat.xor", vec![left, right]),
        Intrinsic::NatShl(left, right) => print_intrinsic_call("Nat.shl", vec![left, right]),
        Intrinsic::NatShr(left, right) => print_intrinsic_call("Nat.shr", vec![left, right]),
        Intrinsic::NatRotl(left, right) => print_intrinsic_call("Nat.rotl", vec![left, right]),
        Intrinsic::NatRotr(left, right) => print_intrinsic_call("Nat.rotr", vec![left, right]),
        Intrinsic::NatClz(operand) => print_intrinsic_call("Nat.clz", vec![operand]),
        Intrinsic::NatCtz(operand) => print_intrinsic_call("Nat.ctz", vec![operand]),
        Intrinsic::NatPopcnt(operand) => print_intrinsic_call("Nat.popcnt", vec![operand]),
        Intrinsic::IntType => pure("Int"),
        Intrinsic::Int(value) => pure(format!("{value:+}")),
        Intrinsic::IntEql(left, right) => print_intrinsic_call("Int.eql", vec![left, right]),
        Intrinsic::IntNeq(left, right) => print_intrinsic_call("Int.neq", vec![left, right]),
        Intrinsic::IntAdd(left, right) => print_intrinsic_call("Int.add", vec![left, right]),
        Intrinsic::IntSub(left, right) => print_intrinsic_call("Int.sub", vec![left, right]),
        Intrinsic::IntMul(left, right) => print_intrinsic_call("Int.mul", vec![left, right]),
        Intrinsic::IntDiv(left, right) => print_intrinsic_call("Int.div", vec![left, right]),
        Intrinsic::IntRem(left, right) => print_intrinsic_call("Int.rem", vec![left, right]),
        Intrinsic::IntLt(left, right) => print_intrinsic_call("Int.lt", vec![left, right]),
        Intrinsic::IntGt(left, right) => print_intrinsic_call("Int.gt", vec![left, right]),
        Intrinsic::IntLte(left, right) => print_intrinsic_call("Int.lte", vec![left, right]),
        Intrinsic::IntGte(left, right) => print_intrinsic_call("Int.gte", vec![left, right]),
        Intrinsic::IntAnd(left, right) => print_intrinsic_call("Int.and", vec![left, right]),
        Intrinsic::IntOr(left, right) => print_intrinsic_call("Int.or", vec![left, right]),
        Intrinsic::IntXor(left, right) => print_intrinsic_call("Int.xor", vec![left, right]),
        Intrinsic::IntShl(left, right) => print_intrinsic_call("Int.shl", vec![left, right]),
        Intrinsic::IntShr(left, right) => print_intrinsic_call("Int.shr", vec![left, right]),
        Intrinsic::IntRotl(left, right) => print_intrinsic_call("Int.rotl", vec![left, right]),
        Intrinsic::IntRotr(left, right) => print_intrinsic_call("Int.rotr", vec![left, right]),
        Intrinsic::IntClz(operand) => print_intrinsic_call("Int.clz", vec![operand]),
        Intrinsic::IntCtz(operand) => print_intrinsic_call("Int.ctz", vec![operand]),
        Intrinsic::IntPopcnt(operand) => print_intrinsic_call("Int.popcnt", vec![operand]),
        Intrinsic::FltType => pure("Flt"),
        Intrinsic::Flt(value) => print_flt(value.to_f32()),
        Intrinsic::FltAdd(left, right) => print_intrinsic_call("Flt.add", vec![left, right]),
        Intrinsic::FltSub(left, right) => print_intrinsic_call("Flt.sub", vec![left, right]),
        Intrinsic::FltMul(left, right) => print_intrinsic_call("Flt.mul", vec![left, right]),
        Intrinsic::FltDiv(left, right) => print_intrinsic_call("Flt.div", vec![left, right]),
        Intrinsic::FltRem(left, right) => print_intrinsic_call("Flt.rem", vec![left, right]),
        Intrinsic::FltEql(left, right) => print_intrinsic_call("Flt.eql", vec![left, right]),
        Intrinsic::FltNeq(left, right) => print_intrinsic_call("Flt.neq", vec![left, right]),
        Intrinsic::FltLt(left, right) => print_intrinsic_call("Flt.lt", vec![left, right]),
        Intrinsic::FltGt(left, right) => print_intrinsic_call("Flt.gt", vec![left, right]),
        Intrinsic::FltLte(left, right) => print_intrinsic_call("Flt.lte", vec![left, right]),
        Intrinsic::FltGte(left, right) => print_intrinsic_call("Flt.gte", vec![left, right]),
        Intrinsic::FltMin(left, right) => print_intrinsic_call("Flt.min", vec![left, right]),
        Intrinsic::FltMax(left, right) => print_intrinsic_call("Flt.max", vec![left, right]),
        Intrinsic::FltNeg(operand) => print_intrinsic_call("Flt.neg", vec![operand]),
        Intrinsic::FltAbs(operand) => print_intrinsic_call("Flt.abs", vec![operand]),
        Intrinsic::FltSqrt(operand) => print_intrinsic_call("Flt.sqrt", vec![operand]),
        Intrinsic::FltFloor(operand) => print_intrinsic_call("Flt.floor", vec![operand]),
        Intrinsic::FltCeil(operand) => print_intrinsic_call("Flt.ceil", vec![operand]),
        Intrinsic::FltTrunc(operand) => print_intrinsic_call("Flt.trunc", vec![operand]),
        Intrinsic::FltNearest(operand) => print_intrinsic_call("Flt.nearest", vec![operand]),
        Intrinsic::FltCopysign(left, right) => {
            print_intrinsic_call("Flt.copysign", vec![left, right])
        }
        Intrinsic::FltToLeBytes(operand) => print_intrinsic_call("Flt.to_le_bytes", vec![operand]),
        Intrinsic::FltOfLeBytes(operand) => print_intrinsic_call("Flt.of_le_bytes", vec![operand]),
        Intrinsic::NatToInt(operand) => print_intrinsic_call("Nat.to_int", vec![operand]),
        Intrinsic::NatToFlt(operand) => print_intrinsic_call("Nat.to_flt", vec![operand]),
        Intrinsic::ByteType => pure("Byte"),
        Intrinsic::Byte(value) => pure(format!("0x{value:02X}")),
        Intrinsic::ByteToNat(operand) => print_intrinsic_call("Byte.to_nat", vec![operand]),
        Intrinsic::NatToByte(operand) => print_intrinsic_call("Nat.to_byte", vec![operand]),
        Intrinsic::ByteEql(left, right) => print_intrinsic_call("Byte.eql", vec![left, right]),
        Intrinsic::ByteLt(left, right) => print_intrinsic_call("Byte.lt", vec![left, right]),
        Intrinsic::ByteLte(left, right) => print_intrinsic_call("Byte.lte", vec![left, right]),
        Intrinsic::ByteGt(left, right) => print_intrinsic_call("Byte.gt", vec![left, right]),
        Intrinsic::ByteGte(left, right) => print_intrinsic_call("Byte.gte", vec![left, right]),
        Intrinsic::IntToNat(operand) => print_intrinsic_call("Int.to_nat", vec![operand]),
        Intrinsic::IntToFlt(operand) => print_intrinsic_call("Int.to_flt", vec![operand]),
        Intrinsic::FltToNat(operand) => print_intrinsic_call("Flt.to_nat", vec![operand]),
        Intrinsic::FltToInt(operand) => print_intrinsic_call("Flt.to_int", vec![operand]),
        Intrinsic::BinType(grain) => pure(match grain {
            Grain::B => "Bits",
            Grain::X => "Bytes",
        }),
        // Entries are comma-delimited, so an operand is an ordinary term needing no parenthesization, and a coalesced run prints one escaped atom per entry — the glued spelling has no bracketed counterpart, and lowering re-coalesces the run either way. An empty segment list prints `b[]`/`x[]` on its own.
        Intrinsic::Bin(grain, segments) => listed(
            match grain {
                Grain::B => "b[",
                Grain::X => "x[",
            },
            segments
                .into_iter()
                .flat_map(move |segment| match segment {
                    BinSegment::Bytes(atoms) => atoms
                        .into_iter()
                        .map(move |atom| {
                            pure(match grain {
                                Grain::B => format!("\\{atom}"),
                                Grain::X => format!("\\{atom:02x}"),
                            })
                        })
                        .collect::<Vec<_>>(),
                    BinSegment::Atom(operand) => vec![print_term(operand)],
                    BinSegment::Spread(operand) => vec![flat([pure(".."), print_term(operand)])],
                })
                .collect(),
            "]",
        ),
        Intrinsic::BinLen(grain, operand) => print_intrinsic_call(
            format!(
                "{}.len",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![operand],
        ),
        Intrinsic::BinEql(grain, left, right) => print_intrinsic_call(
            format!(
                "{}.eql",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![left, right],
        ),
        Intrinsic::BinGet(grain, bin, index) => print_intrinsic_call(
            format!(
                "{}.get",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![bin, index],
        ),
        Intrinsic::BinSlice(grain, bin, start, end) => print_intrinsic_call(
            format!(
                "{}.slice",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![bin, start, end],
        ),
        Intrinsic::BinAppend(grain, bin, atom) => print_intrinsic_call(
            format!(
                "{}.append",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![bin, atom],
        ),
        Intrinsic::BinConcat(grain, left, right) => print_intrinsic_call(
            format!(
                "{}.concat",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![left, right],
        ),
        Intrinsic::ListType(elem) => print_intrinsic_call("List", vec![elem]),
        Intrinsic::List(entries) => listed(
            "[",
            entries
                .into_iter()
                .map(|entry| match entry {
                    ListEntry::Elem(term) => print_term(term),
                    ListEntry::Spread(term) => flat([pure(".."), print_term(term)]),
                })
                .collect(),
            "]",
        ),
        Intrinsic::ListLen(ty, operand) => print_intrinsic_call("List.len", vec![ty, operand]),
        Intrinsic::ListGet(ty, list, index) => {
            print_intrinsic_call("List.get", vec![ty, list, index])
        }
        Intrinsic::ListSlice(ty, list, start, end) => {
            print_intrinsic_call("List.slice", vec![ty, list, start, end])
        }
        Intrinsic::ListAppend(ty, list, elem) => {
            print_intrinsic_call("List.append", vec![ty, list, elem])
        }
        Intrinsic::ListConcat(ty, left, right) => {
            print_intrinsic_call("List.concat", vec![ty, left, right])
        }
        Intrinsic::ListMap(a, b, list, f) => print_intrinsic_call("List.map", vec![a, b, list, f]),
        Intrinsic::HandleType => pure("Handle"),
        Intrinsic::Handle(token) => pure(format!("Handle({token})")),
        Intrinsic::HandleEql(left, right) => print_intrinsic_call("Handle.eql", vec![left, right]),
        Intrinsic::ProcExit(code) => print_intrinsic_call("proc.exit", vec![code]),
        Intrinsic::CellType(elem) => print_intrinsic_call("Cell", vec![elem]),
        Intrinsic::Cell(type_, init) => print_intrinsic_call("Cell.new", vec![type_, init]),
        Intrinsic::CellSet(type_, cell, value) => {
            print_intrinsic_call("Cell.set", vec![type_, cell, value])
        }
        Intrinsic::CellGet(type_, cell) => print_intrinsic_call("Cell.get", vec![type_, cell]),
        Intrinsic::IoType(result) => print_intrinsic_call("Io", vec![result]),
        Intrinsic::IoPure(type_, value) => print_intrinsic_call("Io.pure", vec![type_, value]),
        Intrinsic::IoBind(from, to, action, f) => {
            print_intrinsic_call("Io.bind", vec![from, to, action, f])
        }
    }
}

pub(crate) fn print_term(term: Term) -> Printer {
    // The formatter's comment weave: during a `format` run, a parsed term claims every not-yet-claimed comment written before its own start — a leading comment breaks onto its own line before the term, a trailing one rides its line's end through the suffix channel. Printing builds in source order and never reorders, which is what makes the claim order correct. Outside a format run the claim is empty and printing is unchanged.
    let offset = term.span().map(|span| span.start);
    claimed_before(offset, || print_term_inner(term))
}

/// Claim every not-yet-claimed comment written before `offset` and prefix it to the document `build` produces — a leading comment on its own line, a trailing one riding its line's end through the suffix channel. The claim happens before the build, so an ancestor claims ahead of its descendants; `None` (a spanless term) claims nothing.
fn claimed_before(offset: Option<usize>, build: impl FnOnce() -> Printer) -> Printer {
    let comments = match offset {
        Some(offset) => claim_comments_before(offset),
        None => Vec::new(),
    };
    let doc = build();
    if comments.is_empty() {
        return doc;
    }
    let mut parts: Vec<Printer> = comments
        .into_iter()
        .map(|(text, trailing)| {
            if trailing {
                line_suffix(format!(" {text}"))
            } else {
                flat([pure(text), hard_line()])
            }
        })
        .collect();
    parts.push(doc);
    flat(parts)
}

/// Where a `let` binding claims its leading comments: the start of its earliest spanned component, since neither the `let` keyword nor the binder pattern records a span. A comment above the `let` precedes all of these, so any of them bounds the claim; taking the earliest keeps comments written inside the signature with the component they lead.
fn binding_claim_offset(binding: &LetBinding) -> Option<usize> {
    let earliest = match &binding.signature {
        LetSignature::Name {
            type_: Some(type_), ..
        } => type_,
        LetSignature::Name { type_: None, body } => body,
        LetSignature::Func { params, output, .. } => match params.first() {
            Some(param) => &param.type_,
            None => output,
        },
    };
    earliest.span().map(|span| span.start)
}

/// The parentheses the grammar demands and the tree does not record. An infix operand parenthesizes when its own operator binds looser than its position requires — the exact mirror of `op_precedence`'s climb, with the right operand one level up for left-associativity — and any whole-term form (a binding, a match, a lambda, an arrow, an effect form) parenthesizes unconditionally, since the operand grammar cannot produce it bare.
fn print_operand(term: Term, min_prec: u8) -> Printer {
    let parenthesized = match term.as_subterm() {
        Subterm::Infix(infix) => op_precedence(infix.op) < min_prec,
        Subterm::Rec(_)
        | Subterm::Let(_)
        | Subterm::Match(_)
        | Subterm::Choose(_)
        | Subterm::FuncType(_)
        | Subterm::Func(_) => true,
        _ => false,
    };
    match parenthesized {
        true => flat([pure("("), print_term(term), pure(")")]),
        false => print_term(term),
    }
}

/// [`print_operand`] for the head of an application, projection, or bang — a position above every operator, so any infix parenthesizes. A numeric literal parenthesizes too: `(1).0` must not reprint as the float literal `1.0`.
fn print_suffix_head(term: Term) -> Printer {
    match term.as_subterm() {
        Subterm::NumLit(_) | Subterm::Intrinsic(Intrinsic::Flt(_)) => {
            flat([pure("("), print_term(term), pure(")")])
        }
        _ => print_operand(term, u8::MAX),
    }
}

fn print_term_inner(term: Term) -> Printer {
    match term.into_subterm() {
        Subterm::Type => pure("Type"),
        Subterm::Prop => pure("Prop"),
        Subterm::Intrinsic(intrinsic) => print_intrinsic(intrinsic),
        Subterm::Foreign(function, args) => print_intrinsic_call(function.label.clone(), args),
        Subterm::Name(name) => pure(name.join()),
        // Both spell `?`: the written/desugared distinction matters to zonk's reporting, not to how the term reads.
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
            listed(
                "(",
                params.into_iter().map(print_func_type_param).collect(),
                ")",
            ),
            pure(" -> "),
            print_term(output),
        ]),
        // A lambda's body rides its arrow: inline when it fits, on the next line one level deeper when it does not — the corpus writes `(x, acc) => f(x)` inline.
        Subterm::Func(Func { params, body }) => flat([
            listed(
                "(",
                params.into_iter().map(print_func_pattern_param).collect(),
                ")",
            ),
            attached_body(" =>", print_term(body)),
        ]),
        Subterm::Apply(Apply { head, params }) => riding_call(head, params),
        Subterm::TupleType(TupleType { fields }) => {
            listed("{", fields.into_iter().map(print_field).collect(), "}")
        }
        Subterm::Tuple(Tuple { fields }) => {
            if fields.len() == 1 {
                let field = fields.into_iter().next().unwrap();
                // A labeled one-element tuple needs no trailing comma — the `=` already disambiguates it from a parenthesized term.
                let trailer = if field.label.is_some() { ")" } else { ",)" };
                flat([pure("("), print_tuple_field(field), pure(trailer)])
            } else {
                listed(
                    "(",
                    fields.into_iter().map(print_tuple_field).collect(),
                    ")",
                )
            }
        }
        Subterm::Proj(Proj { head, field }) => {
            let field = match field {
                Field::Index(index) => format!(".{index}"),
                Field::Label(label) => format!(".{label}"),
            };
            flat([print_suffix_head(head), pure(field)])
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
                listed("(", params.into_iter().map(print_term).collect(), ")")
            },
            pure(" "),
            listed_block(
                "{",
                entries.into_iter().map(print_struct_entry).collect(),
                "}",
            ),
        ]),
        // An arm ladder is width-adaptive: `match carry | true => b[\\1] | false => b[] end` is how the corpus writes a two-arm decision and how it reads best, and forcing every one of them onto five lines is what made a proof-heavy module half again as tall as it was written.
        //
        // Two ladders are broken regardless of width, by [`ladder_breaks`]. Arms sit at the ladder's own column when broken, never indented, so `| pattern =>` and the `end` that closes it line up.
        Subterm::Choose(Choose { arms, default }) => {
            let forced = arms.len() + 1 > MAX_FLAT_ARMS
                || std::iter::once(&default)
                    .chain(arms.iter().map(|arm| &arm.body))
                    .any(is_ladder);
            let separator = match forced {
                true => hard_line,
                false => line,
            };
            group(flat([
                pure("choose"),
                flat(
                    arms.into_iter()
                        .map(|ChooseArm { test, body }| {
                            let head = match test {
                                ChooseTest::Cond(condition) => {
                                    flat([separator(), pure("| "), print_term(condition)])
                                }
                                ChooseTest::Bind { pattern, value } => flat([
                                    separator(),
                                    pure("| "),
                                    print_match_pattern(pattern),
                                    pure(" = "),
                                    print_term(value),
                                ]),
                            };
                            flat([head, attached_body(" =>", print_term(body))])
                        })
                        .collect::<Vec<_>>(),
                ),
                separator(),
                pure("| _"),
                attached_body(" =>", print_term(default)),
                separator(),
                pure("end"),
            ]))
        }
        Subterm::Match(Match { head, motive, arms }) => {
            let forced =
                arms.len() > MAX_FLAT_ARMS || arms.iter().map(|arm| &arm.body).any(is_ladder);
            let separator = match forced {
                true => hard_line,
                false => line,
            };
            group(flat([
                pure("match "),
                print_term(head),
                print_motive(motive),
                flat(
                    arms.into_iter()
                        .map(|arm| {
                            flat([
                                separator(),
                                pure("| "),
                                print_match_pattern(arm.pattern),
                                attached_body(" =>", print_term(arm.body)),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                separator(),
                pure("end"),
            ]))
        }
        Subterm::Let(Let { bindings, tail }) => {
            // The binding documents materialize before the tail's: claiming is a build-order side effect, and an eagerly evaluated tail would claim every comment in the chain, including those leading later bindings. Each binding claims at its own head, so a comment above a `let` stays above it instead of sliding under the `=`.
            let bindings = bindings
                .into_iter()
                .map(|binding| {
                    claimed_before(binding_claim_offset(&binding), || {
                        flat([
                            pure("let "),
                            print_pattern(binding.binder),
                            print_let_signature(binding.signature, false),
                            pure(";"),
                            hard_line(),
                        ])
                    })
                })
                .collect::<Vec<_>>();
            flat(bindings.into_iter().chain([print_term(tail)]))
        }
        Subterm::Rec(Rec { items, tail }) => {
            let bindings = items
                .into_iter()
                .map(|item| flat([pure(item.label), print_let_signature(item.signature, false)]));
            flat([
                pure("rec "),
                sep_flat(bindings, || flat([hard_line(), pure("and ")])),
                pure(";"),
                hard_line(),
                print_term(tail),
            ])
        }
        Subterm::Bang(term) => flat([print_suffix_head(term), pure("!")]),
        // An overflowing operator chain breaks with the operator leading the continuation line.
        Subterm::Infix(Infix { op, left, right }) => {
            let precedence = op_precedence(op);
            group(flat([
                print_operand(left, precedence),
                indent(flat([
                    line(),
                    pure(format!("{} ", op.symbol())),
                    print_operand(right, precedence + 1),
                ])),
            ]))
        }
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

/// A `let`/`rec` signature and body. `top` selects the corpus's top-level shape — the body *always* on the next line after `=` — while a local binding's body rides the `=` inline when it fits.
fn print_let_signature(signature: LetSignature, top: bool) -> Printer {
    let bound = |body: Term| {
        if top {
            flat([pure(" ="), hard_line(), indent(print_term(body))])
        } else {
            attached_body(" =", print_term(body))
        }
    };
    match signature {
        LetSignature::Name { type_, body } => flat([
            match type_ {
                Some(type_) => flat([pure(": "), print_term(type_)]),
                None => pure(""),
            },
            bound(body),
        ]),
        // The parameter list and the `-> output` share **one** group, so what is measured is the whole signature rather than the parameters alone. Measured apart, parameters that fit on their own stay flat and leave the return type to break — which puts the `{` of a tuple type at the end of a long line and its `}` alone with the `=`. Together, the parameters break first and the return type, now starting a fresh line, fits beside the arrow. Should it still not fit, its own group breaks then, which is the right order of concessions.
        LetSignature::Func {
            params,
            output,
            body,
        } => {
            let items = params
                .into_iter()
                .map(print_func_sugar_param)
                .collect::<Vec<_>>();
            let signature = match items.is_empty() {
                true => flat([pure("()"), pure(" -> "), print_term(output)]),
                false => group(flat([
                    pure("("),
                    indent(flat([
                        soft_line(),
                        sep_flat(items, || flat([pure(","), line()])),
                        if_break("", ","),
                    ])),
                    soft_line(),
                    pure(")"),
                    pure(" -> "),
                    print_term(output),
                ])),
            };
            flat([signature, bound(body)])
        }
    }
}

fn print_pub(vis_pub: bool) -> Printer {
    if vis_pub { pure("pub ") } else { pure("") }
}

fn print_group_item(item: &GroupItem) -> String {
    match item {
        GroupItem::Mod(s) => format!("mod {s}"),
        GroupItem::Let(s) => format!("let {s}"),
        GroupItem::Both(s) => s.clone(),
    }
}

fn print_top_use(item: TopUse) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("use "),
        pure(item.name.join()),
        match item.group {
            // Filled rather than `listed`, because an import is a run of short interchangeable names and not a structure. `listed` is a group, so a head too wide for one line put every name on a line of its own — twenty-six for `/sys/Nat`. The names wrap like prose instead, and each carries its own comma so the fill only ever inserts the gap.
            UseGroup::Named(items) => {
                filled("/{", items.iter().map(print_group_item).collect(), "}")
            }
            UseGroup::Glob => pure("/*"),
        },
        pure(";"),
    ])
}

fn print_top_let(item: TopLet) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("let "),
        pure(item.label),
        print_let_signature(item.signature, true),
        pure(";"),
    ])
}

fn print_wire_type(type_: WireType) -> Printer {
    match type_ {
        WireType::Nat => pure("Nat"),
        WireType::Int => pure("Int"),
        WireType::Bool => pure("Bool"),
        WireType::Bytes => pure("Bytes"),
        WireType::Handle => pure("Handle"),
        WireType::List(element) => {
            flat([pure("List("), print_wire_type(element.into()), pure(")")])
        }
    }
}

// `parse_wire_signature` only ever produces exactly one, unnamed (`_`) result — `foreign` has no surface syntax for `/sys/Handle`'s named-record results — so the sole result is always present.
fn print_wire_signature(signature: WireSignature) -> Printer {
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
        listed(
            "(",
            params
                .into_iter()
                .map(|(_, type_)| print_wire_type(type_))
                .collect(),
            ")",
        ),
        pure(" -> "),
        print_wire_type(output),
    ])
}

fn print_top_foreign(item: TopForeign) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("foreign "),
        pure(item.label),
        pure(": "),
        print_wire_signature(item.signature),
        pure(";"),
    ])
}

fn print_top_rec(items: Vec<TopLet>) -> Printer {
    let mut iter = items.into_iter();
    let first = iter.next().unwrap();
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.vis_pub),
        pure("rec "),
        pure(first.label),
        print_let_signature(first.signature, true),
        flat(
            rest.into_iter()
                .map(|item| {
                    flat([
                        hard_line(),
                        // `pub` precedes `and`, which is the spelling the grammar accepts and the one the `induct` group beside this already emits. Reversed, a `pub` member of a `rec` group printed as `and pub f` and would not reparse — the formatter's verify gate refused the file rather than writing it, which is why `/std/Toml/values.crs` had never been formatted.
                        print_pub(item.vis_pub),
                        pure("and "),
                        pure(item.label),
                        print_let_signature(item.signature, true),
                    ])
                })
                .collect::<Vec<_>>(),
        ),
        pure(";"),
    ])
}

fn print_top_mod(item: TopMod) -> Printer {
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
            hard_line(),
            indent(print_module_items(module.items)),
            hard_line(),
            pure("end"),
        ]),
    }
}

pub(crate) fn print_module_items(items: Vec<TopItem>) -> Printer {
    sep_flat(items.into_iter().map(print_top_item), hard_line)
}

fn print_top_induct_case(case: TopCase) -> Printer {
    let payload = case
        .payload
        .into_iter()
        .map(|param| {
            // Plicity prefixes the name (`@x` = implicit) — shared with `print_field`.
            flat([
                print_plicity(param.plicity),
                print_field(TupleTypeParam {
                    label: param.label,
                    func_params: None,
                    type_: param.type_,
                }),
            ])
        })
        .collect();

    let target = match case.target {
        Some(exprs) => flat([
            pure(": "),
            listed("(", exprs.into_iter().map(print_term).collect(), ")"),
        ]),
        None => pure(""),
    };

    flat([
        hard_line(),
        pure(format!("| {}", case.label)),
        listed("(", payload, ")"),
        target,
    ])
}

fn print_top_induct_params(params: Vec<(Plicity, String, Term)>) -> Printer {
    if params.is_empty() {
        return pure("");
    }

    listed(
        "(",
        params
            .into_iter()
            .map(|(plicity, name, ty)| {
                flat([
                    print_plicity(plicity),
                    pure(name),
                    pure(": "),
                    print_term(ty),
                ])
            })
            .collect(),
        ")",
    )
}

/// The head's arity after the name: the (mandatory) result sort, preceded by an index telescope when the inductive is indexed. `: Sort` for a plain type, `: (indices) -> Sort` for an indexed one — the spellings `parse_induct_arity` accepts, so a printed declaration round-trips.
fn print_top_induct_arity(
    indices: Vec<(Option<String>, Term)>,
    rep_pub: bool,
    result_sort: Term,
) -> Printer {
    if indices.is_empty() {
        return flat([pure(": "), print_pub(rep_pub), print_term(result_sort)]);
    }

    flat([
        pure(": "),
        listed("(", indices.into_iter().map(print_labeled).collect(), ")"),
        pure(" -> "),
        print_pub(rep_pub),
        print_term(result_sort),
    ])
}

fn print_top_induct(group: Vec<TopInduct>) -> Printer {
    let mut iter = group.into_iter();
    let first = iter.next().unwrap();
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.vis_pub),
        pure("induct "),
        pure(first.label),
        print_top_induct_params(first.params),
        print_top_induct_arity(first.indices, first.rep_pub, first.result_sort),
        flat(
            first
                .cases
                .into_iter()
                .map(print_top_induct_case)
                .collect::<Vec<_>>(),
        ),
        flat(
            rest.into_iter()
                .map(|u| {
                    flat([
                        hard_line(),
                        print_pub(u.vis_pub),
                        pure("and "),
                        pure(u.label),
                        print_top_induct_params(u.params),
                        print_top_induct_arity(u.indices, u.rep_pub, u.result_sort),
                        flat(
                            u.cases
                                .into_iter()
                                .map(print_top_induct_case)
                                .collect::<Vec<_>>(),
                        ),
                    ])
                })
                .collect::<Vec<_>>(),
        ),
        hard_line(),
        pure("end"),
    ])
}

fn print_top_struct(item: TopStruct) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("struct "),
        pure(item.label),
        print_top_induct_params(item.params),
        pure(": "),
        print_pub(item.rep_pub),
        print_term(item.result_sort),
        pure(" "),
        listed_hard("{", item.fields.into_iter().map(print_field).collect(), "}"),
    ])
}

fn print_concept_field(field: ConceptField) -> Printer {
    // A superclass field is anonymous: `use <type>`, no label.
    if field.is_super {
        return flat([pure("use "), print_term(field.type_)]);
    }
    // The signature sugar `label(params) -> type` re-sugars from the retained parameter list (never set on a super field).
    match field.func_params {
        Some(params) => flat([
            pure(field.label),
            listed(
                "(",
                params.into_iter().map(print_func_type_param).collect(),
                ")",
            ),
            pure(" -> "),
            print_term(field.type_),
        ]),
        None => flat([pure(field.label), pure(": "), print_term(field.type_)]),
    }
}

fn print_top_concept(item: TopConcept) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("concept "),
        pure(item.label),
        print_top_induct_params(item.params),
        pure(": "),
        print_pub(item.rep_pub),
        print_term(item.result_sort),
        pure(" "),
        listed_hard(
            "{",
            item.fields.into_iter().map(print_concept_field).collect(),
            "}",
        ),
    ])
}

fn print_top_witness(item: TopWitness) -> Printer {
    let params = if item.params.is_empty() {
        pure("")
    } else {
        flat([
            pure(" "),
            listed(
                "(",
                item.params
                    .into_iter()
                    .map(print_func_sugar_param)
                    .collect(),
                ")",
            ),
            pure(" =>"),
        ])
    };

    let app = if item.args.is_empty() {
        pure(item.concept.join())
    } else {
        flat([
            pure(item.concept.join()),
            listed("(", item.args.into_iter().map(print_term).collect(), ")"),
        ])
    };

    flat([
        pure("satisfy"),
        params,
        pure(" "),
        app,
        pure(" "),
        listed_hard(
            "{",
            item.entries.into_iter().map(print_witness_entry).collect(),
            "}",
        ),
    ])
}

/// A witness-body entry: a `use <term>` fill or an implementation field — `label = value`, or the definition sugar `label(params) = value` re-sugared from the retained parameter list.
fn print_witness_entry(entry: WitnessEntry) -> Printer {
    let field = match entry {
        WitnessEntry::Use(term) => return flat([pure("use "), print_term(term)]),
        WitnessEntry::Field(field) => field,
    };

    match field.func_params {
        Some(params) => flat([
            pure(field.label),
            listed("(", params.into_iter().map(print_func_param).collect(), ")"),
            attached_body(" =", print_term(field.value)),
        ]),
        None => flat([
            pure(field.label),
            attached_body(" =", print_term(field.value)),
        ]),
    }
}

pub(crate) fn print_top_item(item: TopItem) -> Printer {
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
