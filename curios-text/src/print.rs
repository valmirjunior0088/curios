#[cfg(test)]
mod tests;

use {
    super::{
        Apply, Argument, BinPattern, BinSegment, Choose, ChooseArm, ChooseTest, ConceptField,
        Field, Func, FuncParam, FuncSugarParam, FuncType, FuncTypeParam, GroupItem, Infix,
        Intrinsic, Let, LetSignature, ListEntry, ListPattern, Match, MatchPattern,
        MatchPatternField, Nat, NatLiteral, NatPattern, NumLit, Pattern, PatternField, Proj, Radix,
        StructLit, StructLitEntry, Subterm, Syn, Term, TopCase, TopConcept, TopForeign, TopInduct,
        TopItem, TopLet, TopMod, TopStruct, TopTest, TopUse, TopWitness, Tuple, TupleField,
        TupleType, TupleTypeParam, UseGroup, WitnessEntry,
    },
    crate::parse::op_precedence,
    curios_abi::{WireSignature, WireType, stdio},
    curios_num::Natural,
    curios_print::{
        Printer, begins, fill, flat, group, hard_line, if_break, indent, line, pure, reaches,
        sep_flat, soft_line,
    },
    curios_utilities::{Grain, Plicity},
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

/// A bracketed **block**: flat when it fits, and otherwise broken open with its elements *wrapped* across as many lines as they need rather than stacked one per line.
///
/// **The shape a run of values takes, which is neither of the two either side of it.** [`listed`] gives a group and stacks when it breaks, which is right for a structure and spends a line per element on a table of 256 bytes. [`filled`] wraps but has no group at all, which is right for an import that owns its whole line and wrong for anything nested: the group is a *break opportunity the enclosing construct relies on*, and a literal that drops it leaves a deeply nested arm ladder with nowhere to split, printing wide however the fill itself is measured. This keeps the group and fills the broken form.
///
/// The closer rides the last element, as [`listed`]'s does and for the same reason.
fn listed_block_wrapped(open: &'static str, items: Vec<Printer>, close: &'static str) -> Printer {
    if items.is_empty() {
        return flat([pure(open), pure(close)]);
    }

    let last = items.len() - 1;
    let entries = items
        .into_iter()
        .enumerate()
        .map(|(index, item)| match index == last {
            true => item,
            false => flat([item, pure(",")]),
        })
        .collect::<Vec<_>>();

    group(flat([
        pure(open),
        indent(flat([soft_line(), fill(entries)])),
        pure(close),
    ]))
}

/// Whether every element of a packed literal is an atom — a plain numeral or a name — so that printing one flat costs nothing.
///
/// **What decides between [`listed_block_wrapped`] and [`listed`], and the reason the choice is about content rather than length.** A fill lays out the *gaps* between items and prints each item flat, never breaking inside one; that is right for a run of bytes and wrong for an element with structure of its own, which would be laid out on a single line however wide it grows. `/std/BigNat`'s proofs pack whole `Eq/trans(…)` chains into their literals and printed 545 columns that way. [`listed`] gives each element a line and lets it break within itself, which is what such an element needs.
fn packs_atoms(segments: &[BinSegment]) -> bool {
    segments.iter().all(|segment| match segment {
        BinSegment::Atom(term) => {
            matches!(term.as_subterm(), Subterm::NumLit(_) | Subterm::Name(_))
        }
        BinSegment::Spread(_) => false,
    })
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

/// The wrapping counterpart to [`listed`], for a bracketed run of short interchangeable atoms: the items begin on the opening delimiter's own line and wrap across as many lines as they need, each continuation at the next indent.
///
/// [`listed`] is the right shape for a structure — when one part needs its own line they all take one. A run of names is not a structure, and giving an import that treatment spends a line per name.
///
/// No enclosing `group`, and that absence *is* the layout. A group asks whether the whole run fits on one line, which is the wrong question to put to content that would rather wrap: answering no broke a `soft_line` after the delimiter and spent a line on nothing but the delimiter itself. A fill needs no such permission, deciding each gap for itself, so the first name rides the delimiter and only the wraps are indented. Items arrive as plain strings and their commas are attached here, so the fill inserts only the gap between them.
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
    flat([pure(open), indent(fill(entries)), pure(close)])
}

/// The one shape every call takes: flat when it fits, otherwise the head on its own line and each argument on its own, with the closing bracket *riding* the last one.
///
/// One shape regardless of what the last argument is. A call whose trailing lambda hung on the head line read well in isolation, but it made the layout depend on the *kind* of the final argument, and a leading argument that then broke — a lambda too wide for the line, a `let` in a nested call — stranded the block after its own closing bracket. Reading a call should not require knowing which of two layouts it got.
///
/// It is `listed` minus two things: no trailing comma, and no break before the closer. The last argument already ends the call, and saying so with `,` and a lone `)` spends two lines on punctuation.
fn riding_call(head: Term, arguments: Vec<Argument>) -> Printer {
    let items = arguments
        .into_iter()
        .map(|argument| flat([print_plicity(argument.plicity), print_term(argument.term)]))
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
fn listed_hard(
    open: &'static str,
    opens_at: Option<usize>,
    items: Vec<Printer>,
    close: &'static str,
) -> Printer {
    if items.is_empty() {
        return pure(format!("{open}{close}"));
    }
    flat([
        pure(open),
        // The body's first break comes before the first field's own mark, so a comment riding the opening delimiter's line is reported here instead — see `reached_before`. `opens_at` is where the first field begins.
        reached_before(opens_at),
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
fn attached_body(introducer: &'static str, body: Term) -> Printer {
    // Taken as a term rather than a document so the body's own start is in reach: a comment written after the introducer, on the introducer's line, is owed as soon as the source is reported consumed up to the body. Reported *before* the break, because paying it after would carry it onto the body's line — and each run would carry it one construct deeper than the last, which is a formatter that never settles.
    let reached = match body.span() {
        Some(span) => reaches(span.start),
        None => flat([]),
    };

    group(flat([
        pure(introducer),
        reached,
        indent(flat([line(), print_term(body)])),
    ]))
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
    // `Display` for `f32` never uses exponent notation (that is `{:e}`), so decimalizing is one suffix check.
    let mut string = value.to_string();

    if !string.contains('.') {
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

/// One function-sugar binder (a `let`/`satisfy` telescope parameter). A `use` binder is anonymous — `use type`, no label; otherwise the plicity prefixes the name (`@x` = implicit).
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
    // A labeled field's label carries no span, so this is what puts the mark at the field's own head rather than at its value — without it a comment written above the field would be placed inside it. An unlabeled field *is* its value, and `print_term` already marks the right place.
    let start = member_start([&field.value]);
    match (field.label, field.func_params) {
        (Some(label), Some(params)) => marked(start, || {
            flat([
                pure(label),
                listed("(", params.into_iter().map(print_func_param).collect(), ")"),
                attached_body(" =", field.value),
            ])
        }),
        (Some(label), None) => marked(start, || {
            flat([pure(label), attached_body(" =", field.value)])
        }),
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
        MatchPattern::Char(character) => print_char_literal(character),
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

fn format_radix(n: &Natural, radix: Radix) -> String {
    match radix {
        Radix::Dec => format!("{n}"),
        Radix::Hex => format!("0x{n:X}"),
        Radix::Bin => format!("0b{n:b}"),
    }
}

/// An intrinsic operation as the surface calls it: its `/sys` path applied, `Nat/shl(a, b)`, with the type arguments it takes implicitly marked `@` as the application of that declaration marks them. A `/sys` body is the only place a text-stage intrinsic node exists, and its reading spells the operation the way the `/std` re-export a reader writes does.
fn print_intrinsic_call(
    name: impl Into<String> + 'static,
    implicits: Vec<Term>,
    explicits: Vec<Term>,
) -> Printer {
    let arguments = implicits
        .into_iter()
        .map(|term| flat([print_plicity(Plicity::Implicit), print_term(term)]))
        .chain(explicits.into_iter().map(print_term))
        .collect::<Vec<_>>();
    // A row with no parameters is a constant in `/sys`, not a nullary function, and is named the way a constant is.
    if arguments.is_empty() {
        return pure(name);
    }
    flat([pure(name), listed("(", arguments, ")")])
}

fn print_intrinsic(intrinsic: Intrinsic) -> Printer {
    match intrinsic {
        Intrinsic::BoolType => pure("Bool"),
        Intrinsic::Bool(false) => pure("false"),
        Intrinsic::Bool(true) => pure("true"),
        Intrinsic::BoolAnd(left, right) => {
            print_intrinsic_call("Bool/and", vec![], vec![left, right])
        }
        Intrinsic::BoolOr(left, right) => {
            print_intrinsic_call("Bool/or", vec![], vec![left, right])
        }
        Intrinsic::BoolXor(left, right) => {
            print_intrinsic_call("Bool/xor", vec![], vec![left, right])
        }
        Intrinsic::BoolEql(left, right) => {
            print_intrinsic_call("Bool/eql", vec![], vec![left, right])
        }
        Intrinsic::BoolNeq(left, right) => {
            print_intrinsic_call("Bool/neq", vec![], vec![left, right])
        }
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
        Intrinsic::NatEql(left, right) => {
            print_intrinsic_call("Nat/eql", vec![], vec![left, right])
        }
        Intrinsic::NatNeq(left, right) => {
            print_intrinsic_call("Nat/neq", vec![], vec![left, right])
        }
        Intrinsic::NatAdd(left, right) => {
            print_intrinsic_call("Nat/add", vec![], vec![left, right])
        }
        Intrinsic::NatSub(left, right) => {
            print_intrinsic_call("Nat/sub", vec![], vec![left, right])
        }
        Intrinsic::NatMul(left, right) => {
            print_intrinsic_call("Nat/mul", vec![], vec![left, right])
        }
        Intrinsic::NatLt(left, right) => print_intrinsic_call("Nat/lt", vec![], vec![left, right]),
        Intrinsic::NatDiv {
            dividend: left,
            divisor: right,
            ..
        } => print_intrinsic_call("Nat/div", vec![], vec![left, right]),
        Intrinsic::NatRem {
            dividend: left,
            divisor: right,
            ..
        } => print_intrinsic_call("Nat/rem", vec![], vec![left, right]),
        Intrinsic::NatLe(left, right) => print_intrinsic_call("Nat/le", vec![], vec![left, right]),
        Intrinsic::NatAnd(left, right) => {
            print_intrinsic_call("Nat/and", vec![], vec![left, right])
        }
        Intrinsic::NatOr(left, right) => print_intrinsic_call("Nat/or", vec![], vec![left, right]),
        Intrinsic::NatXor(left, right) => {
            print_intrinsic_call("Nat/xor", vec![], vec![left, right])
        }
        Intrinsic::NatShl(left, right) => {
            print_intrinsic_call("Nat/shl", vec![], vec![left, right])
        }
        Intrinsic::NatShr(left, right) => {
            print_intrinsic_call("Nat/shr", vec![], vec![left, right])
        }
        Intrinsic::IntType => pure("Int"),
        Intrinsic::Int(value) => pure(format!("{value:+}")),
        Intrinsic::IntEql(left, right) => {
            print_intrinsic_call("Int/eql", vec![], vec![left, right])
        }
        Intrinsic::IntNeq(left, right) => {
            print_intrinsic_call("Int/neq", vec![], vec![left, right])
        }
        Intrinsic::IntAdd(left, right) => {
            print_intrinsic_call("Int/add", vec![], vec![left, right])
        }
        Intrinsic::IntSub(left, right) => {
            print_intrinsic_call("Int/sub", vec![], vec![left, right])
        }
        Intrinsic::IntMul(left, right) => {
            print_intrinsic_call("Int/mul", vec![], vec![left, right])
        }
        Intrinsic::IntDiv {
            dividend: left,
            divisor: right,
            ..
        } => print_intrinsic_call("Int/div", vec![], vec![left, right]),
        Intrinsic::IntRem {
            dividend: left,
            divisor: right,
            ..
        } => print_intrinsic_call("Int/rem", vec![], vec![left, right]),
        Intrinsic::IntLt(left, right) => print_intrinsic_call("Int/lt", vec![], vec![left, right]),
        Intrinsic::IntLe(left, right) => print_intrinsic_call("Int/le", vec![], vec![left, right]),
        Intrinsic::IntAnd(left, right) => {
            print_intrinsic_call("Int/and", vec![], vec![left, right])
        }
        Intrinsic::IntOr(left, right) => print_intrinsic_call("Int/or", vec![], vec![left, right]),
        Intrinsic::IntXor(left, right) => {
            print_intrinsic_call("Int/xor", vec![], vec![left, right])
        }
        Intrinsic::IntShl(left, right) => {
            print_intrinsic_call("Int/shl", vec![], vec![left, right])
        }
        Intrinsic::IntShr(left, right) => {
            print_intrinsic_call("Int/shr", vec![], vec![left, right])
        }
        Intrinsic::FltType => pure("Flt"),
        Intrinsic::Flt(value) => print_flt(value.to_f32()),
        Intrinsic::FltAdd(left, right) => {
            print_intrinsic_call("Flt/add", vec![], vec![left, right])
        }
        Intrinsic::FltSub(left, right) => {
            print_intrinsic_call("Flt/sub", vec![], vec![left, right])
        }
        Intrinsic::FltMul(left, right) => {
            print_intrinsic_call("Flt/mul", vec![], vec![left, right])
        }
        Intrinsic::FltDiv(left, right) => {
            print_intrinsic_call("Flt/div", vec![], vec![left, right])
        }
        Intrinsic::FltRem(left, right) => {
            print_intrinsic_call("Flt/rem", vec![], vec![left, right])
        }
        Intrinsic::FltEql(left, right) => {
            print_intrinsic_call("Flt/eql", vec![], vec![left, right])
        }
        Intrinsic::FltNeq(left, right) => {
            print_intrinsic_call("Flt/neq", vec![], vec![left, right])
        }
        Intrinsic::FltLt(left, right) => print_intrinsic_call("Flt/lt", vec![], vec![left, right]),
        Intrinsic::FltLe(left, right) => print_intrinsic_call("Flt/le", vec![], vec![left, right]),
        Intrinsic::FltMin(left, right) => {
            print_intrinsic_call("Flt/min", vec![], vec![left, right])
        }
        Intrinsic::FltMax(left, right) => {
            print_intrinsic_call("Flt/max", vec![], vec![left, right])
        }
        Intrinsic::FltNeg(operand) => print_intrinsic_call("Flt/neg", vec![], vec![operand]),
        Intrinsic::FltAbs(operand) => print_intrinsic_call("Flt/abs", vec![], vec![operand]),
        Intrinsic::FltSqrt(operand) => print_intrinsic_call("Flt/sqrt", vec![], vec![operand]),
        Intrinsic::FltFloor(operand) => print_intrinsic_call("Flt/floor", vec![], vec![operand]),
        Intrinsic::FltCeil(operand) => print_intrinsic_call("Flt/ceil", vec![], vec![operand]),
        Intrinsic::FltTrunc(operand) => print_intrinsic_call("Flt/trunc", vec![], vec![operand]),
        Intrinsic::FltNearest(operand) => {
            print_intrinsic_call("Flt/nearest", vec![], vec![operand])
        }
        Intrinsic::FltCopysign(left, right) => {
            print_intrinsic_call("Flt/copysign", vec![], vec![left, right])
        }
        Intrinsic::FltToLeBytes(operand) => {
            print_intrinsic_call("Flt/to_le_bytes", vec![], vec![operand])
        }
        Intrinsic::FltOfLeBytes { bin: operand, .. } => {
            print_intrinsic_call("Flt/of_le_bytes", vec![], vec![operand])
        }
        Intrinsic::NatToInt(operand) => print_intrinsic_call("Nat/to_int", vec![], vec![operand]),
        Intrinsic::NatToFlt(operand) => print_intrinsic_call("Nat/to_flt", vec![], vec![operand]),
        Intrinsic::ByteType => pure("Byte"),
        Intrinsic::Byte(value) => pure(format!("0x{value:02X}")),
        Intrinsic::ByteToNat(operand) => print_intrinsic_call("Byte/to_nat", vec![], vec![operand]),
        Intrinsic::NatToByte(operand) => print_intrinsic_call("Nat/to_byte", vec![], vec![operand]),
        Intrinsic::ByteEql(left, right) => {
            print_intrinsic_call("Byte/eql", vec![], vec![left, right])
        }
        Intrinsic::ByteLt(left, right) => {
            print_intrinsic_call("Byte/lt", vec![], vec![left, right])
        }
        Intrinsic::ByteLe(left, right) => {
            print_intrinsic_call("Byte/le", vec![], vec![left, right])
        }
        Intrinsic::IntToNat { int: operand, .. } => {
            print_intrinsic_call("Int/to_nat", vec![], vec![operand])
        }
        Intrinsic::IntToFlt(operand) => print_intrinsic_call("Int/to_flt", vec![], vec![operand]),
        Intrinsic::FltToNat { flt: operand, .. } => {
            print_intrinsic_call("Flt/to_nat", vec![], vec![operand])
        }
        Intrinsic::FltToInt { flt: operand, .. } => {
            print_intrinsic_call("Flt/to_int", vec![], vec![operand])
        }
        Intrinsic::BinType(grain) => pure(match grain {
            Grain::B => "Bits",
            Grain::X => "Bytes",
        }),
        // Entries are comma-delimited, so an operand is an ordinary term needing no parenthesization — a constant atom is a numeral term and prints as written. An empty segment list prints `b[]`/`x[]` on its own.
        Intrinsic::Bin(grain, segments) => {
            let open = match grain {
                Grain::B => "b[",
                Grain::X => "x[",
            };
            // A run of atoms wraps as a block; anything with structure in it takes a line per element, so each may break within itself — see `packs_atoms`.
            let wrapped = packs_atoms(&segments);
            let entries = segments
                .into_iter()
                .map(|segment| match segment {
                    BinSegment::Atom(operand) => print_term(operand),
                    BinSegment::Spread(operand) => flat([pure(".."), print_term(operand)]),
                })
                .collect();

            match wrapped {
                true => listed_block_wrapped(open, entries, "]"),
                false => listed(open, entries, "]"),
            }
        }
        Intrinsic::BinLen(grain, operand) => print_intrinsic_call(
            format!(
                "{}/len",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![],
            vec![operand],
        ),
        Intrinsic::BinEql(grain, left, right) => print_intrinsic_call(
            format!(
                "{}/eql",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![],
            vec![left, right],
        ),
        Intrinsic::BinGet {
            grain,
            bin,
            index,
            in_range: _,
        } => print_intrinsic_call(
            format!(
                "{}/get",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![],
            vec![bin, index],
        ),
        Intrinsic::BinSlice {
            grain,
            bin,
            start,
            length,
            within: _,
        } => print_intrinsic_call(
            format!(
                "{}/slice",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![],
            vec![bin, start, length],
        ),
        Intrinsic::BinAppend {
            grain,
            bin,
            element: atom,
        } => print_intrinsic_call(
            format!(
                "{}/append",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![],
            vec![bin, atom],
        ),
        Intrinsic::BinConcat { grain, left, right } => print_intrinsic_call(
            format!(
                "{}/concat",
                match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                }
            ),
            vec![],
            vec![left, right],
        ),
        Intrinsic::ListType(elem) => print_intrinsic_call("List", vec![], vec![elem]),
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
        Intrinsic::ListLen {
            element: ty,
            list: operand,
        } => print_intrinsic_call("List/len", vec![ty], vec![operand]),
        Intrinsic::ListGet {
            element: ty,
            list,
            index,
            in_range: _,
        } => print_intrinsic_call("List/get", vec![ty], vec![list, index]),
        Intrinsic::ListSlice {
            element: ty,
            list,
            start,
            length,
            within: _,
        } => print_intrinsic_call("List/slice", vec![ty], vec![list, start, length]),
        Intrinsic::ListAppend {
            element: ty,
            list,
            item: elem,
        } => print_intrinsic_call("List/append", vec![ty], vec![list, elem]),
        Intrinsic::ListConcat {
            element: ty,
            left,
            right,
        } => print_intrinsic_call("List/concat", vec![ty], vec![left, right]),
        Intrinsic::ListMap {
            from: a,
            to: b,
            list,
            function: f,
        } => print_intrinsic_call("List/map", vec![a, b], vec![list, f]),
        Intrinsic::HandleType => pure("Handle"),
        // The three `/sys/Handle` constants are the only handles a `/sys` body plants; the last arm spells a token no source can write rather than fail the print.
        Intrinsic::Handle(stdio::STDIN) => pure("Handle/stdin"),
        Intrinsic::Handle(stdio::STDOUT) => pure("Handle/stdout"),
        Intrinsic::Handle(stdio::STDERR) => pure("Handle/stderr"),
        Intrinsic::Handle(token) => pure(format!("Handle({token})")),
        Intrinsic::HandleEql(left, right) => {
            print_intrinsic_call("Handle/eql", vec![], vec![left, right])
        }
        Intrinsic::ProcExit(code) => print_intrinsic_call("proc/exit", vec![], vec![code]),
        Intrinsic::CellType(elem) => print_intrinsic_call("Cell", vec![], vec![elem]),
        Intrinsic::Cell {
            element: type_,
            initial: init,
        } => print_intrinsic_call("Cell/new", vec![type_], vec![init]),
        Intrinsic::CellSet {
            element: type_,
            cell,
            value,
        } => print_intrinsic_call("Cell/set", vec![type_], vec![cell, value]),
        Intrinsic::CellGet {
            element: type_,
            cell,
        } => print_intrinsic_call("Cell/get", vec![type_], vec![cell]),
        Intrinsic::IoType(result) => print_intrinsic_call("Io", vec![], vec![result]),
        Intrinsic::IoPure {
            result: type_,
            value,
        } => print_intrinsic_call("Io/pure", vec![type_], vec![value]),
        Intrinsic::IoBind {
            from,
            to,
            action,
            continuation: f,
        } => print_intrinsic_call("Io/bind", vec![from, to], vec![action, f]),
    }
}

pub(crate) fn print_term(term: Term) -> Printer {
    // The formatter's comment weave, and the whole of what the printer contributes to it: a term reports where its own text begins and how far into the source it reaches. Nothing here decides where a comment goes — that is a fact about the output line, which only the renderer knows.
    //
    // The *end* mark is what reaches a comment written past a separator the enclosing printer emits: a term's span runs to the next token, so such a comment falls inside it, and the line comes to owe the comment before the break that would otherwise carry it away. Outside a format run there are no comments and both marks emit nothing.
    let bounds = term.span().map(|span| (span.start, span.end));

    marked(bounds.map(|(start, _)| start), || match bounds {
        Some((start, end)) => flat([begins(start), print_term_inner(term), reaches(end)]),
        None => print_term_inner(term),
    })
}

/// Note that `build`'s document begins at `offset`, so the renderer can place there whatever the source held and the document does not — see [`Printer::Mark`](curios_print::Printer::Mark). `None` (a spanless node) notes nothing.
///
/// **Nothing is claimed and nothing is decided here.** A comment's place is a fact about the output — which line it rides, or which line it takes — and only the renderer knows that. What this marks is the one thing the builder does know: where in the source the document has reached.
fn marked(offset: Option<usize>, build: impl FnOnce() -> Printer) -> Printer {
    match offset {
        Some(offset) => flat([begins(offset), build()]),
        None => build(),
    }
}

/// Report the source consumed up to `offset` before a break, so a comment written on the line the break ends is paid onto that line rather than carried past it.
///
/// **The law every member list obeys.** A break separating one source construct from the next comes *before* the next construct's own mark, so a comment written after the previous one — on the line the break is about to end — is not yet owed when the line closes, and is paid on the following line instead. Each run then finds it one construct deeper than the last, which is a formatter that never settles. Reporting the position first pays it where it was written; a comment on a line of its own is untouched, since only what *begins* something pays one of those.
fn reached_before(offset: Option<usize>) -> Printer {
    match offset {
        Some(offset) => reaches(offset),
        None => flat([]),
    }
}

/// Where a member of a delimited list begins: a `match` or `choose` arm, a tuple or struct-literal field, a concept or witness field, an `induct` case.
///
/// None of these records a span of its own — an arm is a pattern and a body, a field a bare label and a value — so without a position derived at the member's own head, the mark would sit at the first spanned *descendant* and a comment written above the member would surface inside the member's body. [`signature_start`] is this same rule for `let` and `and` clauses, and its doc records the convergence failure both exist to prevent.
///
/// The earliest spanned component bounds it, for the reason it does there: a comment above the member precedes every component, so any one of them would place it, and taking the earliest keeps a comment written *inside* the member's head with the component it leads. A member whose components are all spanless reports nothing, exactly as a signature with no spanned component does.
fn member_start<'a>(terms: impl IntoIterator<Item = &'a Term>) -> Option<usize> {
    terms
        .into_iter()
        .filter_map(|term| term.span().map(|span| span.start))
        .min()
}

/// Where a `let` binding or an `and` clause begins: the start of its earliest spanned component, since none of the introducer keyword, the binder pattern, and the clause label records a span. A comment above the binding precedes all of these, so any of them bounds it; taking the earliest keeps comments written inside the signature with the component they lead.
///
/// A clause with no position of its own does not thereby keep its comment: the mark falls to the first *descendant* with a span, which is how a comment above an `and` clause once surfaced between a parameter and its type. It then reparsed as a leading comment somewhere new, so the next format run moved it again — the one way this formatter can fail to converge, and what `formatting_converges_from_every_comment_position` now checks.
fn signature_start(signature: &LetSignature) -> Option<usize> {
    let earliest = match signature {
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
        Subterm::Let(_)
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
        // A builtin row surfaces under its `/sys` subject (`Handle/write`); a user's `foreign` declaration under the name they gave it.
        Subterm::Foreign(function, args) => {
            let name = match &function.subject {
                Some(subject) => format!("{subject}/{}", function.label),
                None => function.label.clone(),
            };
            print_intrinsic_call(name, vec![], args)
        }
        Subterm::Name(name) => pure(name.join()),
        // Both spell `?`: the written/desugared distinction matters to zonk's reporting, not to how the term reads.
        Subterm::Hole | Subterm::Goal => pure("?"),
        // Never parsed: the witness lowering mints it, and a `satisfy` prints its `;` from the declaration, not from here.
        Subterm::Derive => pure("derive"),
        Subterm::Syn(Syn::Char(character)) => print_char_literal(character),
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
            attached_body(" =>", body),
        ]),
        Subterm::Apply(Apply { head, arguments }) => riding_call(head, arguments),
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
                        .map(|arm| {
                            // A choose arm's test is spanned and precedes its body, so it is the earliest component; the head is built *inside* the mark so that printing the test cannot report a later position first.
                            let start = member_start([
                                match &arm.test {
                                    ChooseTest::Cond(condition) => condition,
                                    ChooseTest::Bind { value, .. } => value,
                                },
                                &arm.body,
                            ]);
                            let ChooseArm { test, body } = arm;
                            flat([
                                // Reported before the break, so a comment riding the previous line is paid there — see `reached_before`.
                                reached_before(start),
                                separator(),
                                marked(start, || {
                                    let head = match test {
                                        ChooseTest::Cond(condition) => {
                                            flat([pure("| "), print_term(condition)])
                                        }
                                        ChooseTest::Bind { pattern, value } => flat([
                                            pure("| "),
                                            print_match_pattern(pattern),
                                            pure(" = "),
                                            print_term(value),
                                        ]),
                                    };
                                    flat([head, attached_body(" =>", body)])
                                }),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                separator(),
                marked(member_start([&default]), || {
                    flat([pure("| _"), attached_body(" =>", default)])
                }),
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
                            // The mark sits *after* the separator: it is what breaks the line, so a comment written on its own line before it leads this arm rather than riding the previous one. The position is reported *before* the break, so a comment riding the previous line is paid there — see `reached_before`.
                            let start = member_start([&arm.body]);
                            flat([
                                reached_before(start),
                                separator(),
                                marked(start, || {
                                    flat([
                                        pure("| "),
                                        print_match_pattern(arm.pattern),
                                        attached_body(" =>", arm.body),
                                    ])
                                }),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
                separator(),
                pure("end"),
            ]))
        }
        Subterm::Let(Let { groups, tail }) => {
            // The statement documents materialize before the tail's, so the marks come out in source order; an eagerly evaluated tail would report its own position ahead of every binding's. Every clause marks its own head, so a comment above a `let` stays above it instead of sliding under the `=` — and a clause after the first carries its `and` inside the mark, since a separator carrying the keyword would print it before the comment the clause leads.
            let statements = groups
                .into_iter()
                .map(|group| {
                    let count = group.members.len();
                    let mut parts = Vec::new();
                    for (index, member) in group.members.into_iter().enumerate() {
                        let start = signature_start(&member.signature);
                        let keyword = match index {
                            0 => "let ",
                            _ => {
                                parts.push(hard_line());
                                "and "
                            }
                        };
                        parts.push(marked(start, || {
                            let mut clause = vec![
                                pure(keyword),
                                print_pattern(member.binder),
                                print_let_signature(member.signature, false),
                            ];
                            // A lone binding keeps its terminator inside the mark; a group's terminator closes the last clause from outside it, as the top-level item's does.
                            if count == 1 {
                                clause.extend([pure(";"), hard_line()]);
                            }
                            flat(clause)
                        }));
                    }
                    if count > 1 {
                        parts.extend([pure(";"), hard_line()]);
                    }
                    flat(parts)
                })
                .collect::<Vec<_>>();
            flat(statements.into_iter().chain([print_term(tail)]))
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
            sign,
        }) => pure(format!(
            "{}{}",
            sign.symbol(),
            format_radix(&magnitude, radix)
        )),
    }
}

/// A character literal as written: the five escapes by their spellings, anything else verbatim — shared by the expression and match-pattern positions.
fn print_char_literal(character: char) -> Printer {
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

/// A `let` signature and body. `top` selects the corpus's top-level shape — the body *always* on the next line after `=` — while a local binding's body rides the `=` inline when it fits.
fn print_let_signature(signature: LetSignature, top: bool) -> Printer {
    let bound = |body: Term| {
        if top {
            flat([pure(" ="), hard_line(), indent(print_term(body))])
        } else {
            attached_body(" =", body)
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

fn print_top_test(test: TopTest) -> Printer {
    flat([
        pure("test "),
        pure(test.label),
        pure("() ="),
        hard_line(),
        indent(print_term(test.body)),
        pure(";"),
    ])
}

fn print_top_let(items: Vec<TopLet>) -> Printer {
    let mut iter = items.into_iter();
    let first = iter.next().expect("a `let` item has a member");
    let rest = iter.collect::<Vec<_>>();

    flat([
        print_pub(first.vis_pub),
        pure("let "),
        pure(first.label),
        print_let_signature(first.signature, true),
        flat(
            rest.into_iter()
                .map(|item| {
                    let start = signature_start(&item.signature);
                    // The separator stays outside the mark, so a comment leading this clause opens a line of its own above `and` rather than running on from the previous clause's last character.
                    flat([
                        hard_line(),
                        marked(start, || {
                            flat([
                                // `pub` precedes `and`, which is the spelling the grammar accepts and the one the `induct` group beside this already emits. Reversed, a `pub` member of a group printed as `and pub f` and would not reparse — the formatter's verify gate refused the file rather than writing it, which is why `/std/Toml/values.crs` had never been formatted.
                                print_pub(item.vis_pub),
                                pure("and "),
                                pure(item.label),
                                print_let_signature(item.signature, true),
                            ])
                        }),
                    ])
                })
                .collect::<Vec<_>>(),
        ),
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
    // Computed before anything below builds a document, since printing a payload type would otherwise report its own position first and place this case's leading comment inside the payload list.
    let start = member_start(
        case.payload
            .iter()
            .map(|param| &param.type_)
            .chain(case.target.iter().flatten()),
    );
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
        marked(start, || {
            flat([
                pure(format!("| {}", case.label)),
                listed("(", payload, ")"),
                target,
            ])
        }),
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

/// [`signature_start`] for an `induct` clause, over the components an inductive head has: its parameters, then its index telescope, then its result sort, then the first payload or target its cases carry.
///
/// The label is spanless like a `let`'s binder, so a clause with no position of its own would leave its leading comment to the first spanned descendant. The cases are the fallback because a *sort* is spanless too — `parse_type` and `parse_prop` build their term from a bare `Subterm` — so `and Odd : Type` has no located component in its head at all. Any offset within the clause bounds it equally well: only the clause's own text lies between its head and its first case, and a comment written in there is one this hoists above `and` rather than one it misplaces.
fn induct_start(item: &TopInduct) -> Option<usize> {
    let head = match (item.params.first(), item.indices.first()) {
        (Some((_, _, type_)), _) => Some(type_),
        (None, Some((_, index))) => Some(index),
        (None, None) => None,
    };
    let case_component = || {
        let case = item.cases.first()?;
        match case.payload.first() {
            Some(param) => Some(&param.type_),
            None => case.target.as_ref()?.first(),
        }
    };
    head.or(Some(&item.result_sort))
        .into_iter()
        .chain(case_component())
        .find_map(|term| term.span().map(|span| span.start))
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
                    let start = induct_start(&u);
                    flat([
                        hard_line(),
                        marked(start, || {
                            flat([
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
                        }),
                    ])
                })
                .collect::<Vec<_>>(),
        ),
        hard_line(),
        pure("end"),
    ])
}

/// A `struct` item: one structure, or a group joined by `and`, each later member marked at its head as a `let` group's clauses are.
fn print_top_struct(items: Vec<TopStruct>) -> Printer {
    let mut iter = items.into_iter();
    let first = iter.next().expect("a `struct` item has a member");
    let rest = iter
        .map(|item| {
            let start = struct_member_start(&item);
            flat([
                hard_line(),
                marked(start, || print_struct_member(item, "and ")),
            ])
        })
        .collect::<Vec<_>>();
    flat([print_struct_member(first, "struct "), flat(rest)])
}

/// Where a struct member begins: its earliest spanned component — a parameter type, the result sort, or the first field type.
fn struct_member_start(item: &TopStruct) -> Option<usize> {
    [
        item.params
            .first()
            .and_then(|(_, _, type_)| type_.span().map(|span| span.start)),
        item.result_sort.span().map(|span| span.start),
        item.fields
            .first()
            .and_then(|field| field.type_.span().map(|span| span.start)),
    ]
    .into_iter()
    .flatten()
    .min()
}

fn print_struct_member(item: TopStruct, keyword: &'static str) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure(keyword),
        pure(item.label),
        print_top_induct_params(item.params),
        pure(": "),
        print_pub(item.rep_pub),
        print_term(item.result_sort),
        pure(" "),
        listed_hard(
            "{",
            item.fields
                .first()
                .and_then(|field| field.type_.span())
                .map(|span| span.start),
            item.fields.into_iter().map(print_field).collect(),
            "}",
        ),
    ])
}

fn print_concept_field(field: ConceptField) -> Printer {
    // Marked before the branch, because every branch prints a field and a comment above one leads the field however it is spelled. A superclass field used to return before marking, so its comment fell through to the type term and surfaced *inside* the field, between `use` and the type it leads.
    let start = member_start([&field.type_]);

    // A superclass field is anonymous: `use <type>`, no label.
    if field.is_super {
        return marked(start, || flat([pure("use "), print_term(field.type_)]));
    }
    match field.func_params {
        Some(params) => marked(start, || {
            flat([
                pure(field.label),
                listed(
                    "(",
                    params.into_iter().map(print_func_type_param).collect(),
                    ")",
                ),
                pure(" -> "),
                print_term(field.type_),
            ])
        }),
        None => marked(start, || {
            flat([pure(field.label), pure(": "), print_term(field.type_)])
        }),
    }
}

/// A `concept` item: one concept, or a group joined by `and`, printed as a `struct` group is.
fn print_top_concept(items: Vec<TopConcept>) -> Printer {
    let mut iter = items.into_iter();
    let first = iter.next().expect("a `concept` item has a member");
    let rest = iter
        .map(|item| {
            let start = concept_member_start(&item);
            flat([
                hard_line(),
                marked(start, || print_concept_member(item, "and ")),
            ])
        })
        .collect::<Vec<_>>();
    flat([print_concept_member(first, "concept "), flat(rest)])
}

fn concept_member_start(item: &TopConcept) -> Option<usize> {
    [
        item.params
            .first()
            .and_then(|(_, _, type_)| type_.span().map(|span| span.start)),
        item.result_sort.span().map(|span| span.start),
        item.fields
            .first()
            .and_then(|field| field.type_.span().map(|span| span.start)),
    ]
    .into_iter()
    .flatten()
    .min()
}

fn print_concept_member(item: TopConcept, keyword: &'static str) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure(keyword),
        pure(item.label),
        print_top_induct_params(item.params),
        pure(": "),
        print_pub(item.rep_pub),
        print_term(item.result_sort),
        pure(" "),
        listed_hard(
            "{",
            item.fields
                .first()
                .and_then(|field| field.type_.span())
                .map(|span| span.start),
            item.fields.into_iter().map(print_concept_field).collect(),
            "}",
        ),
    ])
}

fn print_top_witness(items: Vec<TopWitness>) -> Printer {
    let mut iter = items.into_iter();
    let first = iter.next().expect("a `satisfy` item has a member");
    let rest = iter
        .map(|item| {
            let start = witness_member_start(&item);
            // As for a `let` group: the separator stays outside the mark, so a comment leading this member opens a line of its own above `and`.
            flat([
                hard_line(),
                marked(start, || print_witness_member(item, "and")),
            ])
        })
        .collect::<Vec<_>>();

    flat([print_witness_member(first, "satisfy"), flat(rest)])
}

fn print_witness_member(item: TopWitness, keyword: &'static str) -> Printer {
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

    // A derived witness ends at the `;` in the brace block's place: the declaration is the whole of what was written.
    let body = match item.body {
        Some(entries) => flat([
            pure(" "),
            listed_hard(
                "{",
                entries.first().and_then(witness_entry_start),
                entries.into_iter().map(print_witness_entry).collect(),
                "}",
            ),
        ]),
        None => pure(";"),
    };

    flat([pure(keyword), params, pure(" "), app, body])
}

/// Where a witness-body entry begins, for the mark that pays a comment riding the opening brace's line.
/// Where an `and` witness clause begins: its earliest spanned component — a telescope parameter's type, a concept argument, or the first entry — since the keyword, the concept name and the braces record no span. [`signature_start`]'s rule for a `let` clause.
fn witness_member_start(item: &TopWitness) -> Option<usize> {
    [
        item.params
            .first()
            .and_then(|param| param.type_.span().map(|span| span.start)),
        item.args
            .first()
            .and_then(|arg| arg.span().map(|span| span.start)),
        item.body
            .as_ref()
            .and_then(|entries| entries.first())
            .and_then(witness_entry_start),
    ]
    .into_iter()
    .flatten()
    .min()
}

fn witness_entry_start(entry: &WitnessEntry) -> Option<usize> {
    let term = match entry {
        WitnessEntry::Use(term) => term,
        WitnessEntry::Field(field) => &field.value,
    };

    term.span().map(|span| span.start)
}

/// A witness-body entry: a `use <term>` fill or an implementation field — `label = value`, or the definition sugar `label(params) = value` re-sugared from the retained parameter list.
fn print_witness_entry(entry: WitnessEntry) -> Printer {
    let field = match entry {
        WitnessEntry::Use(term) => return flat([pure("use "), print_term(term)]),
        WitnessEntry::Field(field) => field,
    };

    let start = member_start([&field.value]);
    match field.func_params {
        Some(params) => marked(start, || {
            flat([
                pure(field.label),
                listed("(", params.into_iter().map(print_func_param).collect(), ")"),
                attached_body(" =", field.value),
            ])
        }),
        None => marked(start, || {
            flat([pure(field.label), attached_body(" =", field.value)])
        }),
    }
}

pub(crate) fn print_top_item(item: TopItem) -> Printer {
    match item {
        TopItem::Mod(m) => print_top_mod(m),
        TopItem::Use(u) => print_top_use(u),
        TopItem::Let(l) => print_top_let(l),
        TopItem::Induct(group) => print_top_induct(group),
        TopItem::Struct(s) => print_top_struct(s),
        TopItem::Concept(c) => print_top_concept(c),
        TopItem::Witness(w) => print_top_witness(w),
        TopItem::Foreign(f) => print_top_foreign(f),
        TopItem::Test(t) => print_top_test(t),
    }
}
