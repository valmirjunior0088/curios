#[cfg(test)]
mod test_support;

#[cfg(test)]
mod body_layout_tests;
#[cfg(test)]
mod call_layout_tests;
#[cfg(test)]
mod list_layout_tests;

use {
    super::{
        Apply, Argument, BytesPattern, BytesSegment, CasePayloadParam, Choose, ChooseArm,
        ChooseTest, ConceptField, CoreEntry, CoreForm, CoreHead, CoreIntrinsic, CoreItem,
        CoreMember, CoreText, CoreValue, Decorated, DecoratorField, Dialect, Doc, Field,
        FieldSugarParam, Func, FuncParam, FuncSugarParam, FuncType, FuncTypeParam, GroupItem,
        Infix, Intrinsic, IntrinsicForm, Label, Let, LetSignature, ListEntry, ListPattern, Literal,
        Match, MatchPattern, MatchPatternField, Name, NatLiteral, NatPattern, NumLit, Pattern,
        PatternField, Program, Proj, ProofLiteral, Radix, Shown, ShownForm, StructField, StructLit,
        StructLitEntry, Subterm, Term, TopCase, TopConcept, TopForeign, TopInduct, TopItem, TopLet,
        TopMod, TopStruct, TopTest, TopUse, TopWitness, Tuple, TupleField, TupleType,
        TupleTypeParam, UseGroup, WitnessField, intrinsic_form, intrinsic_path,
    },
    crate::{ELIDED, Writer, op_precedence, written_intrinsic},
    curios_abi::{ResultShape, WireResults, WireReturn, WireSignature, WireType},
    curios_common::{Plicity, recurse},
    curios_num::{Floating, Natural},
    curios_parse::unseen,
    curios_print::{
        FillItem, Printer, ends, fill, flat, flow, group, hard_line, if_break, indent, line, named,
        named_at, own_line_if_broken, pure, reached, sep_flat, soft_line,
    },
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
/// The closer rides because the last element already ends the sequence; following it with `,` and a bracket on a line of its own spends two lines on punctuation. Calls, tuple literals and types, and variant payloads are all this shape — what they delimit is a run of expressions, and none of them cares what kind the last one is. [`listed_block`] is this shape padded, for the brace forms that read as records.
fn listed(open: impl Into<String>, items: Vec<Printer>, close: &'static str) -> Printer {
    let open = pure(open);
    if items.is_empty() {
        return flat([open, indent(ends()), pure(close)]);
    }
    group(flat([
        open,
        indent(flat([
            soft_line(),
            sep_flat(items, || flat([pure(","), line()])),
            ends(),
        ])),
        pure(close),
    ]))
}

/// A bracketed **record**: [`listed`] with its braces padded — `{ a, b }` flat, and broken one field per line with the closer riding the last after a space (`c }`). Struct literals and patterns take this shape.
///
/// The padding is what sets a record's braces apart from a tuple type's, which [`listed`] prints bare (`{a: Nat}`), and the riding closer keeps it for the same reason.
fn listed_block(open: impl Into<String>, items: Vec<Printer>, close: &'static str) -> Printer {
    let open = pure(open);
    if items.is_empty() {
        return flat([open, indent(ends()), pure(close)]);
    }
    group(flat([
        open,
        indent(flat([
            line(),
            sep_flat(items, || flat([pure(","), line()])),
            ends(),
        ])),
        pure(" "),
        pure(close),
    ]))
}

/// A parenthesized **binder telescope** — a `let`, `satisfy`, `struct`, `concept` or `induct` parameter list: flat with no padding (`(a: Nat, b: Nat)`), and broken with a trailing comma and the closer on a line of its own.
///
/// A closer that does not ride, where [`listed`]'s does, for two reasons. A telescope is a thing that *grows*: with the closer riding, adding a parameter is a two-line change, because the line above has to give the bracket up and take a comma. And where the declaration that follows opens a brace body — every one of these but `let` — a riding closer leaves the last parameter and the first field at one indent, with only the `)` buried mid-line to say which is which.
///
/// No enclosing `group`, because the extent of the group is the caller's: a `let` measures the telescope together with its `-> output`, and a `satisfy` with the application it concludes, so the parameters break first and what follows them gets a fresh line, while the others measure the parentheses alone.
fn telescope(items: Vec<Printer>) -> Printer {
    flat([
        pure("("),
        indent(flat([
            soft_line(),
            sep_flat(items, || flat([pure(","), line()])),
            if_break("", ","),
            ends(),
        ])),
        soft_line(),
        pure(")"),
    ])
}

/// The wrapping counterpart to [`listed`], for a bracketed run of short interchangeable atoms: the items begin on the opening delimiter's own line and wrap across as many lines as they need, each continuation at the next indent.
///
/// [`listed`] is the right shape for a structure — when one part needs its own line they all take one. A run of names is not a structure, and giving an import that treatment spends a line per name.
///
/// No enclosing `group`, and that absence *is* the layout. A group asks whether the whole run fits on one line, which is the wrong question to put to content that would rather wrap: answering no would break a `soft_line` after the delimiter and spend a line on nothing but the delimiter itself. A fill needs no such permission, deciding each gap for itself, so the first name rides the delimiter and only the wraps are indented. Items arrive as plain strings and their commas are attached here, so the fill inserts only the gap between them.
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
    flat([
        pure(open),
        indent(flat([fill(entries), ends()])),
        pure(close),
    ])
}

/// Whether a term prints as one indivisible piece — a name, a numeral, a character, a one-line string, a projection off one, or a form its dialect says is one — so that a run of them has no break of its own.
fn is_atom<D: Layout>(term: &Term<D>) -> bool {
    match term.as_subterm() {
        Subterm::Name(_)
        | Subterm::NumLit(_)
        | Subterm::ProofLiteral(ProofLiteral::Char(_))
        | Subterm::Literal(Literal::Flt(_)) => true,
        Subterm::ProofLiteral(ProofLiteral::Str(literal)) => !literal.block,
        Subterm::Proj(Proj { head, .. }) => is_atom(head),
        Subterm::Core(form) => D::form_is_atom(form),
        _ => false,
    }
}

/// Whether a term carries a break of its own that a call can hand its layout to — a list, a packed literal or a tuple of two or more fields, which break inside the brackets, or a lambda, whose body rides its arrow and breaks below it: see [`TreePrinter::riding_call`].
fn carries_its_own_break<D: Layout>(term: &Term<D>) -> bool {
    match term.as_subterm() {
        Subterm::Literal(Literal::List(entries)) => !entries.is_empty(),
        Subterm::Literal(Literal::Bytes(segments)) => !segments.is_empty(),
        Subterm::Tuple(Tuple { fields }) => fields.len() >= 2,
        Subterm::Func(_) => true,
        _ => false,
    }
}

/// Whether an argument list hands its break to its last argument: one that carries a break of its own, after nothing but atoms.
fn hugs<D: Layout>(arguments: &[Argument<D>]) -> bool {
    arguments.split_last().is_some_and(|(last, leading)| {
        carries_its_own_break(&last.term) && leading.iter().all(|argument| is_atom(&argument.term))
    })
}

/// What a chain of applications is applied to: the head under every argument list.
fn chain_base<D: Layout>(term: &Term<D>) -> &Term<D> {
    match term.as_subterm() {
        Subterm::Apply(Apply { head, .. }) => chain_base(head),
        _ => term,
    }
}

/// An argument list with no break of its own: its arguments joined by `, `, any break left to what they carry.
fn unbroken(items: Vec<Printer>) -> Printer {
    flat([
        pure("("),
        sep_flat(items, || pure(", ")),
        indent(ends()),
        pure(")"),
    ])
}

/// An argument list's parentheses with a break after the opener and between its arguments, one level in, and the closer riding the last: flat or broken as the group around it is.
fn argument_lines(items: Vec<Printer>) -> Printer {
    flat([
        pure("("),
        indent(flat([
            soft_line(),
            sep_flat(items, || flat([pure(","), line()])),
            ends(),
        ])),
        pure(")"),
    ])
}

/// A block string literal, printed as the lines of its value one level in from the opener's line, with the closer at that level. Each line prints as itself: the leading whitespace lines share puts them right of the closer, which is exactly what a reader strips, so nothing is removed here — while trailing whitespace, tabs, carriage returns, backslashes and every third consecutive quote are escaped, since the reader would strip, mistake or close on them. A scalar prints as itself, as in the one-line form, but for any other control character, which prints as its braced escape.
fn print_block_string(value: &str) -> Printer {
    let mut body = Vec::new();
    for line in value.split('\n') {
        body.push(hard_line());
        body.push(pure(spell_block_line(line)));
    }
    body.push(hard_line());
    body.push(pure("\"\"\""));
    flat([pure("\"\"\""), indent(flat(body))])
}

/// One line of a block string literal's value, spelled so that it reads back as itself.
fn spell_block_line(line: &str) -> String {
    let kept = line.trim_end_matches(' ').len();
    let mut spelled = String::with_capacity(line.len());
    let mut quotes = 0;
    for (offset, character) in line.char_indices() {
        match character {
            '"' if quotes == 2 => {
                spelled.push_str("\\\"");
                quotes = 0;
                continue;
            }
            '"' => quotes += 1,
            '\\' => spelled.push_str("\\\\"),
            '\t' => spelled.push_str("\\t"),
            '\r' => spelled.push_str("\\r"),
            ' ' if offset >= kept => spelled.push_str("\\u{20}"),
            hidden if unseen(hidden) => spelled.push_str(&braced_escape(hidden)),
            _ => spelled.push(character),
        }
        if character == '"' {
            spelled.push('"');
        } else {
            quotes = 0;
        }
    }
    spelled
}

/// The always-broken brace block a `struct`, `concept`, or `satisfy` body is: one field per line at the next indent with a trailing comma, the closing brace at the opening's column, regardless of how little would fit flat. An empty body prints the bare delimiters.
fn listed_hard(open: &'static str, items: Vec<Printer>, close: &'static str) -> Printer {
    if items.is_empty() {
        return flat([pure(open), indent(ends()), pure(close)]);
    }
    flat([
        pure(open),
        indent(flat([
            hard_line(),
            sep_flat(items, || flat([pure(","), hard_line()])),
            pure(","),
            ends(),
        ])),
        hard_line(),
        pure(close),
    ])
}

/// The most arms a ladder may keep on one line. Beyond this it is a dispatch table rather than a decision, and a table reads as rows.
const MAX_FLAT_ARMS: usize = 3;

/// Whether a term is itself an arm ladder — the arm body that forces its enclosing ladder to break.
///
/// Nested flat, two ladders put both their `end`s on one line and nothing says which closes which: `match b | true => match c | true => false | false => true end | false => c end`. Breaking the outer gives every `end` a column that names what it closes. This is about pairing delimiters by eye, not about density — which is why it applies at any width.
fn is_ladder<D: Layout>(body: &Term<D>) -> bool {
    matches!(body.as_subterm(), Subterm::Match(_) | Subterm::Choose(_))
}

/// A `Flt` literal as written: a finite value as its signed decimal, and the values no decimal spells as their literals, `+inf.0`, `-inf.0`, `+nan.0` and `-nan.0`. Source spells no other NaN, so the syntax holds none.
fn print_flt(value: Floating) -> Printer {
    if !value.is_finite() {
        let sign = match value.abs().to_bits() == value.to_bits() {
            true => "+",
            false => "-",
        };
        let word = match value.is_nan() {
            true => "nan",
            false => "inf",
        };
        return pure(format!("{sign}{word}.0"));
    }

    // `Display` for `f64` never uses exponent notation (that is `{:e}`), so decimalizing is one suffix check.
    let mut string = f64::from(value).to_string();

    if !string.contains('.') {
        string.push_str(".0");
    }

    if !string.starts_with('-') {
        string.insert(0, '+');
    }

    pure(string)
}

/// The optional `; ih` tail of a `Nat` fold's succ arm or a `List`/`Bytes` fold's cons arm — any irrefutable pattern, printed through `print_pattern`; `None` prints nothing at all (a plain case-split), matching how it was written.
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
        Pattern::Binder(name) => pure(name),
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
            pure(head.join()),
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

/// A match-arm pattern: a plain binder, an inductive constructor tag applied to sub-patterns, a tuple pattern, or a struct pattern — the refutable counterpart of `print_pattern` (see `MatchPattern`'s doc comment). `Variant` stays positional (constructors have no field labels); `Tuple`/`Struct` mirror `print_pattern`'s own field-printing exactly.
pub fn print_match_pattern(pattern: MatchPattern) -> Printer {
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
            pure(head.join()),
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
        MatchPattern::Nat(NatPattern::Lit(NatLiteral(n, radix))) => pure(format_radix(&n, radix)),
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
        MatchPattern::Bytes(BytesPattern::End) => pure("x[]"),
        MatchPattern::Bytes(BytesPattern::Atom {
            head_label,
            tail_label,
            ih,
        }) => flat([
            pure("x["),
            pure(head_label),
            pure(", .."),
            pure(tail_label),
            pure("]"),
            print_cons_ih(ih),
        ]),
    }
}

// Zero-padded to the width the literal was written at. A `digits` of `0` — a numeral the compiler minted — asks for no minimum, which is the natural width.
fn format_radix(n: &Natural, radix: Radix) -> String {
    match radix {
        Radix::Dec(digits) => format!("{n:0digits$}"),
        Radix::Hex(digits) => format!("0x{n:0digits$X}"),
        Radix::Bin(digits) => format!("0b{n:0digits$b}"),
    }
}

/// `/sys`'s own trees as they stand: each operand as it is, and each declaration of `/sys` a form names by the path a reader writes it by ([`intrinsic_path`]) — `/sys/Bool`, a carrier by the name `/sys` hoists it to. Absolute, because `/sys` imports nothing, so it is the name a page resolves to link it and spells below `/sys` to show it.
struct Quoted;

impl<D: Dialect> Writer<Term<D>, D> for Quoted {
    fn operand(&self, operand: Term<D>) -> Option<Term<D>> {
        Some(operand)
    }

    fn sys(&self, path: &str) -> Option<Term<D>> {
        Some(Subterm::Name(Name::new(true, intrinsic_path(path))).into())
    }
}

/// The documentation comment above what `doc` documents: every line printed as written, each on a line of its own.
fn print_doc(doc: Option<Doc>) -> Printer {
    let Some(doc) = doc else {
        return pure("");
    };

    flat(doc.lines.into_iter().map(|line| {
        let spelled = match line.is_empty() {
            true => "---".to_string(),
            false => format!("--- {line}"),
        };
        flat([pure(spelled), hard_line()])
    }))
}

/// A character literal as written: the five escapes by their spellings, any other character no reader sees by its braced escape, anything else verbatim — shared by the expression and match-pattern positions.
fn print_char_literal(character: char) -> Printer {
    let escaped = match character {
        '\'' => "\\'".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        hidden if unseen(hidden) => braced_escape(hidden),
        _ => character.to_string(),
    };
    pure(format!("'{escaped}'"))
}

/// A scalar as `\u{…}`, for a character a literal cannot show — what [`unseen`] reads as printing as nothing, a control, a format character such as a zero-width joiner or a direction override, white space other than a blank: written verbatim it is a byte no reader sees, and the escape is the only spelling of it that survives a reading.
fn braced_escape(character: char) -> String {
    format!("\\u{{{:X}}}", u32::from(character))
}

fn print_pub(vis_pub: bool) -> Printer {
    if vis_pub { pure("pub ") } else { pure("") }
}

fn print_group_item(item: &GroupItem) -> String {
    match item {
        GroupItem::Mod(s) => format!("mod {s}"),
        GroupItem::Let(s) => format!("let {s}"),
        GroupItem::Both(s) => s.to_string(),
    }
}

fn print_wire_type(type_: WireType) -> Printer {
    pure(type_.to_string())
}

// A result as the surface spells it, which is the shape `WireResults` already decides: nothing is `{}`, one is the bare type forwarded through, and several are the tuple type the guest projects by — each field labelled, because a tuple's labels are part of its identity and dropping one would print a different type from the one declared.
fn print_wire_results(results: &WireResults) -> Printer {
    match results.shape() {
        ResultShape::Unit => pure("{}"),
        ResultShape::Single(type_) => print_wire_type(type_),
        ResultShape::Record(fields) => listed(
            "{",
            fields
                .into_iter()
                .map(|(label, type_)| {
                    flat([pure(label.to_string()), pure(": "), print_wire_type(type_)])
                })
                .collect(),
            "}",
        ),
    }
}

/// A row's signature as its declaration writes it after the name: the operand list, empty or not, and what the call answers.
pub fn print_wire_signature(signature: &WireSignature) -> Printer {
    flat([
        listed(
            "(",
            signature
                .params
                .iter()
                .copied()
                .map(print_wire_type)
                .collect(),
            ")",
        ),
        pure(" -> "),
        match &signature.returns {
            WireReturn::Answers(results) => print_wire_results(results),
            WireReturn::Never => pure("Never"),
        },
    ])
}

/// What a dialect's printer lays out beyond the forms both dialects write: a hook for each production its grammar has of its own ([`Grammar`](crate::Grammar)) — an operand, a case's decorator, a declaration's head — and none of them in a program, whose tree holds no form of Core text.
trait Layout: Dialect {
    /// What a render of this dialect is told beside the tree it prints.
    type Options: Copy;

    /// An operand only this dialect writes, followed by `suffix`, the call chain and the marks after it.
    fn form(printer: TreePrinter<Self>, form: Self::Form, suffix: Printer) -> Printer;

    /// Whether `form` prints as one indivisible piece, as a name does.
    fn form_is_atom(form: &Self::Form) -> bool;

    /// Whether a call chain may follow `form` as it stands, where the parentheses that close it bind no tighter than the chain.
    fn takes_suffix(form: &Self::Form) -> bool;

    /// A case, its label, payload and targets after its bar, under the decorator before it.
    fn cased(printer: TreePrinter<Self>, fields: Self::Fields, case: Printer) -> Printer;

    /// A declaration under the decorator its head states, before its declaring word or the `and` of a group's later member.
    fn headed(printer: TreePrinter<Self>, head: &Self::Head, member: Printer) -> Printer;

    /// The name a declaration declares, after its declaring word.
    fn declared(head: &Self::Head, label: Label) -> Printer;

    /// The sort a nominal declaration's head lands in.
    fn result_sort(printer: TreePrinter<Self>, sort: Term<Self>) -> Printer;
}

impl Layout for Program {
    type Options = ();

    fn form(_: TreePrinter<Self>, form: Self::Form, _: Printer) -> Printer {
        match form {}
    }

    fn form_is_atom(form: &Self::Form) -> bool {
        match *form {}
    }

    fn takes_suffix(form: &Self::Form) -> bool {
        match *form {}
    }

    fn cased(_: TreePrinter<Self>, (): (), case: Printer) -> Printer {
        case
    }

    fn headed(_: TreePrinter<Self>, _: &(), member: Printer) -> Printer {
        member
    }

    fn declared(_: &(), label: Label) -> Printer {
        pure(label.to_string())
    }

    fn result_sort(printer: TreePrinter<Self>, sort: Term) -> Printer {
        printer.term(sort)
    }
}

/// The printer of a tree of the dialect `D`, the one every text of the surface grammar is laid out by: a program's, which the formatter writes a file in and a report's term is translated to, Core text's, and a shown term's. It holds what a render of `D` is told beside the tree, and learns nothing while printing.
struct TreePrinter<D: Layout> {
    options: D::Options,
}

impl<D: Layout> Clone for TreePrinter<D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<D: Layout> Copy for TreePrinter<D> {}

/// The printer of a program.
const PROGRAM: TreePrinter<Program> = TreePrinter { options: () };

impl<D: Layout> TreePrinter<D> {
    /// Simple entries pack together; spreads and structured expressions stand apart in a broken layout. Packed literals always retain their outer group as a break opportunity for enclosing expressions; a plain list of atoms packs directly from its opener.
    fn wrapped_list(self, open: &'static str, entries: Vec<ListEntry<D>>) -> Printer {
        if entries.is_empty() {
            return flat([pure(open), indent(ends()), pure("]")]);
        }
        let last = entries.len() - 1;
        let items = entries
            .into_iter()
            .enumerate()
            .map(|(index, entry)| {
                let (printer, compact) = match entry {
                    ListEntry::Elem(term) => {
                        let compact = is_atom(&term)
                            || match term.as_subterm() {
                                Subterm::Literal(Literal::List(entries)) => entries.is_empty(),
                                Subterm::Literal(Literal::Bytes(segments)) => segments.is_empty(),
                                Subterm::Tuple(Tuple { fields }) => fields.is_empty(),
                                _ => false,
                            };
                        (self.term(term), compact)
                    }
                    ListEntry::Spread(term) => (flat([pure(".."), self.term(term)]), false),
                };
                FillItem {
                    printer: if index == last {
                        printer
                    } else {
                        flat([printer, pure(",")])
                    },
                    compact,
                }
            })
            .collect::<Vec<_>>();
        let structured = items.iter().any(|item| !item.compact);
        let grouped = open != "[" || structured;
        let body = flat([
            pure(open),
            indent(flat([
                if grouped { soft_line() } else { flat([]) },
                if open == "[" || structured {
                    flow(items)
                } else {
                    fill(items.into_iter().map(|item| item.printer))
                },
                ends(),
            ])),
            pure("]"),
        ]);
        if grouped { group(body) } else { body }
    }

    /// The one shape every call takes: flat when it fits, otherwise the head on its own line and each argument on its own, with the closing bracket *riding* the last one — except that a call whose last argument carries a break of its own, with nothing but atoms before it, hands its break to that argument: `Str/flatten([` on the head's line, the elements one level in, and `])` riding the last; `List/map(names, (name) =>` on the head's line and the body one level in below it.
    ///
    /// The hug is bounded by what the leading arguments can do. An unbounded trailing-lambda hug would make the layout depend on the *kind* of the final argument, and a leading argument that then broke — a lambda too wide for the line, a `let` in a nested call — would strand the block after its own closing bracket. Atoms cannot break, so a hugged argument's opener is always on the head's line, and that argument's own group is the only decision the call makes; a call led by anything else keeps the one shape.
    ///
    /// It is `listed` minus two things: no trailing comma, and no break before the closer. The last argument already ends the call, and saying so with `,` and a lone `)` spends two lines on punctuation.
    ///
    /// A head under two or more argument lists over an atom is [`TreePrinter::curried_call`]'s: the lists are one call's arguments written apart, and decided apart they leave `)(` mid-line with an argument hanging below it.
    fn riding_call(self, head: Term<D>, arguments: Vec<Argument<D>>, suffix: Printer) -> Printer {
        if matches!(head.as_subterm(), Subterm::Apply(_)) && is_atom(chain_base(&head)) {
            return self.curried_call(head, arguments, suffix);
        }
        self.suffix_head(head, flat([self.argument_list(arguments), suffix]))
    }

    /// One argument list as [`TreePrinter::riding_call`] writes it: hugging its last argument where [`hugs`] says so, and otherwise flat when it fits, its arguments one per line with the closer riding the last when it does not.
    fn argument_list(self, arguments: Vec<Argument<D>>) -> Printer {
        let hugged = hugs(&arguments);
        let items = arguments
            .into_iter()
            .map(|argument| self.argument(argument))
            .collect::<Vec<_>>();
        match hugged {
            true => unbroken(items),
            false => group(argument_lines(items)),
        }
    }

    /// A curried call, `h(a)(b)(c)` over an atom `h`, laid out as the call its arguments would make written in one list, `)(` standing where a comma would, but for the first list, which hugs the head. Flat when the chain fits; otherwise `h(a)(` stands on the head's line, the first list a group of its own that breaks inside only where it cannot fit there, and each later list's arguments go one level in, one per line, every closer riding its list's last argument and the next opener riding that closer, whatever follows the chain riding the last. The chain hugs its last argument where that call would: where the argument carries a break of its own and every argument before it, in every list, is an atom, all of its lists then written on the head's line. Each inner application leaves its source marks where the nested printing would, its start before the head and its end after its own list, so a comment meets the same token boundaries.
    fn curried_call(self, head: Term<D>, arguments: Vec<Argument<D>>, suffix: Printer) -> Printer {
        // Innermost list first once reversed; the outermost list's end is already in `suffix`, which the caller marked.
        let mut lists = vec![(arguments, None)];
        let mut starts = Vec::new();
        let mut current = head;
        while matches!(current.as_subterm(), Subterm::Apply(_)) {
            let span = current.span().map(|span| (span.start, span.end));
            let Subterm::Apply(Apply { head, arguments }) = current.into_subterm() else {
                unreachable!("matched as an application")
            };
            starts.extend(span.map(|(start, _)| start));
            lists.push((arguments, span.map(|(_, end)| end)));
            current = head;
        }
        lists.reverse();
        let marked = |end: Option<usize>| end.map_or_else(|| flat([]), reached);
        let hugged = lists.split_last().is_some_and(|((last, _), earlier)| {
            hugs(last)
                && earlier
                    .iter()
                    .all(|(arguments, _)| arguments.iter().all(|argument| is_atom(&argument.term)))
        });
        if hugged {
            let written = lists
                .into_iter()
                .map(|(arguments, end)| {
                    let items = arguments
                        .into_iter()
                        .map(|argument| self.argument(argument))
                        .collect();
                    flat([unbroken(items), marked(end)])
                })
                .collect::<Vec<_>>();
            return flat([
                flat(starts.into_iter().map(reached)),
                self.suffix_head(current, flat([flat(written), suffix])),
            ]);
        }
        let mut lists = lists.into_iter();
        let (first, first_end) = lists.next().expect("a chain has a first list");
        // Ungrouped, so every break of a later list is the chain's own.
        let later = lists
            .map(|(arguments, end)| {
                let items = arguments
                    .into_iter()
                    .map(|argument| self.argument(argument))
                    .collect();
                flat([argument_lines(items), marked(end)])
            })
            .collect::<Vec<_>>();
        group(flat([
            flat(starts.into_iter().map(reached)),
            self.suffix_head(
                current,
                flat([self.argument_list(first), marked(first_end)]),
            ),
            flat(later),
            suffix,
        ]))
    }

    /// A body that rides its introducer — ` => body`, ` = body` — inline when it fits, on the next line one level deeper when it does not.
    fn attached_body(self, introducer: &'static str, body: Term<D>) -> Printer {
        group(flat([
            pure(introducer),
            indent(flat([line(), self.term(body)])),
        ]))
    }

    /// Prints a match's optional motive — ` : ` and the written term (ordinarily a lambda, `(k, v) => P`) — or nothing at all when the motive was omitted in the source.
    fn motive(self, motive: Option<Term<D>>) -> Printer {
        match motive {
            Some(motive) => flat([pure(": "), self.term(motive)]),
            None => pure(""),
        }
    }

    /// One argument of an application, under the mark it was written with.
    fn argument(self, argument: Argument<D>) -> Printer {
        flat([print_plicity(argument.plicity), self.term(argument.term)])
    }

    /// One Π-binder, as in a function type: `@?label : type` (the label optional).
    fn func_type_param(self, param: FuncTypeParam<D>) -> Printer {
        let typed = self.term(param.type_);
        let body = match param.label {
            Some(label) => flat([pure(label), pure(": "), typed]),
            None => typed,
        };
        flat([print_plicity(param.plicity), body])
    }

    /// One member of a definition telescope (`let`, `satisfy`): its mark, then the binder with its type, or a premise's type alone.
    fn func_sugar_param(self, param: FuncSugarParam<D>) -> Printer {
        let plicity = param.plicity();
        let member = match param {
            FuncSugarParam::Bound { binder, type_, .. } => {
                flat([print_pattern(binder), pure(": "), self.term(type_)])
            }
            FuncSugarParam::Premise(type_) => self.term(type_),
        };
        flat([print_plicity(plicity), member])
    }

    /// One lambda parameter: the binder name with its optional domain annotation.
    fn func_param(self, (plicity, name, annotation): FieldSugarParam<D>) -> Printer {
        let bound = match annotation {
            Some(ty) => flat([pure(name), pure(": "), self.term(ty)]),
            None => pure(name),
        };
        flat([print_plicity(plicity), bound])
    }

    /// A tuple-literal / struct-literal field: positional, `label = value`, or the definition sugar `label(params) = value` re-sugared from the retained parameter list.
    fn tuple_field(self, field: TupleField<D>) -> Printer {
        match (field.label, field.func_params) {
            (Some(label), Some(params)) => flat([
                pure(label),
                listed(
                    "(",
                    params
                        .into_iter()
                        .map(|param| self.func_param(param))
                        .collect(),
                    ")",
                ),
                self.attached_body(" =", field.value),
            ]),
            (Some(label), None) => flat([pure(label), self.attached_body(" =", field.value)]),
            (None, _) => self.term(field.value),
        }
    }

    /// A struct-literal entry: a `..base` spread, a `use <term>` fill, or a field, plain or under `@`.
    fn struct_entry(self, entry: StructLitEntry<D>) -> Printer {
        match entry {
            StructLitEntry::Field(field) => self.tuple_field(field),
            StructLitEntry::Implicit(field) => flat([pure("@"), self.tuple_field(field)]),
            StructLitEntry::Use(term) => flat([pure("use "), self.term(term)]),
            StructLitEntry::Spread(term) => flat([pure(".."), self.term(term)]),
        }
    }

    /// One lambda parameter: its plicity mark (`@`/`use`), the binder pattern, and its optional domain annotation — the pattern-accepting counterpart of [`TreePrinter::func_param`], forked for the same reason `parse_func_pattern_param` is (see `parse.rs`). A lambda's `use` member is `use _`, the wildcard holding its place.
    fn func_pattern_param(self, param: FuncParam<D>) -> Printer {
        let FuncParam {
            plicity,
            pattern,
            annotation,
        } = param;
        let bound = match annotation {
            Some(ty) => flat([print_pattern(pattern), pure(": "), self.term(ty)]),
            None => print_pattern(pattern),
        };
        flat([print_plicity(plicity), bound])
    }

    fn labeled(self, (label, ty): (Option<String>, Term<D>)) -> Printer {
        match label {
            Some(label) => flat([pure(label), pure(": "), self.term(ty)]),
            None => self.term(ty),
        }
    }

    /// A Σ-type / struct field: positional, `label : type`, or the signature sugar `label(params) -> type` re-sugared from the retained parameter list.
    fn field(self, param: TupleTypeParam<D>) -> Printer {
        match (param.label, param.func_params) {
            (Some(label), Some(params)) => flat([
                pure(label.to_string()),
                listed(
                    "(",
                    params
                        .into_iter()
                        .map(|param| self.func_type_param(param))
                        .collect(),
                    ")",
                ),
                pure(" -> "),
                self.term(param.type_),
            ]),
            (Some(label), None) => {
                flat([pure(label.to_string()), pure(": "), self.term(param.type_)])
            }
            (None, _) => self.term(param.type_),
        }
    }

    /// A written literal, as written.
    fn literal(self, literal: Literal<D>) -> Printer {
        match literal {
            Literal::Bool(false) => pure("false"),
            Literal::Bool(true) => pure("true"),
            Literal::Flt(value) => print_flt(value),
            // Entries are comma-delimited, so an operand is an ordinary term needing no parenthesization — a constant atom is a numeral term and prints as written. An empty segment list prints `x[]` on its own.
            Literal::Bytes(segments) => {
                let entries = segments
                    .into_iter()
                    .map(|segment| match segment {
                        BytesSegment::Atom(operand) => ListEntry::Elem(operand),
                        BytesSegment::Spread(operand) => ListEntry::Spread(operand),
                    })
                    .collect();
                self.wrapped_list("x[", entries)
            }
            Literal::List(entries) => self.wrapped_list("[", entries),
        }
    }

    /// An intrinsic as core's one table writes it ([`intrinsic_form`]), through the translation a report's term is written by. A `/sys` declaration is the only place a text-stage intrinsic node exists, and a handle the host minted is one no source spells, shown by its token.
    fn intrinsic(self, intrinsic: Intrinsic<D>) -> Printer {
        match intrinsic_form(intrinsic, true) {
            IntrinsicForm::Handle(token) => pure(format!("Handle({token})")),
            form => {
                self.term(written_intrinsic(form, &Quoted).expect("a quoted form is written whole"))
            }
        }
    }

    fn term(self, term: Term<D>) -> Printer {
        self.term_followed(term, flat([]))
    }

    /// A postfix chain stays inside its head's layout decision: a ladder that fits only before its call breaks its arms first. Source marks still end each term before its suffix, so following the original sees the same token boundaries.
    fn term_followed(self, term: Term<D>, suffix: Printer) -> Printer {
        // The whole of what the printer tells a formatter's renderer: where the term's own text begins and ends in the source. The renderer follows the source by itself and needs neither to place a comment; they are where it picks the text up again past a literal this printer spells another way than it was written — see `Printer::Mark`. Outside a format run both emit nothing.
        // With room to recurse: a term a report prints is as deep as the reduction that built it, a type a recursion nested once a step among them.
        recurse(|| match term.span().map(|span| (span.start, span.end)) {
            Some((start, end)) => flat([
                reached(start),
                self.term_inner(term, flat([reached(end), suffix])),
            ]),
            None => self.term_inner(term, suffix),
        })
    }

    /// The parentheses the grammar demands and the tree does not record. An infix operand parenthesizes when its own operator binds looser than its position requires — the exact mirror of `op_precedence`'s climb, with the right operand one level up for left-associativity — and any whole-term form (a binding, a lambda, an arrow) parenthesizes unconditionally, since the operand grammar cannot produce it bare. A `match` or `choose` does not: `end` closes it, so it is an atom and prints bare in every position.
    fn operand(self, term: Term<D>, min_prec: u8) -> Printer {
        let parenthesized = match term.as_subterm() {
            Subterm::Infix(infix) => op_precedence(infix.op) < min_prec,
            Subterm::Let(_) | Subterm::FuncType(_) | Subterm::Func(_) => true,
            _ => false,
        };
        match parenthesized {
            true => flat([pure("("), self.term(term), indent(ends()), pure(")")]),
            false => self.term(term),
        }
    }

    /// [`TreePrinter::operand`] for the head of an application, projection, or bang — a position above every operator, so any infix parenthesizes. A numeric literal parenthesizes too: `(1).0` must not reprint as the float literal `1.0`.
    fn suffix_head(self, term: Term<D>, suffix: Printer) -> Printer {
        match term.as_subterm() {
            Subterm::NumLit(_) | Subterm::Literal(Literal::Flt(_)) => flat([
                pure("("),
                self.term(term),
                indent(ends()),
                pure(")"),
                suffix,
            ]),
            Subterm::Apply(_)
            | Subterm::Proj(_)
            | Subterm::Bang(_)
            | Subterm::Match(_)
            | Subterm::Choose(_) => self.term_followed(term, suffix),
            // A decorator takes the call chain after its operand, so under a suffix a decorated head is parenthesized: `(#{…} f)(x)` states the head's fields, where `#{…} f(x)` states the call's.
            Subterm::Core(form) if !D::takes_suffix(form) => flat([
                pure("("),
                self.term(term),
                indent(ends()),
                pure(")"),
                suffix,
            ]),
            _ => flat([self.operand(term, u8::MAX), suffix]),
        }
    }

    fn term_inner(self, term: Term<D>, suffix: Printer) -> Printer {
        // Where a name was written, which a page asks the lowering about instead of resolving the text again.
        let written = term.span().map(|span| span.start..span.end);
        let body = match term.into_subterm() {
            Subterm::Type => pure("Type"),
            Subterm::Prop => pure("Prop"),
            Subterm::Sort => pure("Sort"),
            Subterm::Literal(literal) => self.literal(literal),
            Subterm::Intrinsic(intrinsic) => self.intrinsic(intrinsic),
            Subterm::Name(name) => match written {
                Some(written) => named_at(name.join(), written),
                None => named(name.join()),
            },
            // Both spell `?`: the written/desugared distinction matters to zonk's reporting, not to how the term reads.
            Subterm::Hole | Subterm::Goal => pure("?"),
            Subterm::Held => pure("_"),
            // Never parsed: the witness lowering mints it, and a `satisfy` prints its `;` from the declaration, not from here.
            Subterm::Derive => pure("derive"),
            Subterm::ProofLiteral(ProofLiteral::Char(character)) => print_char_literal(character),
            Subterm::ProofLiteral(ProofLiteral::Str(literal)) if literal.block => {
                print_block_string(&literal.value)
            }
            Subterm::ProofLiteral(ProofLiteral::Str(literal)) => pure(format!(
                "\"{}\"",
                literal
                    .value
                    .chars()
                    .map(|character| match character {
                        '"' => "\\\"".to_string(),
                        '\\' => "\\\\".to_string(),
                        '\n' => "\\n".to_string(),
                        '\t' => "\\t".to_string(),
                        '\r' => "\\r".to_string(),
                        hidden if unseen(hidden) => braced_escape(hidden),
                        _ => character.to_string(),
                    })
                    .collect::<String>()
            )),
            Subterm::FuncType(FuncType { params, output }) => flat([
                listed(
                    "(",
                    params
                        .into_iter()
                        .map(|param| self.func_type_param(param))
                        .collect(),
                    ")",
                ),
                pure(" -> "),
                self.term(output),
            ]),
            // A lambda's body rides its arrow: inline when it fits, on the next line one level deeper when it does not — the corpus writes `(x, acc) => f(x)` inline.
            Subterm::Func(Func { params, body }) => flat([
                listed(
                    "(",
                    params
                        .into_iter()
                        .map(|param| self.func_pattern_param(param))
                        .collect(),
                    ")",
                ),
                self.attached_body(" =>", body),
            ]),
            Subterm::Apply(Apply { head, arguments }) => {
                return self.riding_call(head, arguments, suffix);
            }
            Subterm::TupleType(TupleType { fields }) => listed(
                "{",
                fields.into_iter().map(|field| self.field(field)).collect(),
                "}",
            ),
            Subterm::Core(form) => return D::form(self, form, suffix),
            Subterm::Tuple(Tuple { fields }) => {
                if fields.len() == 1 {
                    let field = fields.into_iter().next().unwrap();
                    // A labeled one-element tuple needs no trailing comma — the `=` already disambiguates it from a parenthesized term.
                    let comma = if field.label.is_some() { "" } else { "," };
                    flat([
                        pure("("),
                        self.tuple_field(field),
                        pure(comma),
                        indent(ends()),
                        pure(")"),
                    ])
                } else {
                    listed(
                        "(",
                        fields
                            .into_iter()
                            .map(|field| self.tuple_field(field))
                            .collect(),
                        ")",
                    )
                }
            }
            Subterm::Proj(Proj { head, field }) => {
                let field = match field {
                    Field::Index(index) => format!(".{index}"),
                    Field::Label(label) => format!(".{label}"),
                };
                return self.suffix_head(head, flat([pure(field), suffix]));
            }
            Subterm::StructLit(StructLit {
                head,
                params,
                entries,
            }) => flat([
                pure(head.join()),
                match params {
                    None => pure(""),
                    Some(params) => listed(
                        "(",
                        params
                            .into_iter()
                            .map(|argument| self.argument(argument))
                            .collect(),
                        ")",
                    ),
                },
                pure(" "),
                listed_block(
                    "{",
                    entries
                        .into_iter()
                        .map(|entry| self.struct_entry(entry))
                        .collect(),
                    "}",
                ),
            ]),
            // An arm ladder is width-adaptive: `match carry | true => x[\\1] | false => x[] end` is how the corpus writes a two-arm decision and how it reads best, and forcing every one of them onto five lines would make a proof-heavy module half again as tall as it was written.
            //
            // Two ladders are broken regardless of width, by [`is_ladder`]. Arms sit at the ladder's own column when broken, never indented, so `| pattern =>` and the `end` that closes it line up.
            Subterm::Choose(Choose { arms, default }) => {
                let forced = arms.len() + 1 > MAX_FLAT_ARMS
                    || std::iter::once(&default)
                        .chain(arms.iter().map(|arm| &arm.body))
                        .any(is_ladder);
                let separator = match forced {
                    true => hard_line,
                    false => line,
                };
                return group(flat([
                    pure("choose"),
                    flat(
                        arms.into_iter()
                            .map(|ChooseArm { test, body }| {
                                let head = match test {
                                    ChooseTest::Cond(condition) => {
                                        flat([pure("| "), self.term(condition)])
                                    }
                                    ChooseTest::Bind { pattern, value } => flat([
                                        pure("| "),
                                        print_match_pattern(pattern),
                                        pure(" = "),
                                        self.term(value),
                                    ]),
                                };
                                flat([separator(), head, self.attached_body(" =>", body)])
                            })
                            .collect::<Vec<_>>(),
                    ),
                    separator(),
                    pure("| _"),
                    self.attached_body(" =>", default),
                    separator(),
                    pure("end"),
                    suffix,
                ]));
            }
            Subterm::Match(Match { head, motive, arms }) => {
                let forced =
                    arms.len() > MAX_FLAT_ARMS || arms.iter().map(|arm| &arm.body).any(is_ladder);
                let separator = match forced {
                    true => hard_line,
                    false => line,
                };
                return group(flat([
                    pure("match "),
                    self.term(head),
                    self.motive(motive),
                    flat(
                        arms.into_iter()
                            .map(|arm| {
                                // Where the pattern ends, for the reason a term is marked — see `term_followed`: a literal leaf may be spelled another way than it was written.
                                let written = match arm.span {
                                    Some(span) => reached(span.end),
                                    None => flat([]),
                                };
                                flat([
                                    separator(),
                                    pure("| "),
                                    print_match_pattern(arm.pattern),
                                    written,
                                    self.attached_body(" =>", arm.body),
                                ])
                            })
                            .collect::<Vec<_>>(),
                    ),
                    separator(),
                    pure("end"),
                    suffix,
                ]));
            }
            Subterm::Let(Let { groups, tail }) => {
                let statements = groups
                    .into_iter()
                    .map(|group| {
                        let mut parts = Vec::new();
                        for (index, member) in group.members.into_iter().enumerate() {
                            let keyword = match index {
                                0 => "let ",
                                _ => {
                                    parts.push(hard_line());
                                    "and "
                                }
                            };
                            parts.extend([
                                pure(keyword),
                                print_pattern(member.binder),
                                self.let_signature(member.signature, false),
                            ]);
                        }
                        parts.extend([pure(";"), hard_line()]);
                        flat(parts)
                    })
                    .collect::<Vec<_>>();
                // A run of statements reads as one block, so a block written as the tail of another is one only between parentheses.
                let tail = match tail.as_subterm() {
                    Subterm::Let(_) => {
                        flat([pure("("), self.term(tail), indent(ends()), pure(")")])
                    }
                    _ => self.term(tail),
                };
                flat(statements.into_iter().chain([tail]))
            }
            Subterm::Bang(term) => return self.suffix_head(term, flat([pure("!"), suffix])),
            // An overflowing operator chain breaks with the operator leading the continuation line.
            Subterm::Infix(Infix { op, left, right }) => {
                let precedence = op_precedence(op);
                group(flat([
                    self.operand(left, precedence),
                    indent(flat([
                        line(),
                        pure(format!("{} ", op.symbol())),
                        self.operand(right, precedence + 1),
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
        };
        flat([body, suffix])
    }

    /// A `let` signature and body. `top` selects the corpus's top-level shape — the body *always* on the next line after `=` — while a local binding's body rides the `=` inline when it fits.
    fn let_signature(self, signature: LetSignature<D>, top: bool) -> Printer {
        let bound = |body: Term<D>| {
            if top {
                flat([pure(" ="), hard_line(), indent(self.term(body))])
            } else {
                self.attached_body(" =", body)
            }
        };
        match signature {
            LetSignature::Name { type_, body } => flat([
                match type_ {
                    Some(type_) => flat([pure(": "), self.term(type_)]),
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
                let signature = self.signature(params, output);
                flat([signature, bound(body)])
            }
        }
    }

    /// A definition's parameters and the `-> output` after them, in the one group [`TreePrinter::let_signature`] measures them in.
    fn signature(self, params: Vec<FuncSugarParam<D>>, output: Term<D>) -> Printer {
        let items = params
            .into_iter()
            .map(|param| self.func_sugar_param(param))
            .collect::<Vec<_>>();
        match items.is_empty() {
            true => flat([pure("()"), pure(" -> "), self.term(output)]),
            false => group(flat([telescope(items), pure(" -> "), self.term(output)])),
        }
    }

    fn top_test(self, test: TopTest<D>) -> Printer {
        D::headed(
            self,
            &test.core,
            flat([
                pure("test "),
                D::declared(&test.core, test.label),
                pure(" ="),
                hard_line(),
                indent(flat([self.term(test.body), ends()])),
                pure(";"),
            ]),
        )
    }

    /// One member of a `let` group under `keyword`, `let` for the first and `and` for a later one: its body, and not the `;` the group owes.
    fn let_member(self, item: TopLet<D>, keyword: &'static str) -> Printer {
        flat([
            print_doc(item.doc),
            D::headed(
                self,
                &item.core,
                flat([
                    // `pub` precedes `and`, which is the spelling the grammar accepts and the one the `induct` group beside this already emits. Reversed, a `pub` member of a group would print as `and pub f` and not reparse, and the formatter's verify gate would refuse the file rather than write it.
                    print_pub(item.vis_pub),
                    pure(keyword),
                    D::declared(&item.core, item.label),
                    self.let_signature(item.signature, true),
                ]),
            ),
        ])
    }

    fn top_let(self, items: Vec<TopLet<D>>) -> Printer {
        let mut iter = items.into_iter();
        let first = iter.next().expect("a `let` item has a member");
        let rest = iter
            .map(|item| flat([hard_line(), self.let_member(item, "and ")]))
            .collect::<Vec<_>>();

        flat([
            self.let_member(first, "let "),
            flat(rest),
            // Only the last body ends the item: a comment written under an earlier one stands above the `and` that follows it.
            indent(ends()),
            pure(";"),
        ])
    }

    fn top_foreign(self, item: TopForeign<D>) -> Printer {
        flat([
            print_doc(item.doc),
            D::headed(
                self,
                &item.core,
                flat([
                    print_pub(item.vis_pub),
                    pure("foreign "),
                    D::declared(&item.core, item.label),
                    print_wire_signature(&item.signature),
                    pure(";"),
                ]),
            ),
        ])
    }

    /// A case's payload binders, each under its mark.
    fn payload(self, payload: Vec<CasePayloadParam<D>>) -> Printer {
        let items = payload
            .into_iter()
            .map(|param| {
                // Plicity prefixes the name (`@x` = implicit) — shared with `field`.
                flat([
                    print_plicity(param.plicity),
                    self.field(TupleTypeParam {
                        label: param.label.map(Label::from),
                        func_params: None,
                        type_: param.type_,
                    }),
                ])
            })
            .collect();
        listed("(", items, ")")
    }

    /// A case's target: the indices it pins, after its payload.
    fn target(self, target: Option<Vec<Term<D>>>) -> Printer {
        match target {
            Some(exprs) => flat([
                pure(": "),
                listed(
                    "(",
                    exprs.into_iter().map(|expr| self.term(expr)).collect(),
                    ")",
                ),
            ]),
            None => pure(""),
        }
    }

    fn top_induct_case(self, case: TopCase<D>) -> Printer {
        flat([
            hard_line(),
            print_doc(case.doc),
            pure("| "),
            D::cased(
                self,
                case.core,
                flat([
                    pure(case.label.to_string()),
                    self.payload(case.payload),
                    self.target(case.target),
                ]),
            ),
        ])
    }

    fn top_induct_params(self, params: Vec<FuncTypeParam<D>>) -> Printer {
        if params.is_empty() {
            return pure("");
        }

        group(telescope(
            params
                .into_iter()
                .map(|param| self.func_type_param(param))
                .collect(),
        ))
    }

    /// The head's arity after the name: the (mandatory) result sort, preceded by an index telescope when the inductive is indexed. `: Sort` for a plain type, `: (indices) -> Sort` for an indexed one — the spellings `parse_induct_arity` accepts, so a printed declaration round-trips.
    fn top_induct_arity(
        self,
        indices: Vec<(Option<String>, Term<D>)>,
        rep_pub: bool,
        result_sort: Term<D>,
    ) -> Printer {
        if indices.is_empty() {
            return flat([
                pure(": "),
                print_pub(rep_pub),
                D::result_sort(self, result_sort),
            ]);
        }

        flat([
            pure(": "),
            listed(
                "(",
                indices
                    .into_iter()
                    .map(|index| self.labeled(index))
                    .collect(),
                ")",
            ),
            pure(" -> "),
            print_pub(rep_pub),
            D::result_sort(self, result_sort),
        ])
    }

    /// One member of an `induct` group under its keyword: `induct` for the first, `and` for a later one.
    fn induct_member(self, item: TopInduct<D>, keyword: &'static str) -> Printer {
        flat([
            print_doc(item.doc),
            D::headed(
                self,
                &item.core,
                flat([
                    print_pub(item.vis_pub),
                    pure(keyword),
                    D::declared(&item.core, item.label),
                    self.top_induct_params(item.params),
                    self.top_induct_arity(item.indices, item.rep_pub, item.result_sort),
                    flat(
                        item.cases
                            .into_iter()
                            .map(|case| self.top_induct_case(case))
                            .collect::<Vec<_>>(),
                    ),
                ]),
            ),
        ])
    }

    fn top_induct(self, group: Vec<TopInduct<D>>) -> Printer {
        let mut iter = group.into_iter();
        let first = iter.next().expect("an `induct` item has a member");
        let rest = iter
            .map(|item| flat([hard_line(), self.induct_member(item, "and ")]))
            .collect::<Vec<_>>();

        flat([
            self.induct_member(first, "induct "),
            flat(rest),
            hard_line(),
            pure("end"),
        ])
    }

    /// A `struct` item: one structure, or a group joined by `and`.
    fn top_struct(self, items: Vec<TopStruct<D>>) -> Printer {
        let mut iter = items.into_iter();
        let mut first = iter.next().expect("a `struct` item has a member");
        let rest = iter
            .map(|mut item| {
                flat([
                    hard_line(),
                    print_doc(item.doc.take()),
                    self.struct_member(item, "and "),
                ])
            })
            .collect::<Vec<_>>();
        flat([
            print_doc(first.doc.take()),
            self.struct_member(first, "struct "),
            flat(rest),
        ])
    }

    fn struct_member(self, item: TopStruct<D>, keyword: &'static str) -> Printer {
        D::headed(
            self,
            &item.core,
            flat([
                print_pub(item.vis_pub),
                pure(keyword),
                D::declared(&item.core, item.label),
                self.top_induct_params(item.params),
                pure(": "),
                print_pub(item.rep_pub),
                D::result_sort(self, item.result_sort),
                pure(" "),
                listed_hard(
                    "{",
                    item.fields
                        .into_iter()
                        .map(|field| self.struct_field(field))
                        .collect(),
                    "}",
                ),
            ]),
        )
    }

    /// A struct field: its documentation comment, then the Σ-type field it is written as.
    fn struct_field(self, field: StructField<D>) -> Printer {
        flat([
            print_doc(field.doc),
            print_plicity(field.plicity),
            self.field(field.param),
        ])
    }

    /// A concept field as written: a superclass as `use T`, by the word a later field reads it by where Core text names one, `use w: T`, and a method by its name and type, sugar included.
    fn concept_field_written(self, field: ConceptField<D>) -> Printer {
        // A superclass edge is a `use` member: the mark and its type, and no label but the word Core text reads it by.
        let (false, Some(label)) = (field.is_super(), field.label.clone()) else {
            return match field.label {
                Some(label) if field.is_super() => flat([
                    pure("use "),
                    pure(label.to_string()),
                    pure(": "),
                    self.term(field.type_),
                ]),
                _ => flat([pure("use "), self.term(field.type_)]),
            };
        };
        match field.func_params {
            Some(params) => flat([
                pure(label),
                listed(
                    "(",
                    params
                        .into_iter()
                        .map(|param| self.func_type_param(param))
                        .collect(),
                    ")",
                ),
                pure(" -> "),
                self.term(field.type_),
            ]),
            None => flat([pure(label), pure(": "), self.term(field.type_)]),
        }
    }

    fn concept_field(self, mut field: ConceptField<D>) -> Printer {
        flat([
            print_doc(field.doc.take()),
            self.concept_field_written(field),
        ])
    }

    /// A `concept` item: one concept, or a group joined by `and`, printed as a `struct` group is.
    fn top_concept(self, items: Vec<TopConcept<D>>) -> Printer {
        let mut iter = items.into_iter();
        let mut first = iter.next().expect("a `concept` item has a member");
        let rest = iter
            .map(|mut item| {
                flat([
                    hard_line(),
                    print_doc(item.doc.take()),
                    self.concept_member(item, "and "),
                ])
            })
            .collect::<Vec<_>>();
        flat([
            print_doc(first.doc.take()),
            self.concept_member(first, "concept "),
            flat(rest),
        ])
    }

    fn concept_member(self, item: TopConcept<D>, keyword: &'static str) -> Printer {
        D::headed(
            self,
            &item.core,
            flat([
                print_pub(item.vis_pub),
                pure(keyword),
                D::declared(&item.core, item.label),
                self.top_induct_params(item.params),
                pure(": "),
                print_pub(item.rep_pub),
                D::result_sort(self, item.result_sort),
                pure(" "),
                listed_hard(
                    "{",
                    item.fields
                        .into_iter()
                        .map(|field| self.concept_field(field))
                        .collect(),
                    "}",
                ),
            ]),
        )
    }
}

/// A dialect that writes Core text's forms over its own terms: a declaration under its decorator and its absolute name, a case under its decorator, and an operand under a decorator, a witness by its identity and an intrinsic node by its variant.
trait CoreWritten: Layout + Dialect<Head = CoreHead<Self>, Fields = Vec<DecoratorField<Self>>> {
    /// The form of Core text `form` is, where it is one.
    fn as_core(form: &Self::Form) -> Option<&CoreForm<Self>>;

    /// Whether a render under `options` writes a decorator above its operand where the two do not fit their line, as a text is written, rather than beside it, as a sentence quotes one.
    fn decorators_above(options: Self::Options) -> bool;
}

/// Whether `term` is what a decorator takes bare: a held place, or an applied atom — a primary with its call chain — which every form is but a binding, a lambda, an arrow, an operator, a decorated term and the text stage's intrinsic node, each parenthesized under it.
fn decorates_bare<D: CoreWritten>(term: &Term<D>) -> bool {
    match term.as_subterm() {
        Subterm::Let(_)
        | Subterm::Func(_)
        | Subterm::FuncType(_)
        | Subterm::Infix(_)
        | Subterm::Intrinsic(_) => false,
        Subterm::Core(form) => D::form_is_atom(form),
        _ => true,
    }
}

/// Whether `sort` is what a head's sort is written as bare: `Type` or `Prop`, under a decorator or not.
fn sort_written_bare<D: CoreWritten>(sort: &Term<D>) -> bool {
    let bare = |term: &Term<D>| matches!(term.as_subterm(), Subterm::Type | Subterm::Prop);
    let decorated = match sort.as_subterm() {
        Subterm::Core(form) => match D::as_core(form) {
            Some(CoreForm::Decorated(Decorated { term, .. })) => bare(term),
            _ => false,
        },
        _ => false,
    };

    bare(sort) || decorated
}

/// Whether a form of Core text prints as one indivisible piece: all but a decorated term.
fn core_form_is_atom<D: Dialect>(form: &CoreForm<D>) -> bool {
    !matches!(form, CoreForm::Decorated(_))
}

/// Whether a call chain may follow a form of Core text as it stands: an intrinsic written with no operand list would take a call's arguments as its own.
fn core_takes_suffix<D: Dialect>(form: &CoreForm<D>) -> bool {
    match form {
        CoreForm::Decorated(_) => false,
        CoreForm::Witness(_) => true,
        CoreForm::Intrinsic(CoreIntrinsic { operands, .. }) => operands.is_some(),
    }
}

/// Core text's forms: an operand under a decorator, a witness by its identity, an intrinsic node by its variant, and the decorator before a declaration and a case, with each declaration named by its absolute name ([`CoreText`]). A file is laid out as a text, each decorator above what it decorates where the two do not fit their line.
impl Layout for CoreText {
    type Options = ();

    fn form(printer: TreePrinter<Self>, form: CoreForm, suffix: Printer) -> Printer {
        flat([printer.core_form(form), suffix])
    }

    fn form_is_atom(form: &CoreForm) -> bool {
        core_form_is_atom(form)
    }

    fn takes_suffix(form: &CoreForm) -> bool {
        core_takes_suffix(form)
    }

    fn cased(printer: TreePrinter<Self>, fields: Vec<DecoratorField>, case: Printer) -> Printer {
        printer.decorated(fields, case)
    }

    fn headed(printer: TreePrinter<Self>, head: &CoreHead, member: Printer) -> Printer {
        printer.decorated(head.fields.clone(), member)
    }

    fn declared(head: &CoreHead, _: Label) -> Printer {
        pure(head.name.faithful().to_string())
    }

    fn result_sort(printer: TreePrinter<Self>, sort: Term<CoreText>) -> Printer {
        printer.core_result_sort(sort)
    }
}

impl CoreWritten for CoreText {
    fn as_core(form: &CoreForm) -> Option<&CoreForm> {
        Some(form)
    }

    fn decorators_above((): ()) -> bool {
        true
    }
}

/// What a render of a shown term is told.
#[derive(Clone, Copy)]
pub struct ShownOptions {
    /// Whether a decorator stands above its operand where the two do not fit their line, as a dump written as a text has it, or beside it, as a refusal quoted in a sentence does.
    pub decorators_above: bool,
}

/// A shown term's forms: Core text's, and the ones only a bounded print or a reader's spelling writes ([`Shown`]).
impl Layout for Shown {
    type Options = ShownOptions;

    fn form(printer: TreePrinter<Self>, form: ShownForm, suffix: Printer) -> Printer {
        let written = match form {
            ShownForm::Core(form) => printer.core_form(form),
            ShownForm::Elided => pure(ELIDED),
            ShownForm::Spelled(text) => pure(text),
            ShownForm::Metavar { id, spine } if spine.is_empty() => pure(format!("?{id}")),
            ShownForm::Metavar { id, spine } => flat([
                pure(format!("?{id}[")),
                sep_flat(
                    spine
                        .into_iter()
                        .map(|entry| printer.term(entry))
                        .collect::<Vec<_>>(),
                    || pure(", "),
                ),
                pure("]"),
            ]),
            ShownForm::Ascribed { term, type_ } => listed(
                "(",
                vec![flat([printer.term(term), pure(": "), printer.term(type_)])],
                ")",
            ),
        };
        flat([written, suffix])
    }

    fn form_is_atom(form: &ShownForm) -> bool {
        match form {
            ShownForm::Core(form) => core_form_is_atom(form),
            ShownForm::Elided
            | ShownForm::Spelled(_)
            | ShownForm::Metavar { .. }
            | ShownForm::Ascribed { .. } => true,
        }
    }

    fn takes_suffix(form: &ShownForm) -> bool {
        match form {
            ShownForm::Core(form) => core_takes_suffix(form),
            ShownForm::Elided
            | ShownForm::Spelled(_)
            | ShownForm::Metavar { .. }
            | ShownForm::Ascribed { .. } => true,
        }
    }

    fn cased(
        printer: TreePrinter<Self>,
        fields: Vec<DecoratorField<Shown>>,
        case: Printer,
    ) -> Printer {
        printer.decorated(fields, case)
    }

    fn headed(printer: TreePrinter<Self>, head: &CoreHead<Shown>, member: Printer) -> Printer {
        printer.decorated(head.fields.clone(), member)
    }

    fn declared(head: &CoreHead<Shown>, _: Label) -> Printer {
        pure(head.name.faithful().to_string())
    }

    fn result_sort(printer: TreePrinter<Self>, sort: Term<Shown>) -> Printer {
        printer.core_result_sort(sort)
    }
}

impl CoreWritten for Shown {
    fn as_core(form: &ShownForm) -> Option<&CoreForm<Shown>> {
        match form {
            ShownForm::Core(form) => Some(form),
            ShownForm::Elided
            | ShownForm::Spelled(_)
            | ShownForm::Metavar { .. }
            | ShownForm::Ascribed { .. } => None,
        }
    }

    fn decorators_above(options: ShownOptions) -> bool {
        options.decorators_above
    }
}

impl<D: CoreWritten> TreePrinter<D> {
    /// A form of Core text: an operand under its decorator, parenthesized where it is larger than an applied atom, a witness by its identity, an intrinsic node by its variant over its operands.
    fn core_form(self, form: CoreForm<D>) -> Printer {
        match form {
            CoreForm::Decorated(Decorated { fields, term }) => {
                let operand = match decorates_bare(&term) {
                    true => self.term(term),
                    false => flat([pure("("), self.term(term), indent(ends()), pure(")")]),
                };
                self.decorated(fields, operand)
            }
            CoreForm::Witness(id) => pure(id.to_string()),
            CoreForm::Intrinsic(CoreIntrinsic { variant, operands }) => flat([
                pure(format!("#{variant}")),
                match operands {
                    Some(operands) => listed(
                        "(",
                        operands
                            .into_iter()
                            .map(|operand| self.value(operand))
                            .collect(),
                        ")",
                    ),
                    None => pure(""),
                },
            ]),
        }
    }

    /// The sort a head lands in: bare where it is a sort, and between parentheses otherwise, where a name would read the field block after it as a literal of its own.
    fn core_result_sort(self, sort: Term<D>) -> Printer {
        match sort_written_bare(&sort) {
            true => self.term(sort),
            false => flat([pure("("), self.term(sort), indent(ends()), pure(")")]),
        }
    }

    /// `node` under a decorator setting each of `fields`, or `node` alone where it sets none: where the render writes decorators above, the two on the line they are reached on where they fit it whole and otherwise the decorator on a line of its own and `node` on the line beneath it, and beside it otherwise.
    fn decorated(self, fields: Vec<DecoratorField<D>>, node: Printer) -> Printer {
        if fields.is_empty() {
            return node;
        }
        let decorator = self.decorator_fields("#{", fields);

        match D::decorators_above(self.options) {
            true => own_line_if_broken(flat([decorator, line(), node])),
            false => flat([decorator, pure(" "), node]),
        }
    }

    /// `fields` after `open` and before the brace that closes them, each `label = value`.
    fn decorator_fields(self, open: &'static str, fields: Vec<DecoratorField<D>>) -> Printer {
        listed(
            open,
            fields
                .into_iter()
                .map(|DecoratorField { label, value }| {
                    flat([pure(label.to_string()), pure(" = "), self.value(value)])
                })
                .collect(),
            "}",
        )
    }

    /// What a decorator's field holds and an intrinsic takes: a term, fields between braces, a list of values each under its mark, or the root module.
    fn value(self, value: CoreValue<D>) -> Printer {
        match value {
            CoreValue::Term(term) => self.term(term),
            CoreValue::Fields(fields) => self.decorator_fields("{", fields),
            CoreValue::List(entries) => listed(
                "[",
                entries
                    .into_iter()
                    .map(|(plicity, value)| flat([print_plicity(plicity), self.value(value)]))
                    .collect(),
                "]",
            ),
            CoreValue::Root => pure("/"),
        }
    }

    /// One member of a group: the first under its own word, a later one after `and`, a nominal one keeping its word there, and a family closed by its own `end`.
    fn core_member(self, member: CoreMember<D>, first: bool) -> Printer {
        let keyword = |word: &'static str, later: &'static str| match first {
            true => word,
            false => later,
        };
        match member {
            CoreMember::Let(item) => self.let_member(item, keyword("let ", "and ")),
            CoreMember::Induct(item) => flat([
                self.induct_member(item, keyword("induct ", "and induct ")),
                hard_line(),
                pure("end"),
            ]),
            CoreMember::Struct(mut item) => flat([
                print_doc(item.doc.take()),
                self.struct_member(item, keyword("struct ", "and struct ")),
            ]),
            CoreMember::Concept(mut item) => flat([
                print_doc(item.doc.take()),
                self.concept_member(item, keyword("concept ", "and concept ")),
            ]),
        }
    }

    /// One item of Core text: a group, its members a line apart and closed by the `;` a `let` owes where it is the last, a foreign row, or a test.
    fn core_item(self, item: CoreItem<D>) -> Printer {
        match item {
            CoreItem::Group(members) => {
                let owes = matches!(members.last(), Some(CoreMember::Let(_)));
                let mut parts = Vec::new();
                for (index, member) in members.into_iter().enumerate() {
                    if index > 0 {
                        parts.push(hard_line());
                    }
                    parts.push(self.core_member(member, index == 0));
                }
                if owes {
                    parts.extend([indent(ends()), pure(";")]);
                }
                flat(parts)
            }
            CoreItem::Foreign(item) => self.top_foreign(item),
            CoreItem::Test(item) => self.top_test(item),
        }
    }

    /// The term a program closes with, and the type it states on the line beneath it.
    fn core_entry(self, entry: CoreEntry<D>) -> Printer {
        match entry.type_ {
            Some(type_) => flat([
                self.term(entry.body),
                hard_line(),
                pure(": "),
                self.term(type_),
            ]),
            None => self.term(entry.body),
        }
    }
}

/// The printer of Core text.
const CORE_TEXT: TreePrinter<CoreText> = TreePrinter { options: () };

/// One item of a `.crsc`, as the formatter writes it.
pub fn print_core_item(item: CoreItem) -> Printer {
    CORE_TEXT.core_item(item)
}

/// The entry a `.crsc` closes with, as the formatter writes it.
pub fn print_core_entry(entry: CoreEntry) -> Printer {
    CORE_TEXT.core_entry(entry)
}

pub fn print_term(term: Term) -> Printer {
    PROGRAM.term(term)
}

/// A shown term, laid out as `options` say.
pub fn print_shown(term: Term<Shown>, options: ShownOptions) -> Printer {
    TreePrinter::<Shown> { options }.term(term)
}

/// One item of a shown module, laid out as `options` say.
pub fn print_shown_item(item: CoreItem<Shown>, options: ShownOptions) -> Printer {
    TreePrinter::<Shown> { options }.core_item(item)
}

/// The entry a shown program closes with, laid out as `options` say.
pub fn print_shown_entry(entry: CoreEntry<Shown>, options: ShownOptions) -> Printer {
    TreePrinter::<Shown> { options }.core_entry(entry)
}

fn print_top_use(item: TopUse) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("use "),
        pure(item.name.join()),
        match item.group {
            // Filled rather than `listed`, because an import is a run of short interchangeable names and not a structure. `listed` is a group, so a head too wide for one line would put every name on a line of its own. The names wrap like prose instead, and each carries its own comma so the fill only ever inserts the gap.
            UseGroup::Named(items) => {
                filled("/{", items.iter().map(print_group_item).collect(), "}")
            }
            UseGroup::Glob => pure("/*"),
        },
        pure(";"),
    ])
}

fn print_top_mod(item: TopMod) -> Printer {
    match item.module {
        None => flat([
            print_doc(item.doc),
            print_pub(item.vis_pub),
            pure("mod "),
            pure(item.label),
            pure(";"),
        ]),
        Some(module) => flat([
            print_doc(item.doc),
            print_pub(item.vis_pub),
            pure("mod "),
            pure(item.label),
            hard_line(),
            indent(flat([print_module_items(module.items), ends()])),
            hard_line(),
            pure("end"),
        ]),
    }
}

/// How one module item is separated from the next, wherever a module is printed: a `use` following a `use` or a file-backed `mod` keeps the separation it was written with, closed up against it or a blank line apart, and everything else takes a blank line. `written_apart` is whether the source parts the two with a blank line, `None` where there is no source to read, and then the `use` closes up.
///
/// Which imports read as one paragraph is the author's to say: a facade's `mod x; use x/{…}` stacks, and the file's imports that follow its last facade stand apart from it, which no rule over the two items alone can tell from the facade's own lines.
///
/// Shared with [`crate::format::emit`], which prints a *file*'s items, so the two cannot disagree about what a module looks like.
pub fn between_items(
    previous: Option<&TopItem>,
    next: &TopItem,
    written_apart: Option<bool>,
) -> Printer {
    let closes_up = match previous {
        None => return pure(""),
        Some(TopItem::Use(_)) => matches!(next, TopItem::Use(_)),
        // A *file-backed* `mod x;` is one line of the same paragraph; an inline `mod … end` spans lines of its own, so what follows it starts a new one.
        Some(TopItem::Mod(previous)) => {
            previous.module.is_none() && matches!(next, TopItem::Use(_))
        }
        Some(_) => false,
    };
    match closes_up && written_apart != Some(true) {
        true => hard_line(),
        false => flat([hard_line(), hard_line()]),
    }
}

pub fn print_module_items(items: Vec<TopItem>) -> Printer {
    let mut parts = Vec::with_capacity(items.len() * 2);
    let mut previous: Option<&TopItem> = None;

    for item in &items {
        parts.push(between_items(previous, item, None));
        parts.push(print_top_item(item.clone()));
        previous = Some(item);
    }

    flat(parts)
}

fn print_top_witness(items: Vec<TopWitness>) -> Printer {
    let mut iter = items.into_iter();
    let mut first = iter.next().expect("a `satisfy` item has a member");
    let rest = iter
        .map(|mut item| {
            flat([
                hard_line(),
                print_doc(item.doc.take()),
                print_witness_member(item, "and"),
            ])
        })
        .collect::<Vec<_>>();

    flat([
        print_doc(first.doc.take()),
        print_witness_member(first, "satisfy"),
        flat(rest),
    ])
}

/// A witness's telescope and the application it concludes, in **one** group, as a `let`'s parameters share one with its `-> output` and for its reason. Measured apart, a telescope that fits on its own stays flat and leaves the application to break, which stacks the concept's arguments at the body's indent with the `) {` that tells the two apart riding the last of them. Together, the telescope breaks first and the application, now starting a fresh line, fits beside the arrow.
fn witness_signature(params: Vec<Printer>, app: Printer) -> Printer {
    match params.is_empty() {
        true => app,
        false => group(flat([telescope(params), pure(" => "), app])),
    }
}

/// The concept application a witness concludes: the concept's name, then its argument list where one was written.
fn witnessed(concept: &Name, args: Option<Vec<Argument>>) -> Printer {
    match args {
        None => pure(concept.join()),
        Some(args) => flat([
            pure(concept.join()),
            listed(
                "(",
                args.into_iter()
                    .map(|argument| PROGRAM.argument(argument))
                    .collect(),
                ")",
            ),
        ]),
    }
}

fn print_witness_member(item: TopWitness, keyword: &'static str) -> Printer {
    let params = item
        .params
        .into_iter()
        .map(|param| PROGRAM.func_sugar_param(param))
        .collect();
    let app = witnessed(&item.concept, item.args);

    // A derived witness's block holds `..` alone, on a line of its own as a lone entry is, and with no comma since it is none.
    let body = match item.body {
        Some(entries) => flat([
            pure(" "),
            listed_hard(
                "{",
                entries.into_iter().map(print_witness_entry).collect(),
                "}",
            ),
        ]),
        None => flat([
            pure(" {"),
            indent(flat([hard_line(), pure(".."), ends()])),
            hard_line(),
            pure("}"),
        ]),
    };

    flat([
        pure(keyword),
        pure(" "),
        witness_signature(params, app),
        body,
    ])
}

/// A witness-body entry, which is an implementation field — `label = value`, or the definition sugar `label(params) = value` re-sugared from the retained parameter list.
fn print_witness_entry(field: WitnessField) -> Printer {
    match field.func_params {
        Some(params) => flat([
            pure(field.label),
            listed(
                "(",
                params
                    .into_iter()
                    .map(|param| PROGRAM.func_param(param))
                    .collect(),
                ")",
            ),
            PROGRAM.attached_body(" =", field.value),
        ]),
        None => flat([pure(field.label), PROGRAM.attached_body(" =", field.value)]),
    }
}

pub fn print_top_item(item: TopItem) -> Printer {
    match item {
        TopItem::Mod(m) => print_top_mod(m),
        TopItem::Use(u) => print_top_use(u),
        TopItem::Let(l) => PROGRAM.top_let(l),
        TopItem::Induct(group) => PROGRAM.top_induct(group),
        TopItem::Struct(s) => PROGRAM.top_struct(s),
        TopItem::Concept(c) => PROGRAM.top_concept(c),
        TopItem::Witness(w) => print_top_witness(w),
        TopItem::Foreign(f) => PROGRAM.top_foreign(f),
        TopItem::Test(t) => PROGRAM.top_test(t),
        // Verbatim: there is no tree to print, and the formatter never reaches here, since it refuses a module it could not read whole.
        TopItem::Broken(b) => pure(b.text().to_string()),
    }
}

// === Declaration heads ==========================================================
//
// What a page shows of a declaration: everything before its body, printed from the same tree and by the same printers the formatter uses, so a signature on a page is the signature in the file. Each takes a reference and clones what it prints, since the printers consume what they are handed and a record is built beside the tree rather than from it.

/// The head of a `let` member: its visibility, name and signature, without the body.
pub fn print_let_head(item: &TopLet) -> Printer {
    let signature = match &item.signature {
        LetSignature::Name {
            type_: Some(type_), ..
        } => flat([pure(": "), print_term(type_.clone())]),
        LetSignature::Name { type_: None, .. } => pure(""),
        LetSignature::Func { params, output, .. } => {
            PROGRAM.signature(params.clone(), output.clone())
        }
    };
    flat([
        print_pub(item.vis_pub),
        pure("let "),
        pure(item.label.to_string()),
        signature,
    ])
}

/// The head of an `induct` member: its visibility, name, parameters and arity, without the cases.
pub fn print_induct_head(item: &TopInduct) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("induct "),
        pure(item.label.to_string()),
        PROGRAM.top_induct_params(item.params.clone()),
        PROGRAM.top_induct_arity(item.indices.clone(), item.rep_pub, item.result_sort.clone()),
    ])
}

/// One constructor as a page lists it: its label, payload and target, without the bar that introduces it in a declaration.
pub fn print_case_head(case: &TopCase) -> Printer {
    flat([
        pure(case.label.clone()),
        PROGRAM.payload(case.payload.clone()),
        PROGRAM.target(case.target.clone()),
    ])
}

/// The head of a constructor where a module exposes it beside its type: the payload as the block writes it, then the family it produces, called once per parameter group — `success(A) -> Result(E, A)`, `cons(@n: Nat, head: T, tail: Vec(T)(n)) -> Vec(T)(n + 1)` for an indexed family, and `refl(@z: A) -> Eq()(z, z)` where every parameter is hidden.
///
/// **The result replaces the block's index suffix rather than joining it.** Inside the block a case states only the indices it targets, because the family is the declaration it sits in; standing alone it has to say what it produces, and writing both would spell those indices twice. What it deliberately does not state is the family's parameters, which are implicit at every value constructor: that is the lowering's rule to apply, and the card names its owner so the one card that does state them is a click away. Of the family's own parameters it writes the explicit ones, as a signature writes the family, a hidden one being optional in the call that owns it.
pub fn print_case_result_head(item: &TopInduct, case: &TopCase) -> Printer {
    let mut family = named(item.label.to_string());
    if !item.params.is_empty() {
        let explicit = item
            .params
            .iter()
            .filter(|param| param.plicity == Plicity::Explicit)
            .filter_map(|param| param.label.clone().map(pure))
            .collect();
        family = flat([family, listed("(", explicit, ")")]);
    }
    if let Some(target) = &case.target {
        let indices = target.iter().cloned().map(print_term).collect();
        family = flat([family, listed("(", indices, ")")]);
    }

    flat([
        pure(case.label.clone()),
        PROGRAM.payload(case.payload.clone()),
        pure(" -> "),
        family,
    ])
}

/// The head of a `struct` member: its visibility, name, parameters and sort, without the fields.
pub fn print_struct_head(item: &TopStruct) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("struct "),
        pure(item.label.to_string()),
        PROGRAM.top_induct_params(item.params.clone()),
        pure(": "),
        print_pub(item.rep_pub),
        print_term(item.result_sort.clone()),
    ])
}

/// One struct field as a page lists it.
pub fn print_struct_field_head(field: &StructField) -> Printer {
    flat([
        print_plicity(field.plicity),
        PROGRAM.field(field.param.clone()),
    ])
}

/// The head of a `concept` member: its visibility, name, parameters and sort, without the fields.
pub fn print_concept_head(item: &TopConcept) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("concept "),
        pure(item.label.to_string()),
        PROGRAM.top_induct_params(item.params.clone()),
        pure(": "),
        print_pub(item.rep_pub),
        print_term(item.result_sort.clone()),
    ])
}

/// One concept field as a page lists it: a superclass as `use T`, a method as written, sugar included.
pub fn print_concept_field_head(field: &ConceptField) -> Printer {
    PROGRAM.concept_field_written(field.clone())
}

/// The head of a `satisfy` member: the keyword, its telescope and the concept application it witnesses, without the body.
pub fn print_witness_head(item: &TopWitness) -> Printer {
    let params = item
        .params
        .iter()
        .cloned()
        .map(|param| PROGRAM.func_sugar_param(param))
        .collect();
    let app = witnessed(&item.concept, item.args.clone());
    flat([pure("satisfy "), witness_signature(params, app)])
}

/// The head of a `test` declaration: the keyword and its name, without the body.
pub fn print_test_head(item: &TopTest) -> Printer {
    flat([pure("test "), pure(item.label.to_string())])
}

/// The head of a `foreign` declaration, which is the whole of it.
pub fn print_foreign_head(item: &TopForeign) -> Printer {
    flat([
        print_pub(item.vis_pub),
        pure("foreign "),
        pure(item.label.to_string()),
        print_wire_signature(&item.signature),
    ])
}
