use {
    super::{
        CONCEPT_FIELD_TAKES_NO_IMPLICIT, INDEX_TAKES_NO_MARK, PAYLOAD_TAKES_NO_USE,
        STRUCT_FIELD_TAKES_NO_USE, members, parse_apply_argument, parse_binding,
        parse_field_member, parse_func_sugar_param, parse_func_type_param, parse_identifier,
        parse_keyword, parse_literal, parse_mark, parse_name, parse_premise, parse_prop,
        parse_tuple_field_prefix, parse_type, raised,
    },
    crate::{
        CasePayloadParam, ConceptField, DOC_BEFORE_NOTHING, Doc, FuncTypeParam, GroupItem, Label,
        Module, Name, StructField, Term, TopCase, TopConcept, TopForeign, TopInduct, TopItem,
        TopLet, TopMod, TopStruct, TopTest, TopUse, TopWitness, UseGroup, WitnessField, declared,
        parse_declared_label, parse_doc, parse_identifier_raw, parse_term, parse_whitespace,
        reserved_keyword, text_after, word_after,
    },
    curios_abi::{WireLeaf, WireResults, WireReturn, WireSignature, WireType},
    curios_parse::{
        Mark, Parser, ParserError, commit, fail, fail_from, lazy, look_ahead, many0, mark,
        not_ahead, pure, raise, refusal, sep_by0_trailing, sep_by1_trailing, spanned, tagging,
        take_exact,
    },
    curios_utilities::{Plicity, Qualifier, is_keyword},
    std::iter,
};

pub(super) fn parse_pub<'a>() -> Parser<'a, bool> {
    parse_keyword("pub").map(|()| true).or(pure(false))
}

/// What refuses a documentation comment above an import: nothing documents a `use`, since an import has no page and a re-export links to the declaration it re-exports.
const DOC_BEFORE_USE: &str = "a documentation comment cannot precede `use`: an import is not a declaration, and a re-export is read at the declaration it re-exports";

/// What refuses a documentation comment above a test, which is never part of an interface.
const DOC_BEFORE_TEST: &str =
    "a documentation comment cannot precede `test`: a test is not part of the interface";

/// The head of a later member of an `and` group: its documentation comment, its `pub`, and the `and` itself.
///
/// Recoverable when there is no documentation, since the absence of `and` is how a group ends. With one, the keyword after the block — past a `pub` — decides: `and` makes it this group's next member, so a failure past it is the diagnosis; another word makes it the next item's, so the group ends recoverably and the item loop reads the block again; anything else is nothing the block may document.
fn parse_and_head<'a>() -> Parser<'a, (Option<Doc>, bool)> {
    parse_doc().flat_map(|doc| {
        let head = parse_pub().and_drop(parse_keyword("and"));
        let Some(doc) = doc else {
            return head.map(|vis_pub| (None, vis_pub));
        };

        match word_after(&doc) {
            "and" => head
                .map_err(DOC_BEFORE_NOTHING)
                .map(move |vis_pub| (Some(doc), vis_pub)),
            "" | "end" => commit(fail(DOC_BEFORE_NOTHING)),
            // Recoverable: another word hands the block to the next item, which reads it again.
            _ => fail(DOC_BEFORE_NOTHING),
        }
    })
}

/// `parser`, refused with the documentation diagnosis when `doc` is present and the parser fails — for a member's head token, whose absence after a documentation comment is exactly that mistake.
fn documented<'a, T: 'a>(doc: &Option<Doc>, parser: Parser<'a, T>) -> Parser<'a, T> {
    match doc {
        Some(_) => commit(parser.map_err(DOC_BEFORE_NOTHING)),
        None => parser,
    }
}

// A top-level `let` item: one definition, or the group `let f … and g …;`. Each member takes its own `pub` — before `let` for the first, before `and` for each later one — and one `;` terminates the whole item.

// A `test` declaration: `test name = body;`. `pub` is refused by name: a test's identifier is its report line, not an export.
//
// Like `satisfy`, `test` stays a contextual word everywhere else, and the label is exactly how far that reaches. The label's own refusal stays recoverable because the head is already eaten when this runs: its failure sits past the item loop's choice point, so uncaught it would abort the loop rather than end it, and `test(1)` could not be a program's tail. Past the label there is nothing to fall through to — `test name` is two names in a row and so no term at all — so the rest commits and reports its own fault.
pub(super) fn parse_top_test<'a>(vis_pub: bool) -> Parser<'a, TopItem> {
    match vis_pub {
        true => fail("a test is never `pub`: its name is its report line, not an export"),
        false => pure(()),
    }
    .and_keep(declared(commit(
        parse_literal("=")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(";")),
    )))
    .map(|(label, body)| TopItem::Test(TopTest { label, body }))
}

pub(super) fn parse_top_let<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopItem> {
    let member = |doc: Option<Doc>, vis_pub: bool| {
        parse_binding().map(move |(label, signature)| TopLet {
            doc,
            vis_pub,
            label,
            signature,
        })
    };

    member(doc, vis_pub)
        .flat_map(move |first| {
            // The item declares what its head declared: a failure anywhere after the first member — a later member, the `;` — is named for the first, whatever a later member's own tag said.
            let tag = first.label.to_string();
            tagging(
                Some(tag),
                many0(move || {
                    parse_and_head().flat_map(move |(doc, vis_pub)| commit(member(doc, vis_pub)))
                })
                .and_drop(parse_literal(";")),
            )
            .map(move |rest| (first, rest))
        })
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Let)
}

/// The words a wire type is one of, which the refusal of anything else names.
const WIRE_TYPE_EXPECTED: &str =
    "expected a wire type (Nat, Int, Bool, Byte, Flt, Bytes, Bits, Handle, or List(...))";

/// The words a `List`'s element is one of.
const WIRE_LEAF_EXPECTED: &str =
    "expected a List element type (Nat, Int, Bool, Bytes, Bits, or Handle — List does not nest)";

/// What refuses `List` written with no element.
const LIST_STATES_ITS_ELEMENT: &str = "`List` is written with its element type, as in `List(Nat)`";

/// What refuses a result that no form of one begins.
const WIRE_RESULT_EXPECTED: &str = "expected a wire result: a wire type, a braced list of labelled wire types, `{}` for none, or `Never`";

/// What refuses `()` where a result stands.
const WIRE_RESULT_IS_A_TYPE: &str =
    "`()` is the unit value, and a result is a type: a row that answers nothing is written `-> {}`";

// An `List` element type — the wire grammar minus `List` itself. Splitting it out of `parse_wire_type` is what makes `List(List(T))` unwritable: codegen forces and embeds exactly one level at the host boundary, so a second one would silently hand the host rope structs instead of flat arrays.
fn parse_wire_leaf<'a>() -> Parser<'a, WireLeaf> {
    refuse_qualified_wire_name()
        .and_keep(mark().and(parse_identifier().map_err(WIRE_LEAF_EXPECTED)))
        .flat_map(|(start, name)| match name {
            "Nat" => pure(WireLeaf::Nat),
            "Int" => pure(WireLeaf::Int),
            "Bool" => pure(WireLeaf::Bool),
            "Bytes" => pure(WireLeaf::Bytes),
            "Bits" => pure(WireLeaf::Bits),
            "Handle" => pure(WireLeaf::Handle),
            // A word read here is an element type or nothing, so the refusal commits and is the diagnosis. Left to backtrack it reached no reader at all: the enclosing list took the failure for an empty one and the `)` after it complained instead, naming a paren for a mistake about a type.
            other => commit(fail_from(
                &start,
                format!("{WIRE_LEAF_EXPECTED}, found '{other}'"),
            )),
        })
}

// A wire type is one of a closed set of bare words, so a path spelled with `/` is refused by name rather than left to `parse_identifier`, which stops at the slash and lets the enclosing paren take the blame.
fn refuse_qualified_wire_name<'a>() -> Parser<'a, ()> {
    commit(not_ahead("/").map_err("a wire type is written bare: `Nat` rather than `/std/Nat`, since the wire grammar is its own closed vocabulary and resolves no names"))
}

// One of the wire types, by its own closed grammar — not an ordinary Curios type, so this needs no name resolution: `Nat`/`Int`/`Bool`/`Byte`/`Flt`/`Bytes`/`Bits`/`Handle` are literal keywords here, and `List(T)` takes a leaf.
pub(super) fn parse_wire_type<'a>() -> Parser<'a, WireType> {
    refuse_qualified_wire_name()
        .and_keep(mark().and(parse_identifier().map_err(WIRE_TYPE_EXPECTED)))
        .flat_map(|(start, name)| match name {
            "Nat" => pure(WireType::Nat),
            "Int" => pure(WireType::Int),
            "Bool" => pure(WireType::Bool),
            "Byte" => pure(WireType::Byte),
            "Flt" => pure(WireType::Flt),
            "Bytes" => pure(WireType::Bytes),
            "Bits" => pure(WireType::Bits),
            "Handle" => pure(WireType::Handle),
            // `List` is read, so its element follows or nothing does. Left to backtrack, an operand list came back empty and its `)` complained where the word stands.
            "List" => commit(
                parse_literal("(")
                    .map_err(LIST_STATES_ITS_ELEMENT)
                    .and_keep(parse_wire_leaf)
                    .and_drop(parse_literal(")")),
            )
            .map(WireType::List),
            // As in [`parse_wire_leaf`]: a word here is a wire type or nothing. `parse_identifier` failing on a non-identifier is what keeps the empty parameter list `() -> T` working — the arm is never reached at the `)`.
            other => commit(fail_from(
                &start,
                format!("{WIRE_TYPE_EXPECTED}, found '{other}'"),
            )),
        })
}

/// What refuses `_` where a result's field is labelled: a tuple type reads `_` as no label, and a row's results are read by their labels alone.
const WIRE_FIELD_IS_LABELLED: &str = "a result's field is read by its label, so `_` cannot stand for one: write a name, as in `status: Nat`";

/// A field of a braced result as it is read: its label and type, beside the report that refuses the label as one an earlier field has, made where the label stands for [`wire_fields`] to raise.
type WireField = ((String, WireType), ParserError);

// The label of a result's field, read as a tuple type's is — `documentation/syntax.md` makes a tuple's labels part of its identity — so a name, no keyword and no `_`, each refused at the word: left to the declaration's type, the refusal is the elaborator's and holds no position.
//
// Inside a result's braces a word is a field's label or nothing, so one with no `:` after it is refused as a field, at the word, where a backtrack would leave the closing brace to name itself for a mistake about a field.
fn parse_wire_label<'a>() -> Parser<'a, (String, ParserError)> {
    mark()
        .and(parse_identifier_raw)
        .flat_map(|(start, word)| match word {
            "_" => commit(fail_from(&start, WIRE_FIELD_IS_LABELLED)),
            word if is_keyword(word) => commit(fail_from(&start, reserved_keyword(word))),
            word => look_ahead(parse_whitespace().and_keep(take_exact(":")))
                .or(commit(fail_from(
                    &start,
                    format!("a result's field is written `label: type`, as in `status: Nat`; '{word}' is read as a label and no `:` follows it"),
                )))
                .and_keep(refusal(
                    &start,
                    format!("a result names each of its fields once: '{word}' labels an earlier one"),
                ))
                .map(move |repeated| (word.to_string(), repeated)),
        })
        .and_drop(parse_whitespace)
}

// One labelled field of a braced result: `status: Nat`. The label is kept as written, so nothing may invent one or reorder the field it names.
fn parse_wire_field<'a>() -> Parser<'a, WireField> {
    parse_wire_label()
        .and_drop(parse_literal(":"))
        .and(commit(parse_wire_type))
        .map(|((label, repeated), type_)| ((label, type_), repeated))
}

// The results a braced field list denotes, in the order written, or the report of why it denotes none: a tuple type's labels are part of its identity, so this may not drop the label a single field carries, nor take a label twice. `start` is where the fields begin, so a field standing alone is refused where it is written.
fn wire_fields<'a>(start: &Mark, fields: Vec<WireField>) -> Parser<'a, WireResults> {
    let mut read: Vec<(String, WireType)> = Vec::new();
    for ((label, type_), repeated) in fields {
        if read.iter().any(|(earlier, _)| *earlier == label) {
            return raise(repeated);
        }

        read.push((label, type_));
    }

    let mut read = read.into_iter();
    match (read.next(), read.next()) {
        (None, _) => pure(WireResults::none()),
        // A row forwards one result through as itself, so `{x: T}` would name a one-field tuple no row can carry — and the label would be dropped rather than kept, which is a different type from the one written.
        (Some((label, _)), None) => fail_from(
            start,
            format!(
                "a result is spelled bare rather than in braces when there is one of it: write the type itself, not `{{{label}: …}}`"
            ),
        ),
        (Some(first), Some(second)) => pure(WireResults::record(first, second, read.collect())),
    }
}

// What a call comes back with: a bare wire type, a braced field list naming the tuple type the row yields, or `Never`. Each form is told by how it begins, and past the arrow one stands or nothing does, so where none begins the refusal says what one is, rather than leaving the first alternative to name its brace.
fn parse_wire_return<'a>() -> Parser<'a, WireReturn> {
    parse_wire_record()
        .map(WireReturn::Answers)
        .or(refuse_wire_unit_value())
        .or(parse_wire_word())
        .or(commit(fail(WIRE_RESULT_EXPECTED)))
}

// A braced result: `{}`, no result at all — the unit *type*, which is what a result position holds — or the fields of the tuple type the row yields. The brace discriminates it, so what follows commits.
fn parse_wire_record<'a>() -> Parser<'a, WireResults> {
    parse_literal("{").and_keep(commit(
        mark()
            .and(sep_by0_trailing(parse_wire_field, || parse_literal(",")))
            // The list is judged once it is seen to close: before that, a field standing alone may be one whose separator is missing, which is the brace's to report.
            .and_drop(look_ahead(parse_literal("}")))
            .flat_map(|(start, fields)| wire_fields(&start, fields))
            .and_drop(parse_literal("}")),
    ))
}

// `()` where a result stands: the unit value, which never stands here. Refused by name, since `{}` is what its writer meant.
fn refuse_wire_unit_value<'a>() -> Parser<'a, WireReturn> {
    mark()
        .and_drop(parse_literal("("))
        .and_drop(take_exact(")"))
        .flat_map(|start| commit(fail_from(&start, WIRE_RESULT_IS_A_TYPE)))
}

// A return that is one word: `Never`, or a wire type. `Never` says the call does not return; it stands where a result does and is no wire type, so it stands in no operand, field or element. The word is looked at before either form reads it, so any other is the wire type's to read, and to refuse in its own sentence.
fn parse_wire_word<'a>() -> Parser<'a, WireReturn> {
    look_ahead(parse_identifier_raw)
        .or(pure(""))
        .flat_map(|word| match word {
            "Never" => parse_identifier().map(|_| WireReturn::Never),
            _ => parse_wire_type()
                .map(WireResults::single)
                .map(WireReturn::Answers),
        })
}

// `(T, T, ...) -> R`: the operands a row takes, none included, and what its call answers. An operand is its wire type alone and so is a bare result, while a braced result names its fields, because a row of two or more results reaches the guest as a tuple type whose labels it projects by.
pub(super) fn parse_wire_signature<'a>() -> Parser<'a, WireSignature> {
    parse_literal("(")
        .and_keep(sep_by0_trailing(parse_wire_type, || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal("->"))
        .and(lazy(parse_wire_return))
        .map(|(params, returns)| WireSignature { params, returns })
}

// `foreign name(T, ...) -> R;` — a function with no body, bound to an implementation where the program is linked. The list follows the name as a function definition's does, with no colon: a row's signature is not its type.
pub(super) fn parse_top_foreign<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopItem> {
    declared(parse_wire_signature().and_drop(parse_literal(";"))).map(move |(label, signature)| {
        TopItem::Foreign(TopForeign {
            doc,
            vis_pub,
            label,
            signature,
        })
    })
}

pub(super) fn parse_top_mod<'a>(
    doc: Option<Doc>,
    vis_pub: bool,
    start: Mark,
) -> Parser<'a, TopItem> {
    parse_declared_label().flat_map(move |label| {
        // Named for nothing: a module declares a scope rather than a binding, and a broken item inside its body is the module's, not the binding the body's item named.
        tagging(
            None,
            many0(|| spanned(parse_top_item))
                .and_drop(parse_keyword("end"))
                .map(|items| {
                    let (spans, items) = items.into_iter().unzip();

                    Some(Module { items, spans })
                }),
        )
        .or(parse_literal(";").map(|()| None))
        // The span reaches back to the mark the dispatch took before `pub`, since the head it covers was consumed there.
        .and(mark)
        .map(move |(module, end)| {
            TopItem::Mod(TopMod {
                doc,
                span: Some(start.to(&end)),
                vis_pub,
                label,
                module,
            })
        })
    })
}

// Like `parse_name`, but additionally accepts an empty absolute path. The leading `/` is only consumed when followed by an identifier — so for `use /{X};` the path is empty-abs (consumes nothing) and `/` is left for `parse_use_group` to consume as its separator. Raw segments, and no trailing-whitespace consumption at all: the group separator follows the path tightly, so `use /std /{X};` is refused like any other whitespace inside a path.
pub(super) fn parse_use_path<'a>() -> Parser<'a, Name> {
    spanned(
        take_exact("/")
            .and_keep(parse_identifier_raw)
            .and(many0(|| take_exact("/").and_keep(parse_identifier_raw)))
            .map(|(first, rest)| {
                Name::new(
                    true,
                    Qualifier::from(
                        iter::once(first)
                            .chain(rest)
                            .map(str::to_string)
                            .collect::<Vec<_>>(),
                    ),
                )
            })
            .or(parse_identifier_raw()
                .and(many0(|| take_exact("/").and_keep(parse_identifier_raw)))
                .map(|(first, rest)| {
                    Name::new(
                        false,
                        Qualifier::from(
                            iter::once(first)
                                .chain(rest)
                                .map(str::to_string)
                                .collect::<Vec<_>>(),
                        ),
                    )
                }))
            .or(pure(Name::new(true, Qualifier::empty())))
            .flat_map(|name| {
                let segments = name.qualifier().segments();

                match segments.iter().any(|segment| is_keyword(segment)) {
                    true => fail(format!(
                        "path '{}' contains a reserved keyword",
                        name.qualifier().join()
                    )),
                    false => pure(name),
                }
            }),
    )
    .map(|(span, name)| name.with_span(span))
}

pub(super) fn parse_group_item<'a>() -> Parser<'a, GroupItem> {
    parse_keyword("mod")
        .and_keep(parse_declared_label)
        .map(GroupItem::Mod)
        .or(parse_keyword("let")
            .and_keep(parse_declared_label)
            .map(GroupItem::Let))
        .or(parse_declared_label().map(GroupItem::Both))
}

// The `{` is the prefix that discriminates a group, so the rest commits and owns its fault. Left recoverable, a missing `}` would fail here and fall through to `parse_use_group`'s last alternative, whose committed refusal outranks the offset — so a reader who forgot one brace would be told at length how to write the group already on their screen, with the caret on the `/` before it.
pub(super) fn parse_brace_group<'a>() -> Parser<'a, Vec<GroupItem>> {
    parse_literal("{").and_keep(commit(
        sep_by0_trailing(parse_group_item, || parse_literal(",")).and_drop(parse_literal("}")),
    ))
}

pub(super) fn parse_use_group<'a>() -> Parser<'a, UseGroup> {
    take_exact("/")
        .and_keep(parse_brace_group)
        .map(UseGroup::Named)
        .or(take_exact("/")
            .and_keep(parse_literal("*"))
            .map(|()| UseGroup::Glob))
        // The path is read, so what stands here is a group or a mistake — `use` is reserved and begins no term, and the arm that dispatched it has already committed. Left to the alternatives above, a bare `use /std/Nat;` would ask for the `/` that introduces a group and never say a group was what it wanted.
        .or(commit(fail(
            "a `use` imports through a group: `use /std/{Nat};` for one name, `use /std/{Nat, Bool};` for several, or `use /std/*;` for the exported surface — there is no bare `use path;` form",
        )))
}

// The span reaches back to the mark the dispatch took before `pub`, as a `mod`'s does, and closes on the `;` rather than on the whitespace after it: it is what a report about the whole declaration underlines.
pub(super) fn parse_top_use<'a>(vis_pub: bool, start: Mark) -> Parser<'a, TopItem> {
    parse_use_path()
        .and(parse_use_group)
        .and_drop(take_exact(";"))
        .and(mark)
        .and_drop(parse_whitespace)
        .map(move |((name, group), end)| {
            TopItem::Use(TopUse {
                span: Some(start.to(&end)),
                vis_pub,
                name,
                group,
            })
        })
}

// A payload binder, plain or `@` (implicit at the constructor function): `m : Nat` (named) or a bare type (positional), under either mark. A payload is no other grammar's, so `use` is refused where it stands.
pub(super) fn parse_induct_payload_field<'a>() -> Parser<'a, CasePayloadParam> {
    mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            Plicity::Witness => commit(fail_from(&start, PAYLOAD_TAKES_NO_USE)),
            Plicity::Explicit | Plicity::Implicit => parse_identifier()
                .and_drop(parse_literal(":"))
                .and(lazy(parse_term))
                .map(|(name, type_): (&str, Term)| (Some(name.to_string()), type_))
                .or(lazy(parse_term).map(|type_| (None, type_)))
                .map(move |(label, type_)| CasePayloadParam {
                    plicity,
                    label,
                    type_,
                }),
        })
}

pub(super) fn parse_top_induct_case<'a>() -> Parser<'a, TopCase> {
    parse_doc().flat_map(|doc| {
        // As `parse_and_head` decides: a bar makes the block this case's, `and` or `pub` hand it to the group's next member, and anything else is nothing it may document.
        let bar = match &doc {
            None => parse_literal("|"),
            Some(doc) if text_after(doc).starts_with('|') => {
                parse_literal("|").map_err(DOC_BEFORE_NOTHING)
            }
            // Recoverable: `and` hands the block to the group's next member, which reads it again.
            Some(doc) if word_after(doc) == "and" => fail(DOC_BEFORE_NOTHING),
            Some(_) => commit(fail(DOC_BEFORE_NOTHING)),
        };

        bar.and_keep(parse_declared_label)
            .and(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_induct_payload_field, || {
                        parse_literal(",")
                    }))
                    .and_drop(parse_literal(")")),
            )
            // The case target: `: (index-exprs)` — the terminal with its mandatory part (the inductive name and the parameters) elided.
            .and(
                parse_literal(":")
                    // Only a target may follow a constructor's payload, so past the `:` this owns the diagnosis; recoverable, a malformed target would be read as no target and the case list's `end` would complain at the colon.
                    .and_keep(commit(
                        parse_literal("(")
                            .and_keep(sep_by0_trailing(|| lazy(parse_term), || parse_literal(",")))
                            .and_drop(parse_literal(")")),
                    ))
                    .map(Some)
                    .or(pure(None)),
            )
            .map(
                move |((label, payload), target): ((Label, Vec<_>), Option<Vec<Term>>)| TopCase {
                    doc,
                    label,
                    payload,
                    target,
                },
            )
    })
}

// A declaration's parameter: `name : type`, `@name : type` to make it implicit at the type-constructor function (it is implicit at the value constructors either way — the mark's only job is the type constructor, where unmarked parameters are written out), or `use Concept(args)`, a premise the type names the dictionary of. A plain or `@` parameter is always named: the declaration's own types are what mention it.
pub(super) fn parse_induct_param<'a>() -> Parser<'a, FuncTypeParam> {
    mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            // A declaration's parameter list is no other grammar's, so a refused premise is raised where it stands.
            Plicity::Witness => raised(parse_premise(start)).map(|type_| FuncTypeParam {
                plicity: Plicity::Witness,
                label: None,
                type_,
            }),
            Plicity::Explicit | Plicity::Implicit => parse_identifier()
                .and_drop(parse_literal(":"))
                .and(lazy(parse_term))
                .map(move |(name, type_): (&str, Term)| FuncTypeParam {
                    plicity,
                    label: Some(name.to_string()),
                    type_,
                }),
        })
}

// A head index-telescope entry: `n : Nat` or a bare `Nat`. The name is documentary (and a dependency hook for later entries) — never in scope in the cases — so it is optional, and an index is always written, so a mark is refused where it stands.
pub(super) fn parse_induct_index<'a>() -> Parser<'a, (Option<String>, Term)> {
    mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            Plicity::Explicit => parse_identifier()
                .and_drop(parse_literal(":"))
                .and(lazy(parse_term))
                .map(|(name, ty): (&str, Term)| (Some(name.to_string()), ty))
                .or(lazy(parse_term).map(|ty| (None, ty))),
            Plicity::Implicit | Plicity::Witness => commit(fail_from(&start, INDEX_TAKES_NO_MARK)),
        })
}

/// A parsed inductive head arity: the index telescope (each binder optionally named) and the sort it lands in.
type InductArity = (Vec<(Option<String>, Term)>, bool, Term);

/// A declaration-local result sort, with an independent representation visibility marker. This parser is deliberately not used for ordinary sort positions: `pub Type` and `pub Prop` are not terms.
fn parse_representation_sort<'a>() -> Parser<'a, (bool, Term)> {
    parse_pub().and(parse_sort)
}

// The head's arity after the `:` — either an index telescope landing in a sort, `(n : Nat) -> Prop`, or a bare sort, `Prop`. The sort is mandatory: an index telescope must state where it lands (`-> Sort`), and a sortless head is a parse error, never an implicit `Type`.
pub(super) fn parse_induct_arity<'a>() -> Parser<'a, InductArity> {
    parse_literal("(")
        .and_keep(sep_by0_trailing(parse_induct_index, || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .and(parse_literal("->").and_keep(parse_representation_sort))
        .map(|(indices, (rep_pub, sort))| (indices, rep_pub, sort))
        .or(parse_representation_sort().map(|(rep_pub, sort)| (Vec::new(), rep_pub, sort)))
}

pub(super) fn parse_top_induct_body<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopInduct> {
    declared(
        parse_literal("(")
            .and_keep(sep_by1_trailing(parse_induct_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .or(pure(vec![]))
            // The head's arity: `: (n : Nat) -> Prop` or `: Prop`. The sort is required — there is no implicit `Type`.
            .and(parse_literal(":").and_keep(parse_induct_arity))
            .and(many0(parse_top_induct_case)),
    )
    .flat_map(
        move |(label, ((params, (indices, rep_pub, result_sort)), cases))| {
            // Targets are required on every case iff the head declares indices, with arity equal to the index telescope's.
            for case in &cases {
                match (&case.target, indices.len()) {
                    (None, 0) => {}
                    (None, _) => {
                        return fail(format!(
                            "case '{}' of indexed inductive '{label}' must state its \
                             index target: `{}(...) : (...)`",
                            case.label, case.label,
                        ));
                    }
                    (Some(_), 0) => {
                        return fail(format!(
                            "case '{}' states an index target, but inductive '{label}' \
                             declares no indices",
                            case.label,
                        ));
                    }
                    (Some(target), arity) if target.len() != arity => {
                        return fail(format!(
                            "case '{}' of inductive '{label}' states {} index \
                             expression(s), but the head declares {arity}",
                            case.label,
                            target.len(),
                        ));
                    }
                    _ => {}
                }
            }

            pure(TopInduct {
                doc,
                vis_pub,
                rep_pub,
                label,
                params,
                indices,
                result_sort,
                cases,
            })
        },
    )
}

pub(super) fn parse_top_induct<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopItem> {
    parse_top_induct_body(doc, vis_pub)
        .flat_map(|first| {
            // Named for the head's family, as a `let` group is for its first member — the `end` included.
            let tag = first.label.to_string();
            tagging(
                Some(tag),
                many0(|| {
                    parse_and_head().flat_map(|(doc, vis_pub)| parse_top_induct_body(doc, vis_pub))
                })
                .and_drop(parse_keyword("end")),
            )
            .map(move |rest| (first, rest))
        })
        .map(|(first, rest)| TopItem::Induct(iter::once(first).chain(rest).collect()))
}

/// A universe sort: exactly `Type` or `Prop`. The result sort of a struct or an inductive head is always one of these two — the only universes — so the sort position parses this targeted form rather than a generic `lazy(parse_term)`. A generic term parser is both too loose and, for a struct, greedily eats the `{` opening the field block.
pub(super) fn parse_sort<'a>() -> Parser<'a, Term> {
    parse_prop().or(parse_type)
}

// One field of a `struct`: its documentation comment, then its mark and the Σ-type field grammar, which is what follows `@` as it is what a plain field is. A structure's field list is no other grammar's, so a `use` member is refused where it stands. A documentation comment before the closing brace documents nothing and says so.
fn parse_struct_field<'a>() -> Parser<'a, StructField> {
    parse_doc().flat_map(|doc| {
        documented(&doc, not_ahead("}"))
            .and_keep(mark().and(parse_mark))
            .flat_map(|(start, plicity)| match plicity {
                Plicity::Witness => commit(fail_from(&start, STRUCT_FIELD_TAKES_NO_USE)),
                Plicity::Explicit | Plicity::Implicit => {
                    parse_field_member().map(move |param| (plicity, param))
                }
            })
            .map(move |(plicity, param)| StructField {
                doc,
                plicity,
                param,
            })
    })
}

// One structure of a `struct` item, after its `pub` and keyword: the name, the parameters, the result sort with its own `pub`, and the fields.
fn parse_struct_member<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopStruct> {
    declared(
        parse_literal("(")
            .and_keep(sep_by1_trailing(parse_induct_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .or(pure(vec![]))
            // The result sort: `: Type` or `: Prop` after the parameters. Required.
            .and(parse_literal(":").and_keep(parse_representation_sort))
            .and_drop(parse_literal("{"))
            .and(sep_by0_trailing(parse_struct_field, || parse_literal(",")))
            .and_drop(parse_literal("}")),
    )
    .map(
        move |(label, ((params, (rep_pub, result_sort)), fields))| TopStruct {
            doc,
            vis_pub,
            rep_pub,
            label,
            params,
            result_sort,
            fields,
        },
    )
}

// A `struct` item: one structure, or a `struct A … and B …` group whose fields name one another. Each member takes its own `pub`, before `struct` for the first and before `and` for the rest.
pub(super) fn parse_top_struct<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopItem> {
    parse_struct_member(doc, vis_pub)
        .flat_map(|first| {
            let tag = first.label.to_string();
            tagging(
                Some(tag),
                many0(|| {
                    parse_and_head().flat_map(|(doc, vis_pub)| parse_struct_member(doc, vis_pub))
                }),
            )
            .map(move |rest| (first, rest))
        })
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Struct)
}

// A concept field: `label : term`, the signature sugar `label(params) -> term` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar (mirroring top-level `let`'s function sugar) — or `use Concept(args)`, a superclass edge, whose type must be a concept application, checked at lowering. A method takes no mark.
pub(super) fn parse_concept_field<'a>() -> Parser<'a, ConceptField> {
    let method = parse_declared_label()
        .and(
            parse_literal("(")
                .and_keep(sep_by0_trailing(parse_func_type_param, || {
                    parse_literal(",")
                }))
                .and_drop(parse_literal(")"))
                .and_drop(parse_literal("->"))
                // Past the arrow the list is this sugar's, so a member it refuses is its own to report.
                .flat_map(members)
                // As in `parse_tuple_type_field`: past the `->` or the `:` a type must follow, so the refusal is this one rather than the `}` the field list falls back to.
                .and(commit(lazy(parse_term)))
                .map(|(params, output): (Vec<FuncTypeParam>, Term)| (Some(params), output))
                .or(parse_literal(":")
                    .and_keep(commit(lazy(parse_term)))
                    .map(|type_| (None, type_))),
        )
        .map(|(label, (func_params, type_)): (Label, _)| ConceptField {
            doc: None,
            label: Some(label),
            func_params,
            type_,
        });

    // A concept's field list is no other grammar's, so a refused edge or a marked method is raised where it stands.
    let field = mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            Plicity::Witness => raised(parse_premise(start)).map(|type_| ConceptField {
                doc: None,
                label: None,
                func_params: None,
                type_,
            }),
            Plicity::Implicit => commit(fail_from(&start, CONCEPT_FIELD_TAKES_NO_IMPLICIT)),
            Plicity::Explicit => method,
        });

    // The documentation comment is read first, and a field's spellings then share it; one before the closing brace documents nothing and says so.
    parse_doc().flat_map(|doc| {
        documented(&doc, not_ahead("}"))
            .and_keep(field)
            .map(move |field| ConceptField { doc, ..field })
    })
}

// One concept of a `concept` item, after its `pub` and keyword — the struct member's shape with concept fields.
fn parse_concept_member<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopConcept> {
    declared(
        parse_literal("(")
            .and_keep(sep_by1_trailing(parse_induct_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .or(pure(vec![]))
            // The representation sort: `: pub Type`, `: Type`, `: pub Prop`, or `: Prop` after the parameters. Required, like a struct's.
            .and(parse_literal(":").and_keep(parse_representation_sort))
            .and_drop(parse_literal("{"))
            .and(sep_by0_trailing(parse_concept_field, || parse_literal(",")))
            .and_drop(parse_literal("}")),
    )
    .map(
        move |(label, ((params, (rep_pub, result_sort)), fields))| TopConcept {
            doc,
            vis_pub,
            rep_pub,
            label,
            params,
            result_sort,
            fields,
        },
    )
}

// A `concept` item: one concept, or a `concept A … and B …` group whose method types name one another's dictionaries. Each member takes its own `pub`, as a `struct` group's do.
pub(super) fn parse_top_concept<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopItem> {
    parse_concept_member(doc, vis_pub)
        .flat_map(|first| {
            let tag = first.label.to_string();
            tagging(
                Some(tag),
                many0(|| {
                    parse_and_head().flat_map(|(doc, vis_pub)| parse_concept_member(doc, vis_pub))
                }),
            )
            .map(move |rest| (first, rest))
        })
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Concept)
}

// A witness field: `label = term`, or the definition sugar `label(params) = term` — the tuple-field grammar with the label mandatory, kept as written in the AST node (`func_params`); `into_core` undoes the sugar.
pub(super) fn parse_witness_field<'a>() -> Parser<'a, WitnessField> {
    parse_tuple_field_prefix()
        // As in `parse_tuple_field`: past the `=` the implementation must follow, so a mistake in it is the diagnosis rather than the `}` the entry list falls back to.
        .and(commit(lazy(parse_term)))
        .map(|((label, func_params), value)| WitnessField {
            label,
            func_params,
            value,
        })
}

/// What refuses a `use` entry in a witness body.
const WITNESS_NEVER_FILLS_A_SUPERCLASS: &str = "a witness never writes a superclass slot: resolution fills it, so every path to the superclass finds the one registered witness";

// A witness-body entry is an implementation field. A concept literal also admits a `use <term>` fill for one of the concept's `use`-marked fields; a witness does not, because a registered witness carrying a superclass other than the registered one answers `==` one way through the table and another way through its own projection. The word is read and refused by name rather than left to fail as a missing label, which would say nothing about why.
fn parse_witness_entry<'a>() -> Parser<'a, WitnessField> {
    mark()
        .and_drop(parse_keyword("use"))
        .flat_map(|start| commit(fail_from(&start, WITNESS_NEVER_FILLS_A_SUPERCLASS)))
        .or(parse_witness_field)
}

// One witness: `Concept(args) { … }`, or `(params) => Concept(args) { … }` with a nonempty telescope. The separator makes the parameterized form's terminal concept application explicit; an empty telescope must use the bare form instead. The brace block holds the entries, or `..` alone — the derived form, whose body the compiler writes — and either may follow either head. `..` is no entry: nothing stands beside it, and past it the block closes.
fn parse_witness_member<'a>(doc: Option<Doc>) -> Parser<'a, TopWitness> {
    parse_literal("(")
        .and_keep(sep_by1_trailing(parse_func_sugar_param, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal("=>"))
        // Past the separator this is a witness's telescope, so a member it refuses is its own to report.
        .flat_map(members)
        .or(pure(vec![]))
        .and(parse_name)
        // Past the concept's name this is a witness and nothing else, so the fall-through ends here as `test`'s ends at its label. `satisfy Name` is two names in a row and so no term, and the call `satisfy(…)` the dispatch keeps this arm recoverable for never reaches this point: its argument list is no telescope, and a `(` is no name.
        .and(commit(
            parse_literal("(")
                .and_keep(sep_by0_trailing(parse_apply_argument, || {
                    parse_literal(",")
                }))
                .and_drop(parse_literal(")"))
                .map(Some)
                .or(pure(None))
                .and(
                    parse_literal("{").and_keep(
                        parse_literal("..")
                            .and_keep(commit(parse_literal("}")))
                            .map(|()| None)
                            .or(sep_by0_trailing(parse_witness_entry, || parse_literal(","))
                                .and_drop(parse_literal("}"))
                                .map(Some)),
                    ),
                ),
        ))
        .map(move |((params, concept), (args, body))| TopWitness {
            doc,
            params,
            concept,
            args,
            body,
        })
}

/// What refuses a `pub` on a witness, wherever in a group it is written.
const WITNESS_NEVER_PUB: &str =
    "a witness is never `pub`: it is reached by resolution, not by name";

// A witness declaration is anonymous: `satisfy Concept(args) { … }`, or a group `satisfy C(A) { … } and D(B) { … }` of witnesses that resolve through one another. The keyword is *not* a commit point: `satisfy` is contextual, so a program's tail may call a function of that name, and the dispatch keeps this arm recoverable for it. A witness has no `pub`, so neither does an `and` member, and one written here is refused by name as a test's is.
pub(super) fn parse_top_witness<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopItem> {
    match vis_pub {
        true => fail(WITNESS_NEVER_PUB),
        false => pure(()),
    }
    .and_keep(parse_witness_member(doc))
    .and(many0(|| {
        parse_and_head().flat_map(|(doc, vis_pub)| match vis_pub {
            true => fail(WITNESS_NEVER_PUB),
            false => parse_witness_member(doc),
        })
    }))
    .map(|(first, rest)| iter::once(first).chain(rest).collect())
    .map(TopItem::Witness)
}

/// What a head that names no item reports. Lists the heads rather than the offending word alone, because the reader's next question is what they were allowed to write.
const NOT_A_TOP_LEVEL_ITEM: &str = "Expected a top-level item: one of 'mod', 'use', 'concept', 'satisfy', 'test', 'let', 'induct', 'struct' or 'foreign'";

/// One top-level item, dispatched on the head it begins with.
///
/// **A dispatch rather than nine ordered alternatives, because the heads are disjoint.** Every item is led by one word, so a choice chain would re-read the same optional `pub` and identifier once per arm and then report whichever arm happened to read furthest — which, since [`parse_keyword`] rejects only *after* consuming the identifier, is always the first, so every unrecognized head would blame `mod`. Reading the head once and switching on it makes the arm that owns the diagnosis the arm that produced it.
///
/// **A head commits when it is reserved and cannot begin a term**, which is exactly `mod`, `use`, `induct`, `struct` and `foreign`. Nothing else may be written in their place, so once one is read its arm owns the error and [`commit`] stops an enclosing choice from backtracking into a vaguer one.
///
/// **The other four fall through, and the language decides which.** A failure inside one of these arms backtracks like any other, which is what lets the item loop end and a program's tail begin where an item does not. `concept`, `satisfy` and `test` are contextual words — `documentation/syntax.md` keeps them ordinary identifiers outside a declaration position, so one of them here may really be a program's tail calling a function of that name. `let` is reserved but shared with the term grammar: a top-level `let` requires an annotation, and `let x = 1; tail` has to fall through to a local `let`. It falls through only as far as the `=`: past `let name : T =` the local reading is the same text failing the same way, so the body commits and a broken body is the item's own diagnosis, which is what lets the item loop recover past it. An unrecognized head is recoverable for the same reason — it is how the item loop terminates before a program's tail begins.
///
/// **How far the fall-through reaches is the arm's to say, and `test` says: as far as the label.** The rest of the grammar catches the prefix that discriminates an alternative and lets the tail commit — `parse_func` catches `(…) =>` and reports a broken body itself. `test` may be a program's tail, but `test name` is two names in a row and so no term at all, so [`parse_top_test`] lets its own refusal backtrack no further than the label and this arm hands it the item uncaught: past the label, a mistake inside a test is the diagnosis. `concept` and `satisfy` end theirs inside themselves for the same reason. Neither can end it at the keyword — `satisfy (…) => …` and a call `satisfy(…)` share their next token — so each commits at the first point its own grammar could be nothing else: `satisfy` at the concept's name, since `satisfy Name` is two names in a row and a `(` is no name, and a `concept` field at the `:` or `->` that introduces its type.
///
/// **A `pub` in front makes every arm commit, because it removes the fall-through those refusals stay recoverable for.** `pub` is a keyword, so no term begins with one: after reading it there is no tail for a failed item to become, and an unrecognized head after it names no item rather than ending the item loop. Leaving those arms recoverable would throw the diagnosis away where it matters most. A module recovers it — `Module::parse_items_end` re-runs the item parser once input remains — but an entrypoint's grammar runs `parse_term` instead, which tries a local `let` at the `pub`, fails one token in, and wins [`Parser::or`]'s furthest-failure tie-break over the real error the arm has just let backtrack. Every mistake inside a `pub let` in a program would then report `Expected keyword 'let', obtained 'pub'` against the `let`, and the refusals `parse_top_witness` and `parse_top_test` write by hand for a `pub` they cannot accept would never reach a reader.
///
/// **A documentation comment in front makes every arm commit too, and for the same reason.** Once a `---` block is read there is nothing else it can be: a program's tail is not documented, an unrecognized head after it names no item, and the block itself is the diagnosis. The head is then read committed as well, so a block at the end of a file reports itself rather than the item loop's generic end.
pub(crate) fn parse_top_item<'a>() -> Parser<'a, TopItem> {
    parse_doc().flat_map(|doc| {
        let head = parse_pub().and(parse_identifier_raw);
        let head = match &doc {
            Some(_) => commit(head.map_err(DOC_BEFORE_NOTHING)),
            None => head,
        };

        mark().and(head).flat_map(move |(start, (vis_pub, head))| {
            // Recoverable only where a failed item may still be a program's tail, which a `pub` or a documentation comment rules out.
            let documented = doc.is_some();
            let fallible = |parser| match vis_pub || documented {
                true => commit(parser),
                false => parser,
            };

            // The head is read *raw*, so an unrecognized one is reported against the word itself rather than wherever the whitespace after it ended — which for a one-word line is the next line, or end of input. Every arm therefore consumes that whitespace itself.
            let body = match head {
                "mod" => commit(parse_top_mod(doc, vis_pub, start)),
                "use" => match documented {
                    true => commit(fail(DOC_BEFORE_USE)),
                    false => commit(parse_top_use(vis_pub, start)),
                },
                "induct" => commit(parse_top_induct(doc, vis_pub)),
                "struct" => commit(parse_top_struct(doc, vis_pub)),
                "foreign" => commit(parse_top_foreign(doc, vis_pub)),
                "concept" => fallible(parse_top_concept(doc, vis_pub)),
                "satisfy" => fallible(parse_top_witness(doc, vis_pub)),
                // The fall-through ends at the label, so this arm hands the item over uncaught rather than through `fallible`, whose commitment reaches past it. A `pub` commits the whole arm, which `parse_top_test` refuses by name.
                "test" => match (documented, vis_pub) {
                    (true, _) => commit(fail(DOC_BEFORE_TEST)),
                    (false, true) => commit(parse_top_test(vis_pub)),
                    (false, false) => parse_top_test(vis_pub),
                },
                "let" => fallible(parse_top_let(doc, vis_pub)),
                _ => {
                    return fallible(fail_from(
                        &start,
                        match documented {
                            true => DOC_BEFORE_NOTHING,
                            false => NOT_A_TOP_LEVEL_ITEM,
                        },
                    ));
                }
            };

            parse_whitespace().and_keep(body)
        })
    })
}
