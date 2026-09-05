use super::*;

pub(super) fn parse_pub<'a>() -> Parser<'a, bool> {
    catch(parse_keyword("pub")).map(|()| true).or(pure(false))
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
            return catch(head).map(|vis_pub| (None, vis_pub));
        };

        match word_after(&doc) {
            "and" => head
                .map_err(DOC_BEFORE_NOTHING)
                .map(move |vis_pub| (Some(doc), vis_pub)),
            "" | "end" => fail(DOC_BEFORE_NOTHING),
            _ => catch(fail(DOC_BEFORE_NOTHING)),
        }
    })
}

/// `parser`, refused with the documentation diagnosis when `doc` is present and the parser fails — for a member's head token, whose absence after a documentation comment is exactly that mistake.
fn documented<'a, T: 'a>(doc: &Option<Doc>, parser: Parser<'a, T>) -> Parser<'a, T> {
    match doc {
        Some(_) => parser.map_err(DOC_BEFORE_NOTHING),
        None => parser,
    }
}

// A top-level `let` item: one definition, or the group `let f … and g …;`. Each member takes its own `pub` — before `let` for the first, before `and` for each later one — and one `;` terminates the whole item.

// A `test` declaration: `test name(params) = body;`. The parentheses are the function sugar written out — required, holding the telescope a `let`'s signature holds: empty for the harness's nullary test, a parameter list for a property. `pub` is refused by name: a test's identifier is its report line, not an export. Like `satisfy`, `test` stays a contextual word everywhere else.
pub(super) fn parse_top_test<'a>(vis_pub: bool) -> Parser<'a, TopItem> {
    match vis_pub {
        true => fail("a test is never `pub`: its name is its report line, not an export"),
        false => pure(()),
    }
    .and_keep(parse_label())
    .and_drop(parse_literal("("))
    .and(sep_by0_trailing(parse_func_sugar_param, || {
        parse_literal(",")
    }))
    .and_drop(parse_literal(")"))
    .and_drop(parse_literal("="))
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
    .map(|((label, params), body)| {
        TopItem::Test(TopTest {
            label,
            params,
            body,
        })
    })
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
        .and(many0(move || {
            parse_and_head().flat_map(move |(doc, vis_pub)| member(doc, vis_pub))
        }))
        .and_drop(parse_literal(";"))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Let)
}

// An `List` element type — the wire grammar minus `List` itself. Splitting it out of `parse_wire_type` is what makes `List(List(T))` unwritable: codegen forces and embeds exactly one level at the host boundary, so a second one would silently hand the host rope structs instead of flat arrays.
fn parse_wire_leaf<'a>() -> Parser<'a, WireLeaf> {
    parse_identifier().flat_map(|name| match name {
        "Nat" => pure(WireLeaf::Nat),
        "Int" => pure(WireLeaf::Int),
        "Bool" => pure(WireLeaf::Bool),
        "Bytes" => pure(WireLeaf::Bytes),
        "Handle" => pure(WireLeaf::Handle),
        other => fail(format!(
            "expected a List element type (Nat, Int, Bool, Bytes, or Handle — List does not nest), found '{other}'"
        )),
    })
}

// One of the six wire types, by its own closed grammar — not an ordinary Curios type, so this needs no name resolution: `Nat`/`Int`/`Bool`/`Bytes`/`Handle` are literal keywords here, and `List(T)` takes a leaf.
pub(super) fn parse_wire_type<'a>() -> Parser<'a, WireType> {
    parse_identifier().flat_map(|name| match name {
        "Nat" => pure(WireType::Nat),
        "Int" => pure(WireType::Int),
        "Bool" => pure(WireType::Bool),
        "Bytes" => pure(WireType::Bytes),
        "Handle" => pure(WireType::Handle),
        "List" => catch(parse_literal("("))
            .and_keep(parse_wire_leaf())
            .and_drop(parse_literal(")"))
            .map(WireType::List),
        other => fail(format!(
            "expected a wire type (Nat, Int, Bool, Bytes, Handle, or List(...)), found '{other}'"
        )),
    })
}

// `(T, T, ...) -> T` (a foreign function) or a bare `T` (a zero-argument foreign, like `host_ops`'s `clock_wall`). Params carry no surface label — `a0`, `a1`, … name them positionally; the single result is unnamed (`_`), since a `foreign` declaration has no surface syntax for a named record result the way `/sys/Handle`'s Rust-side rows do.
pub(super) fn parse_wire_signature<'a>() -> Parser<'a, WireSignature> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0_trailing(parse_wire_type, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_wire_type))
    .map(|(params, output)| WireSignature {
        params: params
            .into_iter()
            .enumerate()
            .map(|(index, type_)| (format!("a{index}"), type_))
            .collect(),
        results: WireResults::single("_".to_string(), output),
    })
    .or(parse_wire_type().map(|output| WireSignature {
        params: vec![],
        results: WireResults::single("_".to_string(), output),
    }))
}

// `foreign name : T;` — a name and a wire signature with no body, bound to a host-provided implementation at link time. Mirrors `parse_top_let`, but ends after the signature instead of parsing `= body`.
pub(super) fn parse_top_foreign<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopItem> {
    parse_label()
        .and_drop(parse_literal(":"))
        .and(parse_wire_signature())
        .and_drop(parse_literal(";"))
        .map(move |(label, signature)| {
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
    parse_label().flat_map(move |label| {
        catch(
            many0(parse_top_item)
                .and_drop(parse_keyword("end"))
                .map(|items| Some(Module { items })),
        )
        .or(parse_literal(";").map(|()| None))
        // The span reaches back to the mark the dispatch took before `pub`, since the head it covers was consumed there.
        .and(mark())
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
        catch(
            take_exact("/")
                .and_keep(parse_identifier_raw())
                .and(many0(|| {
                    catch(take_exact("/").and_keep(parse_identifier_raw()))
                })),
        )
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
        .or(catch(parse_identifier_raw().and(many0(|| {
            catch(take_exact("/").and_keep(parse_identifier_raw()))
        })))
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
    catch(parse_keyword("mod").and_keep(parse_label()))
        .map(GroupItem::Mod)
        .or(catch(parse_keyword("let").and_keep(parse_label())).map(GroupItem::Let))
        .or(parse_label().map(GroupItem::Both))
}

pub(super) fn parse_brace_group<'a>() -> Parser<'a, Vec<GroupItem>> {
    catch(parse_literal("{"))
        .and_keep(sep_by0_trailing(parse_group_item, || parse_literal(",")))
        .and_drop(parse_literal("}"))
}

pub(super) fn parse_use_group<'a>() -> Parser<'a, UseGroup> {
    catch(take_exact("/").and_keep(parse_brace_group()))
        .map(UseGroup::Named)
        .or(catch(take_exact("/").and_keep(parse_literal("*"))).map(|()| UseGroup::Glob))
}

// The span reaches back to the mark the dispatch took before `pub`, as a `mod`'s does, and closes on the `;` rather than on the whitespace after it: it is what a report about the whole declaration underlines.
pub(super) fn parse_top_use<'a>(vis_pub: bool, start: Mark) -> Parser<'a, TopItem> {
    parse_use_path()
        .and(parse_use_group())
        .and_drop(take_exact(";"))
        .and(mark())
        .and_drop(parse_whitespace())
        .map(move |((name, group), end)| {
            TopItem::Use(TopUse {
                span: Some(start.to(&end)),
                vis_pub,
                name,
                group,
            })
        })
}

// A payload binder: `@m : Nat` (named, implicit at the constructor function), `m : Nat` (named), or a bare type (positional). Plicity's `@` (on the name) requires a name — a positional binder has nothing for a later type or the target to mention.
pub(super) fn parse_induct_payload_field<'a>() -> Parser<'a, CasePayloadParam> {
    catch(
        parse_plicity()
            .and(parse_identifier())
            .and_drop(parse_literal(":")),
    )
    .and(lazy(parse_term))
    .map(
        |((plicity, name), type_): ((Plicity, &str), Term)| CasePayloadParam {
            plicity,
            label: Some(name.to_string()),
            type_,
        },
    )
    .or(lazy(parse_term).map(|type_| CasePayloadParam {
        plicity: Plicity::Explicit,
        label: None,
        type_,
    }))
}

pub(super) fn parse_top_induct_case<'a>() -> Parser<'a, TopCase> {
    parse_doc().flat_map(|doc| {
        // As `parse_and_head` decides: a bar makes the block this case's, `and` or `pub` hand it to the group's next member, and anything else is nothing it may document.
        let bar = match &doc {
            None => parse_literal("|"),
            Some(doc) if text_after(doc).starts_with('|') => {
                parse_literal("|").map_err(DOC_BEFORE_NOTHING)
            }
            Some(doc) if word_after(doc) == "and" => catch(fail(DOC_BEFORE_NOTHING)),
            Some(_) => fail(DOC_BEFORE_NOTHING),
        };

        bar.and_keep(parse_identifier())
            .and(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_induct_payload_field, || {
                        parse_literal(",")
                    }))
                    .and_drop(parse_literal(")")),
            )
            // The case target: `: (index-exprs)` — the terminal with its mandatory part (the inductive name and the parameters) elided.
            .and(
                catch(parse_literal(":"))
                    .and_keep(parse_literal("("))
                    .and_keep(sep_by0_trailing(|| lazy(parse_term), || parse_literal(",")))
                    .and_drop(parse_literal(")"))
                    .map(Some)
                    .or(pure(None)),
            )
            .map(
                move |((label, payload), target): ((&str, Vec<_>), Option<Vec<Term>>)| TopCase {
                    doc,
                    label: label.to_string(),
                    payload,
                    target,
                },
            )
    })
}

// An inductive parameter: `name : type`, or `@name : type` to make it implicit at the type-constructor function (it is implicit at the value constructors either way — the mark's only job is the type constructor, where unmarked parameters are written out).
pub(super) fn parse_induct_param<'a>() -> Parser<'a, (Plicity, String, Term)> {
    parse_plicity()
        .and(parse_identifier())
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(|((plicity, name), ty): ((Plicity, &str), Term)| (plicity, name.to_string(), ty))
}

// A head index-telescope entry: `n : Nat` or a bare `Nat`. The name is documentary (and a dependency hook for later entries) — never in scope in the cases — so it is optional and never takes `@`.
pub(super) fn parse_induct_index<'a>() -> Parser<'a, (Option<String>, Term)> {
    catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(name, ty): (&str, Term)| (Some(name.to_string()), ty))
        .or(lazy(parse_term).map(|ty| (None, ty)))
}

/// A parsed inductive head arity: the index telescope (each binder optionally named) and the sort it lands in.
type InductArity = (Vec<(Option<String>, Term)>, bool, Term);

/// A declaration-local result sort, with an independent representation visibility marker. This parser is deliberately not used for ordinary sort positions: `pub Type` and `pub Prop` are not terms.
fn parse_representation_sort<'a>() -> Parser<'a, (bool, Term)> {
    parse_pub().and(parse_sort())
}

// The head's arity after the `:` — either an index telescope landing in a sort, `(n : Nat) -> Prop`, or a bare sort, `Prop`. The sort is mandatory: an index telescope must state where it lands (`-> Sort`), and a sortless head is a parse error, never an implicit `Type`.
pub(super) fn parse_induct_arity<'a>() -> Parser<'a, InductArity> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0_trailing(parse_induct_index, || parse_literal(",")))
            .and_drop(parse_literal(")")),
    )
    .and(parse_literal("->").and_keep(parse_representation_sort()))
    .map(|(indices, (rep_pub, sort))| (indices, rep_pub, sort))
    .or(parse_representation_sort().map(|(rep_pub, sort)| (Vec::new(), rep_pub, sort)))
}

pub(super) fn parse_top_induct_body<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopInduct> {
    parse_label()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_induct_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .or(pure(vec![])),
        )
        // The head's arity: `: (n : Nat) -> Prop` or `: Prop`. The sort is required — there is no implicit `Type`.
        .and(parse_literal(":").and_keep(parse_induct_arity()))
        .and(many0(parse_top_induct_case))
        .flat_map(
            move |(((label, params), (indices, rep_pub, result_sort)), cases)| {
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
        .and(many0(|| {
            parse_and_head().flat_map(|(doc, vis_pub)| parse_top_induct_body(doc, vis_pub))
        }))
        .and_drop(parse_keyword("end"))
        .map(|(first, rest)| TopItem::Induct(iter::once(first).chain(rest).collect()))
}

/// A universe sort: exactly `Type` or `Prop`. The result sort of a struct or an inductive head is always one of these two — the only universes — so the sort position parses this targeted form rather than a generic `lazy(parse_term)`. A generic term parser is both too loose and, for a struct, greedily eats the `{` opening the field block.
pub(super) fn parse_sort<'a>() -> Parser<'a, Term> {
    parse_prop().or(parse_type())
}

// One field of a `struct`: its documentation comment, then the Σ-type field grammar. A documentation comment before the closing brace documents nothing and says so.
fn parse_struct_field<'a>() -> Parser<'a, StructField> {
    parse_doc().flat_map(|doc| {
        documented(&doc, not_ahead("}"))
            .and_keep(parse_tuple_type_field())
            .map(move |param| StructField { doc, param })
    })
}

// One structure of a `struct` item, after its `pub` and keyword: the name, the parameters, the result sort with its own `pub`, and the fields.
fn parse_struct_member<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopStruct> {
    parse_label()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_induct_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .or(pure(vec![])),
        )
        // The result sort: `: Type` or `: Prop` after the parameters. Required.
        .and(parse_literal(":").and_keep(parse_representation_sort()))
        .and_drop(parse_literal("{"))
        .and(sep_by0_trailing(parse_struct_field, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(
            move |(((label, params), (rep_pub, result_sort)), fields)| TopStruct {
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
        .and(many0(|| {
            parse_and_head().flat_map(|(doc, vis_pub)| parse_struct_member(doc, vis_pub))
        }))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Struct)
}

// A concept field: `use? label : term`, or the signature sugar `label(params) -> term` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar (mirroring top-level `let`'s function sugar). A `use`-prefixed field is a superclass edge — its type must be a concept application, checked at lowering.
pub(super) fn parse_concept_field<'a>() -> Parser<'a, ConceptField> {
    let super_field = catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(|type_| ConceptField {
            doc: None,
            is_super: true,
            label: String::new(),
            func_params: None,
            type_,
        });

    let plain_or_sugar = parse_identifier()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_func_type_param, || {
                        parse_literal(",")
                    }))
                    .and_drop(parse_literal(")"))
                    .and_drop(parse_literal("->")),
            )
            .and(lazy(parse_term))
            .map(|(params, output): (Vec<FuncTypeParam>, Term)| (Some(params), output))
            .or(catch(parse_literal(":"))
                .and_keep(lazy(parse_term))
                .map(|type_| (None, type_))),
        )
        .map(|(label, (func_params, type_)): (&str, _)| ConceptField {
            doc: None,
            is_super: false,
            label: label.to_string(),
            func_params,
            type_,
        });

    // The documentation comment is read first, and a field's two spellings then share it; one before the closing brace documents nothing and says so.
    parse_doc().flat_map(|doc| {
        documented(&doc, not_ahead("}"))
            .and_keep(super_field.or(plain_or_sugar))
            .map(move |field| ConceptField { doc, ..field })
    })
}

// One concept of a `concept` item, after its `pub` and keyword — the struct member's shape with concept fields.
fn parse_concept_member<'a>(doc: Option<Doc>, vis_pub: bool) -> Parser<'a, TopConcept> {
    parse_label()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_induct_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .or(pure(vec![])),
        )
        // The representation sort: `: pub Type`, `: Type`, `: pub Prop`, or `: Prop` after the parameters. Required, like a struct's.
        .and(parse_literal(":").and_keep(parse_representation_sort()))
        .and_drop(parse_literal("{"))
        .and(sep_by0_trailing(parse_concept_field, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(
            move |(((label, params), (rep_pub, result_sort)), fields)| TopConcept {
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
        .and(many0(|| {
            parse_and_head().flat_map(|(doc, vis_pub)| parse_concept_member(doc, vis_pub))
        }))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Concept)
}

// A witness field: `label = term`, or the definition sugar `label(params) = term` — the tuple-field grammar with the label mandatory, kept as written in the AST node (`func_params`); `into_core` undoes the sugar.
pub(super) fn parse_witness_field<'a>() -> Parser<'a, WitnessField> {
    catch(parse_tuple_field_prefix())
        .and(lazy(parse_term))
        .map(|((label, func_params), value)| WitnessField {
            label,
            func_params,
            value,
        })
}

// A witness-body entry: a `use <term>` fill for one of the concept's `use`-marked fields, or an implementation field.
pub(super) fn parse_witness_entry<'a>() -> Parser<'a, WitnessEntry> {
    catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(WitnessEntry::Use)
        .or(parse_witness_field().map(WitnessEntry::Field))
}

// One witness: `Concept(args) { … }`, or `(params) => Concept(args) { … }` with a nonempty telescope. The separator makes the parameterized form's terminal concept application explicit; an empty telescope must use the bare form instead. The body is the brace block, or `;` in its place — the derived form, whose body the compiler writes — and either may follow either head.
fn parse_witness_member<'a>(doc: Option<Doc>) -> Parser<'a, TopWitness> {
    catch(
        parse_literal("(")
            .and_keep(sep_by1_trailing(parse_func_sugar_param, || {
                parse_literal(",")
            }))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .or(pure(vec![]))
    .and(parse_name())
    .and(
        catch(
            parse_literal("(")
                .and_keep(sep_by0_trailing(|| lazy(parse_term), || parse_literal(",")))
                .and_drop(parse_literal(")")),
        )
        .or(pure(vec![])),
    )
    .and(
        catch(parse_literal("{"))
            .and_keep(sep_by0_trailing(parse_witness_entry, || parse_literal(",")))
            .and_drop(parse_literal("}"))
            .map(Some)
            .or(parse_literal(";").map(|()| None)),
    )
    .map(move |(((params, concept), args), body)| TopWitness {
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
/// **A dispatch rather than nine ordered alternatives, because the heads are disjoint.** Every item is led by one word, so a choice chain re-read the same optional `pub` and identifier once per arm and then reported whichever arm happened to read furthest — which, since [`parse_keyword`] rejects only *after* consuming the identifier, was always the first. Every unrecognized head therefore blamed `mod`. Reading the head once and switching on it makes the arm that owns the diagnosis the arm that produced it.
///
/// **A head commits when it is reserved and cannot begin a term**, which is exactly `mod`, `use`, `induct`, `struct` and `foreign`. Nothing else may be written in their place, so once one is read its arm owns the error and [`commit`] stops an enclosing choice from backtracking into a vaguer one.
///
/// **The other four are [`catch`]ed back to recoverable, and the language decides which.** The head is already consumed when the arm runs, so without it a failure inside one of them is fatal by progress alone and would abort the item loop instead of falling through. `concept`, `satisfy` and `test` are contextual words — `documentation/syntax.md` keeps them ordinary identifiers outside a declaration position, so one of them here may really be a program's tail calling a function of that name. `let` is reserved but shared with the term grammar: a top-level `let` requires an annotation, and `let x = 1; tail` has to fall through to a local `let`. An unrecognized head is recoverable for the same reason — it is how the item loop terminates before a program's tail begins.
///
/// **A `pub` in front makes every arm commit, because it removes the fall-through the `catch` exists for.** `pub` is a keyword, so no term begins with one: after reading it there is no tail for a failed item to become, and an unrecognized head after it names no item rather than ending the item loop. Leaving those arms recoverable threw the diagnosis away wherever it mattered most. A module recovers it — `Module::parse_items_end` re-runs the item parser once input remains — but an entrypoint's grammar runs `parse_term` instead, which tries a local `let` at the `pub`, fails one token in, and wins [`Parser::or`]'s furthest-failure tie-break over the real error the `catch` had just made backtrackable. Every mistake inside a `pub let` in a program therefore reported `Expected keyword 'let', obtained 'pub'` against the `let`, and the refusals `parse_top_witness` and `parse_top_test` write by hand for a `pub` they cannot accept never reached a reader at all.
///
/// **A documentation comment in front makes every arm commit too, and for the same reason.** Once a `-- |` block is read there is nothing else it can be: a program's tail is not documented, an unrecognized head after it names no item, and the block itself is the diagnosis. The head is then read committed as well, so a block at the end of a file reports itself rather than the item loop's generic end.
pub(crate) fn parse_top_item<'a>() -> Parser<'a, TopItem> {
    parse_doc().flat_map(|doc| {
        let head = parse_pub().and(parse_identifier_raw());
        let head = match &doc {
            Some(_) => head.map_err(DOC_BEFORE_NOTHING),
            None => catch(head),
        };

        mark().and(head).flat_map(move |(start, (vis_pub, head))| {
            // Recoverable only where a failed item may still be a program's tail, which a `pub` or a documentation comment rules out.
            let documented = doc.is_some();
            let fallible = |parser| match vis_pub || documented {
                true => commit(parser),
                false => catch(parser),
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
                "test" => match documented {
                    true => commit(fail(DOC_BEFORE_TEST)),
                    false => fallible(parse_top_test(vis_pub)),
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
