use super::*;

pub(super) fn parse_pub<'a>() -> Parser<'a, bool> {
    catch(parse_keyword("pub")).map(|()| true).or(pure(false))
}

// A top-level `let` item: one definition, or the group `let f … and g …;`. Each member takes its own `pub` — before `let` for the first, before `and` for each later one — and one `;` terminates the whole item.

// A `test` declaration: `test name() = body;`. The parentheses are the function sugar written out — required, and empty until the property-testing specification opens that seam, so anything between them fails as an unexpected token with no rule behind it. `pub` is refused by name: a test's identifier is its report line, not an export. Like `satisfy`, `test` stays a contextual word everywhere else.
pub(super) fn parse_top_test<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("test")))
        .flat_map(|(vis_pub, ())| match vis_pub {
            true => fail("a test is never `pub`: its name is its report line, not an export"),
            false => pure(()),
        })
        .and_keep(parse_identifier())
        .and_drop(parse_literal("("))
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .map(|(label, body)| {
            TopItem::Test(TopTest {
                label: label.to_string(),
                body,
            })
        })
}

pub(super) fn parse_top_let<'a>() -> Parser<'a, TopItem> {
    let member = |vis_pub: bool| {
        parse_binding().map(move |(label, signature)| TopLet {
            vis_pub,
            label,
            signature,
        })
    };

    catch(parse_pub().and(parse_keyword("let")))
        .flat_map(move |(vis_pub, ())| member(vis_pub))
        .and(many0(move || {
            catch(parse_pub().and(parse_keyword("and")))
                .flat_map(move |(vis_pub, ())| member(vis_pub))
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

// `(T, T, ...) -> T` (a foreign function) or a bare `T` (a zero-argument foreign, like `host_ops`'s `io_clock_wall`). Params carry no surface label — `a0`, `a1`, … name them positionally; the single result is unnamed (`_`), since a `foreign` declaration has no surface syntax for a named record result the way `/sys/Handle`'s Rust-side rows do.
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
        results: vec![("_".to_string(), output)],
    })
    .or(parse_wire_type().map(|output| WireSignature {
        params: vec![],
        results: vec![("_".to_string(), output)],
    }))
}

// `foreign name : T;` — a name and a wire signature with no body, bound to a host-provided implementation at link time. Mirrors `parse_top_let`, but ends after the signature instead of parsing `= body`.
pub(super) fn parse_top_foreign<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("foreign"))).flat_map(|(vis_pub, ())| {
        parse_identifier()
            .and_drop(parse_literal(":"))
            .and(parse_wire_signature())
            .and_drop(parse_literal(";"))
            .map(move |(label, signature)| {
                TopItem::Foreign(TopForeign {
                    vis_pub,
                    label: label.to_string(),
                    signature,
                })
            })
    })
}

pub(super) fn parse_top_mod<'a>() -> Parser<'a, TopItem> {
    spanned(
        catch(parse_pub().and(parse_keyword("mod"))).flat_map(|(vis_pub, ())| {
            parse_identifier().flat_map(move |name| {
                catch(
                    many0(parse_top_item)
                        .and_drop(parse_keyword("end"))
                        .map(|items| Some(Module { items })),
                )
                .or(parse_literal(";").map(|()| None))
                .map(move |module| (vis_pub, name.to_string(), module))
            })
        }),
    )
    .map(|(span, (vis_pub, label, module))| {
        TopItem::Mod(TopMod {
            span: Some(span),
            vis_pub,
            label,
            module,
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
    catch(parse_keyword("mod").and_keep(parse_identifier()))
        .map(|s| GroupItem::Mod(s.to_string()))
        .or(catch(parse_keyword("let").and_keep(parse_identifier()))
            .map(|s| GroupItem::Let(s.to_string())))
        .or(parse_identifier().map(|s| GroupItem::Both(s.to_string())))
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

pub(super) fn parse_top_use<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("use"))).flat_map(|(vis_pub, ())| {
        parse_use_path()
            .and(parse_use_group())
            .and_drop(parse_literal(";"))
            .map(move |(name, group)| {
                TopItem::Use(TopUse {
                    vis_pub,
                    name,
                    group,
                })
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
    parse_literal("|")
        .and_keep(parse_identifier())
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
            |((label, payload), target): ((&str, Vec<_>), Option<Vec<Term>>)| TopCase {
                label: label.to_string(),
                payload,
                target,
            },
        )
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

pub(super) fn parse_top_induct_body<'a>(vis_pub: bool) -> Parser<'a, TopInduct> {
    parse_identifier()
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

                let label = label.to_string();
                pure(TopInduct {
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

pub(super) fn parse_top_induct<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("induct"))).flat_map(|(vis_pub, ())| {
        parse_top_induct_body(vis_pub)
            .and(many0(|| {
                catch(parse_pub().and(parse_keyword("and")))
                    .flat_map(|(vis_pub2, ())| parse_top_induct_body(vis_pub2))
            }))
            .and_drop(parse_keyword("end"))
            .map(|(first, rest)| TopItem::Induct(iter::once(first).chain(rest).collect()))
    })
}

/// A universe sort: exactly `Type` or `Prop`. The result sort of a struct or an inductive head is always one of these two — the only universes — so the sort position parses this targeted form rather than a generic `lazy(parse_term)`. A generic term parser is both too loose and, for a struct, greedily eats the `{` opening the field block.
pub(super) fn parse_sort<'a>() -> Parser<'a, Term> {
    parse_prop().or(parse_type())
}

// One structure of a `struct` item, after its `pub` and keyword: the name, the parameters, the result sort with its own `pub`, and the fields.
fn parse_struct_member<'a>(vis_pub: bool) -> Parser<'a, TopStruct> {
    parse_identifier()
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
        .and(sep_by0_trailing(parse_tuple_type_field, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal("}"))
        .map(
            move |(((label, params), (rep_pub, result_sort)), fields)| TopStruct {
                vis_pub,
                rep_pub,
                label: label.to_string(),
                params,
                result_sort,
                fields,
            },
        )
}

// A `struct` item: one structure, or a `struct A … and B …` group whose fields name one another. Each member takes its own `pub`, before `struct` for the first and before `and` for the rest.
pub(super) fn parse_top_struct<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("struct")))
        .flat_map(|(vis_pub, ())| parse_struct_member(vis_pub))
        .and(many0(|| {
            catch(parse_pub().and(parse_keyword("and")))
                .flat_map(|(vis_pub, ())| parse_struct_member(vis_pub))
        }))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Struct)
}

// A concept field: `use? label : term`, or the signature sugar `label(params) -> term` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar (mirroring top-level `let`'s function sugar). A `use`-prefixed field is a superclass edge — its type must be a concept application, checked at lowering.
pub(super) fn parse_concept_field<'a>() -> Parser<'a, ConceptField> {
    let super_field = catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(|type_| ConceptField {
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
            is_super: false,
            label: label.to_string(),
            func_params,
            type_,
        });

    super_field.or(plain_or_sugar)
}

// One concept of a `concept` item, after its `pub` and keyword — the struct member's shape with concept fields.
fn parse_concept_member<'a>(vis_pub: bool) -> Parser<'a, TopConcept> {
    parse_identifier()
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
                vis_pub,
                rep_pub,
                label: label.to_string(),
                params,
                result_sort,
                fields,
            },
        )
}

// A `concept` item: one concept, or a `concept A … and B …` group whose method types name one another's dictionaries. Each member takes its own `pub`, as a `struct` group's do.
pub(super) fn parse_top_concept<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("concept")))
        .flat_map(|(vis_pub, ())| parse_concept_member(vis_pub))
        .and(many0(|| {
            catch(parse_pub().and(parse_keyword("and")))
                .flat_map(|(vis_pub, ())| parse_concept_member(vis_pub))
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

// One witness: `Concept(args) { … }`, or `(params) => Concept(args) { … }` with a nonempty telescope. The separator makes the parameterized form's terminal concept application explicit; an empty telescope must use the bare form instead.
fn parse_witness_member<'a>() -> Parser<'a, TopWitness> {
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
    .and_drop(parse_literal("{"))
    .and(sep_by0_trailing(parse_witness_entry, || parse_literal(",")))
    .and_drop(parse_literal("}"))
    .map(|(((params, concept), args), entries)| TopWitness {
        params,
        concept,
        args,
        entries,
    })
}

// A witness declaration is anonymous: `satisfy Concept(args) { … }`, or a group `satisfy C(A) { … } and D(B) { … }` of witnesses that resolve through one another. The keyword is the commit point — nothing else at item position begins with `satisfy`. A witness has no `pub`, so neither does an `and` member.
pub(super) fn parse_top_witness<'a>() -> Parser<'a, TopItem> {
    catch(parse_keyword("satisfy"))
        .and_keep(parse_witness_member())
        .and(many0(|| {
            catch(parse_keyword("and")).and_keep(parse_witness_member())
        }))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Witness)
}

pub(crate) fn parse_top_item<'a>() -> Parser<'a, TopItem> {
    parse_top_mod()
        .or(parse_top_use())
        .or(parse_top_concept())
        .or(parse_top_witness())
        .or(parse_top_test())
        .or(parse_top_let())
        .or(parse_top_induct())
        .or(parse_top_struct())
        .or(parse_top_foreign())
}
