use super::*;

pub(super) fn parse_intrinsic<'a>() -> Parser<'a, Term> {
    parse_bool_intrinsic()
        // Decimal floats first: `5.0` is a `Flt`, not the integer `5` projected.
        .or(parse_flt_value())
        .or(parse_char_lit())
        .or(parse_num_lit())
        .or(parse_string_literal())
        .or(parse_bin_literal())
        .or(parse_list_literal())
}

pub(super) fn parse_parens<'a>() -> Parser<'a, Term> {
    parse_literal("(")
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(")"))
}

// A Σ-type / struct-declaration field: an optional label and the field type, or the signature sugar `label(params) -> type` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar. Shared by tuple types and `struct` decls. The sugared catch spans through `->`, so a positional field that merely starts with an application (`f(x)`) backtracks cleanly.
pub(super) fn parse_tuple_type_field<'a>() -> Parser<'a, TupleTypeParam> {
    catch(
        parse_identifier()
            .and(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_func_type_param, || {
                        parse_literal(",")
                    }))
                    .and_drop(parse_literal(")")),
            )
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(
        |((label, params), output): ((&str, Vec<FuncTypeParam>), Term)| TupleTypeParam {
            label: Some(label.to_string()),
            func_params: Some(params),
            type_: output,
        },
    )
    .or(catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(label, type_): (&str, Term)| TupleTypeParam {
            label: Some(label.to_string()),
            func_params: None,
            type_,
        }))
    .or(lazy(parse_term).map(|type_| TupleTypeParam {
        label: None,
        func_params: None,
        type_,
    }))
}

pub(super) fn parse_tuple_type<'a>() -> Parser<'a, Term> {
    catch(parse_literal("{"))
        .and_keep(sep_by0_trailing(parse_tuple_type_field, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal("}"))
        .map(|fields| {
            Subterm::TupleType(TupleType {
                fields: fields.into_iter().collect(),
            })
        })
        .map(Into::into)
}

// A parsed labeled-field prefix: the label and, for the definition sugar, the written lambda-parameter list.
type TupleFieldPrefix = (String, Option<Vec<(Plicity, Label, Option<Term>)>>);

// The committing prefix of a labeled tuple/struct-literal field: `label =` or the definition sugar `label(params) =`. The caller wraps it in `catch`, so a positional field that merely starts with an identifier or an application backtracks cleanly.
pub(super) fn parse_tuple_field_prefix<'a>() -> Parser<'a, TupleFieldPrefix> {
    parse_identifier()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0_trailing(parse_func_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .map(Some)
            .or(pure(None)),
        )
        .and_drop(parse_literal("="))
        .map(|(label, func_params): (&str, _)| (label.to_string(), func_params))
}

// A tuple-literal / struct-literal field: `label = value`, the definition sugar `label(params) = value` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar — or a positional value.
pub(super) fn parse_tuple_field<'a>() -> Parser<'a, TupleField> {
    catch(parse_tuple_field_prefix())
        .and(lazy(parse_term))
        .map(|((label, func_params), value)| TupleField {
            label: Some(label),
            func_params,
            value,
        })
        .or(lazy(parse_term).map(|value| TupleField {
            label: None,
            func_params: None,
            value,
        }))
}

pub(super) fn parse_tuple<'a>() -> Parser<'a, Term> {
    // Two committing prefixes distinguish a tuple literal from a parenthesized term: a first field followed by a comma (`(x,` / `(a = 1,`), or a named first field alone (`(a = 1)` / `(f(x) = e)` — the `=` already disambiguates, so the one-element form needs no trailing comma).
    catch(
        parse_literal("(")
            .and_keep(parse_tuple_field())
            .and_drop(parse_literal(",")),
    )
    .and(sep_by0_trailing(parse_tuple_field, || parse_literal(",")))
    .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
    .or(
        catch(parse_literal("(").and_keep(parse_tuple_field_prefix()))
            .and(lazy(parse_term))
            .map(|((label, func_params), value)| {
                vec![TupleField {
                    label: Some(label),
                    func_params,
                    value,
                }]
            }),
    )
    .and_drop(parse_literal(")"))
    .map(|fields| Subterm::Tuple(Tuple { fields }))
    .map(Into::into)
}

// A struct-literal entry: a `..base` spread (no term begins with `..` — a leading-dot float has a single dot — so the prefix commits), a `use <term>` fill for a concept's `use`-marked field (mirroring the call-site argument form — `use` is reserved, so it can never begin a field label or value), or a plain field. Spread position and multiplicity are core elaboration's job.
pub(super) fn parse_struct_entry<'a>() -> Parser<'a, StructLitEntry> {
    catch(parse_literal(".."))
        .and_keep(lazy(parse_term))
        .map(StructLitEntry::Spread)
        .or(catch(parse_keyword("use"))
            .and_keep(lazy(parse_term))
            .map(StructLitEntry::Use))
        .or(parse_tuple_field().map(StructLitEntry::Field))
}

// A struct literal: `Name { … }` or `Name(args) { … }`. The trailing `{` inside the `catch` is the commit point — it distinguishes the literal from a bare name / name-application (no brace) and from a Σ-type `{ x : A }` (no head name), so there is no grammar conflict. Plain entries reuse the tuple-value grammar (`= value` or positional) and `use <term>` fills a concept's `use`-marked field; the head's arguments are plain terms (`@`-pinning is not the struct idiom — the head type pins instead).
pub(super) fn parse_struct_lit<'a>() -> Parser<'a, Term> {
    catch(
        parse_name()
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0_trailing(|| lazy(parse_term), || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            .and_drop(parse_literal("{")),
    )
    .and(sep_by0_trailing(parse_struct_entry, || parse_literal(",")))
    .and_drop(parse_literal("}"))
    .map(|((head, params), entries)| {
        Subterm::StructLit(StructLit {
            head,
            params,
            entries,
        })
        .into()
    })
}

// A leading `@` marks a binder (or call-site argument) implicit.
pub(super) fn parse_plicity<'a>() -> Parser<'a, Plicity> {
    catch(parse_literal("@"))
        .map(|()| Plicity::Implicit)
        .or(pure(Plicity::Explicit))
}
