use super::*;

pub(super) fn parse_intrinsic<'a>() -> Parser<'a, Term> {
    parse_bool_intrinsic()
        // A numeral's dangling dot is refused ahead of both numeral forms, then decimal floats: `5.0` is a `Flt`, not the integer `5` projected.
        .or(refuse_dangling_dot())
        .or(parse_flt_value())
        .or(parse_char_lit())
        .or(parse_num_lit())
        // The block form is tried first, since its opener begins with the one-line form's.
        .or(parse_block_string_literal())
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
    parse_label()
        .and(
            parse_literal("(")
                .and_keep(sep_by0_trailing(parse_func_type_param, || {
                    parse_literal(",")
                }))
                .and_drop(parse_literal(")")),
        )
        .and_drop(parse_literal("->"))
        // A field's type position commits once its introducer is read: past a `->` or a `:` nothing else may stand here, and the unlabeled alternative below would otherwise read the label alone as the whole type and leave the enclosing `}` to complain at the introducer.
        .and(commit(lazy(parse_term)))
        .map(
            |((label, params), output): ((Label, Vec<FuncTypeParam>), Term)| TupleTypeParam {
                label: Some(label),
                func_params: Some(params),
                type_: output,
            },
        )
        .or(parse_label()
            .and_drop(parse_literal(":"))
            .and(commit(lazy(parse_term)))
            .map(|(label, type_): (Label, Term)| TupleTypeParam {
                label: Some(label),
                func_params: None,
                type_,
            }))
        .or(lazy(parse_term).map(|type_| TupleTypeParam {
            label: None,
            func_params: None,
            type_,
        }))
        .or(refuse_keyword_field_label())
}

// A keyword written where a field is labelled. Tried last, so reaching it means none of the three real forms could read this field, and it commits: `parse_label` refuses a keyword uncommittedly — it has to, since a positional field is a term and a term may open with one — and the refusal was then discarded for the enclosing `}`, which reported `Expected '}', obtained 'e'` for a field written `end : Nat`.
//
// The introducer is required, and is what keeps this from claiming a positional field that merely opens with a keyword: `{ match b | true => T end }` reaches here only if the term failed, and its `match` is followed by a scrutinee rather than by `:` or `(`, so the arm's own diagnosis stands. A term that *is* `<keyword> (` — a `let` over a tuple pattern — parses as the positional form and never reaches this alternative at all.
fn refuse_keyword_field_label<'a>() -> Parser<'a, TupleTypeParam> {
    mark()
        .and(parse_identifier_raw())
        .and_drop(parse_whitespace())
        .and_drop(look_ahead(parse_literal(":").or(parse_literal("("))))
        .flat_map(|(start, word)| match is_keyword(word) {
            true => commit(fail_from(&start, reserved_keyword(word))),
            false => fail("not a keyword written as a field label"),
        })
}

pub(super) fn parse_tuple_type<'a>() -> Parser<'a, Term> {
    parse_literal("{")
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

// The committing prefix of a labeled tuple/struct-literal field: `label =` or the definition sugar `label(params) =`. It fails recoverably, so a positional field that merely starts with an identifier or an application backtracks cleanly; the `=` is guarded against `==` and `=>` via `not_ahead`, mirroring the bind arm's idiom, since `(a == b, 2)` is a positional field whose value begins with a name and must not be read as the label `a`.
pub(super) fn parse_tuple_field_prefix<'a>() -> Parser<'a, TupleFieldPrefix> {
    parse_identifier()
        .and(
            parse_literal("(")
                .and_keep(sep_by0_trailing(parse_func_param, || parse_literal(",")))
                .and_drop(parse_literal(")"))
                .map(Some)
                .or(pure(None)),
        )
        .and_drop(
            take_exact("=")
                .and_drop(not_ahead("="))
                .and_drop(not_ahead(">")),
        )
        .and_drop(parse_whitespace())
        .map(|(label, func_params): (&str, _)| (label.to_string(), func_params))
}

// A tuple-literal / struct-literal field: `label = value`, the definition sugar `label(params) = value` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar — or a positional value.
pub(super) fn parse_tuple_field<'a>() -> Parser<'a, TupleField> {
    parse_tuple_field_prefix()
        // Past a guarded `=` the value must follow: no positional field can hold one at its top level, so the field owns the diagnosis rather than falling back to the positional reading and leaving the enclosing `}` or `)` to complain at the label.
        .and(commit(lazy(parse_term)))
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

    parse_literal("(")
        .and_keep(parse_tuple_field())
        .and_drop(parse_literal(","))
        .and(sep_by0_trailing(parse_tuple_field, || parse_literal(",")))
        .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
        .or(parse_literal("(")
            .and_keep(parse_tuple_field_prefix())
            .and(lazy(parse_term))
            .map(|((label, func_params), value)| {
                vec![TupleField {
                    label: Some(label),
                    func_params,
                    value,
                }]
            }))
        .and_drop(parse_literal(")"))
        .map(|fields| Subterm::Tuple(Tuple { fields }))
        .map(Into::into)
}

// A struct-literal entry: a `..base` spread (no term begins with `..` — a leading-dot float has a single dot — so the prefix commits), a `use <term>` fill for a concept's `use`-marked field (mirroring the call-site argument form — `use` is reserved, so it can never begin a field label or value), or a plain field. Spread position and multiplicity are core elaboration's job.
pub(super) fn parse_struct_entry<'a>() -> Parser<'a, StructLitEntry> {
    parse_literal("..")
        .and_keep(lazy(parse_term))
        .map(StructLitEntry::Spread)
        .or(parse_keyword("use")
            .and_keep(lazy(parse_term))
            .map(StructLitEntry::Use))
        .or(parse_tuple_field().map(StructLitEntry::Field))
}

// A struct literal: `Name { … }` or `Name(args) { … }`. The trailing `{` is the commit point — it distinguishes the literal from a bare name / name-application (no brace) and from a Σ-type `{ x : A }` (no head name), so there is no grammar conflict. Plain entries reuse the tuple-value grammar (`= value` or positional) and `use <term>` fills a concept's `use`-marked field; the head's arguments are plain terms (`@`-pinning is not the struct idiom — the head type pins instead).
pub(super) fn parse_struct_lit<'a>() -> Parser<'a, Term> {
    parse_name()
        .and(
            parse_literal("(")
                .and_keep(sep_by0_trailing(|| lazy(parse_term), || parse_literal(",")))
                .and_drop(parse_literal(")"))
                .or(pure(vec![])),
        )
        .and_drop(parse_literal("{"))
        .and(commit(
            sep_by0_trailing(parse_struct_entry, || parse_literal(","))
                .and_drop(parse_literal("}")),
        ))
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
    parse_literal("@")
        .map(|()| Plicity::Implicit)
        .or(pure(Plicity::Explicit))
}
