use {
    super::{
        Grammar, TUPLE_FIELD_TAKES_NO_MARK, begins_a_name, digit_led, members,
        parse_apply_argument, parse_block_string_literal, parse_bool_intrinsic,
        parse_bytes_literal, parse_char_lit, parse_decorated_label, parse_func_pattern_param,
        parse_func_type_param, parse_held, parse_list_literal, parse_literal, parse_mark,
        parse_name, parse_number, parse_string_literal, parse_word_raw, parse_written_word,
        refuse_dangling_dot,
    },
    crate::{
        Binder, FuncParam, FuncTypeParam, Label, StructLit, StructLitEntry, Subterm, Term, Tuple,
        TupleField, TupleType, TupleTypeParam, parse_body, parse_identifier_raw, parse_term,
        parse_whitespace, reserved_keyword,
    },
    curios_common::{Plicity, is_reserved},
    curios_parse::{
        Parsed, Parser, ParserState, commit, fail, fail_from, look_ahead, mark, not_ahead, pure,
        raise, refusal, sep_by0_trailing, take_exact,
    },
    std::iter,
};

pub fn parse_intrinsic<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    parse_bool_intrinsic
        // A numeral's dangling dot is refused ahead of both numeral forms, and a number reads its decimal form first: `5.0` is a `Flt`, not the integer `5` projected.
        .or(refuse_dangling_dot)
        .or(parse_number)
        .or(parse_char_lit)
        // The block form is tried first, since its opener begins with the one-line form's.
        .or(parse_block_string_literal)
        .or(parse_string_literal)
        .or(parse_bytes_literal)
        .or(parse_list_literal)
        .parse(state)
}

pub fn parse_parens<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    parse_literal("(")
        .and_keep(parse_term::<D>)
        .and_drop(parse_literal(")"))
        .parse(state)
}

// A tuple type's field: [`parse_field_member`], and a mark refused where none of its forms read the field.
pub fn parse_tuple_type_field<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, TupleTypeParam<D>> {
    parse_field_member.or(refuse_field_mark).parse(state)
}

/// A field's label under its decorator, and the parameters its signature sugar writes.
type SugaredField<D> = (Binder<D>, Vec<FuncTypeParam<D>>);

// A Σ-type / struct-declaration field, past whatever mark its site takes: an optional label and the field type, or the signature sugar `label(params) -> type` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar. Shared by tuple types and `struct` decls. The sugared catch spans through `->`, so a positional field that merely starts with an application (`f(x)`) backtracks cleanly.
pub fn parse_field_member<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, TupleTypeParam<D>> {
    parse_decorated_label::<D>
        .and(
            parse_literal("(")
                .and_keep(sep_by0_trailing(parse_func_type_param, || {
                    parse_literal(",")
                }))
                .and_drop(parse_literal(")")),
        )
        .and_drop(parse_literal("->"))
        // Past the arrow the list is this sugar's, so a member it refuses is its own to report.
        .flat_map(|(labelled, params)| {
            members(params).map(move |params| (labelled.clone(), params))
        })
        // A field's type position commits once its introducer is read: past a `->` or a `:` nothing else may stand here, and the unlabeled alternative below would otherwise read the label alone as the whole type and leave the enclosing `}` to complain at the introducer.
        .and(commit(parse_term::<D>))
        .map(
            |((binder, params), output): (SugaredField<D>, Term<D>)| TupleTypeParam {
                core: binder.core,
                label: Some(binder.label),
                func_params: Some(params),
                type_: output,
            },
        )
        .or(parse_decorated_label::<D>
            .and_drop(parse_literal(":"))
            .and(commit(parse_term::<D>))
            .map(|(binder, type_): (Binder<D>, Term<D>)| TupleTypeParam {
                core: binder.core,
                label: Some(binder.label),
                func_params: None,
                type_,
            }))
        .or(refuse_numbered_field_label)
        .or(parse_term::<D>.map(|type_| TupleTypeParam {
            core: D::Fields::default(),
            label: None,
            func_params: None,
            type_,
        }))
        .or(refuse_keyword_field_label)
        .parse(state)
}

// A word that begins with a digit written where a field is labelled. Tried ahead of the positional field, which would read a number and leave the `:` to answer for a mistake about a name: no type is a number and then a colon, so the refusal is exact, and commits.
fn refuse_numbered_field_label<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, TupleTypeParam<D>> {
    mark()
        .and(parse_word_raw)
        .flat_map(|(start, word)| match begins_a_name(word) {
            true => fail("not a number written as a field label").boxed(),
            false => refusal(&start, digit_led(word)).boxed(),
        })
        .and_drop(parse_whitespace)
        .and_drop(look_ahead(parse_literal(":")))
        .flat_map(|report| commit(raise(report)))
        .parse(state)
}

// A mark written where a tuple type's field is declared. Tried last, as the keyword refusal is and for its reason: no field form begins with `@` or with `use`, so reaching this means none read the field, and neither can begin a term, so the refusal commits.
fn refuse_field_mark<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, TupleTypeParam<D>> {
    mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            Plicity::Explicit => fail("not a mark written on a field").boxed(),
            Plicity::Implicit | Plicity::Witness => {
                commit(fail_from(&start, TUPLE_FIELD_TAKES_NO_MARK)).boxed()
            }
        })
        .parse(state)
}

// A keyword written where a field is labelled. Tried last, so reaching it means none of the three real forms could read this field, and it commits: `parse_label` refuses a keyword uncommittedly — it has to, since a positional field is a term and a term may open with one — and the refusal would then be discarded for the enclosing `}`, which reports `Expected '}', obtained 'e'` for a field written `end : Nat`.
//
// The introducer is required, and is what keeps this from claiming a positional field that merely opens with a keyword: `{ match b | true => T end }` reaches here only if the term failed, and its `match` is followed by a scrutinee rather than by `:` or `(`, so the arm's own diagnosis stands. A term that *is* `<keyword> (` — a `let` over a tuple pattern — parses as the positional form and never reaches this alternative at all.
fn refuse_keyword_field_label<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, TupleTypeParam<D>> {
    mark()
        .and(parse_identifier_raw)
        .and_drop(parse_whitespace)
        .and_drop(look_ahead(parse_literal(":").or(parse_literal("("))))
        .flat_map(|(start, word)| match is_reserved(word) {
            true => commit(fail_from(&start, reserved_keyword(word))).boxed(),
            false => fail("not a keyword written as a field label").boxed(),
        })
        .parse(state)
}

pub fn parse_tuple_type<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
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
        .parse(state)
}

// A parsed labeled-field prefix: the label and, for the definition sugar, the written lambda-parameter list.
type TupleFieldPrefix<D> = (Label, Option<Vec<FuncParam<D>>>);

// The committing prefix of a labeled tuple/struct-literal field: `label =` or the definition sugar `label(params) =`. It fails recoverably, so a positional field that merely starts with an identifier or an application backtracks cleanly; the `=` is guarded against `==` and `=>` via `not_ahead`, mirroring the bind arm's idiom, since `(a == b, 2)` is a positional field whose value begins with a name and must not be read as the label `a`. The sugar's list is a call's as far as its closing parenthesis — `f(@A, use dict, x)` is both — so a member the sugar refuses is held until the `=` and raised past it.
pub fn parse_tuple_field_prefix<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, TupleFieldPrefix<D>> {
    parse_written_word
        .and(
            parse_literal("(")
                .and_keep(sep_by0_trailing(parse_func_pattern_param, || {
                    parse_literal(",")
                }))
                .and_drop(parse_literal(")"))
                .map(Some)
                .or(pure(None)),
        )
        .and_drop(parse_field_equals)
        .flat_map(|(label, func_params): (Label, _)| match func_params {
            Some(params) => members(params)
                .map(move |params| (label, Some(params)))
                .boxed(),
            None => pure((label, None)).boxed(),
        })
        .parse(state)
}

// The `=` a labeled field's prefix ends in, guarded against `==` and `=>`.
pub fn parse_field_equals<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    take_exact("=")
        .and_drop(not_ahead("="))
        .and_drop(not_ahead(">"))
        .and_drop(parse_whitespace)
        .parse(state)
}

// A labeled field's value: a body where the field is written as a function, `label(params) = body`, and a term where it is a value.
fn field_value<'a, D: Grammar>(function: bool) -> impl Parser<'a, Term<D>> {
    match function {
        true => parse_body::<D>.boxed(),
        false => parse_term::<D>.boxed(),
    }
}

// A tuple-literal / struct-literal field: `label = value`, the definition sugar `label(params) = value` — kept as written in the AST node (`func_params`); `into_core` undoes the sugar — or a positional value.
pub fn parse_tuple_field<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, TupleField<D>> {
    parse_tuple_field_prefix
        // Past a guarded `=` the value must follow: no positional field can hold one at its top level, so the field owns the diagnosis rather than falling back to the positional reading and leaving the enclosing `}` or `)` to complain at the label.
        .flat_map(|prefix| {
            commit(field_value(prefix.1.is_some())).map(move |value| (prefix.clone(), value))
        })
        .map(|((label, func_params), value)| TupleField {
            label: Some(label),
            func_params,
            value,
        })
        .or(parse_term::<D>.map(|value| TupleField {
            label: None,
            func_params: None,
            value,
        }))
        .parse(state)
}

pub fn parse_tuple<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    // Two committing prefixes distinguish a tuple literal from a parenthesized term: a first field followed by a comma (`(x,` / `(a = 1,`), or a named first field alone (`(a = 1)` / `(f(x) = e)` — the `=` already disambiguates, so the one-element form needs no trailing comma).

    parse_literal("(")
        .and_keep(parse_tuple_field)
        .and_drop(parse_literal(","))
        .and(sep_by0_trailing(parse_tuple_field, || parse_literal(",")))
        .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
        .or(parse_literal("(")
            .and_keep(parse_tuple_field_prefix)
            .flat_map(|prefix| {
                field_value(prefix.1.is_some()).map(move |value| (prefix.clone(), value))
            })
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
        .parse(state)
}

// A struct-literal entry: a `..base` spread (no term begins with `..` — a leading-dot float has a single dot — so the prefix commits), a `use <term>` fill for a concept's `use`-marked field (the call-site argument form — a mark can never begin a field label or value), or a field, plain or under `@`: after the mark comes the field as a plain one is written, `label = value` or the value alone, and `@_` holds a hidden field's place. Spread position and multiplicity are core elaboration's job.
pub fn parse_struct_entry<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, StructLitEntry<D>> {
    parse_literal("..")
        .and_keep(parse_term::<D>)
        .map(StructLitEntry::Spread)
        .or(parse_mark.flat_map(|plicity| match plicity {
            Plicity::Witness => parse_held
                .or(parse_term::<D>)
                .map(StructLitEntry::Use)
                .boxed(),
            Plicity::Implicit => parse_held
                .map(|value| TupleField {
                    label: None,
                    func_params: None,
                    value,
                })
                .or(parse_tuple_field)
                .map(StructLitEntry::Implicit)
                .boxed(),
            Plicity::Explicit => parse_tuple_field.map(StructLitEntry::Field).boxed(),
        }))
        .parse(state)
}

// A struct literal: `Name { … }` or `Name(args) { … }`, where a list with nothing written, `Name() { … }`, is kept as the application it is and not read as the bare name. The trailing `{` is the commit point — it distinguishes the literal from a bare name / name-application (no brace) and from a Σ-type `{ x : A }` (no head name), so there is no grammar conflict. Plain entries reuse the tuple-value grammar (`= value` or positional) and `use <term>` fills a concept's `use`-marked field; the head's arguments are a call's, marks included, since an applied head is an application of the type former.
pub fn parse_struct_lit<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    parse_name
        .and(
            parse_literal("(")
                .and_keep(sep_by0_trailing(parse_apply_argument, || {
                    parse_literal(",")
                }))
                .and_drop(parse_literal(")"))
                .map(Some)
                .or(pure(None)),
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
        .parse(state)
}
