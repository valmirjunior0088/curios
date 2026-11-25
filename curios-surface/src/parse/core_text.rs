//! The productions only Core text has: an operand under a decorator and the values a decorator states, a witness named by its identity, an intrinsic node, a declaration named by its absolute name, and the file a `.crsc` is.
//!
//! **A file is items, then the entry a program closes with.** An item is a `test`, a `foreign` row, or a group: declarations of any kinds joined by `and`, each written as its own keyword writes it — a `let` member after `and` by its name alone, as a program's group writes one — and closed by the `;` a `let` owes where its last member is one. The entry states its type on the line beneath it, `: T`.

use {
    super::{
        Grammar, clear_comments, one_of, parse_and_head, parse_atomic_term, parse_concept_member,
        parse_file_head, parse_foreign_member, parse_held, parse_identifier_raw, parse_keyword,
        parse_label, parse_let_member, parse_literal, parse_name, parse_sort, parse_struct_member,
        parse_term, parse_terminator, parse_test_member, parse_top_induct_body, take_comments,
        trimmed, with_span,
    },
    crate::{
        CoreEntry, CoreFile, CoreForm, CoreHead, CoreIntrinsic, CoreItem, CoreNominals, CoreText,
        CoreValue, Decorated, DecoratorField, Label, MatchPattern, Subterm, Term, TopConcept,
        TopInduct, TopLet, TopStruct, parse_whitespace,
    },
    curios_common::{Keyword, Qualifier, Source, Span, is_identifier_char},
    curios_core::{Global, HeadKey, WitnessId},
    curios_parse::{
        Parsed, Parser, ParserError, ParserState, commit, fail, look_ahead, many0, mark, not_ahead,
        pure, run_parser, sep_by0_trailing, sep_by1_trailing, spanned, tagging, take_eof,
        take_exact, take_while,
    },
    std::{str::FromStr, sync::Arc},
};

impl Grammar for CoreText {
    const NAMES_WITNESSES: bool = true;

    fn parse_form<'a>(state: ParserState<'a>) -> Parsed<'a, Term<CoreText>> {
        parse_decorated(state)
    }

    fn parse_atom<'a>(state: ParserState<'a>) -> Parsed<'a, Term<CoreText>> {
        with_span(
            parse_witness_name
                .map(CoreForm::Witness)
                .or(parse_intrinsic.map(CoreForm::Intrinsic))
                .map(|form| Subterm::Core(form).into()),
        )
        .parse(state)
    }

    fn parse_result_sort<'a>(state: ParserState<'a>) -> Parsed<'a, Term<CoreText>> {
        // A sort the head lands in is a sort under its decorator, or a term between parentheses: bare, a name would read the field block after it as a literal of its own.
        with_span(
            parse_decorator
                .and(commit(parse_sort))
                .map(|(fields, term)| {
                    Subterm::Core(CoreForm::Decorated(Decorated { fields, term })).into()
                }),
        )
        .or(parse_sort)
        .or(parse_literal("(")
            .and_keep(parse_term)
            .and_drop(parse_literal(")")))
        .parse(state)
    }

    fn parse_fields<'a>(state: ParserState<'a>) -> Parsed<'a, Vec<DecoratorField>> {
        parse_decorator.or(pure(Vec::new())).parse(state)
    }

    // A dispatch's key no numeral spells, written as the value it is: `#Byte(65)`.
    fn parse_pattern_form<'a>(state: ParserState<'a>) -> Parsed<'a, MatchPattern<CoreText>> {
        parse_intrinsic.map(MatchPattern::Form).parse(state)
    }

    fn declaring<'a, T: 'a>(
        fields: Vec<DecoratorField>,
        rest: impl Parser<'a, T>,
    ) -> impl Parser<'a, ((CoreHead, Label), T)> {
        move |state: ParserState<'a>| {
            let fields = fields.clone();
            parse_declared_name
                .flat_map(move |(name, label)| {
                    let head = CoreHead {
                        fields: fields.clone(),
                        name,
                    };
                    tagging(Some(label.to_string()), rest).map(move |rest| ((head, label), rest))
                })
                .parse(state)
        }
    }
}

/// `#{field = value, …} operand`, committed once `#{` is read: no other form begins with `#{`. The operand is an applied atom, so a decorator states the fields of one node and a second decorator on it is written in parentheses; it is `_` where the node is a variable no binder in the text binds.
fn parse_decorated<'a>(state: ParserState<'a>) -> Parsed<'a, Term<CoreText>> {
    with_span(
        parse_decorator
            .and(commit(parse_held.or(parse_atomic_term)))
            .map(|(fields, term)| {
                Subterm::Core(CoreForm::Decorated(Decorated { fields, term })).into()
            }),
    )
    .parse(state)
}

/// `#{field = value, …}`, committed once `#{` is read: what a decorator states, before an operand, a declaration or a case.
fn parse_decorator<'a>(state: ParserState<'a>) -> Parsed<'a, Vec<DecoratorField>> {
    take_exact("#{")
        .and_drop(parse_whitespace)
        .and_keep(commit(parse_fields_until("}")))
        .parse(state)
}

/// The fields of a decorator or a struct value, then the brace that closes them.
fn parse_fields_until<'a>(close: &'static str) -> impl Parser<'a, Vec<DecoratorField>> {
    move |state: ParserState<'a>| {
        sep_by0_trailing(parse_decorator_field, || parse_literal(","))
            .and_drop(parse_literal(close))
            .parse(state)
    }
}

/// One field a decorator states, `label = value`, as a structure's literal states one: the `=` is guarded against `==` and `=>`, as a labelled field's is.
fn parse_decorator_field<'a>(state: ParserState<'a>) -> Parsed<'a, DecoratorField> {
    parse_label
        .and_drop(field_equals)
        .and(commit(parse_value))
        .map(|(label, value)| DecoratorField { label, value })
        .parse(state)
}

/// The `=` between a field's label and its value.
fn field_equals<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    take_exact("=")
        .and_drop(not_ahead("="))
        .and_drop(not_ahead(">"))
        .and_drop(parse_whitespace)
        .parse(state)
}

/// What a field holds and an intrinsic takes: a struct value, where a `{` opens fields or closes at once; a list; the root module, a `/` that begins no path; or a term.
fn parse_value<'a>(state: ParserState<'a>) -> Parsed<'a, CoreValue> {
    parse_struct_value
        .map(CoreValue::Fields)
        .or(parse_literal("[")
            .and_keep(commit(
                sep_by0_trailing(|| parse_value, || parse_literal(","))
                    .and_drop(parse_literal("]")),
            ))
            .map(CoreValue::List))
        .or(take_exact("/")
            .and_drop(not_ahead_identifier)
            .and_drop(parse_whitespace)
            .map(|()| CoreValue::Root))
        .or(parse_term.map(CoreValue::Term))
        .parse(state)
}

/// `{label = value, …}` or `{}`, told from a tuple type by what follows the brace.
fn parse_struct_value<'a>(state: ParserState<'a>) -> Parsed<'a, Vec<DecoratorField>> {
    parse_literal("{")
        .and_drop(look_ahead(
            parse_literal("}").or(parse_label.and_drop(field_equals).map(|_| ())),
        ))
        .and_keep(commit(parse_fields_until("}")))
        .parse(state)
}

/// Where no identifier character follows.
fn not_ahead_identifier<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    look_ahead(take_while(is_identifier_char))
        .flat_map(|word| match word.is_empty() {
            true => pure(()).boxed(),
            false => fail("a path continues").boxed(),
        })
        .parse(state)
}

/// `#Variant` or `#Variant(operands)`, committed once `#` and a word are read: `#` and a brace is a decorator, and no operand of a program begins with `#`.
fn parse_intrinsic<'a>(state: ParserState<'a>) -> Parsed<'a, CoreIntrinsic> {
    take_exact("#")
        .and_keep(spanned(parse_identifier_raw).map(|(span, word)| Label::spanned(word, span)))
        .and(commit(
            take_exact("(")
                .and_drop(parse_whitespace)
                .and_keep(commit(
                    sep_by0_trailing(parse_value, || parse_literal(","))
                        .and_drop(parse_literal(")")),
                ))
                .map(Some)
                .or(parse_whitespace.map(|()| None)),
        ))
        .map(|(variant, operands)| CoreIntrinsic { variant, operands })
        .parse(state)
}

/// The words a witness key's intrinsic heads are spelled by.
const HEAD_WORDS: &[(&str, HeadKey)] = &[
    ("Nat", HeadKey::Nat),
    ("Byte", HeadKey::Byte),
    ("Int", HeadKey::Int),
    ("Flt", HeadKey::Flt),
    ("Bool", HeadKey::Bool),
    ("Bytes", HeadKey::Bytes),
    ("Handle", HeadKey::Handle),
    ("List", HeadKey::List),
    ("Latch", HeadKey::Latch),
    ("Box", HeadKey::Box),
    ("Io", HeadKey::Io),
];

/// A witness's name: its module's path, then `witness~` and its identity — the concept, the key's heads, and its place where it is not the first at them, `/m/witness~/C(K)#1` — or, for one spelling no key, its place, `/m/witness~3`. Committed once `witness~` is read.
fn parse_witness_name<'a>(state: ParserState<'a>) -> Parsed<'a, WitnessId> {
    parse_witness_module
        .flat_map(|module| commit(parse_witness_identity(module)).and_drop(parse_whitespace))
        .parse(state)
}

/// The module a witness's name begins with, through its `witness~`.
fn parse_witness_module<'a>(state: ParserState<'a>) -> Parsed<'a, Qualifier> {
    take_while(|character| is_identifier_char(character) || character == '/')
        .flat_map(|path: &str| {
            let module = path
                .strip_prefix('/')
                .and_then(|path| path.strip_suffix("witness"))
                .filter(|module| module.is_empty() || module.ends_with('/'));
            match module {
                Some(module) => {
                    let segments = module
                        .split('/')
                        .filter(|segment| !segment.is_empty())
                        .map(str::to_string)
                        .collect::<Vec<_>>();
                    take_exact("~")
                        .map(move |()| Qualifier::from(segments.clone()))
                        .boxed()
                }
                None => fail("not a witness's name").boxed(),
            }
        })
        .parse(state)
}

/// What follows a witness's `witness~`, under `module`.
fn parse_witness_identity<'a>(module: Qualifier) -> impl Parser<'a, WitnessId> {
    move |state: ParserState<'a>| {
        parse_witness_place
            .map(move |place| WitnessId::Unspelled { module, place })
            .or(parse_absolute_path
                .and(
                    take_exact("(")
                        .and_drop(parse_whitespace)
                        .and_keep(sep_by1_trailing(parse_head_key, || parse_literal(",")))
                        .and_drop(take_exact(")")),
                )
                .and(take_exact("#").and_keep(parse_witness_place).or(pure(0)))
                .map(move |((concept, heads), place)| WitnessId::Spelled {
                    concept,
                    key: heads.into(),
                    module,
                    place,
                }))
            .parse(state)
    }
}

/// A witness's place among its module's witnesses: a run of digits.
fn parse_witness_place<'a>(state: ParserState<'a>) -> Parsed<'a, u32> {
    take_while(|character: char| character.is_ascii_digit())
        .flat_map(|digits: &str| match digits.parse::<u32>() {
            Ok(place) => pure(place).boxed(),
            Err(_) => fail("a witness's place is a numeral").boxed(),
        })
        .parse(state)
}

/// `/a/b/c`, read raw.
fn parse_absolute_path<'a>(state: ParserState<'a>) -> Parsed<'a, Qualifier> {
    many0(|| take_exact("/").and_keep(parse_identifier_raw))
        .flat_map(|segments: Vec<&str>| match segments.is_empty() {
            true => fail("Expected an absolute path").boxed(),
            false => pure(Qualifier::from(
                segments.into_iter().map(str::to_string).collect::<Vec<_>>(),
            ))
            .boxed(),
        })
        .parse(state)
}

/// One head of a witness's key: a nominal former by its path, an intrinsic carrier by its word, or a tuple type's shape, `{x: _, _}`.
fn parse_head_key<'a>(state: ParserState<'a>) -> Parsed<'a, HeadKey> {
    parse_absolute_path
        .and_drop(parse_whitespace)
        .map(|path| HeadKey::Nominal(Global::Authored(path)))
        .or(parse_identifier_raw
            .and_drop(parse_whitespace)
            .flat_map(
                |word: &str| match HEAD_WORDS.iter().find(|(spelled, _)| *spelled == word) {
                    Some((_, head)) => pure(head.clone()).boxed(),
                    None => fail("not a key's head").boxed(),
                },
            ))
        .or(parse_literal("{")
            .and_keep(sep_by0_trailing(
                || {
                    parse_literal("_")
                        .map(|()| String::new())
                        .or(parse_identifier_raw
                            .map(str::to_string)
                            .and_drop(parse_literal(":"))
                            .and_drop(parse_literal("_")))
                },
                || parse_literal(","),
            ))
            .and_drop(parse_literal("}"))
            .map(HeadKey::TupleType))
        .parse(state)
}

/// The name a declaration of Core text declares, which is absolute: a path, whose last segment is its label, or a witness's identity, which is its own, its label the identity's own text without the blanks a parse runs on over.
fn parse_declared_name<'a>(state: ParserState<'a>) -> Parsed<'a, (Global, Label)> {
    spanned(parse_witness_name)
        .map(|(span, id)| {
            let span = trimmed(span);
            let label = Label::spanned(&span.source.text()[span.start..span.end], span.clone());

            (Global::Witness(id), label)
        })
        .or(parse_declaring_path)
        .parse(state)
}

/// `/a/b/c` where a declaration's name stands: the name it declares and the label `c`, committed once a `/` begins it.
fn parse_declaring_path<'a>(state: ParserState<'a>) -> Parsed<'a, (Global, Label)> {
    const ABSOLUTE: &str =
        "a declaration of Core text is named by its absolute path: `let /std/Nat/add …`";
    look_ahead(take_exact("/"))
        .map_err(ABSOLUTE)
        .and_keep(commit(parse_name.flat_map(|name| match name.leaf_span() {
            Some(span) if name.is_abs() => {
                let label = Label::spanned(name.last(), span.clone());

                pure((Global::Authored(*name.qualifier()), label)).boxed()
            }
            _ => fail(ABSOLUTE).boxed(),
        })))
        .parse(state)
}

/// The words that head an item of Core text, in the order a refusal lists them.
const CORE_ITEM_HEADS: [Keyword; 6] = [
    Keyword::Let,
    Keyword::Induct,
    Keyword::Struct,
    Keyword::Concept,
    Keyword::Test,
    Keyword::Foreign,
];

/// One item of a file, past the decorator before it.
fn parse_core_item<'a>(state: ParserState<'a>) -> Parsed<'a, CoreItem> {
    CoreText::parse_fields
        .and(mark().and(parse_identifier_raw).and_drop(parse_whitespace))
        .flat_map(|(fields, (start, word))| match Keyword::read(word) {
            Some(Keyword::Test) => commit(parse_test_member(fields))
                .map(CoreItem::Test)
                .boxed(),
            Some(Keyword::Foreign) => commit(parse_foreign_member(None, fields, false))
                .map(CoreItem::Foreign)
                .boxed(),
            Some(Keyword::Let) => commit(parse_lets(fields)).map(CoreItem::Lets).boxed(),
            Some(Keyword::Induct) => commit(parse_nominals(fields, Keyword::Induct, induct_form))
                .map(|forms| CoreItem::Nominals(CoreNominals::Induct(forms)))
                .boxed(),
            Some(Keyword::Struct) => commit(parse_nominals(fields, Keyword::Struct, struct_form))
                .map(|forms| CoreItem::Nominals(CoreNominals::Struct(forms)))
                .boxed(),
            Some(Keyword::Concept) => {
                commit(parse_nominals(fields, Keyword::Concept, concept_form))
                    .map(|forms| CoreItem::Nominals(CoreNominals::Concept(forms)))
                    .boxed()
            }
            _ => {
                let heads = CORE_ITEM_HEADS.map(|head| format!("'{}'", head.spelling()));
                curios_parse::fail_from(
                    &start,
                    format!("Expected an item of Core text: {}", one_of(heads)),
                )
                .boxed()
            }
        })
        .parse(state)
}

/// A group of definitions from its first, read past its `let`: each later one after its decorator and `and`, by its name alone, then the `;` the last owes.
fn parse_lets<'a>(fields: Vec<DecoratorField>) -> impl Parser<'a, Vec<TopLet<CoreText>>> {
    parse_let_member(None, fields, false)
        .and(many0(|| {
            parse_and_head::<CoreText>.flat_map(|head| parse_let_member(None, head.fields, false))
        }))
        .and_drop(parse_terminator("a `let`"))
        .and_drop(parse_whitespace)
        .map(|(first, rest)| std::iter::once(first).chain(rest).collect())
}

/// A group of nominal declarations of one kind from its first, read past its `keyword`: each later one after its decorator, `and` and the same `keyword`, read by `form`.
fn parse_nominals<'a, T: 'a>(
    fields: Vec<DecoratorField>,
    keyword: Keyword,
    form: fn(Vec<DecoratorField>) -> curios_parse::Boxed<'a, T>,
) -> impl Parser<'a, Vec<T>> {
    form(fields)
        .and(many0(move || {
            parse_and_head::<CoreText>
                .and_drop(parse_keyword(keyword))
                .flat_map(move |head| form(head.fields))
        }))
        .map(|(first, rest)| std::iter::once(first).chain(rest).collect())
}

/// A family's form past its `induct`, closed by its `end`.
fn induct_form<'a>(fields: Vec<DecoratorField>) -> curios_parse::Boxed<'a, TopInduct<CoreText>> {
    parse_top_induct_body(None, fields, false)
        .and_drop(parse_keyword(Keyword::End))
        .boxed()
}

/// A record's form past its `struct`, closed by its brace.
fn struct_form<'a>(fields: Vec<DecoratorField>) -> curios_parse::Boxed<'a, TopStruct<CoreText>> {
    parse_struct_member(None, fields, false).boxed()
}

/// A concept's form past its `concept`, closed by its brace.
fn concept_form<'a>(fields: Vec<DecoratorField>) -> curios_parse::Boxed<'a, TopConcept<CoreText>> {
    parse_concept_member(None, fields, false).boxed()
}

/// The entry a program closes with and the type it states on the line beneath it, where the file closes with one.
fn parse_core_entry<'a>(state: ParserState<'a>) -> Parsed<'a, Option<CoreEntry>> {
    parse_term
        .and(
            parse_literal(":")
                .and_keep(parse_term)
                .map(Some)
                .or(pure(None)),
        )
        .map(|(body, type_)| Some(CoreEntry { body, type_ }))
        .or(pure(None))
        .parse(state)
}

/// What the formatter reads a `.crsc` as: its file, each item's span parallel to its items, and every comment the parse consumed, ascending.
pub struct CoreFormatInput {
    pub file: CoreFile,
    pub item_spans: Vec<Span>,
    pub comments: Vec<Span>,
}

/// `source` read whole as a file of Core text, for the formatter: each item's span kept beside it, as a module's are ([`parse_for_format`](crate::parse_for_format)), and the comments the parse consumed.
pub fn parse_core_for_format(source: &Arc<Source>) -> Result<CoreFormatInput, ParserError> {
    clear_comments();
    let (file, item_spans) = CoreFile::parse_spanned(source)?;

    Ok(CoreFormatInput {
        file,
        item_spans,
        comments: take_comments(),
    })
}

impl CoreFile {
    /// The file `source` holds, read whole.
    pub fn parse(source: &Arc<Source>) -> Result<Self, ParserError> {
        Self::parse_spanned(source).map(|(file, _)| file)
    }

    /// [`CoreFile::parse`], each item's span kept beside it in the order of the items: what the formatter lays a file out by, and a reader anchors the module it makes by.
    pub fn parse_spanned(source: &Arc<Source>) -> Result<(Self, Vec<Span>), ParserError> {
        let (items, entry) = run_parser(
            parse_file_head
                .and_keep(many0(|| spanned(parse_core_item)))
                .and(parse_core_entry)
                .and_drop(take_eof()),
            source,
        )?;
        let (spans, items) = items.into_iter().unzip();

        Ok((CoreFile { items, entry }, spans))
    }
}

impl FromStr for CoreFile {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        CoreFile::parse(&Source::inline(input))
    }
}
