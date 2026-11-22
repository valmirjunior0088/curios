use {
    super::{
        BINDER_FOLLOWS_IMPLICIT, Grammar, MEMO_MATCH_PATTERN, MEMO_PATTERN, PARAMETER_IS_A_PATTERN,
        PATTERN_TAKES_NO_USE, Read, begins_a_name, digit_led, member_ends, members,
        parse_binder_label, parse_block_str, parse_keyword, parse_label, parse_line_str,
        parse_literal, parse_mark, parse_name, parse_named_premise, parse_nat_digits, parse_number,
        parse_place, parse_qualified_name, parse_quoted_char, parse_word_raw, parse_written_word,
        refused, require_space, trimmed, unbindable,
    },
    crate::{
        Binder, Elements, FuncParam, FuncType, FuncTypeParam, Label, MatchPattern,
        MatchPatternField, NatLiteral, Pattern, PatternField, Subterm, Term, TuplePattern,
        parse_term, parse_whitespace,
    },
    curios_common::{Keyword, Plicity, Standing, recurse},
    curios_parse::{
        Parsed, Parser, ParserState, commit, fail, fail_from, is_blank, look_ahead, many0, mark,
        memoize, not_ahead, preceded_by_space, pure, refusal, sep_by0_trailing, spanned,
        take_exact, take_n, take_while, uncommit,
    },
    std::iter,
};

// What a plain or `@` member is where the site declares and a name is optional: `label: type`, its word under what Core text's decorator states of its binder, or the type alone. A decorator no word follows is a decorated type's, read as one.
fn parse_declared_member<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, (D::Fields, Option<Label>, Term<D>)> {
    D::parse_fields
        .and(parse_written_word)
        .and_drop(parse_literal(":"))
        .and(parse_term::<D>)
        .map(|((core, label), type_)| (core, Some(label), type_))
        .or(parse_term::<D>.map(|type_| (D::Fields::default(), None, type_)))
        .parse(state)
}

// One Π-binder: `use Concept(args)`, or under `@` or no mark `label: type` or the type alone. A word no term can read, written with its type, is read and its report held ([`parse_unreadable_binder`]): the list is a lambda's as readily until the arrow after it says it is a function type's, which raises it.
pub fn parse_func_type_param<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, Read<FuncTypeParam<D>>> {
    mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            Plicity::Witness => parse_named_premise(start)
                .map(|read| {
                    read.map(|(core, label, type_)| FuncTypeParam {
                        plicity: Plicity::Witness,
                        core,
                        label,
                        type_,
                    })
                })
                .boxed(),
            Plicity::Explicit | Plicity::Implicit => parse_unreadable_binder::<_, D>
                .or(parse_declared_member.map(move |(core, label, type_)| {
                    Ok(FuncTypeParam {
                        plicity,
                        core,
                        label,
                        type_,
                    })
                }))
                .boxed(),
        })
        .parse(state)
}

pub fn parse_func_type<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    uncommit(
        parse_literal("(")
            .and_keep(sep_by0_trailing(parse_func_type_param, || {
                parse_literal(",")
            }))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    // Past the arrow this is a function type, so a member it refuses is its own to report.
    .flat_map(members)
    .and(parse_term::<D>)
    .map(|(params, output): (Vec<FuncTypeParam<D>>, Term<D>)| {
        Subterm::FuncType(FuncType {
            params: params.into_iter().collect(),
            output,
        })
    })
    .map(Into::into)
    .parse(state)
}

// A binder name: a plain identifier (`_` to ignore). `let`, lambda, function-definition-sugar and field-sugar parameters accept a full `Pattern` instead (see `parse_pattern`); match arms accept a full `MatchPattern` instead (see `parse_match_pattern`).
pub fn parse_binder<'a>(state: ParserState<'a>) -> Parsed<'a, Label> {
    parse_binder_label.parse(state)
}

// A field's label, under the decorator Core text may write before it as before a binder: [`parse_label`], not [`parse_binder`], since a projection reads the label whatever a later field makes of the word — `.Type` reads a field a later field's `Type` never could.
pub fn parse_decorated_label<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Binder<D>> {
    D::parse_fields
        .and(parse_label)
        .map(|(core, label)| Binder { core, label })
        .parse(state)
}

// A binder of a pattern: its word, under the decorator Core text may write before it stating the rest of the label Core keeps for the binder (see [`Binder`]); a program's grammar reads no decorator. A decorator no word follows fails back, so whatever else may stand where the binder does is still read.
pub fn parse_decorated_binder<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Binder<D>> {
    D::parse_fields
        .and(parse_binder)
        .map(|(core, label)| Binder { core, label })
        .parse(state)
}

// The committing prefix of a labeled pattern field: `label =`. It fails recoverably, so a positional field that merely starts with an identifier backtracks cleanly — mirrors `parse_tuple_field_prefix`, with no definition-sugar form (a pattern field is never itself a function).
pub fn parse_pattern_field_prefix<'a>(state: ParserState<'a>) -> Parsed<'a, Label> {
    parse_written_word.and_drop(parse_literal("=")).parse(state)
}

// A tuple-pattern / struct-pattern field: `label = pattern` or a bare positional pattern — the literal mirror of `parse_tuple_field`, with `Term` replaced by `Pattern`. Field-punning (`Name { x, y }`) falls out for free: it is just the positional case where the sub-pattern happens to be a binder matching the field.
pub fn parse_pattern_field<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, PatternField<D>> {
    parse_pattern_field_prefix
        .and(parse_pattern::<D>)
        .map(|(label, value)| PatternField {
            label: Some(label),
            value,
        })
        .or(parse_pattern::<D>.map(|value| PatternField { label: None, value }))
        .parse(state)
}

// A tuple pattern `(p1, p2, …)` / `(label = p, …)`, or `()` of no fields — the literal mirror of `parse_tuple` and `parse_empty_tuple`, with `Term` replaced by `Pattern`. A bare `(p)` with neither a comma nor a label is not a one-element tuple pattern (the language has no such thing, exactly like tuple literals) — it falls through to the parenthesized-pattern case in `parse_pattern` below.
pub fn parse_tuple_pattern<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Pattern<D>> {
    spanned(tuple_pattern_fields::<D>)
        .map(|(span, fields)| {
            Pattern::Tuple(TuplePattern {
                fields,
                span: Some(trimmed(span)),
            })
        })
        .parse(state)
}

// What [`parse_tuple_pattern`] reads between and including its parentheses.
fn tuple_pattern_fields<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, Vec<PatternField<D>>> {
    parse_literal("(")
        .and_keep(parse_pattern_field::<D>)
        .and_drop(parse_literal(","))
        .and(sep_by0_trailing(parse_pattern_field::<D>, || {
            parse_literal(",")
        }))
        .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
        .or(parse_literal("(")
            .and_keep(parse_pattern_field_prefix)
            .and(parse_pattern::<D>)
            .map(|(label, value)| {
                vec![PatternField {
                    label: Some(label),
                    value,
                }]
            }))
        .or(parse_literal("(").map(|_| Vec::new()))
        .and_drop(parse_literal(")"))
        .parse(state)
}

// A struct pattern `Name { p1, p2, … }` / `Name { label = p, … }` — mirrors `parse_struct_lit`, but with no `(args)` head-parameter form: the written head name is descriptive only, never resolved or validated (see `Pattern`).
pub fn parse_struct_pattern<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Pattern<D>> {
    struct_pattern::<D>(false).parse(state)
}

/// What refuses a `let`'s struct pattern that reaches something other than its closing brace.
const UNCLOSED_STRUCT_PATTERN: &[&str] = &[
    "a struct pattern closes with `}`",
    "a `let` reads `Name { … }` as its binder",
];

// A struct pattern's field: [`parse_pattern_field`] under the mark it is written under, a hidden field under `@`, as a literal's entry is. `use` is refused by its rule, where `committed` says the text is a struct pattern's alone: a field is plain or `@`.
fn parse_struct_pattern_field<'a, D: Grammar>(
    committed: bool,
) -> impl Parser<'a, (Plicity, PatternField<D>)> {
    move |state: ParserState<'a>| {
        mark()
            .and(parse_mark)
            .flat_map(move |(start, plicity)| match (plicity, committed) {
                (Plicity::Explicit | Plicity::Implicit, _) => parse_pattern_field::<D>
                    .map(move |field| (plicity, field))
                    .boxed(),
                (Plicity::Witness, true) => commit(fail_from(&start, PATTERN_TAKES_NO_USE)).boxed(),
                (Plicity::Witness, false) => fail_from(&start, PATTERN_TAKES_NO_USE).boxed(),
            })
            .parse(state)
    }
}

// [`parse_struct_pattern`], committed past `Name {` where `committed` says the brace discriminates it. Uncommitted it keeps the token's own refusal, since it is then one guess among the alternatives a sibling may improve on.
fn struct_pattern<'a, D: Grammar>(committed: bool) -> impl Parser<'a, Pattern<D>> {
    let fields = move || {
        sep_by0_trailing(
            move || parse_struct_pattern_field::<D>(committed),
            || parse_literal(","),
        )
    };
    let rest = match committed {
        true => {
            commit(fields().and_drop(parse_literal("}").map_err(UNCLOSED_STRUCT_PATTERN))).boxed()
        }
        false => fields().and_drop(parse_literal("}")).boxed(),
    };

    move |state: ParserState<'a>| {
        parse_name
            .and_drop(parse_literal("{"))
            .and(rest)
            .map(|(head, fields)| Pattern::Struct { head, fields })
            .parse(state)
    }
}

// A `let`'s binder: [`parse_pattern`], with a struct pattern committed once `Name {` is read. Nothing but a pattern follows `let`, so the brace discriminates it and an unclosed one is reported as itself rather than as the signature the plain binder `Name` would have opened. A lambda's parameters, and the definition sugar's, share the prefix with a struct literal a term may hold there — `(Name { x = 1 })`, `f(Name { x = 1 })` — and go on backtracking.
pub fn parse_let_binder<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Pattern<D>> {
    struct_pattern::<D>(true)
        .or(parse_pattern::<D>)
        .parse(state)
}

// A binder pattern at `let`, lambda-parameter, and function-definition-sugar-parameter position (see `Pattern`): a plain name, a tuple pattern, a struct pattern, or a parenthesized pattern (pure grouping, mirroring `parse_parens`). Struct and tuple forms are tried before the bare-name case — not after, as a plain identifier prefix (e.g. `Point` in `Point { z, w = ww }`) would otherwise be consumed by the binder case before the disambiguating `{`/`,`/`=` is ever seen, exactly like `parse_struct_lit` is tried before a bare name at the term level.
pub fn parse_pattern<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Pattern<D>> {
    recurse(|| memoize(MEMO_PATTERN, parse_pattern_inner::<D>).parse(state))
}

fn parse_pattern_inner<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Pattern<D>> {
    parse_struct_pattern::<D>
        .or(parse_tuple_pattern::<D>)
        .or(parse_literal("(")
            .and_keep(parse_pattern::<D>)
            .and_drop(parse_literal(")")))
        .or(parse_ctor_pattern::<D>)
        .or(parse_decorated_binder::<D>.map(Pattern::Binder))
        .parse(state)
}

// A constructor binder pattern `tag(p, …)`, [`parse_ctor_match_pattern`]'s shape over binder patterns, each payload under its mark. Tried before the bare name its tag would otherwise be read as, and never committed: a definition sugar's head, `f(x: T)`, shares the prefix, and a `let` tells the two apart only at the `->` the sugar writes ([`super::expr`]'s `parse_let_group`). A payload under `use` is no pattern, and fails here for the forms sharing the prefix to report.
fn parse_ctor_pattern<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Pattern<D>> {
    let argument = |state: ParserState<'a>| {
        parse_mark
            .flat_map(|plicity| match plicity {
                Plicity::Witness => fail(PATTERN_TAKES_NO_USE[0]).boxed(),
                Plicity::Explicit | Plicity::Implicit => parse_pattern::<D>
                    .map(move |pattern| (plicity, pattern))
                    .boxed(),
            })
            .parse(state)
    };

    parse_written_word
        .and_drop(parse_literal("("))
        .and(sep_by0_trailing(argument, || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(|(tag, args)| Pattern::Variant { tag, args })
        .parse(state)
}

/// What refuses an alternative written inside a pattern.
const ALTERNATIVES_STAND_AT_THE_TOP: &[&str] = &[
    "alternatives stand at the top of an arm, each a whole pattern",
    "write `some('a') | some('b')`, not `some('a' | 'b')`",
];

// A pattern written inside another — a constructor's payload, a tuple's or a struct's field, a parenthesized group. An alternative stands at the top of an arm alone, so a bar after one is refused by name, where it stands, rather than left for the enclosing form to report as the token it did not expect; `||` is no bar and is left to it.
fn parse_inner_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_match_pattern::<D>
        .and_drop(
            look_ahead(parse_literal("|").and_drop(not_ahead("|")))
                .map(|_| true)
                .or(pure(false))
                .flat_map(|alternative| match alternative {
                    true => parse_whitespace
                        .and_keep(mark)
                        .flat_map(|start| commit(fail_from(&start, ALTERNATIVES_STAND_AT_THE_TOP)))
                        .boxed(),
                    false => pure(()).boxed(),
                }),
        )
        .parse(state)
}

// A match-arm field: `label = pattern` or a bare positional pattern — the `MatchPattern` counterpart of `parse_pattern_field`.
pub fn parse_match_pattern_field<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPatternField<D>> {
    parse_pattern_field_prefix
        .and(parse_inner_match_pattern::<D>)
        .map(|(label, value)| MatchPatternField {
            label: Some(label),
            value,
        })
        .or(parse_inner_match_pattern::<D>.map(|value| MatchPatternField { label: None, value }))
        .parse(state)
}

// A tuple match pattern `(p1, p2, …)` / `(label = p, …)`, or `()` — the `MatchPattern` counterpart of `parse_tuple_pattern`.
pub fn parse_tuple_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_literal("(")
        .and_keep(parse_match_pattern_field::<D>)
        .and_drop(parse_literal(","))
        .and(sep_by0_trailing(parse_match_pattern_field::<D>, || {
            parse_literal(",")
        }))
        .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
        .or(parse_literal("(")
            .and_keep(parse_pattern_field_prefix)
            .and(parse_inner_match_pattern::<D>)
            .map(|(label, value)| {
                vec![MatchPatternField {
                    label: Some(label),
                    value,
                }]
            }))
        .or(parse_literal("(").map(|_| Vec::new()))
        .and_drop(parse_literal(")"))
        .map(MatchPattern::Tuple)
        .parse(state)
}

// A struct match pattern `Name { p1, p2, … }` / `Name { label = p, @hidden = p, … }` — the `MatchPattern` counterpart of `parse_struct_pattern`, mirroring struct literals rather than the positional constructor-call shape (structs have field labels; inductive constructors don't — see `MatchPattern`).
pub fn parse_struct_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    // Each field under the mark it is written under, a hidden one under `@`. Nothing but a pattern is read in an arm, so `use` is refused by its rule where it stands, as a constructor pattern's is.
    let field = || {
        mark()
            .and(parse_mark)
            .flat_map(|(start, plicity)| match plicity {
                Plicity::Explicit | Plicity::Implicit => parse_match_pattern_field::<D>
                    .map(move |field| (plicity, field))
                    .boxed(),
                Plicity::Witness => commit(fail_from(&start, PATTERN_TAKES_NO_USE)).boxed(),
            })
    };

    parse_name
        .and_drop(parse_literal("{"))
        .and(sep_by0_trailing(field, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(|(head, fields)| MatchPattern::Struct { head, fields })
        .parse(state)
}

// A constructor match pattern `tag(p, …)` — `nil()` for the nullary case. The `(` immediately after the tag is the commit point, distinguishing it from a bare name. Each argument is itself a full `MatchPattern`, so a constructor's payload can nest arbitrarily (`some(some(x))`, `pair(some(x), y)`, …). Each argument retains its plicity: a payload slot the constructor declared `@` must be matched `@name`. `use` is refused where it stands — a payload is plain or `@`, and no pattern or term begins with the word, so nothing else could be meant.
pub fn parse_ctor_arg<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, (Plicity, MatchPattern<D>)> {
    mark()
        .and(parse_mark)
        .flat_map(|(start, plicity)| match plicity {
            Plicity::Witness => commit(fail_from(&start, PATTERN_TAKES_NO_USE)).boxed(),
            Plicity::Explicit | Plicity::Implicit => parse_inner_match_pattern::<D>
                .map(move |pattern| (plicity, pattern))
                .boxed(),
        })
        .parse(state)
}

pub fn parse_ctor_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_written_word
        .and_drop(parse_literal("("))
        .and(sep_by0_trailing(parse_ctor_arg::<D>, || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(
            |(tag, args): (Label, Vec<(Plicity, MatchPattern<D>)>)| MatchPattern::Variant {
                tag,
                args,
            },
        )
        .parse(state)
}

// A sort's word standing alone where an arm binds, refused at the word: a binder of it binds what nothing reads ([`unbindable`]). Tried first, ahead of the qualified head's refusal, which reads the word as a path of one segment and fails past it, where the binder alternative, last, refuses the word without committing and so loses that tie. The word is only looked at until it is one, so any other fails at the choice point; and one a path, a structure's fields or a constructor's arguments follow is left to those forms.
fn refuse_sort_binder<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, MatchPattern<D>> {
    look_ahead(parse_word_raw)
        .flat_map(|word| {
            let sort = Keyword::read(word).map(Keyword::standing) == Some(Standing::Sort);
            match unbindable(word).filter(|_| sort) {
                Some(why) => mark()
                    .and_drop(parse_word_raw)
                    .and_drop(not_ahead("/"))
                    .and_drop(parse_whitespace)
                    .and_drop(not_ahead("{"))
                    .and_drop(not_ahead("("))
                    .flat_map(move |start| commit(fail_from(&start, why)))
                    .boxed(),
                None => fail("expected: a sort's word standing alone").boxed(),
            }
        })
        .parse(state)
}

// A match pattern written with a qualified head, refused by name.
//
// A constructor pattern names its constructor *bare*: the tag is resolved against the scrutinee's type rather than looked up, so the namespace is never spelled. Left to fall through, `Option/some(n)` parsed `Option` as a `Binder` and the arm then reported `Expected '=>'` against the `/` — with a `=>` plainly written four columns along.
//
// **Tried first, and guarded by the one form that may carry a path.** A struct head *is* a path (`parse_struct_match_pattern`, kept as written for the lowering to resolve), so `not_ahead` hands `Name/Sub { … }` back; nothing else in this grammar begins with one, since `parse_qualified_name` rejects a single segment and so never reaches the `Binder` case. First is what makes the refusal stick: [`Parser::or`] consults the *first* alternative's fatality but picks between two failures on offset alone, and the struct alternative fails at exactly this one's offset — both stop at the delimiter that is not a `{` — so from any later position the tie would keep the vaguer error.
//
// The refusal commits, which is what carries it out of the arm instead of leaving the vaguer complaint of whatever the text falls through to. A bind guard is read after the condition reading so that it can: the condition has already had its turn at text beginning this way, so there is nothing left to re-read and nothing to take the commitment back for.
pub fn parse_qualified_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_qualified_name.and_drop(not_ahead("{")).flat_map(|name| {
        commit(fail(format!(
            "a constructor pattern names its constructor bare: write `{}` rather than `{}`, since the scrutinee's type supplies the namespace",
            name.last(),
            name.join(),
        )))
    }).parse(state)
}

// A nested `Bool` leaf: `true` or `false`. Tried as dedicated keywords before the generic `Binder` fallback in `parse_match_pattern` — `parse_binder` doesn't itself reject keyword text, mirroring the same precedent already used for `Bool` literals at term level (see the `Subterm::Intrinsic(Intrinsic::Bool)` case above).
pub fn parse_bool_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_keyword(Keyword::False)
        .map(|()| MatchPattern::Bool(false))
        .or(parse_keyword(Keyword::True).map(|()| MatchPattern::Bool(true)))
        .parse(state)
}

// The `pred + step` leaf of a `Nat` match-arm pattern. Tried after the constructor pattern and before the generic `Binder` fallback in `parse_match_pattern`: it shares a leading identifier with both, so `Binder` would otherwise silently swallow every `name + 2` input before this ever gets a chance to commit. A space is required on each side of `+` (mirroring `parse_infix_op`'s own space-sensitivity, via the same `preceded_by_space`/`require_space` intrinsics and a `take_exact` operator token that doesn't itself eat trailing whitespace), so a glued `pred+1` is no successor pattern, and neither is a step of none; `refuse_misread_successor` refuses each by name.
pub fn parse_nat_succ_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_decorated_binder::<D>
        .and_drop(preceded_by_space)
        .and_drop(take_exact("+"))
        .and_drop(require_space)
        .and(parse_step)
        .map(
            |(pred_label, step): (Binder<D>, NatLiteral)| MatchPattern::Successor {
                pred_label,
                step,
            },
        )
        .parse(state)
}

// The step a successor pattern takes: a numeral of one or more, read whole, so `10` is a step of ten and not a step of one before a stray `0`.
fn parse_step<'a>(state: ParserState<'a>) -> Parsed<'a, NatLiteral> {
    parse_nat_digits
        .flat_map(|step| match step.0.is_zero() {
            true => fail("a successor pattern takes a step of one or more").boxed(),
            false => pure(step).boxed(),
        })
        .parse(state)
}

// What a successor pattern's step is written in: a numeral's digits, its radix prefix and its separators.
fn is_step_char(char: char) -> bool {
    char.is_ascii_alphanumeric() || char == '_'
}

// A label and a `+` that make no successor pattern, refused by what failed rather than read as the binder `p` with `+…` left over for the arm grammar to report as the token it did not expect: a side of the `+` without its blank, which reads `p+1` as the binder `p` and the literal `+1`, a step of none, which is the binder `p` itself, or a step that is no numeral. Tried after the spaced successor form, and before the binder the label would otherwise become; the `+` is consumed so the refusal is past the choice point, the caret underlining the spacing from the label's end or the step itself.
fn refuse_misread_successor<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, MatchPattern<D>> {
    parse_decorated_binder::<D>
        .and_drop(look_ahead(take_while(is_blank).and(take_exact("+"))))
        .and_keep(mark)
        .flat_map(|start| {
            // The step is consumed too: the spaced form's own refusal, caught, sits right after the `+`, and a committed refusal that ties it on offset loses [`Parser::or`]'s tie-break.
            // Whether a blank stands before the `+` is read off the text behind it, since the label takes the blanks after it.
            take_while(is_blank)
                .and_keep(preceded_by_space().map(|()| true).or(pure(false)))
                .and_drop(take_exact("+"))
                .and(take_while(is_blank))
                .and(mark)
                .and(look_ahead(parse_nat_digits.map(Some).or(pure(None))))
                .and_drop(take_while(is_step_char))
                .flat_map(move |(((spaced, after), step), numeral)| {
                    match (!spaced || after.is_empty(), numeral) {
                        (true, _) => commit(fail_from(
                            &start,
                            &["a successor pattern takes whitespace on both sides of its `+`, as in `p + 1`", "`p+1` is the binder `p` followed by the literal `+1`"],
                        ))
                        .boxed(),
                        // A spaced step the successor form did not take failed past it, where that refusal stands.
                        (false, Some(NatLiteral(value, _))) if !value.is_zero() => {
                            fail("a successor pattern takes a step of one or more").boxed()
                        }
                        (false, Some(_)) => commit(fail_from(
                            &step,
                            &["a successor pattern takes a step of one or more, as in `p + 2`", "`p + 0` is the binder `p`"],
                        ))
                        .boxed(),
                        (false, None) => commit(fail_from(
                            &step,
                            &["a successor pattern's step is a numeral, as in `p + 2`"],
                        ))
                        .boxed(),
                    }
                })
        })
        .parse(state)
}

// A character literal leaf — a `Nat` dispatch case spelled by its scalar value, compiled exactly as the numeral it denotes.
pub fn parse_char_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_quoted_char
        .and_drop(parse_whitespace)
        .map(MatchPattern::Char)
        .parse(state)
}

// A string literal leaf, one line or a block, kept in the spelling it was written in: a `Str` taken apart by its bytes.
pub fn parse_str_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_block_str
        .or(parse_line_str)
        .map(MatchPattern::Str)
        .parse(state)
}

// A numeral leaf: the numeral a term writes, its sign and its radix included, which the matrix compiler realizes at its column's type as the term is realized at its expected one. A floating-point literal is refused by what it is, with the guard that tests it: a pattern tests a value's identity, which tells `+0.0` from `-0.0` and one NaN from another, where `==` does neither. Read only where a sign or a digit leads, so text no numeral begins, a run's `..` among it, fails here at its start and keeps its own refusal; a float is read whole and refused where it starts, so a payload written as one is no constructor's list gone wrong.
fn parse_numeral_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    look_ahead(take_n(1))
        .flat_map(|lead: &str| {
            match lead.starts_with(|char: char| char.is_ascii_digit() || char == '-' || char == '+')
            {
                true => pure(()).boxed(),
                false => fail("a numeral begins with a sign or a digit").boxed(),
            }
        })
        .and_keep(mark())
        .and(spanned(parse_number::<D>))
        .flat_map(|(start, (span, literal))| match &*literal {
            Subterm::NumLit(numeral) => pure(MatchPattern::Numeral(numeral.clone())).boxed(),
            _ => {
                let written = span.source.text()[span.start..span.end].trim_end();
                commit(fail_from(
                    &start,
                    vec![
                        "a floating-point literal is no pattern: a pattern tests identity, which tells `+0.0` from `-0.0` where `==` does not".to_string(),
                        format!("bind the value and test it in a guard: `| x, x == {written} =>`"),
                    ],
                ))
                .boxed()
            }
        })
        .parse(state)
}

// A run, the literal that builds a sequence: `[]`, `[a, b]` or `[a, b, ..rest]` after `open`, `[` for a list and `x[` for a packed sequence, each element a pattern and an open run's rest, after one element at least, a binder. Nothing but a run follows its bracket, so it commits there, and once `..` is read only a binder can follow: what fails inside is reported where it stands.
fn parse_run<'a, D: Grammar>(open: &'static str) -> impl Parser<'a, Elements<D>> {
    move |state: ParserState<'a>| {
        parse_literal(open)
            .and_keep(commit(
                // Entries first: where neither reads, the one reported is the element a run begins with.
                parse_run_entries::<D>
                    .and_drop(parse_literal("]"))
                    .or(parse_literal("]").map(|()| Elements::Closed(Vec::new()))),
            ))
            .parse(state)
    }
}

// A nonempty run's entries: its elements, then its rest where it is open, and one trailing comma.
fn parse_run_entries<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Elements<D>> {
    parse_inner_match_pattern::<D>
        .and(many0(|| {
            parse_literal(",").and_keep(parse_inner_match_pattern::<D>)
        }))
        .and(
            parse_literal(",")
                .and_drop(parse_literal(".."))
                .and_keep(commit(parse_decorated_binder::<D>))
                .map(Some)
                .or(pure(None)),
        )
        .and_drop(parse_literal(",").or(pure(())))
        .map(|((first, then), rest)| match rest {
            Some(rest) => Elements::Open {
                first: Box::new(first),
                then,
                rest,
            },
            None => Elements::Closed(iter::once(first).chain(then).collect()),
        })
        .parse(state)
}

// A run over a list.
pub fn parse_list_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_run::<D>("[").map(MatchPattern::List).parse(state)
}

// A run over a packed sequence, its bracket `x[` glued as a packed literal's is.
pub fn parse_bytes_match_pattern<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    parse_run::<D>("x[").map(MatchPattern::Bytes).parse(state)
}

// A match-arm pattern: a plain binder, an inductive constructor applied to (possibly nested) sub-patterns, a tuple pattern, a struct pattern, a literal leaf — a `Bool`, a character, a string or a numeral — a successor, or a `List`/`Bytes` run — see `MatchPattern`. Struct and constructor forms are tried before the bare-name case for the same reason `parse_pattern` tries `Struct`/`Tuple` first: a plain identifier prefix (`Point` in `Point { z, w = ww }`, `some` in `some(x)`) would otherwise be consumed by the binder case before the disambiguating `{`/`(` is ever seen. The literal leaves are tried before `Tuple` (none of their prefixes — `[`, `x[`, a sign, a digit, `true`/`false` — overlap `Tuple`'s `(`) and, for the successor leaf (`parse_nat_succ_match_pattern`) specifically, before `Binder` (see its own doc comment). A run owns what follows its bracket. A qualified head is refused ahead of all of them, for the reason `parse_qualified_match_pattern` states.
pub fn parse_match_pattern<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, MatchPattern<D>> {
    recurse(|| memoize(MEMO_MATCH_PATTERN, parse_match_pattern_inner::<D>).parse(state))
}

fn parse_match_pattern_inner<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatchPattern<D>> {
    refuse_sort_binder::<D>
        .or(parse_qualified_match_pattern::<D>)
        .or(parse_bytes_match_pattern::<D>)
        .or(parse_struct_match_pattern::<D>)
        .or(parse_ctor_match_pattern::<D>)
        .or(parse_bool_match_pattern::<D>)
        .or(parse_char_match_pattern::<D>)
        .or(parse_str_match_pattern::<D>)
        .or(parse_numeral_match_pattern::<D>)
        .or(D::parse_pattern_form)
        .or(parse_nat_succ_match_pattern::<D>)
        .or(parse_list_match_pattern::<D>)
        .or(parse_tuple_match_pattern::<D>)
        .or(parse_literal("(")
            .and_keep(parse_inner_match_pattern::<D>)
            .and_drop(parse_literal(")")))
        .or(refuse_misread_successor::<D>)
        .or(parse_decorated_binder::<D>.map(MatchPattern::Binder))
        .parse(state)
}

// The optional `: type` after a binder.
fn parse_annotation<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Option<Term<D>>> {
    parse_literal(":")
        .and_keep(parse_term::<D>)
        .map(Some)
        .or(pure(None))
        .parse(state)
}

// What a member is where the site binds, under each mark: `use _`, the member's place; after `@`, a binder with an optional annotation, a type in its place being read and reported; and with no mark the binder as `bound` reads it, a term no binder reads, a word that begins with a digit among them, being read and reported. A report is held, and raised only where the list is seen to be parameters, so the list is still read as a call's or a parenthesized term's where it is one. Shared by a lambda's parameters and the definition sugar's, which differ only in what a binder is.
fn parse_bound_member<'a, B: 'a, D: Grammar>(
    bound: impl Parser<'a, B> + Copy + 'a,
    wildcard: fn() -> B,
) -> impl Parser<'a, Read<(Plicity, B, Option<Term<D>>)>> {
    move |state: ParserState<'a>| {
        mark()
            .and(parse_mark)
            .flat_map(move |(start, plicity)| match plicity {
                // A witness binder Core text writes is the variable its body reads it by, over its domain.
                Plicity::Witness if D::NAMES_WITNESSES => bound
                    .and(parse_annotation)
                    .and_drop(member_ends)
                    .map(|(binder, annotation)| Ok((Plicity::Witness, binder, annotation)))
                    .boxed(),
                Plicity::Witness => parse_place::<D>(start)
                    .map(move |read| read.map(|()| (Plicity::Witness, wildcard(), None)))
                    .boxed(),
                Plicity::Implicit => bound
                    .and(parse_annotation)
                    .and_drop(member_ends)
                    .map(|(binder, annotation)| Ok((Plicity::Implicit, binder, annotation)))
                    .or(parse_unreadable_binder::<_, D>.and_drop(member_ends))
                    .or(parse_term::<D>.flat_map(move |_| refused(&start, BINDER_FOLLOWS_IMPLICIT)))
                    .boxed(),
                // A binder its list's separator follows; a word that begins with a digit, likewise; any other term the separator follows, read only to be reported; and last the binder alone, so a list that cannot go on past it says so where it stops.
                Plicity::Explicit => bound
                    .and(parse_annotation)
                    .and_drop(member_ends)
                    .map(|(binder, annotation)| Ok((Plicity::Explicit, binder, annotation)))
                    .or(parse_numbered_binder::<_, D>.and_drop(member_ends))
                    .or(parse_unreadable_binder::<_, D>.and_drop(member_ends))
                    // Uncommitted: a refusal the term's own forms commit to — a call's list that does not close — is no refusal of the list it stands in, which a tuple's or a call's reading may still take.
                    .or(uncommit(parse_term::<D>)
                        .and_drop(member_ends)
                        .flat_map(move |_| refused(&start, PARAMETER_IS_A_PATTERN)))
                    .or(bound
                        .and(parse_annotation)
                        .map(|(binder, annotation)| Ok((Plicity::Explicit, binder, annotation))))
                    .boxed(),
            })
            .parse(state)
    }
}

// A word that begins with a digit where a member binds, read with the annotation after it and reported: no name begins with one. A parenthesized number is a term until an arrow follows it, so the report is held as every refused member's is.
pub fn parse_numbered_binder<'a, T: 'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Read<T>> {
    mark()
        .and(parse_word_raw)
        .flat_map(|(start, word)| match begins_a_name(word) {
            true => fail("expected: a word that begins with a digit").boxed(),
            false => refusal(&start, digit_led(word)).boxed(),
        })
        .and_drop(parse_whitespace)
        .and_drop(parse_annotation::<D>)
        .map(Err)
        .parse(state)
}

// A reserved word or a sort where a member binds with its type, read with the annotation and reported, as a numbered one is ([`parse_numbered_binder`]): the binder alternative refuses the word without committing, so that a term may still begin with it, and the member written as its type alone would otherwise be tried in its place and fail at the `:`. Without a `:` after it the word may be the member's type, which that refusal names.
pub fn parse_unreadable_binder<'a, T: 'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, Read<T>> {
    D::parse_fields
        .and_keep(mark())
        .and(parse_word_raw)
        .and_drop(look_ahead(parse_whitespace.and(parse_literal(":"))))
        .flat_map(|(start, word)| match unbindable(word) {
            Some(why) => refusal(&start, why).boxed(),
            None => fail("expected: a word no term can read").boxed(),
        })
        .and_drop(parse_whitespace)
        .and_drop(parse_annotation::<D>)
        .map(Err)
        .parse(state)
}

// A lambda parameter with an optional domain annotation and a binder pattern, which the definition sugar `label(params) = value` of a tuple, a structure and a witness reads too (`parse_tuple_field_prefix`), since the sugar lowers to a lambda. It retains its mark — `@name`, or `use _` — so a hidden-binder field type (`pure : (@A, x) -> M(A)`) can be implemented as `pure(@A, x) = …` rather than losing the mark. `(x)` is sugar for `(x : _)`; the annotation, when present, parses as an arbitrary term and stops at the closing `)` (mirrors `parse_func_type_param`).
pub fn parse_func_pattern_param<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, Read<FuncParam<D>>> {
    parse_bound_member(parse_pattern::<D>, || Pattern::Binder("_".into()))
        .map(|read| {
            read.map(|(plicity, pattern, annotation)| FuncParam {
                plicity,
                pattern,
                annotation,
            })
        })
        .parse(state)
}
