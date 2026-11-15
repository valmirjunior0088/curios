use {
    super::{
        Grammar, members, parse_func_pattern_param, parse_infix_expr, parse_keyword, parse_literal,
        parse_match_pattern, parse_name, trimmed, with_span,
    },
    crate::{
        Alternative, Func, Guard, HeadlessArm, HeadlessMatch, Match, MatrixArm, Subterm, Term,
        parse_term, parse_whitespace,
    },
    curios_parse::{
        Parsed, Parser, ParserState, commit, fail_from, look_ahead, many0, mark, not_ahead, pure,
        sep_by0_trailing, spanned, take_exact, uncommit,
    },
};

pub fn parse_func<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    parse_literal("(")
        .and_keep(sep_by0_trailing(parse_func_pattern_param, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal("=>"))
        // Past the arrow this is a lambda and nothing else, so a parameter it refuses and its body own the diagnosis. Left to backtrack, the parameter list would be re-read as a tuple or a parenthesized term and the arrow itself would become the complaint.
        .flat_map(members)
        .and(commit(parse_term::<D>))
        .map(|(params, body)| Subterm::Func(Func { params, body }).into())
        .parse(state)
}

/// What refuses a `match` whose arms are written in braces.
const MATCH_TAKES_BARS: &[&str] = &[
    "a `match` writes its arms after bars and closes with `end`, with no braces",
    "write `match n | 0 => … | _ => … end`",
];

// A `match` written as another language writes it, `match n { 0 => …, _ => … }`, reads as a scrutinee that is a structure literal, which commits at its brace and reports the `}` it wanted at the first arrow. Probed before the scrutinee: a name, a brace, a pattern that opens with no parenthesis and an arrow are no literal's beginning — an entry is a term, and a lambda's parameters are parenthesized — so the refusal is exact, and stands at the brace.
//
// A refusal the pattern grammar commits to stays in the probe: where the scrutinee is a structure literal, what stands in its braces is a term, `match Wrap { Option/some(7) } | …`, which no pattern's rule judges.
fn refuse_braced_match<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, ()> {
    look_ahead(
        parse_name
            .and_drop(parse_literal("{"))
            .and_drop(not_ahead("("))
            .and_drop(uncommit(parse_match_pattern::<D>))
            .and_drop(take_exact("=>")),
    )
    .map(|_| true)
    .or(pure(false))
    .flat_map(|braced| match braced {
        true => parse_name
            .and_keep(mark)
            .and_drop(take_exact("{"))
            .flat_map(|start| commit(fail_from(&start, MATCH_TAKES_BARS)))
            .boxed(),
        false => pure(()).boxed(),
    })
    .parse(state)
}

// The head of a `match` past its keyword, `head [: motive]`. The motive is an ordinary term — a term of the eliminator's motive type, with no grammar of its own and so no disambiguation rule: `parse_term` is called directly and there is nothing to backtrack. It cannot run into the arms because `|` is not an infix operator (`parse_infix_symbol` has `||` but no bare `|`), so a motive term always terminates at the first `|`.
fn parse_match_head<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, (Term<D>, Option<Term<D>>)> {
    refuse_braced_match::<D>
        .and_keep(parse_term::<D>)
        .and(
            parse_literal(":")
                // The `:` is what introduces a motive, so past it a term must follow. Recoverable, a malformed motive would be read as no motive at all and the arms' `end` would complain at the colon.
                .and_keep(commit(parse_term::<D>))
                .map(Some)
                .or(pure(None)),
        )
        .parse(state)
}

// One of an arm's alternatives: a pattern, beside where it is written.
fn parse_alternative<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Alternative<D>> {
    spanned(parse_match_pattern::<D>)
        .map(|(span, pattern)| Alternative {
            pattern,
            span: Some(trimmed(span)),
        })
        .parse(state)
}

// An alternative after the first: a term has no bare `|`, so past a pattern a bar can be nothing but another alternative, and what follows it must be one.
fn parse_later_alternative<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Alternative<D>> {
    parse_literal("|")
        .and_keep(commit(parse_alternative::<D>))
        .parse(state)
}

// The alternatives of an arm: one pattern, then another after each `|`, up to what follows them.
fn parse_alternatives<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Vec<Alternative<D>>> {
    parse_alternative::<D>
        .and(many0(parse_later_alternative::<D>))
        .map(|(first, mut rest)| {
            rest.insert(0, first);
            rest
        })
        .parse(state)
}

// A match-arm: `| pattern | … , guard … => body`, where each `pattern` may nest across constructors, tuples, and structs (see `MatchPattern`, `parse_match_pattern`). A bare binder arm is legal alone (equivalent to a `let`), a binder may stand beside a concrete pattern in any column, and a final bare `_` is the row that cannot fail. Arms have no priority order, so two may match one value only where the first is a case of the other, and the alternatives of one arm match no value in common (`curios-elab`'s matrix compiler). A comma past the alternatives can open nothing but a guard.
pub fn parse_induct_match_branch<'a, D: Grammar>(
    state: ParserState<'a>,
) -> Parsed<'a, MatrixArm<D>> {
    parse_literal("|")
        .and_keep(commit(
            parse_alternatives::<D>
                .and(parse_later_guards::<D>)
                .and_drop(parse_literal("=>"))
                .and(parse_term::<D>),
        ))
        .map(|((alternatives, guards), body)| MatrixArm {
            alternatives,
            guards,
            body,
        })
        .parse(state)
}

// A guard's term: an operand, an infix chain and no wider, as an operator's operand is. The `=>` that ends an arm's guards would otherwise be read as a lambda's arrow wherever what precedes it can be a lambda's parameters — `(o, p) => x` — and a `let`, a lambda or a function type stands in a guard in parentheses, as it does beside an operator.
fn parse_guard_term<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    with_span(parse_infix_expr::<D>(0)).parse(state)
}

// A condition guard: a term no bind's `=` follows. Read first and uncommitted: a bind guard's pattern may begin as no term does — `some(_)`, whose `_` a call's arguments refuse once inside their parentheses — and the bind reading must still have its turn; a term that runs on into a `=` is no condition, the `=` being no operator. What else follows is the arm's to diagnose, as a missing `=>`.
fn parse_holds_guard<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Guard<D>> {
    uncommit(parse_guard_term::<D>)
        .and_drop(not_ahead("=").or(look_ahead(take_exact("=>"))))
        .map(Guard::Holds)
        .parse(state)
}

// A bind guard `pattern = value`, committed once its `=` is read — guarded against `==` and `=>` as the bind arm's is. Read after the condition reading, so the pattern grammar keeps its commitments: a pattern it refuses by name, a qualified constructor above all, is the diagnosis.
fn parse_bind_guard<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Guard<D>> {
    parse_alternative::<D>
        .and_drop(
            take_exact("=")
                .and_drop(not_ahead("="))
                .and_drop(not_ahead(">")),
        )
        .and_drop(parse_whitespace)
        .and(commit(parse_guard_term::<D>))
        .map(|(pattern, value)| Guard::Binds {
            pattern: Box::new(pattern),
            value,
        })
        .parse(state)
}

// One guard: a condition, or a bind where the text is a pattern and a `=`.
fn parse_guard<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Guard<D>> {
    parse_holds_guard::<D>
        .or(parse_bind_guard::<D>)
        .parse(state)
}

// The guards after an arm's first, or after its patterns: each opened by a comma, past which a guard must follow.
fn parse_later_guards<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Vec<Guard<D>>> {
    many0(|| parse_literal(",").and_keep(commit(parse_guard::<D>))).parse(state)
}

// An arm of a match with no head, `| guard, … => body`. Its first guard is read uncommitted, so the default's `| _ =>` — no term, and a pattern that meets no `=` — leaves the arms for `parse_headless_default`; past it the arm is a guarded arm and nothing else.
fn parse_headless_arm<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, HeadlessArm<D>> {
    parse_literal("|")
        .and_keep(parse_guard::<D>)
        .and(commit(
            parse_later_guards::<D>
                .and_drop(parse_literal("=>"))
                .and(parse_term::<D>),
        ))
        .map(|(first, (mut guards, body))| {
            guards.insert(0, first);
            HeadlessArm { guards, body }
        })
        .parse(state)
}

/// What refuses a match with no head that closes with no default arm.
const HEADLESS_MATCH_TAKES_A_DEFAULT: &[&str] = &[
    "a `match` with no head ends with a default arm, taken where no arm above it holds",
    "write `| _ => …` before `end`",
];

// The default closing a match with no head, `| _ => body` — guards alone enumerate no shapes, so `_` is required, not optional — or the arm parser explaining why what stands here is no arm.
//
// **A bare default here would report the wrong thing.** `many0` drops a recoverable arm failure (`curios_parse`'s repetition keeps only uncaught ones), leaving the default to invent `Expected '_'` at the arm's first token: a missing `=>`, or a pattern the guard grammar refuses, reported as a default the reader never meant to write. Re-running the arm parser recovers the diagnosis it already had, and [`Parser::or`] then keeps whichever error reached further, so a genuinely absent default still reports as one — both alternatives fail at the same `|` and the first takes the tie. The alternative never succeeds, since the loop stopped here precisely because the arm parser failed, so no arm is parsed twice. This is `Module::parse_items_end`'s recovery, for the same reason.
//
// **An `end` here is a match with no head and no default.** The arms are read and the word that closes the form stands where the default does, so the rule is what is broken, and it is refused by name at that word, where a bare default would report the bar it wanted and a letter of `end`.
fn parse_headless_default<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    look_ahead(parse_keyword("end"))
        .map(|()| true)
        .or(pure(false))
        .flat_map(|ended| match ended {
            true => mark()
                .and_drop(take_exact("end"))
                .flat_map(|start| commit(fail_from(&start, HEADLESS_MATCH_TAKES_A_DEFAULT)))
                .boxed(),
            false => parse_literal("|")
                .and_keep(parse_literal("_"))
                .and_drop(parse_literal("=>"))
                .and_keep(parse_term::<D>)
                .or(parse_headless_arm::<D>.map(|arm| arm.body))
                .boxed(),
        })
        .parse(state)
}

// A match with no head, past its keyword: guarded arms, then the default, then `end`.
fn parse_headless_match<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    many0(parse_headless_arm::<D>)
        .and(parse_headless_default::<D>)
        .and_drop(parse_keyword("end"))
        .map(|(arms, default)| Subterm::HeadlessMatch(HeadlessMatch { arms, default }).into())
        .parse(state)
}

// A headed match, past its keyword: the head, the arms, then `end`.
fn parse_headed_match<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    parse_match_head::<D>
        .and(many0(parse_induct_match_branch::<D>))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), arms)| Subterm::Match(Match { head, motive, arms }).into())
        .parse(state)
}

// A `match` is the pattern matrix, headed or not. Every headed carrier form — `Bool`, `Nat` (induction *and* literal dispatch), `List`, `Bytes` — is just a matrix whose arm patterns are that carrier's leaves (see `parse_match_pattern`), compiled by `curios-elab`'s matrix compiler, and a match with no head is the matrix of no columns. A bar right after the keyword is what tells the two apart, since no head begins with one. Zero arms are legal under a head: under index inversion every impossible arm is silently omittable, and a scrutinee whose indices clash with *every* constructor's target eliminates with no arms at all.
pub fn parse_match<'a, D: Grammar>(state: ParserState<'a>) -> Parsed<'a, Term<D>> {
    parse_keyword("match")
        .and_keep(look_ahead(parse_literal("|")).map(|_| true).or(pure(false)))
        .flat_map(|headless| match headless {
            true => parse_headless_match::<D>.boxed(),
            false => parse_headed_match::<D>.boxed(),
        })
        .parse(state)
}
