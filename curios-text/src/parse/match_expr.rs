use {
    super::{
        members, parse_func_pattern_param, parse_keyword, parse_literal, parse_match_pattern,
        parse_name, parse_pattern, parse_term_before_arrow, trimmed,
    },
    crate::{
        Choose, ChooseArm, ChooseTest, Func, Match, MatrixArm, Pattern, Subterm, Term, parse_term,
        parse_whitespace,
    },
    curios_parse::{
        Parser, commit, fail, fail_from, lazy, look_ahead, many0, mark, not_ahead, pure,
        sep_by0_trailing, spanned, take_exact,
    },
};

pub(super) fn parse_func<'a>() -> Parser<'a, Term> {
    parse_literal("(")
        .and_keep(sep_by0_trailing(parse_func_pattern_param, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal("=>"))
        // Past the arrow this is a lambda and nothing else, so a parameter it refuses and its body own the diagnosis. Left to backtrack, the parameter list would be re-read as a tuple or a parenthesized term and the arrow itself would become the complaint.
        .flat_map(members)
        .and(commit(lazy(parse_term)))
        .map(|(params, body)| Subterm::Func(Func { params, body }).into())
}

/// What refuses a `match` whose arms are written in braces.
const MATCH_TAKES_BARS: &str = "a `match` writes its arms after bars and closes with `end`, with no braces: `match n | 0 => … | _ => … end`";

// A `match` written as another language writes it, `match n { 0 => …, _ => … }`, reads as a scrutinee that is a structure literal, which commits at its brace and reports the `}` it wanted at the first arrow. Probed before the scrutinee: a name, a brace, a pattern that opens with no parenthesis and an arrow are no literal's beginning — an entry is a term, and a lambda's parameters are parenthesized — so the refusal is exact, and stands at the brace.
fn refuse_braced_match<'a>() -> Parser<'a, ()> {
    look_ahead(
        parse_name()
            .and_drop(parse_literal("{"))
            .and_drop(not_ahead("("))
            .and_drop(parse_match_pattern)
            .and_drop(take_exact("=>")),
    )
    .map(|_| true)
    .or(pure(false))
    .flat_map(|braced| match braced {
        true => parse_name()
            .and_keep(mark)
            .and_drop(take_exact("{"))
            .flat_map(|start| commit(fail_from(&start, MATCH_TAKES_BARS))),
        false => pure(()),
    })
}

// `match head [: motive]`. The motive is an ordinary term — a term of the eliminator's motive type, with no grammar of its own and so no disambiguation rule: `parse_term` is called directly and there is nothing to backtrack. It cannot run into the arms because `|` is not an infix operator (`parse_infix_symbol` has `||` but no bare `|`), so a motive term always terminates at the first `|`.
pub(super) fn parse_match_prefix<'a>() -> Parser<'a, (Term, Option<Term>)> {
    parse_keyword("match")
        .and_keep(refuse_braced_match)
        .and_keep(lazy(parse_term))
        .and(
            parse_literal(":")
                // The `:` is what introduces a motive, so past it a term must follow. Recoverable, a malformed motive would be read as no motive at all and the arms' `end` would complain at the colon.
                .and_keep(commit(lazy(parse_term)))
                .map(Some)
                .or(pure(None)),
        )
}

// A match-arm: `| pattern => body`, where `pattern` may nest across constructors, tuples, and structs (see `MatchPattern`, `parse_match_pattern`). A bare binder arm is legal alone (equivalent to a `let`), a binder may stand beside a concrete pattern in any column, and a final bare `_` is the row that cannot fail. Arms have no priority order, so two may match one value only where the first is a case of the other (`curios-elab`'s matrix compiler).
pub(super) fn parse_induct_match_branch<'a>() -> Parser<'a, MatrixArm> {
    parse_literal("|")
        .and_keep(commit(
            spanned(parse_match_pattern)
                .and_drop(parse_literal("=>"))
                .and(lazy(parse_term)),
        ))
        .map(|((span, pattern), body)| MatrixArm {
            pattern,
            body,
            span: Some(trimmed(span)),
        })
}

// The `; ih` induction-hypothesis tail on a `Nat` succ or `List`/`Bin` cons leaf pattern: `;` sets the hypothesis apart from the scrutinee's shape. The hypothesis binds the *fold result*, not scrutinee shape, so it takes the same irrefutable pattern grammar a `let` binder does — `; ih` a plain name, `; (cur, live)` a destructuring. A plain case-split needs no hypothesis, so the tail may be omitted — `None`, not a placeholder; lowering mints a fresh internal name for it directly.
pub(super) fn parse_cons_ih<'a>() -> Parser<'a, Option<Pattern>> {
    parse_literal(";")
        .and_keep(parse_pattern)
        .map(Some)
        .or(pure(None))
}

// A bind arm `| pattern = value => body` (Rust `if let`): the arm fires when `value` matches the refutable `pattern`. The `=` separator is guarded against `==` (equality) and `=>` (an empty value) via `not_ahead`, mirroring the `!` vs `!=` idiom. Tried *after* the condition arm, since a bind pattern like `some(x)` also parses as a condition term and the condition arm cannot take a bind arm from it: `=` is no infix operator, so a condition term stops there and the arm fails wanting its `=>`. Reading it second is what lets it keep its commitments — a pattern the grammar refuses by name, a qualified constructor above all, is then the diagnosis instead of being re-read as an ordinary call and reported at the `=` that follows it.
pub(super) fn parse_bind_arm<'a>() -> Parser<'a, ChooseArm> {
    parse_literal("|")
        .and_keep(parse_match_pattern)
        .and_drop(
            take_exact("=")
                .and_drop(not_ahead("="))
                .and_drop(not_ahead(">")),
        )
        .and_drop(parse_whitespace)
        .and(lazy(parse_term_before_arrow))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|((pattern, value), body)| ChooseArm {
            test: ChooseTest::Bind { pattern, value },
            body,
        })
}

// A choose condition arm `| cond => body`. The catch spans through `=>` so a condition term that merely *starts* like the default backtracks cleanly. A bare `_` parses as an ordinary `Name` term, so the choose's `| _ =>` default would otherwise be swallowed here; the `flat_map` guard rejects a lone `_` condition, letting `many0` stop and hand the default to `parse_choose_default`. A condition that merely *begins* with `_` (e.g. `_ready`) has a longer head and passes the guard, parsing as an ordinary condition.
pub(super) fn parse_cond_arm<'a>() -> Parser<'a, ChooseArm> {
    parse_literal("|")
        .and_keep(lazy(parse_term_before_arrow))
        .flat_map(|cond| match cond.as_subterm() {
            Subterm::Name(name) if name.is_single() && name.head() == "_" => {
                fail("the `| _ =>` default is not a condition arm")
            }
            _ => pure(cond),
        })
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|(condition, body)| ChooseArm {
            test: ChooseTest::Cond(condition),
            body,
        })
}

// One choose arm: a condition arm first, else a bind arm. The condition arm is the reading that cannot be mistaken — its term stops at a `=`, which is no infix operator — so trying it first costs the bind arm nothing and leaves the bind arm free to commit.
pub(super) fn parse_choose_arm<'a>() -> Parser<'a, ChooseArm> {
    parse_cond_arm().or(parse_bind_arm)
}

// The mandatory `| _ =>` default arm closing a `choose` — a `Bool` ladder is a dispatch form (it enumerates no shapes), so `_` is required, not optional — or the arm parser explaining why what stands here is no arm.
//
// **A bare default here would report the wrong thing.** `many0` drops a recoverable arm failure (`curios_parse`'s repetition keeps only uncaught ones), leaving the default to invent `Expected '_'` at the arm's first token: a missing `=>`, or a pattern the arm grammar refuses, reported as a default the reader never meant to write. Re-running the arm parser recovers the diagnosis it already had, and [`Parser::or`] then keeps whichever error reached further, so a genuinely absent default still reports as one — both alternatives fail at the same `|` and the first takes the tie. The alternative never succeeds, since the loop stopped here precisely because the arm parser failed, so no arm is parsed twice. This is `Module::parse_items_end`'s recovery, for the same reason.
pub(super) fn parse_choose_default<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_keep(parse_literal("_"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
        .or(lazy(parse_choose_arm).map(|arm| arm.body))
}

// `choose | test => body | … | _ => default end`. No head term — the arms are `Bool` conditions or refutable binds, tried top-to-bottom, first to fire wins. Distinguished from `match` by keyword alone, so there is no disambiguation to speak of here.
pub(super) fn parse_choose<'a>() -> Parser<'a, Term> {
    parse_keyword("choose")
        .and_keep(many0(parse_choose_arm).and(parse_choose_default))
        .and_drop(parse_keyword("end"))
        .map(|(arms, default)| Subterm::Choose(Choose { arms, default }).into())
}

// A `match` is exactly one surface shape: the general headed pattern matrix. Every headed carrier form — `Bool`, `Nat` (induction *and* literal dispatch), `List`, `Bin` — is just a matrix whose arm patterns are that carrier's leaves (see `parse_match_pattern`), compiled by `curios-elab`'s matrix compiler. The headless ladder is `choose` (`parse_choose`), parsed separately. Zero arms are legal: under index inversion every impossible arm is silently omittable, and a scrutinee whose indices clash with *every* constructor's target eliminates with no arms at all.
pub(super) fn parse_match<'a>() -> Parser<'a, Term> {
    parse_match_prefix()
        .and(many0(parse_induct_match_branch))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), arms)| Subterm::Match(Match { head, motive, arms }))
        .map(Into::into)
}
