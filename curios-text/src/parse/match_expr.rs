use super::*;

pub(super) fn parse_func<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0_trailing(parse_func_pattern_param, || {
                parse_literal(",")
            }))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(params, body)| Subterm::Func(Func { params, body }).into())
}

// `match head [: motive]`. The motive is an ordinary term — a term of the eliminator's motive type, with no grammar of its own and so no disambiguation rule: `parse_term` is called directly and there is nothing to backtrack. It cannot run into the arms because `|` is not an infix operator (`parse_num_op` has `||` but no bare `|`), so a motive term always terminates at the first `|`.
pub(super) fn parse_match_prefix<'a>() -> Parser<'a, (Term, Option<Term>)> {
    catch(parse_keyword("match"))
        .and_keep(lazy(parse_term))
        .and(
            catch(parse_literal(":").and_keep(lazy(parse_term)))
                .map(Some)
                .or(pure(None)),
        )
}

// A match-arm: `| pattern => body`, where `pattern` may nest across constructors, tuples, and structs (see `MatchPattern`, `parse_match_pattern`). Full enumeration only ("Path A" — see `into_core::match_compile`'s doc comment): a bare binder arm is legal alone (equivalent to a `let`), but lowering rejects mixing it with a concrete-shape arm in the same column, since that would be a catch-all/row-priority pattern this grammar doesn't otherwise support.
pub(super) fn parse_induct_match_branch<'a>() -> Parser<'a, MatrixArm> {
    catch(parse_literal("|"))
        .and_keep(parse_match_pattern())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|(pattern, body)| MatrixArm { pattern, body })
}

// The `; ih` induction-hypothesis tail on a `Nat` succ or `Lst`/`Bin` cons leaf pattern: `;` sets the hypothesis apart from the scrutinee's shape. The hypothesis binds the *fold result*, not scrutinee shape, so it takes the same irrefutable pattern grammar a `let` binder does — `; ih` a plain name, `; (cur, live)` a destructuring. A plain case-split needs no hypothesis, so the tail may be omitted — `None`, not a placeholder; lowering mints a fresh internal name for it directly.
pub(super) fn parse_cons_ih<'a>() -> Parser<'a, Option<Pattern>> {
    catch(parse_literal(";").and_keep(parse_pattern()))
        .map(Some)
        .or(pure(None))
}

// A bind arm `| pattern = value => body` (Rust `if let`): the arm fires when `value` matches the refutable `pattern`. The `=` separator is guarded against `==` (equality) and `=>` (an empty value) via `not_ahead`, mirroring the `!` vs `!=` idiom. Tried *before* the condition arm — a bind pattern like `some(x)` also parses as a condition term — so the `catch` backtracks to the `|` when no `=` separator follows the pattern, and the condition arm re-parses the same text as a `Bool` term.
pub(super) fn parse_bind_arm<'a>() -> Parser<'a, ChooseArm> {
    catch(
        parse_literal("|")
            .and_keep(parse_match_pattern())
            .and_drop(
                take_exact("=")
                    .and_drop(not_ahead("="))
                    .and_drop(not_ahead(">")),
            )
            .and_drop(parse_whitespace())
            .and(lazy(parse_term))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|((pattern, value), body)| ChooseArm {
        test: ChooseTest::Bind { pattern, value },
        body,
    })
}

// A choose condition arm `| cond => body`. The catch spans through `=>` so a condition term that merely *starts* like the default backtracks cleanly. A bare `_` parses as an ordinary `Name` term, so the choose's `| _ =>` default would otherwise be swallowed here; the `flat_map` guard rejects a lone `_` condition, letting `many0` stop and hand the default to `parse_choose_default`. A condition that merely *begins* with `_` (e.g. `_ready`) has a longer head and passes the guard, parsing as an ordinary condition.
pub(super) fn parse_cond_arm<'a>() -> Parser<'a, ChooseArm> {
    catch(
        parse_literal("|")
            .and_keep(lazy(parse_term))
            .flat_map(|cond| match cond.as_subterm() {
                Subterm::Name(name) if name.is_single() && name.head() == "_" => {
                    fail("the `| _ =>` default is not a condition arm")
                }
                _ => pure(cond),
            })
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(condition, body)| ChooseArm {
        test: ChooseTest::Cond(condition),
        body,
    })
}

// One choose arm: a bind arm first (its `catch` backtracks when the pattern is not followed by `=`), else a condition arm.
pub(super) fn parse_choose_arm<'a>() -> Parser<'a, ChooseArm> {
    catch(parse_bind_arm()).or(parse_cond_arm())
}

// The mandatory `| _ =>` default arm closing a `choose`. A `Bool` ladder is a dispatch form (it enumerates no shapes), so `_` is required, not optional.
pub(super) fn parse_choose_default<'a>() -> Parser<'a, Term> {
    catch(parse_literal("|").and_keep(parse_literal("_")))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

// `choose | test => body | … | _ => default end`. No head term — the arms are `Bool` conditions or refutable binds, tried top-to-bottom, first to fire wins. Distinguished from `match` by keyword alone, so there is no disambiguation to speak of here.
pub(super) fn parse_choose<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("choose"))
        .and_keep(many0(parse_choose_arm).and(parse_choose_default()))
        .and_drop(parse_keyword("end"))
        .map(|(arms, default)| Subterm::Choose(Choose { arms, default }).into())
}

// A `match` is exactly one surface shape now: the general headed pattern matrix. Every headed carrier form — `Bool`, `Nat` (induction *and* literal dispatch), `Lst`, `Bin` — is just a matrix whose arm patterns are that carrier's leaves (see `parse_match_pattern`), lowered by `into_core::match_compile`'s `compile_bool`/`compile_nat`/`compile_lst`/`compile_bin`. The headless ladder is `choose` (`parse_choose`), parsed separately. Zero arms are legal: under inversion (Rung C) every impossible arm is silently omittable, and a scrutinee whose indices clash with *every* constructor's target eliminates with no arms at all.
pub(super) fn parse_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(many0(parse_induct_match_branch))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), arms)| Subterm::Match(Match { head, motive, arms }))
        .map(Into::into)
}
