use super::*;

pub(super) fn parse_func<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_pattern_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(params, body)| Subterm::Func(Func { params, body }).into())
}

// The motive ladder, binder parenthesized in every form (the bare-label
// `x => P` form is retired): `(x) => P`, `(x : Vec(T, k)) => P`, or a
// constant term. The catches span through `=>` so a constant motive that
// merely *starts* with a paren (a tuple type, a Π type) backtracks cleanly.
pub(super) fn parse_motive<'a>() -> Parser<'a, Motive> {
    catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(label, body): (&str, Term)| Motive::Scrutinee {
        label: label.to_string(),
        body,
    })
    .or(catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":"))
            .and(parse_name())
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(
        |(((label, name), slots), body): (((&str, Name), Vec<Term>), Term)| Motive::Annotated {
            label: label.to_string(),
            name,
            slots,
            body,
        },
    ))
    .or(lazy(parse_term).map(Motive::Constant))
}

pub(super) fn parse_match_prefix<'a>() -> Parser<'a, (Term, Option<Motive>)> {
    catch(parse_keyword("match"))
        .and_keep(lazy(parse_term))
        .and(
            catch(parse_literal(":").and_keep(parse_motive()))
                .map(Some)
                .or(pure(None)),
        )
}

// A match-arm: `| pattern => body`, where `pattern` may nest across
// constructors, tuples, and structs (see `MatchPattern`, `parse_match_pattern`).
// Full enumeration only ("Path A" — see `into_core::match_compile`'s doc comment): a
// bare binder arm is legal alone (equivalent to a `let`), but lowering
// rejects mixing it with a concrete-shape arm in the same column, since that
// would be a catch-all/row-priority pattern this grammar doesn't otherwise
// support.
pub(super) fn parse_inductive_match_branch<'a>() -> Parser<'a, MatrixArm> {
    catch(parse_literal("|"))
        .and_keep(parse_match_pattern())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|(pattern, body)| MatrixArm { pattern, body })
}

// Zero arms are legal: under inversion (Rung C) every impossible arm is
// silently omittable, and a scrutinee whose indices clash with *every*
// constructor's target eliminates with no arms at all.
pub(super) fn parse_inductive_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(many0(parse_inductive_match_branch))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), arms)| {
            Subterm::Match(Match::Matrix(MatrixMatch { head, motive, arms }))
        })
        .map(Into::into)
}

// The `; ih` induction-hypothesis tail on a `Lst`/`Bin` cons leaf pattern
// (`parse_lst_cons_match_pattern`/`parse_bin_cons_match_pattern`): `;` sets the
// hypothesis apart from the scrutinee's shape. A plain case-split needs no
// induction hypothesis, so `; ih` may be omitted — `None`, not a placeholder
// name; lowering mints a fresh internal name for it directly.
pub(super) fn parse_cons_ih<'a>() -> Parser<'a, Option<String>> {
    catch(parse_literal(";").and_keep(parse_identifier()))
        .map(|name| Some(name.to_string()))
        .or(pure(None))
}

// A bind arm `| pattern = value => body` (Rust `if let`): the arm fires when
// `value` matches the refutable `pattern`. The `=` separator is guarded against
// `==` (equality) and `=>` (an empty value) via `not_ahead`, mirroring the `!`
// vs `!=` idiom. Tried *before* the condition arm — a bind pattern like
// `some(x)` also parses as a condition term — so the `catch` backtracks to the
// `|` when no `=` separator follows the pattern, and the condition arm re-parses
// the same text as a `Bln` term.
pub(super) fn parse_bind_case<'a>() -> Parser<'a, LadderArm> {
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
    .map(|((pattern, value), body)| LadderArm {
        test: LadderTest::Bind { pattern, value },
        body,
    })
}

// A headless ladder condition arm `| cond => body`. The catch spans through `=>`
// so a condition term that merely *starts* like the default backtracks cleanly.
// A bare `_` parses as an ordinary `Name` term, so the ladder's `| _ =>` default
// would otherwise be swallowed here; the `flat_map` guard rejects a lone `_`
// condition, letting `many0` stop and hand the default to `parse_cond_default`.
// A condition that merely *begins* with `_` (e.g. `_ready`) has a longer head
// and passes the guard, parsing as an ordinary condition.
pub(super) fn parse_cond_case<'a>() -> Parser<'a, LadderArm> {
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
    .map(|(condition, body)| LadderArm {
        test: LadderTest::Cond(condition),
        body,
    })
}

// One ladder arm: a bind arm first (its `catch` backtracks when the pattern is
// not followed by `=`), else a condition arm.
pub(super) fn parse_ladder_arm<'a>() -> Parser<'a, LadderArm> {
    catch(parse_bind_case()).or(parse_cond_case())
}

// The mandatory `| _ =>` default arm closing a headless ladder. A `Bln` ladder
// is a dispatch form (it enumerates no shapes), so `_` is required, not optional.
pub(super) fn parse_cond_default<'a>() -> Parser<'a, Term> {
    catch(parse_literal("|").and_keep(parse_literal("_")))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

// The headless form `match | test => body | … | _ => default end`. No head term
// follows `match` — the arms are `Bln` conditions or refutable binds, tried
// top-to-bottom, first to fire wins. Prefix-disambiguated from every headed
// form: no term starts with `|`, so those forms' `lazy(parse_term)` backtracks.
pub(super) fn parse_cond_match<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("match"))
        .and_keep(many0(parse_ladder_arm).and(parse_cond_default()))
        .and_drop(parse_keyword("end"))
        .map(|(arms, default)| Subterm::Match(Match::Cond(CondMatch { arms, default })).into())
}

// A `match` is one of exactly two surface shapes: the headless `Bln`/bind
// ladder (no head term after `match`), or the general headed pattern matrix.
// Every headed carrier form — `Bln`, `Nat` (induction *and* literal dispatch),
// `Lst`, `Bin` — is just a matrix whose arm patterns are that carrier's leaves
// (see `parse_match_pattern`), lowered by `into_core::match_compile`'s
// `compile_bln`/`compile_nat`/`compile_lst`/`compile_bin`. The headless form is
// tried first: no term starts with `|`, so a headed form's `lazy(parse_term)`
// head backtracks cleanly when there is none.
pub(super) fn parse_match<'a>() -> Parser<'a, Term> {
    catch(parse_cond_match()).or(parse_inductive_match())
}
