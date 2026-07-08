use super::*;

pub(super) fn parse_use_func_type_param<'a>() -> Parser<'a, FuncTypeParam> {
    catch(parse_keyword("use"))
        .and_keep(lazy(parse_term))
        .map(|type_| FuncTypeParam {
            plicity: Plicity::Witness,
            label: None,
            type_,
        })
}

pub(super) fn parse_func_type_param<'a>() -> Parser<'a, FuncTypeParam> {
    parse_use_func_type_param().or(parse_plicity()
        .and(
            catch(parse_identifier().and_drop(parse_literal(":")))
                .and(lazy(parse_term))
                .map(|(label, ty): (&str, Term)| (Some(label.to_string()), ty))
                .or(lazy(parse_term).map(|ty| (None, ty))),
        )
        .map(|(plicity, (label, type_))| FuncTypeParam {
            plicity,
            label,
            type_,
        }))
}

pub(super) fn parse_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_type_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(|(params, output): (Vec<FuncTypeParam>, Term)| {
        Subterm::FuncType(FuncType {
            params: params.into_iter().collect(),
            output,
        })
    })
    .map(Into::into)
}

// A lambda parameter with an optional domain annotation. `(x)` is sugar for
// `(x : _)`; the annotation, when present, parses as an arbitrary term and stops
// at the closing `)` (mirrors `parse_func_type_param`).
// A binder name: a plain identifier (`_` to ignore). The `label(params) =
// value`/`label(params) -> type` definition-sugar parameter lists stay
// single-name-only — see `parse_func_param` below. `let`, lambda, and
// function-definition-sugar parameters accept a full `Pattern` instead (see
// `parse_pattern`); match arms accept a full `MatchPattern` instead (see
// `parse_match_pattern`).
pub(super) fn parse_binder<'a>() -> Parser<'a, String> {
    parse_identifier().map(str::to_string)
}

// The committing prefix of a labeled pattern field: `label =`. The caller
// wraps it in `catch`, so a positional field that merely starts with an
// identifier backtracks cleanly — mirrors `parse_tuple_field_prefix`, with no
// definition-sugar form (a pattern field is never itself a function).
pub(super) fn parse_pattern_field_prefix<'a>() -> Parser<'a, String> {
    parse_identifier()
        .and_drop(parse_literal("="))
        .map(str::to_string)
}

// A tuple-pattern / struct-pattern field: `label = pattern` or a bare
// positional pattern — the literal mirror of `parse_tuple_field`, with `Term`
// replaced by `Pattern`. Field-punning (`Name { x, y }`) falls out for free:
// it is just the positional case where the sub-pattern happens to be a
// binder matching the field.
pub(super) fn parse_pattern_field<'a>() -> Parser<'a, PatternField> {
    catch(parse_pattern_field_prefix())
        .and(lazy(parse_pattern))
        .map(|(label, value)| PatternField {
            label: Some(label),
            value,
        })
        .or(lazy(parse_pattern).map(|value| PatternField { label: None, value }))
}

// A tuple pattern `(p1, p2, …)` / `(label = p, …)` — the literal mirror of
// `parse_tuple`, with `Term` replaced by `Pattern`. A bare `(p)` with neither
// a comma nor a label is not a one-element tuple pattern (the language has no
// such thing, exactly like tuple literals) — it falls through to the
// parenthesized-pattern case in `parse_pattern` below.
pub(super) fn parse_tuple_pattern<'a>() -> Parser<'a, Pattern> {
    catch(
        parse_literal("(")
            .and_keep(parse_pattern_field())
            .and_drop(parse_literal(",")),
    )
    .and(sep_by0_trailing(parse_pattern_field, || parse_literal(",")))
    .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
    .or(
        catch(parse_literal("(").and_keep(parse_pattern_field_prefix()))
            .and(lazy(parse_pattern))
            .map(|(label, value)| {
                vec![PatternField {
                    label: Some(label),
                    value,
                }]
            }),
    )
    .and_drop(parse_literal(")"))
    .map(Pattern::Tuple)
}

// A struct pattern `Name { p1, p2, … }` / `Name { label = p, … }` — mirrors
// `parse_struct_lit`, but with no `(args)` head-parameter form: the written
// head name is descriptive only, never resolved or validated (see `Pattern`).
pub(super) fn parse_struct_pattern<'a>() -> Parser<'a, Pattern> {
    catch(parse_name().and_drop(parse_literal("{")))
        .and(sep_by0_trailing(parse_pattern_field, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(|(head, fields)| Pattern::Struct {
            head: head.join(),
            fields,
        })
}

// A binder pattern: a plain name (today's only case, unchanged, at `let`,
// lambda-parameter, and function-definition-sugar-parameter position — see
// `Pattern`), a tuple pattern, a struct pattern, or a parenthesized pattern
// (pure grouping, mirroring `parse_parens`). Struct and tuple forms are tried
// before the bare-name case — not after, as a plain identifier prefix (e.g.
// `Point` in `Point { z, w = ww }`) would otherwise be consumed by the binder
// case before the disambiguating `{`/`,`/`=` is ever seen, exactly like
// `parse_struct_lit` is tried before a bare name at the term level.
pub(super) fn parse_pattern<'a>() -> Parser<'a, Pattern> {
    parse_struct_pattern()
        .or(parse_tuple_pattern())
        .or(catch(parse_literal("("))
            .and_keep(lazy(parse_pattern))
            .and_drop(parse_literal(")")))
        .or(parse_binder().map(|name| Pattern::Binder(Some(name))))
}

// A match-arm field: `label = pattern` or a bare positional pattern — the
// `MatchPattern` counterpart of `parse_pattern_field`.
pub(super) fn parse_match_pattern_field<'a>() -> Parser<'a, MatchPatternField> {
    catch(parse_pattern_field_prefix())
        .and(lazy(parse_match_pattern))
        .map(|(label, value)| MatchPatternField {
            label: Some(label),
            value,
        })
        .or(lazy(parse_match_pattern).map(|value| MatchPatternField { label: None, value }))
}

// A tuple match pattern `(p1, p2, …)` / `(label = p, …)` — the `MatchPattern`
// counterpart of `parse_tuple_pattern`.
pub(super) fn parse_tuple_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        parse_literal("(")
            .and_keep(parse_match_pattern_field())
            .and_drop(parse_literal(",")),
    )
    .and(sep_by0_trailing(parse_match_pattern_field, || {
        parse_literal(",")
    }))
    .map(|(first, rest)| iter::once(first).chain(rest).collect::<Vec<_>>())
    .or(
        catch(parse_literal("(").and_keep(parse_pattern_field_prefix()))
            .and(lazy(parse_match_pattern))
            .map(|(label, value)| {
                vec![MatchPatternField {
                    label: Some(label),
                    value,
                }]
            }),
    )
    .and_drop(parse_literal(")"))
    .map(MatchPattern::Tuple)
}

// A struct match pattern `Name { p1, p2, … }` / `Name { label = p, … }` —
// the `MatchPattern` counterpart of `parse_struct_pattern`, mirroring struct
// literals rather than the positional constructor-call shape (structs have
// field labels; inductive constructors don't — see `MatchPattern`).
pub(super) fn parse_struct_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_name().and_drop(parse_literal("{")))
        .and(sep_by0_trailing(parse_match_pattern_field, || {
            parse_literal(",")
        }))
        .and_drop(parse_literal("}"))
        .map(|(head, fields)| MatchPattern::Struct {
            head: head.join(),
            fields,
        })
}

// A constructor match pattern `tag(p, …)` — `nil()` for the nullary case.
// The `(` immediately after the tag is the commit point, distinguishing it
// from a bare name. Unlike `parse_func_param`'s definition-sugar arguments,
// each argument here is itself a full `MatchPattern`, so a constructor's
// payload can nest arbitrarily (`some(some(x))`, `pair(some(x), y)`, …).
pub(super) fn parse_ctor_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_identifier().and_drop(parse_literal("(")))
        .and(sep_by0(parse_match_pattern, || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(
            |(tag, args): (&str, Vec<MatchPattern>)| MatchPattern::Ctor {
                tag: tag.to_string(),
                args,
            },
        )
}

// A nested `Bln` leaf: `true` or `false`. Tried as dedicated keywords before
// the generic `Binder` fallback in `parse_match_pattern` — `parse_binder`
// doesn't itself reject keyword text, mirroring the same precedent already
// used for `Bln` literals at term level (see the `Subterm::Prim(Prim::Bln)`
// case above).
pub(super) fn parse_bln_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_keyword("false"))
        .map(|()| MatchPattern::Bln(false))
        .or(catch(parse_keyword("true")).map(|()| MatchPattern::Bln(true)))
}

// The `0` leaf of a nested `Nat` pattern — mirrors `parse_nat_match`'s own
// zero-case check.
pub(super) fn parse_nat_zero_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_nat().flat_map(|lit| match lit {
        NatLiteral::Number(n, _) if n.is_zero() => pure(MatchPattern::Nat(NatPattern::Zero)),
        _ => fail("expected 0 as a nested Nat zero pattern"),
    }))
}

// The `pred + 1; ih` leaf of a nested `Nat` pattern. `ih` is mandatory here
// (no optional-ih alternative), mirroring `parse_nat_match`'s own asymmetry
// against the `Lst`/`Bin` cons leaves below. Tried after `Ctor` and before
// the generic `Binder` fallback in `parse_match_pattern`: it shares a
// leading identifier with both, so `Binder` would otherwise silently
// swallow every `name+1;ih` input before this ever gets a chance to commit.
// A space is required on each side of `+` (mirroring `parse_infix_op`'s own
// space-sensitivity, via the same `preceded_by_space`/`require_space`
// primitives and a `take_exact` operator token that doesn't itself eat
// trailing whitespace) — `pred+1` sets this apart visually from a plain
// binder in a way `pred + 1` doesn't need help with.
pub(super) fn parse_nat_succ_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        parse_identifier()
            .and_drop(preceded_by_space())
            .and_drop(take_exact("+"))
            .and_drop(require_space())
            .and_drop(parse_literal("1"))
            .and_drop(parse_literal(";")),
    )
    .and(parse_identifier())
    .map(|(pred_label, ih_label): (&str, &str)| {
        MatchPattern::Nat(NatPattern::Succ {
            pred_label: pred_label.to_string(),
            ih_label: ih_label.to_string(),
        })
    })
}

// The `[]` leaf of a nested `Lst` pattern.
pub(super) fn parse_lst_nil_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_literal("[]")).map(|()| MatchPattern::Lst(LstPattern::Nil))
}

// The `[head, ..tail][; ih]` leaf of a nested `Lst` pattern — mirrors
// `parse_lst_cons_branch` minus the leading `|` and trailing `=> body`.
pub(super) fn parse_lst_cons_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        parse_literal("[")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(","))
            .and_drop(parse_literal("..")),
    )
    .and(parse_identifier())
    .and_drop(parse_literal("]"))
    .and(parse_cons_ih())
    .map(|((head, tail), ih_label)| {
        MatchPattern::Lst(LstPattern::Cons {
            head_label: head.to_string(),
            tail_label: tail.to_string(),
            ih_label,
        })
    })
}

// The `\\` leaf of a nested `Bin` pattern (the empty bytestring literal).
pub(super) fn parse_bin_end_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_literal("\\\\")).map(|()| MatchPattern::Bin(BinPattern::End))
}

// The `\head\..tail[; ih]` leaf of a nested `Bin` pattern — mirrors
// `parse_bin_cons_branch` minus the leading `|` and trailing `=> body`.
pub(super) fn parse_bin_byte_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        parse_literal("\\")
            .and_keep(parse_identifier())
            .and_drop(parse_literal("\\"))
            .and_drop(parse_literal("..")),
    )
    .and(parse_identifier())
    .and(parse_cons_ih())
    .map(|((head, tail), ih_label)| {
        MatchPattern::Bin(BinPattern::Byte {
            head_label: head.to_string(),
            tail_label: tail.to_string(),
            ih_label,
        })
    })
}

// A match-arm pattern: a plain binder, an inductive constructor applied to
// (possibly nested) sub-patterns, a tuple pattern, a struct pattern, or one
// of the `Bln`/`Nat`/`Lst`/`Bin` literal leaves — see `MatchPattern`. Struct
// and constructor forms are tried before the bare-name case for the same
// reason `parse_pattern` tries `Struct`/`Tuple` first: a plain identifier
// prefix (`Point` in `Point { z, w = ww }`, `some` in `some(x)`) would
// otherwise be consumed by the binder case before the disambiguating
// `{`/`(` is ever seen. The literal leaves are tried before `Tuple` (none of
// their prefixes — `[`, `\`, a digit, `true`/`false` — overlap `Tuple`'s
// `(`) and, for `NatSucc` specifically, before `Binder` (see its own doc
// comment).
pub(super) fn parse_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    parse_struct_match_pattern()
        .or(parse_ctor_match_pattern())
        .or(parse_bln_match_pattern())
        .or(parse_nat_zero_match_pattern())
        .or(parse_nat_succ_match_pattern())
        .or(parse_lst_nil_match_pattern())
        .or(parse_lst_cons_match_pattern())
        .or(parse_bin_end_match_pattern())
        .or(parse_bin_byte_match_pattern())
        .or(parse_tuple_match_pattern())
        .or(catch(parse_literal("("))
            .and_keep(lazy(parse_match_pattern))
            .and_drop(parse_literal(")")))
        .or(parse_binder().map(MatchPattern::Binder))
}

pub(super) fn parse_func_param<'a>() -> Parser<'a, (String, Option<Term>)> {
    // A leading `use` on a lambda parameter is accepted and dropped: lambdas
    // carry no plicity marks (checking against a Π type supplies them), so the
    // marker is purely documentary here.
    catch(parse_keyword("use"))
        .or(pure(()))
        .and_keep(parse_binder())
        .and(
            catch(parse_literal(":").and_keep(lazy(parse_term)))
                .map(Some)
                .or(pure(None)),
        )
}

// A lambda parameter with an optional domain annotation and a binder pattern
// in place of a plain name — the pattern-accepting counterpart of
// `parse_func_param`, forked rather than generalized in place because
// `parse_func_param` also serves the out-of-scope `label(params) = value`
// definition sugar (`parse_tuple_field_prefix`), which stays single-name-only.
pub(super) fn parse_func_pattern_param<'a>() -> Parser<'a, (Pattern, Option<Term>)> {
    // A leading `use` on a lambda parameter is accepted and dropped: lambdas
    // carry no plicity marks (checking against a Π type supplies them), so the
    // marker is purely documentary here.
    catch(parse_keyword("use"))
        .or(pure(()))
        .and_keep(parse_pattern())
        .and(
            catch(parse_literal(":").and_keep(lazy(parse_term)))
                .map(Some)
                .or(pure(None)),
        )
}
