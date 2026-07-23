use super::*;

// Grammar keys for the packrat cache (see `parser::memoize`), mirroring
// `expr.rs`'s `MEMO_TERM`/`MEMO_ATOMIC_TERM`. `parse_pattern`/`parse_match_pattern`
// each have their own `(...)`-grouping alternative and are re-probed at the same
// offset by every caller that speculatively tries a lambda/match-arm shape
// (`parse_func`'s parameter list, `parse_ctor_match_pattern`'s argument list, …) —
// without memoization here, a run of nested parens is exponential: each candidate
// caller re-walks the whole remaining nesting fresh.
const MEMO_PATTERN: u32 = 2;
const MEMO_MATCH_PATTERN: u32 = 3;

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
            .and_keep(sep_by0_trailing(parse_func_type_param, || {
                parse_literal(",")
            }))
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
    memoize(MEMO_PATTERN, parse_pattern_inner())
}

fn parse_pattern_inner<'a>() -> Parser<'a, Pattern> {
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
// payload can nest arbitrarily (`some(some(x))`, `pair(some(x), y)`, …). Each
// argument retains its plicity: a payload slot the constructor declared `@`
// must be matched `@name`. `use` is rejected — witness payloads are not a
// surface feature (`parse_plicity` never consumes `use`, so it stays a keyword
// no pattern can begin with).
pub(super) fn parse_ctor_arg<'a>() -> Parser<'a, (Plicity, MatchPattern)> {
    parse_plicity().and(lazy(parse_match_pattern))
}

pub(super) fn parse_ctor_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_identifier().and_drop(parse_literal("(")))
        .and(sep_by0_trailing(parse_ctor_arg, || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(
            |(tag, args): (&str, Vec<(Plicity, MatchPattern)>)| MatchPattern::Variant {
                tag: tag.to_string(),
                args,
            },
        )
}

// A nested `Bool` leaf: `true` or `false`. Tried as dedicated keywords before
// the generic `Binder` fallback in `parse_match_pattern` — `parse_binder`
// doesn't itself reject keyword text, mirroring the same precedent already
// used for `Bool` literals at term level (see the `Subterm::Prim(Prim::Bool)`
// case above).
pub(super) fn parse_bln_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_keyword("false"))
        .map(|()| MatchPattern::Bool(false))
        .or(catch(parse_keyword("true")).map(|()| MatchPattern::Bool(true)))
}

// The `0` leaf of a `Nat` match-arm pattern (the zero case of an induction,
// or literal `0` in a switch). Only the numeral `0` maps here; every other
// literal is `parse_nat_lit_match_pattern`.
pub(super) fn parse_nat_zero_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_nat().flat_map(|lit| match lit {
        NatLiteral(n, _) if n.is_zero() => pure(MatchPattern::Nat(NatPattern::Zero)),
        _ => fail("expected 0 as a nested Nat zero pattern"),
    }))
}

// The `pred + 1; ih` leaf of a `Nat` match-arm pattern, with the same
// optional `; ih` as the `Lst`/`Bin` cons leaves below (`parse_cons_ih`).
// Tried after `Ctor` and before
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
            .and_drop(parse_literal("1")),
    )
    .and(parse_cons_ih())
    .map(|(pred_label, ih_label): (&str, Option<String>)| {
        MatchPattern::Nat(NatPattern::Succ {
            pred_label: pred_label.to_string(),
            ih_label,
        })
    })
}

// The literal-dispatch leaf `k` of a `Nat` match-arm pattern (`| 5 =>`,
// `| 0x90 =>`). Reuses `parse_nat_literal_u32`, so hex/char literals dispatch by
// value; a column of these (with no `pred + 1; ih` arm) lowers to a `switch`.
// `0` is rejected here: it is always the `Zero` leaf (tried earlier in
// `parse_match_pattern`), keeping one canonical leaf per value. Tried before the
// generic `Binder` fallback, which would otherwise swallow a bare digit as an
// identifier.
pub(super) fn parse_nat_lit_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_nat_literal_u32().flat_map(|k| match k {
        0 => fail("0 is the Nat zero pattern, not a literal-dispatch case"),
        k => pure(MatchPattern::Nat(NatPattern::Lit(k))),
    }))
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
    catch(take_exact("b\\").and_drop(parse_whitespace()))
        .map(|()| MatchPattern::Bin(BinPattern::End(Grain::B)))
        .or(catch(take_exact("x\\").and_drop(parse_whitespace()))
            .map(|()| MatchPattern::Bin(BinPattern::End(Grain::X))))
}

// The `\head\..tail[; ih]` leaf of a nested `Bin` pattern — mirrors
// `parse_bin_cons_branch` minus the leading `|` and trailing `=> body`.
pub(super) fn parse_bin_byte_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        take_exact("b\\")
            .and_keep(parse_identifier_raw())
            .and_drop(take_exact("\\"))
            .and_drop(take_exact("..")),
    )
    .and(parse_identifier())
    .and(parse_cons_ih())
    .map(|((head, tail), ih_label)| {
        MatchPattern::Bin(BinPattern::Atom {
            grain: Grain::B,
            head_label: head.to_string(),
            tail_label: tail.to_string(),
            ih_label,
        })
    })
    .or(catch(
        take_exact("x\\")
            .and_keep(parse_identifier_raw())
            .and_drop(take_exact("\\"))
            .and_drop(take_exact("..")),
    )
    .and(parse_identifier())
    .and(parse_cons_ih())
    .map(|((head, tail), ih_label)| {
        MatchPattern::Bin(BinPattern::Atom {
            grain: Grain::X,
            head_label: head.to_string(),
            tail_label: tail.to_string(),
            ih_label,
        })
    }))
}

// A match-arm pattern: a plain binder, an inductive constructor applied to
// (possibly nested) sub-patterns, a tuple pattern, a struct pattern, or one
// of the `Bool`/`Nat`/`Lst`/`Bits`/`Bytes` literal leaves — see `MatchPattern`. Struct
// and constructor forms are tried before the bare-name case for the same
// reason `parse_pattern` tries `Struct`/`Tuple` first: a plain identifier
// prefix (`Point` in `Point { z, w = ww }`, `some` in `some(x)`) would
// otherwise be consumed by the binder case before the disambiguating
// `{`/`(` is ever seen. The literal leaves are tried before `Tuple` (none of
// their prefixes — `[`, `\`, a digit, `true`/`false` — overlap `Tuple`'s
// `(`) and, for `NatSucc` specifically, before `Binder` (see its own doc
// comment).
pub(super) fn parse_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    memoize(MEMO_MATCH_PATTERN, parse_match_pattern_inner())
}

fn parse_match_pattern_inner<'a>() -> Parser<'a, MatchPattern> {
    parse_bin_byte_match_pattern()
        .or(parse_bin_end_match_pattern())
        .or(parse_struct_match_pattern())
        .or(parse_ctor_match_pattern())
        .or(parse_bln_match_pattern())
        .or(parse_nat_zero_match_pattern())
        .or(parse_nat_succ_match_pattern())
        .or(parse_nat_lit_match_pattern())
        .or(parse_lst_nil_match_pattern())
        .or(parse_lst_cons_match_pattern())
        .or(parse_tuple_match_pattern())
        .or(catch(parse_literal("("))
            .and_keep(lazy(parse_match_pattern))
            .and_drop(parse_literal(")")))
        .or(parse_binder().map(MatchPattern::Binder))
}

// One parameter of the definition sugar `label(params) = value` (tuple, struct,
// and witness fields). Like a lambda binder it retains its plicity mark: `@name`
// (implicit) or `use name` (witness) — the mark is copied onto the generated
// function value's slot, so a hidden-binder field type (`pure : (@A, x) -> M(A)`)
// can be implemented as `pure(@A, x) = …` rather than losing the mark.
pub(super) fn parse_func_param<'a>() -> Parser<'a, (Plicity, String, Option<Term>)> {
    parse_func_binder_plicity()
        .and(parse_binder())
        .and(
            catch(parse_literal(":").and_keep(lazy(parse_term)))
                .map(Some)
                .or(pure(None)),
        )
        .map(|((plicity, name), annotation)| (plicity, name, annotation))
}

// A lambda parameter with an optional domain annotation and a binder pattern
// in place of a plain name — the pattern-accepting counterpart of
// `parse_func_param`, forked rather than generalized in place because
// `parse_func_param` also serves the out-of-scope `label(params) = value`
// definition sugar (`parse_tuple_field_prefix`), which stays single-name-only.
// A lambda parameter's plicity mark: `@` (implicit) or `use` (witness) prefixing
// the binder pattern, or no mark (explicit). Unlike the function-type and
// definition-sugar `use` forms — where a witness binder is anonymous and `use`
// is followed by the domain *type* — a lambda's `use` names a binder the body
// can reference (`use show`), so the mark precedes an ordinary pattern.
fn parse_func_binder_plicity<'a>() -> Parser<'a, Plicity> {
    catch(parse_keyword("use"))
        .map(|()| Plicity::Witness)
        .or(catch(parse_literal("@")).map(|()| Plicity::Implicit))
        .or(pure(Plicity::Explicit))
}

pub(super) fn parse_func_pattern_param<'a>() -> Parser<'a, FuncParam> {
    parse_func_binder_plicity()
        .and(parse_pattern())
        .and(
            catch(parse_literal(":").and_keep(lazy(parse_term)))
                .map(Some)
                .or(pure(None)),
        )
        .map(|((plicity, pattern), annotation)| FuncParam {
            plicity,
            pattern,
            annotation,
        })
}
