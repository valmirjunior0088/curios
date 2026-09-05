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

// A binder name: a plain identifier (`_` to ignore). The `label(params) = value`/`label(params) -> type` definition-sugar parameter lists stay single-name-only — see `parse_func_param` below. `let`, lambda, and function-definition-sugar parameters accept a full `Pattern` instead (see `parse_pattern`); match arms accept a full `MatchPattern` instead (see `parse_match_pattern`).
pub(super) fn parse_binder<'a>() -> Parser<'a, Label> {
    parse_label()
}

// The committing prefix of a labeled pattern field: `label =`. The caller wraps it in `catch`, so a positional field that merely starts with an identifier backtracks cleanly — mirrors `parse_tuple_field_prefix`, with no definition-sugar form (a pattern field is never itself a function).
pub(super) fn parse_pattern_field_prefix<'a>() -> Parser<'a, String> {
    parse_identifier()
        .and_drop(parse_literal("="))
        .map(str::to_string)
}

// A tuple-pattern / struct-pattern field: `label = pattern` or a bare positional pattern — the literal mirror of `parse_tuple_field`, with `Term` replaced by `Pattern`. Field-punning (`Name { x, y }`) falls out for free: it is just the positional case where the sub-pattern happens to be a binder matching the field.
pub(super) fn parse_pattern_field<'a>() -> Parser<'a, PatternField> {
    catch(parse_pattern_field_prefix())
        .and(lazy(parse_pattern))
        .map(|(label, value)| PatternField {
            label: Some(label),
            value,
        })
        .or(lazy(parse_pattern).map(|value| PatternField { label: None, value }))
}

// A tuple pattern `(p1, p2, …)` / `(label = p, …)` — the literal mirror of `parse_tuple`, with `Term` replaced by `Pattern`. A bare `(p)` with neither a comma nor a label is not a one-element tuple pattern (the language has no such thing, exactly like tuple literals) — it falls through to the parenthesized-pattern case in `parse_pattern` below.
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

// A struct pattern `Name { p1, p2, … }` / `Name { label = p, … }` — mirrors `parse_struct_lit`, but with no `(args)` head-parameter form: the written head name is descriptive only, never resolved or validated (see `Pattern`).
pub(super) fn parse_struct_pattern<'a>() -> Parser<'a, Pattern> {
    catch(parse_name().and_drop(parse_literal("{")))
        .and(sep_by0_trailing(parse_pattern_field, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(|(head, fields)| Pattern::Struct {
            head: head.join(),
            fields,
        })
}

// A binder pattern: a plain name (today's only case, unchanged, at `let`, lambda-parameter, and function-definition-sugar-parameter position — see `Pattern`), a tuple pattern, a struct pattern, or a parenthesized pattern (pure grouping, mirroring `parse_parens`). Struct and tuple forms are tried before the bare-name case — not after, as a plain identifier prefix (e.g. `Point` in `Point { z, w = ww }`) would otherwise be consumed by the binder case before the disambiguating `{`/`,`/`=` is ever seen, exactly like `parse_struct_lit` is tried before a bare name at the term level.
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

// A match-arm field: `label = pattern` or a bare positional pattern — the `MatchPattern` counterpart of `parse_pattern_field`.
pub(super) fn parse_match_pattern_field<'a>() -> Parser<'a, MatchPatternField> {
    catch(parse_pattern_field_prefix())
        .and(lazy(parse_match_pattern))
        .map(|(label, value)| MatchPatternField {
            label: Some(label),
            value,
        })
        .or(lazy(parse_match_pattern).map(|value| MatchPatternField { label: None, value }))
}

// A tuple match pattern `(p1, p2, …)` / `(label = p, …)` — the `MatchPattern` counterpart of `parse_tuple_pattern`.
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

// A struct match pattern `Name { p1, p2, … }` / `Name { label = p, … }` — the `MatchPattern` counterpart of `parse_struct_pattern`, mirroring struct literals rather than the positional constructor-call shape (structs have field labels; inductive constructors don't — see `MatchPattern`).
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

// A constructor match pattern `tag(p, …)` — `nil()` for the nullary case. The `(` immediately after the tag is the commit point, distinguishing it from a bare name. Unlike `parse_func_param`'s definition-sugar arguments, each argument here is itself a full `MatchPattern`, so a constructor's payload can nest arbitrarily (`some(some(x))`, `pair(some(x), y)`, …). Each argument retains its plicity: a payload slot the constructor declared `@` must be matched `@name`. `use` is rejected — witness payloads are not a surface feature (`parse_plicity` never consumes `use`, so it stays a keyword no pattern can begin with).
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

// A match pattern written with a qualified head, refused by name.
//
// A constructor pattern names its constructor *bare*: the tag is resolved against the scrutinee's type rather than looked up, so the namespace is never spelled. Left to fall through, `Option/some(n)` parsed `Option` as a `Binder` and the arm then reported `Expected '=>'` against the `/` — with a `=>` plainly written four columns along.
//
// **Tried first, and guarded by the one form that may carry a path.** A struct head *is* a path (`parse_struct_match_pattern`, documentary and unresolved), so `not_ahead` hands `Name/Sub { … }` back; nothing else in this grammar begins with one, since `parse_qualified_name` rejects a single segment and so never reaches the `Binder` case. First is what makes the refusal stick: [`Parser::or`] consults the *first* alternative's fatality but picks between two failures on offset alone, and the struct alternative fails at exactly this one's offset — both stop at the delimiter that is not a `{` — so from any later position the tie would keep the vaguer error.
//
// The failure is past the choice point and so fatal, which is what carries it out of the arm. `parse_bind_arm` catches it back, so a `choose` condition arm beginning the same way still re-parses as a term.
pub(super) fn parse_qualified_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_qualified_name().and_drop(not_ahead("{"))).flat_map(|name| {
        fail(format!(
            "a constructor pattern names its constructor bare: write `{}` rather than `{}`, since the scrutinee's type supplies the namespace",
            name.last(),
            name.join(),
        ))
    })
}

// A nested `Bool` leaf: `true` or `false`. Tried as dedicated keywords before the generic `Binder` fallback in `parse_match_pattern` — `parse_binder` doesn't itself reject keyword text, mirroring the same precedent already used for `Bool` literals at term level (see the `Subterm::Intrinsic(Intrinsic::Bool)` case above).
pub(super) fn parse_bool_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_keyword("false"))
        .map(|()| MatchPattern::Bool(false))
        .or(catch(parse_keyword("true")).map(|()| MatchPattern::Bool(true)))
}

// The `0` leaf of a `Nat` match-arm pattern (the zero case of an induction, or literal `0` in a switch). Only the numeral `0` maps here; every other literal is `parse_nat_lit_match_pattern`.
pub(super) fn parse_nat_zero_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_nat_digits().flat_map(|lit| match lit {
        NatLiteral(n, _) if n.is_zero() => pure(MatchPattern::Nat(NatPattern::Zero)),
        _ => fail("expected 0 as a nested Nat zero pattern"),
    }))
}

// The `pred + 1; ih` leaf of a `Nat` match-arm pattern, with the same optional `; ih` as the `List`/`Bin` cons leaves below (`parse_cons_ih`). Tried after `Ctor` and before the generic `Binder` fallback in `parse_match_pattern`: it shares a leading identifier with both, so `Binder` would otherwise silently swallow every `name+1;ih` input before this ever gets a chance to commit. A space is required on each side of `+` (mirroring `parse_infix_op`'s own space-sensitivity, via the same `preceded_by_space`/`require_space` intrinsics and a `take_exact` operator token that doesn't itself eat trailing whitespace) — `pred+1` sets this apart visually from a plain binder in a way `pred + 1` doesn't need help with.
pub(super) fn parse_nat_succ_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        parse_identifier()
            .and_drop(preceded_by_space())
            .and_drop(take_exact("+"))
            .and_drop(require_space())
            .and_drop(parse_literal("1")),
    )
    .and(parse_cons_ih())
    .map(|(pred_label, ih): (&str, Option<Pattern>)| {
        MatchPattern::Nat(NatPattern::Succ {
            pred_label: pred_label.to_string(),
            ih,
        })
    })
}

// The literal-dispatch leaf `k` of a `Nat` match-arm pattern (`| 5 =>`, `| 0x90 =>`). Reads the numeral by value, so hex literals dispatch by value; a column of these (with no `pred + 1; ih` arm) lowers to a `switch`. `0` is rejected here: it is always the `Zero` leaf (tried earlier in `parse_match_pattern`), keeping one canonical leaf per value. Tried before the generic `Binder` fallback, which would otherwise swallow a bare digit as an identifier.
//
// The numeral is kept whole. Narrowing it to the erased `u32` here made the parser choose `curios-ersd`'s width, and the failure was caught along with "this is not a numeral" — so an oversized dispatch case fell through to `Binder`, a digit run being an identifier, rather than refusing. Where that width is chosen is `curios-elab`'s erase boundary, which refuses what it cannot represent.
pub(super) fn parse_nat_lit_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        parse_nat_digits().flat_map(|NatLiteral(value, _)| match value.is_zero() {
            true => fail("0 is the Nat zero pattern, not a literal-dispatch case"),
            false => pure(MatchPattern::Nat(NatPattern::Lit(value))),
        }),
    )
}

// A character literal leaf — a `Nat` dispatch case spelled by its scalar value, compiled exactly as the numeral it denotes.
pub(super) fn parse_char_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(MatchPattern::Char)
}

// The `[]` leaf of a nested `List` pattern.
pub(super) fn parse_list_nil_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_literal("[]")).map(|()| MatchPattern::List(ListPattern::Nil))
}

// The `[head, ..tail][; ih]` leaf of a nested `List` pattern.
pub(super) fn parse_list_cons_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(
        parse_literal("[")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(","))
            .and_drop(parse_literal("..")),
    )
    .and(parse_identifier())
    .and_drop(parse_literal("]"))
    .and(parse_cons_ih())
    .map(|((head, tail), ih)| {
        MatchPattern::List(ListPattern::Cons {
            head_label: head.to_string(),
            tail_label: tail.to_string(),
            ih,
        })
    })
}

// The `b[]`/`x[]` leaf of a nested `Bin` pattern (the grain's empty literal) — the packed counterpart of `parse_list_nil_match_pattern`, and glued exactly as `[]` is.
pub(super) fn parse_bin_end_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    catch(parse_literal("b[]"))
        .map(|()| MatchPattern::Bin(BinPattern::End(Grain::B)))
        .or(catch(parse_literal("x[]")).map(|()| MatchPattern::Bin(BinPattern::End(Grain::X))))
}

// The `b[head, ..tail][; ih]` leaf of a nested `Bin` pattern — the packed counterpart of `parse_list_cons_match_pattern`, differing only in the grain letter that selects the carrier.
pub(super) fn parse_bin_byte_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    parse_bin_cons_match_pattern(Grain::B, "b[").or(parse_bin_cons_match_pattern(Grain::X, "x["))
}

fn parse_bin_cons_match_pattern<'a>(
    grain: Grain,
    prefix: &'static str,
) -> Parser<'a, MatchPattern> {
    catch(
        parse_literal(prefix)
            .and_keep(parse_identifier())
            .and_drop(parse_literal(","))
            .and_drop(parse_literal("..")),
    )
    .and(parse_identifier())
    .and_drop(parse_literal("]"))
    .and(parse_cons_ih())
    .map(move |((head, tail), ih)| {
        MatchPattern::Bin(BinPattern::Atom {
            grain,
            head_label: head.to_string(),
            tail_label: tail.to_string(),
            ih,
        })
    })
}

// A match-arm pattern: a plain binder, an inductive constructor applied to (possibly nested) sub-patterns, a tuple pattern, a struct pattern, or one of the `Bool`/`Nat`/`List`/`Bits`/`Bytes` literal leaves — see `MatchPattern`. Struct and constructor forms are tried before the bare-name case for the same reason `parse_pattern` tries `Struct`/`Tuple` first: a plain identifier prefix (`Point` in `Point { z, w = ww }`, `some` in `some(x)`) would otherwise be consumed by the binder case before the disambiguating `{`/`(` is ever seen. The literal leaves are tried before `Tuple` (none of their prefixes — `[`, `b[`, `x[`, a digit, `true`/`false` — overlap `Tuple`'s `(`) and, for `NatSucc` specifically, before `Binder` (see its own doc comment). The packed cons leaf is tried before the packed empty leaf so `b[` commits to the longer form and backtracks to `b[]` only when no binder follows. A qualified head is refused ahead of all of them, for the reason `parse_qualified_match_pattern` states.
pub(super) fn parse_match_pattern<'a>() -> Parser<'a, MatchPattern> {
    memoize(MEMO_MATCH_PATTERN, parse_match_pattern_inner())
}

fn parse_match_pattern_inner<'a>() -> Parser<'a, MatchPattern> {
    parse_qualified_match_pattern()
        .or(parse_bin_byte_match_pattern())
        .or(parse_bin_end_match_pattern())
        .or(parse_struct_match_pattern())
        .or(parse_ctor_match_pattern())
        .or(parse_bool_match_pattern())
        .or(parse_char_match_pattern())
        .or(parse_nat_zero_match_pattern())
        .or(parse_nat_succ_match_pattern())
        .or(parse_nat_lit_match_pattern())
        .or(parse_list_nil_match_pattern())
        .or(parse_list_cons_match_pattern())
        .or(parse_tuple_match_pattern())
        .or(catch(parse_literal("("))
            .and_keep(lazy(parse_match_pattern))
            .and_drop(parse_literal(")")))
        .or(parse_binder().map(MatchPattern::Binder))
}

// One parameter of the definition sugar `label(params) = value` (tuple, struct, and witness fields). Like a lambda binder it retains its plicity mark: `@name` (implicit) or `use name` (witness) — the mark is copied onto the generated function value's slot, so a hidden-binder field type (`pure : (@A, x) -> M(A)`) can be implemented as `pure(@A, x) = …` rather than losing the mark.
pub(super) fn parse_func_param<'a>() -> Parser<'a, (Plicity, String, Option<Term>)> {
    parse_func_binder_plicity()
        .and(parse_binder())
        .and(
            catch(parse_literal(":").and_keep(lazy(parse_term)))
                .map(Some)
                .or(pure(None)),
        )
        .map(|((plicity, name), annotation)| (plicity, name.to_string(), annotation))
}

// A lambda parameter's plicity mark: `@` (implicit) or `use` (witness) prefixing the binder pattern, or no mark (explicit). Unlike the function-type and definition-sugar `use` forms — where a witness binder is anonymous and `use` is followed by the domain *type* — a lambda's `use` names a binder the body can reference (`use show`), so the mark precedes an ordinary pattern.
fn parse_func_binder_plicity<'a>() -> Parser<'a, Plicity> {
    catch(parse_keyword("use"))
        .map(|()| Plicity::Witness)
        .or(catch(parse_literal("@")).map(|()| Plicity::Implicit))
        .or(pure(Plicity::Explicit))
}

// A lambda parameter with an optional domain annotation and a binder pattern in place of a plain name — the pattern-accepting counterpart of `parse_func_param`, forked rather than generalized in place because `parse_func_param` also serves the out-of-scope `label(params) = value` definition sugar (`parse_tuple_field_prefix`), which stays single-name-only. `(x)` is sugar for `(x : _)`; the annotation, when present, parses as an arbitrary term and stops at the closing `)` (mirrors `parse_func_type_param`).
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
