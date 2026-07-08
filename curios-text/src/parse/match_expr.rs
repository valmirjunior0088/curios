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

pub(super) fn parse_bln_false_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_keep(parse_keyword("false"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

pub(super) fn parse_bln_true_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_keep(parse_keyword("true"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

pub(super) fn parse_bln_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(
            catch(parse_bln_false_branch())
                .and(parse_bln_true_branch())
                .or(parse_bln_true_branch()
                    .and(parse_bln_false_branch())
                    .map(|(tc, fc)| (fc, tc))),
        )
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), (false_case, true_case))| {
            Subterm::Match(Match::Bln(BlnMatch {
                head,
                motive,
                false_case,
                true_case,
            }))
        })
        .map(Into::into)
}

pub(super) fn parse_nat_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(
            catch(
                parse_literal("|")
                    .and_keep(parse_nat())
                    .flat_map(|lit| match lit {
                        NatLiteral::Number(n, _) if n.is_zero() => pure(()),
                        _ => fail("expected 0 as NatFold zero case"),
                    })
                    .and_drop(parse_literal("=>"))
                    .and_keep(lazy(parse_term)),
            )
            .and(
                parse_literal("|")
                    .and_keep(parse_identifier())
                    .and_drop(parse_literal("+"))
                    .and_drop(parse_literal("1"))
                    .and_drop(parse_literal(";"))
                    .and(parse_identifier())
                    .and_drop(parse_literal("=>"))
                    .and(lazy(parse_term)),
            ),
        )
        .and_drop(parse_keyword("end"))
        .map(
            |((head, motive), (zero_case, ((pred_label, ih_label), succ_case)))| {
                Subterm::Match(Match::Nat(NatMatch::Induction {
                    head,
                    motive,
                    zero_case,
                    pred_label: pred_label.to_string(),
                    ih_label: ih_label.to_string(),
                    succ_case,
                }))
            },
        )
        .map(Into::into)
}

pub(super) fn parse_nat_case<'a>() -> Parser<'a, (u32, Term)> {
    catch(parse_literal("|").and_keep(parse_nat_literal_u32()))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
}

pub(super) fn parse_nat_default<'a>() -> Parser<'a, Term> {
    catch(parse_literal("|").and_keep(parse_literal("_")))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

pub(super) fn parse_nat_switch<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(catch(many0(parse_nat_case).and(parse_nat_default())))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), (cases, default))| {
            Subterm::Match(Match::Nat(NatMatch::Dispatch {
                head,
                motive,
                cases: cases.into_iter().collect(),
                default,
            }))
            .into()
        })
}

// A match-arm: `| pattern => body`, where `pattern` may nest across
// constructors, tuples, and structs (see `MatchPattern`, `parse_match_pattern`).
// Full enumeration only ("Path A" — see `to_core::match_compile`'s doc comment): a
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

// The `| [] =>` identity arm of an `Lst` fold (the empty `Lst` literal).
pub(super) fn parse_lst_empty_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_drop(parse_literal("[]"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

// The `; ih =>` tail shared by both carriers' cons arms: `;` sets the
// induction hypothesis apart from the scrutinee's shape. A plain case-split
// needs no induction hypothesis, so `; ih` may be omitted — `None`, not a
// placeholder name; lowering mints a fresh internal name for it directly.
pub(super) fn parse_cons_ih<'a>() -> Parser<'a, Option<String>> {
    catch(parse_literal(";").and_keep(parse_identifier()))
        .map(|name| Some(name.to_string()))
        .or(pure(None))
}

// A cons arm's three binder names: the peeled `head`, the rest `tail`, and
// the optional induction hypothesis `ih` (`None` when `; ih` is omitted).
type ConsLabels = (String, String, Option<String>);

// The `| [head, ..tail]; ih =>` cons arm of an `Lst` fold. Mirrors the `Lst`
// literal's own bracket-and-comma shape (`parse_lst_literal`): `head` is the
// peeled leading element, `tail` the rest.
pub(super) fn parse_lst_cons_branch<'a>() -> Parser<'a, (ConsLabels, Term)> {
    parse_literal("|")
        .and_drop(parse_literal("["))
        .and_keep(parse_identifier())
        .and_drop(parse_literal(","))
        .and_drop(parse_literal(".."))
        .and(parse_identifier())
        .and_drop(parse_literal("]"))
        .and(parse_cons_ih())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|(((head, tail), ih), cons_case)| {
            ((head.to_string(), tail.to_string(), ih), cons_case)
        })
}

// The `| \head\..tail; ih =>` cons arm of a `Bin` fold. Mirrors the `Bin`
// literal's own backslash-delimited shape (`parse_bin_literal`): `head` is
// the leading byte, `tail` the rest.
pub(super) fn parse_bin_cons_branch<'a>() -> Parser<'a, (ConsLabels, Term)> {
    parse_literal("|")
        .and_drop(parse_literal("\\"))
        .and_keep(parse_identifier())
        .and_drop(parse_literal("\\"))
        .and_drop(parse_literal(".."))
        .and(parse_identifier())
        .and(parse_cons_ih())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|(((head, tail), ih), cons_case)| {
            ((head.to_string(), tail.to_string(), ih), cons_case)
        })
}

pub(super) fn parse_lst_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(catch(parse_lst_empty_branch()).and(parse_lst_cons_branch()))
        .and_drop(parse_keyword("end"))
        .map(
            |((head, motive), (empty_case, ((head_label, tail_label, ih_label), cons_case)))| {
                Subterm::Match(Match::Lst(LstMatch {
                    head,
                    motive,
                    empty_case,
                    head_label,
                    tail_label,
                    ih_label,
                    cons_case,
                }))
            },
        )
        .map(Into::into)
}

// The `| \\ =>` identity arm of a `Bin` fold (the empty bytestring literal).
pub(super) fn parse_bin_empty_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_drop(parse_literal("\\\\"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

pub(super) fn parse_bin_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(catch(parse_bin_empty_branch()).and(parse_bin_cons_branch()))
        .and_drop(parse_keyword("end"))
        .map(
            |((head, motive), (empty_case, ((head_label, tail_label, ih_label), cons_case)))| {
                Subterm::Match(Match::Bin(BinMatch {
                    head,
                    motive,
                    empty_case,
                    head_label,
                    tail_label,
                    ih_label,
                    cons_case,
                }))
            },
        )
        .map(Into::into)
}

pub(super) fn parse_match<'a>() -> Parser<'a, Term> {
    catch(parse_bln_match())
        .or(catch(parse_nat_match()))
        .or(catch(parse_nat_switch()))
        .or(catch(parse_lst_match()))
        .or(catch(parse_bin_match()))
        .or(parse_inductive_match())
}
