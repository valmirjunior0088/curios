use {
    super::{
        Apply, Atom, AtomMatch, AtomType, BinLiteral, BlnMatch, Entrypoint, Func, FuncType,
        GroupItem, Let, LetSignature, Match, Module, Motive, Name, Nat, NatLiteral, NatMatch, Path,
        Prim, Proj, Rec, RecItem, Subterm, Term, TopCase, TopItem, TopLet, TopMod, TopUnion,
        TopUse, Tuple, TupleType, UnionCase, UnionMatch, UseGroup,
    },
    crate::{
        Source,
        parser::{
            Parser, ParserError, catch, fail, lazy, many0, many1, pure, run_parser, sep_by0,
            sep_by1, spanned, take_eof, take_exact, take_n, take_while,
        },
    },
    num_bigint::BigUint,
    num_traits::{ToPrimitive, Zero},
    std::{iter, rc::Rc, str::FromStr},
};

const CHARACTERS: &[char] = &['_'];

const KEYWORDS: &[&str] = &[
    "let", "match", "rec", "and", "mod", "use", "pub", "end", "false", "true", "union",
];

fn parse_whitespace<'a>() -> Parser<'a, ()> {
    take_while(|char| char.is_whitespace())
        .and(
            catch(
                take_exact("--")
                    .and_keep(take_while(|char| char != '\n'))
                    .and_keep(lazy(parse_whitespace)),
            )
            .or(pure(())),
        )
        .map(|_| ())
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    take_while(|char| CHARACTERS.contains(&char) || char.is_alphanumeric())
        .flat_map(|identifier| match identifier.is_empty() {
            true => fail("Expected identifier"),
            false => pure(identifier),
        })
        .and_drop(parse_whitespace())
}

fn parse_name<'a>() -> Parser<'a, Name> {
    spanned(
        catch(take_exact("/"))
            .map(|()| true)
            .or(pure(false))
            .and(parse_identifier().and(many0(|| {
                catch(take_exact("/").and_keep(parse_identifier()))
            })))
            .flat_map(|(is_abs, (first, rest))| {
                let segments = iter::once(first)
                    .chain(rest)
                    .map(str::to_string)
                    .collect::<Vec<_>>();

                match segments
                    .iter()
                    .any(|segment| KEYWORDS.contains(&segment.as_str()))
                {
                    true => fail(format!(
                        "path '{}' contains a reserved keyword",
                        segments.join("/")
                    )),
                    false => pure(Name::new(is_abs, Path::from(segments))),
                }
            }),
    )
    .map(|(span, name)| name.with_span(span))
}

fn parse_qualified_name<'a>() -> Parser<'a, Name> {
    catch(parse_name().flat_map(|name| match name.is_single() {
        true => fail("expected a qualified path"),
        false => pure(name),
    }))
}

fn parse_keyword<'a>(expected: &'static str) -> Parser<'a, ()> {
    parse_identifier().flat_map(move |obtained| match expected == obtained {
        true => pure(()),
        false => fail(format!(
            "Expected keyword '{expected}', obtained '{obtained}'"
        )),
    })
}

fn parse_type<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Type"))
        .map(|()| Subterm::Type)
        .map(Into::into)
}

fn parse_int_value<'a>() -> Parser<'a, Term> {
    catch(
        take_n(1)
            .flat_map(|sign| match sign {
                "+" | "-" => pure(sign.to_string()),
                _ => fail("Expected '+' or '-'"),
            })
            .and(take_while(|char| char.is_ascii_digit()))
            .flat_map::<i32, _>(|(sign, digits)| match format!("{sign}{digits}").parse() {
                Ok(value) => pure(value),
                Err(_) => fail("Expected integer literal"),
            })
            .and_drop(parse_whitespace()),
    )
    .map(|value| Subterm::Prim(Prim::Int(value)))
    .map(Into::into)
}

fn parse_usize<'a>() -> Parser<'a, usize> {
    take_while(|char: char| char.is_ascii_digit())
        .flat_map(|digits| match digits.parse::<usize>() {
            Ok(value) => pure(value),
            Err(_) => fail("expected usize"),
        })
        .and_drop(parse_whitespace())
}

fn parse_nat_digits<'a>() -> Parser<'a, BigUint> {
    take_while(|char: char| char.is_ascii_digit())
        .flat_map(|digits| match digits.parse::<BigUint>() {
            Ok(value) => pure(value),
            Err(_) => fail("expected nat"),
        })
        .and_drop(parse_whitespace())
}

fn parse_nat<'a>() -> Parser<'a, NatLiteral> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(NatLiteral::Char)
    .or(catch(parse_nat_digits()).map(NatLiteral::number))
}

fn parse_nat_value<'a>() -> Parser<'a, Term> {
    parse_nat()
        .map(|nat| match nat {
            NatLiteral::Number(n) if n.is_zero() => Subterm::Prim(Prim::Nat(Nat::Zero)),
            nat => Subterm::Prim(Prim::Nat(Nat::Succ(
                nat,
                Subterm::Prim(Prim::Nat(Nat::Zero)).into(),
            ))),
        })
        .map(Into::into)
}

// Successor as the infix `+`: a nat literal on either side of a base term
// produces `Nat::Succ(literal, base)`. Whitespace around `+` is conventional
// (keeps `n + 1` distinct from the `+1` int literal at a glance).
fn parse_nat_succ<'a>() -> Parser<'a, Term> {
    let lit_first = catch(parse_nat_literal().and_drop(parse_literal("+")))
        .and(parse_atomic_term())
        .map(|(spine, base)| Subterm::Prim(Prim::Nat(Nat::Succ(spine, base))));

    let base_first = catch(parse_atomic_term().and_drop(parse_literal("+")))
        .and(parse_nat_literal())
        .map(|(base, spine)| Subterm::Prim(Prim::Nat(Nat::Succ(spine, base))));

    lit_first.or(base_first).map(Into::into)
}

fn parse_nat_literal<'a>() -> Parser<'a, NatLiteral> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(NatLiteral::Char)
    .or(catch(parse_nat_digits()).map(NatLiteral::number))
}

fn parse_nat_literal_u32<'a>() -> Parser<'a, u32> {
    parse_nat_literal().flat_map(|lit| {
        let n = match lit {
            NatLiteral::Number(n) => n,
            NatLiteral::Char(c) => BigUint::from(c as u32),
        };
        match n.to_u32() {
            Some(k) => pure(k),
            None => fail("nat literal too large for u32"),
        }
    })
}

fn parse_flt_value<'a>() -> Parser<'a, Term> {
    catch(
        take_n(1)
            .flat_map(|sign| match sign {
                "+" | "-" => pure(sign.to_string()),
                _ => fail("Expected '+' or '-'"),
            })
            .and(take_while(|char| {
                ".-+eE".contains(char) || char.is_ascii_digit()
            }))
            .flat_map::<f32, _>(|(sign, digits)| {
                let has_dot = digits.contains('.');

                let has_decimal = digits
                    .split_once('.')
                    .map(|(_, suffix)| {
                        suffix
                            .chars()
                            .next()
                            .is_some_and(|char| char.is_ascii_digit())
                    })
                    .unwrap_or(false);

                if !has_dot || !has_decimal {
                    return fail("Expected float literal with dot and decimal");
                }

                match format!("{sign}{digits}").parse() {
                    Ok(value) => pure(value),
                    Err(_) => fail("Expected float literal"),
                }
            })
            .and_drop(parse_whitespace()),
    )
    .map(|value| Subterm::Prim(Prim::Flt(value)))
    .map(Into::into)
}

fn parse_hex_byte<'a>() -> Parser<'a, u8> {
    take_exact("\\").and_keep(take_while(|char| char.is_ascii_hexdigit()).flat_map(|hex| {
        match hex.len() {
            2 => pure(u8::from_str_radix(hex, 16).expect("valid hex pair")),
            _ => fail("Expected exactly 2 hex digits after \\"),
        }
    }))
}

fn parse_string_chunk<'a>() -> Parser<'a, String> {
    catch(
        take_while(|char| char != '\\' && char != '"').flat_map(|chunk| match chunk.is_empty() {
            true => fail("empty chunk"),
            false => pure(chunk.to_string()),
        }),
    )
    .or(catch(take_exact("\\")).and_keep(
        take_exact("n")
            .map(|_| "\n".to_string())
            .or(take_exact("t").map(|()| "\t".to_string()))
            .or(take_exact("r").map(|()| "\r".to_string()))
            .or(take_exact("\\").map(|()| "\\".to_string()))
            .or(take_exact("\"").map(|()| "\"".to_string()))
            .or(fail("Unknown string escape sequence")),
    ))
}

fn parse_char_value<'a>() -> Parser<'a, char> {
    catch(take_exact("\\"))
        .and_keep(
            take_exact("n")
                .map(|_| '\n')
                .or(take_exact("t").map(|_| '\t'))
                .or(take_exact("r").map(|_| '\r'))
                .or(take_exact("\\").map(|_| '\\'))
                .or(take_exact("'").map(|_| '\''))
                .or(fail("Unknown char escape sequence")),
        )
        .or(
            take_n(1).flat_map(|string| match string.chars().next().unwrap() {
                '\'' => fail("use \\' to include a single quote in a char literal"),
                char => pure(char),
            }),
        )
}

fn parse_string_literal<'a>() -> Parser<'a, Term> {
    catch(take_exact("\""))
        .and_keep(many0(parse_string_chunk))
        .and_drop(take_exact("\""))
        .and_drop(parse_whitespace())
        .map(|chunks| Subterm::Prim(Prim::Bin(BinLiteral::String(chunks.concat()))))
        .map(Into::into)
}

fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    catch(many1(parse_hex_byte).and_drop(parse_whitespace()))
        .map(|bytes| Subterm::Prim(Prim::Bin(BinLiteral::Bytes(bytes))))
        .map(Into::into)
}

fn parse_arr_literal<'a>() -> Parser<'a, Term> {
    catch(parse_literal("["))
        .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(|elems| Subterm::Prim(Prim::Arr(elems.into_iter().collect())))
        .map(Into::into)
}

fn parse_bln_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("false"))
        .map(|()| Subterm::Prim(Prim::Bln(false)))
        .or(catch(parse_keyword("true")).map(|()| Subterm::Prim(Prim::Bln(true))))
        .map(Into::into)
}

// Primitive types and operations are no longer surface syntax — they live in the
// `sys` module (see `prelude.rs`) and parse as ordinary names. Only genuine
// literals (and the boolean keywords) remain here.
fn parse_prim<'a>() -> Parser<'a, Term> {
    parse_bln_prim()
        .or(parse_flt_value())
        .or(parse_int_value())
        .or(parse_nat_value())
        .or(parse_string_literal())
        .or(parse_bin_literal())
        .or(parse_arr_literal())
}

fn parse_atom_label<'a>() -> Parser<'a, Atom> {
    take_exact("'").and_keep(parse_identifier()).map(Atom::from)
}

fn parse_atom<'a>() -> Parser<'a, Term> {
    parse_atom_label().map(Subterm::Atom).map(Into::into)
}

fn parse_atom_type<'a>() -> Parser<'a, Term> {
    parse_literal("'[")
        .and_keep(sep_by0(
            || parse_identifier().map(Atom::from),
            || parse_literal(","),
        ))
        .and_drop(parse_literal("]"))
        .map(|atoms| {
            Subterm::AtomType(AtomType {
                atoms: atoms.into_iter().collect(),
            })
        })
        .map(Into::into)
}

fn parse_parens<'a>() -> Parser<'a, Term> {
    parse_literal("(")
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(")"))
}

fn parse_tuple_type_field<'a>() -> Parser<'a, (Option<String>, Term)> {
    catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(label, term): (&str, Term)| (Some(label.to_string()), term))
        .or(lazy(parse_term).map(|term| (None, term)))
}

fn parse_tuple_type<'a>() -> Parser<'a, Term> {
    catch(parse_literal("{"))
        .and_keep(sep_by0(parse_tuple_type_field, || parse_literal(",")))
        .and_drop(parse_literal("}"))
        .map(|fields| {
            Subterm::TupleType(TupleType {
                fields: fields.into_iter().collect(),
            })
        })
        .map(Into::into)
}

fn parse_tuple<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(",")),
    )
    .and(sep_by0(|| lazy(parse_term), || parse_literal(",")))
    .and_drop(parse_literal(")"))
    .map(|(first, rest)| {
        Subterm::Tuple(Tuple {
            fields: iter::once(first).chain(rest).collect(),
        })
    })
    .map(Into::into)
}

fn parse_func_type_param<'a>() -> Parser<'a, (Option<String>, Term)> {
    catch(parse_identifier().and_drop(parse_literal(":")))
        .and(lazy(parse_term))
        .map(|(label, ty): (&str, Term)| (Some(label.to_string()), ty))
        .or(lazy(parse_term).map(|ty| (None, ty)))
}

fn parse_paren_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_type_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(|(params, output): (Vec<(Option<String>, Term)>, Term)| {
        Subterm::FuncType(FuncType {
            params: params.into_iter().collect(),
            output,
        })
    })
    .map(Into::into)
}

fn parse_non_dependent_func_type<'a>() -> Parser<'a, Term> {
    catch(parse_atomic_term().and_drop(parse_literal("->")))
        .and(lazy(parse_term))
        .map(|(input, output)| {
            Subterm::FuncType(FuncType {
                params: vec![(None, input)],
                output,
            })
        })
        .map(Into::into)
}

fn parse_func_type<'a>() -> Parser<'a, Term> {
    parse_paren_func_type().or(parse_non_dependent_func_type())
}

fn parse_func<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(
                || parse_identifier().map(|s: &str| s.to_string()),
                || parse_literal(","),
            ))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(params, body)| Subterm::Func(Func { params, body }).into())
}

fn parse_motive<'a>() -> Parser<'a, Motive> {
    catch(parse_identifier().and_drop(parse_literal("=>")))
        .map(|label| Some(label.to_string()))
        .or(pure(None))
        .and(lazy(parse_term))
        .map(|(label, body)| Motive { label, body })
}

fn parse_match_prefix<'a>() -> Parser<'a, (Term, Motive)> {
    catch(parse_keyword("match"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(":"))
        .and(parse_motive())
}

fn parse_bln_false_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_keep(parse_keyword("false"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

fn parse_bln_true_branch<'a>() -> Parser<'a, Term> {
    parse_literal("|")
        .and_keep(parse_keyword("true"))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

fn parse_bln_match<'a>() -> Parser<'a, Term> {
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

fn parse_nat_fold_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(
            catch(
                parse_literal("|")
                    .and_keep(parse_nat_literal())
                    .flat_map(|lit| match lit {
                        NatLiteral::Number(n) if n.is_zero() => pure(()),
                        _ => fail("expected 0 as NatFold zero case"),
                    })
                    .and_drop(parse_literal("=>"))
                    .and_keep(lazy(parse_term)),
            )
            .and(
                parse_literal("|")
                    .and_keep(parse_identifier())
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

fn parse_nat_case<'a>() -> Parser<'a, (u32, Term)> {
    catch(parse_literal("|").and_keep(parse_nat_literal_u32()))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
}

fn parse_nat_default<'a>() -> Parser<'a, Term> {
    catch(parse_literal("|").and_keep(parse_literal("_")))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
}

fn parse_nat_match<'a>() -> Parser<'a, Term> {
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

fn parse_atom_branch<'a>() -> Parser<'a, (Atom, Term)> {
    catch(parse_literal("|").and_keep(parse_atom_label()))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
}

fn parse_atom_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(many1(parse_atom_branch))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), cases)| {
            Subterm::Match(Match::Atom(AtomMatch {
                head,
                motive,
                cases: cases.into_iter().collect(),
            }))
        })
        .map(Into::into)
}

fn parse_union_match_branch<'a>() -> Parser<'a, (String, UnionCase)> {
    catch(parse_literal("|").and_keep(parse_identifier()))
        .and(
            parse_literal("(")
                .and_keep(sep_by0(
                    || parse_identifier().map(|s: &str| s.to_string()),
                    || parse_literal(","),
                ))
                .and_drop(parse_literal(")")),
        )
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|((label, binders), body): ((&str, Vec<String>), Term)| {
            (label.to_string(), UnionCase { binders, body })
        })
}

fn parse_union_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(many1(parse_union_match_branch))
        .and_drop(parse_keyword("end"))
        .map(|((head, motive), branches)| {
            Subterm::Match(Match::Union(UnionMatch {
                head,
                motive,
                cases: branches.into_iter().collect(),
            }))
        })
        .map(Into::into)
}

fn parse_match<'a>() -> Parser<'a, Term> {
    catch(parse_bln_match())
        .or(catch(parse_nat_fold_match()))
        .or(catch(parse_nat_match()))
        .or(catch(parse_union_match()))
        .or(parse_atom_match())
}

fn parse_binding<'a>() -> Parser<'a, RecItem> {
    parse_identifier()
        .and(parse_let_signature())
        .map(|(label, signature)| RecItem {
            label: label.to_string(),
            signature,
        })
}

fn parse_rec<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("rec"))
        .and_keep(sep_by1(parse_binding, || parse_keyword("and")))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(items, tail)| Subterm::Rec(Rec { items, tail }))
        .map(Into::into)
}

fn parse_func_sugar_param<'a>() -> Parser<'a, (String, Term)> {
    parse_identifier()
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(|(name, ty): (&str, Term)| (name.to_string(), ty))
}

// Parses the part of a `let` binding after its name. Yields either
// `LetSignature::Func` for the function-definition sugar
// `(p : T, ...) -> R = body`, or `LetSignature::Name` for the plain
// `: T = body` form.
fn parse_let_signature<'a>() -> Parser<'a, LetSignature> {
    let func = catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_sugar_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal("="))
    .and(lazy(parse_term))
    .map(
        |((params, output), body): ((Vec<(String, Term)>, Term), Term)| LetSignature::Func {
            params,
            output,
            body,
        },
    );

    let name = catch(parse_literal(":"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|(type_, body)| LetSignature::Name { type_, body });

    func.or(name)
}

fn parse_let<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let"))
        .and_keep(parse_identifier())
        .and(parse_let_signature())
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|((label, signature), tail)| {
            Subterm::Let(Let {
                label: label.to_string(),
                signature,
                tail,
            })
        })
        .map(Into::into)
}

fn parse_proj_suffix<'a>() -> Parser<'a, usize> {
    catch(take_exact(".").and_keep(parse_usize().map_err("Expected numeric index after '.'")))
}

enum Suffix {
    Proj(usize),
    Apply(Vec<Term>),
}

fn parse_suffix<'a>() -> Parser<'a, Suffix> {
    parse_proj_suffix()
        .map(Suffix::Proj)
        .or(catch(parse_literal("("))
            .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .map(Suffix::Apply))
}

fn parse_empty_tuple<'a>() -> Parser<'a, Term> {
    catch(parse_literal("(").and_keep(parse_literal(")")))
        .map(|_| Subterm::Tuple(Tuple { fields: vec![] }))
        .map(Into::into)
}

fn with_span<'a>(parser: Parser<'a, Term>) -> Parser<'a, Term> {
    spanned(parser).map(|(span, term)| term.with_span(span))
}

fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    with_span(
        parse_qualified_name()
            .map(|n| Subterm::Name(n).into())
            .or(parse_type())
            .or(parse_prim())
            .or(parse_atom_type())
            .or(parse_atom())
            .or(parse_tuple_type())
            .or(parse_empty_tuple())
            .or(parse_tuple())
            .or(parse_parens())
            .or(parse_name().map(|n| Subterm::Name(n).into()))
            .and(many0(parse_suffix))
            .map(|(head, suffixes): (Term, _)| {
                suffixes
                    .into_iter()
                    .fold(head, |head, suffix| match suffix {
                        Suffix::Proj(index) => Subterm::Proj(Proj { head, index }).into(),
                        Suffix::Apply(params) => Subterm::Apply(Apply { head, params }).into(),
                    })
            }),
    )
}

fn parse_term<'a>() -> Parser<'a, Term> {
    with_span(
        parse_rec()
            .or(parse_let())
            .or(parse_match())
            .or(parse_func_type())
            .or(parse_func())
            .or(parse_nat_succ())
            .or(parse_atomic_term()),
    )
}

impl Term {
    pub fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(parse_term())
                .and_drop(take_eof()),
            source,
        )
    }
}

impl FromStr for Term {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Term::parse(&Source::inline(input))
    }
}

fn parse_pub<'a>() -> Parser<'a, bool> {
    catch(parse_keyword("pub")).map(|()| true).or(pure(false))
}

fn parse_top_let<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("let"))).flat_map(|(is_pub, ())| {
        parse_identifier()
            .and(parse_let_signature())
            .and_drop(parse_literal(";"))
            .map(move |(label, signature)| {
                TopItem::Let(TopLet {
                    is_pub,
                    label: label.to_string(),
                    signature,
                })
            })
    })
}

fn parse_top_rec<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("rec")))
        .flat_map(|(is_pub, ())| {
            parse_binding().map(move |item| TopLet {
                is_pub,
                label: item.label,
                signature: item.signature,
            })
        })
        .and(many0(|| {
            catch(parse_pub().and(parse_keyword("and"))).flat_map(|(is_pub, ())| {
                parse_binding().map(move |item| TopLet {
                    is_pub,
                    label: item.label,
                    signature: item.signature,
                })
            })
        }))
        .and_drop(parse_literal(";"))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Rec)
}

fn parse_top_mod<'a>() -> Parser<'a, TopItem> {
    spanned(catch(parse_pub().and(parse_keyword("mod"))).flat_map(|(is_pub, ())| {
        parse_identifier().flat_map(move |name| {
            catch(
                many0(parse_top_item)
                    .and_drop(parse_keyword("end"))
                    .map(|items| Some(Module { items })),
            )
            .or(parse_literal(";").map(|()| None))
            .map(move |module| (is_pub, name.to_string(), module))
        })
    }))
    .map(|(span, (is_pub, label, module))| {
        TopItem::Mod(TopMod {
            span: Some(span),
            is_pub,
            label,
            module,
        })
    })
}

// Like `parse_name`, but additionally accepts an empty absolute path. The
// leading `/` is only consumed when followed by an identifier — so for
// `use /{X};` the path is empty-abs (consumes nothing) and `/` is left for
// `parse_use_group` to consume as its separator.
fn parse_use_path<'a>() -> Parser<'a, Name> {
    spanned(
        catch(take_exact("/").and_keep(parse_identifier()).and(many0(|| {
            catch(take_exact("/").and_keep(parse_identifier()))
        })))
        .map(|(first, rest)| {
            Name::new(
                true,
                Path::from(
                    iter::once(first)
                        .chain(rest)
                        .map(str::to_string)
                        .collect::<Vec<_>>(),
                ),
            )
        })
        .or(catch(parse_identifier().and(many0(|| {
            catch(take_exact("/").and_keep(parse_identifier()))
        })))
        .map(|(first, rest)| {
            Name::new(
                false,
                Path::from(
                    iter::once(first)
                        .chain(rest)
                        .map(str::to_string)
                        .collect::<Vec<_>>(),
                ),
            )
        }))
        .or(pure(Name::new(true, Path::empty())))
        .flat_map(|name| {
            match name
                .path()
                .segments()
                .iter()
                .any(|segment| KEYWORDS.contains(&segment.as_str()))
            {
                true => fail(format!(
                    "path '{}' contains a reserved keyword",
                    name.path().join()
                )),
                false => pure(name),
            }
        }),
    )
    .map(|(span, name)| name.with_span(span))
}

fn parse_group_item<'a>() -> Parser<'a, GroupItem> {
    catch(parse_keyword("mod").and_keep(parse_identifier()))
        .map(|s| GroupItem::Mod(s.to_string()))
        .or(catch(parse_keyword("let").and_keep(parse_identifier()))
            .map(|s| GroupItem::Let(s.to_string())))
        .or(parse_identifier().map(|s| GroupItem::Both(s.to_string())))
}

fn parse_brace_group<'a>() -> Parser<'a, Vec<GroupItem>> {
    catch(parse_literal("{"))
        .and_keep(sep_by0(parse_group_item, || parse_literal(",")))
        .and_drop(parse_literal("}"))
}

fn parse_use_group<'a>() -> Parser<'a, UseGroup> {
    catch(take_exact("/").and_keep(parse_brace_group()))
        .map(UseGroup::Named)
        .or(catch(take_exact("/").and_keep(parse_literal("*"))).map(|()| UseGroup::Glob))
}

fn parse_top_use<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("use"))).flat_map(|(is_pub, ())| {
        parse_use_path()
            .and(parse_use_group())
            .and_drop(parse_literal(";"))
            .map(move |(name, group)| {
                TopItem::Use(TopUse {
                    is_pub,
                    name,
                    group,
                })
            })
    })
}

fn parse_top_union_case<'a>() -> Parser<'a, TopCase> {
    parse_literal("|")
        .and_keep(parse_identifier())
        .and(
            parse_literal("(")
                .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
                .and_drop(parse_literal(")")),
        )
        .map(|(label, payload_types): (&str, Vec<Term>)| TopCase {
            label: label.to_string(),
            payload_types: payload_types.into_iter().collect(),
        })
}

fn parse_top_union_body<'a>(is_pub: bool) -> Parser<'a, TopUnion> {
    parse_identifier()
        .and(
            catch(
                parse_literal("(")
                    .and_keep(sep_by0(parse_func_sugar_param, || parse_literal(",")))
                    .and_drop(parse_literal(")")),
            )
            .or(pure(vec![])),
        )
        .and(many1(parse_top_union_case))
        .map(move |((label, params), cases)| TopUnion {
            is_pub,
            label: label.to_string(),
            params: params.into_iter().collect(),
            cases,
        })
}

fn parse_top_union<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("union"))).flat_map(|(is_pub, ())| {
        parse_top_union_body(is_pub)
            .and(many0(|| {
                catch(parse_pub().and(parse_keyword("and")))
                    .flat_map(|(is_pub2, ())| parse_top_union_body(is_pub2))
            }))
            .and_drop(parse_keyword("end"))
            .map(|(first, rest)| TopItem::Union(iter::once(first).chain(rest).collect()))
    })
}

fn parse_top_item<'a>() -> Parser<'a, TopItem> {
    parse_top_mod()
        .or(parse_top_use())
        .or(parse_top_let())
        .or(parse_top_union())
        .or(parse_top_rec())
}

impl Module {
    pub fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and_drop(take_eof())
                .map(|items| Module { items }),
            source,
        )
    }
}

impl FromStr for Module {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Module::parse(&Source::inline(input))
    }
}

impl Entrypoint {
    pub fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and(lazy(parse_term))
                .and_drop(take_eof())
                .map(|(items, tail)| Entrypoint::new(items, tail)),
            source,
        )
    }
}

impl FromStr for Entrypoint {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Entrypoint::parse(&Source::inline(input))
    }
}

