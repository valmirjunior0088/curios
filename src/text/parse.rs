use {
    super::{
        Apply, Atom, AtomMatch, AtomType, BinLiteral, BlnMatch, Entrypoint, Func, FuncType, Let,
        Match, Module, Motive, Name, Nat, NatLiteral, NatMatch, Prim, Proj, Rec, RecItem, Term,
        TopItem, TopLet, TopMod, TopUse, Tuple, TupleType,
    },
    crate::parser::{
        Parser, ParserError, catch, fail, lazy, many0, many1, pure, run_parser, sep_by0, sep_by1,
        spanned, take_eof, take_exact, take_n, take_while,
    },
    std::{iter, str::FromStr},
};

const CHARACTERS: &[char] = &['_'];

const KEYWORDS: &[&str] = &[
    "let", "match", "rec", "and", "mod", "use", "pub", "end", "false", "true",
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
    parse_identifier()
        .and(many0(|| {
            catch(take_exact("/").and_keep(parse_identifier()))
        }))
        .map(|(first, rest)| {
            iter::once(first)
                .chain(rest)
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .flat_map(|path| {
            match path
                .iter()
                .any(|segment| KEYWORDS.contains(&segment.as_str()))
            {
                true => fail(format!(
                    "path '{}' contains a reserved keyword",
                    path.join("/")
                )),
                false => pure(Name::from(path)),
            }
        })
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
    catch(parse_keyword("Type")).map(|()| Term::Type)
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
    .map(|value| Term::Prim(Prim::Int(value)))
}

fn parse_u32<'a>() -> Parser<'a, u32> {
    take_while(|char: char| char.is_ascii_digit())
        .flat_map(|digits| match digits.parse::<u32>() {
            Ok(value) => pure(value),
            Err(_) => fail("expected u32"),
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
    .map(NatLiteral::from)
    .or(catch(parse_u32()).map(NatLiteral::from))
}

fn parse_nat_value<'a>() -> Parser<'a, Term> {
    parse_nat().map(|nat| match nat {
        NatLiteral::Number(0) => Term::Prim(Prim::Nat(Nat::Zero)),
        nat => Term::Prim(Prim::Nat(Nat::Succ(
            nat,
            Box::new(Term::Prim(Prim::Nat(Nat::Zero))),
        ))),
    })
}

fn parse_nat_succ<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("Nat.succ")
            .and_drop(parse_literal("("))
            .and_keep(parse_nat_literal())
            .and_drop(parse_literal(",")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(")"))
    .map(|(spine, inner)| {
        Term::Prim(Prim::Nat(Nat::Succ(
            NatLiteral::Number(spine),
            Box::new(inner),
        )))
    })
    .or(parse_prim1("Nat.succ", |a| {
        Prim::Nat(Nat::Succ(NatLiteral::Number(1), Box::new(a)))
    }))
}

fn parse_nat_literal<'a>() -> Parser<'a, u32> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(|c| c as u32)
    .or(catch(parse_u32()))
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
    .map(|value| Term::Prim(Prim::Flt(value)))
}

fn parse_prim1<'a, F>(name: &'static str, ctor: F) -> Parser<'a, Term>
where
    F: FnOnce(Term) -> Prim + 'a,
{
    catch(parse_literal(name).and_drop(parse_literal("(")))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(")"))
        .map(move |a| Term::Prim(ctor(a)))
}

fn parse_prim2<'a, F>(name: &'static str, ctor: F) -> Parser<'a, Term>
where
    F: FnOnce(Term, Term) -> Prim + 'a,
{
    catch(parse_literal(name).and_drop(parse_literal("(")))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(","))
        .and(lazy(parse_term))
        .and_drop(parse_literal(")"))
        .map(move |(a, b)| Term::Prim(ctor(a, b)))
}

fn parse_prim3<'a, F>(name: &'static str, ctor: F) -> Parser<'a, Term>
where
    F: FnOnce(Term, Term, Term) -> Prim + 'a,
{
    catch(parse_literal(name).and_drop(parse_literal("(")))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(","))
        .and(lazy(parse_term))
        .and_drop(parse_literal(","))
        .and(lazy(parse_term))
        .and_drop(parse_literal(")"))
        .map(move |((a, b), c)| Term::Prim(ctor(a, b, c)))
}

fn parse_prim_variadic<'a, F>(name: &'static str, ctor: F) -> Parser<'a, Term>
where
    F: FnOnce(Vec<Term>) -> Prim + 'a,
{
    catch(parse_literal(name).and_drop(parse_literal("(")))
        .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .map(move |ops| Term::Prim(ctor(ops)))
}

fn parse_nat_prim<'a>() -> Parser<'a, Term> {
    parse_prim2("Nat.eql", Prim::nat_eql)
        .or(parse_prim2("Nat.neq", Prim::nat_neq))
        .or(parse_prim2("Nat.add", Prim::nat_add))
        .or(parse_prim2("Nat.sub", Prim::nat_sub))
        .or(parse_prim2("Nat.mul", Prim::nat_mul))
        .or(parse_prim2("Nat.lte", Prim::nat_lte))
        .or(parse_prim2("Nat.gte", Prim::nat_gte))
        .or(parse_prim2("Nat.lt", Prim::nat_lt))
        .or(parse_prim2("Nat.div", Prim::nat_div))
        .or(parse_prim2("Nat.rem", Prim::nat_rem))
        .or(parse_prim2("Nat.gt", Prim::nat_gt))
        .or(parse_prim1("Nat.to_int", Prim::nat_to_int))
        .or(parse_prim1("Nat.to_flt", Prim::nat_to_flt))
        .or(parse_prim1("Nat.to_str", Prim::nat_to_str))
        .or(parse_nat_succ())
        .or(parse_nat_value())
        .or(catch(parse_keyword("Nat")).map(|()| Term::Prim(Prim::NatType)))
}

fn parse_int_prim<'a>() -> Parser<'a, Term> {
    parse_prim2("Int.eql", Prim::int_eql)
        .or(parse_prim2("Int.neq", Prim::int_neq))
        .or(parse_prim2("Int.add", Prim::int_add))
        .or(parse_prim2("Int.sub", Prim::int_sub))
        .or(parse_prim2("Int.mul", Prim::int_mul))
        .or(parse_prim2("Int.div", Prim::int_div))
        .or(parse_prim2("Int.rem", Prim::int_rem))
        .or(parse_prim2("Int.lte", Prim::int_lte))
        .or(parse_prim2("Int.gte", Prim::int_gte))
        .or(parse_prim2("Int.lt", Prim::int_lt))
        .or(parse_prim2("Int.gt", Prim::int_gt))
        .or(parse_prim1("Int.to_nat", Prim::int_to_nat))
        .or(parse_prim1("Int.to_flt", Prim::int_to_flt))
        .or(parse_prim1("Int.to_str", Prim::int_to_str))
        .or(parse_int_value())
        .or(catch(parse_keyword("Int")).map(|()| Term::Prim(Prim::IntType)))
}

fn parse_flt_prim<'a>() -> Parser<'a, Term> {
    parse_prim2("Flt.add", Prim::flt_add)
        .or(parse_prim2("Flt.sub", Prim::flt_sub))
        .or(parse_prim2("Flt.mul", Prim::flt_mul))
        .or(parse_prim2("Flt.div", Prim::flt_div))
        .or(parse_prim2("Flt.eql", Prim::flt_eql))
        .or(parse_prim2("Flt.neq", Prim::flt_neq))
        .or(parse_prim2("Flt.lte", Prim::flt_lte))
        .or(parse_prim2("Flt.gte", Prim::flt_gte))
        .or(parse_prim2("Flt.lt", Prim::flt_lt))
        .or(parse_prim2("Flt.gt", Prim::flt_gt))
        .or(parse_prim2("Flt.min", Prim::flt_min))
        .or(parse_prim2("Flt.max", Prim::flt_max))
        .or(parse_prim1("Flt.neg", Prim::flt_neg))
        .or(parse_prim1("Flt.abs", Prim::flt_abs))
        .or(parse_prim1("Flt.sqrt", Prim::flt_sqrt))
        .or(parse_prim1("Flt.floor", Prim::flt_floor))
        .or(parse_prim1("Flt.ceil", Prim::flt_ceil))
        .or(parse_prim1("Flt.trunc", Prim::flt_trunc))
        .or(parse_prim1("Flt.nearest", Prim::flt_nearest))
        .or(parse_prim1("Flt.to_nat", Prim::flt_to_nat))
        .or(parse_prim1("Flt.to_int", Prim::flt_to_int))
        .or(parse_prim1("Flt.to_str", Prim::flt_to_str))
        .or(parse_flt_value())
        .or(catch(parse_keyword("Flt")).map(|()| Term::Prim(Prim::FltType)))
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
        .map(|chunks| Term::Prim(Prim::Bin(BinLiteral::String(chunks.concat()))))
}

fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    catch(many1(parse_hex_byte).and_drop(parse_whitespace()))
        .map(|bytes| Term::Prim(Prim::Bin(BinLiteral::Bytes(bytes))))
}

fn parse_bin_prim<'a>() -> Parser<'a, Term> {
    parse_prim1("Bin.len", Prim::bin_len)
        .or(parse_prim2("Bin.eql", Prim::bin_eql))
        .or(parse_prim2("Bin.get", Prim::bin_get))
        .or(parse_prim3("Bin.slice", Prim::bin_slice))
        .or(parse_prim2("Bin.append", Prim::bin_append))
        .or(parse_prim_variadic("Bin.concat", |ops| {
            Prim::bin_concat(ops)
        }))
        .or(catch(parse_keyword("Bin")).map(|()| Term::Prim(Prim::BinType)))
        .or(parse_string_literal())
        .or(parse_bin_literal())
}

fn parse_arr_literal<'a>() -> Parser<'a, Term> {
    catch(parse_literal("["))
        .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(|elems| {
            Term::Prim(Prim::Arr(
                elems.into_iter().map(|elem| elem.into()).collect(),
            ))
        })
}

fn parse_arr_prim<'a>() -> Parser<'a, Term> {
    parse_prim1("Arr.len", Prim::arr_len)
        .or(parse_prim2("Arr.get", Prim::arr_get))
        .or(parse_prim3("Arr.slice", Prim::arr_slice))
        .or(parse_prim2("Arr.append", Prim::arr_append))
        .or(parse_prim_variadic("Arr.concat", |ops| {
            Prim::arr_concat(ops)
        }))
        .or(catch(parse_keyword("Arr").and_drop(parse_literal("(")))
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(")"))
            .map(|elem| Term::Prim(Prim::arr_type(elem))))
        .or(parse_arr_literal())
}

fn parse_sys_prim<'a>() -> Parser<'a, Term> {
    parse_prim1("Sys.print", Prim::sys_print)
        .or(catch(parse_literal("Sys.read")).map(|_| Term::Prim(Prim::SysRead)))
}

fn parse_bln_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Bln"))
        .map(|()| Term::Prim(Prim::BlnType))
        .or(catch(parse_keyword("false")).map(|()| Term::Prim(Prim::Bln(false))))
        .or(catch(parse_keyword("true")).map(|()| Term::Prim(Prim::Bln(true))))
}

fn parse_prim<'a>() -> Parser<'a, Term> {
    parse_bln_prim()
        .or(parse_flt_prim())
        .or(parse_int_prim())
        .or(parse_nat_prim())
        .or(parse_bin_prim())
        .or(parse_arr_prim())
        .or(parse_sys_prim())
}

fn parse_atom_label<'a>() -> Parser<'a, Atom> {
    take_exact("'").and_keep(parse_identifier()).map(Atom::from)
}

fn parse_atom<'a>() -> Parser<'a, Term> {
    parse_atom_label().map(Term::Atom)
}

fn parse_atom_type<'a>() -> Parser<'a, Term> {
    parse_literal("'[")
        .and_keep(sep_by0(
            || parse_identifier().map(Atom::from),
            || parse_literal(","),
        ))
        .and_drop(parse_literal("]"))
        .map(|atoms| {
            Term::AtomType(AtomType {
                atoms: atoms.into_iter().collect(),
            })
        })
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
            Term::TupleType(TupleType {
                fields: fields
                    .into_iter()
                    .map(|(label, term)| (label, term.into()))
                    .collect(),
            })
        })
}

fn parse_tuple<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(",")),
    )
    .and(sep_by1(|| lazy(parse_term), || parse_literal(",")))
    .and_drop(parse_literal(")"))
    .map(|(first, rest)| {
        Term::Tuple(Tuple {
            fields: iter::once(first)
                .chain(rest)
                .map(|term| term.into())
                .collect(),
        })
    })
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
        Term::FuncType(FuncType {
            params: params.into_iter().map(|(l, t)| (l, t.into())).collect(),
            output: output.into(),
        })
    })
}

fn parse_non_dependent_func_type<'a>() -> Parser<'a, Term> {
    catch(parse_atomic_term().and_drop(parse_literal("->")))
        .and(lazy(parse_term))
        .map(|(input, output)| {
            Term::FuncType(FuncType {
                params: vec![(None, input.into())],
                output: output.into(),
            })
        })
}

fn parse_func_type<'a>() -> Parser<'a, Term> {
    parse_paren_func_type().or(parse_non_dependent_func_type())
}

fn parse_func<'a>() -> Parser<'a, Term> {
    let multi = catch(
        parse_literal("(")
            .and_keep(sep_by0(
                || parse_identifier().map(|s: &str| s.to_string()),
                || parse_literal(","),
            ))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .map(|(params, body)| {
        Term::Func(Func {
            params,
            body: body.into(),
        })
    });

    let single = catch(parse_identifier().and_drop(parse_literal("=>")))
        .and(lazy(parse_term))
        .map(|(label, body): (&str, Term)| {
            Term::Func(Func {
                params: vec![label.to_string()],
                body: body.into(),
            })
        });

    multi.or(single)
}

fn parse_motive<'a>() -> Parser<'a, Motive> {
    catch(parse_identifier().and_drop(parse_literal("=>")))
        .map(|label| Some(label.to_string()))
        .or(pure(None))
        .and(lazy(parse_term))
        .map(|(label, body)| Motive {
            label,
            body: body.into(),
        })
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
            Term::Match(Match::Bln(BlnMatch {
                head: head.into(),
                motive,
                false_case: false_case.into(),
                true_case: true_case.into(),
            }))
        })
}

fn parse_nat_fold_match<'a>() -> Parser<'a, Term> {
    catch(parse_match_prefix())
        .and(
            catch(
                parse_literal("|")
                    .and_keep(parse_nat_literal())
                    .flat_map(|n| match n {
                        0 => pure(()),
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
                Term::Match(Match::Nat(NatMatch::Induction {
                    head: head.into(),
                    motive,
                    zero_case: zero_case.into(),
                    pred_label: pred_label.to_string(),
                    ih_label: ih_label.to_string(),
                    succ_case: succ_case.into(),
                }))
            },
        )
}

fn parse_nat_case<'a>() -> Parser<'a, (u32, Term)> {
    catch(parse_literal("|").and_keep(parse_nat_literal()))
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
            Term::Match(Match::Nat(NatMatch::Dispatch {
                head: head.into(),
                motive,
                cases: cases
                    .into_iter()
                    .map(|(nat, term)| (nat, term.into()))
                    .collect(),
                default: default.into(),
            }))
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
            Term::Match(Match::Atom(AtomMatch {
                head: head.into(),
                motive,
                cases: cases
                    .into_iter()
                    .map(|(atom, term)| (atom, term.into()))
                    .collect(),
            }))
        })
}

fn parse_match<'a>() -> Parser<'a, Term> {
    catch(parse_bln_match())
        .or(catch(parse_nat_fold_match()))
        .or(catch(parse_nat_match()))
        .or(parse_atom_match())
}

fn parse_binding<'a>() -> Parser<'a, RecItem> {
    parse_identifier()
        .and(parse_let_signature())
        .map(|(label, (type_, value))| RecItem {
            label: label.to_string(),
            type_: type_.into(),
            value: value.into(),
        })
}

fn parse_rec<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("rec"))
        .and_keep(sep_by1(parse_binding, || parse_keyword("and")))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(items, tail)| {
            Term::Rec(Rec {
                items,
                tail: tail.into(),
            })
        })
}

fn parse_func_sugar_param<'a>() -> Parser<'a, (String, Term)> {
    parse_identifier()
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .map(|(name, ty): (&str, Term)| (name.to_string(), ty))
}

// Parses the part of a `let` binding after its name, yielding `(type, body)`.
// Supports the function-definition sugar `(p : T, ...) -> R = body`, which
// desugars to type `(p : T, ...) -> R` and body `(p, ...) => body`.
fn parse_let_signature<'a>() -> Parser<'a, (Term, Term)> {
    catch(
        parse_literal("(")
            .and_keep(sep_by0(parse_func_sugar_param, || parse_literal(",")))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal("="))
    .and(lazy(parse_term))
    .map(
        |((params, output), body): ((Vec<(String, Term)>, Term), Term)| {
            let type_ = Term::FuncType(FuncType {
                params: params
                    .iter()
                    .map(|(name, ty)| (Some(name.clone()), ty.clone().into()))
                    .collect(),
                output: output.into(),
            });
            let value = Term::Func(Func {
                params: params.into_iter().map(|(name, _)| name).collect(),
                body: body.into(),
            });
            (type_, value)
        },
    )
    .or(catch(parse_literal(":"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term)))
}

fn parse_let<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let"))
        .and_keep(parse_identifier())
        .and(parse_let_signature())
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|((label, (type_, body)), tail)| {
            Term::Let(Let {
                label: label.to_string(),
                type_: type_.into(),
                body: body.into(),
                tail: tail.into(),
            })
        })
}

fn parse_proj_suffix<'a>() -> Parser<'a, usize> {
    catch(
        take_exact(".").and_keep(
            parse_u32()
                .map_err("Expected numeric index after '.'")
                .map(|n| n as usize),
        ),
    )
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
        .map(|_| Term::Tuple(Tuple { fields: vec![] }))
}

fn with_span<'a>(parser: Parser<'a, Term>) -> Parser<'a, Term> {
    spanned(parser).map(|(span, term)| Term::Spanned(span, term.into()))
}

fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    with_span(
        parse_qualified_name()
            .map(Term::Name)
            .or(parse_type())
            .or(parse_prim())
            .or(parse_atom_type())
            .or(parse_atom())
            .or(parse_tuple_type())
            .or(parse_empty_tuple())
            .or(parse_tuple())
            .or(parse_parens())
            .or(parse_name().map(Term::Name))
            .and(many0(parse_suffix))
            .map(|(head, suffixes)| {
                suffixes
                    .into_iter()
                    .fold(head, |head, suffix| match suffix {
                        Suffix::Proj(index) => Term::Proj(Proj {
                            head: head.into(),
                            index,
                        }),
                        Suffix::Apply(params) => Term::Apply(Apply {
                            head: head.into(),
                            params: params.into_iter().map(Into::into).collect(),
                        }),
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
            .or(parse_atomic_term()),
    )
}

impl FromStr for Term {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        run_parser(
            parse_whitespace()
                .and_keep(parse_term())
                .and_drop(take_eof()),
            input,
        )
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
            .map(move |(label, (type_, body))| {
                TopItem::Let(TopLet {
                    is_pub,
                    label: label.to_string(),
                    type_: type_.into(),
                    body: body.into(),
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
                type_: item.type_,
                body: item.value,
            })
        })
        .and(many0(|| {
            catch(parse_pub().and(parse_keyword("and"))).flat_map(|(is_pub, ())| {
                parse_binding().map(move |item| TopLet {
                    is_pub,
                    label: item.label,
                    type_: item.type_,
                    body: item.value,
                })
            })
        }))
        .and_drop(parse_literal(";"))
        .map(|(first, rest)| iter::once(first).chain(rest).collect())
        .map(TopItem::Rec)
}

fn parse_top_mod<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("mod"))).flat_map(|(is_pub, ())| {
        parse_identifier().flat_map(move |name| {
            catch(
                many0(parse_top_item)
                    .and_drop(parse_keyword("end"))
                    .map(|items| Some(Module { items })),
            )
            .or(parse_literal(";").map(|()| None))
            .map(move |module| {
                TopItem::Mod(TopMod {
                    is_pub,
                    label: name.to_string(),
                    module,
                })
            })
        })
    })
}

fn parse_brace_group<'a>() -> Parser<'a, Vec<String>> {
    catch(parse_literal("{"))
        .and_keep(sep_by1(
            || parse_identifier().map(|s| s.to_string()),
            || parse_literal(","),
        ))
        .and_drop(parse_literal("}"))
}

fn parse_top_use<'a>() -> Parser<'a, TopItem> {
    catch(parse_pub().and(parse_keyword("use"))).flat_map(|(is_pub, ())| {
        catch(take_exact("/"))
            .map(|()| true)
            .or(pure(false))
            .and(parse_name())
            .and(
                catch(take_exact("/").and_keep(parse_brace_group()))
                    .map(Some)
                    .or(pure(None)),
            )
            .and_drop(parse_literal(";"))
            .map(move |((is_abs, name), group)| {
                TopItem::Use(TopUse {
                    is_pub,
                    is_abs,
                    name,
                    group,
                })
            })
    })
}

fn parse_top_item<'a>() -> Parser<'a, TopItem> {
    parse_top_mod()
        .or(parse_top_use())
        .or(parse_top_let())
        .or(parse_top_rec())
}

impl FromStr for Module {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and_drop(take_eof())
                .map(|items| Module { items }),
            input,
        )
    }
}

impl FromStr for Entrypoint {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and(lazy(parse_term))
                .and_drop(take_eof())
                .map(|(items, tail)| Entrypoint { items, tail }),
            input,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_rec_func_and_apply() {
        assert_eq!(
            "rec id : (x : Type) -> Type = x => x; id(a)"
                .parse::<Term>()
                .unwrap(),
            Term::Rec(Rec {
                items: vec![RecItem {
                    label: "id".to_string(),
                    type_: Term::FuncType(FuncType {
                        params: vec![(Some("x".to_string()), Term::Type.into())],
                        output: Term::Type.into(),
                    })
                    .into(),
                    value: Term::Func(Func {
                        params: vec!["x".to_string()],
                        body: Term::Name(Name::from(["x".to_string()])).into(),
                    })
                    .into(),
                }],
                tail: Term::Apply(Apply {
                    head: Term::Name(Name::from(["id".to_string()])).into(),
                    params: vec![Term::Name(Name::from(["a".to_string()])).into()],
                })
                .into(),
            })
        );
    }

    #[test]
    fn parse_let_tuple_and_atoms() {
        assert_eq!(
            "let x : '[hot, cold] = 'hot; (x, 'cold)"
                .parse::<Term>()
                .unwrap(),
            Term::Let(Let {
                label: "x".to_string(),
                type_: Term::AtomType(AtomType {
                    atoms: ["cold", "hot"].into_iter().map(Atom::from).collect(),
                })
                .into(),
                body: Term::Atom(Atom::from("hot")).into(),
                tail: Term::Tuple(Tuple {
                    fields: vec![
                        Term::Name(Name::from(["x".to_string()])).into(),
                        Term::Atom(Atom::from("cold")).into(),
                    ],
                })
                .into(),
            })
        );
    }

    #[test]
    fn parse_match_single_branch() {
        assert_eq!(
            "match 'foo : k => '[foo] | 'foo => 'foo end"
                .parse::<Term>()
                .unwrap(),
            Term::Match(Match::Atom(AtomMatch {
                head: Term::Atom(Atom::from("foo")).into(),
                motive: Motive {
                    label: Some("k".to_string()),
                    body: Term::AtomType(AtomType {
                        atoms: [Atom::from("foo")].into_iter().collect(),
                    })
                    .into(),
                },
                cases: [(Atom::from("foo"), Term::Atom(Atom::from("foo")).into())]
                    .into_iter()
                    .collect(),
            }))
        );
    }

    #[test]
    fn parse_int_literal_and_flt_literal_are_disambiguated() {
        assert_eq!("+42".parse::<Term>().unwrap(), Term::Prim(Prim::Int(42)));
        assert_eq!(
            "42".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Succ(
                NatLiteral::Number(42),
                Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
            )))
        );
        assert_eq!(
            "+42.0".parse::<Term>().unwrap(),
            Term::Prim(Prim::Flt(42.0_f32))
        );
    }

    #[test]
    fn parse_prim() {
        assert_eq!("Int".parse::<Term>().unwrap(), Term::Prim(Prim::IntType));
        assert_eq!("Flt".parse::<Term>().unwrap(), Term::Prim(Prim::FltType));
        assert_eq!("Nat".parse::<Term>().unwrap(), Term::Prim(Prim::NatType));
        assert_eq!("+42".parse::<Term>().unwrap(), Term::Prim(Prim::Int(42)));
        assert_eq!(
            "42".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Succ(
                NatLiteral::Number(42),
                Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
            )))
        );
        assert_eq!(
            "+1.5".parse::<Term>().unwrap(),
            Term::Prim(Prim::Flt(1.5_f32))
        );
        assert_eq!(
            "Int.add(+1, +2)".parse::<Term>().unwrap(),
            Term::Prim(Prim::IntAdd(
                Term::Prim(Prim::Int(1)).into(),
                Term::Prim(Prim::Int(2)).into(),
            ))
        );
        assert_eq!(
            "Nat.add(1, 2)".parse::<Term>().unwrap(),
            Term::Prim(Prim::NatAdd(
                Term::Prim(Prim::Nat(Nat::Succ(
                    NatLiteral::Number(1),
                    Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
                )))
                .into(),
                Term::Prim(Prim::Nat(Nat::Succ(
                    NatLiteral::Number(2),
                    Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
                )))
                .into(),
            ))
        );
        assert_eq!(
            "Flt.mul(+1.5, +2.0)".parse::<Term>().unwrap(),
            Term::Prim(Prim::FltMul(
                Term::Prim(Prim::Flt(1.5_f32)).into(),
                Term::Prim(Prim::Flt(2.0_f32)).into(),
            ))
        );
    }

    #[test]
    fn parse_char_literal_ascii() {
        assert_eq!(
            "'a'".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Succ(
                NatLiteral::Char('a'),
                Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
            )))
        );
    }

    #[test]
    fn parse_char_literal_escape() {
        assert_eq!(
            "'\\n'".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Succ(
                NatLiteral::Char('\n'),
                Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
            )))
        );
    }

    #[test]
    fn parse_char_literal_no_suffix_is_bin() {
        assert_eq!(
            "\"a\"".parse::<Term>().unwrap(),
            Term::Prim(Prim::Bin(BinLiteral::String("a".to_string())))
        );
    }

    #[test]
    fn parse_char_literal_multi_char_is_error() {
        assert!("'ab'".parse::<Term>().is_err());
    }

    #[test]
    fn parse_char_literal_empty_is_error() {
        assert!("''".parse::<Term>().is_err());
    }

    #[test]
    fn parse_top_let_without_pub() {
        assert_eq!(
            "let x : Type = Type;".parse::<Module>().unwrap().items,
            vec![TopItem::Let(TopLet {
                is_pub: false,
                label: "x".to_string(),
                type_: Term::Type.into(),
                body: Term::Type.into(),
            })]
        );
    }

    #[test]
    fn parse_top_let_with_pub() {
        assert_eq!(
            "pub let x : Type = Type;".parse::<Module>().unwrap().items,
            vec![TopItem::Let(TopLet {
                is_pub: true,
                label: "x".to_string(),
                type_: Term::Type.into(),
                body: Term::Type.into(),
            })]
        );
    }

    #[test]
    fn parse_top_rec_mixed_pub() {
        assert_eq!(
            r#"
                pub rec id : (x : Type) -> Type = x => x
                and helper : Type = Type;
            "#
            .parse::<Module>()
            .unwrap()
            .items,
            vec![TopItem::Rec(vec![
                TopLet {
                    is_pub: true,
                    label: "id".to_string(),
                    type_: Term::FuncType(FuncType {
                        params: vec![(Some("x".to_string()), Term::Type.into())],
                        output: Term::Type.into(),
                    })
                    .into(),
                    body: Term::Func(Func {
                        params: vec!["x".to_string()],
                        body: Term::Name(Name::from(["x".to_string()])).into(),
                    })
                    .into(),
                },
                TopLet {
                    is_pub: false,
                    label: "helper".to_string(),
                    type_: Term::Type.into(),
                    body: Term::Type.into(),
                },
            ])]
        );
    }

    #[test]
    fn parse_module_roundtrip() {
        let m = r#"
            use Bar;
            pub let x : Type = Type;
            rec f : Type = Type;
        "#
        .parse::<Module>()
        .unwrap();
        assert_eq!(m.items.len(), 3);
        assert!(matches!(m.items[0], TopItem::Use(_)));
        assert!(matches!(
            m.items[1],
            TopItem::Let(TopLet { is_pub: true, .. })
        ));
        assert!(matches!(m.items[2], TopItem::Rec(_)));
    }

    #[test]
    fn parse_nested_module() {
        let m = r#"
            mod Inner
                pub let x : Type = Type;
            end
        "#
        .parse::<Module>()
        .unwrap();
        assert_eq!(
            m.items,
            vec![TopItem::Mod(TopMod {
                is_pub: false,
                label: "Inner".to_string(),
                module: Some(Module {
                    items: vec![TopItem::Let(TopLet {
                        is_pub: true,
                        label: "x".to_string(),
                        type_: Term::Type.into(),
                        body: Term::Type.into(),
                    })],
                }),
            })]
        );
    }

    #[test]
    fn parse_entrypoint_roundtrip() {
        let entrypoint = r#"
            use Foo;
            use Bar;
            pub rec f : Type = Type;
            let x : Type = Type;
            f
        "#
        .parse::<Entrypoint>()
        .unwrap();
        assert_eq!(entrypoint.items.len(), 4);
        assert!(matches!(entrypoint.items[0], TopItem::Use(_)));
        assert!(matches!(entrypoint.items[1], TopItem::Use(_)));
        assert!(matches!(entrypoint.items[2], TopItem::Rec(_)));
        assert!(matches!(
            entrypoint.items[3],
            TopItem::Let(TopLet { is_pub: false, .. })
        ));
        assert_eq!(entrypoint.tail, Term::Name(Name::from(["f".to_string()])));
    }

    #[test]
    fn parse_qualified_path() {
        assert_eq!(
            "Foo/bar/baz".parse::<Term>().unwrap(),
            Term::Name(Name::from([
                "Foo".to_string(),
                "bar".to_string(),
                "baz".to_string()
            ]))
        );
    }

    #[test]
    fn parse_type_name_as_path_segment() {
        assert_eq!(
            "Nat/double".parse::<Term>().unwrap(),
            Term::Name(Name::from(["Nat".to_string(), "double".to_string()]))
        );
        assert_eq!(
            "Type/foo".parse::<Term>().unwrap(),
            Term::Name(Name::from(["Type".to_string(), "foo".to_string()]))
        );
    }

    #[test]
    fn bare_type_names_still_parse_as_prims() {
        assert_eq!("Nat".parse::<Term>().unwrap(), Term::Prim(Prim::NatType));
        assert_eq!("Type".parse::<Term>().unwrap(), Term::Type);
        assert_eq!(
            "Nat.add(1, 2)".parse::<Term>().unwrap(),
            Term::Prim(Prim::NatAdd(
                Term::Prim(Prim::Nat(Nat::Succ(
                    NatLiteral::Number(1),
                    Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
                )))
                .into(),
                Term::Prim(Prim::Nat(Nat::Succ(
                    NatLiteral::Number(2),
                    Box::new(Term::Prim(Prim::Nat(Nat::Zero)))
                )))
                .into(),
            ))
        );
    }

    #[test]
    fn parse_use_of_type_named_module() {
        assert_eq!(
            "use Nat;".parse::<Module>().unwrap().items,
            vec![TopItem::Use(TopUse {
                is_pub: false,
                is_abs: false,
                name: Name::from(["Nat".to_string()]),
                group: None,
            })]
        );
        assert_eq!(
            "use Foo/Int;".parse::<Module>().unwrap().items,
            vec![TopItem::Use(TopUse {
                is_pub: false,
                is_abs: false,
                name: Name::from(["Foo".to_string(), "Int".to_string()]),
                group: None,
            })]
        );
    }

    #[test]
    fn parse_use_brace_group() {
        assert_eq!(
            "use /std/{Bin, Arr};".parse::<Module>().unwrap().items,
            vec![TopItem::Use(TopUse {
                is_pub: false,
                is_abs: true,
                name: Name::from(["std".to_string()]),
                group: Some(vec!["Bin".to_string(), "Arr".to_string()]),
            })]
        );
    }

    #[test]
    fn parse_proj_numeric_suffix() {
        assert_eq!(
            "(r).0".parse::<Term>().unwrap(),
            Term::Proj(Proj {
                head: Term::Name(Name::from(["r".to_string()])).into(),
                index: 0,
            })
        );
    }

    #[test]
    fn parse_proj_chained_suffixes() {
        assert_eq!(
            "(r).1.0".parse::<Term>().unwrap(),
            Term::Proj(Proj {
                head: Term::Proj(Proj {
                    head: Term::Name(Name::from(["r".to_string()])).into(),
                    index: 1,
                })
                .into(),
                index: 0,
            })
        );
    }

    #[test]
    fn parse_proj_on_name_directly() {
        assert_eq!(
            "r.2".parse::<Term>().unwrap(),
            Term::Proj(Proj {
                head: Term::Name(Name::from(["r".to_string()])).into(),
                index: 2,
            })
        );
    }

    #[test]
    fn parse_empty_tuple_type() {
        assert_eq!(
            "{}".parse::<Term>().unwrap(),
            Term::TupleType(TupleType { fields: vec![] })
        );
    }

    #[test]
    fn parse_empty_tuple() {
        assert_eq!(
            "()".parse::<Term>().unwrap(),
            Term::Tuple(Tuple { fields: vec![] })
        );
    }
}
