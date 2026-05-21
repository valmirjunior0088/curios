use {
    super::{
        Apply, Atom, AtomType, Bin, Entrypoint, Func, FuncType, Let, Match, Module, Name, Nat,
        NatFold, NatMatch, Prim, Rec, RecItem, Split, Term, TopItem, TopLet, TopMod, TopUse, Tuple,
        TupleType,
    },
    crate::parser::{
        Parser, ParserError, catch, fail, lazy, many0, many1, pure, run_parser, sep_by0, sep_by1,
        take_eof, take_exact, take_while,
    },
    std::{iter, str::FromStr},
};

const KEYWORDS: &[&str] = &[
    "let", "match", "rec", "and", "split", "->", "=>", "mod", "use", "pub", "end",
];

fn parse_whitespace<'a>() -> Parser<'a, &'a str> {
    take_while(|char| char.is_whitespace())
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    take_while(|char| "._-@#$!%&*<>".contains(char) || char.is_alphanumeric())
        .flat_map(|identifier| match identifier.is_empty() {
            true => fail("Expected identifier"),
            false => pure(identifier),
        })
        .and_drop(parse_whitespace())
}

fn parse_name<'a>() -> Parser<'a, Name> {
    parse_identifier()
        .and(many0(|| take_exact("/").and_keep(parse_identifier())))
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
        take_while(|char| char == '-' || char.is_ascii_digit())
            .flat_map::<i32, _>(|digits| match digits.parse() {
                Ok(value) => pure(value),
                Err(_) => fail("Expected integer literal"),
            })
            .and_drop(parse_keyword("i")),
    )
    .map(|value| Term::Prim(Prim::Int(value)))
}

fn parse_nat<'a>() -> Parser<'a, Nat> {
    catch(
        take_exact("\"")
            .and_keep(many0(parse_string_chunk))
            .and_drop(take_exact("\""))
            .and_drop(parse_whitespace())
            .and_drop(parse_keyword("n")),
    )
    .map(|chunks| chunks.concat())
    .flat_map(|string| match string.chars().count() {
        1 => pure(Nat::Char(string.chars().next().unwrap())),
        _ => fail("char literal requires exactly one Unicode scalar value"),
    })
    .or(catch(
        take_while(|char| char.is_ascii_digit())
            .flat_map::<u32, _>(|digits| match digits.parse() {
                Ok(value) => pure(value),
                Err(_) => fail("expected natural number"),
            })
            .and_drop(parse_keyword("n")),
    )
    .map(Nat::Number))
}

fn parse_nat_value<'a>() -> Parser<'a, Term> {
    parse_nat().map(|nat| Term::Prim(Prim::Nat(nat)))
}

fn parse_nat_literal<'a>() -> Parser<'a, u32> {
    parse_nat().map(|nat| match nat {
        Nat::Number(number) => number,
        Nat::Char(char) => char as u32,
    })
}

fn parse_flt_value<'a>() -> Parser<'a, Term> {
    take_while(|char| ".-+eE".contains(char) || char.is_ascii_digit())
        .flat_map::<f32, _>(|digits| {
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

            match digits.parse() {
                Ok(value) => pure(value),
                Err(_) => fail("Expected float literal"),
            }
        })
        .and_drop(parse_whitespace())
        .map(|value| Term::Prim(Prim::Flt(value)))
}

fn parse_nat_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Nat"))
        .map(|()| Term::Prim(Prim::NatType))
        .or(parse_nat_value())
        .or(catch(parse_keyword("Nat.eql"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_eql(left, right))))
        .or(catch(parse_keyword("Nat.neq"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_neq(left, right))))
        .or(catch(parse_keyword("Nat.add"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_add(left, right))))
        .or(catch(parse_keyword("Nat.sub"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_sub(left, right))))
        .or(catch(parse_keyword("Nat.mul"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_mul(left, right))))
        .or(catch(parse_keyword("Nat.lt"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_lt(left, right))))
        .or(catch(parse_keyword("Nat.div"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_div(left, right))))
        .or(catch(parse_keyword("Nat.rem"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_rem(left, right))))
        .or(catch(parse_keyword("Nat.gt"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_gt(left, right))))
        .or(catch(parse_keyword("Nat.lte"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_lte(left, right))))
        .or(catch(parse_keyword("Nat.gte"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_gte(left, right))))
        .or(catch(parse_keyword("Nat.to_int"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::nat_to_int(inner))))
        .or(catch(parse_keyword("Nat.to_flt"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::nat_to_flt(inner))))
}

fn parse_int_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Int"))
        .map(|()| Term::Prim(Prim::IntType))
        .or(parse_int_value())
        .or(catch(parse_keyword("Int.eql"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_eql(left, right))))
        .or(catch(parse_keyword("Int.neq"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_neq(left, right))))
        .or(catch(parse_keyword("Int.add"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_add(left, right))))
        .or(catch(parse_keyword("Int.sub"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_sub(left, right))))
        .or(catch(parse_keyword("Int.mul"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_mul(left, right))))
        .or(catch(parse_keyword("Int.div"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_div(left, right))))
        .or(catch(parse_keyword("Int.rem"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_rem(left, right))))
        .or(catch(parse_keyword("Int.lt"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_lt(left, right))))
        .or(catch(parse_keyword("Int.gt"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_gt(left, right))))
        .or(catch(parse_keyword("Int.lte"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_lte(left, right))))
        .or(catch(parse_keyword("Int.gte"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::int_gte(left, right))))
        .or(catch(parse_keyword("Int.to_nat"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::int_to_nat(inner))))
        .or(catch(parse_keyword("Int.to_flt"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::int_to_flt(inner))))
}

fn parse_flt_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Flt"))
        .map(|()| Term::Prim(Prim::FltType))
        .or(parse_flt_value())
        .or(catch(parse_keyword("Flt.add"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_add(left, right))))
        .or(catch(parse_keyword("Flt.sub"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_sub(left, right))))
        .or(catch(parse_keyword("Flt.mul"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_mul(left, right))))
        .or(catch(parse_keyword("Flt.div"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_div(left, right))))
        .or(catch(parse_keyword("Flt.eql"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_eql(left, right))))
        .or(catch(parse_keyword("Flt.neq"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_neq(left, right))))
        .or(catch(parse_keyword("Flt.lt"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_lt(left, right))))
        .or(catch(parse_keyword("Flt.gt"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_gt(left, right))))
        .or(catch(parse_keyword("Flt.lte"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_lte(left, right))))
        .or(catch(parse_keyword("Flt.gte"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_gte(left, right))))
        .or(catch(parse_keyword("Flt.min"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_min(left, right))))
        .or(catch(parse_keyword("Flt.max"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_max(left, right))))
        .or(catch(parse_keyword("Flt.neg"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_neg(inner))))
        .or(catch(parse_keyword("Flt.abs"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_abs(inner))))
        .or(catch(parse_keyword("Flt.sqrt"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_sqrt(inner))))
        .or(catch(parse_keyword("Flt.floor"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_floor(inner))))
        .or(catch(parse_keyword("Flt.ceil"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_ceil(inner))))
        .or(catch(parse_keyword("Flt.trunc"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_trunc(inner))))
        .or(catch(parse_keyword("Flt.nearest"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_nearest(inner))))
        .or(catch(parse_keyword("Flt.to_nat"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_to_nat(inner))))
        .or(catch(parse_keyword("Flt.to_int"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_to_int(inner))))
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
            .or(take_exact("t").map(|_| "\t".to_string()))
            .or(take_exact("r").map(|_| "\r".to_string()))
            .or(take_exact("\\").map(|_| "\\".to_string()))
            .or(take_exact("\"").map(|_| "\"".to_string()))
            .or(fail("Unknown string escape sequence")),
    ))
}

fn parse_string_literal<'a>() -> Parser<'a, Term> {
    catch(take_exact("\""))
        .and_keep(many0(parse_string_chunk))
        .and_drop(take_exact("\""))
        .and_drop(parse_whitespace())
        .map(|chunks| Term::Prim(Prim::Bin(Bin::String(chunks.concat()))))
}

fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    catch(many1(parse_hex_byte).and_drop(parse_whitespace()))
        .map(|bytes| Term::Prim(Prim::Bin(Bin::Bytes(bytes))))
}

fn parse_bin_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Bin.len"))
        .and_keep(lazy(parse_atomic_term))
        .map(|bin| Term::Prim(Prim::bin_len(bin)))
        .or(catch(parse_keyword("Bin.eql"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::bin_eql(left, right))))
        .or(catch(parse_keyword("Bin.get"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(bin, index)| Term::Prim(Prim::bin_get(bin, index))))
        .or(catch(parse_keyword("Bin.slice"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|((bin, start), end)| Term::Prim(Prim::bin_slice(bin, start, end))))
        .or(catch(parse_keyword("Bin.append"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(bin, byte)| Term::Prim(Prim::bin_append(bin, byte))))
        .or(catch(parse_keyword("Bin.concat"))
            .and_keep(sep_by0(|| lazy(parse_atomic_term), || parse_literal(",")))
            .map(|ops| Term::Prim(Prim::bin_concat(ops))))
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
    catch(parse_keyword("Arr.len"))
        .and_keep(lazy(parse_atomic_term))
        .map(|list| Term::Prim(Prim::arr_len(list)))
        .or(catch(parse_keyword("Arr.get"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(list, index)| Term::Prim(Prim::arr_get(list, index))))
        .or(catch(parse_keyword("Arr.slice"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|((list, start), end)| Term::Prim(Prim::arr_slice(list, start, end))))
        .or(catch(parse_keyword("Arr.append"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(list, elem)| Term::Prim(Prim::arr_append(list, elem))))
        .or(catch(parse_keyword("Arr.concat"))
            .and_keep(sep_by0(|| lazy(parse_atomic_term), || parse_literal(",")))
            .map(|ops| Term::Prim(Prim::arr_concat(ops))))
        .or(catch(parse_keyword("Arr"))
            .and_keep(lazy(parse_atomic_term))
            .map(|elem| Term::Prim(Prim::arr_type(elem))))
        .or(parse_arr_literal())
}

fn parse_prim<'a>() -> Parser<'a, Term> {
    parse_flt_prim()
        .or(parse_int_prim())
        .or(parse_nat_prim())
        .or(parse_bin_prim())
        .or(parse_arr_prim())
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
        .and_keep(sep_by1(parse_tuple_type_field, || parse_literal(",")))
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
            fields: iter::once(first).chain(rest).map(Into::into).collect(),
        })
    })
}

fn parse_dependent_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(")"))
    .and_drop(parse_keyword("->"))
    .map(|(label, input): (&str, Term)| (Some(label.to_string()), input))
    .and(lazy(parse_term))
    .map(|((label, input), output)| {
        Term::FuncType(FuncType {
            label,
            input: input.into(),
            output: output.into(),
        })
    })
}

fn parse_non_dependent_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_atomic_term()
            .and(many0(parse_atomic_term))
            .map(|(head, params)| Apply::many(head, params))
            .and_drop(parse_keyword("->")),
    )
    .and(lazy(parse_term))
    .map(|(input, output)| {
        Term::FuncType(FuncType {
            label: None,
            input: input.into(),
            output: output.into(),
        })
    })
}

fn parse_func_type<'a>() -> Parser<'a, Term> {
    parse_dependent_func_type().or(parse_non_dependent_func_type())
}

fn parse_func<'a>() -> Parser<'a, Term> {
    catch(parse_identifier().and_drop(parse_literal("=>")))
        .and(lazy(parse_term))
        .map(|(label, body)| {
            Term::Func(Func {
                label: label.to_string(),
                body: body.into(),
            })
        })
}

fn parse_nat_fold<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Nat.fold"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(":"))
        .and(parse_identifier())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .and_drop(parse_literal("|"))
        .and_drop(parse_keyword("0n"))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .and_drop(parse_literal("|"))
        .and(parse_identifier())
        .and(parse_identifier())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .map(
            |((((((head, motive_label), motive), zero_case), pred_label), ih_label), succ_case)| {
                Term::NatFold(NatFold {
                    head: head.into(),
                    motive_label: motive_label.to_string(),
                    motive: motive.into(),
                    zero_case: zero_case.into(),
                    pred_label: pred_label.to_string(),
                    ih_label: ih_label.to_string(),
                    succ_case: succ_case.into(),
                })
            },
        )
}

fn parse_nat_match_case<'a>() -> Parser<'a, (u32, Term)> {
    catch(parse_literal("|").and_keep(parse_nat_literal()))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
}

fn parse_nat_match_default<'a>() -> Parser<'a, Term> {
    catch(parse_literal("|").and_keep(parse_literal("_")))
        .and_drop(parse_literal("=>"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(";"))
}

fn parse_nat_match<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Nat.match"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(":"))
        .and(parse_identifier())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .and(many0(parse_nat_match_case))
        .and(parse_nat_match_default())
        .map(|((((head, motive_label), motive), cases), default)| {
            Term::NatMatch(NatMatch {
                head: head.into(),
                motive_label: motive_label.to_string(),
                motive: motive.into(),
                cases: cases
                    .into_iter()
                    .map(|(nat, term)| (nat, term.into()))
                    .collect(),
                default: default.into(),
            })
        })
}

fn parse_match_branch<'a>() -> Parser<'a, (Atom, Term)> {
    catch(parse_literal("|").and_keep(parse_atom_label()))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
}

fn parse_match<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("match"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(":"))
        .and(parse_identifier())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .and(many1(parse_match_branch))
        .map(|(((head, motive_label), motive), cases)| {
            Term::Match(Match {
                head: head.into(),
                motive_label: motive_label.to_string(),
                motive: motive.into(),
                cases: cases
                    .into_iter()
                    .map(|(atom, term)| (atom, term.into()))
                    .collect(),
            })
        })
}

fn parse_binding<'a>() -> Parser<'a, RecItem> {
    parse_identifier()
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|((label, type_), value)| RecItem {
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

fn parse_split<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("split"))
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(":"))
        .and(parse_identifier())
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .and_drop(parse_literal("|"))
        .and_drop(parse_literal("("))
        .and(sep_by1(|| parse_identifier(), || parse_literal(",")))
        .and_drop(parse_literal(")"))
        .and_drop(parse_literal("=>"))
        .and(lazy(parse_term))
        .map(|((((head, motive_label), motive), field_labels), tail)| {
            Term::Split(Split {
                head: head.into(),
                motive_label: motive_label.to_string(),
                motive: motive.into(),
                field_labels: field_labels
                    .into_iter()
                    .map(|label| label.to_string())
                    .collect(),
                tail: tail.into(),
            })
        })
}

fn parse_let<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let"))
        .and_keep(parse_identifier())
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(((label, type_), body), tail)| {
            Term::Let(Let {
                label: label.to_string(),
                type_: type_.into(),
                body: body.into(),
                tail: tail.into(),
            })
        })
}

fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    parse_type()
        .or(parse_prim())
        .or(parse_atom_type())
        .or(parse_atom())
        .or(parse_tuple_type())
        .or(parse_tuple())
        .or(parse_parens())
        .or(parse_name().map(Term::Name))
}

fn parse_term<'a>() -> Parser<'a, Term> {
    parse_rec()
        .or(parse_split())
        .or(parse_let())
        .or(parse_nat_fold())
        .or(parse_nat_match())
        .or(parse_match())
        .or(parse_func_type())
        .or(parse_func())
        .or(parse_atomic_term()
            .and(many0(parse_atomic_term))
            .map(|(head, params)| Apply::many(head, params)))
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
            .and_drop(parse_literal(":"))
            .and(lazy(parse_term))
            .and_drop(parse_literal("="))
            .and(lazy(parse_term))
            .and_drop(parse_literal(";"))
            .map(move |((label, type_), body)| {
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
            many0(parse_top_item)
                .and_drop(parse_keyword("end"))
                .map(move |items| {
                    TopItem::Mod(TopMod {
                        is_pub,
                        label: name.to_string(),
                        module: Module { items },
                    })
                })
        })
    })
}

fn parse_top_use<'a>() -> Parser<'a, TopItem> {
    catch(parse_keyword("use"))
        .and_keep(catch(take_exact("/")).map(|()| true).or(pure(false)))
        .and(parse_name())
        .map(|(is_abs, name)| TopItem::Use(TopUse { is_abs, name }))
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
            "rec id : (x : Type) -> Type = x => x; id a"
                .parse::<Term>()
                .unwrap(),
            Term::Rec(Rec {
                items: vec![RecItem {
                    label: "id".to_string(),
                    type_: Term::FuncType(FuncType {
                        label: Some("x".to_string()),
                        input: Term::Type.into(),
                        output: Term::Type.into(),
                    })
                    .into(),
                    value: Term::Func(Func {
                        label: "x".to_string(),
                        body: Term::Name(Name::from(["x".to_string()])).into(),
                    })
                    .into(),
                }],
                tail: Term::Apply(Apply {
                    head: Term::Name(Name::from(["id".to_string()])).into(),
                    param: Term::Name(Name::from(["a".to_string()])).into(),
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
    fn parse_split_with_motive() {
        assert_eq!(
            "split ('left, 'right) : p => Type; | (x, y) => p"
                .parse::<Term>()
                .unwrap(),
            Term::Split(Split {
                head: Term::Tuple(Tuple {
                    fields: vec![
                        Term::Atom(Atom::from("left")).into(),
                        Term::Atom(Atom::from("right")).into(),
                    ],
                })
                .into(),
                motive_label: "p".to_string(),
                motive: Term::Type.into(),
                field_labels: vec!["x".to_string(), "y".to_string()],
                tail: Term::Name(Name::from(["p".to_string()])).into(),
            })
        );
    }

    #[test]
    fn parse_match_single_branch() {
        assert_eq!(
            "match 'foo : k => '[foo]; | 'foo => 'foo;"
                .parse::<Term>()
                .unwrap(),
            Term::Match(Match {
                head: Term::Atom(Atom::from("foo")).into(),
                motive_label: "k".to_string(),
                motive: Term::AtomType(AtomType {
                    atoms: [Atom::from("foo")].into_iter().collect(),
                })
                .into(),
                cases: [(Atom::from("foo"), Term::Atom(Atom::from("foo")).into())]
                    .into_iter()
                    .collect(),
            })
        );
    }

    #[test]
    fn parse_int_literal_and_flt_literal_are_disambiguated() {
        assert_eq!("42i".parse::<Term>().unwrap(), Term::Prim(Prim::Int(42)));
        assert_eq!(
            "42n".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Number(42)))
        );
        assert_eq!(
            "42.0".parse::<Term>().unwrap(),
            Term::Prim(Prim::Flt(42.0_f32))
        );
    }

    #[test]
    fn parse_prim() {
        assert_eq!("Int".parse::<Term>().unwrap(), Term::Prim(Prim::IntType));
        assert_eq!("Flt".parse::<Term>().unwrap(), Term::Prim(Prim::FltType));
        assert_eq!("Nat".parse::<Term>().unwrap(), Term::Prim(Prim::NatType));
        assert_eq!("42i".parse::<Term>().unwrap(), Term::Prim(Prim::Int(42)));
        assert_eq!(
            "42n".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Number(42)))
        );
        assert_eq!(
            "1.5".parse::<Term>().unwrap(),
            Term::Prim(Prim::Flt(1.5_f32))
        );
        assert_eq!(
            "Int.add 1i 2i".parse::<Term>().unwrap(),
            Term::Prim(Prim::IntAdd(
                Term::Prim(Prim::Int(1)).into(),
                Term::Prim(Prim::Int(2)).into(),
            ))
        );
        assert_eq!(
            "Nat.add 1n 2n".parse::<Term>().unwrap(),
            Term::Prim(Prim::NatAdd(
                Term::Prim(Prim::Nat(Nat::Number(1))).into(),
                Term::Prim(Prim::Nat(Nat::Number(2))).into(),
            ))
        );
        assert_eq!(
            "Flt.mul 1.5 2.0".parse::<Term>().unwrap(),
            Term::Prim(Prim::FltMul(
                Term::Prim(Prim::Flt(1.5_f32)).into(),
                Term::Prim(Prim::Flt(2.0_f32)).into(),
            ))
        );
    }

    #[test]
    fn parse_char_literal_ascii() {
        assert_eq!(
            "\"a\"n".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Char('a')))
        );
    }

    #[test]
    fn parse_char_literal_escape() {
        assert_eq!(
            "\"\\n\"n".parse::<Term>().unwrap(),
            Term::Prim(Prim::Nat(Nat::Char('\n')))
        );
    }

    #[test]
    fn parse_char_literal_no_suffix_is_bin() {
        assert_eq!(
            "\"a\"".parse::<Term>().unwrap(),
            Term::Prim(Prim::Bin(Bin::String("a".to_string())))
        );
    }

    #[test]
    fn parse_char_literal_multi_char_is_error() {
        assert!("\"ab\"n".parse::<Term>().is_err());
    }

    #[test]
    fn parse_char_literal_empty_is_error() {
        assert!("\"\"n".parse::<Term>().is_err());
    }

    #[test]
    fn parse_top_let_without_pub() {
        assert_eq!(
            "let x : Type = Type;\n".parse::<Module>().unwrap().items,
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
            "pub let x : Type = Type;\n"
                .parse::<Module>()
                .unwrap()
                .items,
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
            "pub rec id : (x : Type) -> Type = x => x\nand helper : Type = Type;\n"
                .parse::<Module>()
                .unwrap()
                .items,
            vec![TopItem::Rec(vec![
                TopLet {
                    is_pub: true,
                    label: "id".to_string(),
                    type_: Term::FuncType(FuncType {
                        label: Some("x".to_string()),
                        input: Term::Type.into(),
                        output: Term::Type.into(),
                    })
                    .into(),
                    body: Term::Func(Func {
                        label: "x".to_string(),
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
        let m = "use Bar\npub let x : Type = Type;\nrec f : Type = Type;\n"
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
        let m = "mod Inner\npub let x : Type = Type;\nend\n"
            .parse::<Module>()
            .unwrap();
        assert_eq!(
            m.items,
            vec![TopItem::Mod(TopMod {
                is_pub: false,
                label: "Inner".to_string(),
                module: Module {
                    items: vec![TopItem::Let(TopLet {
                        is_pub: true,
                        label: "x".to_string(),
                        type_: Term::Type.into(),
                        body: Term::Type.into(),
                    })],
                },
            })]
        );
    }

    #[test]
    fn parse_entrypoint_roundtrip() {
        let entrypoint = "use Foo\nuse Bar\npub rec f : Type = Type;\nlet x : Type = Type;\nf"
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
}
