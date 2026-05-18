use {
    super::{
        Apply, Atom, AtomType, Func, FuncType, Let, LetRec, Match, NatMatch, Prim, Split, Term,
        Tuple, TupleType, Type, Var,
    },
    crate::parser::{
        Parser, ParserError, catch, fail, lazy, many0, many1, pure, run_parser, sep_by0, sep_by1,
        take_eof, take_exact, take_while,
    },
    std::str::FromStr,
};

fn parse_whitespace<'a>() -> Parser<'a, &'a str> {
    take_while(|char| char.is_whitespace())
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    take_while(|char| "._-@#$!%&*".contains(char) || char.is_alphanumeric())
        .flat_map(|identifier| match identifier.is_empty() {
            true => fail("Expected identifier"),
            false => pure(identifier),
        })
        .and_drop(parse_whitespace())
}

const KEYWORDS: &[&str] = &["let", "match"];

fn parse_label<'a>() -> Parser<'a, Term> {
    parse_identifier().flat_map(|identifier| match KEYWORDS.contains(&identifier) {
        true => fail(format!("'{identifier}' is a reserved keyword")),
        false => pure(Var::free(identifier).into()),
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
    catch(parse_keyword("Type")).map(|()| Type.into())
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

fn parse_nat_value<'a>() -> Parser<'a, Term> {
    catch(
        take_while(|char| char.is_ascii_digit())
            .flat_map::<u32, _>(|digits| match digits.parse() {
                Ok(value) => pure(value),
                Err(_) => fail("Expected natural literal"),
            })
            .and_drop(parse_keyword("n")),
    )
    .map(|value| Term::Prim(Prim::Nat(value)))
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
        .map(|value: f32| Term::Prim(Prim::Flt(value.to_bits())))
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
        .or(catch(parse_keyword("Nat.div"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_div(left, right))))
        .or(catch(parse_keyword("Nat.rem"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_rem(left, right))))
        .or(catch(parse_keyword("Nat.lt"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::nat_lt(left, right))))
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
        .or(catch(parse_keyword("Flt.div"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_div(left, right))))
        .or(catch(parse_keyword("Flt.min"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_min(left, right))))
        .or(catch(parse_keyword("Flt.max"))
            .and_keep(lazy(parse_atomic_term))
            .and(lazy(parse_atomic_term))
            .map(|(left, right)| Term::Prim(Prim::flt_max(left, right))))
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
}

fn parse_conv_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Nat.to_int"))
        .and_keep(lazy(parse_atomic_term))
        .map(|inner| Term::Prim(Prim::nat_to_int(inner)))
        .or(catch(parse_keyword("Int.to_nat"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::int_to_nat(inner))))
        .or(catch(parse_keyword("Int.to_flt"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::int_to_flt(inner))))
        .or(catch(parse_keyword("Nat.to_flt"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::nat_to_flt(inner))))
        .or(catch(parse_keyword("Flt.to_int"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_to_int(inner))))
        .or(catch(parse_keyword("Flt.to_nat"))
            .and_keep(lazy(parse_atomic_term))
            .map(|inner| Term::Prim(Prim::flt_to_nat(inner))))
}

fn parse_hex_byte<'a>() -> Parser<'a, u8> {
    take_exact("\\").and_keep(
        take_while(|c: char| c.is_ascii_hexdigit()).flat_map(|hex: &str| {
            if hex.len() == 2 {
                pure(u8::from_str_radix(hex, 16).expect("valid hex pair"))
            } else {
                fail("Expected exactly 2 hex digits after \\")
            }
        }),
    )
}

fn parse_string_chunk<'a>() -> Parser<'a, String> {
    catch(take_while(|c| c != '\\' && c != '"').flat_map(|s: &str| {
        if s.is_empty() {
            fail("empty chunk")
        } else {
            pure(s.to_string())
        }
    }))
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
        .map(|chunks: Vec<String>| Term::Prim(Prim::Bin(chunks.concat().into_bytes())))
}

fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    catch(many1(parse_hex_byte).and_drop(parse_whitespace()))
        .map(|bytes| Term::Prim(Prim::Bin(bytes)))
}

fn parse_bin_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Bin.len"))
        .and_keep(lazy(parse_atomic_term))
        .map(|bin| Term::Prim(Prim::bin_len(bin)))
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
            .map(|ops: Vec<Term>| Term::Prim(Prim::bin_concat(ops))))
        .or(catch(parse_keyword("Bin")).map(|()| Term::Prim(Prim::BinType)))
        .or(parse_string_literal())
        .or(parse_bin_literal())
}

fn parse_arr_literal<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("[")
            .and_keep(sep_by0(|| lazy(parse_term), || parse_literal(",")))
            .and_drop(parse_literal("]")),
    )
    .map(|elems: Vec<Term>| Term::Prim(Prim::Arr(elems.into_iter().map(|e| e.into()).collect())))
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
            .map(|ops: Vec<Term>| Term::Prim(Prim::arr_concat(ops))))
        .or(catch(parse_keyword("Arr"))
            .and_keep(lazy(parse_atomic_term))
            .map(|elem| Term::Prim(Prim::arr_type(elem))))
        .or(parse_arr_literal())
}

fn parse_prim<'a>() -> Parser<'a, Term> {
    parse_flt_prim()
        .or(parse_int_prim())
        .or(parse_nat_prim())
        .or(parse_conv_prim())
        .or(parse_bin_prim())
        .or(parse_arr_prim())
}

fn parse_atom_label<'a>() -> Parser<'a, Atom> {
    take_exact("'").and_keep(parse_identifier()).map(Atom::from)
}

fn parse_atom<'a>() -> Parser<'a, Term> {
    parse_atom_label().map(Into::into)
}

fn parse_atom_type<'a>() -> Parser<'a, Term> {
    parse_literal("'[")
        .and_keep(sep_by0(
            || parse_identifier().map(Atom::from),
            || parse_literal(","),
        ))
        .and_drop(parse_literal("]"))
        .map(|atoms| AtomType::new(atoms).into())
}

fn parse_parens<'a>() -> Parser<'a, Term> {
    parse_literal("(")
        .and_keep(lazy(parse_term))
        .and_drop(parse_literal(")"))
}

fn parse_labeled_field<'a>() -> Parser<'a, (&'a str, Term)> {
    parse_identifier()
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
}

fn parse_tuple_type_field<'a>() -> Parser<'a, (&'a str, Term)> {
    catch(parse_labeled_field()).or(lazy(parse_term).map(|t| ("", t)))
}

fn parse_tuple_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("{")
            .and_keep(sep_by1(parse_tuple_type_field, || parse_literal(",")))
            .and_drop(parse_literal("}")),
    )
    .map(|fields| TupleType::new(fields).into())
}

fn parse_tuple<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(","))
            .and(sep_by1(|| lazy(parse_term), || parse_literal(",")))
            .and_drop(parse_literal(")")),
    )
    .map(|(first, rest)| {
        let mut fields = vec![first];
        fields.extend(rest);
        Tuple::new(fields).into()
    })
}

fn parse_func_type<'a>() -> Parser<'a, Term> {
    catch(
        parse_literal("(")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":"))
            .and(lazy(parse_term))
            .and_drop(parse_literal(")"))
            .and_drop(parse_literal("->")),
    )
    .and(lazy(parse_term))
    .map(|((label, input), output)| FuncType::new(label, input, output).into())
}

fn parse_func<'a>() -> Parser<'a, Term> {
    catch(parse_identifier().and_drop(parse_literal("=>")))
        .and(lazy(parse_term))
        .map(|(label, body)| Func::new(label, body).into())
}

fn parse_nat_match<'a>() -> Parser<'a, Term> {
    catch(
        parse_keyword("Nat.match")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(":"))
            .and(parse_identifier())
            .and_drop(parse_literal("=>")),
    )
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
            NatMatch::new(
                head,
                motive_label,
                motive,
                zero_case,
                pred_label,
                ih_label,
                succ_case,
            )
            .into()
        },
    )
}

fn parse_match_branch<'a>() -> Parser<'a, (Atom, Term)> {
    catch(
        parse_literal("|")
            .and_keep(parse_atom_label())
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
}

fn parse_match<'a>() -> Parser<'a, Term> {
    catch(
        parse_keyword("match")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(":"))
            .and(parse_identifier())
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
    .and(many1(parse_match_branch))
    .map(|(((head, motive_label), motive), cases)| {
        Match::new(head, motive_label, motive, cases).into()
    })
}

fn parse_binding<'a>() -> Parser<'a, (&'a str, Term, Term)> {
    parse_identifier()
        .and_drop(parse_literal(":"))
        .and(lazy(parse_term))
        .and_drop(parse_literal("="))
        .and(lazy(parse_term))
        .map(|((label, type_), body)| (label, type_, body))
}

fn parse_let_rec<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("let").and_drop(parse_literal("{")))
        .and_keep(sep_by1(parse_binding, || parse_literal(";")))
        .and_drop(parse_literal("}"))
        .and_drop(parse_literal(";"))
        .and(lazy(parse_term))
        .map(|(items, tail)| LetRec::new(items, tail).into())
}

fn parse_split<'a>() -> Parser<'a, Term> {
    catch(
        parse_keyword("split")
            .and_keep(lazy(parse_term))
            .and_drop(parse_literal(":"))
            .and(parse_identifier())
            .and_drop(parse_literal("=>")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
    .and_drop(parse_literal("|"))
    .and_drop(parse_literal("("))
    .and(sep_by1(|| parse_identifier(), || parse_literal(",")))
    .and_drop(parse_literal(")"))
    .and_drop(parse_literal("=>"))
    .and(lazy(parse_term))
    .map(|((((head, motive_label), motive), field_labels), tail)| {
        Split::new(head, motive_label, motive, field_labels, tail).into()
    })
}

fn parse_let<'a>() -> Parser<'a, Term> {
    catch(
        parse_keyword("let")
            .and_keep(parse_identifier())
            .and_drop(parse_literal(":"))
            .and(lazy(parse_term))
            .and_drop(parse_literal("=")),
    )
    .and(lazy(parse_term))
    .and_drop(parse_literal(";"))
    .and(lazy(parse_term))
    .map(|(((label, type_), body), tail)| Let::new(label, type_, body, tail).into())
}

fn parse_atomic_term<'a>() -> Parser<'a, Term> {
    parse_type()
        .or(parse_prim())
        .or(parse_atom_type())
        .or(parse_atom())
        .or(parse_tuple_type())
        .or(parse_tuple())
        .or(parse_parens())
        .or(parse_label())
}

fn parse_term<'a>() -> Parser<'a, Term> {
    parse_let_rec()
        .or(parse_split())
        .or(parse_let())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_let_rec_func_and_apply() {
        let term = "let { id : (x : Type) -> Type = x => x }; id a"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            LetRec::new(
                vec![(
                    "id",
                    FuncType::new("x", Type, Type),
                    Func::new("x", Var::free("x"))
                )],
                Apply::many(Var::free("id"), [Var::free("a")]),
            )
            .into(),
        );
    }

    #[test]
    fn parse_let_tuple_and_atoms() {
        let term = "let x : '[hot, cold] = 'hot; (x, 'cold)"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            Let::new(
                "x",
                AtomType::new(["hot", "cold"]),
                Atom::from("hot"),
                Tuple::new([Term::from(Var::free("x")), Term::from(Atom::from("cold"))]),
            )
            .into(),
        );
    }

    #[test]
    fn parse_split_with_motive() {
        let term = "split ('left, 'right) : p => Type; | (x, y) => p"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            Split::new(
                Tuple::new([Atom::from("left"), Atom::from("right")]),
                "p",
                Type,
                ["x", "y"],
                Var::free("p"),
            )
            .into(),
        );
    }

    #[test]
    fn parse_match_single_branch() {
        let term = "match 'foo : k => '[foo]; | 'foo => 'foo;"
            .parse::<Term>()
            .unwrap();

        assert_eq!(
            term,
            Match::new(
                Atom::from("foo"),
                "k",
                AtomType::new(["foo"]),
                [(Atom::from("foo"), Atom::from("foo"))],
            )
            .into(),
        );
    }

    #[test]
    fn parse_int_literal_and_flt_literal_are_disambiguated() {
        assert_eq!("42i".parse::<Term>().unwrap(), Term::Prim(Prim::Int(42)));

        assert_eq!("42n".parse::<Term>().unwrap(), Term::Prim(Prim::Nat(42)));

        assert_eq!(
            "42.0".parse::<Term>().unwrap(),
            Term::Prim(Prim::Flt(42.0_f32.to_bits()))
        );
    }

    #[test]
    fn parse_prim() {
        assert_eq!("Int".parse::<Term>().unwrap(), Term::Prim(Prim::IntType));
        assert_eq!("Flt".parse::<Term>().unwrap(), Term::Prim(Prim::FltType));
        assert_eq!("Nat".parse::<Term>().unwrap(), Term::Prim(Prim::NatType));
        assert_eq!("42i".parse::<Term>().unwrap(), Term::Prim(Prim::Int(42)));
        assert_eq!("42n".parse::<Term>().unwrap(), Term::Prim(Prim::Nat(42)));
        assert_eq!(
            "1.5".parse::<Term>().unwrap(),
            Term::Prim(Prim::Flt(1.5_f32.to_bits()))
        );

        assert_eq!(
            "Int.add 1i 2i".parse::<Term>().unwrap(),
            Term::Prim(Prim::int_add(
                Term::Prim(Prim::Int(1)),
                Term::Prim(Prim::Int(2))
            ))
        );

        assert_eq!(
            "Nat.add 1n 2n".parse::<Term>().unwrap(),
            Term::Prim(Prim::nat_add(
                Term::Prim(Prim::Nat(1)),
                Term::Prim(Prim::Nat(2))
            ))
        );

        assert_eq!(
            "Flt.mul 1.5 2.0".parse::<Term>().unwrap(),
            Term::Prim(Prim::flt_mul(
                Term::Prim(Prim::Flt(1.5_f32.to_bits())),
                Term::Prim(Prim::Flt(2.0_f32.to_bits()))
            ))
        );
    }
}
