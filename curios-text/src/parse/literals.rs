use super::*;

pub(super) fn parse_type<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Type"))
        .map(|()| Subterm::Type)
        .map(Into::into)
}

pub(super) fn parse_prop<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("Prop"))
        .map(|()| Subterm::Prop)
        .map(Into::into)
}

// A polymorphic integer literal: an optional sign glued to a magnitude (decimal, `0x`, or `0b`). Its concrete type (`Nat`/`Int`/`Flt`) is chosen by elaboration — a written sign rules out `Nat`. The sign must touch the digits; `- 42` (spaced) is the subtraction operator, not a negative literal.
pub(super) fn parse_num_lit<'a>() -> Parser<'a, Term> {
    catch(
        catch(take_exact("-"))
            .map(|()| (true, true))
            .or(catch(take_exact("+")).map(|()| (true, false)))
            .or(pure((false, false)))
            .and(parse_nat_digits()),
    )
    .map(|((signed, negative), lit)| {
        let NatLiteral(magnitude, radix) = lit;
        Subterm::NumLit(NumLit {
            magnitude,
            radix,
            signed,
            negative,
        })
    })
    .map(Into::into)
}

// A character literal is a monomorphic, proof-certified `/syn/Char` value.
pub(super) fn parse_char_lit<'a>() -> Parser<'a, Term> {
    catch(
        take_exact("'")
            .and_keep(parse_char_value())
            .and_drop(take_exact("'"))
            .and_drop(parse_whitespace()),
    )
    .map(|character| Subterm::Syn(Syn::Char(character)))
    .map(Into::into)
}

pub(super) fn parse_usize_raw<'a>() -> Parser<'a, usize> {
    take_while(|char: char| char.is_ascii_digit()).flat_map(|digits| {
        match digits.parse::<usize>() {
            Ok(value) => pure(value),
            Err(_) => fail("expected usize"),
        }
    })
}

pub(super) fn parse_radix<'a>(
    prefix: &'static str,
    radix: u32,
    tag: Radix,
) -> Parser<'a, NatLiteral> {
    take_exact(prefix).and_keep(take_while(move |char: char| char.is_digit(radix)).flat_map(
        move |digits| match BigUint::parse_bytes(digits.as_bytes(), radix) {
            Some(value) => pure(NatLiteral(value, tag)),
            None => fail(format!("expected base-{radix} digits after '{prefix}'")),
        },
    ))
}

pub(super) fn parse_nat_digits<'a>() -> Parser<'a, NatLiteral> {
    catch(parse_radix("0x", 16, Radix::Hex))
        .or(catch(parse_radix("0b", 2, Radix::Bin)))
        .or(
            take_while(|char: char| char.is_ascii_digit()).flat_map(|digits| {
                match digits.parse::<BigUint>() {
                    Ok(value) => pure(NatLiteral(value, Radix::Dec)),
                    Err(_) => fail("expected nat"),
                }
            }),
        )
        .and_drop(parse_whitespace())
}

pub(super) fn parse_nat<'a>() -> Parser<'a, NatLiteral> {
    parse_nat_digits()
}

pub(super) fn parse_nat_literal_u32<'a>() -> Parser<'a, u32> {
    parse_nat().flat_map(|lit| {
        let NatLiteral(n, _) = lit;
        match n.to_u32() {
            Some(k) => pure(k),
            None => fail("nat literal too large for u32"),
        }
    })
}

pub(super) fn parse_flt_value<'a>() -> Parser<'a, Term> {
    catch(
        catch(take_exact("-"))
            .map(|()| "-".to_string())
            .or(catch(take_exact("+")).map(|()| "+".to_string()))
            .or(pure(String::new()))
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
    .map(|value| Subterm::Intrinsic(Intrinsic::Flt(Flt::from_f32(value))))
    .map(Into::into)
}

pub(super) fn parse_string_chunk<'a>() -> Parser<'a, String> {
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
            // An unrecognized escape is not an error: the backslash and the
            // following character both stand for themselves, so e.g. `\%`
            // in source yields the two literal characters `\` and `%`.
            .or(take_n(1).map(|char| format!("\\{char}"))),
    ))
}

pub(super) fn parse_char_value<'a>() -> Parser<'a, char> {
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

pub(super) fn parse_string_literal<'a>() -> Parser<'a, Term> {
    catch(take_exact("\""))
        .and_keep(many0(parse_string_chunk))
        .and_drop(take_exact("\""))
        .and_drop(parse_whitespace())
        .map(|chunks| Subterm::Syn(Syn::Str(chunks.concat())))
        .map(Into::into)
}

// One segment of a `Bits`/`Bytes` literal: an escaped constant atom, a term contributing one atom, or a spread contributing a whole packed value. Adjacent bytes are coalesced into `BinSegment::Bytes` runs by the literal parser.
pub(super) enum RawBinSegment {
    Byte(u8),
    Atom(Term),
    Spread(Term),
}

// A `\`-escaped constant atom: `\0`/`\1` for `Bits`, `\` and exactly two hexadecimal digits for `Bytes`. `\` begins an atom and nothing else — no term does — so the parser commits once it sees one and reports a malformed atom rather than backtracking into the term case and blaming the leftovers.
fn parse_bin_atom<'a>(grain: Grain) -> Parser<'a, RawBinSegment> {
    catch(take_exact("\\"))
        .and_keep(match grain {
            Grain::B => parse_bit_atom(),
            Grain::X => parse_hex_atom(),
        })
        .and_drop(parse_whitespace())
        .map(RawBinSegment::Byte)
}

fn parse_bit_atom<'a>() -> Parser<'a, u8> {
    take_exact("0")
        .map(|()| 0u8)
        .or(take_exact("1").map(|()| 1u8))
        .map_err("Expected '\\0' or '\\1' in a Bits literal")
}

fn parse_hex_atom<'a>() -> Parser<'a, u8> {
    take_while(|char: char| char.is_ascii_hexdigit()).flat_map(|hex| match hex.len() {
        2 => pure(u8::from_str_radix(hex, 16).expect("valid hex pair")),
        _ => fail("Expected exactly 2 hexadecimal digits after '\\' in a Bytes literal"),
    })
}

// One entry of a `Bits`/`Bytes` literal, mirroring `parse_lst_entry`: an escaped constant atom, a `..` spread contributing a whole packed value, or a plain term contributing a single atom. The escape has no term spelling of its own on purpose — it is what marks compile-time constant data, which lowering keeps as a packed run rather than a chain of appends (see `into_core`'s `lower_bin_literal`).
fn parse_bin_entry<'a>(grain: Grain) -> Parser<'a, RawBinSegment> {
    parse_bin_atom(grain)
        .or(catch(parse_literal(".."))
            .and_keep(lazy(parse_term))
            .map(RawBinSegment::Spread))
        .or(lazy(parse_term).map(RawBinSegment::Atom))
}

pub(super) fn coalesce_bin_segments(raw: Vec<RawBinSegment>) -> Vec<BinSegment> {
    let mut segments = Vec::new();

    for segment in raw {
        match segment {
            RawBinSegment::Byte(byte) => match segments.last_mut() {
                Some(BinSegment::Bytes(run)) => run.push(byte),
                _ => segments.push(BinSegment::Bytes(vec![byte])),
            },
            RawBinSegment::Atom(term) => segments.push(BinSegment::Atom(term)),
            RawBinSegment::Spread(term) => segments.push(BinSegment::Spread(term)),
        }
    }

    segments
}

// A `Bits`/`Bytes` literal `b[\1, flag, ..rest]` (empty `b[]`) — the packed siblings of the `Lst` literal, told apart by the grain letter glued to the bracket. Only that junction is tight: past the `[` the literal lexes like any other bracketed list, so whitespace, a trailing comma, and arbitrary (unparenthesized) element and spread terms are all free. The glue is what keeps `b` and `x` usable as ordinary binders — `b [1]` is the binder `b` followed by a list, not a `Bits` literal — and an identifier merely ending in the grain letter never starts one, since the name parser reaches `nb[…]` first.
pub(super) fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    parse_bin_literal_grain(Grain::B, "b[").or(parse_bin_literal_grain(Grain::X, "x["))
}

fn parse_bin_literal_grain<'a>(grain: Grain, prefix: &'static str) -> Parser<'a, Term> {
    catch(parse_literal(prefix))
        .and_keep(sep_by0_trailing(
            move || parse_bin_entry(grain),
            || parse_literal(","),
        ))
        .and_drop(parse_literal("]"))
        .map(move |segments| {
            Subterm::Intrinsic(Intrinsic::Bin(grain, coalesce_bin_segments(segments)))
        })
        .map(Into::into)
}

// One entry of an `Lst` literal: a `..` spread contributing a whole list, or a plain element. Unlike a `Bits`/`Bytes` literal, brackets and commas delimit, so spreads take full terms and `[.. xs]` may be spaced (as in struct spread).
pub(super) fn parse_lst_entry<'a>() -> Parser<'a, LstEntry> {
    catch(parse_literal(".."))
        .and_keep(lazy(parse_term))
        .map(LstEntry::Spread)
        .or(lazy(parse_term).map(LstEntry::Elem))
}

// An `Lst` literal `[e0, ..rest, e1, …]` (empty `[]`) — the native contiguous-sequence sibling of the packed binary literals. Builds an `Intrinsic::Lst` directly (the element type is an implicit the literal cannot name; core elaboration infers it); spreads splice in place, any position and count.
pub(super) fn parse_lst_literal<'a>() -> Parser<'a, Term> {
    catch(parse_literal("["))
        .and_keep(sep_by0_trailing(parse_lst_entry, || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(|entries| Subterm::Intrinsic(Intrinsic::Lst(entries)))
        .map(Into::into)
}

pub(super) fn parse_bool_intrinsic<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("false"))
        .map(|()| Subterm::Intrinsic(Intrinsic::Bool(false)))
        .or(catch(parse_keyword("true")).map(|()| Subterm::Intrinsic(Intrinsic::Bool(true))))
        .map(Into::into)
}

// Intrinsic types and operations are no longer surface syntax — they live in the `sys` module (see `prelude.rs`) and parse as ordinary names. Only genuine literals (and the boolean keywords) remain here.
