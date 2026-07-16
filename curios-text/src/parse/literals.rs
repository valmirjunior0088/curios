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

// A polymorphic integer literal: an optional sign glued to a magnitude
// (decimal, `0x`, or `0b`). Its concrete type (`Nat`/`Int`/`Flt`) is chosen by
// elaboration — a written sign rules out `Nat`. The sign must touch the digits;
// `- 42` (spaced) is the subtraction operator, not a negative literal.
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
    .map(|value| Subterm::Prim(Prim::Flt(Flt::from_f32(value))))
    .map(Into::into)
}

pub(super) fn parse_hex_byte<'a>() -> Parser<'a, u8> {
    take_exact("\\").and_keep(take_while(|char| char.is_ascii_hexdigit()).flat_map(|hex| {
        match hex.len() {
            2 => pure(u8::from_str_radix(hex, 16).expect("valid hex pair")),
            _ => fail("Expected exactly 2 hex digits after \\"),
        }
    }))
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
            .or(fail("Unknown string escape sequence")),
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

// A `Bin` spread operand under the TIGHT rule: it must end without consuming
// any whitespace, so the segment loop can require the next segment to begin
// immediately with `\` (`\..xs \01` ends the literal at `xs`). The operand is
// an atomic term in glued form: a name path or a parenthesized term, followed
// by glued suffixes — projections, calls, `!` (`\..hdr.bytes`, `\..f(x)`,
// `\..read()!`). Self-delimiting parts (a call's argument list, the parens
// form) admit interior whitespace freely; only the operand's edges are tight,
// with every closing delimiter matched raw. Anything else — an infix chain, a
// lambda — takes the parenthesized form.
pub(super) fn parse_bin_spread_operand<'a>() -> Parser<'a, Term> {
    with_span(
        parse_name_raw()
            .map(|name| Term::from(Subterm::Name(name)))
            .or(take_exact("(")
                .and_drop(parse_whitespace())
                .and_keep(lazy(parse_term))
                .and_drop(take_exact(")")))
            .and(many0(parse_suffix_raw))
            .map(|(head, suffixes)| apply_suffixes(head, suffixes)),
    )
}

// One segment of a `Bits`/`Bytes` literal: a literal atom, or a `\..` spread. Adjacent
// bytes are coalesced into `BinSegment::Bytes` runs by the literal parser.
pub(super) enum RawBinSegment {
    Byte(u8),
    Spread(Term),
}

pub(super) fn parse_bin_segment<'a>() -> Parser<'a, RawBinSegment> {
    catch(parse_hex_byte())
        .map(RawBinSegment::Byte)
        // Committed after `\..`: an operand failure is fatal (no inner
        // `catch`), so the error points at the segment rather than at
        // whatever the surrounding grammar makes of the leftovers.
        .or(catch(take_exact("\\..")).and_keep(
            parse_bin_spread_operand()
                .map_err("Expected a glued name or parenthesized term after '\\..'")
                .map(RawBinSegment::Spread),
        ))
}

pub(super) fn parse_bit_segment<'a>() -> Parser<'a, RawBinSegment> {
    catch(take_exact("\\0"))
        .map(|()| RawBinSegment::Byte(0))
        .or(catch(take_exact("\\1")).map(|()| RawBinSegment::Byte(1)))
        .or(catch(take_exact("\\..")).and_keep(
            parse_bin_spread_operand()
                .map_err("Expected a glued name or parenthesized term after '\\..'")
                .map(RawBinSegment::Spread),
        ))
}

pub(super) fn coalesce_bin_segments(raw: Vec<RawBinSegment>) -> Vec<BinSegment> {
    let mut segments = Vec::new();

    for segment in raw {
        match segment {
            RawBinSegment::Byte(byte) => match segments.last_mut() {
                Some(BinSegment::Bytes(run)) => run.push(byte),
                _ => segments.push(BinSegment::Bytes(vec![byte])),
            },
            RawBinSegment::Spread(term) => segments.push(BinSegment::Spread(term)),
        }
    }

    segments
}

// A `Bits`/`Bytes` literal: a prefixed empty form, or one-or-more glued atom
// and `\..operand` segments. One whitespace-free lexical unit: after a spread
// operand the literal continues only when the very next character is `\`.
pub(super) fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    let empty_bits = catch(take_exact("b\\").and_drop(parse_whitespace()))
        .map(|()| (curios_base::Grain::B, Vec::new()));
    let bits = catch(take_exact("b").and_keep(many1(parse_bit_segment).map(coalesce_bin_segments)))
        .and_drop(parse_whitespace())
        .map(|segments| (curios_base::Grain::B, segments));
    let empty_bytes = catch(take_exact("x\\").and_drop(parse_whitespace()))
        .map(|()| (curios_base::Grain::X, Vec::new()));
    let bytes =
        catch(take_exact("x").and_keep(many1(parse_bin_segment).map(coalesce_bin_segments)))
            .and_drop(parse_whitespace())
            .map(|segments| (curios_base::Grain::X, segments));

    bits.or(empty_bits)
        .or(bytes)
        .or(empty_bytes)
        .map(|(grain, segments)| Subterm::Prim(Prim::Bin(grain, segments)))
        .map(Into::into)
}

// One entry of an `Lst` literal: a `..` spread contributing a whole list, or
// a plain element. Unlike a `Bits`/`Bytes` literal, brackets and commas delimit, so
// spreads take full terms and `[.. xs]` may be spaced (as in struct spread).
pub(super) fn parse_lst_entry<'a>() -> Parser<'a, LstEntry> {
    catch(parse_literal(".."))
        .and_keep(lazy(parse_term))
        .map(LstEntry::Spread)
        .or(lazy(parse_term).map(LstEntry::Elem))
}

// An `Lst` literal `[e0, ..rest, e1, …]` (empty `[]`) — the native
// contiguous-sequence sibling of the packed binary literals. Builds a `Prim::Lst`
// directly (the element type is an implicit the literal cannot name; core
// elaboration infers it); spreads splice in place, any position and count.
pub(super) fn parse_lst_literal<'a>() -> Parser<'a, Term> {
    catch(parse_literal("["))
        .and_keep(sep_by0(parse_lst_entry, || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(|entries| Subterm::Prim(Prim::Lst(entries)))
        .map(Into::into)
}

pub(super) fn parse_bln_prim<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("false"))
        .map(|()| Subterm::Prim(Prim::Bln(false)))
        .or(catch(parse_keyword("true")).map(|()| Subterm::Prim(Prim::Bln(true))))
        .map(Into::into)
}

// Primitive types and operations are no longer surface syntax — they live in the
// `sys` module (see `prelude.rs`) and parse as ordinary names. Only genuine
// literals (and the boolean keywords) remain here.
