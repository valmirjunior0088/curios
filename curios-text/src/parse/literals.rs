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
            .map(|()| Sign::Negative)
            .or(catch(take_exact("+")).map(|()| Sign::Positive))
            .or(pure(Sign::Unmarked))
            .and(parse_nat_digits()),
    )
    .map(|(sign, lit)| {
        let NatLiteral(magnitude, radix) = lit;
        Subterm::NumLit(NumLit {
            magnitude,
            radix,
            sign,
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
        move |digits| match Natural::parse_bytes(digits.as_bytes(), radix) {
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
                match Natural::parse_bytes(digits.as_bytes(), 10) {
                    Some(value) => pure(NatLiteral(value, Radix::Dec)),
                    None => fail("expected nat"),
                }
            }),
        )
        .and_drop(parse_whitespace())
}

pub(super) fn parse_nat_literal_u32<'a>() -> Parser<'a, u32> {
    parse_nat_digits().flat_map(|lit| {
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
            .flat_map::<Floating, _>(|(sign, digits)| {
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

                // Narrowed by the model rather than by the host's parser, so what a literal *means* is stated in this repository like every other `Flt` value. `str::parse::<f32>` is correctly rounded and gives the same bits on every input, so no program changes; what changes is that the answer no longer depends on the machine the compiler runs on.
                match decimal_parts(digits) {
                    Some((value, scale)) => pure(Floating::of_decimal(sign == "-", &value, scale)),
                    None => fail("Expected float literal"),
                }
            })
            .and_drop(parse_whitespace()),
    )
    .flat_map::<Floating, _>(|value: Floating| {
        // An overflowing magnitude rounds to the infinity of its sign, a value the grammar has no spelling for — refused here, past the catch, so the digits that committed this branch as a float literal cannot silently reparse as something else.
        if value.is_finite() {
            pure(value)
        } else {
            fail("Float literal overflows Flt")
        }
    })
    .map(|value| Subterm::Intrinsic(Intrinsic::Flt(value)))
    .map(Into::into)
}

/// Split a float literal's digits into the numeral they spell and the power of ten scaling it: `12.5e3` is `125` scaled by `2`. The grammar has already established a dot with at least one digit after it, so what is left to refuse is a malformed exponent or a stray character the character class admitted.
///
/// The same decomposition `/std/Flt/of_str` performs, and deliberately so — one decimal is narrowed by one routine, whether a program spells it as a literal or reads it from a string.
fn decimal_parts(digits: &str) -> Option<(Natural, i32)> {
    let (mantissa, exponent) = match digits.split_once(['e', 'E']) {
        Some((mantissa, exponent)) => (mantissa, exponent.parse::<i32>().ok()?),
        None => (digits, 0),
    };

    let (integral, fractional) = mantissa.split_once('.')?;
    let spelled = format!("{integral}{fractional}");

    if fractional.is_empty() || !spelled.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }

    Some((
        Natural::parse_bytes(spelled.as_bytes(), 10)?,
        exponent.checked_sub(i32::try_from(fractional.len()).ok()?)?,
    ))
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
            // An unrecognized escape is not an error: the backslash and the following character both stand for themselves, so e.g. `\%` in source yields the two literal characters `\` and `%`.
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

// One entry of a `Bits`/`Bytes` literal, mirroring `parse_list_entry`: a `..` spread contributing a whole packed value, or a plain term contributing a single atom. A constant atom has no spelling of its own — `1` in a `Bits` literal and `0x48` in a `Bytes` literal are ordinary numeral terms realized at `Bool` and `Byte` by elaboration, and lowering folds adjacent constants into a packed run rather than a chain of appends (see `into_core`'s `lower_bin_literal`).
fn parse_bin_entry<'a>() -> Parser<'a, BinSegment> {
    catch(parse_literal(".."))
        .and_keep(lazy(parse_term))
        .map(BinSegment::Spread)
        .or(lazy(parse_term).map(BinSegment::Atom))
}

// A `Bits`/`Bytes` literal `b[1, flag, ..rest]` (empty `b[]`) — the packed siblings of the `List` literal, told apart by the grain letter glued to the bracket. Only that junction is tight: past the `[` the literal lexes like any other bracketed list, so whitespace, a trailing comma, and arbitrary (unparenthesized) element and spread terms are all free. The glue is what keeps `b` and `x` usable as ordinary binders — `b [1]` is the binder `b` followed by a list, not a `Bits` literal — and an identifier merely ending in the grain letter never starts one, since the name parser reaches `nb[…]` first.
pub(super) fn parse_bin_literal<'a>() -> Parser<'a, Term> {
    parse_bin_literal_grain(Grain::B, "b[").or(parse_bin_literal_grain(Grain::X, "x["))
}

fn parse_bin_literal_grain<'a>(grain: Grain, prefix: &'static str) -> Parser<'a, Term> {
    catch(parse_literal(prefix))
        .and_keep(sep_by0_trailing(parse_bin_entry, || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(move |segments| Subterm::Intrinsic(Intrinsic::Bin(grain, segments)))
        .map(Into::into)
}

// One entry of a `List` literal: a `..` spread contributing a whole list, or a plain element. Unlike a `Bits`/`Bytes` literal, brackets and commas delimit, so spreads take full terms and `[.. xs]` may be spaced (as in struct spread).
pub(super) fn parse_list_entry<'a>() -> Parser<'a, ListEntry> {
    catch(parse_literal(".."))
        .and_keep(lazy(parse_term))
        .map(ListEntry::Spread)
        .or(lazy(parse_term).map(ListEntry::Elem))
}

// An `List` literal `[e0, ..rest, e1, …]` (empty `[]`) — the native contiguous-sequence sibling of the packed binary literals. Builds an `Intrinsic::List` directly (the element type is an implicit the literal cannot name; core elaboration infers it); spreads splice in place, any position and count.
pub(super) fn parse_list_literal<'a>() -> Parser<'a, Term> {
    catch(parse_literal("["))
        .and_keep(sep_by0_trailing(parse_list_entry, || parse_literal(",")))
        .and_drop(parse_literal("]"))
        .map(|entries| Subterm::Intrinsic(Intrinsic::List(entries)))
        .map(Into::into)
}

pub(super) fn parse_bool_intrinsic<'a>() -> Parser<'a, Term> {
    catch(parse_keyword("false"))
        .map(|()| Subterm::Intrinsic(Intrinsic::Bool(false)))
        .or(catch(parse_keyword("true")).map(|()| Subterm::Intrinsic(Intrinsic::Bool(true))))
        .map(Into::into)
}

// Intrinsic types and operations are no longer surface syntax — they live in the `sys` module (see `prelude.rs`) and parse as ordinary names. Only genuine literals (and the boolean keywords) remain here.
