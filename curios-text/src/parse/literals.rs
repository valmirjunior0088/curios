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

// `5.` refused by the rule, rather than read as the numeral `5` — or the name `5`, since a digit is an identifier character — with a `.` left over for the enclosing form to report as the token it did not expect. A dot followed by a digit is a float literal; anything else after a numeral's dot is nothing the grammar spells. Tried before the float and the numeral, since a committed refusal that ties a caught sibling on offset loses [`Parser::or`]'s tie-break to it, and the dot is consumed so the failure is past the choice point, with the caret on it.
pub(super) fn refuse_dangling_dot<'a>() -> Parser<'a, Term> {
    catch(
        take_while(|char: char| char.is_ascii_digit())
            .flat_map(|digits| match digits.is_empty() {
                true => fail("expected a numeral"),
                false => pure(()),
            })
            .and_keep(mark())
            .and_drop(take_exact("."))
            .and_drop(not_ahead_digit()),
    )
    .flat_map(|start| {
        fail_from(
            &start,
            "a floating-point literal has a decimal point followed by at least one digit — `5.0` — and `5.` is not one",
        )
    })
}

// Succeeds, consuming nothing, when the input does not go on with a digit.
fn not_ahead_digit<'a>() -> Parser<'a, ()> {
    look_ahead(take_while(|char: char| char.is_ascii_digit())).flat_map(|digits| {
        match digits.is_empty() {
            true => pure(()),
            false => fail("a digit follows"),
        }
    })
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

// `tag` takes the digit count, so the written width rides along with the base — see [`Radix`].
pub(super) fn parse_radix<'a>(
    prefix: &'static str,
    radix: u32,
    tag: fn(usize) -> Radix,
) -> Parser<'a, NatLiteral> {
    take_exact(prefix).and_keep(take_while(move |char: char| char.is_digit(radix)).flat_map(
        move |digits| match Natural::parse_bytes(digits.as_bytes(), radix) {
            Some(value) => pure(NatLiteral(value, tag(digits.len()))),
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
                    Some(value) => pure(NatLiteral(value, Radix::Dec(digits.len()))),
                    None => fail("expected nat"),
                }
            }),
        )
        .and_drop(parse_whitespace())
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
        take_while(|char| char != '\\' && char != '"' && char != '\n').flat_map(
            |chunk| match chunk.is_empty() {
                true => fail("empty chunk"),
                false => pure(chunk.to_string()),
            },
        ),
    )
    .or(catch(take_exact("\\")).and_keep(parse_escape_tail()))
}

/// What follows a backslash, in either string spelling: the five one-letter escapes and the braced Unicode scalar translated, and any other character standing beside its backslash — an unrecognized escape is not an error, so e.g. `\%` in source yields the two literal characters `\` and `%`. `\u` alone is one of them; only `\u{` is reserved, below.
fn parse_escape_tail<'a>() -> Parser<'a, String> {
    take_exact("n")
        .map(|_| "\n".to_string())
        .or(take_exact("t").map(|()| "\t".to_string()))
        .or(take_exact("r").map(|()| "\r".to_string()))
        .or(take_exact("\\").map(|()| "\\".to_string()))
        .or(take_exact("\"").map(|()| "\"".to_string()))
        .or(parse_unicode_escape().map(|char| char.to_string()))
        .or(take_n(1).map(|char| format!("\\{char}")))
}

/// A raw newline inside a one-line string literal, refused by name: the block form is the spelling for text that spans lines, and accepting the newline here would give one value two spellings for the formatter to choose between.
fn refuse_line_break_in_string<'a>() -> Parser<'a, ()> {
    mark().and_drop(take_exact("\n")).flat_map(|start| {
        fail_from(
            &start,
            "a string literal ends before its line does: close it with `\"`, or spell text that spans lines as a block string literal — `\"\"\"`, a newline, the lines, a newline and `\"\"\"`",
        )
    })
}

/// The braced Unicode escape both literal forms share: `\u{…}` with one to six hex digits naming a scalar — a surrogate, a value past `U+10FFFF`, an empty brace or a stray character is refused. It is tried only once `\u{` is read, so the brace is what reserves the form: in a string `\u` before anything else still stands for itself, and the only source this changes the meaning of is source that spelled `\u{` literally, which none did.
///
/// The printer never writes one back: a scalar prints as itself, so `"\u{65}"` and `"e"` are one literal with one canonical spelling, and `curios format` turns a written escape into the character it names.
fn parse_unicode_escape<'a>() -> Parser<'a, char> {
    take_exact("u{").and_keep(
        take_while(|char: char| char.is_ascii_hexdigit())
            .flat_map(|digits| {
                // Two faults, two sentences: a reader who wrote four digits and is told to write one to six would count them again.
                if !(1..=6).contains(&digits.len()) {
                    return fail("\\u{...} takes one to six hex digits");
                }
                match u32::from_str_radix(digits, 16).ok().and_then(char::from_u32) {
                    Some(char) => pure(char),
                    None => fail(format!(
                        "\\u{{{digits}}} names no Unicode scalar: a surrogate, U+D800 through U+DFFF, or a value past U+10FFFF"
                    )),
                }
            })
            .and_drop(take_exact("}")),
    )
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
                .or(parse_unicode_escape())
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
        .and_drop(take_exact("\"").or(refuse_line_break_in_string()))
        .and_drop(parse_whitespace())
        .map(|chunks| Subterm::Syn(Syn::Str(StrLit::line(chunks.concat()))))
        .map(Into::into)
}

/// One piece of a block string literal's body, read before the block's rules are applied.
enum BlockPiece<'a> {
    /// A run holding no quote, backslash or newline.
    Text(&'a str),
    /// A `"` that does not begin the closer.
    Quote,
    Newline,
    /// A backslash before a newline: the line joins the next.
    Join,
    /// A translated escape.
    Escape(String),
}

fn parse_block_piece<'a>() -> Parser<'a, BlockPiece<'a>> {
    catch(
        take_while(|char| char != '"' && char != '\\' && char != '\n').flat_map(|run| {
            match run.is_empty() {
                true => fail("empty run"),
                false => pure(BlockPiece::Text(run)),
            }
        }),
    )
    .or(catch(take_exact("\n")).map(|()| BlockPiece::Newline))
    // Tried before the general escape, where a backslash before a newline would stand beside it.
    .or(catch(take_exact("\\\n")).map(|()| BlockPiece::Join))
    .or(catch(take_exact("\\")).and_keep(parse_escape_tail().map(BlockPiece::Escape)))
    .or(catch(take_exact("\"").and_drop(not_ahead("\"\""))).map(|()| BlockPiece::Quote))
}

/// A block string literal: `"""` and a newline, the lines, then a newline, optional whitespace and `"""`. Both delimiters take their newline, so the value is exactly the lines between, joined by newlines.
///
/// The rules the body is read under, in order: the leading whitespace the non-blank lines and the closer's line share is removed from each line, so a block reads at the indentation of the code around it and content indented past the closer keeps the difference; a whitespace-only line becomes an empty line and takes no part in that prefix; trailing whitespace is stripped from every line; then the escapes stand, translated after the stripping so `\u{20}` spells a space the stripping would take, and a backslash before a newline joins the two lines. The escapes are the one-line form's — a `"` inside is itself and three quotes are spelled `\"""` — so the two spellings differ in nothing but the newlines and the indentation.
///
/// Refusals are the delimiters': an opener followed by anything but a newline, and a closer that is not alone on its line, each named from where it stands.
pub(super) fn parse_block_string_literal<'a>() -> Parser<'a, Term> {
    mark()
        .flat_map(|open| {
            take_exact("\"\"\"")
                .and_drop(take_while(|char| char == ' ' || char == '\t'))
                // The refusal consumes the offending character before failing, so it stands past the choice point and wins the tie a same-offset failure would lose.
                .and_drop(take_exact("\n").or(take_n(1).flat_map(move |_| {
                    fail_from(
                        &open,
                        "a block string literal opens with `\"\"\"` and a newline; its text begins on the next line",
                    )
                })))
                .and_keep(many0(parse_block_piece))
                .and(mark())
                .and_drop(take_exact("\"\"\""))
                .flat_map(|(pieces, close)| match assemble_block(pieces) {
                    Ok(value) => pure(Subterm::Syn(Syn::Str(StrLit::block(value)))),
                    Err(message) => fail_from(&close, message),
                })
        })
        .and_drop(parse_whitespace())
        .map(Into::into)
}

/// The value a block's pieces spell, under the rules `parse_block_string_literal` states, or the refusal for a closer that shares its line with content.
fn assemble_block(pieces: Vec<BlockPiece<'_>>) -> Result<String, &'static str> {
    // The physical lines, each with whether a backslash joined it to the next; the last is the closer's.
    let mut lines: Vec<(Vec<BlockPiece<'_>>, bool)> = vec![(Vec::new(), false)];
    for piece in pieces {
        match piece {
            BlockPiece::Newline => lines.push((Vec::new(), false)),
            BlockPiece::Join => {
                lines.last_mut().expect("one line at least").1 = true;
                lines.push((Vec::new(), false));
            }
            piece => lines.last_mut().expect("one line at least").0.push(piece),
        }
    }

    let (closer, _) = lines.pop().expect("one line at least");
    let closer_indent = match closer.as_slice() {
        [] => "",
        [BlockPiece::Text(text)] if is_whitespace(text) => text,
        _ => {
            return Err(
                "a block string literal closes with a newline, its indentation and `\"\"\"`; three quotes inside it are spelled `\\\"\"\"`",
            );
        }
    };

    let is_blank = |line: &[BlockPiece<'_>]| match line {
        [] => true,
        [BlockPiece::Text(text)] => is_whitespace(text),
        _ => false,
    };

    let shared = lines
        .iter()
        .filter(|(line, _)| !is_blank(line))
        .map(|(line, _)| indent_of(line))
        .chain([closer_indent])
        .reduce(common_prefix)
        .unwrap_or("");

    let mut value = String::new();
    for (index, (line, joined)) in lines.iter().enumerate() {
        if !is_blank(line) {
            let last = line.len() - 1;
            for (position, piece) in line.iter().enumerate() {
                match piece {
                    BlockPiece::Text(text) => {
                        let mut text: &str = text;
                        if position == 0 {
                            text = &text[shared.len()..];
                        }
                        if position == last && !joined {
                            text = text.trim_end_matches([' ', '\t']);
                        }
                        value.push_str(text);
                    }
                    BlockPiece::Quote => value.push('"'),
                    BlockPiece::Escape(escaped) => value.push_str(escaped),
                    BlockPiece::Newline | BlockPiece::Join => unreachable!("split above"),
                }
            }
        }
        if index + 1 < lines.len() && !joined {
            value.push('\n');
        }
    }

    Ok(value)
}

/// The whitespace a line opens with: the part of a leading run that the shared prefix is taken from.
fn indent_of<'s>(line: &[BlockPiece<'s>]) -> &'s str {
    match line.first() {
        Some(BlockPiece::Text(text)) => {
            &text[..text.len() - text.trim_start_matches([' ', '\t']).len()]
        }
        _ => "",
    }
}

fn is_whitespace(text: &str) -> bool {
    text.trim_matches([' ', '\t']).is_empty()
}

/// The longest prefix `a` and `b` share, as a slice of `a`.
fn common_prefix<'s>(a: &'s str, b: &str) -> &'s str {
    let shared = a
        .char_indices()
        .zip(b.chars())
        .take_while(|((_, x), y)| x == y)
        .map(|((offset, x), _)| offset + x.len_utf8())
        .last()
        .unwrap_or(0);
    &a[..shared]
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
