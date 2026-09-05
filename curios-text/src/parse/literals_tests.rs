//! Numeric, character, string, list and binary literals, and the spread segments inside them.

use {
    crate::*,
    curios_num::Floating,
    curios_utilities::{Grain, Sign},
};

use super::test_support::*;

#[test]
fn integer_literals_are_polymorphic_num_lits() {
    // Integer literals are polymorphic `NumLit`s; the sign is optional and only records whether `Nat` is still a candidate. Decimals stay monomorphic `Flt`.
    assert_eq!("42".parse::<Term>().unwrap(), num_lit(42, Sign::Unmarked));
    assert_eq!("+42".parse::<Term>().unwrap(), num_lit(42, Sign::Positive));
    assert_eq!("-42".parse::<Term>().unwrap(), num_lit(42, Sign::Negative));
    assert_eq!(
        "42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(42.0))))
    );
    assert_eq!(
        "+42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(42.0))))
    );
    assert_eq!(
        "-42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(
            -42.0
        ))))
    );
}

#[test]
fn rejects_a_float_literal_that_overflows_to_infinity() {
    // The model rounds an overflowing magnitude to the infinity of its sign, which the grammar cannot spell — the literal is refused outright rather than backtracked into a different parse of the same digits.
    assert!("1.0e999".parse::<Term>().is_err());
    assert!("-1.0e999".parse::<Term>().is_err());
    // An exponent too large to be a decimal exponent at all is refused before any narrowing, rather than overflowing the count it is read into.
    assert!("1.0e99999999999".parse::<Term>().is_err());
    // The largest finite magnitudes still parse, and the pair below brackets the rounding threshold — `2^128 − 2^103`, which sits *above* the largest finite value. A numeral under it narrows to that value; one over it is an overflow. Both were taken from the model's own oracle table against `str::parse::<f32>`, not from arithmetic done in prose.
    assert_eq!(
        "3.4e38".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(
            3.4e38
        ))))
    );
    assert_eq!(
        "3.4028235e38".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(
            f32::MAX
        ))))
    );
    assert!("3.4028236e38".parse::<Term>().is_err());
    // A subnormal narrows on the `2^-149` grid rather than flushing to zero, and one below half that grid step rounds away.
    assert_eq!(
        "1.0e-45".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(
            1.0e-45
        ))))
    );
    assert_eq!(
        "1.0e-50".parse::<Term>().unwrap(),
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(0.0))))
    );
}

#[test]
fn char_literal_ascii() {
    assert_eq!(
        "'a'".parse::<Term>().unwrap(),
        Term::from(Subterm::Syn(Syn::Char('a')))
    );
}

#[test]
fn char_literal_escape() {
    assert_eq!(
        "'\\n'".parse::<Term>().unwrap(),
        Term::from(Subterm::Syn(Syn::Char('\n')))
    );
}

#[test]
fn char_literals_round_trip_unicode_and_supported_escapes() {
    for source in [
        "'a'", "'λ'", "'😀'", "'\\n'", "'\\t'", "'\\r'", "'\\''", "'\\\\'",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}

#[test]
fn a_character_literal_is_a_dispatch_pattern() {
    // The former exclusion, overturned: a character pattern is a `Nat` dispatch case spelled by its scalar value, and it round-trips as written.
    let source = "match 97 | 'a' => 0 | '\\n' => 1 | _ => 2 end";
    let term = source.parse::<Term>().unwrap();
    let Subterm::Match(match_) = term.as_subterm() else {
        panic!("expected a match");
    };
    assert!(
        matches!(match_.arms[0].pattern, MatchPattern::Char('a')),
        "{:?}",
        match_.arms[0].pattern
    );
    assert!(
        matches!(match_.arms[1].pattern, MatchPattern::Char('\n')),
        "{:?}",
        match_.arms[1].pattern
    );
    assert_eq!(term.to_string(), source);
}

#[test]
fn hex_literal_is_num_lit() {
    assert_eq!(
        "0xC2".parse::<Term>().unwrap(),
        Subterm::NumLit(NumLit {
            magnitude: 194usize.into(),
            radix: Radix::Hex(2),
            sign: Sign::Unmarked,
        })
        .into()
    );
}

#[test]
fn bin_literal_is_num_lit() {
    assert_eq!(
        "0b1010".parse::<Term>().unwrap(),
        Subterm::NumLit(NumLit {
            magnitude: 10usize.into(),
            radix: Radix::Bin(4),
            sign: Sign::Unmarked,
        })
        .into()
    );
}

#[test]
fn nat_radix_round_trips_through_the_printer() {
    for source in ["0xC2", "0xF4", "0b1010", "127"] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}

/// A numeral's written *width* round-trips with its base, so a padded literal prints back as written.
///
/// The width used to be dropped, and every literal printed at its natural one: `0x00` came back as `0x0`. What that costs is a table — `x[0x00, 0x48, 0x69]` is bytes in columns, and `curios format` silently narrowed it to `x[0x0, 0x48, 0x69]`.
#[test]
fn a_padded_numeral_keeps_the_width_it_was_written_at() {
    for source in [
        "0x00",
        "0x0A",
        "0x00FF",
        "0b0001",
        "007",
        "x[0x00, 0x48, 0x69, 0x0A]",
        "b[0, 1, 1]",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}

#[test]
fn string_literal_is_str() {
    assert_eq!(
        "\"a\"".parse::<Term>().unwrap(),
        Term::from(Subterm::Syn(Syn::Str("a".to_string())))
    );
}

#[test]
fn unrecognized_string_escape_is_literal_backslash_and_char() {
    assert_eq!(
        "\"\\%\"".parse::<Term>().unwrap(),
        Term::from(Subterm::Syn(Syn::Str("\\%".to_string())))
    );
}

#[test]
fn a_braced_unicode_escape_names_a_scalar_in_a_string() {
    // `\u{…}` is the one way to spell a scalar without pasting it; a combining mark is the case that needs it.
    for (source, expected) in [
        ("\"\\u{65}\"", "e"),
        ("\"e\\u{301}\"", "e\u{301}"),
        ("\"\\u{1F600}\"", "😀"),
        ("\"\\u{0}\"", "\0"),
    ] {
        assert_eq!(
            source.parse::<Term>().unwrap(),
            Term::from(Subterm::Syn(Syn::Str(expected.to_string()))),
            "{source}"
        );
    }
}

#[test]
fn a_braced_unicode_escape_names_a_scalar_in_a_character() {
    assert_eq!(
        "'\\u{301}'".parse::<Term>().unwrap(),
        Term::from(Subterm::Syn(Syn::Char('\u{301}')))
    );
    // The printer writes the scalar itself, so the escape has no round trip of its own: one scalar, one spelling.
    assert_eq!("'\\u{65}'".parse::<Term>().unwrap().to_string(), "'e'");
    assert_eq!("\"\\u{65}\"".parse::<Term>().unwrap().to_string(), "\"e\"");
}

#[test]
fn a_backslash_u_without_a_brace_still_stands_for_itself() {
    // Only the brace reserves the form: the break is confined to source that spelled `\u{`.
    for (source, expected) in [
        ("\"\\u\"", "\\u"),
        ("\"\\ux\"", "\\ux"),
        ("\"\\u {41}\"", "\\u {41}"),
    ] {
        assert_eq!(
            source.parse::<Term>().unwrap(),
            Term::from(Subterm::Syn(Syn::Str(expected.to_string()))),
            "{source}"
        );
    }
}

#[test]
fn a_malformed_braced_escape_is_refused() {
    // Once `\u{` is read the form is committed: an empty brace, a non-hex digit, a surrogate, a value past the last scalar, a seventh digit, or a missing brace is the diagnosis rather than a fallback, in a string and in a character alike.
    for source in [
        "\"\\u{}\"",
        "\"\\u{zz}\"",
        "\"\\u{D800}\"",
        "\"\\u{110000}\"",
        "\"\\u{0000041}\"",
        "\"\\u{41\"",
        "'\\u{}'",
        "'\\u{D800}'",
        "'\\u{41'",
    ] {
        assert!(source.parse::<Term>().is_err(), "{source}");
    }
}

#[test]
fn char_literal_multi_char_is_error() {
    assert!("'ab'".parse::<Term>().is_err());
}

#[test]
fn char_literal_empty_is_error() {
    assert!("''".parse::<Term>().is_err());
}

#[test]
fn spread_entries_are_struct_literal_only() {
    // A `..base` spread parses as a struct-literal entry, on any head shape.
    let term = "Pair { ..p, snd = b }".parse::<Term>().unwrap();
    let Subterm::StructLit(StructLit { entries, .. }) = term.as_subterm() else {
        panic!("expected a struct literal");
    };
    assert!(matches!(entries[0], StructLitEntry::Spread(_)));
    assert!(matches!(entries[1], StructLitEntry::Field(_)));

    let term = "Pair(Nat, Bin) { ..p }".parse::<Term>().unwrap();
    let Subterm::StructLit(StructLit {
        params, entries, ..
    }) = term.as_subterm()
    else {
        panic!("expected a struct literal");
    };
    assert_eq!(params.len(), 2);
    assert!(matches!(entries[0], StructLitEntry::Spread(_)));

    // A misplaced spread still parses — position and multiplicity are rejected at elaboration, not parse (like non-concept `use` entries).
    let term = "Pair { fst = a, ..p }".parse::<Term>().unwrap();
    let Subterm::StructLit(StructLit { entries, .. }) = term.as_subterm() else {
        panic!("expected a struct literal");
    };
    assert!(matches!(entries[1], StructLitEntry::Spread(_)));

    // No tuple spread: `..` is not a term prefix, so the tuple parser cannot take it as a field, and the term fails to parse.
    assert!("(..p, 2)".parse::<Term>().is_err());
}

#[test]
fn list_literal_spread_entries() {
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };
    let nat = |n: usize| -> Term {
        Subterm::NumLit(NumLit {
            magnitude: n.into(),
            radix: Radix::Dec(n.to_string().len()),
            sign: Sign::Unmarked,
        })
        .into()
    };

    // Spreads splice anywhere, any count; plain elements stay `Elem`.
    assert_eq!(
        "[1, ..xs, 2]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::List(vec![
            ListEntry::Elem(nat(1)),
            ListEntry::Spread(name("xs")),
            ListEntry::Elem(nat(2)),
        ]))
        .into()
    );
    assert_eq!(
        "[..xs, ..ys]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::List(vec![
            ListEntry::Spread(name("xs")),
            ListEntry::Spread(name("ys")),
        ]))
        .into()
    );

    // Brackets delimit, so a list spread takes a full (spaceable) term.
    assert_eq!(
        "[.. xs]".parse::<Term>().unwrap(),
        "[..xs]".parse::<Term>().unwrap()
    );
}

#[test]
fn bin_literal_spread_segments() {
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };

    // Constant atoms are ordinary numeral terms around the spread segments: the surface keeps what was written, and lowering folds adjacent constants into packed runs.
    assert_eq!(
        r"x[0, ..xs, 1]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            vec![
                BinSegment::Atom(num_lit(0, Sign::Unmarked)),
                BinSegment::Spread(name("xs")),
                BinSegment::Atom(num_lit(1, Sign::Unmarked)),
            ]
        ))
        .into()
    );
    assert_eq!(
        r"x[0, 1, ..x, 2, 3]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            vec![
                BinSegment::Atom(num_lit(0, Sign::Unmarked)),
                BinSegment::Atom(num_lit(1, Sign::Unmarked)),
                BinSegment::Spread(name("x")),
                BinSegment::Atom(num_lit(2, Sign::Unmarked)),
                BinSegment::Atom(num_lit(3, Sign::Unmarked)),
            ]
        ))
        .into()
    );

    // A spread operand is an ordinary term: projections and absolute paths need no special grammar.
    assert_eq!(
        r"x[..hdr.bytes]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            vec![BinSegment::Spread(
                Subterm::Proj(Proj {
                    head: name("hdr"),
                    field: Field::Label("bytes".to_string()),
                })
                .into()
            )]
        ))
        .into()
    );
    let term = r"x[../std/x]".parse::<Term>().unwrap();
    let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    let BinSegment::Spread(operand) = &segments[0] else {
        panic!("expected a spread segment");
    };
    assert!(matches!(operand.as_subterm(), Subterm::Name(name) if name.is_abs()));

    // Commas delimit, so an operand that the tight grammar could only take parenthesized — an infix chain — is now written bare.
    let term = r"x[..x + y, 0x01]".parse::<Term>().unwrap();
    let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Spread(operand) if matches!(operand.as_subterm(), Subterm::Infix(_)))
    );
    assert!(
        matches!(&segments[1], BinSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );
    let term = r"x[..read()!, 0x01]".parse::<Term>().unwrap();
    let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Spread(operand) if matches!(operand.as_subterm(), Subterm::Bang(_)))
    );

    // Each grain has an empty literal, spelled like `[]` behind its grain letter.
    assert_eq!(
        "x[]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(Grain::X, vec![])).into()
    );
    assert_eq!(
        "b[]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(Grain::B, vec![])).into()
    );

    // Past the `[` the literal lexes like any other bracketed list: interior whitespace is invisible and one trailing comma is admitted.
    assert_eq!(
        r"x[ 0x00 , ..xs , 0x01 ]".parse::<Term>().unwrap(),
        r"x[0x00, ..xs, 0x01]".parse::<Term>().unwrap()
    );
    assert_eq!(
        r"x[0x00,]".parse::<Term>().unwrap(),
        r"x[0x00]".parse::<Term>().unwrap()
    );

    // Only the prefix-to-bracket junction is tight. Spaced, `b` is an ordinary binder followed by a list (leaving trailing junk here), and an identifier merely ending in the grain letter never starts a literal.
    assert!(r"b [1]".parse::<Term>().is_err());
    assert!(r"nb[1]".parse::<Term>().is_err());

    // The tight spelling and the escaped atom are both gone rather than deprecated.
    for gone in [
        r"x\00",
        r"b\1",
        r"x\",
        r"b\",
        r"x\..xs",
        r"b\.h\..t",
        r"x[\00]",
        r"b[\1]",
        r"x[\48, 0x69]",
    ] {
        assert!(gone.parse::<Term>().is_err());
    }
}

#[test]
fn bin_literal_atom_segments() {
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };

    // A bare term entry splices one generator; constant and non-constant atoms alike stay entries, in either grain.
    assert_eq!(
        r"x[72, b, 0]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            vec![
                BinSegment::Atom(num_lit(72, Sign::Unmarked)),
                BinSegment::Atom(name("b")),
                BinSegment::Atom(num_lit(0, Sign::Unmarked)),
            ]
        ))
        .into()
    );
    assert_eq!(
        r"b[1, flag, 0]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(
            Grain::B,
            vec![
                BinSegment::Atom(num_lit(1, Sign::Unmarked)),
                BinSegment::Atom(name("flag")),
                BinSegment::Atom(num_lit(0, Sign::Unmarked)),
            ]
        ))
        .into()
    );
    assert_eq!(
        r"x[b]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(Grain::X, vec![BinSegment::Atom(name("b"))])).into()
    );

    // `..` marks the spread; without it the same operand contributes a single atom.
    assert_eq!(
        r"x[..xs, b]".parse::<Term>().unwrap(),
        Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            vec![BinSegment::Spread(name("xs")), BinSegment::Atom(name("b")),]
        ))
        .into()
    );

    // An atom operand is an ordinary term, exactly as a spread operand is.
    let term = r"x[hdr.byte, 0x01]".parse::<Term>().unwrap();
    let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::Proj(_)))
    );
    assert!(
        matches!(&segments[1], BinSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );
    let term = r"x[f( x , y ), 0x01]".parse::<Term>().unwrap();
    let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::Apply(_)))
    );

    // A numeral is the only constant spelling, and it stays an `Atom` here: the surface keeps what was written — radix included — and `into_core` folds constants into packed runs.
    let term = r"x[0x48, 0x69]".parse::<Term>().unwrap();
    let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );
    assert!(
        matches!(&segments[1], BinSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );

    // `\` spells nothing anymore — in an atom position or anywhere else.
    for malformed in [
        r"x[\]", r"x[\0]", r"x[\000]", r"b[\2]", r"b[\00]", r"x[\48]", r"b[\1]",
    ] {
        assert!(malformed.parse::<Term>().is_err());
    }
}

#[test]
fn list_bits_and_bytes_spreads_round_trip() {
    // String equality pins the printer's canonical bracketed form for all three carriers, including their distinct empty literals. A constant atom prints back as a numeral in its written radix, in the numeral printer's canonical width and case. Operands print in the ordinary term style — projection and bang heads bare, parenthesized only where the grammar demands it.
    for source in [
        "[1, ..xs, 2]",
        "[..xs]",
        "x[0x0, ..xs, 0x1]",
        "x[..hdr.bytes]",
        r"x[../std/x]",
        r"x[..f(x)]",
        "x[..Io/read!.bytes]",
        r"x[..x + y]",
        "x[]",
        "b[0, ..xs, 1]",
        r"b[..bits]",
        "b[]",
        "x[0x48, b, 0x0]",
        "x[b]",
        r"x[..acc, b]",
        "x[hdr.byte]",
        "x[pick(f, a, b)]",
        "b[1, flag, 0]",
        "b[h, ..t]",
        "x[0x48, 0x69]",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }

    for removed in [r"\00", r"\0", r"\\", r"\..xs", r"[\00]"] {
        assert!(removed.parse::<Term>().is_err());
    }
}
