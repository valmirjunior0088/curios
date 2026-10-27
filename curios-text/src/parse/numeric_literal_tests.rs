//! Numeric literals: polymorphic numerals, signs and the non-finite values, radixes and padded widths, and a float that overflows.

use {super::test_support::*, crate::*, curios_num::Floating, curios_utilities::Sign};

#[test]
fn integer_literals_are_polymorphic_num_lits() {
    // Integer literals are polymorphic `NumLit`s; the sign is optional and only records whether `Nat` is still a candidate. Decimals stay monomorphic `Flt`.
    assert_eq!("42".parse::<Term>().unwrap(), num_lit(42, Sign::Unmarked));
    assert_eq!("+42".parse::<Term>().unwrap(), num_lit(42, Sign::Positive));
    assert_eq!("-42".parse::<Term>().unwrap(), num_lit(42, Sign::Negative));
    assert_eq!(
        "42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(42.0))))
    );
    assert_eq!(
        "+42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(42.0))))
    );
    assert_eq!(
        "-42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(-42.0))))
    );
}

/// `5.` is not a float literal, and the refusal says what one is rather than reporting the dot as the token the enclosing form did not expect.
#[test]
fn a_numeral_with_a_dangling_dot_is_refused_by_the_rule() {
    let report = "5.".parse::<Term>().unwrap_err().format();
    assert!(
        report.contains(
            "a floating-point literal has a decimal point followed by at least one digit"
        ),
        "reported {report}"
    );
    assert!(report.ends_with("1 | 5.\n      |  ^"), "reported {report}");
    assert!("5.0".parse::<Term>().is_ok());
}

#[test]
fn rejects_a_float_literal_that_overflows_to_infinity() {
    // The model rounds an overflowing magnitude to the infinity of its sign, which is refused rather than taken — an infinity has literals of its own — and refused outright rather than backtracked into a different parse of the same digits.
    assert!("1.0e999".parse::<Term>().is_err());
    assert!("-1.0e999".parse::<Term>().is_err());
    // The refusal is the literal's own, spanning it, whether the exponent or the digit count carries it past the range: neither is left for the numeral to reread as an integer and a `.0` projection.
    let exponent = "1.0e309".parse::<Term>().unwrap_err().format();
    assert!(
        exponent.contains("Float literal overflows Flt"),
        "reported {exponent}"
    );
    assert!(
        exponent.ends_with("1 | 1.0e309\n      | ^^^^^^^"),
        "reported {exponent}"
    );
    let digits = format!("1{}.0", "0".repeat(309));
    let report = digits.parse::<Term>().unwrap_err().format();
    assert!(
        report.contains("Float literal overflows Flt"),
        "reported {report}"
    );
    assert!(format!("{digits}e-10").parse::<Term>().is_ok());
    // An exponent too large to be a decimal exponent at all is refused before any narrowing, rather than overflowing the count it is read into.
    assert!("1.0e99999999999".parse::<Term>().is_err());
    // The largest finite magnitudes still parse, and the pair below brackets the rounding threshold — `2^1024 − 2^970`, which sits *above* the largest finite value. A numeral under it narrows to that value; one over it is an overflow. Both were taken from the model's own oracle table against `str::parse::<f64>`, not from arithmetic done in prose.
    assert_eq!(
        "1.7e308".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(1.7e308))))
    );
    assert_eq!(
        "1.7976931348623157e308".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(f64::MAX))))
    );
    assert!("1.7976931348623159e308".parse::<Term>().is_err());
    // A subnormal narrows on the `2^-1074` grid rather than flushing to zero, and one below half that grid step rounds away.
    assert_eq!(
        "5.0e-324".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(5.0e-324))))
    );
    assert_eq!(
        "1.0e-324".parse::<Term>().unwrap(),
        Term::from(Subterm::Literal(Literal::Flt(Floating::from(0.0))))
    );
}

/// The values no decimal spells have signed literals of their own: the infinities, and the default NaN with its sign clear and set. Unsigned, `inf.0` is a projection of a binder named `inf`, and nothing may be glued after the `.0`.
#[test]
fn the_non_finite_values_are_signed_literals() {
    let flt = |value: Floating| Term::from(Subterm::Literal(Literal::Flt(value)));
    let negative = |value: Floating| value.copysign(Floating::infinite(true));

    assert_eq!(
        "+inf.0".parse::<Term>().unwrap(),
        flt(Floating::infinite(false))
    );
    assert_eq!(
        "-inf.0".parse::<Term>().unwrap(),
        flt(Floating::infinite(true))
    );
    assert_eq!("+nan.0".parse::<Term>().unwrap(), flt(Floating::nan()));
    assert_eq!(
        "-nan.0".parse::<Term>().unwrap(),
        flt(negative(Floating::nan()))
    );

    assert!(!matches!(
        &*"inf.0".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Flt(_))
    ));
    assert!("+inf.05".parse::<Term>().is_err());
    assert!("+nan.0x".parse::<Term>().is_err());
}

/// A sign is read once, by the number it leads, so one that touches no magnitude is told what a sign leads. Read by each magnitude for itself, it failed three alternatives at one offset and the report was the first one's word, `Expected 'inf.0'`.
#[test]
fn a_sign_that_touches_no_magnitude_is_told_what_a_sign_leads() {
    for source in [
        "let x : Int = - ;\nx",
        "let x : Int = -y;\nx",
        "let x : Int = +;\nx",
    ] {
        let error = source.parse::<Entrypoint>().unwrap_err();
        let report = error.format();
        assert!(
            report.contains("expected a numeral, `inf.0` or `nan.0` after the sign"),
            "{source:?} reported {report}"
        );
        assert_eq!(
            error.report().span.map(|span| span.start),
            source.find(['-', '+']).map(|sign| sign + 1),
            "{source:?} reported {report}"
        );
    }
    // A magnitude that begins and goes wrong is its own to report.
    let overflow = "1.0e999".parse::<Term>().unwrap_err().format();
    assert!(overflow.contains("overflows Flt"), "{overflow}");
    let signed = "-1.0e999".parse::<Term>().unwrap_err().format();
    assert!(signed.contains("overflows Flt"), "{signed}");
}

/// Where a literal is expected and a blank stands, the report says a blank: the end of the file is named at it alone, since a caret before a blank has text after it.
#[test]
fn a_literal_expected_at_a_blank_obtains_a_blank() {
    let blank = "let c = 'a ;\nc"
        .parse::<Entrypoint>()
        .unwrap_err()
        .format();
    assert!(blank.contains("Expected ''', obtained a blank"), "{blank}");

    let end = "let c = 'a".parse::<Entrypoint>().unwrap_err().format();
    assert!(
        end.contains("Expected ''', obtained 'end-of-file'"),
        "{end}"
    );
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
fn bytes_literal_is_num_lit() {
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
/// The width is kept: printed at its natural one, `0x00` would come back as `0x0`. What that would cost is a table — `x[0x00, 0x48, 0x69]` is bytes in columns, and `curios format` would silently narrow it to `x[0x0, 0x48, 0x69]`.
#[test]
fn a_padded_numeral_keeps_the_width_it_was_written_at() {
    for source in [
        "0x00",
        "0x0A",
        "0x00FF",
        "0b0001",
        "007",
        "x[0x00, 0x48, 0x69, 0x0A]",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}
