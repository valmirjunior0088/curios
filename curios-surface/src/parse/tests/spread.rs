//! List and binary literals and the spread and atom segments inside them.

use {crate::parse::test_support::*, crate::*, curios_common::Sign, curios_core::Numeral};

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
    assert_eq!(params.as_ref().map(Vec::len), Some(2));
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
            value: Numeral::Natural(n.into()),
            radix: Radix::Dec(n.to_string().len()),
        })
        .into()
    };

    // Spreads splice anywhere, any count; plain elements stay `Elem`.
    assert_eq!(
        "[1, ..xs, 2]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::List(vec![
            ListEntry::Elem(nat(1)),
            ListEntry::Spread(name("xs")),
            ListEntry::Elem(nat(2)),
        ]))
        .into()
    );
    assert_eq!(
        "[..xs, ..ys]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::List(vec![
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
fn bytes_literal_spread_segments() {
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };

    // Constant atoms are ordinary numeral terms around the spread segments: the surface keeps what was written, and lowering folds adjacent constants into packed runs.
    assert_eq!(
        r"0x[0, ..xs, 1]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Bytes(vec![
            BytesSegment::Atom(num_lit(0, Sign::Unmarked)),
            BytesSegment::Spread(name("xs")),
            BytesSegment::Atom(num_lit(1, Sign::Unmarked)),
        ]))
        .into()
    );
    assert_eq!(
        r"0x[0, 1, ..x, 2, 3]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Bytes(vec![
            BytesSegment::Atom(num_lit(0, Sign::Unmarked)),
            BytesSegment::Atom(num_lit(1, Sign::Unmarked)),
            BytesSegment::Spread(name("x")),
            BytesSegment::Atom(num_lit(2, Sign::Unmarked)),
            BytesSegment::Atom(num_lit(3, Sign::Unmarked)),
        ]))
        .into()
    );

    // A spread operand is an ordinary term: projections and absolute paths need no special grammar.
    assert_eq!(
        r"0x[..hdr.bytes]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Bytes(vec![BytesSegment::Spread(
            Subterm::Proj(Proj {
                head: name("hdr"),
                field: Field::Label("bytes".into()),
            })
            .into()
        )]))
        .into()
    );
    let term = r"0x[../std/x]".parse::<Term>().unwrap();
    let Subterm::Literal(Literal::Bytes(segments)) = term.as_subterm() else {
        panic!("expected a Bytes literal");
    };
    let BytesSegment::Spread(operand) = &segments[0] else {
        panic!("expected a spread segment");
    };
    assert!(matches!(operand.as_subterm(), Subterm::Name(name) if name.is_abs()));

    // Commas delimit, so an operand that a tight grammar could only take parenthesized — an infix chain — is written bare.
    let term = r"0x[..x + y, 0x01]".parse::<Term>().unwrap();
    let Subterm::Literal(Literal::Bytes(segments)) = term.as_subterm() else {
        panic!("expected a Bytes literal");
    };
    assert!(
        matches!(&segments[0], BytesSegment::Spread(operand) if matches!(operand.as_subterm(), Subterm::Infix(_)))
    );
    assert!(
        matches!(&segments[1], BytesSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );
    let term = r"0x[..read()!, 0x01]".parse::<Term>().unwrap();
    let Subterm::Literal(Literal::Bytes(segments)) = term.as_subterm() else {
        panic!("expected a Bytes literal");
    };
    assert!(
        matches!(&segments[0], BytesSegment::Spread(operand) if matches!(operand.as_subterm(), Subterm::Bang(_)))
    );

    // The empty literal is spelled like `[]` behind its letter.
    assert_eq!(
        "0x[]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Bytes(vec![])).into()
    );

    // Past the `[` the literal lexes like any other bracketed list: interior whitespace is invisible and one trailing comma is admitted.
    assert_eq!(
        r"0x[ 0x00 , ..xs , 0x01 ]".parse::<Term>().unwrap(),
        r"0x[0x00, ..xs, 0x01]".parse::<Term>().unwrap()
    );
    assert_eq!(
        r"0x[0x00,]".parse::<Term>().unwrap(),
        r"0x[0x00]".parse::<Term>().unwrap()
    );

    // Only the prefix-to-bracket junction is tight. Spaced, `x` is an ordinary binder followed by a list (leaving trailing junk here), an identifier merely ending in the letter never starts a literal, and `b` is a name like any other.
    assert!(r"x [1]".parse::<Term>().is_err());
    assert!(r"nx[1]".parse::<Term>().is_err());
    assert!(r"b[1]".parse::<Term>().is_err());

    // The tight spelling and the escaped atom are both gone rather than deprecated.
    for gone in [r"x\00", r"x\", r"x\..xs", r"0x[\00]", r"0x[\48, 0x69]"] {
        assert!(gone.parse::<Term>().is_err());
    }
}

#[test]
fn a_packed_literal_reports_its_own_entry_rather_than_the_form_around_it() {
    // The glued letter and bracket commit, so a bad entry or a missing `]` is the diagnosis. Left to backtrack, the letter read as a bare name and the enclosing form complained at the bracket about the `;` that follows the literal.
    for source in ["0x[0x48, )]", "0x[0, ,]", "0x[0x48, 0x69"] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(
            !report.contains("expected: `;`"),
            "{source:?} reported {report}"
        );
    }
}

#[test]
fn bytes_literal_atom_segments() {
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };

    // A bare term entry splices one generator; constant and non-constant atoms alike stay entries.
    assert_eq!(
        r"0x[72, b, 0]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Bytes(vec![
            BytesSegment::Atom(num_lit(72, Sign::Unmarked)),
            BytesSegment::Atom(name("b")),
            BytesSegment::Atom(num_lit(0, Sign::Unmarked)),
        ]))
        .into()
    );
    assert_eq!(
        r"0x[b]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Bytes(vec![BytesSegment::Atom(name("b"))])).into()
    );

    // `..` marks the spread; without it the same operand contributes a single atom.
    assert_eq!(
        r"0x[..xs, b]".parse::<Term>().unwrap(),
        Subterm::Literal(Literal::Bytes(vec![
            BytesSegment::Spread(name("xs")),
            BytesSegment::Atom(name("b")),
        ]))
        .into()
    );

    // An atom operand is an ordinary term, exactly as a spread operand is.
    let term = r"0x[hdr.byte, 0x01]".parse::<Term>().unwrap();
    let Subterm::Literal(Literal::Bytes(segments)) = term.as_subterm() else {
        panic!("expected a Bytes literal");
    };
    assert!(
        matches!(&segments[0], BytesSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::Proj(_)))
    );
    assert!(
        matches!(&segments[1], BytesSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );
    let term = r"0x[f( x , y ), 0x01]".parse::<Term>().unwrap();
    let Subterm::Literal(Literal::Bytes(segments)) = term.as_subterm() else {
        panic!("expected a Bytes literal");
    };
    assert!(
        matches!(&segments[0], BytesSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::Apply(_)))
    );

    // A numeral is the only constant spelling, and it stays an `Atom` here: the surface keeps what was written — radix included — and `into_core` folds constants into packed runs.
    let term = r"0x[0x48, 0x69]".parse::<Term>().unwrap();
    let Subterm::Literal(Literal::Bytes(segments)) = term.as_subterm() else {
        panic!("expected a Bytes literal");
    };
    assert!(
        matches!(&segments[0], BytesSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );
    assert!(
        matches!(&segments[1], BytesSegment::Atom(operand) if matches!(operand.as_subterm(), Subterm::NumLit(_)))
    );

    // `\` spells nothing — in an atom position or anywhere else.
    for malformed in [r"0x[\]", r"0x[\0]", r"0x[\000]", r"0x[\48]"] {
        assert!(malformed.parse::<Term>().is_err());
    }
}

#[test]
fn list_and_bytes_spreads_round_trip() {
    // String equality pins the printer's canonical bracketed form for both carriers, including their distinct empty literals. A constant atom prints back as a numeral in its written radix, in the numeral printer's canonical width and case. Operands print in the ordinary term style — projection and bang heads bare, parenthesized only where the grammar demands it.
    for source in [
        "[1, ..xs, 2]",
        "[..xs]",
        "0x[0x0, ..xs, 0x1]",
        "0x[..hdr.bytes]",
        r"0x[../std/x]",
        r"0x[..f(x)]",
        "0x[..Io/read!.bytes]",
        r"0x[..x + y]",
        "0x[]",
        "0x[0x48, b, 0x0]",
        "0x[b]",
        r"0x[..acc, b]",
        "0x[hdr.byte]",
        "0x[pick(f, a, b)]",
        "0x[h, ..t]",
        "0x[0x48, 0x69]",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }

    for removed in [r"\00", r"\0", r"\\", r"\..xs", r"[\00]"] {
        assert!(removed.parse::<Term>().is_err());
    }
}
