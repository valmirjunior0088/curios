//! Projections, named and empty tuples, and the struct literal that disambiguates from a tuple type.

use {crate::parse::test_support::*, crate::*};

#[test]
fn proj_numeric_suffix() {
    assert_eq!(
        "(r).0".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Name(Name::from(["r".to_string()])).into(),
            field: Field::Index(0),
        }))
    );
}

#[test]
fn proj_chained_suffixes() {
    assert_eq!(
        "(r).1.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Proj(Proj {
                head: Subterm::Name(Name::from(["r".to_string()])).into(),
                field: Field::Index(1),
            })
            .into(),
            field: Field::Index(0),
        }))
    );
}

#[test]
fn proj_on_name_directly() {
    assert_eq!(
        "r.2".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Name(Name::from(["r".to_string()])).into(),
            field: Field::Index(2),
        }))
    );
}

#[test]
fn proj_label_suffix() {
    assert_eq!(
        "r.status".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Name(Name::from(["r".to_string()])).into(),
            field: Field::Label("status".into()),
        }))
    );
}

#[test]
fn proj_chained_mixed_fields() {
    assert_eq!(
        "r.inner.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Proj(Proj {
                head: Subterm::Name(Name::from(["r".to_string()])).into(),
                field: Field::Label("inner".into()),
            })
            .into(),
            field: Field::Index(0),
        }))
    );
}

#[test]
fn named_tuple_single_needs_no_trailing_comma() {
    assert_eq!(
        "(a = x)".parse::<Term>().unwrap(),
        Term::from(Subterm::Tuple(Tuple {
            fields: vec![TupleField {
                label: Some("a".into()),
                func_params: None,
                value: Subterm::Name(Name::from(["x".to_string()])).into(),
            }],
        }))
    );
    // A bare parenthesized name stays a parenthesized term, not a tuple.
    assert_eq!(
        "(x)".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from(["x".to_string()])))
    );
}

#[test]
fn named_tuple_mixed_fields() {
    assert_eq!(
        "(a = x, y)".parse::<Term>().unwrap(),
        Term::from(Subterm::Tuple(Tuple {
            fields: vec![
                TupleField {
                    label: Some("a".into()),
                    func_params: None,
                    value: Subterm::Name(Name::from(["x".to_string()])).into(),
                },
                TupleField {
                    label: None,
                    func_params: None,
                    value: Subterm::Name(Name::from(["y".to_string()])).into(),
                },
            ],
        }))
    );
}

#[test]
fn empty_tuple_type() {
    assert_eq!(
        "{}".parse::<Term>().unwrap(),
        Term::from(Subterm::TupleType(TupleType { fields: vec![] }))
    );
}

#[test]
fn parse_empty_tuple() {
    assert_eq!(
        "()".parse::<Term>().unwrap(),
        Term::from(Subterm::Tuple(Tuple { fields: vec![] }))
    );
}

// `()` is a pattern wherever one is written, the mirror of the literal of the tuple of no fields, and prints back as it is read.
#[test]
fn the_tuple_of_no_fields_is_a_pattern() {
    for source in [
        "let () = u; u",
        "(()) => 1",
        "((): {}) => 1",
        "let (a, ()) = p; a",
        "match u | () => 1 end",
    ] {
        let term = source.parse::<Term>().unwrap();
        let printed = term.to_string();
        assert!(printed.contains("()"), "{source}: {printed}");
        assert_eq!(
            printed.parse::<Term>().unwrap(),
            term,
            "{source}: {printed}"
        );
    }
}

// A field's definition sugar takes a lambda's parameters, patterns among them, and prints them back as it reads them.
#[test]
fn a_field_definition_takes_patterns() {
    for (source, written) in [
        ("(g((a, b)) = a + b)", "g((a, b))"),
        ("S { g((a, _): {Nat, Nat}) = a, h(()) = 1 }", "h(())"),
        ("(g(@A, (x, y)) = x)", "g(@A, (x, y))"),
    ] {
        let term = source.parse::<Term>().unwrap();
        let printed = term.to_string();
        assert!(printed.contains(written), "{source}: {printed}");
        assert_eq!(
            printed.parse::<Term>().unwrap(),
            term,
            "{source}: {printed}"
        );
    }
}

#[test]
fn parse_one_tuple() {
    assert_eq!(
        "(x,)".parse::<Term>().unwrap(),
        Term::from(Subterm::Tuple(Tuple {
            fields: vec![TupleField {
                label: None,
                func_params: None,
                value: Subterm::Name(Name::from(["x".to_string()])).into(),
            }],
        }))
    );
}

#[test]
fn struct_literal_disambiguates_from_tuple_type() {
    // `Name { x = a }` is a struct literal; a bare `{ x : A }` stays a Σ-type.
    assert_eq!(
        "Pair { fst = a, snd = b }".parse::<Term>().unwrap(),
        Subterm::StructLit(StructLit {
            head: Name::from(["Pair".to_string()]),
            params: None,
            entries: vec![
                StructLitEntry::Field(TupleField {
                    label: Some("fst".into()),
                    func_params: None,
                    value: name("a"),
                }),
                StructLitEntry::Field(TupleField {
                    label: Some("snd".into()),
                    func_params: None,
                    value: name("b"),
                }),
            ],
        })
        .into()
    );
    // A positional single field is the newtype spelling `Str { raw }`.
    assert_eq!(
        "Str { raw }".parse::<Term>().unwrap(),
        Subterm::StructLit(StructLit {
            head: Name::from(["Str".to_string()]),
            params: None,
            entries: vec![StructLitEntry::Field(TupleField {
                label: None,
                func_params: None,
                value: name("raw"),
            })],
        })
        .into()
    );
}

// A field label is a name like any other, so a keyword may not spell one. The refusal has to be an alternative of its own: `parse_label` may only refuse a keyword uncommittedly here, since a *positional* field is a term and a term may open with a keyword — and uncommitted it was discarded for the enclosing `}`, which reported `obtained: `e`\n│  expected: `}`` for a field written `end : Nat`.
#[test]
fn a_reserved_keyword_cannot_label_a_field() {
    for source in [
        "{end : Type}",
        "{n : Type, match : Type}",
        "{use(x : Type) -> Type}",
    ] {
        let error = source.parse::<Term>().unwrap_err();
        assert!(
            error.format().contains("is a reserved keyword"),
            "{source}: {}",
            error.format()
        );
    }
}

// The refusal claims only a field the writer meant to label, which is what its introducer lookahead is for: a positional field that merely opens with a keyword keeps its own diagnosis, and one that parses stays parsed.
#[test]
fn a_positional_field_opening_with_a_keyword_is_left_alone() {
    let error = "{match b | true => , Type}".parse::<Term>().unwrap_err();
    assert!(
        !error.format().contains("reserved keyword"),
        "{}",
        error.format()
    );

    if let Err(error) = "{match b | true => Type end, Type}".parse::<Term>() {
        panic!("{}", error.format());
    }
}
