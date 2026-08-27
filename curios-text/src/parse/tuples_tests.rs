//! Projections, named and empty tuples, and the struct literal that disambiguates from a tuple type.

use crate::*;

use super::test_support::*;

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
            field: Field::Label("status".to_string()),
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
                field: Field::Label("inner".to_string()),
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
                label: Some("a".to_string()),
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
                    label: Some("a".to_string()),
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
            params: vec![],
            entries: vec![
                StructLitEntry::Field(TupleField {
                    label: Some("fst".to_string()),
                    func_params: None,
                    value: name("a"),
                }),
                StructLitEntry::Field(TupleField {
                    label: Some("snd".to_string()),
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
            params: vec![],
            entries: vec![StructLitEntry::Field(TupleField {
                label: None,
                func_params: None,
                value: name("raw"),
            })],
        })
        .into()
    );
}
