use super::*;

#[test]
fn parse_rec_func_and_apply() {
    assert_eq!(
        "rec id : (x : Type) -> Type = (x) => x; id(a)"
            .parse::<Term>()
            .unwrap(),
        Subterm::Rec(Rec {
            items: vec![RecItem {
                label: "id".to_string(),
                signature: LetSignature::Name {
                    type_: Subterm::FuncType(FuncType {
                        params: vec![(Some("x".to_string()), Subterm::Type.into())],
                        output: Subterm::Type.into(),
                    })
                    .into(),
                    body: Subterm::Func(Func {
                        params: vec!["x".to_string()],
                        body: Subterm::Name(Name::from(["x".to_string()])).into(),
                    })
                    .into(),
                },
            }],
            tail: Subterm::Apply(Apply {
                head: Subterm::Name(Name::from(["id".to_string()])).into(),
                params: vec![Subterm::Name(Name::from(["a".to_string()])).into()],
            })
            .into(),
        })
        .into()
    );
}

#[test]
fn parse_let_tuple_and_atoms() {
    assert_eq!(
        "let x : '[hot, cold] = 'hot; (x, 'cold)"
            .parse::<Term>()
            .unwrap(),
        Subterm::Let(Let {
            label: "x".to_string(),
            signature: LetSignature::Name {
                type_: Subterm::AtomType(AtomType {
                    atoms: ["cold", "hot"].into_iter().map(Atom::from).collect(),
                })
                .into(),
                body: Subterm::Atom(Atom::from("hot")).into(),
            },
            tail: Subterm::Tuple(Tuple {
                fields: vec![
                    Subterm::Name(Name::from(["x".to_string()])).into(),
                    Subterm::Atom(Atom::from("cold")).into(),
                ],
            })
            .into(),
        })
        .into()
    );
}

#[test]
fn parse_match_single_branch() {
    assert_eq!(
        "match 'foo : k => '[foo] | 'foo => 'foo end"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match::Atom(AtomMatch {
            head: Subterm::Atom(Atom::from("foo")).into(),
            motive: Motive {
                label: Some("k".to_string()),
                body: Subterm::AtomType(AtomType {
                    atoms: [Atom::from("foo")].into_iter().collect(),
                })
                .into(),
            },
            cases: [(Atom::from("foo"), Subterm::Atom(Atom::from("foo")).into())]
                .into_iter()
                .collect(),
        }))
        .into()
    );
}

#[test]
fn parse_int_literal_and_flt_literal_are_disambiguated() {
    assert_eq!(
        "+42".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Int(42)))
    );
    assert_eq!(
        "42".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Nat(Nat::Succ(
            NatLiteral::number(42usize),
            Subterm::Prim(Prim::Nat(Nat::Zero)).into()
        ))))
    );
    assert_eq!(
        "+42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Flt(42.0_f32)))
    );
}

#[test]
fn parse_prim() {
    assert_eq!(
        "+42".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Int(42)))
    );
    assert_eq!(
        "42".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Nat(Nat::Succ(
            NatLiteral::number(42usize),
            Subterm::Prim(Prim::Nat(Nat::Zero)).into()
        ))))
    );
    assert_eq!(
        "+1.5".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Flt(1.5_f32)))
    );
    assert_eq!(
        "false".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Bln(false)))
    );
    assert_eq!(
        "true".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Bln(true)))
    );
}

#[test]
fn parse_char_literal_ascii() {
    assert_eq!(
        "'a'".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Nat(Nat::Succ(
            NatLiteral::Char('a'),
            Subterm::Prim(Prim::Nat(Nat::Zero)).into()
        ))))
    );
}

#[test]
fn parse_char_literal_escape() {
    assert_eq!(
        "'\\n'".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Nat(Nat::Succ(
            NatLiteral::Char('\n'),
            Subterm::Prim(Prim::Nat(Nat::Zero)).into()
        ))))
    );
}

#[test]
fn parse_char_literal_no_suffix_is_bin() {
    assert_eq!(
        "\"a\"".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Bin(BinLiteral::string("a"))))
    );
}

#[test]
fn parse_char_literal_multi_char_is_error() {
    assert!("'ab'".parse::<Term>().is_err());
}

#[test]
fn parse_char_literal_empty_is_error() {
    assert!("''".parse::<Term>().is_err());
}

#[test]
fn parse_top_let_without_pub() {
    assert_eq!(
        "let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(TopLet {
            is_pub: false,
            label: "x".to_string(),
            signature: LetSignature::Name {
                type_: Subterm::Type.into(),
                body: Subterm::Type.into(),
            },
        })]
    );
}

#[test]
fn parse_top_let_with_pub() {
    assert_eq!(
        "pub let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(TopLet {
            is_pub: true,
            label: "x".to_string(),
            signature: LetSignature::Name {
                type_: Subterm::Type.into(),
                body: Subterm::Type.into(),
            },
        })]
    );
}

#[test]
fn parse_top_rec_mixed_pub() {
    assert_eq!(
        r#"
            pub rec id : (x : Type) -> Type = (x) => x
            and helper : Type = Type;
        "#
        .parse::<Module>()
        .unwrap()
        .items,
        vec![TopItem::Rec(vec![
            TopLet {
                is_pub: true,
                label: "id".to_string(),
                signature: LetSignature::Name {
                    type_: Subterm::FuncType(FuncType {
                        params: vec![(Some("x".to_string()), Subterm::Type.into())],
                        output: Subterm::Type.into(),
                    })
                    .into(),
                    body: Subterm::Func(Func {
                        params: vec!["x".to_string()],
                        body: Subterm::Name(Name::from(["x".to_string()])).into(),
                    })
                    .into(),
                },
            },
            TopLet {
                is_pub: false,
                label: "helper".to_string(),
                signature: LetSignature::Name {
                    type_: Subterm::Type.into(),
                    body: Subterm::Type.into(),
                },
            },
        ])]
    );
}

#[test]
fn parse_module_roundtrip() {
    let m = r#"
        use Bar/{x};
        pub let x : Type = Type;
        rec f : Type = Type;
    "#
    .parse::<Module>()
    .unwrap();
    assert_eq!(m.items.len(), 3);
    assert!(matches!(m.items[0], TopItem::Use(_)));
    assert!(matches!(
        m.items[1],
        TopItem::Let(TopLet { is_pub: true, .. })
    ));
    assert!(matches!(m.items[2], TopItem::Rec(_)));
}

#[test]
fn parse_nested_module() {
    let m = r#"
        mod Inner
            pub let x : Type = Type;
        end
    "#
    .parse::<Module>()
    .unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Mod(TopMod {
            span: None,
            is_pub: false,
            label: "Inner".to_string(),
            module: Some(Module {
                items: vec![TopItem::Let(TopLet {
                    is_pub: true,
                    label: "x".to_string(),
                    signature: LetSignature::Name {
                        type_: Subterm::Type.into(),
                        body: Subterm::Type.into(),
                    },
                })],
            }),
        })]
    );
}

#[test]
fn parse_entrypoint_roundtrip() {
    let entrypoint = r#"
        use Foo/{x};
        use Bar/{x};
        pub rec f : Type = Type;
        let x : Type = Type;
        f
    "#
    .parse::<Entrypoint>()
    .unwrap();
    assert_eq!(entrypoint.module.items.len(), 4);
    assert!(matches!(entrypoint.module.items[0], TopItem::Use(_)));
    assert!(matches!(entrypoint.module.items[1], TopItem::Use(_)));
    assert!(matches!(entrypoint.module.items[2], TopItem::Rec(_)));
    assert!(matches!(
        entrypoint.module.items[3],
        TopItem::Let(TopLet { is_pub: false, .. })
    ));
    assert_eq!(
        entrypoint.tail,
        Term::from(Subterm::Name(Name::from(["f".to_string()])))
    );
}

#[test]
fn parse_qualified_path() {
    assert_eq!(
        "Foo/bar/baz".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from([
            "Foo".to_string(),
            "bar".to_string(),
            "baz".to_string()
        ])))
    );
}

#[test]
fn parse_type_name_as_path_segment() {
    assert_eq!(
        "Nat/double".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from([
            "Nat".to_string(),
            "double".to_string()
        ])))
    );
    assert_eq!(
        "Type/foo".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from([
            "Type".to_string(),
            "foo".to_string()
        ])))
    );
}

#[test]
fn bare_type_names_parse_as_names() {
    assert_eq!(
        "Nat".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from(["Nat".to_string()])))
    );
    assert_eq!(
        "Int".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from(["Int".to_string()])))
    );
    assert_eq!(
        "Flt".parse::<Term>().unwrap(),
        Term::from(Subterm::Name(Name::from(["Flt".to_string()])))
    );
    assert_eq!("Type".parse::<Term>().unwrap(), Term::from(Subterm::Type));
}

#[test]
fn parse_use_brace_group() {
    assert_eq!(
        "use /std/{Bin, Arr};".parse::<Module>().unwrap().items,
        vec![TopItem::Use(TopUse {
            is_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![
                GroupItem::Both("Bin".to_string()),
                GroupItem::Both("Arr".to_string()),
            ]),
        })]
    );
}

#[test]
fn parse_use_brace_group_kinds() {
    assert_eq!(
        "use /std/{mod Bin, let Nat, Arr};"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Use(TopUse {
            is_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![
                GroupItem::Mod("Bin".to_string()),
                GroupItem::Let("Nat".to_string()),
                GroupItem::Both("Arr".to_string()),
            ]),
        })]
    );
}

#[test]
fn parse_use_brace_group_empty() {
    assert_eq!(
        "use /std/{};".parse::<Module>().unwrap().items,
        vec![TopItem::Use(TopUse {
            is_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![]),
        })]
    );
}

#[test]
fn parse_use_glob() {
    assert_eq!(
        "use /sys/Nat/*;".parse::<Module>().unwrap().items,
        vec![TopItem::Use(TopUse {
            is_pub: false,
            name: Name::new(
                true,
                Qualifier::from(["sys".to_string(), "Nat".to_string()])
            ),
            group: UseGroup::Glob,
        })]
    );
}

#[test]
fn parse_proj_numeric_suffix() {
    assert_eq!(
        "(r).0".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Name(Name::from(["r".to_string()])).into(),
            index: 0,
        }))
    );
}

#[test]
fn parse_proj_chained_suffixes() {
    assert_eq!(
        "(r).1.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Proj(Proj {
                head: Subterm::Name(Name::from(["r".to_string()])).into(),
                index: 1,
            })
            .into(),
            index: 0,
        }))
    );
}

#[test]
fn parse_proj_on_name_directly() {
    assert_eq!(
        "r.2".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Name(Name::from(["r".to_string()])).into(),
            index: 2,
        }))
    );
}

#[test]
fn parse_empty_tuple_type() {
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
            fields: vec![Subterm::Name(Name::from(["x".to_string()])).into()],
        }))
    );
}

#[test]
fn parse_top_union_single_variant() {
    let m = "union Foo\n| bar()\nend".parse::<Module>().unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Union(vec![TopUnion {
            is_pub: false,
            label: "Foo".to_string(),
            params: vec![],
            cases: vec![TopCase {
                label: "bar".to_string(),
                payload_types: vec![],
            }],
        }])]
    );
}

#[test]
fn parse_top_union_multi_variant() {
    let m = "pub union Color\n| red()\n| green()\n| blue()\nend"
        .parse::<Module>()
        .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Union(unions) if unions[0].cases.len() == 3 && unions[0].is_pub
    ));
}

#[test]
fn parse_top_union_parameterized() {
    let m = "union Result(A : Type, B : Type)\n| ok(A)\n| err(B)\nend"
        .parse::<Module>()
        .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Union(unions) if unions[0].params.len() == 2 && unions[0].cases.len() == 2
    ));
}

#[test]
fn parse_top_union_and_chain() {
    let m = "union Tree\n| node(Forest)\nand Forest\n| nil()\n| cons(Tree, Forest)\nend"
        .parse::<Module>()
        .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Union(unions) if unions.len() == 2
    ));
}

#[test]
fn parse_union_match_nullary_and_unary() {
    assert_eq!(
        "match v : Bin\n| null() => \"null\"\n| bln(b) => b\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match::Union(UnionMatch {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            motive: Motive {
                label: None,
                body: Subterm::Name(Name::from(["Bin".to_string()])).into(),
            },
            cases: [
                (
                    "null".to_string(),
                    UnionCase {
                        binders: vec![],
                        body: Subterm::Prim(Prim::Bin(BinLiteral::string("null"))).into(),
                    },
                ),
                (
                    "bln".to_string(),
                    UnionCase {
                        binders: vec!["b".to_string()],
                        body: Subterm::Name(Name::from(["b".to_string()])).into(),
                    },
                ),
            ]
            .into_iter()
            .collect(),
        }))
        .into()
    );
}

#[test]
fn parse_union_match_multi_binder() {
    assert_eq!(
        "match v : T\n| lit(a, b) => a\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match::Union(UnionMatch {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            motive: Motive {
                label: None,
                body: Subterm::Name(Name::from(["T".to_string()])).into(),
            },
            cases: [(
                "lit".to_string(),
                UnionCase {
                    binders: vec!["a".to_string(), "b".to_string()],
                    body: Subterm::Name(Name::from(["a".to_string()])).into(),
                },
            )]
            .into_iter()
            .collect(),
        }))
        .into()
    );
}

#[test]
fn parse_atom_match_still_works() {
    assert_eq!(
        "match x : '[foo] | 'foo => 'foo end"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match::Atom(AtomMatch {
            head: Subterm::Name(Name::from(["x".to_string()])).into(),
            motive: Motive {
                label: None,
                body: Subterm::AtomType(AtomType {
                    atoms: [Atom::from("foo")].into_iter().collect(),
                })
                .into(),
            },
            cases: [(Atom::from("foo"), Subterm::Atom(Atom::from("foo")).into())]
                .into_iter()
                .collect(),
        }))
        .into()
    );
}
