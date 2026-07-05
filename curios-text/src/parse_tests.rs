use {
    super::*,
    curios_abi::{WireSignature, WireType},
};

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
                    type_: Some(
                        Subterm::FuncType(FuncType {
                            params: vec![FuncTypeParam {
                                plicity: Plicity::Explicit,
                                label: Some("x".to_string()),
                                type_: Subterm::Type.into(),
                            }],
                            output: Subterm::Type.into(),
                        })
                        .into(),
                    ),
                    body: Subterm::Func(Func {
                        params: vec![(Pattern::Binder(Some("x".to_string())), None)],
                        body: Subterm::Name(Name::from(["x".to_string()])).into(),
                    })
                    .into(),
                },
            }],
            tail: Subterm::Apply(Apply {
                head: Subterm::Name(Name::from(["id".to_string()])).into(),
                params: vec![(
                    Plicity::Explicit,
                    Subterm::Name(Name::from(["a".to_string()])).into()
                )],
            })
            .into(),
        })
        .into()
    );
}

fn num_lit(magnitude: u32, signed: bool, negative: bool) -> Term {
    Subterm::NumLit(NumLit {
        magnitude: magnitude.into(),
        radix: Radix::Dec,
        signed,
        negative,
    })
    .into()
}

#[test]
fn parse_integer_literals_are_polymorphic_num_lits() {
    // Integer literals are polymorphic `NumLit`s; the sign is optional and only
    // records whether `Nat` is still a candidate. Decimals stay monomorphic `Flt`.
    assert_eq!("42".parse::<Term>().unwrap(), num_lit(42, false, false));
    assert_eq!("+42".parse::<Term>().unwrap(), num_lit(42, true, false));
    assert_eq!("-42".parse::<Term>().unwrap(), num_lit(42, true, true));
    assert_eq!(
        "42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Flt(42.0_f32)))
    );
    assert_eq!(
        "+42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Flt(42.0_f32)))
    );
    assert_eq!(
        "-42.0".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Flt(-42.0_f32)))
    );
}

#[test]
fn parse_prim() {
    assert_eq!("42".parse::<Term>().unwrap(), num_lit(42, false, false));
    assert_eq!(
        "1.5".parse::<Term>().unwrap(),
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
fn parse_infix_precedence_and_associativity() {
    // `a + b * c` → `a + (b * c)` (× binds tighter); `a - b - c` → `(a - b) - c`
    // (left-associative); comparison binds looser than arithmetic.
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };
    let infix = |op, left, right| -> Term { Subterm::Infix(Infix { op, left, right }).into() };

    assert_eq!(
        "a + b * c".parse::<Term>().unwrap(),
        infix(
            NumOp::Add,
            name("a"),
            infix(NumOp::Mul, name("b"), name("c")),
        )
    );
    assert_eq!(
        "a - b - c".parse::<Term>().unwrap(),
        infix(
            NumOp::Sub,
            infix(NumOp::Sub, name("a"), name("b")),
            name("c"),
        )
    );
    assert_eq!(
        "a + b < c".parse::<Term>().unwrap(),
        infix(
            NumOp::Lt,
            infix(NumOp::Add, name("a"), name("b")),
            name("c"),
        )
    );
}

#[test]
fn parse_infix_requires_spaces_and_disambiguates_signs() {
    // A spaced `-` is subtraction; a glued `-` is part of a negative literal.
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };

    assert_eq!(
        "a - 42".parse::<Term>().unwrap(),
        Subterm::Infix(Infix {
            op: NumOp::Sub,
            left: name("a"),
            right: num_lit(42, false, false),
        })
        .into()
    );
    // No space ⇒ the operator is not recognised, leaving a trailing token: a
    // parse error rather than a silent reinterpretation.
    assert!("a-42".parse::<Term>().is_err());
    assert!("a +42".parse::<Term>().is_err());
    // `!=` is the not-equal operator, not a postfix bang followed by `=`.
    assert_eq!(
        "a != b".parse::<Term>().unwrap(),
        Subterm::Infix(Infix {
            op: NumOp::Neq,
            left: name("a"),
            right: name("b"),
        })
        .into()
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
fn parse_hex_literal_is_num_lit() {
    assert_eq!(
        "0xC2".parse::<Term>().unwrap(),
        Subterm::NumLit(NumLit {
            magnitude: 194usize.into(),
            radix: Radix::Hex,
            signed: false,
            negative: false,
        })
        .into()
    );
}

#[test]
fn parse_bin_literal_is_num_lit() {
    assert_eq!(
        "0b1010".parse::<Term>().unwrap(),
        Subterm::NumLit(NumLit {
            magnitude: 10usize.into(),
            radix: Radix::Bin,
            signed: false,
            negative: false,
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

#[test]
fn parse_string_literal_is_str() {
    assert_eq!(
        "\"a\"".parse::<Term>().unwrap(),
        Term::from(Subterm::Syn(Syn::Str("a".to_string())))
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
                type_: Some(Subterm::Type.into()),
                body: Subterm::Type.into(),
            },
        })]
    );
}

#[test]
fn parse_top_foreign_without_pub() {
    assert_eq!(
        "foreign frobnicate : (Nat, Bin) -> Nat;"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            is_pub: false,
            label: "frobnicate".to_string(),
            signature: WireSignature {
                params: vec![
                    ("a0".to_string(), WireType::Nat),
                    ("a1".to_string(), WireType::Bin),
                ],
                results: vec![("_".to_string(), WireType::Nat)],
            },
        })]
    );
}

#[test]
fn parse_top_foreign_with_pub() {
    assert_eq!(
        "pub foreign frobnicate : (Nat, Bin) -> Nat;"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            is_pub: true,
            label: "frobnicate".to_string(),
            signature: WireSignature {
                params: vec![
                    ("a0".to_string(), WireType::Nat),
                    ("a1".to_string(), WireType::Bin),
                ],
                results: vec![("_".to_string(), WireType::Nat)],
            },
        })]
    );
}

#[test]
fn parse_top_foreign_zero_arg() {
    assert_eq!(
        "foreign clock : Nat;".parse::<Module>().unwrap().items,
        vec![TopItem::Foreign(TopForeign {
            is_pub: false,
            label: "clock".to_string(),
            signature: WireSignature {
                params: vec![],
                results: vec![("_".to_string(), WireType::Nat)],
            },
        })]
    );
}

#[test]
fn parse_top_foreign_nested_lst() {
    assert_eq!(
        "foreign frobnicate : (Lst(Lst(Nat))) -> Bln;"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            is_pub: false,
            label: "frobnicate".to_string(),
            signature: WireSignature {
                params: vec![(
                    "a0".to_string(),
                    WireType::Lst(Box::new(WireType::Lst(Box::new(WireType::Nat)))),
                )],
                results: vec![("_".to_string(), WireType::Bln)],
            },
        })]
    );
}

#[test]
fn parse_top_foreign_rejects_non_wire_type() {
    assert!("foreign frobnicate : Bool;".parse::<Module>().is_err());
}

#[test]
fn foreign_declaration_round_trips() {
    for source in [
        "foreign frobnicate : (Nat, Bin) -> Nat;",
        "pub foreign frobnicate : (Nat, Bin) -> Nat;",
        "foreign clock : Nat;",
        "foreign frobnicate : (Lst(Lst(Nat))) -> Bln;",
    ] {
        let module = source.parse::<Module>().unwrap();

        assert_eq!(
            module.to_string().parse::<Module>().unwrap(),
            module,
            "round-trip failed for {source:?}"
        );
    }
}

#[test]
fn parse_top_let_with_pub() {
    assert_eq!(
        "pub let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(TopLet {
            is_pub: true,
            label: "x".to_string(),
            signature: LetSignature::Name {
                type_: Some(Subterm::Type.into()),
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
                    type_: Some(
                        Subterm::FuncType(FuncType {
                            params: vec![FuncTypeParam {
                                plicity: Plicity::Explicit,
                                label: Some("x".to_string()),
                                type_: Subterm::Type.into(),
                            }],
                            output: Subterm::Type.into(),
                        })
                        .into(),
                    ),
                    body: Subterm::Func(Func {
                        params: vec![(Pattern::Binder(Some("x".to_string())), None)],
                        body: Subterm::Name(Name::from(["x".to_string()])).into(),
                    })
                    .into(),
                },
            },
            TopLet {
                is_pub: false,
                label: "helper".to_string(),
                signature: LetSignature::Name {
                    type_: Some(Subterm::Type.into()),
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
                        type_: Some(Subterm::Type.into()),
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
        "use /std/{Bin, Lst};".parse::<Module>().unwrap().items,
        vec![TopItem::Use(TopUse {
            is_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![
                GroupItem::Both("Bin".to_string()),
                GroupItem::Both("Lst".to_string()),
            ]),
        })]
    );
}

#[test]
fn parse_use_brace_group_kinds() {
    assert_eq!(
        "use /std/{mod Bin, let Nat, Lst};"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Use(TopUse {
            is_pub: false,
            name: Name::new(true, Qualifier::from(["std".to_string()])),
            group: UseGroup::Named(vec![
                GroupItem::Mod("Bin".to_string()),
                GroupItem::Let("Nat".to_string()),
                GroupItem::Both("Lst".to_string()),
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
            field: Field::Index(0),
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
                field: Field::Index(1),
            })
            .into(),
            field: Field::Index(0),
        }))
    );
}

#[test]
fn parse_proj_on_name_directly() {
    assert_eq!(
        "r.2".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Name(Name::from(["r".to_string()])).into(),
            field: Field::Index(2),
        }))
    );
}

#[test]
fn parse_proj_label_suffix() {
    assert_eq!(
        "r.status".parse::<Term>().unwrap(),
        Term::from(Subterm::Proj(Proj {
            head: Subterm::Name(Name::from(["r".to_string()])).into(),
            field: Field::Label("status".to_string()),
        }))
    );
}

#[test]
fn parse_proj_chained_mixed_fields() {
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
fn parse_named_tuple_single_needs_no_trailing_comma() {
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
fn parse_named_tuple_mixed_fields() {
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
            fields: vec![TupleField {
                label: None,
                func_params: None,
                value: Subterm::Name(Name::from(["x".to_string()])).into(),
            }],
        }))
    );
}

#[test]
fn parse_top_inductive_single_variant() {
    let m = "induct Foo : Type\n| bar()\nend".parse::<Module>().unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Induct(vec![TopInduct {
            is_pub: false,
            label: "Foo".to_string(),
            params: vec![],
            indices: vec![],
            result_sort: Subterm::Type.into(),
            cases: vec![TopCase {
                label: "bar".to_string(),
                payload: vec![],
                target: None,
            }],
        }])]
    );
}

#[test]
fn parse_top_inductive_empty() {
    let m = "induct False : Type\nend".parse::<Module>().unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Induct(vec![TopInduct {
            is_pub: false,
            label: "False".to_string(),
            params: vec![],
            indices: vec![],
            result_sort: Subterm::Type.into(),
            cases: vec![],
        }])]
    );
}

#[test]
fn parse_top_inductive_multi_variant() {
    let m = "pub induct Color : Type\n| red()\n| green()\n| blue()\nend"
        .parse::<Module>()
        .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Induct(group) if group[0].cases.len() == 3 && group[0].is_pub
    ));
}

#[test]
fn parse_top_inductive_parameterized() {
    let m = "induct Result(A : Type, B : Type) : Type\n| ok(A)\n| err(B)\nend"
        .parse::<Module>()
        .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Induct(group) if group[0].params.len() == 2 && group[0].cases.len() == 2
    ));
}

#[test]
fn parse_implicit_marks_on_binders_and_arguments() {
    // `@` marks a Π-type binder implicit, anywhere in the telescope.
    let t = "(@T : Type, x : T) -> T".parse::<Term>().unwrap();
    match t.as_subterm() {
        Subterm::FuncType(ft) => {
            assert_eq!(ft.params[0].plicity, Plicity::Implicit);
            assert_eq!(ft.params[1].plicity, Plicity::Explicit);
        }
        other => panic!("expected a func type, got {other:?}"),
    }

    // ...and a call-site argument, independently of its position.
    let t = "foo(x, @Nat)".parse::<Term>().unwrap();
    match t.as_subterm() {
        Subterm::Apply(apply) => {
            assert_eq!(apply.params[0].0, Plicity::Explicit);
            assert_eq!(apply.params[1].0, Plicity::Implicit);
        }
        other => panic!("expected an apply, got {other:?}"),
    }
}

#[test]
fn parse_implicit_marks_on_let_shorthand_and_inductive_params() {
    let m = "let foo(@T : Type, x : T) -> T = x;"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Let(TopLet {
            signature: LetSignature::Func { params, .. },
            ..
        }) => {
            assert_eq!(params[0].plicity, Plicity::Implicit);
            assert_eq!(params[1].plicity, Plicity::Explicit);
        }
        other => panic!("expected a func let, got {other:?}"),
    }

    // An inductive parameter may carry `@`, making it implicit at the type
    // constructor (it is implicit at the value constructors either way).
    let m = "induct Result(@A : Type, E : Type) : Type\n| success(A)\nend"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Induct(group) => {
            assert_eq!(group[0].params[0].0, Plicity::Implicit);
            assert_eq!(group[0].params[1].0, Plicity::Explicit);
        }
        other => panic!("expected an inductive, got {other:?}"),
    }
}

#[test]
fn parse_top_inductive_and_chain() {
    let m =
        "induct Tree : Type\n| node(Forest)\nand Forest : Type\n| nil()\n| cons(Tree, Forest)\nend"
            .parse::<Module>()
            .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Induct(group) if group.len() == 2
    ));
}

#[test]
fn parse_inductive_match_nullary_and_unary() {
    assert_eq!(
        "match v : Bin\n| null() => \"null\"\n| bln(b) => b\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match::Matrix(MatrixMatch {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            motive: Some(Motive::Constant(
                Subterm::Name(Name::from(["Bin".to_string()])).into()
            )),
            arms: vec![
                MatrixArm {
                    pattern: MatchPattern::Ctor {
                        tag: "null".to_string(),
                        args: vec![],
                    },
                    body: Subterm::Syn(Syn::Str("null".to_string())).into(),
                },
                MatrixArm {
                    pattern: MatchPattern::Ctor {
                        tag: "bln".to_string(),
                        args: vec![MatchPattern::Binder("b".to_string())],
                    },
                    body: Subterm::Name(Name::from(["b".to_string()])).into(),
                },
            ],
        }))
        .into()
    );
}

#[test]
fn parse_inductive_match_multi_binder() {
    assert_eq!(
        "match v : T\n| lit(a, b) => a\nend"
            .parse::<Term>()
            .unwrap(),
        Subterm::Match(Match::Matrix(MatrixMatch {
            head: Subterm::Name(Name::from(["v".to_string()])).into(),
            motive: Some(Motive::Constant(
                Subterm::Name(Name::from(["T".to_string()])).into()
            )),
            arms: vec![MatrixArm {
                pattern: MatchPattern::Ctor {
                    tag: "lit".to_string(),
                    args: vec![
                        MatchPattern::Binder("a".to_string()),
                        MatchPattern::Binder("b".to_string()),
                    ],
                },
                body: Subterm::Name(Name::from(["a".to_string()])).into(),
            }],
        }))
        .into()
    );
}

#[test]
fn parse_match_omitted_motive() {
    // Dropping the `: motive` clause entirely yields `motive: None`; the
    // elaborator later lowers it to a fresh metavariable (sugar for `: _`).
    assert_eq!(
        "match x | foo(y) => y end".parse::<Term>().unwrap(),
        Subterm::Match(Match::Matrix(MatrixMatch {
            head: Subterm::Name(Name::from(["x".to_string()])).into(),
            motive: None,
            arms: vec![MatrixArm {
                pattern: MatchPattern::Ctor {
                    tag: "foo".to_string(),
                    args: vec![MatchPattern::Binder("y".to_string())],
                },
                body: Subterm::Name(Name::from(["y".to_string()])).into(),
            }],
        }))
        .into()
    );
}

#[test]
fn omitted_motive_round_trips() {
    let term = "match x | foo(y) => y end".parse::<Term>().unwrap();
    let printed = term.to_string();
    // An omitted motive prints back without the `: …` clause …
    assert!(!printed.contains(" : "));
    // … and re-parses to the same tree.
    assert_eq!(printed.parse::<Term>().unwrap(), term);
}

#[test]
fn at_on_a_binder_type_is_a_parse_error() {
    // Erasure is sort-driven now: the erasure axis is retired, so `@` on a
    // binder's *type* (the old erased marker) no longer parses. `@` on a *name*
    // is plicity and still parses; the two positions never collide.
    let implicit = "(@n : Nat) -> Nat".parse::<Term>().unwrap();
    match &*implicit {
        Subterm::FuncType(ft) => assert_eq!(ft.params[0].plicity, Plicity::Implicit),
        other => panic!("expected a function type, got {other:?}"),
    }

    // `@` on the type is rejected in every binder position it once marked.
    for src in ["(x : @Nat) -> Nat", "(@x : @Nat) -> Nat"] {
        assert!(src.parse::<Term>().is_err(), "{src} should not parse");
    }

    assert!(
        "let foo(n : @Nat) -> Nat = n;".parse::<Module>().is_err(),
        "@ on a def-form parameter type should not parse",
    );

    assert!(
        "induct Boxed : Type | box(ghost : @Nat) end"
            .parse::<Module>()
            .is_err(),
        "@ on an inductive payload type should not parse",
    );
}

#[test]
fn parse_hole() {
    assert_eq!("?".parse::<Term>().unwrap(), Subterm::Hole.into());
}

#[test]
fn parse_hole_as_argument() {
    let term = "id(?)".parse::<Term>().unwrap();
    match term.into_subterm() {
        Subterm::Apply(apply) => {
            assert_eq!(apply.params.len(), 1);
            assert_eq!(apply.params[0], (Plicity::Explicit, Subterm::Hole.into()));
        }
        other => panic!("expected apply, got {other:?}"),
    }
}

#[test]
fn underscore_prefixed_name_is_not_a_hole() {
    assert!(matches!(
        "_foo".parse::<Term>().unwrap().into_subterm(),
        Subterm::Name(_)
    ));
}

#[test]
fn parse_local_let_without_type() {
    // A local `let x = e` omits the type; it parses to `Name { type_: None }`,
    // and the core elaborator infers the body's type.
    assert_eq!(
        "let x = Type; x".parse::<Term>().unwrap(),
        Subterm::Let(Let {
            binder: Pattern::Binder(Some("x".to_string())),
            signature: LetSignature::Name {
                type_: None,
                body: Subterm::Type.into(),
            },
            tail: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn parse_local_let_with_type_still_works() {
    assert_eq!(
        "let x : Type = Type; x".parse::<Term>().unwrap(),
        Subterm::Let(Let {
            binder: Pattern::Binder(Some("x".to_string())),
            signature: LetSignature::Name {
                type_: Some(Subterm::Type.into()),
                body: Subterm::Type.into(),
            },
            tail: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn parse_func_with_annotation() {
    assert_eq!(
        "(x : Type) => x".parse::<Term>().unwrap(),
        Subterm::Func(Func {
            params: vec![(
                Pattern::Binder(Some("x".to_string())),
                Some(Subterm::Type.into())
            )],
            body: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn parse_func_with_mixed_annotations() {
    // Annotations are per-parameter and optional; a bare param is `None`.
    assert_eq!(
        "(x : Type, y) => x".parse::<Term>().unwrap(),
        Subterm::Func(Func {
            params: vec![
                (
                    Pattern::Binder(Some("x".to_string())),
                    Some(Subterm::Type.into())
                ),
                (Pattern::Binder(Some("y".to_string())), None),
            ],
            body: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn parse_func_without_annotation_still_works() {
    assert_eq!(
        "(x) => x".parse::<Term>().unwrap(),
        Subterm::Func(Func {
            params: vec![(Pattern::Binder(Some("x".to_string())), None)],
            body: Subterm::Name(Name::from(["x".to_string()])).into(),
        })
        .into()
    );
}

#[test]
fn top_level_let_requires_a_type() {
    // The optional-type form is local-only: a module-level `let` without a type
    // is a parse error.
    assert!("let x = Type;".parse::<Module>().is_err());
}

#[test]
fn rec_binding_requires_a_type() {
    // `rec` types cannot be inferred from their (mutually recursive) bodies, so a
    // typeless `rec` binding is a parse error — both at the top level and locally.
    assert!("rec f = Type;".parse::<Module>().is_err());
    assert!("rec f = Type; f".parse::<Term>().is_err());
}

fn name(label: &str) -> Term {
    Subterm::Name(Name::from([label.to_string()])).into()
}

#[test]
fn let_bang_is_no_longer_grammar() {
    // The `let ! = <bind>;` header is gone: `!` sequences through the `Monad`
    // concept without one. `!` is not a binder identifier, so the old form is a
    // parse error rather than a `let`.
    assert!("let ! = bind; body".parse::<Term>().is_err());
}

#[test]
fn parse_bang_suffix() {
    assert_eq!(
        "x!".parse::<Term>().unwrap(),
        Subterm::Bang(name("x")).into()
    );
}

#[test]
fn parse_multi_bang_in_apply() {
    // Each argument keeps its own `!`; the desugarer hoists them left-to-right.
    assert_eq!(
        "f(x!, y!)".parse::<Term>().unwrap(),
        Subterm::Apply(Apply {
            head: name("f"),
            params: vec![
                (Plicity::Explicit, Subterm::Bang(name("x")).into()),
                (Plicity::Explicit, Subterm::Bang(name("y")).into()),
            ],
        })
        .into()
    );
}

#[test]
fn parse_bang_in_let_binding() {
    assert_eq!(
        "let x = e!; x".parse::<Term>().unwrap(),
        Subterm::Let(Let {
            binder: Pattern::Binder(Some("x".to_string())),
            signature: LetSignature::Name {
                type_: None,
                body: Subterm::Bang(name("e")).into(),
            },
            tail: name("x"),
        })
        .into()
    );
}

#[test]
fn parse_bang_in_match_scrutinee_and_arm() {
    // A `!` in the scrutinee and a `!` inside an arm are distinct `Bang` nodes;
    // the elaborator hoists them into different regions.
    let term = "match x! | foo(z) => y! end".parse::<Term>().unwrap();
    match term.into_subterm() {
        Subterm::Match(Match::Matrix(m)) => {
            assert_eq!(m.head, Subterm::Bang(name("x")).into());
            let foo = m.arms.iter().find_map(|arm| {
                matches!(&arm.pattern, MatchPattern::Ctor { tag, .. } if tag == "foo")
                    .then_some(&arm.body)
            });
            assert_eq!(foo, Some(&Subterm::Bang(name("y")).into()));
        }
        other => panic!("expected inductive match, got {other:?}"),
    }
}

#[test]
fn bang_binds_tighter_than_application() {
    // `f(x)!` bangs the whole application; the `!` is the outermost node.
    assert_eq!(
        "f(x)!".parse::<Term>().unwrap(),
        Subterm::Bang(
            Subterm::Apply(Apply {
                head: name("f"),
                params: vec![(Plicity::Explicit, name("x"))],
            })
            .into()
        )
        .into()
    );
}

#[test]
fn bang_binds_tighter_than_projection() {
    // `p.0!` bangs the projection (`!` outermost) …
    assert_eq!(
        "p.0!".parse::<Term>().unwrap(),
        Subterm::Bang(
            Subterm::Proj(Proj {
                head: name("p"),
                field: Field::Index(0),
            })
            .into()
        )
        .into()
    );
    // … while `x!.0` projects out of the banged action (`Proj` outermost).
    assert_eq!(
        "x!.0".parse::<Term>().unwrap(),
        Subterm::Proj(Proj {
            head: Subterm::Bang(name("x")).into(),
            field: Field::Index(0),
        })
        .into()
    );
}

#[test]
fn bang_round_trips() {
    for source in ["f(x!, y!)", "p.0!", "x!.0", "let x = e!; x"] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "round-trip failed for {source:?}"
        );
    }
}

#[test]
fn parse_struct_visibility_spellings() {
    // The two orthogonal markers: the outer `pub` (`is_pub`) exports the type,
    // the kind keyword (`rep_pub`) exports the representation — `record` vs
    // `struct`. All four combinations are legal.
    for (source, is_pub, rep_pub) in [
        ("struct Foo : Type { x : Type } u", false, false),
        ("record Foo : Type { x : Type } u", false, true),
        ("pub struct Foo : Type { x : Type } u", true, false),
        ("pub record Foo : Type { x : Type } u", true, true),
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        let TopItem::Struct(s) = &entrypoint.module.items[0] else {
            panic!("expected a struct declaration for {source:?}");
        };
        assert_eq!((s.is_pub, s.rep_pub), (is_pub, rep_pub), "for {source:?}");
    }
}

#[test]
fn parse_struct_literal_disambiguates_from_tuple_type() {
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

#[test]
fn struct_round_trips() {
    // Declarations (all four visibility spellings, parameterized and
    // parameterless) and literals (inferred / pinned / hole-pinned head, named
    // and positional fields) survive a print → re-parse cycle unchanged.
    for source in [
        "struct Foo : Type { x : Type } u",
        "record Foo : Type { x : Type } u",
        "pub struct Foo : Type { x : Type } u",
        "pub record Pair(A : Type, B : Type) : Type { fst : A, snd : B } u",
        "pub record Meters : Type { Nat } u",
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        assert_eq!(
            entrypoint.to_string().parse::<Entrypoint>().unwrap(),
            entrypoint,
            "declaration round-trip failed for {source:?}"
        );
    }

    for source in [
        "Pair { fst = a, snd = b }",
        "Pair(Nat, Bin) { fst = a, snd = b }",
        "Pair(Nat, ?) { fst = a, snd = b }",
        "Str { raw }",
        "Pair { ..p }",
        "Pair { ..p, snd = b }",
        "Pair(Nat, ?) { ..p, fst = a }",
        "Ord(Nat) { ..o, use my_eql }",
        "Api { ..a, ping(x) = f(x) }",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "literal round-trip failed for {source:?}"
        );
    }
}

#[test]
fn parse_function_field_sugar_in_types() {
    // The signature sugar `label(params) -> T` is admitted by every Σ-type-
    // shaped field list — tuple types and struct/record declarations — and is
    // kept as written: the AST node carries the parameter list (`func_params`)
    // and the output type; `to_core` undoes the sugar.
    let term = "{ len(s : Str) -> Nat, x : Nat }".parse::<Term>().unwrap();
    let Subterm::TupleType(TupleType { fields }) = term.as_subterm() else {
        panic!("expected a tuple type");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].label.as_deref(), Some("len"));
    let params = fields[0].func_params.as_ref().unwrap();
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].label.as_deref(), Some("s"));
    assert!(matches!(fields[0].type_.as_subterm(), Subterm::Name(_)));
    assert!(matches!(
        fields[0].desugared_type().as_subterm(),
        Subterm::FuncType(_)
    ));
    assert_eq!(fields[1].func_params, None);

    let entrypoint = "record Api : Type { version : Nat, ping(x : Nat) -> Nat } u"
        .parse::<Entrypoint>()
        .unwrap();
    let TopItem::Struct(s) = &entrypoint.module.items[0] else {
        panic!("expected a record declaration");
    };
    assert_eq!(s.fields[0].func_params, None);
    assert!(s.fields[1].func_params.is_some());
}

#[test]
fn parse_function_field_sugar_in_values() {
    // The definition sugar `label(params) = body` is admitted by every
    // tuple-shaped field list — tuple literals and struct literals — and is
    // kept as written: the AST node carries the parameter list (`func_params`)
    // and the body; `to_core` undoes the sugar.
    let term = "(bump(x) = f(x), 3)".parse::<Term>().unwrap();
    let Subterm::Tuple(Tuple { fields }) = term.as_subterm() else {
        panic!("expected a tuple literal");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].label.as_deref(), Some("bump"));
    assert_eq!(fields[0].func_params, Some(vec![("x".to_string(), None)]),);
    assert!(matches!(fields[0].value.as_subterm(), Subterm::Apply(_)));
    assert!(matches!(
        fields[0].desugared_value().as_subterm(),
        Subterm::Func(_)
    ));

    // The one-element form needs no trailing comma — the `=` disambiguates.
    let term = "(bump(x : Nat) = f(x))".parse::<Term>().unwrap();
    let Subterm::Tuple(Tuple { fields }) = term.as_subterm() else {
        panic!("expected a tuple literal");
    };
    assert_eq!(fields.len(), 1);
    let params = fields[0].func_params.as_ref().unwrap();
    assert_eq!(params[0].0, "x");
    assert!(params[0].1.is_some());

    let term = "Api { ping(x) = f(x) }".parse::<Term>().unwrap();
    let Subterm::StructLit(StructLit { entries, .. }) = term.as_subterm() else {
        panic!("expected a struct literal");
    };
    let StructLitEntry::Field(field) = &entries[0] else {
        panic!("expected a plain field entry");
    };
    assert_eq!(field.label.as_deref(), Some("ping"));
    assert!(field.func_params.is_some());
}

#[test]
fn positional_fields_that_start_like_the_sugar_backtrack() {
    // A positional application field is not the sugar: without `->` / `=` the
    // sugared alternative backtracks and the field re-parses as a term.
    let term = "{ Lst(Nat), Nat }".parse::<Term>().unwrap();
    let Subterm::TupleType(TupleType { fields }) = term.as_subterm() else {
        panic!("expected a tuple type");
    };
    assert_eq!(fields[0].label, None);
    assert_eq!(fields[0].func_params, None);
    assert!(matches!(fields[0].type_.as_subterm(), Subterm::Apply(_)));

    let term = "(f(x), y)".parse::<Term>().unwrap();
    let Subterm::Tuple(Tuple { fields }) = term.as_subterm() else {
        panic!("expected a tuple literal");
    };
    assert_eq!(fields[0].label, None);
    assert_eq!(fields[0].func_params, None);
    assert!(matches!(fields[0].value.as_subterm(), Subterm::Apply(_)));
}

#[test]
fn function_field_sugar_round_trips() {
    // The retained sugar survives print → re-parse unchanged in every
    // position: Σ-types, record declarations, tuple literals (incl. the
    // one-element form), struct literals, concepts, and witnesses.
    for source in [
        "{ len(s : Str) -> Nat, x : Nat }",
        "(bump(x) = f(x), 3)",
        "(bump(x : Nat) = f(x))",
        "Api { ping(x) = f(x) }",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "term round-trip failed for {source:?}"
        );
    }

    for source in [
        "record Api : Type { version : Nat, ping(x : Nat) -> Nat } u",
        "concept Ord(A : Type) : Type { use Eql(A), cmp(A, A) -> Order } u",
        "satisfy Ord(Nat) { use eql_nat, cmp(a, b) = f(a, b) } u",
        "satisfy Ord(Nat) { cmp(a, b) = f(a, b) } u",
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        assert_eq!(
            entrypoint.to_string().parse::<Entrypoint>().unwrap(),
            entrypoint,
            "item round-trip failed for {source:?}"
        );
    }
}

#[test]
fn pattern_binders_round_trip() {
    // Tuple/struct destructuring patterns at `let`, lambda-parameter, and
    // function-definition-sugar-parameter position: plain names still
    // round-trip unchanged, and compound patterns (nested, field-punned, or
    // mixed with plain-name parameters) survive print → re-parse.
    for source in [
        "let x = pair; x",                        // plain name, unchanged
        "let (x, y) = pair; x",                   // tuple pattern
        "let Point { x, y } = p; x",              // struct pattern, field-punned
        "let Point { x, w = ww } = p; x",         // punned + explicit label mixed
        "let Point { loc = (x, y) } = p; x",      // tuple nested in struct
        "let (Point { x, y }, z) = pair; x",      // struct nested in tuple
        "((x, y) : Point) => x",                  // tuple-pattern lambda param
        "(Point { x, y } : Point) => x",          // struct-pattern lambda param
        "((x, y) : Point, z : Nat) => x",         // mixed pattern/plain-name params
        "let f((x, y) : Point) -> Nat = x; f(p)", // tuple-pattern func-sugar param
        // Function-definition-sugar parameters always require an explicit
        // `: T` annotation (unlike lambda parameters); a struct pattern's
        // head name is descriptive only, never load-bearing as a type.
        "let f((x, y) : Point, Point { z, w = ww } : Point) -> Nat = x + y + z + ww; f(p, q)",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "term round-trip failed for {source:?}"
        );
    }
}

#[test]
fn use_entries_are_struct_literal_only() {
    // A `use <term>` entry parses in a struct literal (a concept literal by
    // intent — non-concept heads are rejected at elaboration, not parse)...
    let term = "Ord(Nat) { use my_eql, cmp = f }".parse::<Term>().unwrap();
    let Subterm::StructLit(StructLit { entries, .. }) = term.as_subterm() else {
        panic!("expected a struct literal");
    };
    assert!(matches!(entries[0], StructLitEntry::Use(_)));
    assert!(matches!(entries[1], StructLitEntry::Field(_)));

    // ...but not in a tuple literal: `use` is reserved, so the tuple parser
    // cannot take it as a field, and the term fails to parse.
    assert!("(use my_eql, 2)".parse::<Term>().is_err());
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

    // A misplaced spread still parses — position and multiplicity are
    // rejected at elaboration, not parse (like non-concept `use` entries).
    let term = "Pair { fst = a, ..p }".parse::<Term>().unwrap();
    let Subterm::StructLit(StructLit { entries, .. }) = term.as_subterm() else {
        panic!("expected a struct literal");
    };
    assert!(matches!(entries[1], StructLitEntry::Spread(_)));

    // No tuple spread: `..` is not a term prefix, so the tuple parser
    // cannot take it as a field, and the term fails to parse.
    assert!("(..p, 2)".parse::<Term>().is_err());
}

#[test]
fn lst_literal_spread_entries() {
    let name = |n: &str| -> Term { Subterm::Name(Name::from([n.to_string()])).into() };
    let nat = |n: usize| -> Term {
        Subterm::NumLit(NumLit {
            magnitude: n.into(),
            radix: Radix::Dec,
            signed: false,
            negative: false,
        })
        .into()
    };

    // Spreads splice anywhere, any count; plain elements stay `Elem`.
    assert_eq!(
        "[1, ..xs, 2]".parse::<Term>().unwrap(),
        Subterm::Prim(Prim::Lst(vec![
            LstEntry::Elem(nat(1)),
            LstEntry::Spread(name("xs")),
            LstEntry::Elem(nat(2)),
        ]))
        .into()
    );
    assert_eq!(
        "[..xs, ..ys]".parse::<Term>().unwrap(),
        Subterm::Prim(Prim::Lst(vec![
            LstEntry::Spread(name("xs")),
            LstEntry::Spread(name("ys")),
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

    // Bytes coalesce into runs around the spread segments.
    assert_eq!(
        r"\00\..xs\01".parse::<Term>().unwrap(),
        Subterm::Prim(Prim::Bin(vec![
            BinSegment::Bytes(vec![0x00]),
            BinSegment::Spread(name("xs")),
            BinSegment::Bytes(vec![0x01]),
        ]))
        .into()
    );
    assert_eq!(
        r"\00\01\..x\02\03".parse::<Term>().unwrap(),
        Subterm::Prim(Prim::Bin(vec![
            BinSegment::Bytes(vec![0x00, 0x01]),
            BinSegment::Spread(name("x")),
            BinSegment::Bytes(vec![0x02, 0x03]),
        ]))
        .into()
    );

    // The glued operand admits projections and absolute paths.
    assert_eq!(
        r"\..hdr.bytes".parse::<Term>().unwrap(),
        Subterm::Prim(Prim::Bin(vec![BinSegment::Spread(
            Subterm::Proj(Proj {
                head: name("hdr"),
                field: Field::Label("bytes".to_string()),
            })
            .into()
        )]))
        .into()
    );
    let term = r"\../std/x".parse::<Term>().unwrap();
    let Subterm::Prim(Prim::Bin(segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    let BinSegment::Spread(operand) = &segments[0] else {
        panic!("expected a spread segment");
    };
    assert!(matches!(operand.as_subterm(), Subterm::Name(name) if name.is_abs()));

    // A call is atomic: its argument list is self-delimiting, so it glues
    // without parens — interior whitespace included — and the literal
    // continues at the raw closing paren. A glued `!` binds to the operand.
    let term = r"\..f( x , y )\01".parse::<Term>().unwrap();
    let Subterm::Prim(Prim::Bin(segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Spread(operand) if matches!(operand.as_subterm(), Subterm::Apply(_)))
    );
    assert!(matches!(&segments[1], BinSegment::Bytes(bytes) if bytes == &vec![0x01]));
    let term = r"\..read()!\01".parse::<Term>().unwrap();
    let Subterm::Prim(Prim::Bin(segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Spread(operand) if matches!(operand.as_subterm(), Subterm::Bang(_)))
    );

    // A parenthesized operand takes a full term (interior whitespace is
    // invisible), for the non-atomic shapes — and admits glued suffixes.
    let term = r"\..( f(x) )\01".parse::<Term>().unwrap();
    let Subterm::Prim(Prim::Bin(segments)) = term.as_subterm() else {
        panic!("expected a Bin literal");
    };
    assert!(
        matches!(&segments[0], BinSegment::Spread(operand) if matches!(operand.as_subterm(), Subterm::Apply(_)))
    );
    assert!(matches!(&segments[1], BinSegment::Bytes(bytes) if bytes == &vec![0x01]));

    // The empty literal stays `\\`, an empty segment list.
    assert_eq!(
        r"\\".parse::<Term>().unwrap(),
        Subterm::Prim(Prim::Bin(vec![])).into()
    );

    // TIGHT: the literal is one whitespace-free lexical unit. A spaced byte
    // after an operand is not part of the literal (and strands as trailing
    // junk here), and the operand itself must be glued to the `\..`.
    assert!(r"\..xs \01".parse::<Term>().is_err());
    assert!(r"\.. xs".parse::<Term>().is_err());
    // A reserved keyword is not a name, glued or otherwise.
    assert!(r"\..use".parse::<Term>().is_err());
}

#[test]
fn lst_and_bin_spreads_round_trip() {
    // String equality pins the printer's canonical (tight, glued) forms —
    // including `\\` for the empty Bin literal.
    for source in [
        "[1, ..xs, 2]",
        "[..xs]",
        r"\00\..xs\01",
        r"\..hdr.bytes",
        r"\../std/x",
        r"\..f(x)",
        r"\..Reader/read_line!.bytes",
        r"\..(x + y)",
        r"\\",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}

#[test]
fn field_lists_admit_a_trailing_comma() {
    // Every brace/paren field list — Σ-types, struct/record declarations,
    // tuple literals, struct literals, concepts, witnesses — admits (and
    // drops) one trailing comma after its last field.
    for (with, without) in [
        ("{ x : Nat, y : Nat, }", "{ x : Nat, y : Nat }"),
        ("(a, b,)", "(a, b)"),
        ("Pair { fst = a, snd = b, }", "Pair { fst = a, snd = b }"),
        (
            "Ord(Nat) { use w, cmp = f, }",
            "Ord(Nat) { use w, cmp = f }",
        ),
    ] {
        assert_eq!(
            with.parse::<Term>().unwrap(),
            without.parse::<Term>().unwrap(),
            "trailing comma changed the parse of {with:?}"
        );
    }

    for (with, without) in [
        (
            "record Foo : Type { x : Type, } u",
            "record Foo : Type { x : Type } u",
        ),
        (
            "concept Show(A : Type) : Type { show(A) -> Str, } u",
            "concept Show(A : Type) : Type { show(A) -> Str } u",
        ),
        (
            "satisfy Show(Nat) { show = Nat/to_str, } u",
            "satisfy Show(Nat) { show = Nat/to_str } u",
        ),
    ] {
        assert_eq!(
            with.parse::<Entrypoint>().unwrap(),
            without.parse::<Entrypoint>().unwrap(),
            "trailing comma changed the parse of {with:?}"
        );
    }

    // A one-element positional tuple's comma stays significant, and a lone or
    // doubled comma stays rejected.
    assert!(matches!(
        "(x,)".parse::<Term>().unwrap().as_subterm(),
        Subterm::Tuple(_)
    ));
    assert!("{ , }".parse::<Term>().is_err());
    assert!("(a,,)".parse::<Term>().is_err());
}

#[test]
fn inductive_match_round_trips() {
    // Constructor-arm rows survive print → re-parse: distinct tags, a nullary
    // `nil()`, and a wildcard payload binder.
    for source in [
        "match xs | cons(x, xs) => x | nil() => y end",
        "match xs | cons(x, _) => x | nil() => y end",
        "match xs | cons(a, b) => a | nil() => y end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "match arm round-trip failed for {source:?}"
        );
    }
}

#[test]
fn matrix_match_round_trips() {
    // Nested/tuple/struct match-arm patterns — the matrix pattern compiler's
    // grammar — survive print → re-parse, including the spec's own
    // motivating example (a single tupled head).
    for source in [
        // A constructor nested inside another constructor's payload.
        "match x | some(some(y)) => y | some(none()) => y | none() => y end",
        // A tuple sub-pattern nested inside a constructor's payload.
        "match x | some((a, b)) => a | none() => a end",
        // A struct sub-pattern nested inside a constructor's payload,
        // including field-punning.
        "match x | some(Point { a, b }) => a | none() => a end",
        // A mixed row: one argument concrete, the other a plain binder.
        "match x | pair(some(a), b) => a | pair(none(), b) => b end",
        // A tuple value as the match target directly (no constructor tag at
        // all), and a struct value likewise — the "structs/tuples as match
        // targets" feature.
        "match p | (a, b) => a end",
        "match p | Point { a, b } => a end",
        // The spec's own motivating example: a single tupled head, four
        // fully-enumerated rows over two independent `Option`-shaped columns.
        "match p : R\n\
         | (some(x), some(y)) => f(x, y)\n\
         | (some(x), none()) => g(x)\n\
         | (none(), some(y)) => h(y)\n\
         | (none(), none()) => d\n\
         end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "matrix match round-trip failed for {source:?}"
        );
    }
}

#[test]
fn parse_concept_item() {
    // Fields: a `use` superclass edge, the signature sugar `cmp(A, A) -> Order`
    // (kept as written — `func_params` carries the parameter list; `to_core`
    // undoes the sugar), and a plain `name : T` field.
    let source = "\
        concept Ord(A : Type) : Type { \
            use Eql(A), \
            cmp(A, A) -> Order, \
            top : A \
        } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concept) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };

    assert_eq!(concept.label, "Ord");
    assert_eq!(concept.params.len(), 1);
    assert_eq!(concept.fields.len(), 3);

    // The `use` field is a superclass edge — anonymous, so its label is empty
    // (lowering mints an internal `_superN`).
    assert!(concept.fields[0].is_super);
    assert_eq!(concept.fields[0].label, "");
    assert_eq!(concept.fields[0].func_params, None);

    // The sugar field keeps its written parameter list; the annotation slot
    // holds the output type, and only `desugared_type` builds the Π-type.
    assert!(!concept.fields[1].is_super);
    assert_eq!(concept.fields[1].label, "cmp");
    let params = concept.fields[1].func_params.as_ref().unwrap();
    assert_eq!(params.len(), 2);
    assert!(matches!(
        concept.fields[1].type_.as_subterm(),
        Subterm::Name(_)
    ));
    assert!(matches!(
        concept.fields[1].desugared_type().as_subterm(),
        Subterm::FuncType(_)
    ));

    // The plain field keeps its written type.
    assert_eq!(concept.fields[2].label, "top");
    assert_eq!(concept.fields[2].func_params, None);
    assert!(matches!(
        concept.fields[2].type_.as_subterm(),
        Subterm::Name(_)
    ));
}

#[test]
fn parse_concept_out_parameter() {
    // `out` marks an output position; unmarked parameters are inputs.
    let source = "concept Convert(A : Type, out B : Type) : Type { convert(A) -> B } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concept) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };

    assert_eq!(concept.params.len(), 2);
    assert!(!concept.params[0].is_out);
    assert_eq!(concept.params[0].label, "A");
    assert!(concept.params[1].is_out);
    assert_eq!(concept.params[1].label, "B");
}

#[test]
fn out_stays_a_valid_parameter_name() {
    // The marker needs a binder after it, so `out : Type` is a parameter
    // *named* `out`, and `out out : Type` is an `out`-marked one named `out`.
    let source = "concept Weird(out : Type, out out : Type) : Type { get : out } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concept) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };

    assert_eq!(concept.params.len(), 2);
    assert!(!concept.params[0].is_out);
    assert_eq!(concept.params[0].label, "out");
    assert!(concept.params[1].is_out);
    assert_eq!(concept.params[1].label, "out");
}

#[test]
fn parse_witness_item() {
    // A premised witness: an `@` binder, a `use` premise, an explicit
    // `use <term>` fill for the concept's superclass field, and the definition
    // sugar (`cmp(a, b) = ...`).
    let source = "\
        satisfy(@A : Type, use Ord(A)) Ord(Lst(A)) { \
            use eql_lst, \
            cmp(a, b) = Order/lt() \
        } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Witness(witness) = &entrypoint.module.items[0] else {
        panic!("expected a witness declaration");
    };

    assert_eq!(witness.concept, Name::from(["Ord".to_string()]));
    assert_eq!(witness.args.len(), 1);

    // The telescope: an implicit `@A` and an anonymous `use` premise.
    assert_eq!(witness.params.len(), 2);
    assert_eq!(witness.params[0].plicity, Plicity::Implicit);
    assert_eq!(witness.params[1].plicity, Plicity::Witness);

    // The definition-sugar field keeps its written parameter list; the value
    // slot holds the body, and only the struct-literal lowering builds the
    // lambda (via `TupleField::desugared_value`). The `use eql_lst` entry fills
    // the concept's `use`-marked field without naming it.
    assert_eq!(witness.entries.len(), 2);
    let WitnessEntry::Use(fill) = &witness.entries[0] else {
        panic!("expected a use fill");
    };
    assert!(matches!(fill.as_subterm(), Subterm::Name(_)));
    let WitnessEntry::Field(cmp) = &witness.entries[1] else {
        panic!("expected an implementation field");
    };
    assert_eq!(cmp.label, "cmp");
    let params = cmp.func_params.as_ref().unwrap();
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].0, "a");
    assert_eq!(params[1].0, "b");
    assert!(matches!(cmp.value.as_subterm(), Subterm::Apply(_)));
}

#[test]
fn parse_use_parameter_forms() {
    // Two anonymous `use` Π-binders, alongside `@` and plain binders.
    let TopItem::Let(item) = &"pub let f(@A : Type, use Show(A), use Eql(A), x : A) -> A = x; u"
        .parse::<Entrypoint>()
        .unwrap()
        .module
        .items[0]
    else {
        panic!("expected a let");
    };
    let LetSignature::Func { params, .. } = &item.signature else {
        panic!("expected function sugar");
    };
    assert_eq!(params.len(), 4);
    assert_eq!(params[0].plicity, Plicity::Implicit);
    assert_eq!(params[1].plicity, Plicity::Witness);
    assert_eq!(params[1].label, Pattern::Binder(None)); // anonymous
    assert_eq!(params[2].plicity, Plicity::Witness);
    assert_eq!(params[2].label, Pattern::Binder(None)); // anonymous
    assert_eq!(params[3].plicity, Plicity::Explicit);
}

#[test]
fn parse_use_argument_form() {
    // `use <term>` at a call site marks a witness argument.
    let term = "f(use dict, x)".parse::<Term>().unwrap();
    let Subterm::Apply(apply) = term.as_subterm() else {
        panic!("expected an application");
    };
    assert_eq!(apply.params[0].0, Plicity::Witness);
    assert_eq!(apply.params[1].0, Plicity::Explicit);
}

#[test]
fn concept_witness_use_round_trip() {
    // Concept/witness declarations and `use` binders/arguments survive a
    // print → re-parse cycle unchanged.
    for source in [
        "concept Show(A : Type) : Type { show : A } u",
        "pub concept Ord(A : Type) : Type { use Eql(A), cmp : A } u",
        "concept Convert(A : Type, out B : Type) : Type { convert : A } u",
        "satisfy Show(Nat) { show = f } u",
        "satisfy(@A : Type, use Show(A)) Show(Lst(A)) { show = g } u",
        "f(use dict, x)",
        "(@A : Type, use Show(A), x : A) -> A",
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        assert_eq!(
            entrypoint.to_string().parse::<Entrypoint>().unwrap(),
            entrypoint,
            "round-trip failed for {source:?}"
        );
    }
}
