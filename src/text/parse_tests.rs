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
                    type_: Some(
                        Subterm::FuncType(FuncType {
                            params: vec![FuncTypeParam {
                                plicity: Plicity::Explicit,
                                quantity: Quantity::Omega,
                                label: Some("x".to_string()),
                                type_: Subterm::Type.into(),
                            }],
                            output: Subterm::Type.into(),
                        })
                        .into(),
                    ),
                    body: Subterm::Func(Func {
                        params: vec![(Pattern::Bind("x".to_string()), None)],
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
fn parse_string_literal_is_str() {
    assert_eq!(
        "\"a\"".parse::<Term>().unwrap(),
        Term::from(Subterm::Prim(Prim::Str("a".to_string())))
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
                                quantity: Quantity::Omega,
                                label: Some("x".to_string()),
                                type_: Subterm::Type.into(),
                            }],
                            output: Subterm::Type.into(),
                        })
                        .into(),
                    ),
                    body: Subterm::Func(Func {
                        params: vec![(Pattern::Bind("x".to_string()), None)],
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
            fields: vec![(
                Some("a".to_string()),
                Subterm::Name(Name::from(["x".to_string()])).into()
            )],
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
                (
                    Some("a".to_string()),
                    Subterm::Name(Name::from(["x".to_string()])).into()
                ),
                (None, Subterm::Name(Name::from(["y".to_string()])).into()),
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
            fields: vec![(None, Subterm::Name(Name::from(["x".to_string()])).into())],
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
            indices: vec![],
            cases: vec![TopCase {
                label: "bar".to_string(),
                payload: vec![],
                target: None,
            }],
        }])]
    );
}

#[test]
fn parse_top_union_empty() {
    let m = "union Void\nend".parse::<Module>().unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Union(vec![TopUnion {
            is_pub: false,
            label: "Void".to_string(),
            params: vec![],
            indices: vec![],
            cases: vec![],
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
fn parse_implicit_marks_on_let_shorthand_and_union_params() {
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

    // A union parameter may carry `@`, making it implicit at the type
    // constructor (it is implicit at the value constructors either way).
    let m = "union Result(@A : Type, E : Type)\n| success(A)\nend"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Union(unions) => {
            assert_eq!(unions[0].params[0].0, Plicity::Implicit);
            assert_eq!(unions[0].params[1].0, Plicity::Explicit);
        }
        other => panic!("expected a union, got {other:?}"),
    }
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
            motive: Some(Motive::Constant(
                Subterm::Name(Name::from(["Bin".to_string()])).into()
            )),
            rows: vec![
                (
                    Pattern::Variant {
                        tag: "null".to_string(),
                        args: vec![],
                    },
                    Subterm::Prim(Prim::Str("null".to_string())).into(),
                ),
                (
                    Pattern::Variant {
                        tag: "bln".to_string(),
                        args: vec![Pattern::Bind("b".to_string())],
                    },
                    Subterm::Name(Name::from(["b".to_string()])).into(),
                ),
            ],
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
            motive: Some(Motive::Constant(
                Subterm::Name(Name::from(["T".to_string()])).into()
            )),
            rows: vec![(
                Pattern::Variant {
                    tag: "lit".to_string(),
                    args: vec![
                        Pattern::Bind("a".to_string()),
                        Pattern::Bind("b".to_string())
                    ],
                },
                Subterm::Name(Name::from(["a".to_string()])).into(),
            )],
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
        Subterm::Match(Match::Union(UnionMatch {
            head: Subterm::Name(Name::from(["x".to_string()])).into(),
            motive: None,
            rows: vec![(
                Pattern::Variant {
                    tag: "foo".to_string(),
                    args: vec![Pattern::Bind("y".to_string())],
                },
                Subterm::Name(Name::from(["y".to_string()])).into(),
            )],
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
fn erased_quantity_on_type_parses_and_round_trips() {
    // `@` on the *type* marks the binder erased (quantity 0); `@` on the *name*
    // is plicity. The two positions are independent and never collide.
    let param = |src: &str| -> FuncTypeParam {
        match &*src.parse::<Term>().unwrap() {
            Subterm::FuncType(ft) => ft.params[0].clone(),
            other => panic!("expected a function type, got {other:?}"),
        }
    };

    let erased = param("(x : @Nat) -> Nat");
    assert_eq!(erased.plicity, Plicity::Explicit);
    assert_eq!(erased.quantity, Quantity::Zero);

    let implicit_erased = param("(@x : @Nat) -> Nat");
    assert_eq!(implicit_erased.plicity, Plicity::Implicit);
    assert_eq!(implicit_erased.quantity, Quantity::Zero);

    // The default is unrestricted, and an implicit-but-relevant binder keeps
    // working — the `Vec/len` pattern.
    let relevant = param("(@n : Nat) -> Nat");
    assert_eq!(relevant.plicity, Plicity::Implicit);
    assert_eq!(relevant.quantity, Quantity::Omega);

    for src in ["(x : @Nat) -> Nat", "(@x : @Nat) -> Nat", "(@n : Nat) -> Nat"] {
        let term = src.parse::<Term>().unwrap();
        assert_eq!(term.to_string().parse::<Term>().unwrap(), term, "{src}");
    }
}

#[test]
fn erased_quantity_on_def_form_param() {
    // Item 3: the combined function-definition sugar `let f(n : @Nat) -> R = …`
    // now carries `@` on the inline parameter type, not only the explicit
    // function-type signature form.
    let m = "let foo(n : @Nat, m : Nat) -> Nat = m;"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Let(TopLet {
            signature: LetSignature::Func { params, .. },
            ..
        }) => {
            assert_eq!(params[0].quantity, Quantity::Zero);
            assert_eq!(params[1].quantity, Quantity::Omega);
        }
        other => panic!("expected a func let, got {other:?}"),
    }
}

#[test]
fn erased_quantity_on_union_payload() {
    // Item 1: `@` on a constructor payload's type marks the field erased
    // (dropped from the runtime variant tuple), on named and positional binders
    // alike. Distinct from the `@`-on-the-name plicity mark.
    let m = "union Boxed | box(ghost : @Nat, val : Nat) end"
        .parse::<Module>()
        .unwrap();
    match &m.items[0] {
        TopItem::Union(unions) => {
            let payload = &unions[0].cases[0].payload;
            assert_eq!(payload[0].quantity, Quantity::Zero);
            assert_eq!(payload[1].quantity, Quantity::Omega);
        }
        other => panic!("expected a union, got {other:?}"),
    }
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
            binder: Pattern::Bind("x".to_string()),
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
            binder: Pattern::Bind("x".to_string()),
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
fn parse_struct_pattern_in_let() {
    // `let Foo { pun, rename = p } = value; tail` — a pun field binds its own
    // label; a rename binds the nested pattern. The head's trailing `{` commits
    // to the struct form, distinguishing it from a plain `let x = …` binder.
    assert_eq!(
        "let Pair { fst, snd = s } = p; fst"
            .parse::<Term>()
            .unwrap(),
        Subterm::Let(Let {
            binder: Pattern::Struct {
                head: Name::from(["Pair".to_string()]),
                fields: vec![
                    ("fst".to_string(), Pattern::Bind("fst".to_string())),
                    ("snd".to_string(), Pattern::Bind("s".to_string())),
                ],
            },
            signature: LetSignature::Name {
                type_: None,
                body: name("p"),
            },
            tail: name("fst"),
        })
        .into()
    );
}

#[test]
fn parse_func_with_annotation() {
    assert_eq!(
        "(x : Type) => x".parse::<Term>().unwrap(),
        Subterm::Func(Func {
            params: vec![(Pattern::Bind("x".to_string()), Some(Subterm::Type.into()))],
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
                (Pattern::Bind("x".to_string()), Some(Subterm::Type.into())),
                (Pattern::Bind("y".to_string()), None),
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
            params: vec![(Pattern::Bind("x".to_string()), None)],
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
fn parse_let_bang() {
    // `let ! = <bind>; <body>`: an atomic bind term, then a full-term body that runs
    // to the end of the region (no `end` terminator). Here the bind is a bare name.
    assert_eq!(
        "let ! = bind; body".parse::<Term>().unwrap(),
        Subterm::LetBang(LetBang {
            bind: name("bind"),
            body: name("body"),
        })
        .into()
    );
}

#[test]
fn parse_let_bang_partial_application_holes() {
    // The bind is typically a partial application carrying `?` holes (e.g.
    // `Parse/bind`'s leading `Type` args); they elaborate to fresh metavariables per
    // `!` site. Atomic maximal-munch ends the bind at `)`, before the `;`.
    assert_eq!(
        "let ! = Parse/bind(?, ?); body".parse::<Term>().unwrap(),
        Subterm::LetBang(LetBang {
            bind: Subterm::Apply(Apply {
                head: Subterm::Name(Name::from(["Parse".to_string(), "bind".to_string()])).into(),
                params: vec![
                    (Plicity::Explicit, Subterm::Hole.into()),
                    (Plicity::Explicit, Subterm::Hole.into()),
                ],
            })
            .into(),
            body: name("body"),
        })
        .into()
    );
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
            binder: Pattern::Bind("x".to_string()),
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
        Subterm::Match(Match::Union(m)) => {
            assert_eq!(m.head, Subterm::Bang(name("x")).into());
            let foo = m.rows.iter().find_map(|(pattern, body)| match pattern {
                Pattern::Variant { tag, .. } if tag == "foo" => Some(body),
                _ => None,
            });
            assert_eq!(foo, Some(&Subterm::Bang(name("y")).into()));
        }
        other => panic!("expected union match, got {other:?}"),
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
fn let_bang_and_bang_round_trip() {
    for source in [
        "let ! = bind; body",
        "let ! = Parse/bind(?, ?); body",
        "f(x!, y!)",
        "p.0!",
        "x!.0",
        "let x = e!; x",
    ] {
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
    // The three legal states on the single private→abstract→transparent scale,
    // distinguished by the outer (`is_pub`) and inner (`rep_pub`) `pub`.
    for (source, is_pub, rep_pub) in [
        ("struct Foo { x : Type } u", false, false),
        ("pub struct Foo { x : Type } u", true, false),
        ("pub struct Foo pub { x : Type } u", true, true),
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
            fields: vec![
                (Some("fst".to_string()), name("a")),
                (Some("snd".to_string()), name("b")),
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
            fields: vec![(None, name("raw"))],
        })
        .into()
    );
}

#[test]
fn struct_round_trips() {
    // Declarations (all three spellings, parameterized and parameterless) and
    // literals (inferred / pinned / hole-pinned head, named and positional
    // fields) survive a print → re-parse cycle unchanged.
    for source in [
        "struct Foo { x : Type } u",
        "pub struct Pair(A : Type, B : Type) pub { fst : A, snd : B } u",
        "pub struct Meters pub { Nat } u",
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
fn struct_pattern_round_trips() {
    // A struct pattern survives print → re-parse: a pun prints as the bare label,
    // a rename as `label = pattern`, and both nest.
    for source in [
        "let Pair { fst, snd } = p; fst",
        "let Pair { fst = a, snd = b } = p; a",
        "let Outer { it = Inner { a, b }, c } = o; c",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "struct pattern round-trip failed for {source:?}"
        );
    }
}

#[test]
fn matrix_pattern_round_trips() {
    // Refutable match rows survive print → re-parse: nested constructor patterns,
    // scalar literals, a `_` catch-all, and a tuple scrutinee with literal fields.
    for source in [
        "match xs | cons(x, cons(y, _)) => y | nil() => x end",
        "match xs | cons(0, _) => x | cons(n, _) => n | nil() => x end",
        "match xs | cons(x, _) => x | _ => y end",
        "match p | (true, false) => x | (_, b) => b end",
    ] {
        let term = source.parse::<Term>().unwrap();
        assert_eq!(
            term.to_string().parse::<Term>().unwrap(),
            term,
            "match pattern round-trip failed for {source:?}"
        );
    }
}
