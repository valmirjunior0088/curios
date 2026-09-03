//! Top-level declarations: `let` and its `and` groups, `foreign`, inductives and structs, and the visibility spellings each admits.

use {
    crate::*,
    curios_abi::{WireLeaf, WireResults, WireSignature, WireType},
    curios_utilities::Plicity,
};

#[test]
fn top_let_without_pub() {
    assert_eq!(
        "let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(vec![TopLet {
            vis_pub: false,
            label: "x".to_string(),
            signature: LetSignature::Name {
                type_: Some(Subterm::Type.into()),
                body: Subterm::Type.into(),
            },
        }])]
    );
}

#[test]
fn top_foreign_without_pub() {
    assert_eq!(
        "foreign frobnicate : (Nat, Bytes) -> Nat;"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            vis_pub: false,
            label: "frobnicate".to_string(),
            signature: WireSignature {
                params: vec![
                    ("a0".to_string(), WireType::Nat),
                    ("a1".to_string(), WireType::Bytes)
                ],
                results: WireResults::single("_".to_string(), WireType::Nat),
            },
        })]
    );
}

#[test]
fn top_foreign_with_pub() {
    assert_eq!(
        "pub foreign frobnicate : (Nat, Bytes) -> Nat;"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            vis_pub: true,
            label: "frobnicate".to_string(),
            signature: WireSignature {
                params: vec![
                    ("a0".to_string(), WireType::Nat),
                    ("a1".to_string(), WireType::Bytes)
                ],
                results: WireResults::single("_".to_string(), WireType::Nat),
            },
        })]
    );
}

#[test]
fn top_foreign_zero_arg() {
    assert_eq!(
        "foreign clock : Nat;".parse::<Module>().unwrap().items,
        vec![TopItem::Foreign(TopForeign {
            vis_pub: false,
            label: "clock".to_string(),
            signature: WireSignature {
                params: vec![],
                results: WireResults::single("_".to_string(), WireType::Nat),
            },
        })]
    );
}

/// `List` does not nest. Codegen forces and embeds exactly one level at the host boundary — a second level would hand the host rope structs where flat arrays belong — so `WireLeaf` keeps the grammar to what codegen implements, and the parser rejects the nested spelling outright rather than accepting a signature nothing can lower.
#[test]
fn top_foreign_rejects_nested_list() {
    assert!(
        "foreign frobnicate : (List(List(Nat))) -> Bool;"
            .parse::<Module>()
            .is_err()
    );
}

/// One level of `List` still parses, over each leaf the wire admits.
#[test]
fn top_foreign_list_of_leaf() {
    assert_eq!(
        "foreign frobnicate : (List(Bytes), List(Handle)) -> List(Nat);"
            .parse::<Module>()
            .unwrap()
            .items,
        vec![TopItem::Foreign(TopForeign {
            vis_pub: false,
            label: "frobnicate".to_string(),
            signature: WireSignature {
                params: vec![
                    ("a0".to_string(), WireType::List(WireLeaf::Bytes)),
                    ("a1".to_string(), WireType::List(WireLeaf::Handle))
                ],
                results: WireResults::single("_".to_string(), WireType::List(WireLeaf::Nat)),
            },
        })]
    );
}

#[test]
fn top_foreign_rejects_non_wire_type() {
    assert!("foreign frobnicate : Str;".parse::<Module>().is_err());
}

#[test]
fn foreign_declaration_round_trips() {
    for source in [
        "foreign frobnicate : (Nat, Bytes) -> Nat;",
        "pub foreign frobnicate : (Nat, Bytes) -> Nat;",
        "foreign clock : Nat;",
        "foreign frobnicate : (List(Bytes), List(Handle)) -> List(Nat);",
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
fn top_let_with_pub() {
    assert_eq!(
        "pub let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(vec![TopLet {
            vis_pub: true,
            label: "x".to_string(),
            signature: LetSignature::Name {
                type_: Some(Subterm::Type.into()),
                body: Subterm::Type.into(),
            },
        }])]
    );
}

#[test]
fn top_inductive_single_variant() {
    let m = "induct Foo : Type\n| bar()\nend".parse::<Module>().unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Induct(vec![TopInduct {
            vis_pub: false,
            rep_pub: false,
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
fn top_inductive_empty() {
    let m = "induct False : Type\nend".parse::<Module>().unwrap();
    assert_eq!(
        m.items,
        vec![TopItem::Induct(vec![TopInduct {
            vis_pub: false,
            rep_pub: false,
            label: "False".to_string(),
            params: vec![],
            indices: vec![],
            result_sort: Subterm::Type.into(),
            cases: vec![],
        }])]
    );
}

#[test]
fn top_inductive_multi_variant() {
    let m = "pub induct Color : pub Type\n| red()\n| green()\n| blue()\nend"
        .parse::<Module>()
        .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Induct(group) if group[0].cases.len() == 3 && group[0].vis_pub
    ));
}

#[test]
fn top_inductive_parameterized() {
    let m = "induct Result(A : Type, B : Type) : Type\n| ok(A)\n| err(B)\nend"
        .parse::<Module>()
        .unwrap();
    assert!(matches!(
        &m.items[0],
        TopItem::Induct(group) if group[0].params.len() == 2 && group[0].cases.len() == 2
    ));
}

#[test]
fn top_inductive_and_chain() {
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
fn let_requires_a_type() {
    // The optional-type form is local-only: a module-level `let` without a type is a parse error.
    assert!("let x = Type;".parse::<Module>().is_err());
}

#[test]
fn a_group_member_states_its_type() {
    // A member after `and` cannot have its type inferred from a body that may mention its siblings, so a typeless one is a parse error — at the top level and locally.
    assert!(
        "let f : Type = Type and g = Type;"
            .parse::<Module>()
            .is_err()
    );
    assert!(
        "let f : Type = Type and g = Type; f"
            .parse::<Term>()
            .is_err()
    );
}

#[test]
fn struct_visibility_spellings() {
    // The two orthogonal markers: the outer `pub` (`vis_pub`) exports the type, while the declaration-local `pub` (`rep_pub`) exports its representation.
    for (source, vis_pub, rep_pub) in [
        ("struct Foo : Type { x : Type } u", false, false),
        ("struct Foo : pub Type { x : Type } u", false, true),
        ("pub struct Foo : Type { x : Type } u", true, false),
        ("pub struct Foo : pub Type { x : Type } u", true, true),
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        let TopItem::Struct(structs) = &entrypoint.module.items[0] else {
            panic!("expected a struct declaration for {source:?}");
        };
        let s = &structs[0];
        assert_eq!((s.vis_pub, s.rep_pub), (vis_pub, rep_pub), "for {source:?}");
    }
}

#[test]
fn inductive_visibility_spellings() {
    for (source, vis_pub, rep_pub) in [
        ("induct U : Type | u() end u", false, false),
        ("induct U : pub Type | u() end u", false, true),
        ("pub induct U : Type | u() end u", true, false),
        ("pub induct U : pub Type | u() end u", true, true),
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        let TopItem::Induct(group) = &entrypoint.module.items[0] else {
            panic!("expected an inductive declaration for {source:?}");
        };
        assert_eq!(
            (group[0].vis_pub, group[0].rep_pub),
            (vis_pub, rep_pub),
            "for {source:?}"
        );
        assert_eq!(
            entrypoint.to_string().parse::<Entrypoint>().unwrap(),
            entrypoint,
            "round-trip failed for {source:?}"
        );
    }
}

#[test]
fn indexed_mutual_inductive_representation_sorts_round_trip() {
    let source = "pub induct Eq(@A : Type) : (x : A, y : A) -> pub Prop | refl(@z : A) : (z, z) pub and Box(A : Type) : Type | box(A) end u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Induct(group) = &entrypoint.module.items[0] else {
        panic!("expected a mutual inductive group");
    };
    assert_eq!(group.len(), 2);
    assert_eq!((group[0].vis_pub, group[0].rep_pub), (true, true));
    assert_eq!((group[1].vis_pub, group[1].rep_pub), (true, false));
    assert_eq!(
        entrypoint.to_string().parse::<Entrypoint>().unwrap(),
        entrypoint
    );
}

#[test]
fn record_is_an_identifier_and_legacy_declarations_are_not_grammar() {
    "let record : Type = Type; record"
        .parse::<Entrypoint>()
        .expect("record should be an ordinary identifier");
    assert!("record R : Type { Type } r".parse::<Entrypoint>().is_err());
}

#[test]
fn representation_pub_is_declaration_local() {
    // Concepts take the representation marker like structs and inductives; ordinary sort positions still reject it — `pub Type` is not a term.
    "pub concept C : pub Type {} c"
        .parse::<Entrypoint>()
        .expect("a concept takes a representation sort");
    assert!("let x : pub Type = Type; x".parse::<Entrypoint>().is_err());
}

#[test]
fn struct_round_trips() {
    // Declarations (all four visibility spellings, parameterized and parameterless) and literals (inferred / pinned / hole-pinned head, named and positional fields) survive a print → re-parse cycle unchanged.
    for source in [
        "struct Foo : Type { x : Type } u",
        "struct Foo : pub Type { x : Type } u",
        "pub struct Foo : Type { x : Type } u",
        "pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B } u",
        "pub struct Meters : pub Type { Nat } u",
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
        "Ordered(Nat) { ..o, use my_eql }",
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
fn function_field_sugar_in_types() {
    // The signature sugar `label(params) -> T` is admitted by every Σ-type-shaped field list — tuple types and struct declarations — and is kept as written: the AST node carries the parameter list (`func_params`) and the output type; `into_core` undoes the sugar.
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

    let entrypoint = "struct Api : pub Type { version : Nat, ping(x : Nat) -> Nat } u"
        .parse::<Entrypoint>()
        .unwrap();
    let TopItem::Struct(structs) = &entrypoint.module.items[0] else {
        panic!("expected a struct declaration");
    };
    let s = &structs[0];
    assert_eq!(s.fields[0].func_params, None);
    assert!(s.fields[1].func_params.is_some());
}

#[test]
fn function_field_sugar_in_values() {
    // The definition sugar `label(params) = body` is admitted by every tuple-shaped field list — tuple literals and struct literals — and is kept as written: the AST node carries the parameter list (`func_params`) and the body; `into_core` undoes the sugar.
    let term = "(bump(x) = f(x), 3)".parse::<Term>().unwrap();
    let Subterm::Tuple(Tuple { fields }) = term.as_subterm() else {
        panic!("expected a tuple literal");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].label.as_deref(), Some("bump"));
    assert_eq!(
        fields[0].func_params,
        Some(vec![(Plicity::Explicit, "x".to_string(), None)]),
    );
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
    assert_eq!(params[0].0, Plicity::Explicit);
    assert_eq!(params[0].1, "x");
    assert!(params[0].2.is_some());

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
    // A positional application field is not the sugar: without `->` / `=` the sugared alternative backtracks and the field re-parses as a term.
    let term = "{ List(Nat), Nat }".parse::<Term>().unwrap();
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
    // The retained sugar survives print → re-parse unchanged in every position: Σ-types, struct declarations, tuple literals (incl. the one-element form), struct literals, concepts, and witnesses.
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
        "struct Api : pub Type { version : Nat, ping(x : Nat) -> Nat } u",
        "concept Ordered(A : Type) : Type { use Equal(A), cmp(A, A) -> Ordering } u",
        "satisfy Ordered(Nat) { use eql_nat, cmp(a, b) = f(a, b) } u",
        "satisfy Ordered(Nat) { cmp(a, b) = f(a, b) } u",
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
fn top_let_group_mixed_pub() {
    assert_eq!(
        r#"
            pub let id : Type = Type
            and helper : Type = Type;
        "#
        .parse::<Module>()
        .unwrap()
        .items,
        vec![TopItem::Let(vec![
            TopLet {
                vis_pub: true,
                label: "id".to_string(),
                signature: LetSignature::Name {
                    type_: Some(Subterm::Type.into()),
                    body: Subterm::Type.into(),
                },
            },
            TopLet {
                vis_pub: false,
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
fn rec_is_an_ordinary_identifier() {
    // Not a keyword any more, so it names a binding like any other word.
    let entrypoint = "let rec : Type = Type;\nrec".parse::<Entrypoint>().unwrap();
    assert!(matches!(&entrypoint.module.items[0], TopItem::Let(items) if items[0].label == "rec"));
}

#[test]
fn a_test_declaration_parses_and_round_trips() {
    let source = "test the_answer_holds() = Test/check(42 == 42);";
    let module = source.parse::<Module>().unwrap();
    let [TopItem::Test(test)] = module.items.as_slice() else {
        panic!("expected one test item, got {:?}", module.items);
    };
    assert_eq!(test.label, "the_answer_holds");
    let printed = module.to_string();
    assert_eq!(
        printed.trim(),
        "test the_answer_holds() =\n    Test/check(42 == 42);"
    );
    assert_eq!(printed.parse::<Module>().unwrap().items, module.items);
}

#[test]
fn a_parameterized_test_declaration_parses_and_round_trips() {
    // The parentheses hold the telescope a `let`'s signature holds — a property's parameters — kept verbatim and printed as a signature's are.
    let source = "test add_commutes(n: Nat, m: Nat) = Test/check(n + m == m + n);";
    let module = source.parse::<Module>().unwrap();
    let [TopItem::Test(test)] = module.items.as_slice() else {
        panic!("expected one test item, got {:?}", module.items);
    };
    assert_eq!(test.params.len(), 2);
    let printed = module.to_string();
    assert_eq!(
        printed.trim(),
        "test add_commutes(n: Nat, m: Nat) =\n    Test/check(n + m == m + n);"
    );
    assert_eq!(printed.parse::<Module>().unwrap().items, module.items);
}

#[test]
fn a_test_takes_no_pub_and_stays_a_contextual_word() {
    // The name is a report line, not an export.
    assert!(
        "pub test t() = Test/check(true);"
            .parse::<Module>()
            .is_err()
    );
    // `test` stays a contextual word everywhere else.
    assert!("let test : Type = Type;".parse::<Module>().is_ok());
    assert!("test(1)".parse::<Entrypoint>().is_ok());
    assert!(
        "let test : Nat = 1;\ntest(test)"
            .parse::<Entrypoint>()
            .is_ok()
    );
}
