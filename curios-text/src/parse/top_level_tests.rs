//! Top-level declarations: `let` and its `and` groups, `foreign`, inductives and structs, and the visibility spellings each admits.

use {
    super::test_support::comments_of,
    crate::*,
    curios_abi::{WireLeaf, WireResults, WireSignature, WireType},
    curios_utilities::Plicity,
};

#[test]
fn top_let_without_pub() {
    assert_eq!(
        "let x : Type = Type;".parse::<Module>().unwrap().items,
        vec![TopItem::Let(vec![TopLet {
            doc: None,
            vis_pub: false,
            label: "x".into(),
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
            doc: None,
            vis_pub: false,
            label: "frobnicate".into(),
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
            doc: None,
            vis_pub: true,
            label: "frobnicate".into(),
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
            doc: None,
            vis_pub: false,
            label: "clock".into(),
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
            doc: None,
            vis_pub: false,
            label: "frobnicate".into(),
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
            doc: None,
            vis_pub: true,
            label: "x".into(),
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
            doc: None,
            vis_pub: false,
            rep_pub: false,
            label: "Foo".into(),
            params: vec![],
            indices: vec![],
            result_sort: Subterm::Type.into(),
            cases: vec![TopCase {
                doc: None,
                label: "bar".into(),
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
            doc: None,
            vis_pub: false,
            rep_pub: false,
            label: "False".into(),
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

/// The optional-type form is local-only: a module-level `let` without a type is refused by the rule, with the caret on the `=`, rather than by the sugar's `Expected '('`, which named one of the two tokens that would have served.
#[test]
fn let_requires_a_type() {
    let report = "let x = Type;".parse::<Module>().unwrap_err().format();
    assert!(
        report.contains("this definition states no type")
            && report.contains("only a local `let` may leave its type out"),
        "reported {report}"
    );
    assert!(
        report.ends_with("    1 | let x = Type;\n      |       ^"),
        "reported {report}"
    );
    assert!(!report.contains("Expected '('"), "reported {report}");
}

/// A member after `and` cannot have its type inferred from a body that may mention its siblings, so a typeless one is refused by the same rule — at the top level and locally.
#[test]
fn a_group_member_states_its_type() {
    let module = "let f : Type = Type and g = Type;"
        .parse::<Module>()
        .unwrap_err()
        .format();
    let local = "let f : Type = Type and g = Type; f"
        .parse::<Term>()
        .unwrap_err()
        .format();
    for report in [module, local] {
        assert!(
            report.contains("every member after `and` state theirs"),
            "reported {report}"
        );
        assert!(!report.contains("Expected '('"), "reported {report}");
    }
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
    assert_eq!(s.fields[0].param.func_params, None);
    assert!(s.fields[1].param.func_params.is_some());
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
        Some(vec![(Plicity::Explicit, "x".into(), None)]),
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
                doc: None,
                vis_pub: true,
                label: "id".into(),
                signature: LetSignature::Name {
                    type_: Some(Subterm::Type.into()),
                    body: Subterm::Type.into(),
                },
            },
            TopLet {
                doc: None,
                vis_pub: false,
                label: "helper".into(),
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

/// A declaration's label is spanned over the word alone, whatever whitespace follows it: the span is what a report about the declaration underlines.
#[test]
fn every_declaration_label_spans_its_word_alone() {
    let module = "pub let  x  : Type = Type;\nmod  Inner ;\ninduct  Foo  : Type | bar() end\nstruct  Pt  : Type { }\nconcept  Sh (A : Type) : Type { }\ntest  it () = Type;\nforeign  clock  : Nat;"
        .parse::<Module>()
        .unwrap();
    let spelled = module
        .items
        .iter()
        .map(|item| match item {
            TopItem::Let(items) => super::test_support::spelled(&items[0].label),
            TopItem::Mod(item) => super::test_support::spelled(&item.label),
            TopItem::Induct(items) => super::test_support::spelled(&items[0].label),
            TopItem::Struct(items) => super::test_support::spelled(&items[0].label),
            TopItem::Concept(items) => super::test_support::spelled(&items[0].label),
            TopItem::Test(item) => super::test_support::spelled(&item.label),
            TopItem::Foreign(item) => super::test_support::spelled(&item.label),
            other => panic!("unexpected item {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(spelled, ["x", "Inner", "Foo", "Pt", "Sh", "it", "clock"]);
}

/// A `-- |` block attaches to the declaration below it, line by line, with a bare `-- |` as a paragraph break; blank lines and plain comments between the block and the declaration are insignificant, and the plain comment stays a comment.
#[test]
fn a_documentation_comment_attaches_to_the_declaration_below_it() {
    let source = "-- | Doubles.\n-- |\n-- | Twice, that is.\n\n-- plain\npub let double(n: Nat) -> Nat = n + n;";
    let module = source.parse::<Module>().unwrap();
    let TopItem::Let(members) = &module.items[0] else {
        panic!("expected a let item");
    };
    let doc = members[0].doc.as_ref().expect("documented");
    assert_eq!(doc.lines, ["Doubles.", "", "Twice, that is."]);
    assert_eq!(comments_of(source), ["-- plain"]);
}

/// A block attaches to a constructor, a struct field and a concept method, and only to the member directly below it.
#[test]
fn a_documentation_comment_attaches_to_a_constructor_a_field_and_a_method() {
    let source = concat!(
        "induct Shape: pub Type\n-- | Round.\n| circle(Nat)\n| square(Nat)\nend\n",
        "struct Point: pub Type {\n    -- | Across.\n    x: Nat,\n    y: Nat,\n}\n",
        "concept Show(A: Type): pub Type {\n    -- | Renders.\n    show(A) -> Str,\n}\n",
    );
    let module = source.parse::<Module>().unwrap();

    let TopItem::Induct(inducts) = &module.items[0] else {
        panic!("expected an induct item");
    };
    assert_eq!(inducts[0].cases[0].doc.as_ref().unwrap().lines, ["Round."]);
    assert_eq!(inducts[0].cases[1].doc, None);

    let TopItem::Struct(structs) = &module.items[1] else {
        panic!("expected a struct item");
    };
    assert_eq!(
        structs[0].fields[0].doc.as_ref().unwrap().lines,
        ["Across."]
    );
    assert_eq!(structs[0].fields[1].doc, None);

    let TopItem::Concept(concepts) = &module.items[2] else {
        panic!("expected a concept item");
    };
    assert_eq!(
        concepts[0].fields[0].doc.as_ref().unwrap().lines,
        ["Renders."]
    );
}

/// A block attaches to a `mod`, a `satisfy`, a `foreign`, and a later member of an `and` group.
#[test]
fn a_documentation_comment_attaches_to_a_module_a_witness_a_foreign_and_a_later_member() {
    let source = concat!(
        "-- | Numbers.\nmod nat;\n",
        "-- | Structural.\nsatisfy Equal(Nat);\n",
        "-- | Ticks.\nforeign clock: Nat;\n",
        "let a: Nat = 1\n-- | The other.\nand b: Nat = 2;\n",
    );
    let module = source.parse::<Module>().unwrap();

    let TopItem::Mod(declaration) = &module.items[0] else {
        panic!("expected a mod item");
    };
    assert_eq!(declaration.doc.as_ref().unwrap().lines, ["Numbers."]);
    let TopItem::Witness(witnesses) = &module.items[1] else {
        panic!("expected a witness item");
    };
    assert_eq!(witnesses[0].doc.as_ref().unwrap().lines, ["Structural."]);
    let TopItem::Foreign(foreign) = &module.items[2] else {
        panic!("expected a foreign item");
    };
    assert_eq!(foreign.doc.as_ref().unwrap().lines, ["Ticks."]);
    let TopItem::Let(members) = &module.items[3] else {
        panic!("expected a let item");
    };
    assert_eq!(members[0].doc, None);
    assert_eq!(members[1].doc.as_ref().unwrap().lines, ["The other."]);
}

/// A block followed by nothing it may document is refused naming the rule, wherever it is written: at the end of a file, before a constructor list's `end`, before a field list's `}`, and before a program's tail.
#[test]
fn a_documentation_comment_before_nothing_is_refused() {
    for source in [
        "-- | lost\n",
        "let x: Nat = 1;\n-- | lost\n\n",
        "induct T: Type\n| a()\n-- | lost\nend",
        "struct P: Type {\n    x: Nat,\n    -- | lost\n}",
        "concept C(A: Type): Type {\n    -- | lost\n}",
    ] {
        let error = source.parse::<Module>().unwrap_err().format();
        assert!(
            error.contains("must immediately precede"),
            "{source}: {error}"
        );
    }

    let error = "let x: Nat = 1;\n-- | lost\nx"
        .parse::<Entrypoint>()
        .unwrap_err()
        .format();
    assert!(error.contains("must immediately precede"), "{error}");
}

/// An import has no page and a test is not part of the interface, so a block before either is refused by name.
#[test]
fn a_documentation_comment_before_use_or_test_is_refused() {
    let error = "-- | lost\nuse /std/{Nat};"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("cannot precede `use`"), "{error}");

    let error = "-- | lost\ntest t() = Test/check(true);"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("cannot precede `test`"), "{error}");
}

/// Two blocks with a gap between them and one declaration below are refused, so a stray block far above is never silently absorbed.
#[test]
fn two_documentation_comments_before_one_declaration_are_refused() {
    for source in [
        "-- | one\n\n-- | two\nlet x: Nat = 1;",
        "-- | one\n-- plain\n-- | two\nlet x: Nat = 1;",
    ] {
        let error = source.parse::<Module>().unwrap_err().format();
        assert!(
            error.contains("two documentation comments"),
            "{source}: {error}"
        );
    }
}

/// A block takes lines of its own: `-- |` after code is refused, and so is `-- |` glued to its text.
#[test]
fn a_documentation_comment_takes_a_line_of_its_own_and_its_space() {
    let error = "let x: Nat = 1; -- | trailing\nlet y: Nat = 2;"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("line of its own"), "{error}");

    let error = "-- |glued\nlet x: Nat = 1;"
        .parse::<Module>()
        .unwrap_err()
        .format();
    assert!(error.contains("with the space"), "{error}");
}

/// A block after a group's last member belongs to whatever follows it: `pub and` opens a member, while `pub let`, `pub induct` and a bare `satisfy` open the next item and end the group. The look-ahead reads past the `pub`, since a `pub` alone says nothing about which it is.
#[test]
fn a_documentation_comment_after_a_group_may_open_the_next_item() {
    let source = concat!(
        "satisfy Show(Nat) {\n    show = f,\n}\n",
        "-- | Next.\npub let x: Nat = 1;\n",
        "let a: Nat = 1\n-- | Member.\npub and b: Nat = 2;\n",
        "-- | Type.\npub induct T: Type\nend\n",
        "-- | Witness.\nsatisfy Spell(T);\n",
    );
    let module = source.parse::<Module>().unwrap();
    assert_eq!(module.items.len(), 5);

    let TopItem::Let(members) = &module.items[1] else {
        panic!("expected the documented let");
    };
    assert_eq!(members[0].doc.as_ref().unwrap().lines, ["Next."]);
    let TopItem::Let(members) = &module.items[2] else {
        panic!("expected the group");
    };
    assert!(members[1].vis_pub);
    assert_eq!(members[1].doc.as_ref().unwrap().lines, ["Member."]);
    let TopItem::Induct(members) = &module.items[3] else {
        panic!("expected the documented induct");
    };
    assert_eq!(members[0].doc.as_ref().unwrap().lines, ["Type."]);
}
