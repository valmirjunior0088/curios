//! Inductives and structs: their variants and `and` chains, their visibility and representation spellings, and the function sugar of a field.

use {crate::*, curios_utilities::Plicity};

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
        "Pair(Nat, Bytes) { fst = a, snd = b }",
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
        "let o : Ordered(Nat) = Ordered { use eql_nat, cmp(a, b) = f(a, b) }; u",
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

// A bar in a case list begins a case and nothing else, so a constructor written with no payload list is reported after its label, where the list is missing, and not at its bar as a missing `end`.
#[test]
fn an_induct_case_with_no_payload_list_reports_where_the_list_is_missing() {
    for (source, column) in [
        ("induct Color : Type | red | green end", 27),
        ("induct Shade : Type | dark() | light end", 38),
    ] {
        let error = source.parse::<Module>().unwrap_err();
        assert!(
            error
                .format()
                .contains("a constructor is declared with its payload list"),
            "{source}: {}",
            error.format()
        );
        assert_eq!(
            error.report().span.expect("located").line_column(),
            (1, column),
            "{source}: {}",
            error.format()
        );
    }
}
