//! Concept and witness declarations, their parameter forms, and the separator syntax a parameterized witness prints.

use {crate::*, curios_utilities::Plicity};

#[test]
fn parse_concept_item() {
    // Fields: a `use` superclass edge, the signature sugar `cmp(A, A) -> Ordering` (kept as written — `func_params` carries the parameter list; `into_core` undoes the sugar), and a plain `name : T` field.
    let source = "\
        concept Ordered(A : Type) : Type { \
            use Equal(A), \
            cmp(A, A) -> Ordering, \
            top : A \
        } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concepts) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };
    let concept = &concepts[0];

    assert_eq!(concept.label, "Ordered");
    assert_eq!(concept.params.len(), 1);
    assert_eq!(concept.fields.len(), 3);
    // `: Type` without `pub` is a sealed (private-representation) concept.
    assert!(!concept.rep_pub);

    // The `use` field is a superclass edge — a type and no label (lowering mints an internal one for the record's telescope).
    assert!(concept.fields[0].is_super());
    assert_eq!(concept.fields[0].label, None);
    assert_eq!(concept.fields[0].func_params, None);

    // The sugar field keeps its written parameter list; the annotation slot holds the output type, and only `desugared_type` builds the Π-type.
    assert!(!concept.fields[1].is_super());
    assert_eq!(
        concept.fields[1].label.as_ref().map(|label| label.as_str()),
        Some("cmp")
    );
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
    assert_eq!(
        concept.fields[2].label.as_ref().map(|label| label.as_str()),
        Some("top")
    );
    assert_eq!(concept.fields[2].func_params, None);
    assert!(matches!(
        concept.fields[2].type_.as_subterm(),
        Subterm::Name(_)
    ));
}

#[test]
fn out_stays_a_valid_parameter_name() {
    let source = "concept Weird(out : Type) : Type { get : out } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concepts) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };
    let concept = &concepts[0];

    assert_eq!(concept.params.len(), 1);
    assert_eq!(concept.params[0].label.as_deref(), Some("out"));
}

#[test]
fn representation_sort_carries_visibility() {
    // `: pub Type` marks the representation transparent; the marker is independent from the name's `pub`.
    let source = "concept Show(A : Type) : pub Type { show : A } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concepts) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };
    let concept = &concepts[0];
    assert!(!concept.vis_pub);
    assert!(concept.rep_pub);
}

#[test]
fn concept_out_marker_is_rejected() {
    let source = "concept Convert(A : Type, out B : Type) : Type { convert(A) -> B } u";
    assert!(source.parse::<Entrypoint>().is_err());
}

/// A concept literal admits a `use <term>` fill and a witness body does not, so the word is refused by name there rather than read as a missing label — and the literal beside it, with the same fill, still parses.
#[test]
fn a_witness_body_refuses_a_superclass_fill_by_name() {
    let report = "satisfy Ordered(Nat) { use eql_nat, cmp(a, b) = f(a, b) } u"
        .parse::<Entrypoint>()
        .unwrap_err()
        .format();
    assert!(
        report.contains("a witness never writes a superclass slot"),
        "{report}"
    );

    "let o : Ordered(Nat) = Ordered { use eql_nat, cmp(a, b) = f(a, b) }; u"
        .parse::<Entrypoint>()
        .expect("a concept literal keeps its fill");
}

#[test]
fn parse_witness_item() {
    // A premised witness: an `@` binder, a `use` premise, and the definition sugar (`cmp(a, b) = ...`).
    let source = "\
        satisfy (@A : Type, use Ordered(A)) => Ordered(List(A)) { \
            cmp(a, b) = Ordering/lt() \
        } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Witness(witnesses) = &entrypoint.module.items[0] else {
        panic!("expected a witness declaration");
    };
    let witness = &witnesses[0];

    assert_eq!(witness.concept, Name::from(["Ordered".to_string()]));
    assert_eq!(witness.args.as_ref().map(Vec::len), Some(1));

    // The telescope: an implicit `@A` and an anonymous `use` premise.
    assert_eq!(witness.params.len(), 2);
    assert_eq!(witness.params[0].plicity, Plicity::Implicit);
    assert_eq!(witness.params[1].plicity, Plicity::Witness);

    // The definition-sugar field keeps its written parameter list; the value slot holds the body, and only the struct-literal lowering builds the lambda (via `TupleField::desugared_value`). The concept's `use`-marked field has no entry: a witness leaves it to resolution.
    let fields = witness.body.as_ref().expect("a written body");
    assert_eq!(fields.len(), 1);
    let cmp = &fields[0];
    assert_eq!(cmp.label, "cmp");
    let params = cmp.func_params.as_ref().unwrap();
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].1, "a");
    assert_eq!(params[1].1, "b");
    assert!(matches!(cmp.value.as_subterm(), Subterm::Apply(_)));
}

#[test]
fn use_parameter_forms() {
    // Two anonymous `use` Π-binders, alongside `@` and plain binders.
    let TopItem::Let(item) = &"pub let f(@A : Type, use Show(A), use Equal(A), x : A) -> A = x; u"
        .parse::<Entrypoint>()
        .unwrap()
        .module
        .items[0]
    else {
        panic!("expected a let");
    };
    let LetSignature::Func { params, .. } = &item[0].signature else {
        panic!("expected function sugar");
    };
    assert_eq!(params.len(), 4);
    assert_eq!(params[0].plicity, Plicity::Implicit);
    assert_eq!(params[1].plicity, Plicity::Witness);
    assert_eq!(params[1].binder, None); // a premise: a type and no binder
    assert_eq!(params[2].plicity, Plicity::Witness);
    assert_eq!(params[2].binder, None);
    assert_eq!(params[3].plicity, Plicity::Explicit);
}

#[test]
fn use_argument_form() {
    // `use <term>` at a call site marks a witness argument.
    let term = "f(use dict, x)".parse::<Term>().unwrap();
    let Subterm::Apply(apply) = term.as_subterm() else {
        panic!("expected an application");
    };
    assert_eq!(apply.arguments[0].plicity, Plicity::Witness);
    assert_eq!(apply.arguments[1].plicity, Plicity::Explicit);
}

#[test]
fn witness_use_round_trip() {
    // Concept/witness declarations and `use` binders/arguments survive a print → re-parse cycle unchanged.
    for source in [
        "concept Show(A : Type) : Type { show : A } u",
        "pub concept Show(A : Type) : pub Type { show : A } u",
        "pub concept Certified(A : Type) : pub Prop { proof : A } u",
        "pub concept Ordered(A : Type) : Type { use Equal(A), cmp : A } u",
        "concept Convert(A : Type, B : Type) : Type { convert : A } u",
        "satisfy Show(Nat) { show = f } u",
        "satisfy (@A : Type, use Show(A)) => Show(List(A)) { show = g } u",
        "satisfy Show(Nat) { show = f } and Show(Bool) { show = g } u",
        "satisfy Show(Nat) { show = f } and (@A : Type, use Show(A)) => Show(List(A)) { show = g } u",
        "satisfy Show(Nat) { .. } u",
        "satisfy (@A : Type, use Show(A)) => Show(List(A)) { .. } u",
        "satisfy Show(Nat) { .. } and Show(Bool) { .. } u",
        "satisfy Show(Nat) { .. } and Show(Bool) { show = g } u",
        "satisfy Show(Nat) { show = f } and (@A : Type, use Show(A)) => Show(List(A)) { .. } u",
        "f(use dict, x)",
        "(@A : Type, use Show(A), x : A) -> A",
        "struct Slot(K : Type, use Key(K), V : Type) : Type { key : K } u",
        "induct Chain(K : Type, use Key(K)) : Type | done() end u",
        "satisfy Named(@Nat) { name = n } u",
        "Box(@Nat) { value = 1 }",
        "Slot(Nat, use other, Str) { key = 2 }",
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        assert_eq!(
            entrypoint.to_string().parse::<Entrypoint>().unwrap(),
            entrypoint,
            "round-trip failed for {source:?}"
        );
    }
}

/// A declaration's parameter list takes a `use` premise beside its named parameters: a mark and a type, with no name.
#[test]
fn a_declaration_parameter_may_be_a_use_premise() {
    let source = "struct Slot(K : Type, use Key(K), @V : Type) : Type { key : K } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Struct(structs) = &entrypoint.module.items[0] else {
        panic!("expected a struct declaration");
    };
    let params = &structs[0].params;

    assert_eq!(params.len(), 3);
    assert_eq!(params[0].plicity, Plicity::Explicit);
    assert_eq!(params[0].label.as_deref(), Some("K"));
    assert_eq!(params[1].plicity, Plicity::Witness);
    assert_eq!(params[1].label, None);
    assert_eq!(params[2].plicity, Plicity::Implicit);
    assert_eq!(params[2].label.as_deref(), Some("V"));
    assert_eq!(
        entrypoint.to_string(),
        "struct Slot(K: Type, use Key(K), @V: Type): Type {\n    key: K,\n}\nu"
    );
}

/// A literal's applied head and a witness's concept application are argument lists, so each argument keeps the mark it was written with.
#[test]
fn a_literal_head_and_a_witness_head_keep_their_marks() {
    let term = "Slot(Nat, use other, @Str) { key = 2 }"
        .parse::<Term>()
        .unwrap();
    let Subterm::StructLit(literal) = term.as_subterm() else {
        panic!("expected a struct literal");
    };
    assert_eq!(
        literal
            .params
            .iter()
            .flatten()
            .map(|argument| argument.plicity)
            .collect::<Vec<_>>(),
        [Plicity::Explicit, Plicity::Witness, Plicity::Implicit],
    );

    let entrypoint = "satisfy Named(@Nat) { name = n } u"
        .parse::<Entrypoint>()
        .unwrap();
    let TopItem::Witness(witnesses) = &entrypoint.module.items[0] else {
        panic!("expected a witness declaration");
    };
    assert_eq!(
        witnesses[0]
            .args
            .iter()
            .flatten()
            .map(|argument| argument.plicity)
            .collect::<Vec<_>>(),
        [Plicity::Implicit],
    );
}

/// A list with nothing written is the application it is anywhere else, so both heads keep it apart from the bare name and print it back.
#[test]
fn a_literal_head_and_a_witness_head_keep_an_empty_list() {
    for (source, applied) in [("Box() { value = 1 }", true), ("Box { value = 1 }", false)] {
        let term = source.parse::<Term>().unwrap();
        let Subterm::StructLit(literal) = term.as_subterm() else {
            panic!("expected a struct literal");
        };
        assert_eq!(literal.params.is_some(), applied, "{source}");
        assert_eq!(term.to_string(), source);
    }

    for (source, applied) in [
        ("satisfy Marker() {}\nu", true),
        ("satisfy Marker {}\nu", false),
    ] {
        let entrypoint = source.parse::<Entrypoint>().unwrap();
        let TopItem::Witness(witnesses) = &entrypoint.module.items[0] else {
            panic!("expected a witness declaration");
        };
        assert_eq!(witnesses[0].args.is_some(), applied, "{source}");
        assert_eq!(entrypoint.to_string(), source);
    }
}

/// A declared parameter list holds at least one parameter: an empty one would declare a type its uses could not tell from one with no list.
#[test]
fn a_declared_parameter_list_is_never_empty() {
    for source in [
        "struct Unit(): Type {} u",
        "induct Empty(): Type end u",
        "concept Marker(): Type {} u",
    ] {
        assert!(
            source.parse::<Entrypoint>().is_err(),
            "unexpectedly parsed {source:?}"
        );
    }
    for source in [
        "struct Unit: Type {} u",
        "induct Empty: Type end u",
        "concept Marker: Type {} u",
    ] {
        assert!(source.parse::<Entrypoint>().is_ok(), "refused {source:?}");
    }
}

#[test]
fn parameterized_witness_prints_separator_syntax() {
    // The witness body is an always-broken brace block, so printing canonicalizes the flat source form.
    let source = "satisfy (@A : Type, use Show(A)) => Show(List(A)) { show = g }\nu";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    assert_eq!(
        entrypoint.to_string(),
        "satisfy (@A: Type, use Show(A)) => Show(List(A)) {\n    show = g,\n}\nu"
    );
}

#[test]
fn witness_telescope_requires_nonempty_separator_form() {
    for source in [
        "satisfy(@A : Type) Show(A) { show = f } u",
        "satisfy (@A : Type) -> Show(A) { show = f } u",
        "satisfy () => Show(Nat) { show = f } u",
    ] {
        assert!(
            source.parse::<Entrypoint>().is_err(),
            "unexpectedly parsed {source:?}"
        );
    }
}

#[test]
fn a_derived_witness_records_no_entries_and_prints_the_dots_on_their_own_line() {
    // A block holding `..` alone is the derived form: the AST records no entries, and the printer writes the dots back as a lone entry sits, with no comma.
    let source = "satisfy (@A : Type, use Show(A)) => Show(List(A)) { .. }\nu";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Witness(witnesses) = &entrypoint.module.items[0] else {
        panic!("expected a witness declaration");
    };
    assert_eq!(witnesses.len(), 1);
    assert!(witnesses[0].body.is_none());
    assert_eq!(witnesses[0].params.len(), 2);
    assert_eq!(
        entrypoint.to_string(),
        "satisfy (@A: Type, use Show(A)) => Show(List(A)) {\n    ..\n}\nu"
    );
}

#[test]
fn a_witness_group_mixes_derived_and_written_members() {
    // A derived member joins an `and` group exactly as a written one does, in either position: each ends at its own `}`, and the group's next member begins at `and`.
    let source = "satisfy Show(Nat) { .. } and Show(Bool) { show = g } and Show(Str) { .. }\nu";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Witness(witnesses) = &entrypoint.module.items[0] else {
        panic!("expected a witness declaration");
    };
    assert_eq!(
        witnesses
            .iter()
            .map(|w| w.body.is_some())
            .collect::<Vec<_>>(),
        [false, true, false]
    );
    assert_eq!(
        entrypoint.to_string(),
        "satisfy Show(Nat) {\n    ..\n}\nand Show(Bool) {\n    show = g,\n}\nand Show(Str) {\n    ..\n}\nu"
    );
}

#[test]
fn a_witness_takes_a_block_of_its_entries_or_of_the_dots_alone() {
    for source in [
        "satisfy Show(Nat) u",
        "satisfy Show(Nat); u",
        "satisfy Show(Nat) and Show(Bool) { .. } u",
        "satisfy Show(Nat) { show = f }; u",
        "satisfy Show(Nat) { .., } u",
        "satisfy Show(Nat) { ..base } u",
        "satisfy Show(Nat) { .., show = f } u",
        "satisfy Show(Nat) { show = f, .. } u",
    ] {
        assert!(
            source.parse::<Entrypoint>().is_err(),
            "unexpectedly parsed {source:?}"
        );
    }
}

#[test]
fn a_witness_group_prints_each_member_on_its_own_and_line() {
    let source = "satisfy Show(Nat) { show = f } and Show(Bool) { show = g }\nu";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    assert_eq!(
        entrypoint.to_string(),
        "satisfy Show(Nat) {\n    show = f,\n}\nand Show(Bool) {\n    show = g,\n}\nu"
    );
}
