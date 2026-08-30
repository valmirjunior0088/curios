//! Concept and witness declarations, their parameter forms, and the separator syntax a parameterized witness prints.

use {crate::*, curios_utilities::Plicity};

#[test]
fn parse_concept_item() {
    // Fields: a `use` superclass edge, the signature sugar `cmp(A, A) -> Order` (kept as written — `func_params` carries the parameter list; `into_core` undoes the sugar), and a plain `name : T` field.
    let source = "\
        concept Ord(A : Type) : Type { \
            use Eql(A), \
            cmp(A, A) -> Order, \
            top : A \
        } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concepts) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };
    let concept = &concepts[0];

    assert_eq!(concept.label, "Ord");
    assert_eq!(concept.params.len(), 1);
    assert_eq!(concept.fields.len(), 3);
    // `: Type` without `pub` is a sealed (private-representation) concept.
    assert!(!concept.rep_pub);

    // The `use` field is a superclass edge — anonymous, so its label is empty (lowering mints an internal `_superN`).
    assert!(concept.fields[0].is_super);
    assert_eq!(concept.fields[0].label, "");
    assert_eq!(concept.fields[0].func_params, None);

    // The sugar field keeps its written parameter list; the annotation slot holds the output type, and only `desugared_type` builds the Π-type.
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
fn out_stays_a_valid_parameter_name() {
    let source = "concept Weird(out : Type) : Type { get : out } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Concept(concepts) = &entrypoint.module.items[0] else {
        panic!("expected a concept declaration");
    };
    let concept = &concepts[0];

    assert_eq!(concept.params.len(), 1);
    assert_eq!(concept.params[0].1, "out");
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

#[test]
fn parse_witness_item() {
    // A premised witness: an `@` binder, a `use` premise, an explicit `use <term>` fill for the concept's superclass field, and the definition sugar (`cmp(a, b) = ...`).
    let source = "\
        satisfy (@A : Type, use Ord(A)) => Ord(List(A)) { \
            use eql_list, \
            cmp(a, b) = Order/lt() \
        } u";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let TopItem::Witness(witnesses) = &entrypoint.module.items[0] else {
        panic!("expected a witness declaration");
    };
    let witness = &witnesses[0];

    assert_eq!(witness.concept, Name::from(["Ord".to_string()]));
    assert_eq!(witness.args.len(), 1);

    // The telescope: an implicit `@A` and an anonymous `use` premise.
    assert_eq!(witness.params.len(), 2);
    assert_eq!(witness.params[0].plicity, Plicity::Implicit);
    assert_eq!(witness.params[1].plicity, Plicity::Witness);

    // The definition-sugar field keeps its written parameter list; the value slot holds the body, and only the struct-literal lowering builds the lambda (via `TupleField::desugared_value`). The `use eql_list` entry fills the concept's `use`-marked field without naming it.
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
    assert_eq!(params[0].1, "a");
    assert_eq!(params[1].1, "b");
    assert!(matches!(cmp.value.as_subterm(), Subterm::Apply(_)));
}

#[test]
fn use_parameter_forms() {
    // Two anonymous `use` Π-binders, alongside `@` and plain binders.
    let TopItem::Let(item) = &"pub let f(@A : Type, use Show(A), use Eql(A), x : A) -> A = x; u"
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
    assert_eq!(params[1].label, Pattern::Binder(None)); // anonymous
    assert_eq!(params[2].plicity, Plicity::Witness);
    assert_eq!(params[2].label, Pattern::Binder(None)); // anonymous
    assert_eq!(params[3].plicity, Plicity::Explicit);
}

#[test]
fn use_argument_form() {
    // `use <term>` at a call site marks a witness argument.
    let term = "f(use dict, x)".parse::<Term>().unwrap();
    let Subterm::Apply(apply) = term.as_subterm() else {
        panic!("expected an application");
    };
    assert_eq!(apply.params[0].0, Plicity::Witness);
    assert_eq!(apply.params[1].0, Plicity::Explicit);
}

#[test]
fn witness_use_round_trip() {
    // Concept/witness declarations and `use` binders/arguments survive a print → re-parse cycle unchanged.
    for source in [
        "concept Show(A : Type) : Type { show : A } u",
        "pub concept Show(A : Type) : pub Type { show : A } u",
        "pub concept Certified(A : Type) : pub Prop { proof : A } u",
        "pub concept Ord(A : Type) : Type { use Eql(A), cmp : A } u",
        "concept Convert(A : Type, B : Type) : Type { convert : A } u",
        "satisfy Show(Nat) { show = f } u",
        "satisfy (@A : Type, use Show(A)) => Show(List(A)) { show = g } u",
        "satisfy Show(Nat) { show = f } and Show(Bool) { show = g } u",
        "satisfy Show(Nat) { show = f } and (@A : Type, use Show(A)) => Show(List(A)) { show = g } u",
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
fn a_witness_group_prints_each_member_on_its_own_and_line() {
    let source = "satisfy Show(Nat) { show = f } and Show(Bool) { show = g }\nu";
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    assert_eq!(
        entrypoint.to_string(),
        "satisfy Show(Nat) {\n    show = f,\n}\nand Show(Bool) {\n    show = g,\n}\nu"
    );
}
