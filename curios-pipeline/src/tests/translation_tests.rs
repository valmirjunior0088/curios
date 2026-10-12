//! A term a report shows reads as its reader wrote it: a declaration's type and body, as elaboration left them, translated back to the surface tree where its reader stands and printed by the printer that formats source.
//!
//! A program per form. What is expected is the program's own text wherever elaboration adds nothing a reader writes, and the fuller form where it does: an argument the declaration marks `@` that the program left to inference is written, since the term holds it.

use {
    super::test_support::*,
    curios_core::{Global, Item, ReaderPosition, build_rename, display_names},
    curios_elab::Reading,
    curios_prelude::with_prelude,
    curios_text::{SYNTAX, Translation},
    curios_unit::Predecessors,
    curios_utilities::Qualifier,
    std::rc::Rc,
};

/// The type and the body `name` elaborates to in `source`, compiled as the unit `/lib`, each as a reader standing in that declaration is shown it. A term the translation declines fails the test: what is held here is the translation, and the compiler's own print of a term is no reading of it.
fn shown(source: &str, name: &str) -> (String, String) {
    let unit = unit_of(source);

    with_prelude(|prelude| {
        let predecessors = Predecessors::over(prelude);
        let lexicons = predecessors.lexicons(&SYNTAX);
        let spellings = Rc::new(unit.text().spellings(&predecessors.text()));
        let reading = Reading {
            module: unit.core(),
            scope: &lexicons,
            syntax: &SYNTAX,
            spellings: Some(&spellings),
            anchors: &[],
            printer: Some(Rc::new(Translation)),
        };
        let owner = Global::Authored(Qualifier::from(["lib", name]));
        let definition = unit
            .core()
            .items
            .iter()
            .flat_map(Item::definitions)
            .find(|definition| definition.name == owner)
            .unwrap_or_else(|| panic!("`{name}` is declared"));

        let print = |term: &curios_core::Term| {
            let spelling = reading.spelling().for_reader(ReaderPosition {
                owner: Some(owner),
                witnesses: Rc::default(),
            });
            let rename = build_rename(&display_names(term), &spelling);
            let spelling = Rc::new(spelling.with_pretty_names(Rc::new(rename)));
            let printed = term.spelled(&spelling).to_string();
            assert!(
                !spelling.declined(),
                "the translation declines a term of `{name}`, which prints as the compiler holds it:\n{printed}"
            );

            printed
        };

        (print(&definition.type_), print(&definition.body))
    })
}

/// The body alone.
fn body(source: &str, name: &str) -> String {
    shown(source, name).1
}

/// The type alone.
fn type_(source: &str, name: &str) -> String {
    shown(source, name).0
}

#[test]
fn a_call_is_written_with_its_name_as_the_reader_reaches_it() {
    let source = r#"
        use /std/{Nat};
        pub let subject: Nat = Nat/add(1, 2);
    "#;

    assert_eq!(
        shown(source, "subject"),
        ("Nat".into(), "Nat/add(1, 2)".into())
    );
}

#[test]
fn an_operator_is_written_as_the_operator() {
    let source = r#"
        use /std/{Nat};
        pub let subject(n: Nat) -> Nat = n * 2 + 1;
    "#;

    assert_eq!(
        shown(source, "subject"),
        ("(n: Nat) -> Nat".into(), "(n) => n * 2 + 1".into())
    );
}

#[test]
fn an_operator_that_takes_a_bound_is_written_without_its_proof() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        pub let subject(n: Nat, d: Nat, @positive: Holds(0 < d)) -> Nat = n / d + n % d;
    "#;

    assert_eq!(
        body(source, "subject"),
        "(n, d, @positive) => n / d + n % d"
    );
}

#[test]
fn a_function_type_keeps_its_marks_and_its_names() {
    let source = r#"
        use /std/{Nat, List};
        pub let subject: (@A: Type, xs: List(A), Nat) -> List(A) = (@A, xs, _) => xs;
    "#;

    assert_eq!(
        shown(source, "subject"),
        (
            "(@A: Type, xs: List(A), Nat) -> List(A)".into(),
            "(@A, xs, _) => xs".into()
        )
    );
}

#[test]
fn a_tuple_and_its_type_are_written_with_their_labels() {
    let source = r#"
        use /std/{Nat, Bool};
        pub let subject: {count: Nat, done: Bool} = (count = 3, done = true);
    "#;

    assert_eq!(
        shown(source, "subject"),
        ("{count: Nat, done: Bool}".into(), "(3, true)".into())
    );
}

#[test]
fn a_tuples_projection_is_written_by_its_place() {
    let source = r#"
        use /std/{Nat, Bool};
        pub let subject(pair: {count: Nat, done: Bool}) -> Bool = pair.done;
    "#;

    assert_eq!(body(source, "subject"), "(pair) => pair.1");
}

// The projection names its structure, so the field is written by the label its declaration gives it, and a plain one with none by its place among the plain ones: the hidden field ahead of it takes no position.
#[test]
fn a_structures_projection_is_written_by_its_label_or_its_plain_place() {
    let source = r#"
        use /std/{Nat, Str};
        use /std/Bool/{Holds};
        pub struct Wide: pub Type { a: Nat, @small: Holds(a < 10), b: Str, Str }
        pub let labelled(wide: Wide) -> Str = wide.b;
        pub let placed(wide: Wide) -> Str = wide.2;
        pub let hidden(wide: Wide) -> Holds(wide.a < 10) = wide.small;
    "#;

    assert_eq!(body(source, "labelled"), "(wide) => wide.b");
    assert_eq!(body(source, "placed"), "(wide) => wide.2");
    assert_eq!(body(source, "hidden"), "(wide) => wide.small");
    assert_eq!(
        type_(source, "hidden"),
        "(wide: Wide) -> Holds(wide.a < 10)"
    );
}

#[test]
fn a_structures_literal_is_written_with_its_hidden_field_under_its_mark() {
    let source = r#"
        use /std/{Nat, Str};
        use /std/Bool/{Holds, True};
        pub struct Wide: pub Type { a: Nat, @small: Holds(a < 10), b: Str }
        pub let subject: Wide = Wide { 3, @small = True/qed(), "x" };
    "#;

    assert_eq!(
        body(source, "subject"),
        "Wide { 3, @small = True/qed(), \"x\" }"
    );
}

#[test]
fn a_nominal_type_and_a_constructor_are_written_with_their_marks() {
    let source = r#"
        use /std/{Nat, Option, Eq};
        pub let chosen: Option(Nat) = Option/some(3);
        pub let equal: Eq(@Nat)(3, 3) = Eq/refl(@Nat, @3);
    "#;

    assert_eq!(
        shown(source, "chosen"),
        ("Option(Nat)".into(), "Option/some(@Nat, 3)".into())
    );
    assert_eq!(
        shown(source, "equal"),
        ("Eq(@Nat)(3, 3)".into(), "Eq/refl(@Nat, @3)".into())
    );
}

// The arms stand in the order the family declares its constructors, which is the order elaboration keeps them in whatever order they were written.
#[test]
fn a_match_on_a_constructor_is_written_as_its_patterns() {
    let source = r#"
        use /std/{Nat, Option};
        pub let subject(chosen: Option(Nat)) -> Nat =
            match chosen
            | none() => 0
            | some(n) => n + 1
            end;
    "#;

    assert_eq!(
        body(source, "subject"),
        "(chosen) => match chosen | some(n) => n + 1 | none() => 0 end"
    );
}

#[test]
fn a_match_on_a_truth_value_is_written_as_its_two_arms() {
    let source = r#"
        use /std/{Nat, Bool};
        pub let subject(flag: Bool) -> Nat = match flag | true => 1 | false => 0 end;
    "#;

    assert_eq!(
        body(source, "subject"),
        "(flag) => match flag | true => 1 | false => 0 end"
    );
}

#[test]
fn a_fold_over_a_number_is_written_with_its_hypothesis() {
    let source = r#"
        use /std/{Nat};
        pub let subject(n: Nat) -> Nat = match n | 0 => 0 | p + 1; ih => ih + 2 end;
    "#;

    assert_eq!(
        body(source, "subject"),
        "(n) => match n | 0 => 0 | p + 1; ih => ih + 2 end"
    );
}

#[test]
fn a_match_over_a_list_is_written_as_its_two_shapes() {
    let source = r#"
        use /std/{Nat, List};
        pub let subject(xs: List(Nat)) -> Nat = match xs | [] => 0 | [x, ..rest] => x end;
    "#;

    assert_eq!(
        body(source, "subject"),
        "(xs) => match xs | [] => 0 | [x, ..rest] => x end"
    );
}

#[test]
fn a_local_binding_is_written_as_a_let() {
    let source = r#"
        use /std/{Nat};
        pub let subject(n: Nat) -> Nat =
            let twice: Nat = n + n;
            twice * twice;
    "#;

    assert_eq!(
        body(source, "subject"),
        "(n) => let twice: Nat = n + n;\n    twice * twice"
    );
}

#[test]
fn literals_are_written_as_the_grammar_spells_them() {
    let source = r#"
        use /std/{Nat, Str, List, Bytes};
        pub let text: Str = "body";
        pub let items: List(Nat) = [1, 2, 3];
        pub let bytes: Bytes = x[0x48, 0x69];
    "#;

    assert_eq!(body(source, "text"), "\"body\"");
    assert_eq!(body(source, "items"), "[1, 2, 3]");
    assert_eq!(body(source, "bytes"), "x[0x48, 0x69]");
}

#[test]
fn a_dispatch_by_value_is_written_with_its_fallthrough() {
    let source = r#"
        use /std/{Nat};
        pub let subject(n: Nat) -> Nat = match n | 1 => 10 | 2 => 20 | _ => 0 end;
    "#;

    assert_eq!(
        body(source, "subject"),
        "(n) => match n | 1 => 10 | 2 => 20 | _ => 0 end"
    );
}

#[test]
fn a_lambda_handed_to_a_call_is_written_where_it_stands() {
    let source = r#"
        use /std/{Nat, List};
        pub let subject(xs: List(Nat)) -> List(Nat) = List/map(xs, (x) => x + 1);
    "#;

    assert_eq!(
        body(source, "subject"),
        "(xs) => List/map(@Nat, @Nat, xs, (x) => x + 1)"
    );
}

// A function's witness is written `use Show(A)` in its type and `use _` in its lambda, and the call its body makes leaves the witness to resolution, which finds that binder.
#[test]
fn a_witness_binder_is_written_unnamed_and_restored_where_it_is_used() {
    let source = r#"
        use /std/{Str, Show};
        pub let subject(@A: Type, use Show(A), x: A) -> Str = Show/show(x);
    "#;

    assert_eq!(
        shown(source, "subject"),
        (
            "(@A: Type, use Show(A), x: A) -> Str".into(),
            "(@A, use _, x) => Show/show(@A, x)".into()
        )
    );
}

// A witness a program names is one resolution would not put back: the table answers `Show(Nat)` with the library's, so the argument is written where the call was handed another.
#[test]
fn a_witness_resolution_would_not_restore_is_written() {
    let source = r#"
        use /std/{Nat, Str, Show};
        pub let loud: Show(Nat) = Show { (n) => "n" };
        pub let subject(n: Nat) -> Str = Show/show(@Nat, use loud, n);
    "#;

    assert_eq!(
        body(source, "subject"),
        "(n) => Show/show(@Nat, use loud, n)"
    );
}

// A `use` argument resolution would put back is left to it, as the program left it.
#[test]
fn a_witness_resolution_restores_is_left_out() {
    let source = r#"
        use /std/{Nat, Str, Show};
        pub let subject(n: Nat) -> Str = Show/show(n);
    "#;

    assert_eq!(body(source, "subject"), "(n) => Show/show(@Nat, n)");
}
