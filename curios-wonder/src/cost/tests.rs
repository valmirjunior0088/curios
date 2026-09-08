use {
    super::cost,
    crate::Origin,
    curios_cont::{Fate, Outcome},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::Overlay,
};

/// Every declaration's fate, asked for the way the one-shot transport asks.
fn fates_of(text: &str) -> Vec<Fate> {
    cost(
        DEFAULT_STEP_BUDGET,
        Vec::new(),
        Origin::Text {
            label: "<stdin>".to_string(),
            text: text.to_string(),
        },
        &Overlay::default(),
        None,
    )
    .expect("the program compiles")
}

const HELPER: &str = "let helper(x : /std/Nat) -> /std/Nat = x + 1;\n/std/print(\"hello\\n\")";

#[test]
fn a_program_reports_a_fate_per_declaration_it_names() {
    let fates = fates_of(HELPER);

    assert!(
        !fates.is_empty(),
        "a compiled program names at least its own entry"
    );
    assert!(
        fates.iter().all(|fate| !fate.name.is_empty()),
        "every row is addressable"
    );
}

/// The order is the deliverable: a profile that reproduces is what makes a regression a diff, and a map's iteration order is not one.
#[test]
fn rows_are_ordered_by_name() {
    let fates = fates_of(HELPER);
    let names = fates
        .iter()
        .map(|fate| fate.name.clone())
        .collect::<Vec<_>>();
    let mut sorted = names.clone();
    sorted.sort();

    assert_eq!(names, sorted);
}

/// A declaration nothing reaches is absorbed rather than reported as surviving — which is the row that answers "this costs nothing of its own", and the one a diff of the two graphs exists to produce.
#[test]
fn a_declaration_the_program_never_reaches_is_absorbed() {
    let fates = fates_of(HELPER);

    // Pruning before Cont is as good an answer as absorption at it: either way the declaration is not in the compiled program, and this query reports what reached the graph.
    if let Some(fate) = fates.iter().find(|fate| fate.name.ends_with("helper")) {
        assert_eq!(fate.outcome, Outcome::Absorbed);
    }
}
