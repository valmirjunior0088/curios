//! One unit compiled over a baseline: what the closure covers, what is reused untouched, and that the result agrees with a whole compile of the same text.
//!
//! Reuse is observed by allocation identity — a reused item carries the very terms the baseline holds, which no elaboration could produce twice — and agreement by the differential predicate in `test_support`. Resource verdicts are deliberately outside the predicate: a partial walk runs in a different cache state, so a budget-marginal declaration can move either way, and the specification says so.

use super::test_support::{assert_modules_agree, recompile_over, reuses_body, unit_of};

/// Three items: `twice` reaches `double`, and `unrelated` reaches neither.
const BASE: &str = "use /std/{Nat};

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = double(2);

pub let unrelated: Nat = 7;
";

#[test]
fn an_unchanged_text_reuses_every_item() {
    let baseline = unit_of(BASE);

    let again = recompile_over(BASE, &baseline).unwrap();

    for name in ["double", "twice", "unrelated"] {
        assert!(
            reuses_body(&baseline, &again, name),
            "{name} was re-elaborated"
        );
    }
    assert_modules_agree(baseline.core(), again.core());
}

#[test]
fn an_edited_body_recompiles_the_item_and_its_dependents_and_agrees_with_the_whole_compile() {
    let edited = BASE.replace("n + n", "n + n + 0");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "double"));
    assert!(
        !reuses_body(&baseline, &incremental, "twice"),
        "a dependent of a changed item is re-elaborated"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
}

#[test]
fn a_removed_item_is_gone_and_the_rest_is_reused() {
    let edited = BASE.replace("pub let unrelated: Nat = 7;\n", "");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert_eq!(incremental.core().items.len(), 2);
    assert!(reuses_body(&baseline, &incremental, "double"));
    assert!(reuses_body(&baseline, &incremental, "twice"));
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
}

/// A moved declaration changes the lowering's order and nothing about any item, so everything is reused and only reassembled.
#[test]
fn an_item_moved_past_another_reuses_every_item() {
    let moved = "use /std/{Nat};

pub let unrelated: Nat = 7;

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = double(2);
";
    let baseline = unit_of(BASE);

    let incremental = recompile_over(moved, &baseline).unwrap();

    for name in ["double", "twice", "unrelated"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was re-elaborated"
        );
    }
    assert_modules_agree(unit_of(moved).core(), incremental.core());
}

/// A lowering numbers its universe metavariables and holes in lowering order, so an item inserted ahead renumbers every polymorphic item after it; the diff identifies them by position and leaves those items reused.
#[test]
fn inserting_a_polymorphic_item_leaves_the_items_after_it_unchanged() {
    let base = "use /std/{Nat};

pub let apply(@A: Type, f: (A) -> A, a: A) -> A = f(a);

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = apply(double, 2);
";
    let inserted = base.replace(
        "pub let apply",
        "pub let id(@A: Type, a: A) -> A = a;\n\npub let apply",
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&inserted, &baseline).unwrap();

    for name in ["apply", "double", "twice"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was re-elaborated"
        );
    }
    assert_eq!(incremental.core().items.len(), 4);
    assert_modules_agree(unit_of(&inserted).core(), incremental.core());
}

/// A struct literal names its declaration in the registry rather than the variable graph, so an item building one depends on the declaration through the reach edge `mentions` cannot see — and a changed registry entry marks its declaring item changed.
#[test]
fn an_edited_struct_recompiles_the_items_that_construct_it() {
    let base = "use /std/{Nat};

pub struct Box: pub Type { value: Nat }

pub let unbox(b: Box) -> Nat = b.value;

pub let value_of_boxed: Nat = (Box { value = 1 }).value;

pub let unrelated: Nat = 7;
";
    let sealed = base.replace("Box: pub Type", "Box: Type");
    let baseline = unit_of(base);

    let incremental = recompile_over(&sealed, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "unbox"));
    assert!(
        !reuses_body(&baseline, &incremental, "value_of_boxed"),
        "an item constructing the struct is in the closure"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&sealed).core(), incremental.core());
}

/// A baseline from unrelated text invalidates everything, and the recompile is then a whole compile by another route.
#[test]
fn an_all_changed_closure_equals_the_whole_compile() {
    let baseline = unit_of("use /std/{Nat};\n\npub let other: Nat = 1;\n");

    let incremental = recompile_over(BASE, &baseline).unwrap();

    assert_eq!(incremental.core().items.len(), 3);
    assert_modules_agree(unit_of(BASE).core(), incremental.core());
}
