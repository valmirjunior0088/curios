//! What a `/sys` description lowers to, and how the host-row join places one.
//!
//! The subject is the description layer, not the roster's contents. What `/sys` declares is checked where it already is — the prelude build elaborates and certifies every one of these declarations against `Intrinsic::signature`, and `into_core/sys_tests.rs` pins the root's reachability and `/sys/Io`'s roster — so a second statement of the roster here would be a copy to keep in step rather than a check. What has no other check is the shape a `Decl` lowers to and the placement the join decides.

use {
    super::*,
    crate::{LetSignature, Pattern},
    curios_abi::{ForeignFunction, ForeignStore, Namespace, WireResults, WireSignature, WireType},
    curios_utilities::Plicity,
};

/// One store row naming `subject` as the `/sys` module it belongs to.
fn row(subject: &str, label: &str) -> ForeignFunction {
    ForeignFunction {
        namespace: Namespace::Sys,
        name: format!("{subject}_{label}"),
        subject: Some(subject.to_string()),
        label: label.to_string(),
        signature: WireSignature {
            params: Vec::new(),
            results: WireResults::single("value".to_string(), WireType::Nat),
        },
        description: String::new(),
    }
}

fn store(rows: &[(&str, &str)]) -> ForeignStore {
    let mut store = ForeignStore::new();

    for (subject, label) in rows {
        store.register(row(subject, label));
    }

    store
}

/// The labels a module declares, in order — the placement question every test below asks.
fn labels(module: &SysModule) -> Vec<&str> {
    module
        .decls
        .iter()
        .map(|decl| decl.label.as_str())
        .collect()
}

// The rule `host_fn`'s zero-arity path used to spell separately: a description with nothing to bind is a value, and `LetSignature::Func` with an empty telescope would lower to a binderless lambda instead.
#[test]
fn an_empty_telescope_lowers_to_a_constant_rather_than_a_nullary_function() {
    let signature = pub_let("stdin", nat(), nat_lit(0)).into_let().signature;

    assert!(
        matches!(signature, LetSignature::Name { .. }),
        "an empty telescope lowered to {signature:?}"
    );
}

#[test]
fn a_telescope_lowers_to_the_function_sugar_carrying_every_plicity_mark() {
    let declaration = pub_fn_marked(
        "get",
        vec![
            (Plicity::Implicit, "T", type_()),
            (Plicity::Explicit, "a", nat()),
        ],
        nat(),
        name("a"),
    );

    let LetSignature::Func { params, .. } = declaration.into_let().signature else {
        panic!("a telescope lowered to a constant");
    };

    let marks = params.iter().map(|param| param.plicity).collect::<Vec<_>>();
    assert_eq!(marks, [Plicity::Implicit, Plicity::Explicit]);
    assert!(matches!(&params[0].label, Pattern::Binder(Some(binder)) if binder.as_str() == "T"));
}

// The whole of what `documented` used to need a `match`, an assertion and a `panic!` arm to do.
#[test]
fn a_gloss_lands_on_the_declaration_it_is_written_above() {
    let declaration = documented(&["Their sum.", "", "A second paragraph."], nat_succ());

    let doc = declaration
        .doc
        .expect("a documented declaration carries one");
    assert_eq!(doc.lines, ["Their sum.", "", "A second paragraph."]);
}

#[test]
fn a_host_row_opens_the_module_its_subject_names_when_nothing_declared_it() {
    let modules = absorb_host_rows(Vec::new(), &store(&[("file", "open"), ("file", "close")]));

    assert_eq!(modules.len(), 1);
    assert_eq!(modules[0].label, "file");
    assert_eq!(labels(&modules[0]), ["open", "close"]);
}

// `Handle` is the carrier in both inputs and `proc` the module of operations in both; one rule places each, and neither is lifted out and appended by hand.
#[test]
fn a_host_row_joins_the_module_that_declared_its_subject() {
    let declared = vec![SysModule::ops("proc", vec![nat_succ()])];
    let modules = absorb_host_rows(declared, &store(&[("proc", "args"), ("file", "open")]));

    assert_eq!(modules.len(), 2, "a joined subject opens no second module");
    assert_eq!(
        labels(&modules[0]),
        ["succ", "args"],
        "a declared binding precedes the rows that join it"
    );
    assert_eq!(modules[1].label, "file");
}

// The defect the declaration replaced: `exit` was placed by a `find` over the joined roster under an `if let`, so a store carrying no `proc` row dropped it silently. Declared, it survives a store that names it nowhere.
#[test]
fn a_declared_module_survives_a_store_that_names_it_in_no_row() {
    let declared = vec![SysModule::ops("proc", vec![nat_succ()])];
    let modules = absorb_host_rows(declared, &store(&[("file", "open")]));

    assert_eq!(labels(&modules[0]), ["succ"]);
}

// Order is the contract the join states: declaration order first, then first-row order for a subject nothing declared.
#[test]
fn a_subject_nothing_declared_keeps_the_order_its_first_row_appears_in() {
    let modules = absorb_host_rows(
        Vec::new(),
        &store(&[("socket", "bind"), ("file", "open"), ("socket", "listen")]),
    );

    let order = modules
        .iter()
        .map(|module| module.label.as_str())
        .collect::<Vec<_>>();

    assert_eq!(order, ["socket", "file"]);
    assert_eq!(labels(&modules[0]), ["bind", "listen"]);
}
