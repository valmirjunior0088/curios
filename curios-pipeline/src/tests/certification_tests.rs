//! The certifier's record a compiled unit files beside its definitions: what it covers, what it says, what an item-level recompile keeps of the baseline's, and the termination flags it marks on a sealed program.

use {
    super::test_support::{recompile_over, reuses_body, unit_of, with_entrypoint_type},
    crate::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_core::Totality,
    curios_prelude::with_prelude,
    curios_text::RootSource,
    curios_unit::Unit,
    std::collections::BTreeMap,
};

/// A total function, one that recurses without descending — a single-member `rec` group, since its body names itself — and a value reaching the first.
const SOURCE: &str = "use /std/{Nat};

pub let double(n: Nat) -> Nat = n + n;

pub let spin(n: Nat) -> Nat = spin(n);

pub let twice: Nat = double(2);
";

/// The certifier's classification of the definition `unit` calls `symbol`.
fn classified(unit: &Unit, symbol: &str) -> Option<Totality> {
    unit.core()
        .items
        .iter()
        .flat_map(|item| item.definitions())
        .find(|definition| definition.name.symbol().ends_with(&format!("/{symbol}")))
        .and_then(|definition| unit.certification().totality(&definition.name))
}

/// A unit the pipeline compiled carries the kernel's record of every definition it holds, each classified as the kernel's own closure found it.
#[test]
fn a_compiled_unit_files_the_certifiers_record_of_every_definition() {
    let unit = unit_of(SOURCE);

    assert!(unit.certification().covers(unit.core()));
    assert_eq!(classified(&unit, "double"), Some(Totality::Total));
    assert_eq!(classified(&unit, "spin"), Some(Totality::Partial));
    assert_eq!(classified(&unit, "twice"), Some(Totality::Total));
}

/// An item-level recompile walks only the closure the edit reached, so its record joins the baseline's classifications for what it reused to its own walk's — covering the unit whole, with a reused item classified as the walk that filed the baseline classified it.
#[test]
fn an_item_level_recompile_keeps_the_baselines_record_for_what_it_reuses() {
    let baseline = unit_of(SOURCE);
    let edited = SOURCE.replace("double(2)", "double(3)");

    let recompiled = recompile_over(&edited, &baseline).expect("the edit compiles");

    assert!(reuses_body(&baseline, &recompiled, "double"));
    assert!(!reuses_body(&baseline, &recompiled, "twice"));
    assert!(recompiled.certification().covers(recompiled.core()));
    assert_eq!(classified(&recompiled, "double"), Some(Totality::Total));
    assert_eq!(classified(&recompiled, "spin"), Some(Totality::Partial));
    assert_eq!(classified(&recompiled, "twice"), Some(Totality::Total));
}

/// The fixed prelude is lent with the record its build's certification filed, and that record covers each root whole — so a walk with the prelude in scope reads the certifier's verdict on every `/sys` and `/std` definition and classifies none of them for itself.
#[test]
fn the_restored_prelude_carries_a_record_covering_every_definition() {
    with_prelude(|prelude| {
        for unit in prelude {
            assert!(unit.certification().covers(unit.core()));
        }
    });
}

/// A sealed program's functions are marked total from the certifier's records of everything it was erased from — the prelude's and the entry's own — and from nothing else: erasure marks nothing, so every mark here is one a record justified.
///
/// Matched by debug name, which erasure derives from the definition's qualified symbol: the symbol itself, or the symbol and a path below it where the definition's value is a function inside it — a single-method witness collapses to its method, named for the field. A function minted inside a body shares that derivation and is never marked, so the check runs from the marks outward: each marked function's definition a record classifies `Total`, none a record classifies `Partial` marked, the prelude contributing marks at all, and the entry's own two definitions marked exactly as its walk classified them. Mutation-checked: marking the sealed arena from no unit's record in scope fails the prelude's half.
#[test]
fn a_sealed_program_marks_its_functions_from_the_certifiers_records() {
    let source = "use /std/{Nat};

let double(n: Nat) -> Nat = n + n;

let spin(n: Nat) -> Nat = spin(n);

double(2) + spin(0)
";
    let mut functions = Vec::new();
    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &with_entrypoint_type(source, Some("/std/Nat")),
        &RootSource::none(),
        |stage| {
            if let Stage::Ersd(module) = stage {
                functions = module
                    .functions()
                    .iter()
                    .flatten()
                    .map(|function| (function.debug_name.clone(), function.total))
                    .collect();
            }
        },
    )
    .expect("the program compiles");
    let prelude = with_prelude(|prelude| {
        prelude
            .iter()
            .flat_map(|unit| unit.certification().iter())
            .map(|(name, totality)| (name.symbol(), totality))
            .collect::<BTreeMap<_, _>>()
    });

    let mut marked_from_the_prelude = 0;
    for (name, total) in &functions {
        let Some(name) = name else {
            assert!(!total, "a function with no definition behind it is marked");
            continue;
        };
        // The definition a function's name derives from: the longest recorded symbol that is the name or a `/`-separated prefix of it.
        let mut definition = Some(name.as_str());
        let from_the_prelude = loop {
            match definition {
                Some(symbol) if prelude.contains_key(symbol) => break Some(prelude[symbol]),
                Some(symbol) => definition = symbol.rsplit_once('/').map(|(above, _)| above),
                None => break None,
            }
        };
        let recorded = match from_the_prelude {
            Some(totality) => Some(totality),
            None if name.ends_with("/double") => Some(Totality::Total),
            None if name.ends_with("/spin") => Some(Totality::Partial),
            None => None,
        };
        if *total {
            assert_eq!(
                recorded,
                Some(Totality::Total),
                "{name} is marked without a record classifying it total"
            );
            marked_from_the_prelude += usize::from(from_the_prelude.is_some());
        }
    }
    assert!(
        marked_from_the_prelude > 0,
        "no prelude function was marked"
    );

    let entry = |suffix: &str| {
        functions
            .iter()
            .filter(|(name, _)| name.as_deref().is_some_and(|name| name.ends_with(suffix)))
            .any(|(_, total)| *total)
    };
    assert!(
        entry("/double"),
        "the entry's total definition is not marked"
    );
    assert!(!entry("/spin"), "the entry's partial definition is marked");
}
