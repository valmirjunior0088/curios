//! Which unit takes an archived root's place, what it is granted, and what it is offered.

use {
    super::{granted, withheld},
    crate::{
        Cache, DEFAULT_STEP_BUDGET, Fold, compile_unit_over, invalidated,
        tests::test_support::compile_with_units,
    },
    curios_prelude::with_prelude,
    curios_text::{Overlay, RootSource, SYNTAX, UnitSource, into_core_unit, std_directory},
    curios_unit::{Predecessors, Unit},
    curios_utilities::{Qualifier, RootKind, test_support::Temporary},
    std::{cell::RefCell, fs, time::Instant},
};

/// The standard library's own tree, as the package claiming `/std` from it.
fn std_from_its_tree() -> RootSource {
    let directory = std_directory();

    RootSource::mounted(
        "std",
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory,
    )
    .declaring([Qualifier::from(["sys"])])
}

/// The same claim from a directory that is not the archive's tree — one nothing creates, so its guard has nothing to remove.
fn std_from_elsewhere() -> RootSource {
    let directory = Temporary::new("standard", "elsewhere");

    RootSource::mounted(
        "std",
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory.to_path_buf(),
    )
}

/// One edit the census measures: what it is, the file it touches, and the text it leaves there.
struct Edit {
    label: &'static str,
    file: &'static str,
    edit: fn(&str) -> String,
}

/// What a question about the standard library costs after an edit to one declaration: the closure the edit reaches and the time each phase takes, for a leaf and for a hub. A measurement, so it reports rather than asserts, and its timings are the profile it was built under.
#[test]
#[ignore = "measurement: lowers the standard library and recompiles it over the archive, reporting closure sizes and per-phase timings"]
fn std_recompile_closure_census() {
    let directory = std_directory();
    let edits = [
        Edit {
            label: "a leaf: a declaration added to /std/Nat",
            file: "Nat.crs",
            edit: |text| format!("{text}\npub let _census_probe(a: Nat) -> Nat =\n    a;\n"),
        },
        Edit {
            label: "a hub: the body of /std/Bool/not respelled",
            file: "Bool.crs",
            edit: |text| text.replacen("xor(b, true)", "xor(true, b)", 1),
        },
    ];

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);

        println!("\n=== recompiling /std over the archive ===");
        for Edit { label, file, edit } in edits {
            let path = directory.join(file);
            let text = edit(&fs::read_to_string(&path).expect("an authored source"));
            let source = std_from_its_tree().with_overlay(Overlay::of([(path, text)]));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            let start = Instant::now();
            let lowered = into_core_unit(&unit, &predecessors.text(), &SYNTAX)
                .expect("the edited library lowers");
            let lowered_in = start.elapsed();
            let start = Instant::now();
            let closure = invalidated(std, lowered.core());
            let diffed_in = start.elapsed();
            let start = Instant::now();
            compile_unit_over(DEFAULT_STEP_BUDGET, predecessors, &SYNTAX, &unit, std)
                .expect("the edited library recompiles");
            let recompiled_in = start.elapsed();

            println!("{label}");
            println!(
                "  closure     {:>6} names of {} items",
                closure.len(),
                std.core().items.len()
            );
            println!("  lower       {lowered_in:>10.1?}");
            println!("  diff+close  {diffed_in:>10.1?}");
            println!(
                "  recompile   {recompiled_in:>10.1?}   (lower, diff, elaborate, judge, erase)"
            );
        }
    });
}

/// A unit supplied whole under `prefix`, holding one declaration.
fn supplied(prefix: &str) -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root(
        prefix,
        RootKind::Ordinary,
        "pub let a : /std/Nat = 1;".parse().unwrap(),
    );

    modules
}

/// The name is the claim: a package named `std` takes the archived root's place wherever it is read from, and however it arrived.
#[test]
fn a_package_named_std_takes_the_archived_roots_place() {
    with_prelude(|prelude| {
        for claim in [std_from_its_tree(), std_from_elsewhere(), supplied("std")] {
            let (index, root) = withheld(prelude, &[claim]).expect("a package named std");

            assert_eq!(index + 1, prelude.len(), "the last root, /std");
            assert_eq!(root.mounts()[0].prefix, Qualifier::from(["std"]));
        }
    });
}

#[test]
fn a_package_named_otherwise_takes_no_roots_place() {
    with_prelude(|prelude| {
        assert!(withheld(prelude, &[supplied("other")]).is_none());
        assert!(withheld(prelude, &[]).is_none());
    });
}

/// Only the last root can be taken, since the roots after a withheld one were compiled against it: a claim on the compiler's own root, which nothing could name anyway, collides with it as any claim does.
#[test]
fn a_package_named_sys_collides_with_the_compilers_own_root() {
    with_prelude(|prelude| {
        assert!(withheld(prelude, &[supplied("sys")]).is_none());
    });

    let error = compile_with_units(&[("sys", "pub let a : /std/Nat = 1;")], "0")
        .expect_err("the compiler's own root is in scope");

    assert!(error.contains("sys"), "unexpected error: {error}");
}

/// A unit after the first has predecessors the archived unit was never compiled against, so it is not the one that takes the root's place — and it collides, as any later claim does.
#[test]
fn only_the_first_unit_can_take_a_roots_place() {
    with_prelude(|prelude| {
        assert!(withheld(prelude, &[supplied("other"), std_from_its_tree()]).is_none());
    });

    let error = compile_with_units(
        &[
            ("other", "pub let a : /std/Nat = 1;"),
            ("std", "pub let b : /std/Nat = 2;"),
        ],
        "0",
    )
    .expect_err("a package named std after another unit claims a root still in scope");

    assert!(error.contains("std"), "unexpected error: {error}");
}

#[test]
fn a_withheld_root_is_granted_the_roots_before_it() {
    with_prelude(|prelude| {
        let prefixes = granted(prelude, prelude.len() - 1, &std_from_its_tree());

        assert!(prefixes.contains(&Qualifier::from(["sys"])));
    });
}

/// The archived unit is offered to the unit taking its root's place and to no other, and the offer reaches the cache, which decides what becomes of it.
///
/// Read off what the cache was offered rather than off a compile over it, so the standard library is never recompiled here: the offer is made before a unit compiles, and a one-declaration `/std`, which does not, is refused only after it.
#[test]
fn the_withheld_root_is_offered_to_the_unit_taking_its_place() {
    struct Recording(RefCell<Vec<bool>>);

    impl Cache for Recording {
        fn get(&self, _: &UnitSource<'_>) -> Option<Unit> {
            None
        }

        fn baseline(&self, _: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Unit> {
            self.0.borrow_mut().push(offered.is_some());
            None
        }

        fn put(&self, _: &UnitSource<'_>, _: &Unit, _: bool) {}
    }

    for (claim, offered) in [("std", true), ("other", false)] {
        let units = [supplied(claim)];
        let recording = Recording(RefCell::new(Vec::new()));
        let _ = Fold::new(DEFAULT_STEP_BUDGET, &units, Some(&recording)).check_units(|_| {});

        assert_eq!(*recording.0.borrow(), [offered], "a unit claiming /{claim}");
    }
}
