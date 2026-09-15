//! Which unit takes an archived root's place, what it is granted, and what it is offered.

use {
    super::{Baselined, granted, withheld},
    crate::{
        Cache, DEFAULT_STEP_BUDGET, compile_unit_over, invalidated,
        tests::test_support::compile_with_units,
    },
    curios_prelude::{SYNTAX, with_stored},
    curios_text::{Overlay, RootSource, UnitSource, into_core_unit},
    curios_unit::{Prefix, Unit},
    curios_utilities::{Qualifier, RootKind, test_support::Temporary},
    std::{fs, path::PathBuf, time::Instant},
};

/// The standard library's own tree, as the package claiming `/std` from it.
fn std_from_its_tree() -> RootSource {
    let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../curios-prelude-archive/std");

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
    let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../curios-prelude-archive/std");
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

    with_stored(|stored| {
        let sys = &stored[0].unit;
        let std = &stored[1].unit;
        let roots = [sys];
        let scope = Prefix::over(&roots);

        println!("\n=== recompiling /std over the archive ===");
        for Edit { label, file, edit } in edits {
            let path = directory.join(file);
            let text = edit(&fs::read_to_string(&path).expect("an authored source"));
            let source = std_from_its_tree().with_overlay(Overlay::of([(path, text)]));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            let start = Instant::now();
            let lowered =
                into_core_unit(&unit, &scope.text(), &SYNTAX).expect("the edited library lowers");
            let lowered_in = start.elapsed();
            let start = Instant::now();
            let closure = invalidated(std, lowered.core());
            let diffed_in = start.elapsed();
            let start = Instant::now();
            compile_unit_over(DEFAULT_STEP_BUDGET, scope, &SYNTAX, &unit, std)
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
    with_stored(|stored| {
        for claim in [std_from_its_tree(), std_from_elsewhere(), supplied("std")] {
            let (index, root) = withheld(stored, &[claim]).expect("a package named std");

            assert_eq!(index + 1, stored.len(), "the last root, /std");
            assert_eq!(root.unit.mounts()[0].prefix, Qualifier::from(["std"]));
        }
    });
}

#[test]
fn a_package_named_otherwise_takes_no_roots_place() {
    with_stored(|stored| {
        assert!(withheld(stored, &[supplied("other")]).is_none());
        assert!(withheld(stored, &[]).is_none());
    });
}

/// Only the last root can be taken, since the roots after a withheld one were compiled against it: a claim on the compiler's own root, which nothing could name anyway, collides with it as any claim does.
#[test]
fn a_package_named_sys_collides_with_the_compilers_own_root() {
    with_stored(|stored| {
        assert!(withheld(stored, &[supplied("sys")]).is_none());
    });

    let error = compile_with_units(&[("sys", "pub let a : /std/Nat = 1;")], "0")
        .expect_err("the compiler's own root is in scope");

    assert!(error.contains("sys"), "unexpected error: {error}");
}

/// A unit after the first has a scope the archived unit was never compiled in, so it is not the one that takes the root's place — and it collides, as any later claim does.
#[test]
fn only_the_first_unit_can_take_a_roots_place() {
    with_stored(|stored| {
        assert!(withheld(stored, &[supplied("other"), std_from_its_tree()]).is_none());
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
    with_stored(|stored| {
        let prefixes = granted(stored, stored.len() - 1, &std_from_its_tree());

        assert!(prefixes.contains(&Qualifier::from(["sys"])));
    });
}

/// The archived unit reaches the caller's cache as an offer for the unit claiming its prefix and for no other, and the cache decides what becomes of it.
#[test]
fn the_withheld_root_is_offered_as_the_baseline() {
    struct Taking;

    impl Cache for Taking {
        fn get(&self, _: &UnitSource<'_>) -> Option<Unit> {
            None
        }

        fn baseline(&self, _: &UnitSource<'_>, offered: Option<Unit>) -> Option<Unit> {
            offered
        }

        fn put(&self, _: &UnitSource<'_>, _: &Unit) {}
    }

    with_stored(|stored| {
        let root = stored.last().expect("the prelude has roots");
        let baselined = Baselined {
            cache: Some(&Taking),
            withheld: Some((Qualifier::from(["std"]), &root.unit)),
        };
        let std = std_from_its_tree();
        let other = supplied("other");

        assert!(
            baselined
                .baseline(&UnitSource::mounted(&std), None)
                .is_some()
        );
        assert!(
            baselined
                .baseline(&UnitSource::mounted(&other), None)
                .is_none()
        );

        let declined = Baselined {
            cache: None,
            withheld: Some((Qualifier::from(["std"]), &root.unit)),
        };
        assert!(
            declined
                .baseline(&UnitSource::mounted(&std), None)
                .is_none(),
            "no cache, no taker"
        );
    });
}
