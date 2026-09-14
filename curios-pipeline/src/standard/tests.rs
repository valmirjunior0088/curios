//! Which unit takes an archived root's place, what it is granted, and what it is offered.

use {
    super::{Baselined, granted, withheld},
    crate::{Cache, tests::test_support::compile_with_units},
    curios_prelude::with_stored,
    curios_text::{RootSource, UnitSource},
    curios_unit::Unit,
    curios_utilities::{Qualifier, RootKind},
    std::{env, path::PathBuf},
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

/// The same claim from a directory the record does not name.
fn std_from_elsewhere() -> RootSource {
    let directory = env::temp_dir().join("curios-standard-elsewhere");

    RootSource::mounted(
        "std",
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory,
    )
}

#[test]
fn a_root_claimed_from_the_directory_its_record_names_is_withheld() {
    with_stored(|stored| {
        let (index, root) =
            withheld(stored, &[std_from_its_tree()]).expect("the tree is the archive's");

        assert_eq!(index + 1, stored.len(), "the last root, /std");
        assert_eq!(root.unit.mounts()[0].prefix, Qualifier::from(["std"]));
    });
}

/// Neither a claim from another directory nor one supplied whole — which reads no directory at all — is the tree the archive came from.
#[test]
fn a_root_claimed_from_elsewhere_is_not_withheld() {
    let mut supplied = RootSource::supplied();
    supplied.insert_root(
        "std",
        RootKind::Ordinary,
        "pub let a : /std/Nat = 1;".parse().unwrap(),
    );

    with_stored(|stored| {
        assert!(withheld(stored, &[std_from_elsewhere()]).is_none());
        assert!(withheld(stored, &[supplied]).is_none());
        assert!(withheld(stored, &[]).is_none());
    });
}

/// A unit after the first has a scope the archived unit was never compiled in, so it is not the one that takes the root's place.
#[test]
fn only_the_first_unit_can_take_a_roots_place() {
    let mut other = RootSource::supplied();
    other.insert_root(
        "other",
        RootKind::Ordinary,
        "pub let a : /std/Nat = 1;".parse().unwrap(),
    );

    with_stored(|stored| {
        assert!(withheld(stored, &[other, std_from_its_tree()]).is_none());
    });
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
        let elsewhere = std_from_elsewhere();
        let mut other = RootSource::supplied();
        other.insert_root(
            "other",
            RootKind::Ordinary,
            "pub let a : /std/Nat = 1;".parse().unwrap(),
        );

        assert!(
            baselined
                .baseline(&UnitSource::mounted(&std), None)
                .is_some()
        );
        assert!(
            baselined
                .baseline(&UnitSource::mounted(&elsewhere), None)
                .is_some(),
            "the offer is by prefix; which directory claims it was decided when the root was withheld"
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

/// A unit supplied whole claiming `/std` reads no directory, so the root stands and the claim collides as it always did.
#[test]
fn a_root_claimed_from_elsewhere_still_collides() {
    let error = compile_with_units(&[("std", "pub let a : /std/Nat = 1;")], "0")
        .expect_err("the archived root is in scope");

    assert!(error.contains("std"), "unexpected error: {error}");
}
