//! The one lint decided over a package rather than a unit: a declared dependency nothing reached.

use {super::unused_dependencies, curios_utilities::Qualifier, std::collections::BTreeSet};

#[test]
fn a_dependency_no_unit_resolved_into_is_unused_and_a_reached_one_is_not() {
    let declared = ["json".to_string(), "shape".to_string()];
    let reached = BTreeSet::from([Qualifier::from(["shape"]), Qualifier::empty()]);
    assert_eq!(
        unused_dependencies(&declared, &reached),
        [&"json".to_string()]
    );
}

/// A name is reached only by a reference resolving into its mount: a deeper qualifier lies within the prefix, a different root does not.
#[test]
fn only_the_mount_prefix_itself_counts_as_reached() {
    let declared = ["json".to_string()];
    let reached = BTreeSet::from([Qualifier::from(["jsonx"])]);
    assert_eq!(
        unused_dependencies(&declared, &reached),
        [&"json".to_string()]
    );
}
