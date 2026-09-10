use crate::*;

fn compilation() -> Vec<Mount> {
    vec![
        Mount::new(Qualifier::from(["sys"]), RootKind::Internal),
        Mount::new(Qualifier::from(["std"]), RootKind::Ordinary),
        Mount::new(Qualifier::from(["json"]), RootKind::Ordinary),
        Mount::new(Qualifier::empty(), RootKind::Ordinary),
    ]
}

#[test]
fn a_name_is_owned_by_the_most_specific_prefix_it_lies_within() {
    let mounts = compilation();

    // The entry's empty prefix contains every qualifier, so first-match would answer all three of these by it.
    assert_eq!(
        Mount::owning(&mounts, &Qualifier::from(["std", "Option", "Option"]))
            .unwrap()
            .prefix,
        Qualifier::from(["std"])
    );
    assert_eq!(
        Mount::owning(&mounts, &Qualifier::from(["sys", "Nat"]))
            .unwrap()
            .kind,
        RootKind::Internal
    );
    assert_eq!(
        Mount::owning(&mounts, &Qualifier::from(["MyApp", "Main"]))
            .unwrap()
            .prefix,
        Qualifier::empty()
    );
}

/// Segment-wise, not textual: `/stdlib` is a module of the entry, not content of `/std`.
#[test]
fn a_longer_spelling_of_a_prefix_is_not_within_it() {
    let mounts = compilation();

    assert_eq!(
        Mount::owning(&mounts, &Qualifier::from(["stdlib"]))
            .unwrap()
            .prefix,
        Qualifier::empty()
    );
}

/// While a prelude root is prepared the entry is not mounted, so the synthetic compilation root is owned by nobody. A real answer rather than a missing one — every caller that asks about ownership has a case for a name no unit claims.
#[test]
fn an_unmounted_name_has_no_owner() {
    let prelude_only = &compilation()[..2];

    assert!(Mount::owning(prelude_only, &Qualifier::empty()).is_none());
    assert_eq!(
        Mount::owning(prelude_only, &Qualifier::from(["std", "Str"]))
            .unwrap()
            .prefix,
        Qualifier::from(["std"])
    );
}
