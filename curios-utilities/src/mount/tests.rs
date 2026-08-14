use crate::*;

fn compilation() -> Vec<Mount> {
    vec![
        Mount::new(Qualifier::from(["sys"]), RootKind::Internal),
        Mount::new(Qualifier::from(["syn"]), RootKind::Privileged),
        Mount::new(Qualifier::from(["std"]), RootKind::Privileged),
        Mount::new(Qualifier::empty(), RootKind::Ordinary),
    ]
}

#[test]
fn a_name_is_owned_by_the_most_specific_prefix_it_lies_within() {
    let mounts = compilation();

    // The entry's empty prefix contains every qualifier, so first-match would answer all four of these as ordinary.
    assert_eq!(
        Mount::owning(&mounts, &Qualifier::from(["std", "Option", "Option"]))
            .unwrap()
            .kind,
        RootKind::Privileged
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

/// While the fixed prelude is prepared the entry is not mounted, so the synthetic compilation root is owned by nobody — and an unowned name is not privileged, which is what keeps `/sys` unreachable from it.
#[test]
fn an_unmounted_name_has_no_owner_and_no_privilege() {
    let prelude_only = &compilation()[..3];

    assert!(Mount::owning(prelude_only, &Qualifier::empty()).is_none());
    assert!(!Mount::privileged(prelude_only, &Qualifier::empty()));
    assert!(Mount::privileged(
        prelude_only,
        &Qualifier::from(["std", "Str"])
    ));
}

#[test]
fn internal_and_privileged_may_reach_an_internal_root() {
    assert!(RootKind::Internal.is_privileged());
    assert!(RootKind::Privileged.is_privileged());
    assert!(!RootKind::Ordinary.is_privileged());
}
