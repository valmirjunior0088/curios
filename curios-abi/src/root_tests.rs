use super::*;

#[test]
fn embedded_roots_classify_by_segment() {
    assert_eq!(RootId::of_segment("sys"), RootId::SYS);
    assert_eq!(RootId::of_segment("syn"), RootId::SYN);
    assert_eq!(RootId::of_segment("std"), RootId::STD);
}

#[test]
fn unknown_segment_is_the_entry_root() {
    assert_eq!(RootId::of_segment(""), RootId::ENTRY);
    assert_eq!(RootId::of_segment("MyApp"), RootId::ENTRY);
}

#[test]
fn embedded_roots_have_the_expected_privilege() {
    assert_eq!(RootId::SYS.kind(), RootKind::Internal);
    assert_eq!(RootId::SYN.kind(), RootKind::Privileged);
    assert_eq!(RootId::STD.kind(), RootKind::Privileged);
    assert_eq!(RootId::ENTRY.kind(), RootKind::Ordinary);
}

#[test]
fn internal_and_privileged_may_reach_an_internal_root() {
    assert!(RootKind::Internal.is_privileged());
    assert!(RootKind::Privileged.is_privileged());
    assert!(!RootKind::Ordinary.is_privileged());
}

#[test]
fn dynamic_roots_are_distinct_from_each_other_and_the_embedded_trio() {
    let first = RootId::dynamic(0);
    let second = RootId::dynamic(1);

    assert_eq!(first, RootId::ENTRY);
    assert_ne!(first, second);
    assert_ne!(first, RootId::SYS);
    assert_ne!(first, RootId::SYN);
    assert_ne!(first, RootId::STD);
}

#[test]
fn roster_resolves_the_embedded_trio() {
    let roster = Roster::new(std::iter::empty());

    assert_eq!(roster.resolve("sys"), RootId::SYS);
    assert_eq!(roster.resolve("syn"), RootId::SYN);
    assert_eq!(roster.resolve("std"), RootId::STD);
}

#[test]
fn roster_resolves_a_registered_dependency_to_its_own_id() {
    let roster = Roster::new(["foo".to_string()]);

    let id = roster.resolve("foo");

    assert_ne!(id, RootId::ENTRY);
    assert_ne!(id, RootId::SYS);
    assert_ne!(id, RootId::SYN);
    assert_ne!(id, RootId::STD);
}

#[test]
fn roster_falls_back_to_entry_for_unrecognized_segments() {
    let roster = Roster::new(["foo".to_string()]);

    assert_eq!(roster.resolve(""), RootId::ENTRY);
    assert_eq!(roster.resolve("MyApp"), RootId::ENTRY);
    assert_eq!(roster.resolve("bar"), RootId::ENTRY);
}

#[test]
fn roster_ids_are_stable_regardless_of_argument_order() {
    let ascending = Roster::new(["bar".to_string(), "foo".to_string()]);
    let descending = Roster::new(["foo".to_string(), "bar".to_string()]);

    assert_eq!(ascending.resolve("foo"), descending.resolve("foo"));
    assert_eq!(ascending.resolve("bar"), descending.resolve("bar"));
}
