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
