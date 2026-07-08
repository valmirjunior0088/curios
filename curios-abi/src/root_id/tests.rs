use crate::*;

#[test]
fn embedded_roots_classify_by_segment() {
    assert_eq!(RootId::of_segment("sys"), RootId::Sys);
    assert_eq!(RootId::of_segment("syn"), RootId::Syn);
    assert_eq!(RootId::of_segment("std"), RootId::Std);
}

#[test]
fn unknown_segment_is_the_entry_root() {
    assert_eq!(RootId::of_segment(""), RootId::Entry);
    assert_eq!(RootId::of_segment("MyApp"), RootId::Entry);
}

#[test]
fn embedded_roots_have_the_expected_privilege() {
    assert_eq!(RootId::Sys.kind(), RootKind::Internal);
    assert_eq!(RootId::Syn.kind(), RootKind::Privileged);
    assert_eq!(RootId::Std.kind(), RootKind::Privileged);
    assert_eq!(RootId::Entry.kind(), RootKind::Ordinary);
}

#[test]
fn internal_and_privileged_may_reach_an_internal_root() {
    assert!(RootKind::Internal.is_privileged());
    assert!(RootKind::Privileged.is_privileged());
    assert!(!RootKind::Ordinary.is_privileged());
}
