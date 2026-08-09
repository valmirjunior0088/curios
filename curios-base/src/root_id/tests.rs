use crate::*;

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
